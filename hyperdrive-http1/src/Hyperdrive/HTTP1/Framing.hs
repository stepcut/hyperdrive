{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | How the body of an HTTP/1.1 message is framed (RFC 9112, Section 6):
-- by @Content-Length@, by the chunked transfer coding, by closing the
-- connection, or not at all. For requests, how to read it; for
-- responses, how to send it.
--
-- This is where request smuggling happens: when two implementations
-- on the path read the framing of the same bytes differently, one of
-- them takes part of a body for a new request. So every request whose
-- framing is not unambiguous is rejected, and the connection closed.
module Hyperdrive.HTTP1.Framing
  ( -- * Requests
    RequestFraming(..)
  , FramingError(..)
  , requestFraming
  , framingErrorStatus
  , acceptsTrailers
    -- * Responses
  , Payload(..)
  , ResponseFraming(..)
  , responseFraming
  , framingFields
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word (Word64)
import Hyperdrive.Semantics.Field (FieldName, FieldValue, Fields, hContentLength, hTransferEncoding, lookupAll, mkFieldName, mkFieldValue)
import Data.Maybe (fromJust)
import Hyperdrive.Semantics.FieldSyntax (Comments(..), parseQuotedString, splitList)
import Hyperdrive.Semantics.Message (HttpVersion, pattern HTTP_1_1)
import Hyperdrive.Semantics.Method (Method, pattern CONNECT, pattern HEAD)
import Hyperdrive.Semantics.Status (Status, StatusClass(..), statusClass, pattern BadRequest, pattern NoContent, pattern NotImplemented, pattern NotModified)
import Hyperdrive.Semantics.Syntax (isDIGIT, isOWS, isTChar)

-- | How a request's body is framed.
data RequestFraming
  = NoBody                -- ^ neither field: the request has no body
  | ContentLength !Word64 -- ^ exactly this many octets
  | Chunked               -- ^ the chunked transfer coding
  deriving (Eq, Show)

-- | A request whose framing cannot be relied on. The server responds
-- with 'framingErrorStatus' and then closes the connection, since the
-- rest of what is on it cannot be told apart from the body.
data FramingError
  = TransferEncodingInHTTP10   -- ^ Transfer-Encoding in an HTTP/1.0 request
  | BothLengthAndEncoding      -- ^ Transfer-Encoding and Content-Length together
  | InvalidTransferEncoding    -- ^ a Transfer-Encoding value that is not a list of transfer codings
  | ChunkedNotFinal            -- ^ Transfer-Encoding whose last coding is not chunked
  | ChunkedTwice               -- ^ the chunked coding applied more than once
  | ChunkedWithParameters      -- ^ the chunked coding with parameters, which it does not have
  | UnsupportedTransferCoding !ByteString -- ^ a transfer coding other than chunked (lower case)
  | InvalidContentLength       -- ^ not a valid length, or different lengths, or one too large
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.1
-- # A server that receives a request message with a transfer coding it
-- # does not understand SHOULD respond with 501 (Not Implemented).

-- | The status to respond with: 501 (Not Implemented) for a transfer
-- coding the server does not support, 400 (Bad Request) otherwise.
framingErrorStatus :: FramingError -> Status
framingErrorStatus (UnsupportedTransferCoding _) = NotImplemented
framingErrorStatus _                             = BadRequest

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.1
-- # A server or client that receives an HTTP/1.0 message containing a
-- # Transfer-Encoding header field MUST treat the message as if the
-- # framing is faulty, even if a Content-Length is present, and close the
-- # connection after processing the message.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.1
-- # A server MAY reject a request that contains both Content-Length and
-- # Transfer-Encoding or process such a request in accordance with the
-- # Transfer-Encoding alone.  Regardless, the server MUST close the
-- # connection after responding to such a request to avoid the potential
-- # attacks.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.3
-- # If a Transfer-Encoding header field is present in a request and
-- # the chunked transfer coding is not the final encoding, the
-- # message body length cannot be determined reliably; the server
-- # MUST respond with the 400 (Bad Request) status code and then
-- # close the connection.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.3
-- # If this is a request message and none of the above are true, then
-- # the message body length is zero (no message body is present).

-- | The framing of a request's body, from its version and header
-- fields, following RFC 9112 Section 6.3:
--
-- * @Transfer-Encoding@ in an HTTP/1.0 request is faulty framing.
-- * @Transfer-Encoding@ together with @Content-Length@ is rejected:
--   RFC 9112 allows obeying @Transfer-Encoding@ alone, but this is the
--   pattern request smuggling uses, and refusing it is safer.
-- * Otherwise @Transfer-Encoding@ must end with @chunked@, applied once
--   and without parameters. No other transfer coding is supported, so
--   any other gets 501.
-- * Otherwise @Content-Length@, if present, gives the length. Repeated
--   field lines or a list are accepted if every value is the same
--   number.
-- * Otherwise there is no body.
requestFraming :: HttpVersion -> Fields -> Either FramingError RequestFraming
requestFraming version fields
  | not (null te), version < HTTP_1_1 = Left TransferEncodingInHTTP10
  | not (null te), not (null cl)      = Left BothLengthAndEncoding
  | not (null te)                     = transferEncoding te
  | not (null cl)                     = ContentLength <$> contentLength cl
  | otherwise                         = Right NoBody
  where
    te = lookupAll hTransferEncoding fields
    cl = lookupAll hContentLength fields
    transferEncoding vs = do
      codings <- concat <$> traverse (either (const (Left InvalidTransferEncoding)) (traverse coding) . splitList NoComments) vs
      case reverse codings of
        (("chunked", params) : earlier)
          | params                          -> Left ChunkedWithParameters
          | any ((== "chunked") . fst) earlier -> Left ChunkedTwice
          | ((c, _) : _) <- reverse earlier  -> Left (UnsupportedTransferCoding c)
          | otherwise                       -> Right Chunked
        _ -> Left ChunkedNotFinal

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.4
-- # As described in Section 6.5, a TE field with a "trailers" member sent
-- # in a request indicates that the client will not discard trailer
-- # fields.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.4
-- # TE                 = #t-codings
-- # t-codings          = "trailers" / ( transfer-coding [ weight ] )

-- | Whether the request's @TE@ has a @trailers@ member, for
-- 'Hyperdrive.Semantics.Message.setAcceptsTrailers'.
--
-- A @TE@ that is not a valid list counts as not having one: it says
-- nothing about the response's framing, so there is no need to reject
-- the request. A weight (@;q=0.5@) has the syntax of a
-- transfer-parameter, so 'coding' reads both, and @trailers;q=1@ is a
-- transfer coding named trailers, not the @trailers@ member. The
-- @Connection: TE@ its sender must also send is not required.
acceptsTrailers :: Fields -> Bool
acceptsTrailers fields =
  case traverse (either (const Nothing) (traverse (either (const Nothing) Just . coding)) . splitList NoComments)
                (lookupAll hTE fields) of
    Just members -> ("trailers", False) `elem` concat members
    Nothing      -> False
  where hTE = fromJust (mkFieldName "TE")

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.4
-- # transfer-coding    = token *( OWS ";" OWS transfer-parameter )
-- # transfer-parameter = token BWS "=" BWS ( token / quoted-string )

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7
-- # All transfer-coding names are case-insensitive

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # The chunked coding does not define any parameters.  Their presence
-- # SHOULD be treated as an error.

-- | A transfer coding: its name in lower case, and whether it has
-- parameters.
coding :: ByteString -> Either FramingError (ByteString, Bool)
coding element = do
  let (name, rest) = B.span isTChar element
  if B.null name then Left InvalidTransferEncoding else Right ()
  params rest
  Right (C.map toLower name, not (B.null rest))
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    -- *( OWS ";" OWS transfer-parameter )
    params s
      | B.null s = Right ()
      | otherwise = case B.uncons (B.dropWhile isOWS s) of
          Just (0x3B, s1) -> do
            let (pname, s2) = B.span isTChar (B.dropWhile isOWS s1)
            if B.null pname then Left InvalidTransferEncoding else Right ()
            case B.uncons (B.dropWhile isOWS s2) of
              Just (0x3D, s3) -> value (B.dropWhile isOWS s3) >>= params
              _               -> Left InvalidTransferEncoding
          _ -> Left InvalidTransferEncoding
    -- ( token / quoted-string ), returning what follows
    value s = case B.uncons s of
      Just (0x22, _) -> either (const (Left InvalidTransferEncoding)) (Right . snd) (parseQuotedString s)
      _ -> let (t, rest) = B.span isTChar s
           in if B.null t then Left InvalidTransferEncoding else Right rest

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.6
-- # Content-Length = 1*DIGIT

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.3
-- # If a message is received without Transfer-Encoding and with an
-- # invalid Content-Length header field, then the message framing is
-- # invalid and the recipient MUST treat it as an unrecoverable
-- # error, unless the field value can be successfully parsed as a
-- # comma-separated list (Section 5.6.1 of [HTTP]), all values in the
-- # list are valid, and all values in the list are the same (in which
-- # case, the message is processed with that single value used as the
-- # Content-Length field value).

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.6
-- # a recipient MUST anticipate potentially large decimal numerals and
-- # prevent parsing errors due to integer conversion overflows or
-- # precision loss due to integer conversion

-- | The length from every @Content-Length@ field line: each a list of
-- one or more decimal numbers, all the same, and none too large for 64
-- bits. "The same" is the same number, so @5, 05@ is 5: every reading
-- that accepts it then agrees on the length.
contentLength :: [FieldValue] -> Either FramingError Word64
contentLength vs = do
  elements <- concat <$> traverse (either (const (Left InvalidContentLength)) Right . splitList NoComments) vs
  case traverse number elements of
    Just (n : ns) | all (== n) ns -> Right n
    _                              -> Left InvalidContentLength
  where
    -- a decimal number that fits in 64 bits (splitList never returns an
    -- empty element)
    number e
      | not (B.all isDIGIT e) = Nothing
      | otherwise = B.foldl' step (Just 0) e
    step acc w = do
      n <- acc
      let d = fromIntegral (w - 0x30)
      if n > (maxBound - d) `div` 10 then Nothing else Just (n * 10 + d)

------------------------------------------------------------------------------
-- Responses
------------------------------------------------------------------------------

-- | What framing needs to know about a response's content: its length,
-- if known before the header section is sent, and whether it has a
-- trailer section. The length of a file comes from the file system
-- when the response is sent.
data Payload = Payload
  { payloadLength   :: !(Maybe Word64)
  , payloadTrailers :: !Bool
  } deriving (Eq, Show)

-- | How to send a response's body.
data ResponseFraming
  = HeaderOnly !(Maybe Word64)
    -- ^ no body: the response ends with its header section, with a
    -- Content-Length if one is given
  | SendLength !Word64
    -- ^ Content-Length, then exactly that many octets
  | SendChunked
    -- ^ Transfer-Encoding: chunked, then the chunks and the trailer section
  | SendUntilClose
    -- ^ neither field: the body ends when the server closes the connection
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.6
-- # A server MUST NOT send a Content-Length header field in any response
-- # with a status code of 1xx (Informational) or 204 (No Content).  A
-- # server MUST NOT send a Content-Length header field in any 2xx
-- # (Successful) response to a CONNECT request (Section 9.3.6).

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.1
-- # A server MUST NOT send a Transfer-Encoding header field in any
-- # response with a status code of 1xx (Informational) or 204 (No
-- # Content).  A server MUST NOT send a Transfer-Encoding header field in
-- # any 2xx (Successful) response to a CONNECT request (Section 9.3.6 of
-- # [HTTP]).

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.6
-- # A server MAY send a Content-Length header field in a 304 (Not
-- # Modified) response to a conditional GET request (Section 15.4.5); a
-- # server MUST NOT send Content-Length in such a response unless its
-- # field value equals the decimal number of octets that would have been
-- # sent in the content of a 200 (OK) response to the same request.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.6
-- # A server MAY send a Content-Length header field in a response to a
-- # HEAD request (Section 9.3.2); a server MUST NOT send Content-Length
-- # in such a response unless its field value equals the decimal number
-- # of octets that would have been sent in the content of a response if
-- # the same request had used the GET method.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.6
-- # Aside from the cases defined above, in the absence of Transfer-
-- # Encoding, an origin server SHOULD send a Content-Length header field
-- # when the content size is known prior to sending the complete header
-- # section.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.1
-- # A server MUST NOT send a response containing Transfer-
-- # Encoding unless the corresponding request indicates HTTP/1.1 (or
-- # later minor revisions).

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.3
-- # Since there is no way to distinguish a successfully completed, close-
-- # delimited response message from a partially received message
-- # interrupted by network failure, a server SHOULD generate encoding or
-- # length-delimited messages whenever possible.

-- | How to send the body of a response, from the version and method of
-- the request it answers, its status and its payload ('Nothing' for
-- 'Hyperdrive.Semantics.Message.ContentNone').
--
-- * A 204, a 2xx to CONNECT, and a 304 have no body and no
--   Content-Length. (A 304 could carry the length of the 200 it stands
--   for, but a 'Hyperdrive.Semantics.Message.Response' does not know
--   it.)
-- * A response to HEAD has no body, and a Content-Length if the length
--   is known: the response is built as it would be for GET, so that is
--   the length GET would send.
-- * Otherwise a known length without trailers is sent with
--   Content-Length. An unknown length, or trailers, need the chunked
--   coding, which an HTTP/1.0 client does not have: it gets
--   Content-Length when the length is known (and the trailers are
--   dropped), and otherwise a body that ends when the connection
--   closes.
responseFraming :: HttpVersion -> Method -> Status -> Maybe Payload -> ResponseFraming
responseFraming version method status payload
  | status == NoContent || status == NotModified = HeaderOnly Nothing
  | method == CONNECT, statusClass status == Successful = HeaderOnly Nothing
  | method == HEAD = HeaderOnly (payload >>= payloadLength)
  | otherwise = case payload of
      Nothing                    -> SendLength 0
      Just (Payload (Just n) False) -> SendLength n
      Just (Payload len _)
        | version >= HTTP_1_1    -> SendChunked
        | Just n <- len          -> SendLength n
        | otherwise              -> SendUntilClose

-- | The framing fields to send in the header section.
framingFields :: ResponseFraming -> [(FieldName, FieldValue)]
framingFields f = case f of
  HeaderOnly (Just n) -> [(hContentLength, decimal n)]
  SendLength n        -> [(hContentLength, decimal n)]
  SendChunked         -> [(hTransferEncoding, chunked)]
  _                   -> []
  where
    decimal = either (error "framingFields") id . mkFieldValue . C.pack . show
    chunked = either (error "framingFields") id (mkFieldValue "chunked")
