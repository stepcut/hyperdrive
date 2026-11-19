{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | Requests and responses (RFC 9110, Section 6)
--
-- These are the version-independent messages that applications see.
-- Each protocol layer (HTTP/1.1, HTTP/2, HTTP/3) turns what arrives on
-- the wire into a 'Request', and a 'Response' into what it sends, so an
-- application never sees which version of HTTP was used.
--
-- Fields that describe framing or the connection rather than the
-- message (@Content-Length@, @Transfer-Encoding@, @Connection@ and so
-- on) are not part of either: the protocol layer interprets them on the
-- way in and generates them on the way out, from the structure of the
-- message. So an application cannot, for example, send a
-- @Content-Length@ that disagrees with its content.
module Hyperdrive.Semantics.Message
  ( -- * Versions
    HttpVersion(..)
  , pattern HTTP_1_0
  , pattern HTTP_1_1
  , pattern HTTP_2
  , pattern HTTP_3
    -- * Requests
  , Request
  , RequestError(..)
  , mkRequest
  , requestVersion
  , requestMethod
  , requestTarget
  , requestHeaders
  , requestBody
  , requestAcceptsTrailers
  , setAcceptsTrailers
  , RequestBody(..)
  , IncompleteBody(..)
  , bytesBody
    -- * Responses
  , Response
  , ResponseError(..)
  , mkResponse
  , responseStatus
  , responseHeaders
  , responseContent
  , Content(..)
  , FileRange
  , mkFileRange
  , rangeOffset
  , rangeLength
  , checkResponseFor
  , TrailerError(..)
  , checkTrailers
    -- * Fields owned by the protocol layer
  , isProtocolField
  ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (find)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Word (Word64, Word8)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Internal (FieldName(..), Fields(..))
import Hyperdrive.Semantics.Method (Method, pattern CONNECT)
import Hyperdrive.Semantics.Status (Status, StatusClass(..), statusClass, pattern NoContent, pattern ResetContent, pattern NotModified, pattern UpgradeRequired)
import Hyperdrive.Semantics.Target (Target(..), normalizeEmptyPath, validForMethod)

------------------------------------------------------------------------------
-- Versions
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-2.5
-- # HTTP's version number consists of two decimal digits separated by a
-- # "." (period or decimal point).

-- | An HTTP version, major and minor. Part of a request's control data:
-- some requirements depend on it, such as never sending a 1xx response
-- to an HTTP/1.0 client.
data HttpVersion = HttpVersion !Word8 !Word8
  deriving (Eq, Ord, Show)

pattern HTTP_1_0, HTTP_1_1, HTTP_2, HTTP_3 :: HttpVersion
pattern HTTP_1_0 = HttpVersion 1 0
pattern HTTP_1_1 = HttpVersion 1 1
pattern HTTP_2   = HttpVersion 2 0
pattern HTTP_3   = HttpVersion 3 0

------------------------------------------------------------------------------
-- Fields owned by the protocol layer
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.6.1
-- # Furthermore, intermediaries SHOULD remove or replace fields that are
-- # known to require removal before forwarding, whether or not they
-- # appear as a connection-option, after applying those fields'
-- # semantics.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.6.1
-- # Note that some versions of HTTP prohibit the use of fields for such
-- # information, and therefore do not allow the Connection field.

-- | Fields that describe the framing of a message or the connection it
-- travels on, rather than the message itself: @Connection@,
-- @Keep-Alive@, @Proxy-Connection@, @TE@, @Transfer-Encoding@,
-- @Upgrade@ and @Content-Length@. They are interpreted and generated
-- by the protocol layer, and never appear in a 'Request' or 'Response'.
isProtocolField :: FieldName -> Bool
isProtocolField (FieldName n) = n `elem` protocolFields

protocolFields :: [ByteString]
protocolFields =
  [ "connection", "keep-alive", "proxy-connection", "te"
  , "transfer-encoding", "upgrade", "content-length" ]

firstProtocolField :: Fields -> Maybe FieldName
firstProtocolField (Fields fs) = fst <$> find (isProtocolField . fst) fs

------------------------------------------------------------------------------
-- Requests
------------------------------------------------------------------------------

-- | The content of a request did not arrive in full: the client
-- disconnected or reset the stream, or sent less or more than it
-- declared. Thrown by 'readChunk' and 'readTrailers'.
data IncompleteBody = IncompleteBody
  deriving (Eq, Show)

instance Exception IncompleteBody

-- | The content of a request, read as it arrives. A body has a single
-- reader: reading it from more than one thread at a time is an error.
--
-- The guarantees every protocol layer gives:
--
-- * 'readChunk' returns an empty chunk only after the whole content has
--   arrived. If it does not arrive in full, 'readChunk' throws
--   'IncompleteBody', never an empty chunk, so a truncated upload is
--   never mistaken for a complete one.
--
-- * If 'bodyLength' is @Just n@, exactly @n@ bytes are delivered, or
--   'IncompleteBody' is thrown.
data RequestBody = RequestBody
  { -- | The length the client declared in advance, if it did. It comes
    -- from the client, so it is not a reason to allocate that much
    -- memory: compare it with your own limit first.
    bodyLength   :: !(Maybe Word64)
    -- | The next chunk of content. An empty chunk means the whole
    -- content has arrived, and every later call also returns an empty
    -- chunk.
  , readChunk    :: IO ByteString
    -- | The trailer section. Any content not yet read is read and
    -- discarded first, which can throw 'IncompleteBody'. Trailers are
    -- never merged into the header section (RFC 9110 Section 6.5.1,
    -- RFC 9112 Section 7.1.2).
  , readTrailers :: IO Fields
  }

-- | A body consisting of the given bytes, with no trailers. For tests,
-- and for code that builds requests itself.
bytesBody :: ByteString -> IO RequestBody
bytesBody bs = do
  ref <- newIORef bs
  let next = atomicModifyIORef' ref (\c -> (B.empty, c))
  pure RequestBody
    { bodyLength   = Just (fromIntegral (B.length bs))
    , readChunk    = next
    , readTrailers = next >> pure emptyFields
    }

-- | A request, as an application sees it.
--
-- Invariants: the form of the target is valid for the method, an empty
-- path has its normal form, and the header fields contain no protocol
-- fields ('isProtocolField') and no @Host@. The authority the request
-- is for is in the target and nowhere else, so routing and the
-- application cannot act on two different hosts.
data Request = Request
  { requestVersion :: !HttpVersion
  , requestMethod  :: !Method
  , requestTarget  :: !Target
  , requestHeaders :: !Fields
  , requestBody    :: !RequestBody
    -- | Whether the client said it will not discard trailer fields in
    -- the response: @TE: trailers@ in HTTP/1.1, HTTP/2 and HTTP/3.
    -- Declared trailers are sent either way; an application whose
    -- trailers are necessary to the client should not rely on them
    -- when this is 'False'. 'mkRequest' sets it to 'False'; the
    -- protocol layer sets it with 'setAcceptsTrailers'.
  , requestAcceptsTrailers :: !Bool
  }

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.5.1
-- # The presence of the keyword "trailers" in the TE header field
-- # (Section 10.1.4) of a request indicates that the client is willing to
-- # accept trailer fields, on behalf of itself and any downstream
-- # clients.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.5.1
-- # Because of the potential for trailer fields to be discarded in
-- # transit, a server SHOULD NOT generate trailer fields that it believes
-- # are necessary for the user agent to receive.

-- | Record whether the client accepts trailer fields. Only the
-- application knows whether its trailers are necessary to the client,
-- so the protocol layer tells it, rather than dropping them
-- (docs/http1-core-design.md, D9).
setAcceptsTrailers :: Bool -> Request -> Request
setAcceptsTrailers b r = r { requestAcceptsTrailers = b }

data RequestError
  = TargetNotValidForMethod     -- ^ the CONNECT or asterisk form used with another method, or an
                                -- HTTP/1.x CONNECT without the CONNECT form
  | RequestProtocolField !FieldName -- ^ a field the protocol layer should have removed
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.1
-- # These forms MUST
-- # NOT be used with other methods.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2.3
-- # The "authority-form" of request-target is only used for CONNECT
-- # requests (Section 9.3.6 of [HTTP]).

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.2
-- # The target URI's authority information is critical for handling a
-- # request.

-- RFC 9112, Section 3.2.2 (not yet in duvet's specifications): with an
-- absolute-form request-target, "the origin server MUST ignore the
-- received Host header field (if any)".

-- | Build a request. The protocol layer has already reconstructed the
-- target and interpreted the protocol fields, expressing what they say
-- as the version, the target and the body. @Host@ (or HTTP/2 and
-- HTTP/3's @:authority@) has been folded into the target, and is
-- rejected here: leaving it in the fields would let an application
-- read a host that routing did not use, as when an absolute-form
-- target and @Host@ disagree.
--
-- A CONNECT request with an ordinary target URI (an "extended CONNECT",
-- RFC 8441 and RFC 9220) only exists in HTTP/2 and HTTP/3. In HTTP/1.x
-- CONNECT always uses the CONNECT form, so anything else is rejected.
mkRequest :: HttpVersion -> Method -> Target -> Fields -> RequestBody -> Either RequestError Request
mkRequest version method target fields body
  | not (validForMethod method target)   = Left TargetNotValidForMethod
  | method == CONNECT, version < HTTP_2, not (isConnectForm target) = Left TargetNotValidForMethod
  | Just n <- firstProtocolField fields  = Left (RequestProtocolField n)
  | not (null (lookupAll hHost fields))  = Left (RequestProtocolField hHost)
  | otherwise = Right (Request version method (normalizeEmptyPath method target) fields body False)
  where
    isConnectForm TargetConnect{} = True
    isConnectForm _               = False

------------------------------------------------------------------------------
-- Responses
------------------------------------------------------------------------------

-- | A range of bytes in a file: the offset of the first byte and the
-- number of bytes. Invariant: the end does not overflow.
data FileRange = FileRange { rangeOffset :: !Word64, rangeLength :: !Word64 }
  deriving (Eq, Show)

-- | A range from an offset and a length, or 'Nothing' if its end would
-- overflow. Whether the range lies within the file can only be checked
-- when it is sent, since the file can change in the meantime.
mkFileRange :: Word64 -> Word64 -> Maybe FileRange
mkFileRange off len
  | len <= maxBound - off = Just (FileRange off len)
  | otherwise             = Nothing

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.4.1
-- # All other responses do include content, although that content might
-- # be of zero length.

-- | The content of a response.
--
-- 'ContentNone' (no content at all, as for 204 and 304) is different
-- from empty content (@'ContentBytes' ""@, content of zero length, as
-- for 205).
data Content
  = ContentNone
  | ContentBytes !ByteString
    -- | Part or all of a file, so that the IO layer can send it without
    -- copying it through user space. The path is chosen by the server,
    -- so any part that came from a request must have been checked with
    -- "Hyperdrive.Semantics.SafePath". The protocol layer sends exactly
    -- the length it announced: if the file has changed size by the time
    -- it is read, or the range lies outside it, the response is aborted
    -- rather than sending more or less.
  | ContentFile !FilePath !(Maybe FileRange)
    -- | Content produced as it is sent: its length, if known, the names
    -- of the fields it may send in its trailer section, and a function
    -- that is given a way to write a chunk and a way to flush what has
    -- been written, and returns the trailer section once it has written
    -- all the content. If a length is given, exactly that many bytes
    -- must be written.
    --
    -- The trailer names are declared up front so that the protocol
    -- layer can announce them in a @Trailer@ header field (RFC 9110
    -- Section 6.6.2), and so that 'mkResponse' can check each one: only
    -- fields whose definitions permit them in trailers may be declared,
    -- which for a field not defined by RFC 9110 the application must
    -- know (Section 6.5.1). The trailer section returned must contain
    -- only declared fields ('checkTrailers').
    --
    -- If the function throws, or writes more or less than the length it
    -- gave, the protocol layer aborts the response: it closes the
    -- connection or resets the stream, and never ends the content
    -- normally, so that a truncated response cannot pass for a
    -- complete one. Bytes beyond the declared length are never sent,
    -- since on an HTTP/1.1 connection a client would take them for the
    -- start of the next response. 'write' throws if the client has gone.
  | ContentStream !(Maybe Word64) ![FieldName] ((ByteString -> IO ()) -> IO () -> IO Fields)

-- | A final response, as an application produces it.
--
-- Invariants: the status is not 1xx or 426; there is no content
-- exactly when the status is 204 or 304, and the content of a 205 is
-- empty; the header fields contain no protocol fields
-- ('isProtocolField') and no repeated singleton; and a stream declares
-- only fields that can be trailers, none of them a singleton also in
-- the header fields.
data Response = Response
  { responseStatus  :: !Status
  , responseHeaders :: !Fields
  , responseContent :: !Content
  }

data ResponseError
  = InterimStatus                     -- ^ a 1xx status, which is not a final response
  | ContentNotAllowed                 -- ^ content with a 204 or 304 status, or content that is not empty bytes with a 205
  | ContentRequired                   -- ^ no content with any other status (a 205 needs empty content)
  | ResponseProtocolField !FieldName  -- ^ a field only the protocol layer may generate
  | RepeatedSingleton !FieldName      -- ^ a field that may only appear once, appearing more than once
  | NotATrailer !FieldName            -- ^ a declared trailer field that cannot be sent in trailers
  | UpgradeNotSupported               -- ^ a 426 status, which must carry an Upgrade field
  | TunnelNotSupported                -- ^ a 2xx response to CONNECT, which would open a tunnel
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.3
-- # Recipients often
-- # attempt to handle this error by using the last syntactically valid
-- # member of the list, leading to potential interoperability and
-- # security issues if different implementations have different error
-- # handling behaviors.

-- | Response fields whose grammar allows a single value, so that a
-- second field line could only make clients and caches disagree about
-- which one applies. From RFC 9110: Content-Type, Content-Range,
-- Content-Location, Location, ETag, Last-Modified, Retry-After, Date and
-- Server; from RFC 9111: Age and Expires.
singletonResponseFields :: [FieldName]
singletonResponseFields = map FieldName
  [ "content-type", "content-range", "content-location", "location", "etag"
  , "last-modified", "retry-after", "date", "server", "age", "expires" ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.2
-- # A 1xx response is terminated by the end of the header section; it
-- # cannot contain content or trailers.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.3.5
-- # A 204 response is terminated by the end of the header section; it
-- # cannot contain content or trailers.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.3.6
-- # Since the 205 status code implies that no additional content will be
-- # provided, a server MUST NOT generate content in a 205 response.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.4.1
-- # All other responses do include content, although that content might
-- # be of zero length.

-- RFC 9112 Section 6.3 ends only HEAD, 1xx, 204 and 304 responses at
-- the header section. A 205 has content of zero length, which HTTP/1.1
-- must frame (Content-Length: 0), or a client reads until the
-- connection closes. So it is empty bytes, not 'ContentNone'.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.5.22
-- # The server MUST send an Upgrade header field in a
-- # 426 response to indicate the required protocol(s) (Section 7.8).

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.4.5
-- # A 304 response is terminated by the end of the header section; it
-- # cannot contain content or trailers.

-- | Build a final response. Interim (1xx) responses are not 'Response's.
--
-- A response to HEAD is built as it would be for GET, content and all:
-- the protocol layer sends the fields that describe the content, such
-- as its length, and not the content itself.
--
-- A 426 (Upgrade Required) is rejected: it must carry an @Upgrade@
-- field naming the protocols to upgrade to, which is a protocol field,
-- and upgrading is not supported yet.
mkResponse :: Status -> Fields -> Content -> Either ResponseError Response
mkResponse status fields content
  | statusClass status == Informational = Left InterimStatus
  | status == UpgradeRequired           = Left UpgradeNotSupported
  | Just n <- firstProtocolField fields  = Left (ResponseProtocolField n)
  | (n : _) <- repeated                  = Left (RepeatedSingleton n)
  | status == ResetContent              = case content of
      ContentBytes b | B.null b -> ok
      ContentNone               -> Left ContentRequired
      _                         -> Left ContentNotAllowed
  | noContent, not (isNone content)     = Left ContentNotAllowed
  | not noContent, isNone content       = Left ContentRequired
  | (n : _) <- notTrailers               = Left (NotATrailer n)
  | (n : _) <- alsoInHeaders             = Left (RepeatedSingleton n)
  | otherwise                           = ok
  where
    ok = Right (Response status fields content)
    noContent = status `elem` [NoContent, NotModified]
    isNone ContentNone = True
    isNone _           = False
    repeated = [ n | n <- singletonResponseFields, length (take 2 (lookupAll n fields)) > 1 ]
    declared = case content of
      ContentStream _ names _ -> names
      _                       -> []
    notTrailers = filter (not . canBeTrailer) declared
    alsoInHeaders = [ n | n <- declared, n `elem` singletonResponseFields, not (null (lookupAll n fields)) ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.3.6
-- # Any 2xx (Successful) response indicates that the sender (and all
-- # inbound proxies) will switch to tunnel mode immediately after the
-- # response header section; data received after that header section is
-- # from the server identified by the request target.

-- | Check a response against the request it answers. The protocol layer
-- calls this before sending any response.
--
-- A 2xx response to CONNECT is rejected: a client takes it to mean the
-- connection is now a tunnel, and would treat whatever follows as
-- tunnelled data rather than HTTP. Tunnels are not supported yet.
checkResponseFor :: Request -> Response -> Either ResponseError Response
checkResponseFor req resp
  | requestMethod req == CONNECT, statusClass (responseStatus resp) == Successful = Left TunnelNotSupported
  | otherwise = Right resp

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.5.1
-- # Many fields cannot be processed outside the header section because
-- # their evaluation is necessary prior to receiving the content, such as
-- # those that describe message framing, routing, authentication, request
-- # modifiers, response controls, or content format.  A sender MUST NOT
-- # generate a trailer field unless the sender knows the corresponding
-- # header field name's definition permits the field to be sent in
-- # trailers.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.3
-- # This means that, aside from the well-known exception noted below, a
-- # sender MUST NOT generate multiple field lines with the same name in a
-- # message (whether in the headers or trailers) or append a field line
-- # when a field line of the same name already exists in the message,
-- # unless that field's definition allows multiple field line values to
-- # be recombined as a comma-separated list

data TrailerError
  = UndeclaredTrailer !FieldName  -- ^ a field the stream did not declare
  | RepeatedTrailer !FieldName    -- ^ a singleton field, given more than once
  deriving (Eq, Show)

-- | Check the trailer section a 'ContentStream' returned, against the
-- response it ends. The protocol layer calls this before sending
-- trailers.
--
-- Every field must be one the stream declared (which 'mkResponse' has
-- checked can be a trailer), and a singleton field may appear only
-- once: 'mkResponse' has already made sure it is not also among the
-- header fields.
checkTrailers :: Response -> Fields -> Either TrailerError Fields
checkTrailers resp trailers
  | (n : _) <- undeclared = Left (UndeclaredTrailer n)
  | (n : _) <- repeated   = Left (RepeatedTrailer n)
  | otherwise             = Right trailers
  where
    names = map fst (fieldsToList trailers)
    declared = case responseContent resp of
      ContentStream _ ns _ -> ns
      _                    -> []
    undeclared = filter (`notElem` declared) names
    repeated = [ n | n <- singletonResponseFields, length (take 2 (lookupAll n trailers)) > 1 ]

-- | Whether a field can be declared as a trailer: not a protocol field,
-- and not one of the fields RFC 9110 rules out, those that describe
-- framing, routing, authentication, request modifiers, response
-- controls or content format. RFC 9110 permits @ETag@ (Section 8.8.3)
-- and @Accept-Ranges@ (Section 14.3) in trailers outright, and
-- @Authentication-Info@ (Section 11.6.3) when the authentication scheme
-- allows it; for any other field, declaring it says the application
-- knows its definition permits it.
canBeTrailer :: FieldName -> Bool
canBeTrailer n@(FieldName b) = not (isProtocolField n) && b `notElem` notTrailerFields

notTrailerFields :: [ByteString]
notTrailerFields =
  [ "trailer", "host"                                                  -- framing, routing
  , "authorization", "proxy-authorization"                             -- authentication
  , "www-authenticate", "proxy-authenticate"
  , "expect", "max-forwards", "range", "if-match", "if-none-match"     -- request modifiers
  , "if-modified-since", "if-unmodified-since", "if-range"
  , "cache-control", "expires", "age", "vary", "location"              -- response controls
  , "retry-after", "set-cookie"
  , "content-type", "content-encoding", "content-language"             -- content format
  , "content-range", "content-location" ]
