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
  , RequestBody(..)
  , bytesBody
    -- * Responses
  , Response
  , ResponseError(..)
  , mkResponse
  , responseStatus
  , responseHeaders
  , responseContent
  , Content(..)
  , FileRange(..)
    -- * Fields owned by the protocol layer
  , isProtocolField
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (find)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64, Word8)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Internal (FieldName(..))
import Hyperdrive.Semantics.Method (Method)
import Hyperdrive.Semantics.Status (Status, StatusClass(..), statusClass, pattern NoContent, pattern ResetContent, pattern NotModified)
import Hyperdrive.Semantics.Target (Target, normalizeEmptyPath, validForMethod)

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
firstProtocolField = find isProtocolField . map fst . fieldsToList

------------------------------------------------------------------------------
-- Requests
------------------------------------------------------------------------------

-- | The content of a request, read as it arrives.
data RequestBody = RequestBody
  { -- | the length of the content, if it was declared in advance
    bodyLength   :: !(Maybe Word64)
    -- | the next chunk of content; an empty chunk means the end, and
    -- every later call also returns an empty chunk
  , readChunk    :: IO ByteString
    -- | the trailer section. Any content not yet read is read and
    -- discarded first.
  , readTrailers :: IO Fields
  }

-- | A body consisting of the given bytes, with no trailers. For tests,
-- and for code that builds requests itself.
bytesBody :: ByteString -> IO RequestBody
bytesBody bs = do
  ref <- newIORef bs
  let next = do c <- readIORef ref; writeIORef ref B.empty; pure c
  pure RequestBody
    { bodyLength   = Just (fromIntegral (B.length bs))
    , readChunk    = next
    , readTrailers = next >> pure emptyFields
    }

-- | A request, as an application sees it.
--
-- Invariants: the form of the target is valid for the method, an empty
-- path has its normal form, and the header fields contain no protocol
-- fields ('isProtocolField').
data Request = Request
  { requestVersion :: !HttpVersion
  , requestMethod  :: !Method
  , requestTarget  :: !Target
  , requestHeaders :: !Fields
  , requestBody    :: !RequestBody
  }

data RequestError
  = TargetNotValidForMethod     -- ^ the CONNECT or asterisk form, used with another method
  | RequestProtocolField !FieldName -- ^ a field the protocol layer should have removed
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.1
-- # These forms MUST
-- # NOT be used with other methods.

-- | Build a request. The protocol layer has already reconstructed the
-- target and interpreted the protocol fields, expressing what they say
-- as the version, the target and the body.
mkRequest :: HttpVersion -> Method -> Target -> Fields -> RequestBody -> Either RequestError Request
mkRequest version method target fields body
  | not (validForMethod method target)   = Left TargetNotValidForMethod
  | Just n <- firstProtocolField fields  = Left (RequestProtocolField n)
  | otherwise = Right (Request version method (normalizeEmptyPath method target) fields body)

------------------------------------------------------------------------------
-- Responses
------------------------------------------------------------------------------

-- | A range of bytes in a file: the offset of the first byte and the
-- number of bytes.
data FileRange = FileRange { rangeOffset :: !Word64, rangeLength :: !Word64 }
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.4.1
-- # All other responses do include content, although that content might
-- # be of zero length.

-- | The content of a response.
--
-- 'ContentNone' (no content at all, as for 204) is different from empty
-- content (@'ContentBytes' ""@, content of zero length).
data Content
  = ContentNone
  | ContentBytes !ByteString
    -- | Part or all of a file, so that the IO layer can send it without
    -- copying it through user space. The path is chosen by the server,
    -- so any part that came from a request must have been checked with
    -- "Hyperdrive.Semantics.SafePath".
  | ContentFile !FilePath !(Maybe FileRange)
    -- | Content produced as it is sent. The function is given a way to
    -- write a chunk and a way to flush what has been written, and
    -- returns the trailer section once it has written all the content.
    -- If a length is given, exactly that many bytes must be written.
  | ContentStream !(Maybe Word64) ((ByteString -> IO ()) -> IO () -> IO Fields)

-- | A final response, as an application produces it.
--
-- Invariants: the status is not 1xx, there is no content exactly when
-- the status says there cannot be, and the header fields contain no
-- protocol fields ('isProtocolField').
data Response = Response
  { responseStatus  :: !Status
  , responseHeaders :: !Fields
  , responseContent :: !Content
  }

data ResponseError
  = InterimStatus                     -- ^ a 1xx status, which is not a final response
  | ContentNotAllowed                 -- ^ content with a 204, 205 or 304 status
  | ContentRequired                   -- ^ no content with any other status
  | ResponseProtocolField !FieldName  -- ^ a field only the protocol layer may generate
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.2
-- # A 1xx response is terminated by the end of the header section; it
-- # cannot contain content or trailers.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.3.5
-- # A 204 response is terminated by the end of the header section; it
-- # cannot contain content or trailers.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.3.6
-- # Since the 205 status code implies that no additional content will be
-- # provided, a server MUST NOT generate content in a 205 response.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.4.5
-- # A 304 response is terminated by the end of the header section; it
-- # cannot contain content or trailers.

-- | Build a final response. Interim (1xx) responses are not 'Response's.
--
-- A response to HEAD is built as it would be for GET, content and all:
-- the protocol layer sends the fields that describe the content, such
-- as its length, and not the content itself.
mkResponse :: Status -> Fields -> Content -> Either ResponseError Response
mkResponse status fields content
  | statusClass status == Informational = Left InterimStatus
  | Just n <- firstProtocolField fields  = Left (ResponseProtocolField n)
  | noContent, not (isNone content)     = Left ContentNotAllowed
  | not noContent, isNone content       = Left ContentRequired
  | otherwise                           = Right (Response status fields content)
  where
    noContent = status `elem` [NoContent, ResetContent, NotModified]
    isNone ContentNone = True
    isNone _           = False
