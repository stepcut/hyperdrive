{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | Requests and responses as the field lists of HTTP/2 and HTTP/3:
-- control data in pseudo-header fields (RFC 9113, Section 8.3; RFC
-- 9114, Section 4.3, which repeats it), the field validity rules of RFC
-- 9113 Section 8.2, and the mapping to and from 'Request' and
-- 'Response' (docs/http2-core-design.md, D11).
--
-- A request that breaks a rule is either malformed (RFC 9113 Section
-- 8.1.1: answered with 400, then the stream is reset) or well formed
-- but refused (answered with a status, as HTTP/1.1 answers the same
-- request; D10).
module Hyperdrive.Semantics.ControlData
  ( RequestControl(..)
  , ControlDataError(..)
  , Malformation(..)
  , Refusal(..)
  , requestFromFields
  , trailersFromFields
  , responseToFields
  , trailersToFields
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Word (Word64)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.FieldSyntax (parseContentLength)
import Hyperdrive.Semantics.Internal (FieldName(..), FieldValue(..))
import Hyperdrive.Semantics.Method (Method, mkMethod, pattern CONNECT)
import Hyperdrive.Semantics.Status (Status, statusCode, pattern BadRequest, pattern MisdirectedRequest)
import Hyperdrive.Semantics.Syntax (isFieldVChar, isToken)
import Hyperdrive.Semantics.Target
import Hyperdrive.Semantics.URI

-- | What a request's field list gives 'Hyperdrive.Semantics.Message.mkRequest':
-- the method, the target, the header fields (without pseudo-header,
-- protocol or @Host@ fields), and what the protocol layer keeps: the
-- length from @Content-Length@, which the DATA must match (RFC 9113
-- Section 8.1.1), and whether @TE: trailers@ was sent.
data RequestControl = RequestControl
  { controlMethod          :: !Method
  , controlTarget          :: !Target
  , controlFields          :: !Fields
  , controlContentLength   :: !(Maybe Word64)
  , controlAcceptsTrailers :: !Bool
  } deriving (Show)

data ControlDataError
  = Malformed !Malformation       -- ^ 400, then RST_STREAM PROTOCOL_ERROR (D10's case 2)
  | Refused !Status !Refusal      -- ^ this status, a normal response (D10's case 3)
  deriving (Eq, Show)

-- | What makes a message malformed (RFC 9113 Section 8). Each carries
-- the field's name where there is one.
data Malformation
  = BadFieldName !ByteString           -- ^ not a token, or with upper case (Section 8.2.1)
  | BadFieldValue !ByteString          -- ^ a value outside field-value (Section 8.2.1)
  | UnknownPseudoHeader !ByteString    -- ^ Section 8.3
  | RepeatedPseudoHeader !ByteString   -- ^ Section 8.3
  | PseudoHeaderAfterField !ByteString -- ^ Section 8.3
  | MissingPseudoHeader !ByteString    -- ^ Section 8.3.1
  | InvalidPseudoHeader !ByteString    -- ^ not a valid value (Section 8.3.1)
  | ConnectWithSchemeOrPath            -- ^ Section 8.5
  | ConnectionSpecificField !ByteString -- ^ Section 8.2.2
  | InvalidAuthority !AuthorityError   -- ^ @:authority@ is not @uri-host [ ":" port ]@ (Section 8.3)
  | TENotTrailers                      -- ^ Section 8.2.2
  | HostDiffersFromAuthority           -- ^ Section 8.3.1 (D11)
  | PseudoHeaderInTrailers !ByteString -- ^ Section 8.1
  | FramingFieldInTrailers !ByteString -- ^ refused as HTTP/1.1 refuses it
  deriving (Eq, Show)

-- | Why a well-formed request is refused, as HTTP/1.1 would refuse it.
data Refusal
  = InvalidHost !AuthorityError      -- ^ @Host@ is not @uri-host [ ":" port ]@
  | RepeatedHost                     -- ^ more than one @Host@ field line
  | NoAuthority                      -- ^ neither, and no default
  | UnsupportedIPvFuture             -- ^ a host this server cannot serve
  | MissingConnectPort               -- ^ a CONNECT authority without a port
  | AsteriskNotOptions               -- ^ @:path@ @*@ with a method other than OPTIONS
  | InvalidContentLength
  | MisdirectedScheme !Scheme        -- ^ a scheme this connection does not serve (421)
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Field validity
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.1
-- # HTTP/2
-- # implementations SHOULD validate field names and values according to
-- # their definitions in Sections 5.1 and 5.5 of [HTTP], respectively,
-- # and treat messages that contain prohibited characters as malformed
-- # (Section 8.1.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.1
-- # A field name MUST NOT contain characters in the ranges 0x00-0x20,
-- #    0x41-0x5a, or 0x7f-0xff (all ranges inclusive).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.1
-- # With the exception of pseudo-header fields (Section 8.3), which
-- #    have a name that starts with a single colon, field names MUST NOT
-- #    include a colon (ASCII COLON, 0x3a).
--
-- A token (RFC 9110 Section 5.1) has none of those, nor a colon; what
-- is left to check is upper case.
validName :: ByteString -> Bool
validName n = isToken n && not (B.any (\w -> w >= 0x41 && w <= 0x5A) n)

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.1
-- # A field value MUST NOT contain the zero value (ASCII NUL, 0x00),
-- #    line feed (ASCII LF, 0x0a), or carriage return (ASCII CR, 0x0d) at
-- #    any position.
--
-- field-value (RFC 9110 Section 5.5) without the whitespace around it:
-- field-vchar, and SP or HTAB between, which excludes all of those.
validValue :: ByteString -> Bool
validValue = B.all (\w -> isFieldVChar w || w == 0x20 || w == 0x09)

-- | Whitespace around a regular field's value is removed, as HTTP/1.1
-- removes it, rather than making the message malformed as RFC 9113
-- Section 8.2.1 says ("A field value MUST NOT start or end with an
-- ASCII whitespace character"): of nine implementations surveyed only
-- h2o refuses it, and enforcing it broke real deployments
-- (docs/http2-core-design.md, D11). A pseudo-header field's value is
-- not stripped: whitespace makes it invalid by its own syntax.
stripWhitespace :: ByteString -> ByteString
stripWhitespace = B.dropWhileEnd ws . B.dropWhile ws
  where ws w = w == 0x20 || w == 0x09

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.1
-- # A request or response that contains a field that violates any of
-- # these conditions MUST be treated as malformed (Section 8.1.1).

isPseudo :: ByteString -> Bool
isPseudo n = not (B.null n) && B.head n == 0x3A

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.2
-- # An endpoint MUST NOT generate an HTTP/2 message containing
-- # connection-specific header fields.  This includes the Connection
-- # header field and those listed as having connection-specific semantics
-- # in Section 7.6.1 of [HTTP] (that is, Proxy-Connection, Keep-Alive,
-- # Transfer-Encoding, and Upgrade).  Any message containing connection-
-- # specific header fields MUST be treated as malformed (Section 8.1.1).
connectionSpecific :: ByteString -> Bool
connectionSpecific n =
  n `elem` ["connection", "proxy-connection", "keep-alive", "transfer-encoding", "upgrade"]

------------------------------------------------------------------------------
-- Requests
------------------------------------------------------------------------------

-- the pseudo-header fields of a request, and the regular fields sorted
data Parts = Parts
  { pMethod, pScheme, pAuthority, pPath :: !(Maybe ByteString)
  , pFields  :: [(ByteString, ByteString)]  -- ^ kept for the application, last first
  , pCookies :: [ByteString]                -- ^ last first
  , pHosts   :: [ByteString]
  , pLengths :: [ByteString]
  , pTE      :: !Bool
  }

noParts :: Parts
noParts = Parts Nothing Nothing Nothing Nothing [] [] [] [] False

-- | The request a field list (a HEADERS frame's, decoded) describes,
-- given the connection's scheme and default authority.
requestFromFields :: TargetContext -> [(ByteString, ByteString)] -> Either ControlDataError RequestControl
requestFromFields ctx list = do
  parts <- either (Left . Malformed) Right (sortFields False noParts list)
  method <- malformed (required ":method" (pMethod parts) >>= valid ":method" mkMethod)
  if method == CONNECT then connect ctx method parts else ordinary ctx method parts
  where malformed = either (Left . Malformed) Right

required :: ByteString -> Maybe ByteString -> Either Malformation ByteString
required name = maybe (Left (MissingPseudoHeader name)) Right

valid :: ByteString -> (ByteString -> Maybe a) -> ByteString -> Either Malformation a
valid name f v = maybe (Left (InvalidPseudoHeader name)) Right (f v)

-- One pass over the list, in order: pseudo-header fields first, then
-- regular ones, each checked.
sortFields :: Bool -> Parts -> [(ByteString, ByteString)] -> Either Malformation Parts
sortFields _ parts [] = Right parts
sortFields regularSeen parts ((n, v) : rest)
  | isPseudo n = do
      -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3
      -- # All pseudo-header fields MUST appear in a field block before all
      -- # regular field lines.  Any request or response that contains a pseudo-
      -- # header field that appears in a field block after a regular field line
      -- # MUST be treated as malformed (Section 8.1.1).
      if regularSeen then Left (PseudoHeaderAfterField n) else Right ()
      if validPseudoValue v then Right () else Left (BadFieldValue n)
      parts' <- pseudo n v parts
      sortFields False parts' rest
  | not (validName n) = Left (BadFieldName n)
  | not (validValue v') = Left (BadFieldValue n)
  | connectionSpecific n = Left (ConnectionSpecificField n)
  | otherwise = do
      parts' <- regular n v' parts
      sortFields True parts' rest
  where
    v' = stripWhitespace v
    -- a pseudo-header field's value is checked by its own syntax below;
    -- here only for what Section 8.2.1 prohibits in any value
    validPseudoValue x = validValue x

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3
-- # Endpoints MUST treat a request or response that
-- # contains undefined or invalid pseudo-header fields as malformed
-- # (Section 8.1.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3
-- # The same pseudo-header field name MUST NOT appear more than once in a
-- # field block.  A field block for an HTTP request or response that
-- # contains a repeated pseudo-header field name MUST be treated as
-- # malformed (Section 8.1.1).
pseudo :: ByteString -> ByteString -> Parts -> Either Malformation Parts
pseudo n v parts = case n of
  ":method"    -> once pMethod (\x -> parts { pMethod = x })
  ":scheme"    -> once pScheme (\x -> parts { pScheme = x })
  ":authority" -> once pAuthority (\x -> parts { pAuthority = x })
  ":path"      -> once pPath (\x -> parts { pPath = x })
  _            -> Left (UnknownPseudoHeader n)
  where
    once get set = case get parts of
      Just _  -> Left (RepeatedPseudoHeader n)
      Nothing -> Right (set (Just v))

regular :: ByteString -> ByteString -> Parts -> Either Malformation Parts
regular n v parts = case n of
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.2
  -- # The only exception to this is the TE header field, which MAY be
  -- # present in an HTTP/2 request; when it is, it MUST NOT contain any
  -- # value other than "trailers".
  "te" | C.map toLower v == "trailers" -> Right parts { pTE = True }
       | otherwise -> Left TENotTrailers
  "host" -> Right parts { pHosts = v : pHosts parts }
  "content-length" -> Right parts { pLengths = v : pLengths parts }
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.3
  -- # If there are multiple Cookie header fields after
  -- # decompression, these MUST be concatenated into a single octet string
  -- # using the two-octet delimiter of 0x3b, 0x20 (the ASCII string "; ")
  -- # before being passed into a non-HTTP/2 context, such as an HTTP/1.1
  -- # connection, or a generic HTTP server application.
  "cookie" -> Right parts
    { pCookies = v : pCookies parts
      -- the joined field takes the place of the first
    , pFields = if null (pCookies parts) then ("cookie", "") : pFields parts else pFields parts }
  _ -> Right parts { pFields = (n, v) : pFields parts }

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.5
-- # *  The ":scheme" and ":path" pseudo-header fields MUST be omitted.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.5
-- # A CONNECT request that does not conform to these restrictions is
-- # malformed (Section 8.1.1).
connect :: TargetContext -> Method -> Parts -> Either ControlDataError RequestControl
connect _ method parts = do
  case (pScheme parts, pPath parts) of
    (Nothing, Nothing) -> Right ()
    _ -> Left (Malformed ConnectWithSchemeOrPath)
  a <- either (Left . Malformed) Right (required ":authority" (pAuthority parts))
  authority <- pseudoAuthority a
  _ <- hostAgrees Nothing authority parts
  _ <- servable authority
  target <- maybe (Left (Refused BadRequest MissingConnectPort)) Right (connectTarget authority)
  finish method target parts

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3.1
-- # All HTTP/2 requests MUST include exactly one valid value for the
-- # ":method", ":scheme", and ":path" pseudo-header fields, unless they
-- # are CONNECT requests (Section 8.5).  An HTTP request that omits
-- # mandatory pseudo-header fields is malformed (Section 8.1.1).
ordinary :: TargetContext -> Method -> Parts -> Either ControlDataError RequestControl
ordinary ctx method parts = do
  scheme <- malformed (required ":scheme" (pScheme parts) >>= valid ":scheme" mkScheme)
  p <- malformed (required ":path" (pPath parts))
  form <- malformed (pathForm p)
  -- the authority: from :authority, else Host, else the default
  authority <- case pAuthority parts of
    Just a -> do
      authority <- pseudoAuthority a
      hostAgrees (Just scheme) authority parts
    Nothing -> case pHosts parts of
      [] -> fromDefault
      [h] | B.null h -> fromDefault
          | otherwise -> refuse InvalidHost (parseAuthority h)
      (_ : _ : _) -> Left (Refused BadRequest RepeatedHost)
  servable authority
  -- = https://www.rfc-editor.org/rfc/rfc9110#section-7.4
  -- # The 421 (Misdirected Request) status code in a response indicates
  -- # that the origin server has rejected the request because it appears to
  -- # have been misdirected (Section 15.5.20).
  --
  -- As for HTTP/1.1 (design-notes P3): only http and https, and https
  -- only on a secured connection.
  if scheme /= HTTP && scheme /= HTTPS
       || scheme == HTTPS && contextScheme ctx /= HTTPS && not (contextTrustedGateway ctx)
    then Left (Refused MisdirectedRequest (MisdirectedScheme scheme)) else Right ()
  target <- case form of
    Nothing -> if validForMethod method TargetAsterisk then Right TargetAsterisk
               else Left (Refused BadRequest AsteriskNotOptions)
    Just (path, query) -> Right (TargetURI scheme authority path query)
  finish method target parts
  where
    malformed = either (Left . Malformed) Right
    fromDefault = maybe (Left (Refused BadRequest NoAuthority)) Right (contextDefaultAuthority ctx)

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3.1
-- # This pseudo-header field MUST NOT be empty for "http" or "https"
-- #       URIs; "http" or "https" URIs that do not contain a path component
-- #       MUST include a value of '/'.
--
-- The path and query of an origin-form, or Nothing for @*@.
pathForm :: ByteString -> Either Malformation (Maybe (Path, Maybe Query))
pathForm "*" = Right Nothing
pathForm p
  | B.null p || B.head p /= 0x2F = Left (InvalidPseudoHeader ":path")
  | otherwise =
      let (path, rest) = B.break (== 0x3F) p
          query = if B.null rest then Right Nothing
                  else maybe (Left (InvalidPseudoHeader ":path")) (Right . Just) (mkQuery (B.drop 1 rest))
      in do
        path' <- maybe (Left (InvalidPseudoHeader ":path")) Right (mkPath path)
        q <- query
        Right (Just (path', q))

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3.1
-- # The recipient of an HTTP/2 request MUST NOT use the Host
-- #       header field to determine the target URI if ":authority" is
-- #       present.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3.1
-- # A server
-- #       SHOULD treat a request as malformed if it contains a Host header
-- #       field that identifies an entity that differs from the entity in
-- #       the ":authority" pseudo-header field.  The values of fields need
-- #       to be normalized to compare them (see Section 6.2 of [RFC3986]).
--
-- Compared as 'authorityKey' does: the host without case, an
-- IPv4-mapped address as IPv4, and the scheme's default port; for
-- CONNECT, which has no scheme, the ports as given.
hostAgrees :: Maybe Scheme -> Authority -> Parts -> Either ControlDataError Authority
hostAgrees scheme authority parts = case pHosts parts of
  [] -> Right authority
  [h] -> do
    host <- refuse InvalidHost (parseAuthority h)
    if key host == key authority then Right authority
      else Left (Malformed HostDiffersFromAuthority)
  _ : _ -> Left (Refused BadRequest RepeatedHost)
  where
    key a = case scheme of
      Just sc -> authorityKey sc a
      Nothing -> (canonicalHost (authorityHost a), authorityPort a)

-- | @:authority@, which must be @uri-host [ ":" port ]@: otherwise the
-- request is malformed (Section 8.3, "invalid pseudo-header fields").
pseudoAuthority :: ByteString -> Either ControlDataError Authority
pseudoAuthority = either (Left . Malformed . InvalidAuthority) Right . parseAuthority

refuse :: (e -> Refusal) -> Either e a -> Either ControlDataError a
refuse f = either (Left . Refused BadRequest . f) Right

servable :: Authority -> Either ControlDataError ()
servable a = case authorityHost a of
  HostIPvFuture _ -> Left (Refused BadRequest UnsupportedIPvFuture)
  _ -> Right ()

-- the fields for the application, and what the protocol layer keeps
finish :: Method -> Target -> Parts -> Either ControlDataError RequestControl
finish method target parts = do
  len <- case pLengths parts of
    [] -> Right Nothing
    ls -> maybe (Left (Refused BadRequest InvalidContentLength)) (Right . Just)
            (parseContentLength (map FieldValue (reverse ls)))
  let cookie = B.intercalate "; " (reverse (pCookies parts))
      fields = [ (FieldName n, FieldValue (if n == "cookie" then cookie else v))
               | (n, v) <- reverse (pFields parts) ]
  Right RequestControl
    { controlMethod = method
    , controlTarget = target
    , controlFields = fieldsFromList fields
    , controlContentLength = len
    , controlAcceptsTrailers = pTE parts }

------------------------------------------------------------------------------
-- Trailers
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1
-- # Trailers MUST NOT
-- # include pseudo-header fields (Section 8.3).  An endpoint that
-- # receives pseudo-header fields in trailers MUST treat the request or
-- # response as malformed (Section 8.1.1).

-- | A request's trailer section from its field list. @Content-Length@
-- and @Transfer-Encoding@ are refused, as HTTP/1.1 refuses them (the
-- second is also connection-specific); @TE@ is connection-specific
-- here, the exception being for a request's header section.
trailersFromFields :: [(ByteString, ByteString)] -> Either Malformation Fields
trailersFromFields list = fieldsFromList <$> traverse one list
  where
    one (n, v0)
      | isPseudo n = Left (PseudoHeaderInTrailers n)
      | not (validName n) = Left (BadFieldName n)
      | not (validValue v) = Left (BadFieldValue n)
      | n == "content-length" = Left (FramingFieldInTrailers n)
      | connectionSpecific n || n == "te" = Left (ConnectionSpecificField n)
      | otherwise = Right (FieldName n, FieldValue v)
      where v = stripWhitespace v0

------------------------------------------------------------------------------
-- Responses
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3.2
-- # For HTTP/2 responses, a single ":status" pseudo-header field is
-- # defined that carries the HTTP status code field (see Section 15 of
-- # [HTTP]).  This pseudo-header field MUST be included in all responses,
-- # including interim responses; otherwise, the response is malformed
-- # (Section 8.1.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2
-- # Field names MUST be converted to lowercase when constructing an
-- # HTTP/2 message.

-- | A response's field list: @:status@, then the header fields, which
-- are already in lower case ('FieldName''s invariant) and hold no
-- protocol field ('Hyperdrive.Semantics.Message.mkResponse''s). The
-- protocol layer adds what it generates (@content-length@, @date@).
responseToFields :: Status -> Fields -> [(ByteString, ByteString)]
responseToFields status fields =
  (":status", C.pack (show (statusCode status))) : trailersToFields fields

-- | A trailer section's field list.
trailersToFields :: Fields -> [(ByteString, ByteString)]
trailersToFields fields = [(fieldNameBytes n, fieldValueBytes v) | (n, v) <- fieldsToList fields]
