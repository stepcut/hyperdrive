{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | The subset of URI syntax (RFC 3986) used by HTTP request targets.
--
-- Only absolute components are handled here: scheme, authority, path
-- and query. Relative references and reference resolution are not
-- needed by a server and are not implemented.
module Hyperdrive.Semantics.URI
  ( -- * Scheme
    Scheme
  , mkScheme
  , schemeBytes
  , pattern HTTP
  , pattern HTTPS
    -- * Authority
  , Authority(..)
  , AuthorityError(..)
  , parseAuthority
  , renderAuthority
  , Host(..)
  , RegName
  , regNameBytes
  , IPv6(..)
  , IPvFuture
  , ipvFutureBytes
  , renderHost
    -- * Path
  , Path
  , mkPath
  , pathBytes
  , pathSegments
  , PathSegmentError(..)
    -- * Query
  , Query
  , mkQuery
  , queryBytes
    -- * Percent-encoding
  , percentDecode
  ) where

import Control.Monad (guard)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List (intersperse)
import qualified Data.List as L
import Data.Word (Word16, Word32, Word64, Word8)
import Hyperdrive.Semantics.Internal (IPvFuture(..), Path(..), Query(..), RegName(..), Scheme(..))
import Hyperdrive.Semantics.Syntax
import Numeric (showHex)

------------------------------------------------------------------------------
-- Scheme
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.1
-- # scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.1
-- # Although schemes are case-
-- # insensitive, the canonical form is lowercase

-- | Validate a scheme, normalizing it to lower case.
mkScheme :: ByteString -> Maybe Scheme
mkScheme bs =
  case B.uncons bs of
    Just (c, rest)
      | isALPHA c && B.all (\w -> isALPHA w || isDIGIT w || B.elem w "+-.") rest
      -> Just (Scheme (B.map toLower bs))
    _ -> Nothing

schemeBytes :: Scheme -> ByteString
schemeBytes (Scheme bs) = bs

pattern HTTP, HTTPS :: Scheme
pattern HTTP  = Scheme "http"
pattern HTTPS = Scheme "https"

------------------------------------------------------------------------------
-- Authority
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # host        = IP-literal / IPv4address / reg-name
data Host
  = HostRegName   !RegName
  | HostIPv4      !Word32
  | HostIPv6      !IPv6
  | HostIPvFuture !IPvFuture
  deriving (Eq, Ord, Show)

-- | A 128-bit IPv6 address, most significant half first
data IPv6 = IPv6 !Word64 !Word64
  deriving (Eq, Ord, Show)

-- | The authority of a target URI. The port is 'Nothing' when it was
-- empty or not given.
data Authority = Authority
  { authorityHost :: !Host
  , authorityPort :: !(Maybe Word16)
  }
  deriving (Eq, Ord, Show)

data AuthorityError
  = AuthorityHasUserinfo
  | AuthorityEmptyHost
  | AuthorityInvalidHost
  | AuthorityInvalidPort
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.2
-- # Host = uri-host [ ":" port ] ; Section 4

-- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.4
-- # Before making use of an "http" or "https" URI reference received from
-- # an untrusted source, a recipient SHOULD parse for userinfo and treat
-- # its presence as an error; it is likely being used to obscure the
-- # authority for the sake of phishing attacks.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.1
-- # A recipient that processes such a URI reference MUST
-- # reject it as invalid.

-- | Parse @uri-host [ ":" port ]@. This is the syntax of the @Host@
-- header field, the @:authority@ pseudo-header field and the authority
-- of an http(s) URI once its userinfo is rejected.
--
-- Userinfo is treated as an error. An empty host is rejected, since
-- an empty host is invalid for both http and https. Ports above 65535
-- are rejected, since they cannot identify a TCP or UDP port.
parseAuthority :: ByteString -> Either AuthorityError Authority
parseAuthority bs
  | B.elem 0x40 bs = Left AuthorityHasUserinfo -- '@' is not allowed anywhere in host or port
  | otherwise = do
      (hostBs, portBs) <- splitHostPort bs
      host <- parseHost hostBs
      port <- maybe (Right Nothing) parsePort portBs
      pure (Authority host port)

-- | Split at the ":" that introduces the port. A ":" can only appear
-- in the host inside an IP-literal.
splitHostPort :: ByteString -> Either AuthorityError (ByteString, Maybe ByteString)
splitHostPort bs
  | B.take 1 bs == "[" =
      case B.elemIndex 0x5D bs of
        Nothing -> Left AuthorityInvalidHost
        Just i  ->
          let (h, rest) = B.splitAt (i + 1) bs
          in case B.uncons rest of
               Nothing                -> Right (h, Nothing)
               Just (0x3A, p)         -> Right (h, Just p)
               Just _                 -> Left AuthorityInvalidHost
  | otherwise =
      case B.elemIndex 0x3A bs of
        Nothing -> Right (bs, Nothing)
        Just i  -> Right (B.take i bs, Just (B.drop (i + 1) bs))

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.3
-- # port        = *DIGIT
parsePort :: ByteString -> Either AuthorityError (Maybe Word16)
parsePort bs
  | B.null bs                     = Right Nothing
  | not (B.all isDIGIT bs)        = Left AuthorityInvalidPort
  | B.length significant > 5      = Left AuthorityInvalidPort
  | n > 65535                     = Left AuthorityInvalidPort
  | otherwise                     = Right (Just (fromIntegral n))
  where
    significant = B.dropWhile (== 0x30) bs
    n = B.foldl' (\acc w -> acc * 10 + fromIntegral (w - 0x30)) (0 :: Int) significant

parseHost :: ByteString -> Either AuthorityError Host
parseHost bs
  | B.take 1 bs == "[" =
      if B.length bs >= 2 && B.last bs == 0x5D
        then parseIPLiteral (B.init (B.tail bs))
        else Left AuthorityInvalidHost
  | Just a <- parseIPv4 bs = Right (HostIPv4 a)
  | B.null bs              = Left AuthorityEmptyHost
  | isRegName bs           = Right (HostRegName (RegName (normalizeRegName bs)))
  | otherwise              = Left AuthorityInvalidHost

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # reg-name    = *( unreserved / pct-encoded / sub-delims )
isRegName :: ByteString -> Bool
isRegName = allEncoded (\w -> isUnreserved w || isSubDelim w)

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # The host subcomponent is case-
-- # insensitive.

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # Although host
-- # is case-insensitive, producers and normalizers should use lowercase
-- # for registered names and hexadecimal addresses for the sake of
-- # uniformity, while only using uppercase letters for percent-encodings.

-- | lower case, except for the hex digits of percent-encodings
normalizeRegName :: ByteString -> ByteString
normalizeRegName = B.pack . go . B.unpack
  where
    go (0x25 : a : b : rest) = 0x25 : toUpper a : toUpper b : go rest
    go (w : rest)            = toLower w : go rest
    go []                    = []

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
parseIPLiteral :: ByteString -> Either AuthorityError Host
parseIPLiteral bs
  | B.take 1 (B.map toLower bs) == "v" =
      if isIPvFuture bs
        then Right (HostIPvFuture (IPvFuture (B.map toLower bs)))
        else Left AuthorityInvalidHost
  | Just a <- parseIPv6 bs = Right (HostIPv6 a)
  | otherwise = Left AuthorityInvalidHost

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # IPvFuture  = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
isIPvFuture :: ByteString -> Bool
isIPvFuture bs =
  case B.uncons bs of
    Just (v, rest) | toLower v == 0x76 ->
      let (ver, rest') = B.break (== 0x2E) rest
      in not (B.null ver) && B.all isHEXDIG ver
         && B.length rest' >= 2
         && B.all (\w -> isUnreserved w || isSubDelim w || w == 0x3A) (B.drop 1 rest')
    _ -> False

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # If host matches the rule for IPv4address, then it should be
-- # considered an IPv4 address literal and not a reg-name.
parseIPv4 :: ByteString -> Maybe Word32
parseIPv4 bs =
  case B.split 0x2E bs of
    [a, b, c, d] -> do
      os <- traverse decOctet [a, b, c, d]
      pure (L.foldl' (\acc o -> (acc `shiftL` 8) .|. fromIntegral o) 0 os)
    _ -> Nothing

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # dec-octet   = DIGIT                 ; 0-9
-- # / %x31-39 DIGIT         ; 10-99
-- # / "1" 2DIGIT            ; 100-199
-- # / "2" %x30-34 DIGIT     ; 200-249
-- # / "25" %x30-35          ; 250-255
decOctet :: ByteString -> Maybe Word8
decOctet bs = do
  guard (B.length bs >= 1 && B.length bs <= 3 && B.all isDIGIT bs)
  guard (B.length bs == 1 || B.head bs /= 0x30) -- no leading zeros
  let n = B.foldl' (\acc w -> acc * 10 + fromIntegral (w - 0x30)) (0 :: Int) bs
  guard (n <= 255)
  pure (fromIntegral n)

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # IPv6address =                            6( h16 ":" ) ls32
-- # /                       "::" 5( h16 ":" ) ls32
-- # / [               h16 ] "::" 4( h16 ":" ) ls32
-- # / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
-- # / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
-- # / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
-- # / [ *4( h16 ":" ) h16 ] "::"              ls32
-- # / [ *5( h16 ":" ) h16 ] "::"              h16
-- # / [ *6( h16 ":" ) h16 ] "::"

-- | The nine alternatives come down to: without "::" there must be
-- exactly eight 16-bit pieces; with a single "::" there may be at most
-- seven. In both cases an IPv4 address may only appear at the end,
-- where it counts as two pieces.
parseIPv6 :: ByteString -> Maybe IPv6
parseIPv6 bs =
  case B.breakSubstring "::" bs of
    (l, r)
      | B.null r -> do
          ps <- pieces True l
          guard (length ps == 8)
          pure (fromPieces ps)
      | otherwise -> do
          let r' = B.drop 2 r
          guard (not ("::" `B.isInfixOf` r'))
          ls <- if B.null l  then pure [] else pieces False l
          rs <- if B.null r' then pure [] else pieces True r'
          let n = length ls + length rs
          guard (n <= 7)
          pure (fromPieces (ls ++ replicate (8 - n) 0 ++ rs))
  where
    pieces :: Bool -> ByteString -> Maybe [Word16]
    pieces allowIPv4 = go . B.split 0x3A
      where
        go [p] | allowIPv4 && B.elem 0x2E p = do
          a <- parseIPv4 p
          pure [fromIntegral (a `shiftR` 16), fromIntegral (a .&. 0xFFFF)]
        go (p : ps) = (:) <$> h16 p <*> go ps
        go []       = Just []

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # h16         = 1*4HEXDIG
h16 :: ByteString -> Maybe Word16
h16 bs = do
  guard (B.length bs >= 1 && B.length bs <= 4 && B.all isHEXDIG bs)
  pure (B.foldl' (\acc w -> acc * 16 + fromIntegral (hexValue w)) 0 bs)

fromPieces :: [Word16] -> IPv6
fromPieces ps =
  let (hi, lo) = splitAt 4 ps
      pack64 = L.foldl' (\acc p -> (acc `shiftL` 16) .|. fromIntegral p) 0
  in IPv6 (pack64 hi) (pack64 lo)

toPieces :: IPv6 -> [Word16]
toPieces (IPv6 hi lo) = unpack64 hi ++ unpack64 lo
  where unpack64 w = [ fromIntegral (w `shiftR` s) | s <- [48, 32, 16, 0] ]

regNameBytes :: RegName -> ByteString
regNameBytes (RegName bs) = bs

ipvFutureBytes :: IPvFuture -> ByteString
ipvFutureBytes (IPvFuture bs) = bs

-- | Render a host in its normal form. IPv6 addresses are rendered as
-- recommended by RFC 5952.
renderHost :: Host -> ByteString
renderHost (HostRegName r)   = regNameBytes r
renderHost (HostIPv4 a)      =
  B.intercalate "." [ C.pack (show ((a `shiftR` s) .&. 0xFF)) | s <- [24, 16, 8, 0] ]
renderHost (HostIPv6 a)      = "[" <> renderIPv6 a <> "]"
renderHost (HostIPvFuture f) = "[" <> ipvFutureBytes f <> "]"

-- | RFC 5952: lower case hex, no leading zeros, and the longest run of
-- two or more zero pieces (the first, if there is a tie) replaced by
-- "::".
renderIPv6 :: IPv6 -> ByteString
renderIPv6 a =
  case longestZeroRun ps of
    Nothing       -> hexes ps
    Just (i, len) -> hexes (take i ps) <> "::" <> hexes (drop (i + len) ps)
  where
    ps = toPieces a
    hexes = mconcat . intersperse ":" . map (\p -> C.pack (showHex p ""))

longestZeroRun :: [Word16] -> Maybe (Int, Int)
longestZeroRun ps =
  case [ (i, len) | (i, len) <- runs 0 ps, len >= 2 ] of
    []  -> Nothing
    rs  -> Just (foldr1 (\r@(_, l) best@(_, lb) -> if l >= lb then r else best) rs)
  where
    runs _ [] = []
    runs i xs@(0 : _) = let len = length (takeWhile (== 0) xs)
                        in (i, len) : runs (i + len) (drop len xs)
    runs i (_ : xs) = runs (i + 1) xs

renderAuthority :: Authority -> ByteString
renderAuthority (Authority h p) =
  renderHost h <> maybe "" (\n -> ":" <> C.pack (show n)) p

------------------------------------------------------------------------------
-- Path
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.3
-- # path-abempty  = *( "/" segment )

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.3
-- # segment       = *pchar

-- | Validate a path. The path is kept exactly as received, including
-- its percent-encoding, because decoding can change its meaning (an
-- encoded "/" is not a segment separator).
mkPath :: ByteString -> Maybe Path
mkPath bs
  | B.null bs = Just (Path bs)
  | B.head bs == 0x2F && allEncoded (\w -> isPChar w || w == 0x2F) bs = Just (Path bs)
  | otherwise = Nothing

pathBytes :: Path -> ByteString
pathBytes (Path bs) = bs

data PathSegmentError
  = DotSegment -- ^ a segment was @.@ or @..@, possibly percent-encoded
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.3
-- # The path segments "." and "..", also known as dot-segments, are
-- # defined for relative reference within the path name hierarchy.

-- | Split a path into percent-decoded segments.
--
-- > pathSegments "/"            == Right []
-- > pathSegments "/a/b%2Fc/"    == Right ["a", "b/c", ""]
-- > pathSegments "/a/%2e%2e/b"  == Left DotSegment
--
-- A request target is always absolute, so a dot-segment can only be
-- an attempt to climb out of the hierarchy. Rather than resolve it,
-- the path is rejected. Note that a decoded segment may contain "/".
pathSegments :: Path -> Either PathSegmentError [ByteString]
pathSegments (Path bs) =
  case B.split 0x2F bs of
    ("" : segs) | segs /= [""] -> traverse decodeSegment segs
    _                          -> Right []
  where
    decodeSegment s =
      case percentDecode s of
        Just d | d == "." || d == ".." -> Left DotSegment
               | otherwise             -> Right d
        Nothing -> Right s -- impossible: the Path invariant guarantees valid encoding

------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.4
-- # query       = *( pchar / "/" / "?" )

-- | Validate a query, keeping it exactly as received.
mkQuery :: ByteString -> Maybe Query
mkQuery bs
  | allEncoded (\w -> isPChar w || w == 0x2F || w == 0x3F) bs = Just (Query bs)
  | otherwise = Nothing

queryBytes :: Query -> ByteString
queryBytes (Query bs) = bs

------------------------------------------------------------------------------
-- Percent-encoding
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc3986#section-2.1
-- # pct-encoded = "%" HEXDIG HEXDIG

-- | Every octet is either allowed by the predicate or is the start of
-- a @pct-encoded@ triplet.
allEncoded :: (Word8 -> Bool) -> ByteString -> Bool
allEncoded ok bs = go 0
  where
    n = B.length bs
    go i
      | i >= n    = True
      | w == 0x25 = i + 2 < n
                    && isHEXDIG (B.index bs (i + 1))
                    && isHEXDIG (B.index bs (i + 2))
                    && go (i + 3)
      | ok w      = go (i + 1)
      | otherwise = False
      where w = B.index bs i

-- | Decode percent-encoded octets. Returns 'Nothing' if a "%" is not
-- followed by two hex digits.
percentDecode :: ByteString -> Maybe ByteString
percentDecode bs
  | B.notElem 0x25 bs = Just bs
  | otherwise         = B.pack <$> go (B.unpack bs)
  where
    go (0x25 : a : b : rest)
      | isHEXDIG a && isHEXDIG b = (hexValue a * 16 + hexValue b :) <$> go rest
    go (0x25 : _) = Nothing
    go (w : rest) = (w :) <$> go rest
    go []         = Just []

------------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------------

hexValue :: Word8 -> Word8
hexValue w
  | w >= 0x61 = w - 0x61 + 10
  | w >= 0x41 = w - 0x41 + 10
  | otherwise = w - 0x30

toLower :: Word8 -> Word8
toLower w
  | w >= 0x41 && w <= 0x5A = w + 0x20
  | otherwise              = w

toUpper :: Word8 -> Word8
toUpper w
  | w >= 0x61 && w <= 0x7A = w - 0x20
  | otherwise              = w
