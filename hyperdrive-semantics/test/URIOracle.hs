-- | A direct transcription of the RFC 3986 ABNF into 'ReadP'.
--
-- This is deliberately naive: each rule follows the text of the RFC as
-- closely as possible, alternatives and all, with no attempt at
-- efficiency. It serves as an independent oracle for the hand-written
-- parsers in "Hyperdrive.Semantics.URI".
module URIOracle
  ( matches
  , scheme
  , hostPort
  , host
  , ipv6address
  , ipv4address
  , pathAbempty
  , query
  ) where

import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit)
import Text.ParserCombinators.ReadP

-- | does the parser match the whole string?
matches :: ReadP () -> String -> Bool
matches p s = not (null (readP_to_S (p <* eof) s))

oneOf :: String -> ReadP ()
oneOf cs = void (satisfy (`elem` cs))

-- | @n*m@ repetition
between' :: Int -> Int -> ReadP () -> ReadP ()
between' lo hi p = choice [ void (count n p) | n <- [lo .. hi] ]

alpha, digit, hexdig :: ReadP ()
alpha  = void (satisfy (\c -> isAsciiUpper c || isAsciiLower c))
digit  = void (satisfy isDigit)
hexdig = void (satisfy isHexDigit)

-- unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
unreserved :: ReadP ()
unreserved = alpha +++ digit +++ oneOf "-._~"

-- pct-encoded = "%" HEXDIG HEXDIG
pctEncoded :: ReadP ()
pctEncoded = char '%' >> hexdig >> hexdig

-- sub-delims  = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
subDelims :: ReadP ()
subDelims = oneOf "!$&'()*+,;="

-- pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
pchar :: ReadP ()
pchar = unreserved +++ pctEncoded +++ subDelims +++ oneOf ":@"

-- scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
scheme :: ReadP ()
scheme = alpha >> void (many (alpha +++ digit +++ oneOf "+-."))

-- RFC 9110: Host = uri-host [ ":" port ]
hostPort :: ReadP ()
hostPort = host >> optional (char ':' >> port)

-- port = *DIGIT
port :: ReadP ()
port = void (many digit)

-- host = IP-literal / IPv4address / reg-name
host :: ReadP ()
host = ipLiteral +++ ipv4address +++ regName

-- IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
ipLiteral :: ReadP ()
ipLiteral = char '[' >> (ipv6address +++ ipvFuture) >> void (char ']')

-- IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
-- (ABNF strings are case-insensitive, so "v" also matches "V")
ipvFuture :: ReadP ()
ipvFuture = do
  oneOf "vV"
  void (many1 hexdig)
  void (char '.')
  void (many1 (unreserved +++ subDelims +++ void (char ':')))

-- IPv6address =                            6( h16 ":" ) ls32
--             /                       "::" 5( h16 ":" ) ls32
--             / [               h16 ] "::" 4( h16 ":" ) ls32
--             / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
--             / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
--             / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
--             / [ *4( h16 ":" ) h16 ] "::"              ls32
--             / [ *5( h16 ":" ) h16 ] "::"              h16
--             / [ *6( h16 ":" ) h16 ] "::"
ipv6address :: ReadP ()
ipv6address = choice
  [                         void (count 6 h16c) >> ls32
  ,              dcolon >> void (count 5 h16c) >> ls32
  , prefix 0 >> dcolon >> void (count 4 h16c) >> ls32
  , prefix 1 >> dcolon >> void (count 3 h16c) >> ls32
  , prefix 2 >> dcolon >> void (count 2 h16c) >> ls32
  , prefix 3 >> dcolon >> h16c >> ls32
  , prefix 4 >> dcolon >> ls32
  , prefix 5 >> dcolon >> h16
  , prefix 6 >> dcolon
  ]
  where
    h16c = h16 >> void (char ':')
    dcolon = void (string "::")
    -- [ *n( h16 ":" ) h16 ]
    prefix n = optional (between' 0 n h16c >> h16)

-- ls32 = ( h16 ":" h16 ) / IPv4address
ls32 :: ReadP ()
ls32 = (h16 >> char ':' >> h16) +++ ipv4address

-- h16 = 1*4HEXDIG
h16 :: ReadP ()
h16 = between' 1 4 hexdig

-- IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
ipv4address :: ReadP ()
ipv4address = decOctet >> char '.' >> decOctet >> char '.' >> decOctet >> char '.' >> decOctet

-- dec-octet = DIGIT / %x31-39 DIGIT / "1" 2DIGIT / "2" %x30-34 DIGIT / "25" %x30-35
decOctet :: ReadP ()
decOctet = choice
  [ digit
  , oneOf "123456789" >> digit
  , char '1' >> void (count 2 digit)
  , char '2' >> oneOf "01234" >> digit
  , string "25" >> oneOf "012345"
  ]

-- reg-name = *( unreserved / pct-encoded / sub-delims )
regName :: ReadP ()
regName = void (many (unreserved +++ pctEncoded +++ subDelims))

-- path-abempty = *( "/" segment )
pathAbempty :: ReadP ()
pathAbempty = void (many (char '/' >> segment))

-- segment = *pchar
segment :: ReadP ()
segment = void (many pchar)

-- query = *( pchar / "/" / "?" )
query :: ReadP ()
query = void (many (pchar +++ oneOf "/?"))
