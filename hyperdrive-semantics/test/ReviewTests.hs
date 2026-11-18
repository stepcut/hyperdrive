{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Independent review tests for hyperdrive-semantics.
--
-- Every expectation below comes from the RFC text quoted in its
-- description (RFC 9110, RFC 9112, RFC 3986, RFC 5952, RFC 1123,
-- RFC 1035, RFC 4291), or, where marked POLICY, from a choice that the
-- API documents and the RFC permits without requiring it. Nothing here
-- was derived from running the library.
module ReviewTests (reviewTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word16, Word64, Word8)

import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.FieldSyntax
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Method
import Hyperdrive.Semantics.Status
import Hyperdrive.Semantics.Target
import Hyperdrive.Semantics.URI

type T = (String, Bool)

reviewTests :: [T]
reviewTests = concat
  [ fieldNameTests
  , fieldValueTests
  , combineTests
  , fieldsTests
  , splitTests
  , fieldListTests
  , quotedStringTests
  , quoteStringTests
  , parameterTests
  , protocolFieldTests
  , requestTests
  , responseTests
  , checkResponseTests
  , trailerTests
  , fileRangeTests
  , hostParseTests
  , canonicalHostTests
  , effectivePortTests
  , authorityKeyTests
  , numericLookalikeTests
  , dnsHostNameTests
  , renderIPv6Tests
  , renderHostTests
  , renderAuthorityTests
  ]

------------------------------------------------------------------------
-- Helpers (total: a helper that cannot build its input makes the test
-- False rather than throwing)

tm :: String -> Maybe Bool -> T
tm d m = (d, m == Just True)

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

fv :: ByteString -> Maybe FieldValue
fv = hush . mkFieldValue

fieldsOf :: [(ByteString, ByteString)] -> Maybe Fields
fieldsOf = fmap fieldsFromList . traverse (\(n, v) -> (,) <$> mkFieldName n <*> fv v)

w :: Word8 -> ByteString
w = B.singleton

isWS :: Word8 -> Bool
isWS c = c == 0x20 || c == 0x09

-- | CTL (RFC 5234: %x00-1F / %x7F) other than HTAB
ctlsNotHTAB :: [Word8]
ctlsNotHTAB = [0x00 .. 0x08] ++ [0x0A .. 0x1F] ++ [0x7F]

-- | CTL other than HTAB, CR, LF and NUL
otherCtls :: [Word8]
otherCtls = [0x01 .. 0x08] ++ [0x0B, 0x0C] ++ [0x0E .. 0x1F] ++ [0x7F]

hex2 :: Word8 -> String
hex2 b = "0x" ++ [digits !! fromIntegral (b `div` 16), digits !! fromIntegral (b `mod` 16)]
  where digits = "0123456789ABCDEF"

authOf :: ByteString -> Maybe Authority
authOf = hush . parseAuthority

hostOf :: ByteString -> Maybe Host
hostOf = fmap authorityHost . authOf

regOf :: ByteString -> Maybe RegName
regOf s = case hostOf s of
  Just (HostRegName r) -> Just r
  _ -> Nothing

-- | renderHost with the brackets of an IP-literal removed, if present,
-- so that the IPv6 text form can be compared on its own (the
-- documentation does not say whether renderHost includes them).
unbracket :: ByteString -> ByteString
unbracket s
  | B.length s >= 2 && BC.head s == '[' && BC.last s == ']' = B.init (B.tail s)
  | otherwise = s

split :: Comments -> ByteString -> Maybe (Either FieldSyntaxError [ByteString])
split c s = splitList c <$> fv s

splitCount :: Comments -> ByteString -> Maybe (Either FieldSyntaxError Int)
splitCount c s = fmap (fmap length) (split c s)

emptyBody :: RequestBody
emptyBody = RequestBody
  { bodyLength = Just 0
  , readChunk = pure B.empty
  , readTrailers = pure emptyFields
  }

uriTarget :: ByteString -> ByteString -> Maybe Target
uriTarget a p = TargetURI HTTP <$> authOf a <*> mkPath p <*> pure Nothing

httpTarget :: ByteString -> Maybe Target
httpTarget = uriTarget "example.com"

connTarget :: Maybe Target
connTarget = connectTarget =<< authOf "server.example.com:443"

requestWith :: Method -> Maybe Target -> [(ByteString, ByteString)] -> Maybe (Either RequestError Request)
requestWith m mt hs = mkRequest HTTP_1_1 m <$> mt <*> fieldsOf hs <*> pure emptyBody

responseWith :: Int -> [(ByteString, ByteString)] -> Content -> Maybe (Either ResponseError Response)
responseWith code hs c = mkResponse <$> mkStatus code <*> fieldsOf hs <*> pure c

noStream :: Maybe Word64 -> Content
noStream l = ContentStream l [] (\_ _ -> pure emptyFields)

isLeftWith :: Eq e => e -> Either e a -> Bool
isLeftWith e = either (== e) (const False)

------------------------------------------------------------------------
-- RFC quotations used repeatedly

qFieldName :: String
qFieldName = "RFC 9110 Section 5.1: \"field-name = token\"; Section 5.6.2: \"token = 1*tchar\""

qTchar :: String
qTchar = "RFC 9110 Section 5.6.2: \"tchar = \"!\" / \"#\" / \"$\" / \"%\" / \"&\" / \"'\" / \"*\" / \"+\" / \"-\" / \".\" / \"^\" / \"_\" / \"`\" / \"|\" / \"~\" / DIGIT / ALPHA\""

qDelims :: String
qDelims = "RFC 9110 Section 5.6.2: \"Delimiters are chosen from the set of US-ASCII visual characters not allowed in a token (DQUOTE and \"(),/:;<=>?@[\\]{}\").\""

qCaseIns :: String
qCaseIns = "RFC 9110 Section 5.1: \"Field names are case-insensitive\""

q55strip :: String
q55strip = "RFC 9110 Section 5.5: \"A field value does not include leading or trailing whitespace. When a specific version of HTTP allows such whitespace to appear in a message, a field parsing implementation MUST exclude such whitespace prior to evaluating the field value.\""

q55grammar :: String
q55grammar = "RFC 9110 Section 5.5: \"field-value = *field-content; field-content = field-vchar [ 1*( SP / HTAB / field-vchar ) field-vchar ]; field-vchar = VCHAR / obs-text; obs-text = %x80-FF\""

q55crlf :: String
q55crlf = "RFC 9110 Section 5.5: \"a recipient of CR, LF, or NUL within a field value MUST either reject the message or replace each of those characters with SP before further processing or forwarding of that message.\""

q55ctl :: String
q55ctl = "RFC 9110 Section 5.5: \"Field values containing other CTL characters are also invalid; however, recipients MAY retain such characters for the sake of robustness\""

q52 :: String
q52 = "RFC 9110 Section 5.2: \"When a field name is repeated within a section, its combined field value consists of the list of corresponding field line values within that section, concatenated in order, with each field line value separated by a comma.\""

q53 :: String
q53 = "RFC 9110 Section 5.3: \"A recipient MAY combine multiple field lines ... by appending each subsequent field line value to the initial field line value in order, separated by a comma (\",\") and optional whitespace (OWS ...). For consistency, use comma SP.\""

q53order :: String
q53order = "RFC 9110 Section 5.3: \"The order in which field lines with the same name are received is therefore significant to the interpretation of the field value\""

q53sender :: String
q53sender = "RFC 9110 Section 5.3: \"a sender MUST NOT generate multiple field lines with the same name in a message ... unless that field's definition allows multiple field line values to be recombined as a comma-separated list\""

q53cookie :: String
q53cookie = "RFC 9110 Section 5.3 Note: \"In practice, the \"Set-Cookie\" header field ([COOKIE]) often appears in a response message across multiple field lines and does not use the list syntax ... recipients ought to handle \"Set-Cookie\" as a special case\""

q5612 :: String
q5612 = "RFC 9110 Section 5.6.1.2: \"A recipient MUST parse and ignore a reasonable number of empty list elements ... a recipient MUST accept lists that satisfy the following syntax: #element => [ element ] *( OWS \",\" OWS [ element ] )\""

q5612ex :: String
q5612ex = "RFC 9110 Section 5.6.1.2: \"the following are valid values for example-list ...: \"foo,bar\" \"foo ,bar,\" \"foo , ,bar,charlie\"\""

q55comma :: String
q55comma = "RFC 9110 Section 5.5: \"Fields that expect to contain a comma within a member ... ought to be defined with delimiters around that element\"; example: Example-URIs: \"http://example.com/a.html,foo\", \"http://without-a-comma.example.com/\""

q564 :: String
q564 = "RFC 9110 Section 5.6.4: \"quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE; qdtext = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text\""

q564pair :: String
q564pair = "RFC 9110 Section 5.6.4: \"Recipients that process the value of a quoted-string MUST handle a quoted-pair as if it were replaced by the octet following the backslash. quoted-pair = \"\\\" ( HTAB / SP / VCHAR / obs-text )\""

q564sender :: String
q564sender = "RFC 9110 Section 5.6.4: \"A sender SHOULD NOT generate a quoted-pair in a quoted-string except where necessary to quote DQUOTE and backslash octets occurring within that string.\""

q565 :: String
q565 = "RFC 9110 Section 5.6.5: \"comment = \"(\" *( ctext / quoted-pair / comment ) \")\"; ctext = HTAB / SP / %x21-27 / %x2A-5B / %x5D-7E / obs-text\""

q565only :: String
q565only = "RFC 9110 Section 5.6.5: \"Comments are only allowed in fields containing \"comment\" as part of their field value definition.\""

q566 :: String
q566 = "RFC 9110 Section 5.6.6: \"parameters = *( OWS \";\" OWS [ parameter ] ); parameter = parameter-name \"=\" parameter-value; parameter-name = token; parameter-value = ( token / quoted-string )\""

q566case :: String
q566case = "RFC 9110 Section 5.6.6: \"Parameter names are case-insensitive. Parameter values might or might not be case-sensitive, depending on the semantics of the parameter name.\""

q566equiv :: String
q566equiv = "RFC 9110 Section 5.6.6: \"A parameter value that matches the token production can be transmitted either as a token or within a quoted-string. The quoted and unquoted values are equivalent.\""

q566ws :: String
q566ws = "RFC 9110 Section 5.6.6 Note: \"Parameters do not allow whitespace (not even \"bad\" whitespace) around the \"=\" character.\""

q761 :: String
q761 = "RFC 9110 Section 7.6.1: \"intermediaries SHOULD remove or replace fields that are known to require removal before forwarding ... Proxy-Connection ..., Keep-Alive ..., TE ..., Transfer-Encoding ..., Upgrade\"; and \"The \"Connection\" header field allows the sender to list desired control options for the current connection.\""

q641 :: String
q641 = "RFC 9110 Section 6.4.1: \"All 1xx (Informational), 204 (No Content), and 304 (Not Modified) responses do not include content. All other responses do include content, although that content might be of zero length.\""

q71forms :: String
q71forms = "RFC 9110 Section 7.1: \"For CONNECT ..., the request target is the host name and port number of the tunnel destination ... For OPTIONS ..., the request target can be a single asterisk (\"*\"). ... These forms MUST NOT be used with other methods.\""

q91case :: String
q91case = "RFC 9110 Section 9.1: \"The method token is case-sensitive\""

q651 :: String
q651 = "RFC 9110 Section 6.5.1: \"Many fields cannot be processed outside the header section because their evaluation is necessary prior to receiving the content, such as those that describe message framing, routing, authentication, request modifiers, response controls, or content format. A sender MUST NOT generate a trailer field unless the sender knows the corresponding header field name's definition permits the field to be sent in trailers.\""

q3986host :: String
q3986host = "RFC 3986 Section 3.2.2: \"The host subcomponent is case-insensitive.\""

q3986ipv4 :: String
q3986ipv4 = "RFC 3986 Section 3.2.2: \"If host matches the rule for IPv4address, then it should be considered an IPv4 address literal and not a reg-name.\"; \"dec-octet = DIGIT / %x31-39 DIGIT / \"1\" 2DIGIT / \"2\" %x30-34 DIGIT / \"25\" %x30-35\"; \"only the dotted-decimal form of four octets is allowed by this grammar\""

q3986unres :: String
q3986unres = "RFC 3986 Section 2.3: \"URIs that differ in the replacement of an unreserved character with its corresponding percent-encoded US-ASCII octet are equivalent: they identify the same resource.\" (unreserved = ALPHA / DIGIT / \"-\" / \".\" / \"_\" / \"~\")"

q3986res :: String
q3986res = "RFC 3986 Section 2.2: \"URIs that differ in the replacement of a reserved character with its corresponding percent-encoded octet are not equivalent.\""

q3986pct :: String
q3986pct = "RFC 3986 Section 6.2.2.1: \"the hexadecimal digits within a percent-encoding triplet (e.g., \"%3a\" versus \"%3A\") are case-insensitive and therefore should be normalized to use uppercase letters for the digits A-F.\""

q3986ipv6 :: String
q3986ipv6 = "RFC 3986 Section 3.2.2: \"Each piece is represented numerically in case-insensitive hexadecimal, using one to four hexadecimal digits (leading zeroes are permitted). ... Optionally, the least-significant two pieces may instead be represented in IPv4 address textual format. A sequence of one or more consecutive zero-valued 16-bit pieces within the address may be elided\""

q3986port :: String
q3986port = "RFC 3986 Section 3.2.3: \"port = *DIGIT\"; RFC 9110 Section 4.3.1: \"normalizing the port to remove any leading zeros\""

q9110default :: String
q9110default = "RFC 9110 Section 4.2.1: \"If the port subcomponent is empty or not given, TCP port 80 (the reserved port for WWW services) is the default.\"; Section 4.2.2: \"If the port subcomponent is empty or not given, TCP port 443 (the reserved port for HTTP over TLS) is the default.\""

q9110norm :: String
q9110norm = "RFC 9110 Section 4.2.3: \"If the port is equal to the default port for a scheme, the normal form is to omit the port subcomponent.\" ... \"The scheme and host are case-insensitive\"; example: http://example.com:80/~smith/home.html, http://EXAMPLE.com/%7Esmith/home.html and http://EXAMPLE.com:/%7esmith/home.html are equivalent"

q9110origin :: String
q9110origin = "RFC 9110 Section 4.3.1: \"Two origins are distinct if they differ in scheme, host, or port.\""

q1123 :: String
q1123 = "RFC 1123 Section 2.1: \"a valid host name can never have the dotted-decimal form #.#.#.#, since at least the highest-level component label will be alphabetic.\""

q952 :: String
q952 = "RFC 952: \"<name> ::= <let>[*[<let-or-digit-or-hyphen>]<let-or-digit>]\", relaxed by RFC 1123 Section 2.1: \"the restriction on the first character is relaxed to allow either a letter or a digit\""

q1035 :: String
q1035 = "RFC 1035 Section 2.3.4: \"labels 63 octets or less; names 255 octets or less\" (a name of 253 characters without the trailing dot is 255 octets on the wire)"

q5952 :: String
q5952 = "RFC 5952 Section 4: \"The recommendation in this section SHOULD be followed by systems when generating an address to be represented as text\""

q5952_41 :: String
q5952_41 = "RFC 5952 Section 4.1: \"Leading zeros MUST be suppressed. For example, 2001:0db8::0001 is not acceptable and must be represented as 2001:db8::1. A single 16-bit 0000 field MUST be represented as 0.\""

q5952_421 :: String
q5952_421 = "RFC 5952 Section 4.2.1: \"The use of the symbol \"::\" MUST be used to its maximum capability. For example, 2001:db8:0:0:0:0:2:1 must be shortened to 2001:db8::2:1.\""

q5952_422 :: String
q5952_422 = "RFC 5952 Section 4.2.2: \"The symbol \"::\" MUST NOT be used to shorten just one 16-bit 0 field. For example, the representation 2001:db8:0:1:1:1:1:1 is correct, but 2001:db8::1:1:1:1:1 is not correct.\""

q5952_423 :: String
q5952_423 = "RFC 5952 Section 4.2.3: \"the longest run of consecutive 16-bit 0 fields MUST be shortened ... When the length of the consecutive 16-bit 0 fields are equal ..., the first sequence of zero bits MUST be shortened. For example, 2001:db8::1:0:0:1 is correct representation.\""

q5952_43 :: String
q5952_43 = "RFC 5952 Section 4.3: \"The characters \"a\", \"b\", \"c\", \"d\", \"e\", and \"f\" in an IPv6 address MUST be represented in lowercase.\""

q5952_5 :: String
q5952_5 = "RFC 5952 Section 5: \"For these addresses, mixed notation is RECOMMENDED if ... the address can be distinguished as having IPv4 addresses embedded in the lower 32 bits solely from the address field through the use of a well-known prefix\" and \"::ffff:192.0.2.1 instead of 0:0:0:0:0:ffff:192.0.2.1\"; RFC 4291 Section 2.5.5.2 defines the IPv4-mapped prefix 0000...0000:FFFF"

q4291 :: String
q4291 = "RFC 4291 Section 2.2: \"0:0:0:0:0:0:0:1 the loopback address ... 0:0:0:0:0:0:0:0 the unspecified address ... may be represented as ... ::1 ... ::\""

------------------------------------------------------------------------
-- Field names (RFC 9110 Section 5.1)

fieldNameTests :: [T]
fieldNameTests =
  [ tm ("mkFieldName accepts a name containing the tchar " ++ show c ++ ". " ++ qFieldName ++ "; " ++ qTchar)
       (Just (fmap fieldNameBytes (mkFieldName n) == Just n))
  | c <- "!#$%&'*+-.^_`|~0123456789"
  , let n = BC.pack ['x', c, 'y']
  ]
  ++
  [ (("mkFieldName rejects a name containing the delimiter " ++ show c ++ ". " ++ qDelims ++ "; " ++ qFieldName),
     isNothing (mkFieldName (BC.pack ['x', c, 'y'])))
  | c <- "\"(),/:;<=>?@[\\]{}"
  ]
  ++
  [ (("mkFieldName rejects a name containing the octet " ++ hex2 b ++ " (whitespace, a control character or non-ASCII, none of which is a tchar). " ++ qFieldName ++ "; " ++ qTchar),
     isNothing (mkFieldName ("x" <> w b <> "y")))
  | b <- [0x20, 0x09, 0x00, 0x0D, 0x0A, 0x01, 0x1B, 0x7F, 0x80, 0xC3, 0xFF]
  ]
  ++
  [ ("mkFieldName rejects the empty name. " ++ qFieldName ++ " (1*tchar: at least one tchar)",
     isNothing (mkFieldName ""))
  , ("mkFieldName rejects the pseudo-header name \":authority\": it is control data, not a field, and \":\" is a delimiter. " ++ qDelims ++ "; RFC 9110 Section 6.2: \"In HTTP/2 ... and HTTP/3 ..., control data is sent as pseudo-header fields with a reserved name prefix (e.g., \":authority\").\"",
     isNothing (mkFieldName ":authority"))
  , ("mkFieldName rejects the pseudo-header name \":path\". " ++ qDelims,
     isNothing (mkFieldName ":path"))
  , ("mkFieldName rejects \"host \" (whitespace before the colon). RFC 9112 Section 5.1: \"No whitespace is allowed between the field name and colon. ... A server MUST reject, with a response status code of 400 (Bad Request), any received request message that contains whitespace between a header field name and colon.\"",
     isNothing (mkFieldName "host "))
  , ("mkFieldName rejects \" host\" (leading whitespace is not a tchar). " ++ qFieldName,
     isNothing (mkFieldName " host"))
  , ("mkFieldName rejects a name containing an obs-fold (CRLF SP). " ++ qFieldName,
     isNothing (mkFieldName "x-a\r\n b"))
  , ("mkFieldName treats \"Content-Type\" and \"content-type\" as the same name. " ++ qCaseIns,
     isJust (mkFieldName "content-type") && mkFieldName "Content-Type" == mkFieldName "content-type")
  , ("mkFieldName treats \"CONTENT-TYPE\" and \"cOnTeNt-TyPe\" as the same name. " ++ qCaseIns,
     isJust (mkFieldName "CONTENT-TYPE") && mkFieldName "CONTENT-TYPE" == mkFieldName "cOnTeNt-TyPe")
  , ("mkFieldName treats names differing only in case of every ASCII letter as equal. " ++ qCaseIns,
     isJust (mkFieldName "abcdefghijklmnopqrstuvwxyz") && mkFieldName "ABCDEFGHIJKLMNOPQRSTUVWXYZ" == mkFieldName "abcdefghijklmnopqrstuvwxyz")
  , ("mkFieldName does not identify names that differ in a non-letter (\"x-a\" vs \"x_a\"). " ++ qFieldName,
     mkFieldName "x-a" /= mkFieldName "x_a")
  , ("POLICY: mkFieldName normalizes to lower case (documented). " ++ qCaseIns,
     fmap fieldNameBytes (mkFieldName "X-Custom-HEADER") == Just "x-custom-header")
  , ("mkFieldName \"Host\" equals hHost. " ++ qCaseIns, mkFieldName "Host" == Just hHost)
  , ("mkFieldName \"Connection\" equals hConnection. " ++ qCaseIns, mkFieldName "Connection" == Just hConnection)
  , ("mkFieldName \"Content-Length\" equals hContentLength. " ++ qCaseIns, mkFieldName "Content-Length" == Just hContentLength)
  , ("mkFieldName \"Content-Type\" equals hContentType. " ++ qCaseIns, mkFieldName "Content-Type" == Just hContentType)
  , ("mkFieldName \"Set-Cookie\" equals hSetCookie. " ++ qCaseIns, mkFieldName "Set-Cookie" == Just hSetCookie)
  , ("mkFieldName \"Transfer-Encoding\" equals hTransferEncoding. " ++ qCaseIns, mkFieldName "Transfer-Encoding" == Just hTransferEncoding)
  ]

------------------------------------------------------------------------
-- Field values (RFC 9110 Section 5.5)

fieldValueTests :: [T]
fieldValueTests =
  [ ("mkFieldValue " ++ show i ++ " gives " ++ show o ++ ". " ++ q55strip,
     fmap fieldValueBytes (mkFieldValue i) == Right o)
  | (i, o) <-
      [ (" foo", "foo"), ("foo ", "foo"), (" foo ", "foo"), ("\tfoo\t", "foo")
      , (" \t foo bar \t ", "foo bar"), ("   ", ""), ("\t", ""), ("  \t  ", "")
      ]
  ]
  ++
  [ ("mkFieldValue accepts the empty value. " ++ q55grammar ++ " (*field-content allows zero)",
     fmap fieldValueBytes (mkFieldValue "") == Right "")
  , ("mkFieldValue keeps an HTAB between visible characters (\"a<HTAB>b\"); HTAB is not one of the \"other CTL characters\". " ++ q55grammar,
     fmap fieldValueBytes (mkFieldValue "a\tb") == Right "a\tb")
  , ("mkFieldValue keeps internal runs of whitespace (\"a  \\t b\"). " ++ q55grammar,
     fmap fieldValueBytes (mkFieldValue "a  \t b") == Right "a  \t b")
  , ("mkFieldValue accepts obs-text octets 0x80 and 0xFF. " ++ q55grammar,
     fmap fieldValueBytes (mkFieldValue ("a" <> w 0x80 <> w 0xFF <> "b")) == Right ("a" <> w 0x80 <> w 0xFF <> "b"))
  , ("mkFieldValue accepts every obs-text octet 0x80-0xFF. " ++ q55grammar,
     fmap fieldValueBytes (mkFieldValue (B.pack [0x80 .. 0xFF])) == Right (B.pack [0x80 .. 0xFF]))
  , ("mkFieldValue accepts every VCHAR 0x21-0x7E, unchanged. " ++ q55grammar,
     fmap fieldValueBytes (mkFieldValue (B.pack [0x21 .. 0x7E])) == Right (B.pack [0x21 .. 0x7E]))
  , ("mkFieldValue accepts a value with commas, quotes and parameters unchanged (text/html; charset=\"utf-8\"). " ++ q55grammar,
     fmap fieldValueBytes (mkFieldValue "text/html; charset=\"utf-8\"") == Right "text/html; charset=\"utf-8\"")
  ]
  ++
  concat
  [ [ ("mkFieldValue never yields a value containing CR, LF or NUL (input " ++ show i ++ "). " ++ q55crlf,
       either (const True) (B.all (\c -> c /= 0x0D && c /= 0x0A && c /= 0x00) . fieldValueBytes) (mkFieldValue i))
    , ("POLICY: mkFieldValue rejects " ++ show i ++ " with ContainsCRLFNUL rather than replacing with SP (documented choice). " ++ q55crlf,
       isLeftWith ContainsCRLFNUL (mkFieldValue i))
    ]
  | i <- [ "\r", "\n", w 0, "a\rb", "a\nb", "a" <> w 0 <> "b", "\rabc", "abc\r", "abc\r\n", "abc\n"
         , "a\r\n b", " \r ", w 0 <> "abc", "abc" <> w 0, "\r\n" ]
  ]
  ++
  [ ("POLICY: mkFieldValue rejects the control character " ++ hex2 b ++ " with ContainsCTL (documented choice not to retain). " ++ q55ctl,
     isLeftWith ContainsCTL (mkFieldValue ("a" <> w b <> "b")))
  | b <- otherCtls
  ]
  ++
  [ ("POLICY: mkFieldValue rejects a leading control character 0x01 (it is not whitespace, so it is not stripped). " ++ q55ctl ++ "; " ++ q55strip,
     isLeftWith ContainsCTL (mkFieldValue (w 0x01 <> "abc")))
  , ("POLICY: mkFieldValue rejects a trailing DEL (0x7F; CTL in RFC 5234 is %x00-1F / %x7F). " ++ q55ctl,
     isLeftWith ContainsCTL (mkFieldValue ("abc" <> w 0x7F)))
  ]

------------------------------------------------------------------------
-- combineValues (RFC 9110 Sections 5.2, 5.3)

combined :: [ByteString] -> Maybe (Maybe ByteString)
combined xs = fmap (fmap fieldValueBytes . combineValues) (traverse fv xs)

noOuterWS :: ByteString -> Bool
noOuterWS b = B.null b || (not (isWS (B.head b)) && not (isWS (B.last b)))

combineTests :: [T]
combineTests =
  [ ("combineValues gives \"Foo, Bar, Baz\" for the lines \"Foo, Bar\" and \"Baz\". RFC 9110 Section 5.2 example: \"The field value for \"Example-Field\" is the list \"Foo, Bar, Baz\".\"; " ++ q53,
     combined ["Foo, Bar", "Baz"] == Just (Just "Foo, Bar, Baz"))
  , ("combineValues of a single line is that line. RFC 9110 Section 5.2: \"When a field name is only present once in a section, the combined \"field value\" for that field consists of the corresponding field line value.\"",
     combined ["a; q=1"] == Just (Just "a; q=1"))
  , ("combineValues keeps the order of the lines (\"b\" then \"a\" gives \"b, a\"). " ++ q52 ++ "; " ++ q53order,
     combined ["b", "a"] == Just (Just "b, a"))
  , ("combineValues of three lines joins all three in order. " ++ q52 ++ "; " ++ q53,
     combined ["a", "b", "c"] == Just (Just "a, b, c"))
  , ("POLICY: combineValues of no lines is Nothing (documented).",
     combineValues [] == Nothing)
  ]
  ++
  [ ("combineValues of " ++ show xs ++ " is a valid field value, with no leading or trailing whitespace (the FieldValue invariant). RFC 9110 Section 5.5: \"A field value does not include leading or trailing whitespace.\"; " ++ q52,
     fmap (fmap noOuterWS) (combined xs) == Just (Just True))
  | xs <- [ ["a", ""], ["", "a"], ["", ""], ["", "", ""], ["a", "", "b"], ["a", "", ""] ]
  ]
  ++
  [ ("combineValues of " ++ show xs ++ " has exactly the list elements of the separate lines, in order. " ++ q52 ++ "; " ++ q5612,
     (do vs <- traverse fv xs
         c <- combineValues vs
         let sep = traverse (splitList NoComments) vs
         pure (fmap concat sep == splitList NoComments c && isRight sep)) == Just True)
  | xs <- [ ["a, b", "c"], ["\"x,y\"", "z"], ["a,", ",b"], ["", "a"], ["a", ""], ["W/\"1\"", "\"2\""] ]
  ]

------------------------------------------------------------------------
-- Field sections

fieldsTests :: [T]
fieldsTests =
  [ ("fieldsToList returns the field lines of fieldsFromList in order, including repeated names. " ++ q53order,
     (do fs <- traverse (\(n, v) -> (,) <$> mkFieldName n <*> fv v) [("a", "1"), ("b", "2"), ("a", "3")]
         pure (fieldsToList (fieldsFromList fs) == fs)) == Just True)
  , ("fieldsToList emptyFields is empty. RFC 9110 Section 5.2: \"Field sections are composed of any number of \"field lines\"\"",
     null (fieldsToList emptyFields))
  , ("appendField adds the new line after the existing ones. " ++ q53order,
     (do a <- mkFieldName "a"; b <- mkFieldName "b"; v1 <- fv "1"; v2 <- fv "2"
         pure (fieldsToList (appendField b v2 (fieldsFromList [(a, v1)])) == [(a, v1), (b, v2)])) == Just True)
  , ("appendField of a name already present keeps both lines, the new one last. " ++ q53order,
     (do a <- mkFieldName "a"; v1 <- fv "1"; v2 <- fv "2"
         pure (lookupAll a (appendField a v2 (fieldsFromList [(a, v1)])) == [v1, v2])) == Just True)
  , ("lookupAll returns every line with the name, in order, skipping other names. " ++ q53order ++ "; " ++ q52,
     (do a <- mkFieldName "a"; fs <- fieldsOf [("A", "1"), ("b", "2"), ("a", "3"), ("c", "4"), ("A", "5")]
         vs <- traverse fv ["1", "3", "5"]
         pure (lookupAll a fs == vs)) == Just True)
  , ("lookupAll finds lines whatever the case of the name they were given with. " ++ qCaseIns,
     (do n <- mkFieldName "X-Foo"; fs <- fieldsOf [("x-foo", "1"), ("X-FOO", "2")]
         pure (length (lookupAll n fs) == 2)) == Just True)
  , ("lookupAll of an absent name is empty. RFC 9110 Section 5.2",
     (do n <- mkFieldName "absent"; fs <- fieldsOf [("a", "1")]; pure (null (lookupAll n fs))) == Just True)
  , ("lookupFirst returns the first of several lines. " ++ q53order,
     (do n <- mkFieldName "a"; fs <- fieldsOf [("a", "1"), ("a", "2")]
         pure (fmap fieldValueBytes (lookupFirst n fs) == Just "1")) == Just True)
  , ("lookupFirst of an absent name is Nothing.",
     (do n <- mkFieldName "a"; fs <- fieldsOf [("b", "1")]
         pure (isNothing (lookupFirst n fs))) == Just True)
  , ("lookupSingleton of an absent field is Right Nothing. RFC 9110 Section 5.5: \"Fields that only anticipate a single member as the field value are referred to as \"singleton fields\".\"",
     (do n <- mkFieldName "content-type"; fs <- fieldsOf [("a", "1")]
         pure (lookupSingleton n fs == Right Nothing)) == Just True)
  , ("lookupSingleton of a field given once returns its value. RFC 9110 Section 5.5 (singleton fields)",
     (do n <- mkFieldName "content-type"; fs <- fieldsOf [("a", "1"), ("Content-Type", "text/plain"), ("b", "2")]
         v <- fv "text/plain"
         pure (lookupSingleton n fs == Right (Just v))) == Just True)
  , ("lookupSingleton of a field on two lines is an error, not a choice. " ++ q53sender ++ "; RFC 9110 Section 5.3: \"later header field lines might include ... deliberately misleading duplicate header fields\"",
     (do n <- mkFieldName "host"; fs <- fieldsOf [("Host", "a.example"), ("x", "1"), ("host", "b.example")]
         pure (lookupSingleton n fs == Left (RepeatedField n))) == Just True)
  , ("POLICY: lookupSingleton reports a repeated singleton even when both lines carry the same value (documented: \"More than one field line with the name is an error\"). " ++ q53sender,
     (do n <- mkFieldName "content-type"; fs <- fieldsOf [("content-type", "text/plain"), ("content-type", "text/plain")]
         pure (lookupSingleton n fs == Left (RepeatedField n))) == Just True)
  ]

------------------------------------------------------------------------
-- splitList (RFC 9110 Section 5.6.1)

splitTests :: [T]
splitTests =
  [ ("splitList \"foo,bar\" gives [foo, bar]. " ++ q5612ex,
     split NoComments "foo,bar" == Just (Right ["foo", "bar"]))
  , ("splitList \"foo ,bar,\" gives [foo, bar]. " ++ q5612ex,
     split NoComments "foo ,bar," == Just (Right ["foo", "bar"]))
  , ("splitList \"foo , ,bar,charlie\" gives [foo, bar, charlie]. " ++ q5612ex,
     split NoComments "foo , ,bar,charlie" == Just (Right ["foo", "bar", "charlie"]))
  , ("splitList of \"\" has no elements. RFC 9110 Section 5.6.1.2: \"Empty elements do not contribute to the count of elements present.\" (documented: \"The result may be empty\")",
     split NoComments "" == Just (Right []))
  , ("splitList of \",\" has no elements. RFC 9110 Section 5.6.1.2: \"Empty elements do not contribute to the count of elements present.\"",
     split NoComments "," == Just (Right []))
  , ("splitList of \",   ,\" has no elements. RFC 9110 Section 5.6.1.2: \"Empty elements do not contribute to the count of elements present.\"",
     split NoComments ",   ," == Just (Right []))
  , ("splitList removes HTAB as well as SP around commas. RFC 9110 Section 5.6.1: \"each separated by a single comma (\",\") and optional whitespace (OWS ...)\"; Section 5.6.3: \"OWS = *( SP / HTAB )\"",
     split NoComments "a\t,\tb" == Just (Right ["a", "b"]))
  , ("splitList skips leading empty elements (\", , a\"). " ++ q5612,
     split NoComments ", , a" == Just (Right ["a"]))
  , ("splitList keeps whitespace inside an element (\"a b, c\" has 2 elements, the first \"a b\"). RFC 9110 Section 5.6.1.2: \"#element => [ element ] *( OWS \",\" OWS [ element ] )\" (only OWS around commas is list syntax)",
     split NoComments "a b, c" == Just (Right ["a b", "c"]))
  , ("splitList accepts 10000 empty elements before an element. " ++ q5612,
     split NoComments (B.replicate 10000 0x2C <> "a") == Just (Right ["a"]))
  , ("splitList accepts 5000 empty elements separated by OWS (\", \" repeated). " ++ q5612,
     split NoComments (B.concat (replicate 5000 ", ") <> "a") == Just (Right ["a"]))
  , ("splitList does not split at a comma inside a quoted-string: the 5.5 Example-URIs value has 2 elements. " ++ q55comma,
     splitCount NoComments "\"http://example.com/a.html,foo\", \"http://without-a-comma.example.com/\"" == Just (Right 2))
  , ("splitList keeps a quoted element whole: \"\\\"a,b\\\", c\" gives [\"\\\"a,b\\\"\", \"c\"] (element returned as written). " ++ q55comma,
     split NoComments "\"a,b\", c" == Just (Right ["\"a,b\"", "c"]))
  , ("splitList: the 5.5 Example-Dates value \"\\\"Sat, 04 May 1996\\\", \\\"Wed, 14 Sep 2005\\\"\" has 2 elements. RFC 9110 Section 5.5: \"Example-Dates: \"Sat, 04 May 1996\", \"Wed, 14 Sep 2005\"\"",
     splitCount NoComments "\"Sat, 04 May 1996\", \"Wed, 14 Sep 2005\"" == Just (Right 2))
  , ("splitList: an escaped DQUOTE does not end a quoted-string (\"\\\"a\\\\\\\",b\\\", c\" has 2 elements). " ++ q564pair,
     splitCount NoComments "\"a\\\",b\", c" == Just (Right 2))
  , ("splitList: an escaped backslash before DQUOTE does not escape the DQUOTE (\"\\\"a\\\\\\\\\\\", b\" has 2 elements). " ++ q564pair,
     split NoComments "\"a\\\\\", b" == Just (Right ["\"a\\\\\"", "b"]))
  , ("splitList keeps whitespace and commas inside quotes (\"\\\" a , b \\\"\" is one element). " ++ q564,
     split NoComments "\" a , b \"" == Just (Right ["\" a , b \""]))
  , ("splitList rejects an unterminated quoted-string. " ++ q564,
     split NoComments "\"abc" == Just (Left UnterminatedQuotedString))
  , ("splitList rejects an unterminated quoted-string in a later element. " ++ q564,
     split NoComments "a, \"b" == Just (Left UnterminatedQuotedString))
  , ("splitList rejects a quoted-string whose closing DQUOTE is escaped. " ++ q564pair,
     split NoComments "\"abc\\\"" == Just (Left UnterminatedQuotedString))
  , ("splitList NoComments treats parentheses as ordinary characters, so \"a (x, y), b\" has 3 elements. " ++ q565only,
     splitCount NoComments "a (x, y), b" == Just (Right 3))
  , ("splitList NoComments accepts an unmatched \")\". " ++ q565only,
     splitCount NoComments "a ), b" == Just (Right 2))
  , ("splitList NoComments accepts an unmatched \"(\". " ++ q565only,
     splitCount NoComments "a (, b" == Just (Right 2))
  , ("splitList on the If-None-Match example \"\\\"xyzzy\\\", \\\"r2d2xxxx\\\", \\\"c3piozzzz\\\"\" gives 3 elements. RFC 9110 Section 13.1.2: \"If-None-Match = \"*\" / #entity-tag\" and example \"If-None-Match: \"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"\"",
     splitCount NoComments "\"xyzzy\", \"r2d2xxxx\", \"c3piozzzz\"" == Just (Right 3))
  , ("splitList on the weak If-None-Match example gives 3 elements. RFC 9110 Section 13.1.2 example: \"If-None-Match: W/\"xyzzy\", W/\"r2d2xxxx\", W/\"c3piozzzz\"\"",
     split NoComments "W/\"xyzzy\", W/\"r2d2xxxx\", W/\"c3piozzzz\"" == Just (Right ["W/\"xyzzy\"", "W/\"r2d2xxxx\"", "W/\"c3piozzzz\""]))
  -- Changed after the review: splitList is not for entity tags (it now says
  -- so), and splitEntityTags was added for them.
  , ("splitEntityTags on the entity-tag list \"\\\"a\\\\\\\", \\\"b\\\"\" gives 2 elements: in an entity-tag a backslash is an ordinary etagc, not an escape, so the first tag is \"a\\\" and the comma separates. RFC 9110 Section 8.8.3: \"opaque-tag = DQUOTE *etagc DQUOTE; etagc = %x21 / %x23-7E / obs-text\" (0x5C is in %x23-7E); Section 13.1.2: \"If-None-Match = \"*\" / #entity-tag\". The documentation offers splitList for any \"list-based field value\".",
     (fmap length . splitEntityTags <$> fv "\"a\\\", \"b\"") == Just (Right 2))
  , ("splitList AllowComments does not split at a comma inside a comment (\"a (x, y), b\" gives [\"a (x, y)\", \"b\"]). " ++ q565,
     split AllowComments "a (x, y), b" == Just (Right ["a (x, y)", "b"]))
  , ("splitList AllowComments on the Via example \"1.0 fred, 1.1 p.example.net\" gives 2 elements. RFC 9110 Section 7.6.3: \"Via = #( received-protocol RWS received-by [ RWS comment ] )\", example \"Via: 1.0 fred, 1.1 p.example.net\"",
     split AllowComments "1.0 fred, 1.1 p.example.net" == Just (Right ["1.0 fred", "1.1 p.example.net"]))
  , ("splitList AllowComments on a Via value with a comment containing a comma gives 2 elements. RFC 9110 Section 7.6.3: \"Via = #( received-protocol RWS received-by [ RWS comment ] )\"; " ++ q565,
     splitCount AllowComments "1.0 fred (Apache, mod_proxy), 1.1 p.example.net" == Just (Right 2))
  , ("splitList AllowComments handles nested comments (\"a (x (y, z) w), b\" has 2 elements). " ++ q565 ++ " (comment may contain comment)",
     splitCount AllowComments "a (x (y, z) w), b" == Just (Right 2))
  , ("splitList AllowComments: an escaped \")\" does not close a comment (\"a (x \\\\) y, z), b\" has 2 elements). " ++ q565 ++ "; " ++ q564pair,
     splitCount AllowComments "a (x \\) y, z), b" == Just (Right 2))
  , ("splitList AllowComments: an escaped \"(\" does not open a nested comment (\"a (x \\\\( y, z), b\" has 2 elements). " ++ q565,
     splitCount AllowComments "a (x \\( y, z), b" == Just (Right 2))
  , ("splitList AllowComments: DQUOTE inside a comment is ctext, not the start of a quoted-string (\"(\\\"), b\" has 2 elements). " ++ q565 ++ " (%x22 is in %x21-27)",
     split AllowComments "(\"), b" == Just (Right ["(\")", "b"]))
  , ("splitList AllowComments: a comment containing DQUOTE and a comma closes at its \")\" (\"(a \\\"b, c\\\") , d\" has 2 elements). " ++ q565,
     splitCount AllowComments "(a \"b, c\") , d" == Just (Right 2))
  , ("splitList AllowComments: \"(\" inside a quoted-string does not open a comment (\"\\\"(\\\", b\" has 2 elements). " ++ q564 ++ " (\"(\" is qdtext)",
     splitCount AllowComments "\"(\", b" == Just (Right 2))
  , ("splitList AllowComments: \")\" inside a quoted-string is not an unmatched parenthesis. " ++ q564,
     splitCount AllowComments "\")\", b" == Just (Right 2))
  , ("splitList AllowComments rejects an unterminated comment. " ++ q565,
     split AllowComments "a (x, b" == Just (Left UnterminatedComment))
  , ("splitList AllowComments rejects an unterminated nested comment. " ++ q565,
     split AllowComments "a ((x), b" == Just (Left UnterminatedComment))
  , ("splitList AllowComments rejects a comment whose only \")\" is escaped. " ++ q565 ++ "; " ++ q564pair,
     split AllowComments "a (x \\), b" == Just (Left UnterminatedComment))
  , ("splitList AllowComments rejects an unmatched \")\". " ++ q565,
     split AllowComments "a ), b" == Just (Left UnexpectedCloseParen))
  , ("splitList AllowComments rejects an extra \")\" after a comment. " ++ q565,
     split AllowComments "a (x)), b" == Just (Left UnexpectedCloseParen))
  , ("splitList AllowComments rejects a lone \")\". " ++ q565,
     split AllowComments ")" == Just (Left UnexpectedCloseParen))
  , ("splitList AllowComments still honours quoted-strings (\"\\\"a,b\\\", c\" has 2 elements). " ++ q564,
     splitCount AllowComments "\"a,b\", c" == Just (Right 2))
  ]

------------------------------------------------------------------------
-- fieldListElements

listOf :: Comments -> ByteString -> [(ByteString, ByteString)] -> Maybe (Either FieldSyntaxError [ByteString])
listOf c n hs = fieldListElements c <$> mkFieldName n <*> fieldsOf hs

fieldListTests :: [T]
fieldListTests =
  [ ("fieldListElements of the lines \"Foo, Bar\" and \"Baz\" is [Foo, Bar, Baz]. RFC 9110 Section 5.2: \"The field value for \"Example-Field\" is the list \"Foo, Bar, Baz\".\"",
     listOf NoComments "example-field" [("Example-Field", "Foo, Bar"), ("Example-Field", "Baz")] == Just (Right ["Foo", "Bar", "Baz"]))
  , ("fieldListElements of an absent field is empty. RFC 9110 Section 5.6.1.2 (empty lists)",
     listOf NoComments "vary" [("a", "b")] == Just (Right []))
  , ("fieldListElements skips lines that are empty or only commas. " ++ q5612 ++ "; RFC 9110 Section 5.6.1.2: \"enough to handle common mistakes by senders that merge values\"",
     listOf NoComments "vary" [("vary", ""), ("vary", "accept"), ("vary", ","), ("vary", ", origin ,")] == Just (Right ["accept", "origin"]))
  , ("fieldListElements ignores lines of other fields between lines of this one, and keeps order. " ++ q53order,
     listOf NoComments "cache-control" [("Cache-Control", "no-cache"), ("x", "1"), ("cache-control", "max-age=0, private")] == Just (Right ["no-cache", "max-age=0", "private"]))
  , ("fieldListElements matches the name case-insensitively. " ++ qCaseIns,
     listOf NoComments "VARY" [("vary", "a"), ("Vary", "b")] == Just (Right ["a", "b"]))
  , ("fieldListElements keeps a quoted comma inside its element across lines. " ++ q55comma,
     fmap (fmap length) (listOf NoComments "x-u" [("x-u", "\"a,b\""), ("x-u", "c, \"d,e\"")]) == Just (Right 3))
  , ("fieldListElements with AllowComments keeps a Via comment containing a comma in one element. RFC 9110 Section 7.6.3 (Via grammar includes comment); " ++ q565,
     fmap (fmap length) (listOf AllowComments "via" [("via", "1.0 fred (a, b)"), ("via", "1.1 p.example.net")]) == Just (Right 2))
  , ("fieldListElements reports a syntax error in any line. " ++ q564,
     fmap isLeft (listOf NoComments "x" [("x", "a"), ("x", "\"b")]) == Just True)
  ]

------------------------------------------------------------------------
-- parseQuotedString (RFC 9110 Section 5.6.4)

qdtextOctets :: [Word8]
qdtextOctets = [0x09, 0x20, 0x21] ++ [0x23 .. 0x5B] ++ [0x5D .. 0x7E] ++ [0x80 .. 0xFF]

quotedPairOctets :: [Word8]
quotedPairOctets = [0x09, 0x20] ++ [0x21 .. 0x7E] ++ [0x80 .. 0xFF]

quotedStringTests :: [T]
quotedStringTests =
  [ ("parseQuotedString \"\\\"abc\\\"rest\" gives (abc, rest). " ++ q564,
     parseQuotedString "\"abc\"rest" == Right ("abc", "rest"))
  , ("parseQuotedString of the empty quoted-string gives (\"\", \"\"). " ++ q564 ++ " (*( qdtext / quoted-pair ) may be empty)",
     parseQuotedString "\"\"" == Right ("", ""))
  , ("parseQuotedString stops at the first unescaped DQUOTE (\"\\\"a\\\"\\\"b\\\"\" gives (a, \"\\\"b\\\"\")). " ++ q564,
     parseQuotedString "\"a\"\"b\"" == Right ("a", "\"b\""))
  , ("parseQuotedString unescapes an escaped DQUOTE. " ++ q564pair,
     parseQuotedString "\"a\\\"b\"" == Right ("a\"b", ""))
  , ("parseQuotedString unescapes an escaped backslash. " ++ q564pair,
     parseQuotedString "\"a\\\\b\"" == Right ("a\\b", ""))
  , ("parseQuotedString unescapes a quoted-pair of an ordinary letter (\\a gives a). " ++ q564pair,
     parseQuotedString "\"\\a\"" == Right ("a", ""))
  , ("parseQuotedString unescapes a quoted-pair of HTAB. " ++ q564pair,
     parseQuotedString "\"a\\\tb\"" == Right ("a\tb", ""))
  , ("parseQuotedString unescapes a quoted-pair of SP. " ++ q564pair,
     parseQuotedString "\"a\\ b\"" == Right ("a b", ""))
  , ("parseQuotedString unescapes a quoted-pair of obs-text 0x80. " ++ q564pair,
     parseQuotedString ("\"\\" <> w 0x80 <> "\"") == Right (w 0x80, ""))
  , ("parseQuotedString accepts every qdtext octet (HTAB, SP, 0x21, 0x23-5B, 0x5D-7E, 0x80-FF) unchanged. " ++ q564,
     parseQuotedString ("\"" <> B.pack qdtextOctets <> "\"") == Right (B.pack qdtextOctets, ""))
  , ("parseQuotedString accepts a quoted-pair of every allowed octet (HTAB, SP, VCHAR, obs-text) and replaces each with the octet. " ++ q564pair,
     parseQuotedString ("\"" <> B.pack (concatMap (\b -> [0x5C, b]) quotedPairOctets) <> "\"") == Right (B.pack quotedPairOctets, ""))
  , ("parseQuotedString keeps a comma, semicolon and parentheses inside the string. " ++ q564,
     parseQuotedString "\"a, b; (c)\"; x" == Right ("a, b; (c)", "; x"))
  , ("parseQuotedString rejects input that does not start with DQUOTE. " ++ q564,
     isLeft (parseQuotedString "abc"))
  , ("parseQuotedString rejects input that starts with whitespace before DQUOTE (the documentation says it parses \"at the start of the input\"). " ++ q564,
     isLeft (parseQuotedString " \"abc\""))
  , ("parseQuotedString rejects the empty input. " ++ q564,
     isLeft (parseQuotedString ""))
  , ("parseQuotedString rejects a lone DQUOTE as unterminated. " ++ q564,
     parseQuotedString "\"" == Left UnterminatedQuotedString)
  , ("parseQuotedString rejects \"\\\"abc\" as unterminated. " ++ q564,
     parseQuotedString "\"abc" == Left UnterminatedQuotedString)
  , ("parseQuotedString rejects a string whose closing DQUOTE is escaped. " ++ q564pair,
     parseQuotedString "\"abc\\\"" == Left UnterminatedQuotedString)
  , ("parseQuotedString rejects a string ending in a lone backslash. " ++ q564pair,
     parseQuotedString "\"abc\\" == Left UnterminatedQuotedString)
  ]
  ++
  [ ("parseQuotedString rejects the control octet " ++ hex2 b ++ " in a quoted-string (not qdtext). " ++ q564,
     parseQuotedString ("\"a" <> w b <> "b\"") == Left InvalidQuotedString)
  | b <- ctlsNotHTAB
  ]
  ++
  [ ("parseQuotedString rejects a quoted-pair of the control octet " ++ hex2 b ++ ". " ++ q564pair,
     parseQuotedString ("\"a\\" <> w b <> "b\"") == Left InvalidQuotedString)
  | b <- ctlsNotHTAB
  ]

------------------------------------------------------------------------
-- quoteString

quoteStringTests :: [T]
quoteStringTests =
  [ ("quoteString \"abc\" is \"\\\"abc\\\"\". " ++ q564 ++ "; " ++ q564sender,
     quoteString "abc" == Just "\"abc\"")
  , ("quoteString of the empty string is \"\\\"\\\"\". " ++ q564,
     quoteString "" == Just "\"\"")
  , ("quoteString escapes DQUOTE. " ++ q564sender,
     quoteString "a\"b" == Just "\"a\\\"b\"")
  , ("quoteString escapes backslash. " ++ q564sender,
     quoteString "a\\b" == Just "\"a\\\\b\"")
  , ("quoteString does not escape parentheses, comma, semicolon or equals. " ++ q564sender,
     quoteString "a(b),c;d=e" == Just "\"a(b),c;d=e\"")
  , ("quoteString keeps HTAB and SP unescaped. " ++ q564 ++ "; " ++ q564sender,
     quoteString "a\tb c" == Just "\"a\tb c\"")
  , ("quoteString carries obs-text unescaped. " ++ q564 ++ "; " ++ q564sender,
     quoteString ("a" <> w 0x80 <> w 0xFF) == Just ("\"a" <> w 0x80 <> w 0xFF <> "\""))
  ]
  ++
  [ ("quoteString refuses the control octet " ++ hex2 b ++ ", which neither qdtext nor quoted-pair can carry. " ++ q564 ++ "; " ++ q564pair,
     quoteString ("a" <> w b) == Nothing)
  | b <- ctlsNotHTAB
  ]
  ++
  [ ("parseQuotedString (quoteString " ++ show s ++ ") gives back the value with nothing left over. " ++ q564pair,
     fmap parseQuotedString (quoteString s) == Just (Right (s, "")))
  | s <- ["", "abc", "a\"b", "a\\b", "\\\"", "\"\"", "a\tb", " ", "a,b;c=d", "(x)", "../../etc/passwd", w 0x80 <> w 0xFF, B.pack qdtextOctets, B.pack quotedPairOctets]
  ]

------------------------------------------------------------------------
-- parseParameters / lookupParameter (RFC 9110 Section 5.6.6)

parameterTests :: [T]
parameterTests =
  [ ("parseParameters of \"\" is []. " ++ q566 ++ " (*( ... ) may be empty)",
     parseParameters "" == Right [])
  , ("parseParameters \"; q=0.5\" gives [(q, 0.5)]. " ++ q566,
     parseParameters "; q=0.5" == Right [("q", "0.5")])
  , ("parseParameters \";charset=utf-8\" gives [(charset, utf-8)]. " ++ q566,
     parseParameters ";charset=utf-8" == Right [("charset", "utf-8")])
  , ("parseParameters lower-cases names but keeps the case of values (\"; Charset=UTF-8\"). " ++ q566case,
     parseParameters "; Charset=UTF-8" == Right [("charset", "UTF-8")])
  , ("parseParameters returns several parameters in order. " ++ q566,
     parseParameters ";a=b;c=d" == Right [("a", "b"), ("c", "d")])
  , ("parseParameters allows OWS before and after \";\". " ++ q566,
     parseParameters " ; a=b ; c=d" == Right [("a", "b"), ("c", "d")])
  , ("parseParameters allows HTAB as OWS. " ++ q566 ++ "; RFC 9110 Section 5.6.3: \"OWS = *( SP / HTAB )\"",
     parseParameters "\t;\ta=b" == Right [("a", "b")])
  , ("parseParameters skips empty parameters (\";;\"). " ++ q566 ++ " ([ parameter ] is optional)",
     parseParameters ";;" == Right [])
  , ("parseParameters skips empty parameters between and after others. " ++ q566,
     parseParameters "; ;a=b;;c=d;" == Right [("a", "b"), ("c", "d")])
  , ("parseParameters accepts a lone \";\" followed by OWS. " ++ q566,
     parseParameters "; " == Right [])
  , ("parseParameters unquotes a quoted value with a space. " ++ q566 ++ "; " ++ q566equiv,
     parseParameters ";a=\"b c\"" == Right [("a", "b c")])
  , ("parseParameters unescapes a quoted-pair in a value. " ++ q564pair,
     parseParameters ";a=\"b\\\"c\"" == Right [("a", "b\"c")])
  , ("parseParameters: a quoted value may contain \";\" and \"=\" without starting a new parameter. " ++ q566 ++ "; " ++ q564,
     parseParameters ";a=\"x;y=z\";b=c" == Right [("a", "x;y=z"), ("b", "c")])
  , ("parseParameters accepts the empty quoted value. " ++ q566 ++ "; " ++ q564,
     parseParameters ";a=\"\"" == Right [("a", "")])
  , ("parseParameters: a token value and the same value quoted are equal. " ++ q566equiv,
     parseParameters ";a=\"b\"" == parseParameters ";a=b" && isRight (parseParameters ";a=b"))
  , ("parseParameters accepts obs-text in a quoted value. " ++ q564,
     parseParameters (";a=\"" <> w 0x80 <> "\"") == Right [("a", w 0x80)])
  , ("parseParameters accepts a value with every non-alphanumeric tchar. " ++ q566 ++ "; " ++ qTchar,
     parseParameters ";a=!#$%&'*+-.^_`|~" == Right [("a", "!#$%&'*+-.^_`|~")])
  , ("parseParameters returns a repeated name twice, in order (documented). " ++ q566,
     parseParameters ";A=1;a=2" == Right [("a", "1"), ("a", "2")])
  , ("parseParameters rejects whitespace before \"=\". " ++ q566ws,
     isLeft (parseParameters ";a =b"))
  , ("parseParameters rejects whitespace after \"=\". " ++ q566ws,
     isLeft (parseParameters ";a= b"))
  , ("parseParameters rejects whitespace on both sides of \"=\". " ++ q566ws,
     isLeft (parseParameters ";a = b"))
  , ("parseParameters rejects a parameter with no \"=\" and value. " ++ q566,
     isLeft (parseParameters ";a"))
  , ("parseParameters rejects an empty value after \"=\". " ++ q566 ++ "; " ++ qFieldName,
     isLeft (parseParameters ";a="))
  , ("parseParameters rejects an empty name. " ++ q566,
     isLeft (parseParameters ";=b"))
  , ("parseParameters rejects a value followed by a space and more text. " ++ q566,
     isLeft (parseParameters ";a=b c"))
  , ("parseParameters rejects a delimiter \"/\" in a token value. " ++ q566 ++ "; " ++ qDelims,
     isLeft (parseParameters ";a=b/c"))
  , ("parseParameters rejects a comma in a token value. " ++ q566 ++ "; " ++ qDelims,
     isLeft (parseParameters ";a=b,c"))
  , ("parseParameters rejects obs-text in a token value. " ++ q566 ++ "; " ++ qTchar,
     isLeft (parseParameters (";a=b" <> w 0x80)))
  , ("parseParameters rejects a delimiter \"@\" in a name. " ++ q566 ++ "; " ++ qDelims,
     isLeft (parseParameters ";a@=b"))
  , ("parseParameters rejects text after a closing DQUOTE. " ++ q566,
     isLeft (parseParameters ";a=\"x\"y"))
  , ("parseParameters rejects an unterminated quoted value. " ++ q564,
     isLeft (parseParameters ";a=\"x"))
  , ("parseParameters rejects input that does not begin with \";\" (\"a=b\"). " ++ q566,
     isLeft (parseParameters "a=b"))
  , ("parseParameters rejects a second \"=\" in a token value (\"a=b=c\"). " ++ q566 ++ "; " ++ qDelims,
     isLeft (parseParameters ";a=b=c"))
  , ("lookupParameter compares names case-insensitively (\"CHARSET\" finds charset). " ++ q566case,
     lookupParameter "CHARSET" [("charset", "utf-8")] == Right (Just "utf-8"))
  , ("lookupParameter of an absent name is Right Nothing. " ++ q566,
     lookupParameter "q" [("charset", "utf-8")] == Right Nothing)
  , ("lookupParameter of a name present once among others returns its value. " ++ q566,
     (lookupParameter "q" =<< parseParameters ";a=1;q=0.5;b=2") == Right (Just "0.5"))
  , ("POLICY: lookupParameter treats a repeated parameter as an error (documented example charset=utf-8;charset=utf-7). " ++ q566case,
     (lookupParameter "charset" =<< parseParameters ";charset=utf-8;charset=utf-7") == Left (DuplicateParameter "charset"))
  , ("POLICY: lookupParameter detects a repetition that differs only in the case of the name, reporting the lower-case name. " ++ q566case,
     (lookupParameter "Q" =<< parseParameters ";q=1;Q=0") == Left (DuplicateParameter "q"))
  ]

------------------------------------------------------------------------
-- isProtocolField

protocolFieldTests :: [T]
protocolFieldTests =
  [ ("isProtocolField " ++ show n ++ " is True: the field describes the connection or framing. " ++ q761,
     fmap isProtocolField (mkFieldName n) == Just True)
  | n <- ["Connection", "Keep-Alive", "Proxy-Connection", "TE", "Transfer-Encoding", "Upgrade", "connection", "TRANSFER-ENCODING", "te"]
  ]
  ++
  [ ("isProtocolField \"Content-Length\" is True: it is used for framing. RFC 9112 Section 6.2: \"For messages that do include content, the Content-Length field value provides the framing information necessary for determining where the data (and message) ends.\"",
     fmap isProtocolField (mkFieldName "Content-Length") == Just True)
  ]
  ++
  [ ("isProtocolField " ++ show n ++ " is False: an end-to-end field. RFC 9110 Section 7.6.1: \"A sender MUST NOT send a connection option corresponding to a field that is intended for all recipients of the content. For example, Cache-Control is never appropriate as a connection option\"",
     fmap isProtocolField (mkFieldName n) == Just False)
  | n <- ["Cache-Control", "Content-Type", "Content-Encoding", "Date", "Vary", "Set-Cookie", "ETag", "Trailer"]
  ]

------------------------------------------------------------------------
-- Requests

requestTests :: [T]
requestTests =
  [ ("mkRequest accepts OPTIONS with the asterisk form. " ++ q71forms,
     fmap isRight (requestWith OPTIONS (Just TargetAsterisk) []) == Just True)
  , ("mkRequest accepts the asterisk form for an HTTP/1.0 OPTIONS request. " ++ q71forms,
     fmap isRight (mkRequest HTTP_1_0 OPTIONS TargetAsterisk <$> fieldsOf [] <*> pure emptyBody) == Just True)
  , ("mkRequest accepts CONNECT with the CONNECT (authority) form. " ++ q71forms,
     fmap isRight (requestWith CONNECT connTarget []) == Just True)
  ]
  ++
  [ ("mkRequest rejects the asterisk form with method " ++ show (methodBytes m) ++ ". " ++ q71forms,
     fmap (isLeftWith TargetNotValidForMethod) (requestWith m (Just TargetAsterisk) []) == Just True)
  | m <- [GET, HEAD, POST, PUT, DELETE, CONNECT, TRACE, PATCH]
  ]
  ++
  [ ("mkRequest rejects the asterisk form with the lower-case method \"options\", which is not OPTIONS. " ++ q71forms ++ "; " ++ q91case,
     (do m <- mkMethod "options"; r <- requestWith m (Just TargetAsterisk) []; pure (isLeftWith TargetNotValidForMethod r)) == Just True)
  , ("mkRequest rejects the CONNECT form with the lower-case method \"connect\". " ++ q71forms ++ "; " ++ q91case,
     (do m <- mkMethod "connect"; r <- requestWith m connTarget []; pure (isLeftWith TargetNotValidForMethod r)) == Just True)
  ]
  ++
  [ ("mkRequest rejects the CONNECT form with method " ++ show (methodBytes m) ++ ". " ++ q71forms,
     fmap (isLeftWith TargetNotValidForMethod) (requestWith m connTarget []) == Just True)
  | m <- [GET, HEAD, POST, PUT, DELETE, OPTIONS, TRACE, PATCH]
  ]
  ++
  [ ("mkRequest accepts an ordinary target URI with method " ++ show (methodBytes m) ++ ". RFC 9110 Section 7.1 (the special forms are restricted, the ordinary form is not)",
     fmap isRight (requestWith m (httpTarget "/a") []) == Just True)
  | m <- [GET, HEAD, POST, PUT, DELETE, OPTIONS, TRACE, PATCH]
  ]
  ++
  [ ("POLICY: mkRequest rejects the protocol field " ++ show n ++ " with RequestProtocolField (documented: the protocol layer interprets it). " ++ q761,
     (do fname <- mkFieldName n; r <- requestWith POST (httpTarget "/") [("Accept", "*/*"), (n, v)]
         pure (isLeftWith (RequestProtocolField fname) r)) == Just True)
  | (n, v) <- [ ("Connection", "close"), ("Keep-Alive", "timeout=5"), ("Proxy-Connection", "keep-alive")
              , ("TE", "trailers"), ("Transfer-Encoding", "chunked"), ("Upgrade", "websocket")
              , ("Content-Length", "0"), ("content-length", "5"), ("TRANSFER-ENCODING", "chunked") ]
  ]
  ++
  [ ("POLICY: mkRequest rejects a Host field (documented: Host has been folded into the target). RFC 9110 Section 7.2: \"The \"Host\" header field in a request provides the host and port information from the target URI\"",
     fmap isLeft (requestWith GET (httpTarget "/") [("Host", "example.com")]) == Just True)
  , ("POLICY: mkRequest rejects a Host field given in upper case. " ++ qCaseIns,
     fmap isLeft (requestWith GET (httpTarget "/") [("HOST", "other.example")]) == Just True)
  , ("mkRequest keeps ordinary header fields, in order, including repeated names. " ++ q53order,
     (do fs <- fieldsOf hs; r <- requestWith GET (httpTarget "/") hs
         pure (either (const False) (\q -> fieldsToList (requestHeaders q) == fieldsToList fs) r)) == Just True)
  , ("mkRequest keeps the method, version and target it was given. RFC 9110 Section 6.2: \"Request message control data includes a request method ..., request target ..., and protocol version\"",
     (do t <- httpTarget "/x"; r <- requestWith PUT (Just t) []
         pure (either (const False) (\q -> requestMethod q == PUT && requestVersion q == HTTP_1_1 && requestTarget q == t) r)) == Just True)
  , ("POLICY: mkRequest gives an empty path its normal form \"/\" for GET (documented invariant). RFC 9110 Section 4.2.3: \"When not being used as the target of an OPTIONS request, an empty path component is equivalent to an absolute path of \"/\", so the normal form is to provide a path of \"/\" instead.\"",
     (do r <- requestWith GET (httpTarget "") []
         pure (either (const False) (\q -> case requestTarget q of TargetURI _ _ p _ -> pathBytes p == "/"; _ -> False) r)) == Just True)
  , ("POLICY: mkRequest leaves an empty path empty for OPTIONS. RFC 9110 Section 4.2.3: \"When not being used as the target of an OPTIONS request, an empty path component is equivalent to an absolute path of \"/\"\"",
     (do r <- requestWith OPTIONS (httpTarget "") []
         pure (either (const False) (\q -> case requestTarget q of TargetURI _ _ p _ -> pathBytes p == ""; _ -> False) r)) == Just True)
  ]
  where
    hs = [("Accept", "text/html"), ("X-A", "1"), ("Accept", "*/*"), ("Cookie", "a=b")]

------------------------------------------------------------------------
-- Responses

responseTests :: [T]
responseTests =
  [ ("mkResponse rejects the interim status " ++ show c ++ " (a Response is final). RFC 9110 Section 15.2: \"The 1xx (Informational) class of status code indicates an interim response ... prior to completing the requested action and sending a final response.\"",
     fmap (isLeftWith InterimStatus) (responseWith c [] ContentNone) == Just True)
  | c <- [100, 101, 102, 103, 150, 199]
  ]
  ++
  [ ("mkResponse rejects " ++ cname ++ " with status " ++ show s ++ ". " ++ q641 ++ "; RFC 9110 Section " ++ sec ++ ": \"A " ++ show s ++ " response is terminated by the end of the header section; it cannot contain content or trailers.\"",
     fmap (isLeftWith ContentNotAllowed) (responseWith s [] c) == Just True)
  | (s, sec) <- [(204, "15.3.5"), (304, "15.4.5")]
  , (cname, c) <- [ ("non-empty content", ContentBytes "x"), ("zero-length content (which is still content)", ContentBytes "")
                  , ("file content", ContentFile "/srv/x" Nothing), ("streamed content of unknown length", noStream Nothing)
                  , ("streamed content of length 0", noStream (Just 0)) ]
  ]
  ++
  [ ("mkResponse accepts status " ++ show s ++ " with no content. " ++ q641,
     fmap isRight (responseWith s [] ContentNone) == Just True)
  | s <- [204, 304]
  ]
  ++
  [ ("mkResponse rejects 205 with non-empty content. RFC 9110 Section 15.3.6: \"Since the 205 status code implies that no additional content will be provided, a server MUST NOT generate content in a 205 response.\"",
     fmap (isLeftWith ContentNotAllowed) (responseWith 205 [] (ContentBytes "x")) == Just True)
  , ("mkResponse accepts 205 with zero-length content: 205 is not among the statuses that have no content, so its content exists and must be empty, and HTTP/1.1 must frame it (e.g. Content-Length: 0) or the client reads to close. " ++ q641 ++ "; RFC 9112 Section 6.3: \"Any response to a HEAD request and any response with a 1xx (Informational), 204 (No Content), or 304 (Not Modified) status code is always terminated by the first empty line after the header fields\" (205 is not listed) and \"8. Otherwise, this is a response message without a declared message body length, so the message body length is determined by the number of octets received prior to the server closing the connection.\"",
     fmap isRight (responseWith 205 [] (ContentBytes "")) == Just True)
  ]
  ++
  [ ("mkResponse rejects status " ++ show s ++ " with no content at all (ContentNone): such a response includes content, possibly of zero length. " ++ q641,
     fmap (isLeftWith ContentRequired) (responseWith s [] ContentNone) == Just True)
  | s <- [200, 201, 202, 203, 206, 300, 301, 302, 303, 307, 308, 400, 401, 403, 404, 405, 410, 421, 500, 501, 503]
  ]
  ++
  [ ("mkResponse accepts 200 with " ++ cname ++ ". " ++ q641,
     fmap isRight (responseWith 200 [] c) == Just True)
  | (cname, c) <- [ ("zero-length content", ContentBytes ""), ("content", ContentBytes "hello")
                  , ("a whole file", ContentFile "/srv/x" Nothing), ("part of a file", maybe ContentNone (ContentFile "/srv/x" . Just) (mkFileRange 10 20))
                  , ("streamed content of unknown length", noStream Nothing), ("streamed content of known length", noStream (Just 3)) ]
  ]
  ++
  [ ("mkResponse accepts status " ++ show s ++ " with zero-length content. " ++ q641,
     fmap isRight (responseWith s [] (ContentBytes "")) == Just True)
  | s <- [201, 301, 404, 500, 599]
  ]
  ++
  [ ("POLICY: mkResponse rejects the protocol field " ++ show n ++ " with ResponseProtocolField (documented). " ++ q761 ++ "; RFC 9110 Section 8.6: \"a sender MUST NOT forward a message with a Content-Length header field value that is known to be incorrect.\"",
     (do fname <- mkFieldName n; r <- responseWith 200 [("Content-Type", "text/plain"), (n, v)] (ContentBytes "abc")
         pure (isLeftWith (ResponseProtocolField fname) r)) == Just True)
  | (n, v) <- [ ("Connection", "close"), ("Keep-Alive", "timeout=5"), ("Proxy-Connection", "close"), ("TE", "trailers")
              , ("Transfer-Encoding", "chunked"), ("Content-Length", "3"), ("CONTENT-LENGTH", "3"), ("transfer-encoding", "gzip, chunked") ]
  ]
  ++
  -- Changed after the review: until upgrades are supported, a 426, which
  -- must carry Upgrade, is rejected rather than sent without it.
  [ ("POLICY: mkResponse rejects a 426 (Upgrade Required) response, since it cannot carry the Upgrade field it requires; was: mkResponse lets a 426 (Upgrade Required) response carry the Upgrade field it requires; no other part of the API can express the acceptable protocols. RFC 9110 Section 15.5.22: \"The server MUST send an Upgrade header field in a 426 response to indicate the required protocol(s)\"; Section 7.8: \"A server that sends a 426 (Upgrade Required) response MUST send an Upgrade header field to indicate the acceptable protocols, in order of descending preference.\"",
     fmap (isLeftWith UpgradeNotSupported) (responseWith 426 [("Content-Type", "text/plain")] (ContentBytes "This service requires use of the HTTP/3.0 protocol.")) == Just True)
  ]
  ++
  [ ("mkResponse rejects two field lines for the singleton field " ++ show n ++ ". " ++ q53sender,
     (do fname <- mkFieldName n; r <- responseWith 200 [(n, v1), ("X-Other", "1"), (n, v2)] (ContentBytes "x")
         pure (isLeftWith (RepeatedSingleton fname) r)) == Just True)
  | (n, v1, v2) <-
      [ ("Content-Type", "text/plain", "text/html")
      , ("Content-Range", "bytes 0-0/10", "bytes 1-1/10")
      , ("Content-Location", "/a", "/b")
      , ("Location", "/a", "/b")
      , ("ETag", "\"a\"", "\"b\"")
      , ("Last-Modified", "Tue, 15 Nov 1994 12:45:26 GMT", "Wed, 16 Nov 1994 12:45:26 GMT")
      , ("Retry-After", "120", "60")
      , ("Date", "Tue, 15 Nov 1994 08:12:31 GMT", "Wed, 16 Nov 1994 08:12:31 GMT")
      , ("Server", "a/1", "b/2")
      , ("Age", "1", "2")
      , ("Expires", "Thu, 01 Dec 1994 16:00:00 GMT", "Fri, 02 Dec 1994 16:00:00 GMT")
      ]
  ]
  ++
  [ ("mkResponse rejects a singleton repeated under differently-cased names (Content-Type / content-type). " ++ q53sender ++ "; " ++ qCaseIns,
     (do r <- responseWith 200 [("Content-Type", "text/plain"), ("content-type", "text/plain")] (ContentBytes "x")
         pure (isLeftWith (RepeatedSingleton hContentType) r)) == Just True)
  , ("mkResponse accepts several Set-Cookie lines. " ++ q53cookie,
     fmap isRight (responseWith 200 [("Set-Cookie", "a=1; Path=/"), ("Set-Cookie", "b=2; Expires=Wed, 09 Jun 2021 10:18:14 GMT")] (ContentBytes "x")) == Just True)
  ]
  ++
  [ ("mkResponse accepts several lines of the list-based field " ++ show n ++ ". RFC 9110 Section 5.3: \"... unless that field's definition allows multiple field line values to be recombined as a comma-separated list\"; " ++ g,
     fmap isRight (responseWith 200 [(n, v1), (n, v2)] (ContentBytes "x")) == Just True)
  | (n, v1, v2, g) <-
      [ ("Vary", "accept", "accept-encoding", "RFC 9110 Section 12.5.5: \"Vary = #( \"*\" / field-name )\"")
      , ("Cache-Control", "no-cache", "private", "RFC 9111 Section 5.2: \"Cache-Control = #cache-directive\"")
      , ("WWW-Authenticate", "Basic realm=\"a\"", "Bearer", "RFC 9110 Section 11.6.1: \"WWW-Authenticate = #challenge\"")
      , ("Allow", "GET", "HEAD", "RFC 9110 Section 10.2.1: \"Allow = #method\"")
      , ("Content-Encoding", "gzip", "br", "RFC 9110 Section 8.4: \"Content-Encoding = #content-coding\"")
      , ("Content-Language", "en", "fr", "RFC 9110 Section 8.5: \"Content-Language = #language-tag\"")
      , ("Via", "1.0 fred", "1.1 p.example.net", "RFC 9110 Section 7.6.3: \"Via = #( received-protocol RWS received-by [ RWS comment ] )\"")
      , ("Accept-Ranges", "bytes", "none", "RFC 9110 Section 14.3: \"acceptable-ranges = 1#range-unit\"")
      ]
  ]
  ++
  [ ("mkResponse accepts each singleton field given once. " ++ q53sender,
     fmap isRight (responseWith 200 [("Content-Type", "text/plain"), ("ETag", "\"x\""), ("Date", "Tue, 15 Nov 1994 08:12:31 GMT"), ("Server", "s/1"), ("Last-Modified", "Tue, 15 Nov 1994 08:12:31 GMT")] (ContentBytes "x")) == Just True)
  , ("mkResponse keeps the status and the header fields it was given, in order. RFC 9110 Section 6.2 (status code is response control data); " ++ q53order,
     (do s <- mkStatus 404; fs <- fieldsOf rh; let r = mkResponse s fs (ContentBytes "nope")
         pure (either (const False) (\x -> responseStatus x == s && fieldsToList (responseHeaders x) == fieldsToList fs) r)) == Just True)
  ]
  where
    rh = [("Content-Type", "text/plain"), ("Set-Cookie", "a=1"), ("Vary", "a"), ("Set-Cookie", "b=2")]

------------------------------------------------------------------------
-- checkResponseFor

checkFor :: Method -> Maybe Target -> Int -> Content -> Maybe (Either ResponseError ())
checkFor m mt s c = do
  rq <- requestWith m mt [] >>= hush
  rs <- responseWith s [] c >>= hush
  pure (() <$ checkResponseFor rq rs)

q936 :: String
q936 = "RFC 9110 Section 9.3.6: \"Any 2xx (Successful) response indicates that the sender (and all inbound proxies) will switch to tunnel mode immediately after the response header section ... Any response other than a successful response indicates that the tunnel has not yet been formed.\""

checkResponseTests :: [T]
checkResponseTests =
  [ ("POLICY: checkResponseFor rejects a " ++ show s ++ " response to CONNECT with TunnelNotSupported (documented: tunnels not supported). " ++ q936,
     fmap (isLeftWith TunnelNotSupported) (checkFor CONNECT connTarget s c) == Just True)
  | (s, c) <- [(200, ContentBytes ""), (204, ContentNone), (206, ContentBytes ""), (299, ContentBytes "")]
  ]
  ++
  [ ("POLICY: checkResponseFor also rejects a 200 response to an extended CONNECT (CONNECT with an ordinary target URI), which opens a tunnel too. " ++ q936,
     -- Changed after the review: an extended CONNECT only exists in
     -- HTTP/2 and HTTP/3, so the request is now built as HTTP/2.
     (do rq <- mkRequest HTTP_2 CONNECT <$> httpTarget "/chat" <*> fieldsOf [] <*> pure emptyBody >>= hush
         rs <- responseWith 200 [] (ContentBytes "") >>= hush
         pure (isLeftWith TunnelNotSupported (checkResponseFor rq rs))) == Just True)
  ]
  ++
  [ ("checkResponseFor accepts a " ++ show s ++ " response to CONNECT, which does not open a tunnel. " ++ q936,
     fmap isRight (checkFor CONNECT connTarget s (ContentBytes "no")) == Just True)
  | s <- [301, 400, 403, 404, 407, 500, 502, 503]
  ]
  ++
  [ ("checkResponseFor accepts a " ++ show s ++ " response to GET. RFC 9110 Section 6.4.1",
     fmap isRight (checkFor GET (httpTarget "/") s (ContentBytes "x")) == Just True)
  | s <- [200, 206, 404, 500]
  ]
  ++
  [ ("checkResponseFor accepts a 204 response to POST. " ++ q641,
     fmap isRight (checkFor POST (httpTarget "/") 204 ContentNone) == Just True)
  , ("POLICY: checkResponseFor accepts a response to HEAD built as for GET, content and all (documented: the protocol layer omits the content). RFC 9110 Section 9.3.2: \"The server SHOULD send the same header fields in response to a HEAD request as it would have sent if the request method had been GET.\"",
     fmap isRight (checkFor HEAD (httpTarget "/") 200 (ContentBytes "abc")) == Just True)
  , ("checkResponseFor accepts a 200 response to OPTIONS *. RFC 9110 Section 9.3.7",
     fmap isRight (checkFor OPTIONS (Just TargetAsterisk) 200 (ContentBytes "")) == Just True)
  ]

------------------------------------------------------------------------
-- checkTrailers (RFC 9110 Section 6.5)

-- Adapted after the review: a stream now declares its trailer fields,
-- 'mkResponse' rejects a declared field that cannot be a trailer, and
-- 'checkTrailers' takes the response and rejects undeclared fields and
-- repeated singletons. This helper declares the given fields and runs
-- both checks, reporting the offending field as before.
trailerCheck :: [(ByteString, ByteString)] -> Maybe (Either FieldName ())
trailerCheck hs = do
  names <- mapM (mkFieldName . fst) hs
  fs <- fieldsOf hs
  pure $ case mkResponse OK emptyFields (ContentStream Nothing names (\_ _ -> pure emptyFields)) of
    Left (NotATrailer n)       -> Left n
    Left (RepeatedSingleton n) -> Left n
    Left e                     -> error ("trailerCheck: " ++ show e)
    Right r -> case checkTrailers r fs of
      Left (UndeclaredTrailer n) -> Left n
      Left (RepeatedTrailer n)   -> Left n
      Right _                    -> Right ()

trailerTests :: [T]
trailerTests =
  [ ("checkTrailers rejects the " ++ what ++ " field " ++ show n ++ " as a trailer. " ++ q651,
     (do fname <- mkFieldName n; r <- trailerCheck [(n, v)]; pure (r == Left fname)) == Just True)
  | (n, v, what) <-
      [ ("Content-Length", "10", "framing"), ("Transfer-Encoding", "chunked", "framing"), ("Connection", "close", "connection")
      , ("Keep-Alive", "timeout=5", "connection"), ("Proxy-Connection", "close", "connection"), ("TE", "trailers", "connection")
      , ("Upgrade", "h2c", "connection"), ("Host", "example.com", "routing")
      , ("Authorization", "Basic YTpi", "authentication"), ("Proxy-Authorization", "Basic YTpi", "authentication")
      , ("WWW-Authenticate", "Basic realm=\"a\"", "authentication"), ("Proxy-Authenticate", "Basic realm=\"a\"", "authentication")
      , ("Cache-Control", "no-store", "response control / request modifier")
      , ("Expect", "100-continue", "request modifier"), ("Range", "bytes=0-1", "request modifier")
      , ("If-Match", "\"a\"", "request modifier"), ("If-None-Match", "\"a\"", "request modifier")
      , ("If-Modified-Since", "Tue, 15 Nov 1994 08:12:31 GMT", "request modifier")
      , ("If-Unmodified-Since", "Tue, 15 Nov 1994 08:12:31 GMT", "request modifier")
      , ("If-Range", "\"a\"", "request modifier"), ("Max-Forwards", "1", "request modifier / routing")
      , ("Content-Type", "text/plain", "content format"), ("Content-Encoding", "gzip", "content format")
      , ("Content-Range", "bytes 0-1/2", "content format / framing")
      ]
  ]
  ++
  [ ("checkTrailers accepts an empty trailer section. RFC 9112 Section 7.1.2: \"trailer-section = *( field-line CRLF )\"",
     trailerCheck [] == Just (Right ()))
  , ("checkTrailers accepts ETag. RFC 9110 Section 8.8.3: \"A sender MAY send the ETag field in a trailer section (see Section 6.5).\"",
     trailerCheck [("ETag", "\"xyzzy\"")] == Just (Right ()))
  , ("checkTrailers accepts Accept-Ranges. RFC 9110 Section 14.3: \"The Accept-Ranges field MAY be sent in a trailer section\"",
     trailerCheck [("Accept-Ranges", "bytes")] == Just (Right ()))
  , ("checkTrailers accepts Authentication-Info (documented). RFC 9110 Section 11.6.3: \"Authentication-Info can be sent as a trailer field (Section 6.5) when the authentication scheme explicitly allows this.\"",
     trailerCheck [("Authentication-Info", "rspauth=\"abc\"")] == Just (Right ()))
  , ("checkTrailers accepts Proxy-Authentication-Info, which RFC 9110 allows in trailers on the same terms as Authentication-Info. RFC 9110 Section 11.7.3: \"Proxy-Authentication-Info can be sent as a trailer field (Section 6.5) when the authentication scheme explicitly allows this.\"",
     trailerCheck [("Proxy-Authentication-Info", "rspauth=\"abc\"")] == Just (Right ()))
  , ("POLICY: checkTrailers accepts an unregistered field (documented: whether its definition permits trailers is the application's responsibility). " ++ q651,
     trailerCheck [("X-Checksum", "sha-256=abc")] == Just (Right ()))
  , ("checkTrailers reports the first offending field when there are several. " ++ q651 ++ " (documented: \"rejecting the first field that cannot be a trailer\")",
     (do ct <- mkFieldName "content-type"; r <- trailerCheck [("ETag", "\"a\""), ("Content-Type", "text/plain"), ("Content-Length", "1")]
         pure (r == Left ct)) == Just True)
  , ("checkTrailers rejects two ETag lines in one trailer section: ETag is a singleton, so generating it twice is not allowed in trailers any more than in headers. RFC 9110 Section 5.3: \"a sender MUST NOT generate multiple field lines with the same name in a message (whether in the headers or trailers) ... unless that field's definition allows multiple field line values to be recombined as a comma-separated list\"; Section 6.5.2: \"Trailer fields that might be generated more than once during a message MUST be defined as a list-based field\"; Section 8.8.3: \"ETag = entity-tag\"",
     fmap isLeft (trailerCheck [("ETag", "\"a\""), ("ETag", "\"b\"")]) == Just True)
  , ("checkTrailers rejects a framing field written in upper case. " ++ q651 ++ "; " ++ qCaseIns,
     trailerCheck [("CONTENT-LENGTH", "1")] == Just (Left hContentLength))
  , ("checkTrailers returns the trailer section unchanged when it accepts it. RFC 9110 Section 6.5.2: \"trailer fields with the same name are processed in the order received\"",
     (do fs <- fieldsOf [("ETag", "\"a\""), ("X-A", "1"), ("X-A", "2")]
         names <- mapM mkFieldName ["ETag", "X-A"]
         r <- either (const Nothing) Just (mkResponse OK emptyFields (ContentStream Nothing names (\_ _ -> pure emptyFields)))
         pure (fmap fieldsToList (checkTrailers r fs) == Right (fieldsToList fs))) == Just True)
  ]

------------------------------------------------------------------------
-- FileRange

fileRangeTests :: [T]
fileRangeTests =
  [ ("POLICY: mkFileRange 10 20 keeps the offset and length (documented). RFC 9110 Section 14.1.1: \"Recipients ... MUST anticipate potentially large numerals and prevent parsing errors due to integer conversion overflows\" (applies to the arithmetic on ranges)",
     fmap (\r -> (rangeOffset r, rangeLength r)) (mkFileRange 10 20) == Just (10, 20))
  , ("POLICY: mkFileRange 0 0 is a valid (empty) range (documented: only overflow is refused).",
     isJust (mkFileRange 0 0))
  , ("POLICY: mkFileRange 0 maxBound does not overflow (documented).",
     isJust (mkFileRange 0 maxBound))
  , ("POLICY: mkFileRange maxBound 2 overflows and is refused (documented: \"Nothing if its end would overflow\"). RFC 9110 Section 14.1.1 (overflow)",
     isNothing (mkFileRange maxBound 2))
  , ("POLICY: mkFileRange 2 maxBound overflows and is refused (documented).",
     isNothing (mkFileRange 2 maxBound))
  , ("POLICY: mkFileRange (2^63) (2^63 + 1) overflows and is refused (documented).",
     isNothing (mkFileRange (2 ^ (63 :: Int)) (2 ^ (63 :: Int) + 1)))
  ]

------------------------------------------------------------------------
-- Host parsing as it bears on comparison

hostParseTests :: [T]
hostParseTests =
  [ ("parseAuthority reads \"127.0.0.1\" as the IPv4 address 0x7F000001. " ++ q3986ipv4,
     hostOf "127.0.0.1" == Just (HostIPv4 0x7F000001))
  , ("parseAuthority reads \"0.0.0.0\" and \"255.255.255.255\" as IPv4 addresses. " ++ q3986ipv4,
     hostOf "0.0.0.0" == Just (HostIPv4 0) && hostOf "255.255.255.255" == Just (HostIPv4 0xFFFFFFFF))
  ]
  ++
  [ ("parseAuthority reads " ++ show s ++ " as a registered name, not an IPv4 address. " ++ q3986ipv4 ++ "; RFC 3986 Section 7.4: \"These additional IP address formats are not allowed in the URI syntax\"",
     isJust (regOf s))
  | s <- ["256.0.0.1", "127.000.000.001", "127.0.0.01", "01.2.3.4", "1.2.3", "127.1", "2130706433", "0x7f.0.0.1", "1.2.3.4.5", "1.2.3.4."]
  ]
  ++
  [ ("parseAuthority reads " ++ show a ++ " and " ++ show b ++ " as the same IPv6 host. " ++ q3986ipv6,
     isJust (hostOf a) && hostOf a == hostOf b)
  | (a, b) <-
      [ ("[::1]", "[0:0:0:0:0:0:0:1]"), ("[::1]", "[0000:0000:0000:0000:0000:0000:0000:0001]"), ("[::1]", "[0::1]"), ("[::1]", "[::0:1]")
      , ("[2001:DB8::1]", "[2001:db8::1]"), ("[2001:db8::1]", "[2001:0db8:0000:0000:0000:0000:0000:0001]")
      , ("[::ffff:127.0.0.1]", "[::ffff:7f00:1]"), ("[::FFFF:7F00:1]", "[0:0:0:0:0:ffff:127.0.0.1]")
      , ("[1:2:3:4:5:6:1.2.3.4]", "[1:2:3:4:5:6:102:304]"), ("[1:2:3:4:5:6:7::]", "[1:2:3:4:5:6:7:0]"), ("[::2:3:4:5:6:7:8]", "[0:2:3:4:5:6:7:8]")
      , ("[2001:db8:0:0:1:0:0:1]", "[2001:0db8:0:0:1:0:0:1]"), ("[2001:db8:0:0:1:0:0:1]", "[2001:db8::1:0:0:1]")
      , ("[2001:db8:0:0:1:0:0:1]", "[2001:db8::0:1:0:0:1]"), ("[2001:db8:0:0:1:0:0:1]", "[2001:0db8::1:0:0:1]")
      , ("[2001:db8:0:0:1:0:0:1]", "[2001:db8:0:0:1::1]"), ("[2001:db8:0:0:1:0:0:1]", "[2001:db8:0000:0:1::1]")
      , ("[2001:db8:0:0:1:0:0:1]", "[2001:DB8:0:0:1::1]")
      ]
  ]
  ++
  [ ("parseAuthority rejects the invalid IP-literal " ++ show s ++ ". RFC 3986 Section 3.2.2: \"IP-literal = \"[\" ( IPv6address / IPvFuture ) \"]\"\" and the IPv6address grammar",
     isLeft (parseAuthority s))
  | s <- [ "[]", "[:::]", "[1:2:3:4:5:6:7:8:9]", "[1::2::3]", "[12345::]", "[g::]", "[::1.2.3]", "[1.2.3.4::]", "[::ffff:256.0.0.1]"
         , "[::ffff:1.2.3.04]", "[1:2:3:4:5:6:7:1.2.3.4]", "[1:2:3:4:5:6:7:8::]", "[::1:2:3:4:5:6:7:8]", "[::1", "::1]", "[::1%eth0]", "[1:2:3:4:5:6:7]" ]
  ]
  ++
  [ ("parseAuthority accepts the IPv6 form " ++ show s ++ ". RFC 3986 Section 3.2.2 IPv6address grammar",
     isJust (hostOf s))
  | s <- [ "[1:2:3:4:5:6:7:8]", "[::2:3:4:5:6:7:8]", "[1::3:4:5:6:7:8]", "[1::4:5:6:7:8]", "[1::5:6:7:8]", "[1::6:7:8]", "[1::7:8]", "[1::8]", "[1::]", "[::]"
         , "[1:2:3:4:5:6:1.2.3.4]", "[::1.2.3.4]", "[1::1.2.3.4]", "[1:2:3:4:5::1.2.3.4]", "[1:2:3:4:5:6:7::]" ]
  ]
  ++
  [ ("parseAuthority rejects userinfo. RFC 9110 Section 4.2.4: \"a recipient SHOULD parse for userinfo and treat its presence as an error\"",
     isLeft (parseAuthority "user@example.com"))
  , ("parseAuthority rejects an empty host. RFC 9110 Section 4.2.1: \"A sender MUST NOT generate an \"http\" URI with an empty host identifier. A recipient that processes such a URI reference MUST reject it as invalid.\"",
     isLeft (parseAuthority "") && isLeft (parseAuthority ":80"))
  , ("parseAuthority rejects a raw non-ASCII octet in a registered name. RFC 3986 Section 3.2.2: \"reg-name = *( unreserved / pct-encoded / sub-delims )\"",
     isLeft (parseAuthority ("ex" <> w 0xC3 <> w 0xA9 <> ".com")))
  ]
  ++
  [ ("parseAuthority reads the port of " ++ show s ++ " as " ++ show p ++ ". " ++ q3986port ++ "; " ++ q9110default,
     fmap authorityPort (authOf s) == Just p)
  | (s, p) <- [ ("example.com", Nothing), ("example.com:", Nothing), ("example.com:80", Just 80), ("example.com:0080", Just 80)
              , ("example.com:000000000000000000000080", Just 80), ("example.com:65535", Just 65535), ("example.com:0", Just 0)
              , ("[::1]:8080", Just 8080), ("[::1]:", Nothing), ("1.2.3.4:08080", Just 8080) ]
  ]
  ++
  [ ("parseAuthority rejects the port in " ++ show s ++ ". RFC 3986 Section 3.2.3: \"port = *DIGIT\"",
     isLeft (parseAuthority s))
  | s <- ["example.com:-1", "example.com:+80", "example.com: 80", "example.com:8 0", "example.com:0x50", "example.com:80a", "example.com:80:80", "[::1]80"]
  ]
  ++
  [ ("POLICY: parseAuthority rejects the port " ++ show s ++ ", above 65535 (documented). RFC 3986 Section 3.2.3: \"port = *DIGIT\"; RFC 9110 Section 4.2.1 (TCP port)",
     parseAuthority s == Left AuthorityInvalidPort)
  | s <- ["example.com:65536", "example.com:99999999999999999999999", "example.com:18446744073709551696"]
  ]

------------------------------------------------------------------------
-- canonicalHost

canonOf :: ByteString -> Maybe Host
canonOf = fmap canonicalHost . hostOf

sameCanon :: ByteString -> ByteString -> Bool
sameCanon a b = isJust (canonOf a) && canonOf a == canonOf b

diffCanon :: ByteString -> ByteString -> Bool
diffCanon a b = isJust (canonOf a) && isJust (canonOf b) && canonOf a /= canonOf b

canonicalHostTests :: [T]
canonicalHostTests =
  [ ("canonicalHost identifies " ++ show a ++ " and " ++ show b ++ ". " ++ why,
     sameCanon a b)
  | (a, b, why) <-
      [ ("EXAMPLE.COM", "example.com", q3986host)
      , ("Www.Example.Com", "www.example.com", q3986host)
      , ("%65xample.com", "example.com", q3986unres)
      , ("%45XAMPLE.com", "example.com", q3986unres ++ "; " ++ q3986host)
      , ("example%2Ecom", "example.com", q3986unres)
      , ("ex%2Dample.com", "ex-ample.com", q3986unres)
      , ("ex%5Fample.com", "ex_ample.com", q3986unres)
      , ("ex%7Eample.com", "ex~ample.com", q3986unres)
      , ("ex%7eample.com", "ex~ample.com", q3986unres)
      , ("ex%c3%a9.com", "ex%C3%A9.com", q3986pct)
      , ("ex%2fample", "ex%2Fample", q3986pct)
      , ("%31%32%37.0.0.1", "127.0.0.1", q3986unres ++ "; " ++ q3986ipv4)
      , ("[::1]", "[0:0:0:0:0:0:0:1]", q3986ipv6)
      , ("[2001:DB8::A]", "[2001:db8:0:0:0:0:0:a]", q3986ipv6)
      , ("[::ffff:127.0.0.1]", "[::FFFF:7F00:0001]", q3986ipv6)
      ]
  ]
  ++
  [ ("canonicalHost keeps " ++ show a ++ " and " ++ show b ++ " apart. " ++ why,
     diffCanon a b)
  | (a, b, why) <-
      [ ("example.com", "example.org", q9110origin)
      , ("a.example.com", "example.com", q9110origin)
      , ("example.com", "example.com.com", q9110origin)
      , ("127.0.0.1", "127.0.0.2", q9110origin)
      , ("[::1]", "[::2]", q9110origin)
      , ("[::1]", "127.0.0.1", q9110origin ++ " (the IPv6 loopback is a different address)")
      , ("[::ffff:127.0.0.1]", "127.0.0.2", q9110origin)
      , ("ex%2Cample", "ex,ample", q3986res)
      , ("ex%2Bample", "ex+ample", q3986res)
      , ("127.000.000.001", "127.0.0.1", q3986ipv4 ++ " (\"127.000.000.001\" is a reg-name)")
      , ("localhost", "127.0.0.1", "RFC 9110 Section 4.3.2: \"If host is a registered name, the registered name is an indirect identifier for use with a name resolution service\"")
      ]
  ]
  ++
  [ ("canonicalHost leaves the IPv4 address " ++ show s ++ " unchanged. " ++ q3986ipv4,
     (do h <- hostOf s; pure (canonicalHost h == h)) == Just True)
  | s <- ["127.0.0.1", "0.0.0.0", "10.1.2.3"]
  ]
  ++
  [ ("canonicalHost leaves an ordinary IPv6 address " ++ show s ++ " unchanged. " ++ q3986ipv6,
     (do h <- hostOf s; pure (canonicalHost h == h)) == Just True)
  | s <- ["[::1]", "[2001:db8::1]", "[fe80::1]"]
  ]
  ++
  [ ("POLICY: canonicalHost maps the IPv4-mapped address " ++ show a ++ " to the IPv4 address " ++ show b ++ " (documented). RFC 4291 Section 2.5.5.2: \"This address type is used to represent the addresses of IPv4 nodes as IPv6 addresses.\"; but see " ++ q9110origin,
     sameCanon a b)
  | (a, b) <- [("[::ffff:127.0.0.1]", "127.0.0.1"), ("[::ffff:7f00:1]", "127.0.0.1"), ("[0:0:0:0:0:FFFF:C000:0201]", "192.0.2.1"), ("[::ffff:0.0.0.0]", "0.0.0.0")]
  ]
  ++
  [ ("POLICY: canonicalHost does not map " ++ show a ++ " (not IPv4-mapped: " ++ why ++ ") to " ++ show b ++ " (documented). RFC 4291 Section 2.5.5.2 (IPv4-mapped prefix is 80 zero bits then FFFF)",
     diffCanon a b)
  | (a, b, why) <-
      [ ("[::127.0.0.1]", "127.0.0.1", "IPv4-compatible ::/96")
      , ("[::ffff:0:7f00:1]", "127.0.0.1", "RFC 2765 ::ffff:0:0:0/96")
      , ("[64:ff9b::7f00:1]", "127.0.0.1", "RFC 6052 64:ff9b::/96")
      , ("[1::ffff:7f00:1]", "127.0.0.1", "non-zero high bits")
      ]
  ]
  ++
  [ ("POLICY: canonicalHost removes a single trailing dot, so " ++ show a ++ " matches " ++ show b ++ " (documented). RFC 3986 Section 3.2.2: \"The rightmost domain label of a fully qualified domain name in DNS may be followed by a single \".\"\"",
     sameCanon a b)
  | (a, b) <- [("example.com.", "example.com"), ("EXAMPLE.COM.", "example.com"), ("localhost.", "localhost"), ("example%2Ecom%2E", "example.com")]
  ]
  ++
  [ ("POLICY: canonicalHost turns \"1.2.3.4.\" into the IPv4 address 1.2.3.4 (documented).",
     canonOf "1.2.3.4." == Just (HostIPv4 0x01020304))
  , ("POLICY: canonicalHost leaves a name ending in two dots alone (documented: it has an empty label and is not the same name).",
     diffCanon "example.com.." "example.com" && diffCanon "example.com.." "example.com.")
  , ("POLICY: canonicalHost does not map the numeric name \"2130706433\" to 127.0.0.1 (documented). RFC 3986 Section 7.4: \"These additional IP address formats are not allowed in the URI syntax due to differences between platform implementations.\"",
     diffCanon "2130706433" "127.0.0.1")
  , ("POLICY: canonicalHost does not map \"0x7f.1\" to 127.0.0.1 (documented). RFC 3986 Section 7.4",
     diffCanon "0x7f.1" "127.0.0.1")
  ]
  ++
  [ ("POLICY: canonicalHost is idempotent on " ++ show s ++ " (documented).",
     (do h <- hostOf s; pure (canonicalHost (canonicalHost h) == canonicalHost h)) == Just True)
  | s <- ["example.com", "example.com.", "example.com..", "example.com...", "1.2.3.4.", "[::ffff:1.2.3.4]", "[::1]", "127.0.0.1", "[v1.x]", "a.", "1.2.3.4..", "%31.2.3.4."]
  ]

------------------------------------------------------------------------
-- effectivePort

effectivePortTests :: [T]
effectivePortTests =
  [ ("effectivePort http with no port is 80. " ++ q9110default, effectivePort HTTP Nothing == Just 80)
  , ("effectivePort https with no port is 443. " ++ q9110default, effectivePort HTTPS Nothing == Just 443)
  , ("effectivePort http with port 8080 is 8080. RFC 3986 Section 3.2.3", effectivePort HTTP (Just 8080) == Just 8080)
  , ("effectivePort https with port 80 is 80 (a given port is never replaced by the default). RFC 3986 Section 3.2.3", effectivePort HTTPS (Just 80) == Just 80)
  , ("effectivePort http with port 443 is 443. RFC 3986 Section 3.2.3", effectivePort HTTP (Just 443) == Just 443)
  , ("effectivePort for a scheme given as \"HTTP\" is 80, since schemes are case-insensitive. RFC 9110 Section 4.2.3: \"The scheme and host are case-insensitive\"",
     fmap (`effectivePort` Nothing) (mkScheme "HTTP") == Just (Just 80))
  , ("effectivePort for a scheme given as \"HtTpS\" is 443. RFC 9110 Section 4.2.3: \"The scheme and host are case-insensitive\"",
     fmap (`effectivePort` Nothing) (mkScheme "HtTpS") == Just (Just 443))
  , ("effectivePort of the empty port in \"example.com:\" is 80 for http. " ++ q9110default,
     fmap (effectivePort HTTP . authorityPort) (authOf "example.com:") == Just (Just 80))
  , ("effectivePort of \"example.com:\" is 443 for https. " ++ q9110default,
     fmap (effectivePort HTTPS . authorityPort) (authOf "example.com:") == Just (Just 443))
  , ("POLICY: effectivePort of another scheme with no port is Nothing (documented). RFC 3986 Section 3.2.3: \"A scheme may define a default port.\"",
     fmap (`effectivePort` Nothing) (mkScheme "foo") == Just Nothing)
  , ("effectivePort of another scheme with a port is that port. RFC 3986 Section 3.2.3",
     fmap (`effectivePort` Just 21) (mkScheme "ftp") == Just (Just 21))
  ]

------------------------------------------------------------------------
-- authorityKey

keyOf :: Scheme -> ByteString -> Maybe (Host, Maybe Word16)
keyOf s a = authorityKey s <$> authOf a

sameKey :: Scheme -> ByteString -> ByteString -> Bool
sameKey s a b = isJust (keyOf s a) && keyOf s a == keyOf s b

diffKey :: Scheme -> ByteString -> ByteString -> Bool
diffKey s a b = isJust (keyOf s a) && isJust (keyOf s b) && keyOf s a /= keyOf s b

authorityKeyTests :: [T]
authorityKeyTests =
  [ ("authorityKey " ++ sn ++ " gives " ++ show a ++ " and " ++ show b ++ " the same key. " ++ why,
     sameKey s a b)
  | (s, sn, a, b, why) <-
      [ (HTTP, "http", "example.com:80", "example.com", q9110norm)
      , (HTTP, "http", "example.com:", "example.com", q9110norm ++ "; RFC 3986 Section 6.2.3: \"an explicit \":port\", for which the port is empty or the default for the scheme, is equivalent to one where the port and its \":\" delimiter are elided\"")
      , (HTTP, "http", "EXAMPLE.com:80", "example.com:", q9110norm)
      , (HTTP, "http", "example.com:0080", "example.com", q3986port ++ "; " ++ q9110norm)
      , (HTTP, "http", "example.com:000000000000000000000080", "example.com", q3986port ++ "; " ++ q9110norm)
      , (HTTPS, "https", "example.com:443", "example.com", q9110norm)
      , (HTTPS, "https", "Example.Com", "example.com:443", "RFC 9110 Section 4.3.1: \"https://Example.Com/happy.js would have the origin { \"https\", \"example.com\", \"443\" }\"")
      , (HTTPS, "https", "example.com:", "example.com:443", q9110default)
      , (HTTP, "http", "example.com:8080", "EXAMPLE.COM:08080", q3986port ++ "; " ++ q3986host)
      , (HTTP, "http", "[::1]:80", "[0:0::1]", q3986ipv6 ++ "; " ++ q9110norm)
      , (HTTP, "http", "%65xample.com", "example.com:80", q3986unres)
      , (HTTP, "http", "%31%32%37.0.0.1", "127.0.0.1:80", q3986unres ++ "; " ++ q3986ipv4)
      , (HTTP, "http", "ex%c3%a9.com", "ex%C3%A9.com:80", q3986pct)
      ]
  ]
  ++
  [ ("authorityKey " ++ sn ++ " keeps " ++ show a ++ " and " ++ show b ++ " apart. " ++ q9110origin,
     diffKey s a b)
  | (s, sn, a, b) <-
      [ (HTTP, "http", "example.com:443", "example.com")
      , (HTTP, "http", "example.com:8080", "example.com")
      , (HTTPS, "https", "example.com:80", "example.com")
      , (HTTPS, "https", "example.com:8443", "example.com")
      , (HTTP, "http", "example.com", "example.org")
      , (HTTP, "http", "a.example.com", "example.com")
      , (HTTP, "http", "127.0.0.1", "127.0.0.2")
      , (HTTP, "http", "[::1]", "127.0.0.1")
      , (HTTP, "http", "127.000.000.001", "127.0.0.1")
      , (HTTP, "http", "example.com:0", "example.com")
      ]
  ]
  ++
  [ ("POLICY: authorityKey http gives \"example.com:80\" and \"Example.COM.\" the same key (documented example; the trailing dot rests on canonicalHost's choice). " ++ q9110norm,
     sameKey HTTP "example.com:80" "Example.COM.")
  , ("POLICY: authorityKey https gives \"[::ffff:192.0.2.1]:443\" and \"192.0.2.1\" the same key (documented IPv4-mapped choice).",
     sameKey HTTPS "[::ffff:192.0.2.1]:443" "192.0.2.1")
  ]

------------------------------------------------------------------------
-- isNumericLookalike

numericLookalikeTests :: [T]
numericLookalikeTests =
  [ ("isNumericLookalike " ++ show s ++ " is True: its last label is numeric, so it is not a valid host name and resolvers may read it as an address. " ++ q1123 ++ "; RFC 3986 Section 7.4: \"some implementations allow each dotted part to be interpreted as decimal, octal, or hexadecimal, as specified in the C language (i.e., a leading 0x or 0X implies hexadecimal; a leading 0 implies octal; otherwise, the number is interpreted as decimal)\"",
     fmap isNumericLookalike (regOf s) == Just True)
  | s <- [ "2130706433", "127.1", "0177.0.0.1", "0x7f.0.0.1", "0X7F.0.0.1", "0x7f.1", "0x7f.0x1", "1.2.3", "256.0.0.1"
         , "127.000.000.001", "01.2.3.4", "example.123", "2130706433.", "1.2.3.4.", "0", "08", "example.0x1f", "1.2.3.4.5", "%31%32%37.1" ]
  ]
  ++
  [ ("isNumericLookalike " ++ show s ++ " is False: the last label is not numeric. " ++ q1123,
     fmap isNumericLookalike (regOf s) == Just False)
  | s <- ["example.com", "example.com.", "localhost", "123.example", "1example.com", "xn--p1ai", "a.b.c", "0.example", "127.0.0.1.example"]
  ]

------------------------------------------------------------------------
-- isDnsHostName

label :: Int -> ByteString
label n = BC.replicate n 'a'

-- | a name of exactly n characters made of labels of at most 63
nameOfLength :: Int -> ByteString
nameOfLength n
  | n <= 63 = label n
  | otherwise = label 63 <> "." <> nameOfLength (n - 64)

dnsHostNameTests :: [T]
dnsHostNameTests =
  [ ("isDnsHostName " ++ show s ++ " is True. " ++ q952 ++ "; " ++ q1035,
     fmap isDnsHostName (regOf s) == Just True)
  | s <- [ "example.com", "www.example.com", "localhost", "a.b", "ex-ample.com", "ex--ample.com", "1example.com", "123.example"
         , "xn--bcher-kva.example", "EXAMPLE.COM", "a1.b2.c3", "example.com." ]
  ]
  ++
  [ ("isDnsHostName " ++ show s ++ " is False (" ++ why ++ "). " ++ q952 ++ "; " ++ q1035,
     fmap isDnsHostName (regOf s) == Just False)
  | (s, why) <-
      [ ("-example.com", "label starts with a hyphen"), ("example-.com", "label ends with a hyphen"), ("example.-com", "label starts with a hyphen")
      , ("ex_ample.com", "underscore"), ("ex!ample.com", "sub-delim"), ("ex~ample.com", "tilde"), ("ex'ample.com", "sub-delim")
      , ("a..b", "empty label"), (".example.com", "empty first label"), ("example.com..", "more than one trailing dot"), (".", "no label")
      , ("ho%00st", "percent-encoded NUL"), ("ho%40st", "percent-encoded @"), ("ex%C3%A9.com", "percent-encoded UTF-8")
      , ("example.123", "numeric last label, " ++ q1123), ("1.2.3.4.", "numeric last label"), ("2130706433", "numeric")
      , ("0x7f.0.0.1", "numeric last label"), ("ex+ample.com", "sub-delim"), ("ex=ample", "sub-delim") ]
  ]
  ++
  [ ("isDnsHostName accepts a 63-character label. " ++ q1035 ++ "; RFC 1123 Section 2.1: \"Host software MUST handle host names of up to 63 characters\"",
     fmap isDnsHostName (regOf (label 63 <> ".com")) == Just True)
  , ("isDnsHostName rejects a 64-character label. " ++ q1035,
     fmap isDnsHostName (regOf (label 64 <> ".com")) == Just False)
  , ("isDnsHostName accepts a 253-character name. " ++ q1035,
     B.length (nameOfLength 253) == 253 && fmap isDnsHostName (regOf (nameOfLength 253)) == Just True)
  , ("isDnsHostName accepts a 253-character name followed by the single trailing dot of a fully qualified name. " ++ q1035 ++ "; RFC 3986 Section 3.2.2: \"The rightmost domain label of a fully qualified domain name in DNS may be followed by a single \".\"\"",
     fmap isDnsHostName (regOf (nameOfLength 253 <> ".")) == Just True)
  , ("isDnsHostName rejects a 254-character name without a trailing dot (it would need 256 octets on the wire). " ++ q1035,
     B.length (nameOfLength 254) == 254 && fmap isDnsHostName (regOf (nameOfLength 254)) == Just False)
  ]

------------------------------------------------------------------------
-- renderHost for IPv6 (RFC 5952)

render6 :: ByteString -> Maybe ByteString
render6 s = unbracket . renderHost <$> hostOf ("[" <> s <> "]")

renderIPv6Tests :: [T]
renderIPv6Tests =
  [ ("renderHost renders [" ++ BC.unpack i ++ "] as " ++ BC.unpack o ++ ". " ++ q ++ "; " ++ q5952,
     render6 i == Just o)
  | (i, o, q) <-
      [ ("2001:0db8::0001", "2001:db8::1", q5952_41)
      , ("2001:db8:0:0:0:0:2:1", "2001:db8::2:1", q5952_421)
      , ("2001:db8::0:1", "2001:db8::1", q5952_421)
      , ("2001:db8:0:1:1:1:1:1", "2001:db8:0:1:1:1:1:1", q5952_422)
      , ("2001:db8::1:1:1:1:1", "2001:db8:0:1:1:1:1:1", q5952_422)
      , ("2001:0:0:1:0:0:0:1", "2001:0:0:1::1", q5952_423)
      , ("2001:db8:0:0:1:0:0:1", "2001:db8::1:0:0:1", q5952_423)
      , ("2001:0db8:0:0:1:0:0:1", "2001:db8::1:0:0:1", q5952_423)
      , ("2001:db8:0:0:1::1", "2001:db8::1:0:0:1", q5952_423)
      , ("2001:DB8:0:0:1::1", "2001:db8::1:0:0:1", q5952_423 ++ "; " ++ q5952_43)
      , ("2001:DB8::ABCD:EF", "2001:db8::abcd:ef", q5952_43)
      , ("A:B:C:D:E:F:A:B", "a:b:c:d:e:f:a:b", q5952_43)
      , ("0:0:0:0:0:0:0:0", "::", q4291 ++ "; " ++ q5952_421)
      , ("0:0:0:0:0:0:0:1", "::1", q4291 ++ "; " ++ q5952_421)
      , ("FF01:0:0:0:0:0:0:101", "ff01::101", q4291 ++ "; " ++ q5952_43)
      , ("2001:DB8:0:0:8:800:200C:417A", "2001:db8::8:800:200c:417a", q4291 ++ "; " ++ q5952_43)
      , ("1:0:0:0:0:0:0:0", "1::", q5952_421)
      , ("fe80:0:0:0:0:0:0:1", "fe80::1", q5952_421)
      , ("1:2:3:4:5:6:7:0", "1:2:3:4:5:6:7:0", q5952_422)
      , ("0:1:2:3:4:5:6:7", "0:1:2:3:4:5:6:7", q5952_422)
      , ("1:2:3:4:5:6:7::", "1:2:3:4:5:6:7:0", q5952_422)
      , ("::2:3:4:5:6:7:8", "0:2:3:4:5:6:7:8", q5952_422)
      , ("1:0:2:0:3:0:4:0", "1:0:2:0:3:0:4:0", q5952_422)
      , ("1:0:0:2:0:0:0:3", "1:0:0:2::3", q5952_423)
      , ("1:0:0:0:2:0:0:0", "1::2:0:0:0", q5952_423)
      , ("1:2:0:0:3:4:0:0", "1:2::3:4:0:0", q5952_423)
      , ("0:0:1:0:0:0:0:1", "0:0:1::1", q5952_423)
      , ("0:0:0:1:0:0:0:0", "0:0:0:1::", q5952_423)
      , ("1:2:3:4:5:6:7:8", "1:2:3:4:5:6:7:8", q5952_41)
      , ("0001:0002:0003:0004:0005:0006:0007:0008", "1:2:3:4:5:6:7:8", q5952_41)
      , ("1:2:3:4:5:6:1.2.3.4", "1:2:3:4:5:6:102:304", q5952_5 ++ " (no well-known prefix, so hexadecimal)")
      , ("2001:db8::192.0.2.1", "2001:db8::c000:201", q5952_5 ++ " (no well-known prefix, so hexadecimal)")
      , ("1::ffff:1.2.3.4", "1::ffff:102:304", q5952_5 ++ " (not the IPv4-mapped prefix: the high 80 bits are not zero)")
      , ("::ffff:192.0.2.1", "::ffff:192.0.2.1", q5952_5)
      , ("::FFFF:C000:0201", "::ffff:192.0.2.1", q5952_5)
      , ("0:0:0:0:0:ffff:192.0.2.1", "::ffff:192.0.2.1", q5952_5)
      , ("0:0:0:0:0:ffff:0:0", "::ffff:0.0.0.0", q5952_5)
      , ("::ffff:255.255.255.255", "::ffff:255.255.255.255", q5952_5)
      , ("::ffff:10.0.0.1", "::ffff:10.0.0.1", q5952_5)
      ]
  ]
  ++
  [ ("renderHost of HostIPv6 (IPv6 0 1) is ::1. " ++ q4291,
     unbracket (renderHost (HostIPv6 (IPv6 0 1))) == "::1")
  , ("renderHost of HostIPv6 (IPv6 0 0) is ::. " ++ q4291,
     unbracket (renderHost (HostIPv6 (IPv6 0 0))) == "::")
  , ("renderHost of the IPv4-mapped HostIPv6 (IPv6 0 0x0000FFFFC0000201) is ::ffff:192.0.2.1. " ++ q5952_5,
     unbracket (renderHost (HostIPv6 (IPv6 0 0x0000FFFFC0000201))) == "::ffff:192.0.2.1")
  , ("renderHost of HostIPv6 (IPv6 0x20010DB800000000 1) is 2001:db8::1. " ++ q5952_41 ++ "; " ++ q5952_421,
     unbracket (renderHost (HostIPv6 (IPv6 0x20010DB800000000 1))) == "2001:db8::1")
  , ("renderHost of an all-ones IPv6 address is ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff. " ++ q5952_43,
     unbracket (renderHost (HostIPv6 (IPv6 maxBound maxBound))) == "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")
  ]
  ++
  [ ("renderHost output for " ++ show s ++ " parses back to the same host. " ++ q3986ipv6 ++ "; RFC 5952 Section 4: \"all implementations MUST accept and be able to handle any legitimate [RFC4291] format\"",
     (do h <- hostOf s; let r = renderHost h
         let r' = if BC.take 1 r == "[" then r else "[" <> r <> "]"
         pure (hostOf r' == Just h)) == Just True)
  | s <- ["[::]", "[::1]", "[1::]", "[2001:db8::1:0:0:1]", "[::ffff:1.2.3.4]", "[1:2:3:4:5:6:7:8]", "[1:0:0:2:0:0:0:3]", "[fe80::1]", "[0:1:2:3:4:5:6:7]"]
  ]

------------------------------------------------------------------------
-- renderHost (other hosts)

renderHostTests :: [T]
renderHostTests =
  [ ("renderHost of HostIPv4 0x7F000001 is 127.0.0.1. RFC 3986 Section 3.2.2: \"IPv4address = dec-octet \".\" dec-octet \".\" dec-octet \".\" dec-octet\"",
     renderHost (HostIPv4 0x7F000001) == "127.0.0.1")
  , ("renderHost of HostIPv4 0 is 0.0.0.0. RFC 3986 Section 3.2.2 (dec-octet has no leading zeros)",
     renderHost (HostIPv4 0) == "0.0.0.0")
  , ("renderHost of HostIPv4 0xFFFFFFFF is 255.255.255.255. RFC 3986 Section 3.2.2",
     renderHost (HostIPv4 0xFFFFFFFF) == "255.255.255.255")
  , ("renderHost of HostIPv4 0x0A00000A is 10.0.0.10 (no leading zeros in dec-octet). RFC 3986 Section 3.2.2",
     renderHost (HostIPv4 0x0A00000A) == "10.0.0.10")
  , ("renderHost of the parsed \"EXAMPLE.Com\" is example.com. RFC 3986 Section 3.2.2: \"producers and normalizers should use lowercase for registered names\"",
     fmap renderHost (hostOf "EXAMPLE.Com") == Just "example.com")
  , ("renderHost uses upper case hex in percent-encodings (\"ex%c3%a9\" renders as ex%C3%A9). " ++ q3986pct ++ "; RFC 3986 Section 3.2.2: \"while only using uppercase letters for percent-encodings\"",
     fmap renderHost (hostOf "ex%c3%a9") == Just "ex%C3%A9")
  , ("renderHost decodes percent-encoded unreserved characters (\"%41%42c.example\" renders as abc.example). RFC 3986 Section 2.3: \"percent-encoded octets in the ranges of ALPHA ... should not be created by URI producers and, when found in a URI, should be decoded to their corresponding unreserved characters by URI normalizers.\"",
     fmap renderHost (hostOf "%41%42c.example") == Just "abc.example")
  , ("renderHost keeps a percent-encoded reserved character encoded (\"ex%2cample\" renders as ex%2Cample). " ++ q3986res,
     fmap renderHost (hostOf "ex%2cample") == Just "ex%2Cample")
  , ("renderHost keeps the trailing dot of a fully qualified name (documented: example.com. and example.com render differently). RFC 3986 Section 3.2.2: \"The rightmost domain label of a fully qualified domain name in DNS may be followed by a single \".\"\"",
     fmap renderHost (hostOf "example.com.") == Just "example.com.")
  , ("renderHost keeps sub-delims in a registered name. RFC 3986 Section 3.2.2: \"reg-name = *( unreserved / pct-encoded / sub-delims )\"",
     fmap renderHost (hostOf "a!$&'()*+,;=b") == Just "a!$&'()*+,;=b")
  , ("renderHost of an IPv4-mapped host after canonicalHost is the dotted IPv4 address (POLICY: documented mapping). RFC 3986 Section 3.2.2",
     fmap (renderHost . canonicalHost) (hostOf "[::ffff:127.0.0.1]") == Just "127.0.0.1")
  ]

------------------------------------------------------------------------
-- renderAuthority

renderAuthorityTests :: [T]
renderAuthorityTests =
  [ ("renderAuthority of the parsed " ++ show i ++ " is " ++ show o ++ ". " ++ why,
     fmap renderAuthority (authOf i) == Just o)
  | (i, o, why) <-
      [ ("example.com", "example.com", "RFC 3986 Section 3.2: \"authority = [ userinfo \"@\" ] host [ \":\" port ]\"")
      , ("example.com:8080", "example.com:8080", "RFC 3986 Section 3.2.3")
      , ("EXAMPLE.com:0080", "example.com:80", q3986port ++ "; " ++ q3986host)
      , ("example.com:", "example.com", "RFC 3986 Section 3.2.3: \"URI producers and normalizers should omit the port component and its \":\" delimiter if port is empty\"")
      , ("127.0.0.1:80", "127.0.0.1:80", "RFC 3986 Section 3.2.2")
      , ("[::1]:443", "[::1]:443", "RFC 3986 Section 3.2.2: \"A host identified by an Internet Protocol literal address ... is distinguished by enclosing the IP literal within square brackets\"; RFC 5952 Section 6: \"For URIs containing IPv6 address literals, [RFC3986] MUST be followed\"")
      , ("[2001:DB8:0:0:0:0:0:1]:08080", "[2001:db8::1]:8080", q5952_421 ++ "; " ++ q5952_43 ++ "; " ++ q3986port)
      , ("[2001:db8::1]", "[2001:db8::1]", "RFC 3986 Section 3.2.2 (IP-literal brackets)")
      , ("[::ffff:192.0.2.1]:80", "[::ffff:192.0.2.1]:80", q5952_5)
      , ("ex%c3%a9.example:1", "ex%C3%A9.example:1", q3986pct)
      , ("example.com:0", "example.com:0", "RFC 3986 Section 3.2.3: \"port = *DIGIT\"")
      , ("example.com:65535", "example.com:65535", "RFC 3986 Section 3.2.3")
      , ("[v1.ABC]:5", "[v1.abc]:5", "RFC 3986 Section 3.2.2: \"IPvFuture = \"v\" 1*HEXDIG \".\" 1*( unreserved / sub-delims / \":\" )\"; " ++ q3986host)
      ]
  ]
  ++
  [ ("parseAuthority (renderAuthority a) == Right a for a parsed from " ++ show s ++ ". RFC 3986 Section 3.2: \"authority = [ userinfo \"@\" ] host [ \":\" port ]\"",
     (do a <- authOf s; pure (parseAuthority (renderAuthority a) == Right a)) == Just True)
  | s <- [ "example.com", "example.com:8080", "127.0.0.1:80", "[::1]:443", "[2001:db8::1]", "[v1.x]", "ex%C3%A9.com", "a-b.c:0"
         , "[::ffff:1.2.3.4]:1", "example.com.", "a!$&'()*+,;=b:9", "256.0.0.1", "[1:0:0:2:0:0:0:3]:65535" ]
  ]
  ++
  [ ("renderAuthority never renders userinfo: the output for " ++ show s ++ " contains no \"@\". RFC 9110 Section 4.2.4: \"A sender MUST NOT generate the userinfo subcomponent (and its \"@\" delimiter) when an \"http\" or \"https\" URI reference is generated within a message as a target URI or field value.\"",
     fmap (BC.notElem '@' . renderAuthority) (authOf s) == Just True)
  | s <- ["example.com", "[::1]:80", "a%40b"]
  ]
