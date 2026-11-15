{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | Test cases derived independently from RFC 9110 and RFC 3986.
--
-- Each entry is (description, passed). Descriptions cite the RFC and
-- section, quote the text that justifies the expected result, and say
-- "POLICY" where the expectation rests on a documented library policy
-- that the RFC permits but does not require.
module IndependentTests (independentTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either (isLeft)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Word (Word16, Word32, Word64, Word8)

import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Method
import Hyperdrive.Semantics.SafePath
import Hyperdrive.Semantics.Status
import Hyperdrive.Semantics.Syntax
import Hyperdrive.Semantics.Target
import Hyperdrive.Semantics.URI

------------------------------------------------------------------------
-- Helpers

w :: Char -> Word8
w = fromIntegral . fromEnum

allW :: (Word8 -> Bool) -> String -> Bool
allW p = all (p . w)

noneW :: (Word8 -> Bool) -> String -> Bool
noneW p = not . any (p . w)

-- methods
methB :: ByteString -> Maybe ByteString
methB = fmap methodBytes . mkMethod

meth :: ByteString -> Method
meth s = fromMaybe (error ("bad test method: " ++ show s)) (mkMethod s)

isGETPat :: Method -> Bool
isGETPat GET = True
isGETPat _   = False

-- fields
fnB :: ByteString -> Maybe ByteString
fnB = fmap fieldNameBytes . mkFieldName

fn :: ByteString -> FieldName
fn s = fromMaybe (error ("bad test field name: " ++ show s)) (mkFieldName s)

fvB :: ByteString -> Either FieldValueError ByteString
fvB = fmap fieldValueBytes . mkFieldValue

fv :: ByteString -> FieldValue
fv s = either (\e -> error ("bad test field value: " ++ show (s, e))) id (mkFieldValue s)

combined :: [ByteString] -> Maybe ByteString
combined = fmap fieldValueBytes . combineValues . map fv

isWS :: Word8 -> Bool
isWS b = b == 0x20 || b == 0x09

-- no leading or trailing SP/HTAB, as field-value requires
noEdgeWS :: ByteString -> Bool
noEdgeWS s = B.null s || (not (isWS (B.head s)) && not (isWS (B.last s)))

pairs :: Fields -> [(ByteString, ByteString)]
pairs = map (\(n, v) -> (fieldNameBytes n, fieldValueBytes v)) . fieldsToList

mkFields :: [(ByteString, ByteString)] -> Fields
mkFields = fieldsFromList . map (\(n, v) -> (fn n, fv v))

-- status
code :: Int -> Maybe Int
code = fmap statusCode . mkStatus

klass :: Int -> Maybe StatusClass
klass = fmap statusClass . mkStatus

-- scheme
schemeB :: ByteString -> Maybe ByteString
schemeB = fmap schemeBytes . mkScheme

-- authority
authErr :: ByteString -> Maybe AuthorityError
authErr s = either Just (const Nothing) (parseAuthority s)

regName :: ByteString -> Maybe (ByteString, Maybe Word16)
regName s = case parseAuthority s of
  Right (Authority (HostRegName r) p) -> Just (regNameBytes r, p)
  _                                   -> Nothing

ipv4 :: ByteString -> Maybe (Word32, Maybe Word16)
ipv4 s = case parseAuthority s of
  Right (Authority (HostIPv4 a) p) -> Just (a, p)
  _                                -> Nothing

ipv6 :: ByteString -> Maybe IPv6
ipv6 s = case parseAuthority s of
  Right (Authority (HostIPv6 a) Nothing) -> Just a
  _                                      -> Nothing

isFuture :: ByteString -> Bool
isFuture s = case parseAuthority s of
  Right (Authority (HostIPvFuture _) _) -> True
  _                                     -> False

portOf :: ByteString -> Either AuthorityError (Maybe Word16)
portOf = fmap authorityPort . parseAuthority

rendered :: ByteString -> Either AuthorityError ByteString
rendered = fmap renderAuthority . parseAuthority

renderedHost :: ByteString -> Either AuthorityError ByteString
renderedHost = fmap (renderHost . authorityHost) . parseAuthority

auth :: ByteString -> Authority
auth s = either (\e -> error ("bad test authority: " ++ show (s, e))) id (parseAuthority s)

-- build an IPv6 address from exactly eight 16-bit pieces
v6 :: [Word16] -> IPv6
v6 ps
  | length ps == 8 = IPv6 (pack (take 4 ps)) (pack (drop 4 ps))
  | otherwise      = error "v6: need 8 pieces"
  where
    pack :: [Word16] -> Word64
    pack = foldl (\acc p -> acc * 65536 + fromIntegral p) 0

r6 :: [Word16] -> ByteString
r6 = renderHost . HostIPv6 . v6

-- path / query
path :: ByteString -> Path
path s = fromMaybe (error ("bad test path: " ++ show s)) (mkPath s)

query :: ByteString -> Query
query s = fromMaybe (error ("bad test query: " ++ show s)) (mkQuery s)

segs :: ByteString -> Maybe (Either PathSegmentError [ByteString])
segs s = pathSegments <$> mkPath s

safeSegs :: ByteString -> Maybe (Either UnsafeSegment [ByteString])
safeSegs s = fmap (map safeSegmentBytes) . safePathSegments <$> mkPath s

safeSeg :: ByteString -> Either UnsafeSegment ByteString
safeSeg = fmap safeSegmentBytes . mkSafeSegment

-- targets
uriT :: ByteString -> Maybe ByteString -> Target
uriT p q = TargetURI HTTP (auth "example.com") (path p) (query <$> q)

hostOf :: ByteString -> Host
hostOf = authorityHost . auth

------------------------------------------------------------------------

independentTests :: [(String, Bool)]
independentTests =
  syntaxTests
  ++ methodTests
  ++ fieldNameTests
  ++ fieldValueTests
  ++ combineTests
  ++ fieldsTests
  ++ statusTests
  ++ schemeTests
  ++ authorityTests
  ++ ipv6Tests
  ++ renderTests
  ++ pathTests
  ++ segmentTests
  ++ queryTests
  ++ percentTests
  ++ targetTests
  ++ safePathTests

------------------------------------------------------------------------
-- Syntax

syntaxTests :: [(String, Bool)]
syntaxTests =
  [ ("RFC 9110 5.6.2 'tchar = \"!\" / \"#\" / \"$\" / \"%\" / \"&\" / \"'\" / \"*\" / \"+\" / \"-\" / \".\" / \"^\" / \"_\" / \"`\" / \"|\" / \"~\" / DIGIT / ALPHA': all listed symbols are tchar"
    , allW isTChar "!#$%&'*+-.^_`|~09azAZ")
  , ("RFC 9110 5.6.2 'Delimiters are chosen from ... (DQUOTE and \"(),/:;<=>?@[\\]{}\")': no delimiter is a tchar"
    , noneW isTChar "\"(),/:;<=>?@[\\]{}")
  , ("RFC 9110 5.6.2 'tokens ... do not include whitespace': SP, HTAB, DEL and obs-text are not tchar"
    , not (isTChar 0x20) && not (isTChar 0x09) && not (isTChar 0x7F) && not (isTChar 0x80))
  , ("RFC 9110 5.6.2 'token = 1*tchar': the empty string is not a token"
    , not (isToken ""))
  , ("RFC 9110 5.6.2 'token = 1*tchar': a single tchar is a token, a string with SP is not"
    , isToken "x" && isToken "!#$%&'*+-.^_`|~" && not (isToken "a b"))
  , ("RFC 5234 via RFC 9110 5.5 'VCHAR': %x21 and %x7E are the bounds of VCHAR"
    , isVChar 0x21 && isVChar 0x7E && not (isVChar 0x20) && not (isVChar 0x7F))
  , ("RFC 9110 5.5 'obs-text = %x80-FF': both bounds included, 0x7F excluded"
    , isObsText 0x80 && isObsText 0xFF && not (isObsText 0x7F))
  , ("RFC 9110 5.5 'field-vchar = VCHAR / obs-text': SP and HTAB are not field-vchar, 0x80 is"
    , isFieldVChar 0x80 && isFieldVChar 0x21 && not (isFieldVChar 0x20) && not (isFieldVChar 0x09) && not (isFieldVChar 0x7F))
  , ("RFC 9110 5.6.3 'OWS = *( SP / HTAB )': only SP and HTAB, not CR, LF, VT or FF"
    , isOWS 0x20 && isOWS 0x09 && not (isOWS 0x0D) && not (isOWS 0x0A) && not (isOWS 0x0B) && not (isOWS 0x0C))
  , ("RFC 5234 'ALPHA = %x41-5A / %x61-7A': neighbours @ [ ` { are not ALPHA"
    , allW isALPHA "AZaz" && noneW isALPHA "@[`{")
  , ("RFC 5234 'DIGIT = %x30-39': neighbours / and : are not DIGIT"
    , allW isDIGIT "09" && noneW isDIGIT "/:")
  , ("RFC 3986 2.1 'The uppercase hexadecimal digits 'A' through 'F' are equivalent to the lowercase digits': HEXDIG includes a-f but not g/G"
    , allW isHEXDIG "09afAF" && noneW isHEXDIG "gG/:@`")
  , ("RFC 3986 2.3 'unreserved = ALPHA / DIGIT / \"-\" / \".\" / \"_\" / \"~\"'"
    , allW isUnreserved "aZ09-._~" && noneW isUnreserved "!%/:@+ ")
  , ("RFC 3986 2.2 'sub-delims = \"!\" / \"$\" / \"&\" / \"'\" / \"(\" / \")\" / \"*\" / \"+\" / \",\" / \";\" / \"=\"'"
    , allW isSubDelim "!$&'()*+,;=" && noneW isSubDelim ":/?#[]@%a-")
  , ("RFC 3986 3.3 'pchar = unreserved / pct-encoded / sub-delims / \":\" / \"@\"': : and @ are pchar"
    , allW isPChar ":@aZ09-._~!$&'()*+,;=")
  , ("RFC 3986 3.3 pchar excludes the other gen-delims '/' '?' '#' '[' ']'; '%' alone is not pchar (pct-encoded checked separately, per API doc)"
    , noneW isPChar "/?#[]% \"<>\\^`{|}")
  ]

------------------------------------------------------------------------
-- Methods

methodTests :: [(String, Bool)]
methodTests =
  [ ("RFC 9110 9.1 'method = token': GET is valid and is the GET pattern"
    , maybe False isGETPat (mkMethod "GET"))
  , ("RFC 9110 9.1 'The method token is case-sensitive': 'get' is a valid method"
    , methB "get" == Just "get")
  , ("RFC 9110 9.1 'The method token is case-sensitive': 'get' is not GET"
    , fmap isGETPat (mkMethod "get") == Just False)
  , ("RFC 9110 9.1 'The method token is case-sensitive': 'Get' is not GET and keeps its case"
    , fmap isGETPat (mkMethod "Get") == Just False && methB "Get" == Just "Get")
  , ("RFC 9110 5.6.2 'token = 1*tchar': empty method rejected"
    , isNothing (mkMethod ""))
  , ("RFC 9110 5.6.2 'token = 1*tchar': method with trailing SP rejected"
    , isNothing (mkMethod "GET "))
  , ("RFC 9110 5.6.2 'token = 1*tchar': method with leading SP rejected"
    , isNothing (mkMethod " GET"))
  , ("RFC 9110 5.6.2 'token = 1*tchar': method containing HTAB rejected"
    , isNothing (mkMethod "GE\tT"))
  , ("RFC 9110 5.6.2 'token = 1*tchar': method containing NUL rejected"
    , isNothing (mkMethod "GE\NULT"))
  , ("RFC 9110 5.6.2 'token = 1*tchar': method containing CR rejected"
    , isNothing (mkMethod "GET\r"))
  , ("RFC 9110 5.6.2 delimiters '\"(),/:;<=>?@[\\]{}' are not tchar: GET/ GET: GET( {GET} rejected"
    , all (isNothing . mkMethod) ["GET/", "GET:", "GET(", "{GET}", "\"GET\"", "GET@", "GET[", "G=T"])
  , ("RFC 9110 5.6.2 'tchar ... ; any VCHAR, except delimiters': obs-text byte in method rejected"
    , isNothing (mkMethod "G\xc9T"))
  , ("RFC 9110 5.6.2 'tchar': DEL in method rejected"
    , isNothing (mkMethod "GET\DEL"))
  , ("RFC 9110 9.1 'Additional methods ... have been specified': extension methods like M-SEARCH are valid"
    , methB "M-SEARCH" == Just "M-SEARCH")
  , ("RFC 9110 5.6.2 'tchar': a method consisting of every tchar symbol is valid"
    , methB "!#$%&'*+-.^_`|~09azAZ" == Just "!#$%&'*+-.^_`|~09azAZ")
  , ("RFC 9110 9.2.1 'the GET, HEAD, OPTIONS, and TRACE methods are defined to be safe'"
    , all isSafe [GET, HEAD, OPTIONS, TRACE])
  , ("RFC 9110 9.2.1 'Of the request methods defined by this specification, the GET, HEAD, OPTIONS, and TRACE methods are defined to be safe': POST, PUT, DELETE, CONNECT are not"
    , not (any isSafe [POST, PUT, DELETE, CONNECT]))
  , ("RFC 5789 2 (PATCH is 'neither safe nor idempotent'; RFC 5789 not provided, cited from the API's own reference): PATCH is not safe"
    , not (isSafe PATCH))
  , ("RFC 9110 9.2.1 safe methods; 9.1 'case-sensitive': 'get' is unknown and so unsafe (POLICY: unknown methods assumed unsafe)"
    , not (isSafe (meth "get")))
  , ("RFC 9110 9.2.1 'GET ... defined to be safe': GET built with mkMethod is safe"
    , isSafe (meth "GET") && isSafe (meth "OPTIONS"))
  , ("RFC 9110 9.2.2 'PUT, DELETE, and safe request methods are idempotent'"
    , all isIdempotent [PUT, DELETE, GET, HEAD, OPTIONS, TRACE])
  , ("RFC 9110 9.2.2 'PUT, DELETE, and safe request methods are idempotent': POST and CONNECT are not"
    , not (isIdempotent POST) && not (isIdempotent CONNECT))
  , ("RFC 5789 2 'PATCH is neither safe nor idempotent' (RFC not provided): PATCH not idempotent"
    , not (isIdempotent PATCH))
  , ("RFC 9110 9.2.2 + 9.1 'case-sensitive': 'put' and unknown 'FOO' not idempotent (POLICY: unknown methods assumed not idempotent)"
    , not (isIdempotent (meth "put")) && not (isIdempotent (meth "FOO")))
  , ("RFC 9110 9.1 'method = token': methodBytes round-trips an extension method exactly"
    , methB "PROPFIND" == Just "PROPFIND" && methodBytes DELETE == "DELETE" && methodBytes PATCH == "PATCH")
  ]

------------------------------------------------------------------------
-- Field names

fieldNameTests :: [(String, Bool)]
fieldNameTests =
  [ ("RFC 9110 5.1 'Field names are case-insensitive': Content-Type normalized to lower case (POLICY: lower-case form)"
    , fnB "Content-Type" == Just "content-type")
  , ("RFC 9110 5.1 'Field names are case-insensitive': HOST and hHost agree"
    , fnB "HOST" == Just (fieldNameBytes hHost))
  , ("RFC 9110 5.1 'field-name = token': underscores and other tchars allowed"
    , fnB "X-Custom_Header.v1" == Just "x-custom_header.v1")
  , ("RFC 9110 5.6.2 'token = 1*tchar': all tchar symbols allowed in a field name"
    , fnB "!#$%&'*+-.^_`|~" == Just "!#$%&'*+-.^_`|~")
  , ("RFC 9110 5.1 'field-name = token': empty field name rejected"
    , isNothing (mkFieldName ""))
  , ("RFC 9110 5.1 'field-name = token': field name with SP rejected"
    , isNothing (mkFieldName "Content Type"))
  , ("RFC 9110 5.1 'field-name = token': trailing SP (as in 'Host :') rejected"
    , isNothing (mkFieldName "Host "))
  , ("RFC 9110 5.1 'field-name = token': ':' is a delimiter so 'Host:' is rejected"
    , isNothing (mkFieldName "Host:"))
  , ("RFC 9110 5.1 'field-name = token': pseudo-header ':authority' is not a field name"
    , isNothing (mkFieldName ":authority"))
  , ("RFC 9110 5.6.2 delimiters: field names containing / @ = { \" are rejected"
    , all (isNothing . mkFieldName) ["a/b", "a@b", "a=b", "{x}", "\"x\"", "a,b", "a;b"])
  , ("RFC 9110 5.6.2 'tchar': NUL, HTAB, DEL and obs-text in a field name rejected"
    , all (isNothing . mkFieldName) ["a\NULb", "a\tb", "a\DELb", "caf\xe9"])
  , ("RFC 9110 5.1 'field-name = token': common field name constants are the lower-case names"
    , map fieldNameBytes [hConnection, hContentLength, hContentType, hHost, hSetCookie, hTransferEncoding]
        == ["connection", "content-length", "content-type", "host", "set-cookie", "transfer-encoding"])
  , ("RFC 9110 5.1 'Field names are case-insensitive': SET-COOKIE equals hSetCookie"
    , fnB "SET-COOKIE" == Just (fieldNameBytes hSetCookie))
  ]

------------------------------------------------------------------------
-- Field values

fieldValueTests :: [(String, Bool)]
fieldValueTests =
  [ ("RFC 9110 5.5 'field-vchar = VCHAR / obs-text': obs-text is allowed"
    , fvB "caf\xe9" == Right "caf\xe9")
  , ("RFC 9110 5.5 'obs-text = %x80-FF': both bounds allowed"
    , fvB "\x80\xff" == Right "\x80\xff")
  , ("RFC 9110 5.5 'A field value does not include leading or trailing whitespace': SP stripped"
    , fvB "  a  " == Right "a")
  , ("RFC 9110 5.5 'MUST exclude such whitespace prior to evaluating the field value': HTAB stripped too"
    , fvB "\t a \t" == Right "a")
  , ("RFC 9110 5.5 'field-content = field-vchar [ 1*( SP / HTAB / field-vchar ) field-vchar ]': interior SP kept"
    , fvB " a b " == Right "a b")
  , ("RFC 9110 5.5 field-content allows interior HTAB (even though HTAB is a CTL in RFC 5234)"
    , fvB "a\tb" == Right "a\tb")
  , ("RFC 9110 5.5 field-content '1*( SP / HTAB / field-vchar )': runs of interior whitespace are kept verbatim"
    , fvB "a   \t b" == Right "a   \t b")
  , ("RFC 9110 5.5 'field-value = *field-content': the empty value is valid"
    , fvB "" == Right "")
  , ("RFC 9110 5.5 'does not include leading or trailing whitespace': all-whitespace value becomes empty"
    , fvB "   " == Right "" && fvB "\t" == Right "")
  , ("RFC 9110 5.5 field-vchar includes VCHAR: quotes, commas, semicolons kept"
    , fvB "\"a, b\"; q=0.5" == Right "\"a, b\"; q=0.5")
  , ("RFC 9110 5.5 'a recipient of CR, LF, or NUL within a field value MUST either reject ... or replace': interior CR rejected (POLICY: reject)"
    , fvB "a\rb" == Left ContainsCRLFNUL)
  , ("RFC 9110 5.5 'CR, LF, or NUL ... MUST either reject ... or replace': interior LF rejected (POLICY: reject)"
    , fvB "a\nb" == Left ContainsCRLFNUL)
  , ("RFC 9110 5.5 'CR, LF, or NUL ... MUST either reject ... or replace': interior NUL rejected (POLICY: reject)"
    , fvB "a\NULb" == Left ContainsCRLFNUL)
  , ("RFC 9110 5.5 'CR, LF, or NUL ... invalid and dangerous': obs-fold CRLF SP is rejected, not unfolded (POLICY: reject)"
    , fvB "a\r\n b" == Left ContainsCRLFNUL)
  , ("RFC 9110 5.5 + 5.6.3: CR is not whitespace, so a trailing CR must not be stripped away; it is rejected"
    , fvB "a\r" == Left ContainsCRLFNUL)
  , ("RFC 9110 5.5 + 5.6.3: a leading LF is not whitespace and is rejected"
    , fvB "\na" == Left ContainsCRLFNUL)
  , ("RFC 9110 5.5 + 5.6.3: a value that is only NUL is rejected, not stripped to empty"
    , fvB "\NUL" == Left ContainsCRLFNUL)
  , ("RFC 9110 5.5 'Field values containing other CTL characters are also invalid' (POLICY: not retained): SOH rejected"
    , fvB "a\SOHb" == Left ContainsCTL)
  , ("RFC 9110 5.5 'other CTL characters are also invalid' (POLICY): US (0x1F) rejected"
    , fvB "a\USb" == Left ContainsCTL)
  , ("RFC 9110 5.5 'other CTL characters are also invalid' (POLICY): DEL (0x7F) is a CTL and rejected"
    , fvB "a\DELb" == Left ContainsCTL)
  , ("RFC 9110 5.5 'other CTL characters are also invalid' (POLICY): ESC rejected"
    , fvB "\ESC[31mred" == Left ContainsCTL)
  , ("RFC 9110 5.6.3 OWS is only SP / HTAB: a leading VT is not stripped, it is a CTL and rejected"
    , fvB "\va" == Left ContainsCTL)
  , ("RFC 9110 5.6.3 OWS is only SP / HTAB: a trailing FF is not stripped, it is a CTL and rejected"
    , fvB "a\f" == Left ContainsCTL)
  , ("RFC 9110 5.5 field-vchar: '~' (0x7E, last VCHAR) and '!' (0x21, first VCHAR) allowed"
    , fvB "!~" == Right "!~")
  ]

------------------------------------------------------------------------
-- combineValues

combineTests :: [(String, Bool)]
combineTests =
  [ ("RFC 9110 5.2 example: 'Foo, Bar' and 'Baz' combine to 'Foo, Bar, Baz'"
    , combined ["Foo, Bar", "Baz"] == Just "Foo, Bar, Baz")
  , ("RFC 9110 5.3 'separated by a comma ... For consistency, use comma SP': three values"
    , combined ["a", "b", "c"] == Just "a, b, c")
  , ("RFC 9110 5.3 'appending each subsequent field line value to the initial field line value in order'"
    , combined ["b", "a"] == Just "b, a")
  , ("RFC 9110 5.2 'When a field name is only present once ... consists of the corresponding field line value'"
    , combined ["only"] == Just "only")
  , ("RFC 9110 5.3 combining zero field lines: (POLICY) Nothing for an empty list"
    , isNothing (combineValues []))
  , ("RFC 9110 5.5 'A field value does not include leading or trailing whitespace': combining 'a' with an empty value must not leave a trailing SP"
    , maybe False noEdgeWS (combined ["a", ""]))
  , ("RFC 9110 5.5 'does not include leading or trailing whitespace': combining an empty value with 'b' must not leave a leading SP"
    , maybe False noEdgeWS (combined ["", "b"]))
  , ("RFC 9110 5.5 'does not include leading or trailing whitespace': combining two empty values gives a value with no edge whitespace"
    , maybe False noEdgeWS (combined ["", ""]))
  , ("RFC 9110 5.5: a combined value is itself a valid field value (re-validating it changes nothing)"
    , maybe False (\r -> fvB r == Right r) (combined ["a", "", "b\tc"]))
  , ("RFC 9110 5.3 combine: interior whitespace of each line value preserved"
    , combined ["a  b", "c\td"] == Just "a  b, c\td")
  ]

------------------------------------------------------------------------
-- Field sections

fieldsTests :: [(String, Bool)]
fieldsTests =
  [ ("RFC 9110 5.2 'Field sections are composed of any number of \"field lines\"': emptyFields has none"
    , null (fieldsToList emptyFields))
  , ("RFC 9110 5.3 'The order in which field lines with the same name are received is ... significant': fieldsFromList/fieldsToList preserve order and duplicates"
    , pairs (mkFields [("A", "1"), ("B", "2"), ("a", "3")]) == [("a", "1"), ("b", "2"), ("a", "3")])
  , ("RFC 9110 5.3 'appending each subsequent field line': appendField adds at the end"
    , pairs (appendField (fn "C") (fv "3") (mkFields [("A", "1"), ("B", "2")]))
        == [("a", "1"), ("b", "2"), ("c", "3")])
  , ("RFC 9110 5.3: appendField onto emptyFields"
    , pairs (appendField hHost (fv "example.com") emptyFields) == [("host", "example.com")])
  , ("RFC 9110 5.1 'Field names are case-insensitive': lookup with a differently cased name finds the field"
    , fmap fieldValueBytes (lookupField (fn "hOsT") (mkFields [("Host", "example.com")])) == Just "example.com")
  , ("RFC 9110 5.3 order significant: lookupField returns the first of several lines"
    , fmap fieldValueBytes (lookupField (fn "x") (mkFields [("X", "1"), ("Y", "2"), ("x", "3")])) == Just "1")
  , ("RFC 9110 5.3 'order ... with the same name ... is significant': lookupAll returns every value in order"
    , map fieldValueBytes (lookupAll (fn "X") (mkFields [("X", "1"), ("Y", "2"), ("x", "3")])) == ["1", "3"])
  , ("RFC 9110 5.2: absent field gives Nothing / []"
    , isNothing (lookupField hHost emptyFields) && null (lookupAll hHost (mkFields [("A", "1")])))
  , ("RFC 9110 5.3 Note 'the \"Set-Cookie\" header field ... cannot be combined into a single field value': separate lines are kept separate"
    , map fieldValueBytes (lookupAll hSetCookie (mkFields [("Set-Cookie", "a=1; Expires=Wed, 21 Oct 2015 07:28:00 GMT"), ("Set-Cookie", "b=2")]))
        == ["a=1; Expires=Wed, 21 Oct 2015 07:28:00 GMT", "b=2"])
  , ("RFC 9110 5.3: an empty field line value is still a field line"
    , map fieldValueBytes (lookupAll (fn "X") (mkFields [("X", ""), ("X", "1")])) == ["", "1"])
  ]

------------------------------------------------------------------------
-- Status codes

statusTests :: [(String, Bool)]
statusTests =
  [ ("RFC 9110 15 'Values outside the range 100..599 are invalid': 99 invalid"
    , isNothing (mkStatus 99))
  , ("RFC 9110 15 'All valid status codes are within the range of 100 to 599, inclusive': 100 valid"
    , code 100 == Just 100)
  , ("RFC 9110 15 'within the range of 100 to 599, inclusive': 599 valid"
    , code 599 == Just 599)
  , ("RFC 9110 15 'Values outside the range 100..599 are invalid': 600 invalid"
    , isNothing (mkStatus 600))
  , ("RFC 9110 15 '(i.e., 600..999) for internal communication of non-HTTP status': 999 invalid"
    , isNothing (mkStatus 999))
  , ("RFC 9110 15 'three-digit integer code': 0, negative and 4-digit codes invalid"
    , all (isNothing . mkStatus) [0, -1, -200, 1000, 2000])
  , ("RFC 9110 15 'HTTP status codes are extensible': unregistered 299 and 471 are valid"
    , code 299 == Just 299 && code 471 == Just 471)
  , ("RFC 9110 15.4.7 '306 (Unused) ... the code is reserved': still within 100..599 so valid"
    , code 306 == Just 306)
  , ("RFC 9110 15 '1xx (Informational)': 100 and 199"
    , klass 100 == Just Informational && klass 199 == Just Informational)
  , ("RFC 9110 15 '2xx (Successful)': 200 and 299"
    , klass 200 == Just Successful && klass 299 == Just Successful)
  , ("RFC 9110 15 '3xx (Redirection)': 300 and 399"
    , klass 300 == Just Redirection && klass 399 == Just Redirection)
  , ("RFC 9110 15 '4xx (Client Error)': 400 and 499; the RFC's own example 471"
    , klass 400 == Just ClientError && klass 499 == Just ClientError && klass 471 == Just ClientError)
  , ("RFC 9110 15 '5xx (Server Error)': 500 and 599"
    , klass 500 == Just ServerError && klass 599 == Just ServerError)
  , ("RFC 9110 15 'The first digit of the status code defines the class': classes of named codes"
    , map statusClass [Continue, OK, NotModified, NotFound, HTTPVersionNotSupported]
        == [Informational, Successful, Redirection, ClientError, ServerError])
  , ("RFC 9110 15.5.14 '413 Content Too Large', 15.5.21 '422 Unprocessable Content', 15.5.20 '421 Misdirected Request'"
    , map statusCode [ContentTooLarge, UnprocessableContent, MisdirectedRequest] == [413, 422, 421])
  , ("RFC 9110 15.4.8 '307 Temporary Redirect', 15.4.9 '308 Permanent Redirect', 15.4.6 '305 Use Proxy'"
    , map statusCode [TemporaryRedirect, PermanentRedirect, UseProxy] == [307, 308, 305])
  , ("RFC 9110 15.5.15 '414 URI Too Long', 15.5.17 '416 Range Not Satisfiable', 15.5.18 '417 Expectation Failed', 15.5.22 '426 Upgrade Required'"
    , map statusCode [URITooLong, RangeNotSatisfiable, ExpectationFailed, UpgradeRequired] == [414, 416, 417, 426])
  , ("RFC 9110 15.2.1 '100 Continue', 15.2.2 '101 Switching Protocols', 15.3.4 '203 Non-Authoritative Information', 15.6.6 '505 HTTP Version Not Supported'"
    , map statusCode [Continue, SwitchingProtocols, NonAuthoritativeInformation, HTTPVersionNotSupported] == [100, 101, 203, 505])
  , ("RFC 9110 15.5.5 '404 Not Found': mkStatus 404 matches the NotFound pattern"
    , case mkStatus 404 of { Just NotFound -> True; _ -> False })
  ]

------------------------------------------------------------------------
-- Schemes

schemeTests :: [(String, Bool)]
schemeTests =
  [ ("RFC 3986 3.1 'scheme = ALPHA *( ALPHA / DIGIT / \"+\" / \"-\" / \".\" )': http is HTTP"
    , mkScheme "http" == Just HTTP)
  , ("RFC 3986 3.1 'An implementation should accept uppercase letters as equivalent to lowercase in scheme names (e.g., allow \"HTTP\" as well as \"http\")'"
    , mkScheme "HTTP" == Just HTTP)
  , ("RFC 3986 3.1 'should only produce lowercase scheme names': HtTpS normalized to https"
    , mkScheme "HtTpS" == Just HTTPS && schemeBytes HTTPS == "https")
  , ("RFC 3986 3.1 'letters, digits, plus (\"+\"), period (\".\"), or hyphen (\"-\")' after the first letter"
    , schemeB "Coap+TCP" == Just "coap+tcp" && schemeB "a1+-." == Just "a1+-.")
  , ("RFC 3986 3.1 scheme 'ALPHA *(...)': a single letter is a scheme"
    , schemeB "a" == Just "a")
  , ("RFC 3986 3.1 'scheme = ALPHA ...': empty scheme rejected"
    , isNothing (mkScheme ""))
  , ("RFC 3986 3.1 'beginning with a letter': schemes starting with digit, +, -, . rejected"
    , all (isNothing . mkScheme) ["1http", "+a", "-a", ".a"])
  , ("RFC 3986 3.1 scheme characters: _ : SP / and non-ASCII rejected"
    , all (isNothing . mkScheme) ["a_b", "http:", "ht tp", "a/b", "h\xe9"])
  ]

------------------------------------------------------------------------
-- Authority: reg-name, IPv4, ports, userinfo, empty host

authorityTests :: [(String, Bool)]
authorityTests =
  [ ("RFC 9110 7.2 'Host = uri-host [ \":\" port ]': plain reg-name, no port"
    , regName "example.com" == Just ("example.com", Nothing))
  , ("RFC 3986 3.2.2 'The host subcomponent is case-insensitive' / 'producers and normalizers should use lowercase for registered names'"
    , regName "EXAMPLE.Com:8080" == Just ("example.com", Just 8080))
  , ("RFC 3986 3.2.2 'reg-name = *( unreserved / pct-encoded / sub-delims )': unreserved chars incl. _ and ~"
    , regName "a_b~c-d.e" == Just ("a_b~c-d.e", Nothing))
  , ("RFC 3986 3.2.2 reg-name allows sub-delims '!$&'()*+,;='"
    , regName "a!$&'()*+,;=b" == Just ("a!$&'()*+,;=b", Nothing))
  , ("RFC 3986 3.2.2 'only using uppercase letters for percent-encodings': lower-casing must not touch percent-encoded hex digits"
    , regName "EX%C3%A9MPLE" == Just ("ex%C3%A9mple", Nothing))
  , ("RFC 3986 6.2.2.1 'hexadecimal digits within a percent-encoding triplet ... should be normalized to use uppercase letters'"
    , regName "%c3%a9t%c3%a9" == Just ("%C3%A9t%C3%A9", Nothing))
  , ("RFC 3986 3.2.2 reg-name pct-encoded = '%' HEXDIG HEXDIG: truncated percent-encoding rejected"
    , authErr "a%2" == Just AuthorityInvalidHost && authErr "a%" == Just AuthorityInvalidHost)
  , ("RFC 3986 2.1 'pct-encoded = \"%\" HEXDIG HEXDIG': non-hex after % in host rejected"
    , authErr "a%zz.com" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 reg-name: SP, '/', '\"', '<', '\\\\', '[' inside a reg-name are invalid"
    , all (\s -> authErr s == Just AuthorityInvalidHost) ["a b", "a/b", "a\"b", "a<b", "a\\b", "a[b", "a]b", "a{b}", "a|b"])
  , ("RFC 3986 3.2.2 'Non-ASCII characters must first be encoded according to UTF-8 ... and then each octet ... percent-encoded': raw non-ASCII host rejected"
    , authErr "caf\xc3\xa9.example" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'IPv4address = dec-octet \".\" dec-octet \".\" dec-octet \".\" dec-octet'"
    , ipv4 "192.0.2.1" == Just (0xC0000201, Nothing))
  , ("RFC 3986 3.2.2 'dec-octet ... \"25\" %x30-35 ; 250-255': 255.255.255.255 is IPv4"
    , ipv4 "255.255.255.255" == Just (0xFFFFFFFF, Nothing))
  , ("RFC 3986 3.2.2 'dec-octet = DIGIT ; 0-9': 0.0.0.0 is IPv4"
    , ipv4 "0.0.0.0" == Just (0, Nothing))
  , ("RFC 3986 3.2.2 dec-octet alternatives '2' %x30-34 DIGIT and '1' 2DIGIT: 249.250.199.100"
    , ipv4 "249.250.199.100" == Just (0xF9FAC764, Nothing))
  , ("RFC 3986 3.2.2 IPv4address with port"
    , ipv4 "10.0.0.1:8080" == Just (0x0A000001, Just 8080))
  , ("RFC 3986 3.2.2 'first-match-wins': 256.1.1.1 is not an IPv4address, so it is a (valid) reg-name"
    , regName "256.1.1.1" == Just ("256.1.1.1", Nothing))
  , ("RFC 3986 3.2.2 'dec-octet ... %x31-39 DIGIT ; 10-99': leading zero not a dec-octet, so 01.2.3.4 is a reg-name"
    , regName "01.2.3.4" == Just ("01.2.3.4", Nothing))
  , ("RFC 3986 3.2.2 dec-octet forbids leading zeros: 1.2.3.04 is a reg-name, not 1.2.3.4"
    , regName "1.2.3.04" == Just ("1.2.3.04", Nothing))
  , ("RFC 3986 3.2.2 'only the dotted-decimal form of four octets is allowed by this grammar': 1.2.3 is a reg-name"
    , regName "1.2.3" == Just ("1.2.3", Nothing))
  , ("RFC 3986 3.2.2 'four octets': 1.2.3.4.5 is a reg-name"
    , regName "1.2.3.4.5" == Just ("1.2.3.4.5", Nothing))
  , ("RFC 3986 3.2.2 'four octets': 1.2.3.4. (trailing dot) is a reg-name"
    , regName "1.2.3.4." == Just ("1.2.3.4.", Nothing))
  , ("RFC 3986 7.4 'These additional IP address formats are not allowed in the URI syntax': 0x7f.0.0.1 is a reg-name"
    , regName "0x7F.0.0.1" == Just ("0x7f.0.0.1", Nothing))
  , ("RFC 3986 3.2.2 reg-name: all-digit name 1234 is a reg-name, with port"
    , regName "1234:80" == Just ("1234", Just 80))
  , ("RFC 3986 3.2.3 'port = *DIGIT': port 0 is syntactically valid"
    , portOf "example.com:0" == Right (Just 0))
  , ("RFC 3986 3.2.3 'port = *DIGIT': port 65535 accepted"
    , portOf "example.com:65535" == Right (Just 65535))
  , ("RFC 9110 4.2.1 'TCP port' (POLICY: ports above 65535 rejected): 65536 rejected"
    , authErr "example.com:65536" == Just AuthorityInvalidPort)
  , ("RFC 9110 4.2.1 'TCP port' (POLICY: > 65535 rejected): 65616 must not wrap to 80"
    , authErr "example.com:65616" == Just AuthorityInvalidPort)
  , ("RFC 9110 4.2.1 'TCP port' (POLICY): 2^64+80 must not wrap to 80"
    , authErr "example.com:18446744073709551696" == Just AuthorityInvalidPort)
  , ("RFC 3986 3.2.3 'port = *DIGIT': leading zeros allowed, 080 is port 80"
    , portOf "example.com:080" == Right (Just 80))
  , ("RFC 3986 3.2.3 'port = *DIGIT': many leading zeros are still port 80"
    , portOf "example.com:000000000000000000000080" == Right (Just 80))
  , ("RFC 9110 4.2.1 'If the port subcomponent is empty or not given' : empty port is Nothing"
    , portOf "example.com:" == Right Nothing)
  , ("RFC 3986 3.2.3 'port = *DIGIT': signs, letters and spaces in port rejected"
    , all (\s -> authErr s == Just AuthorityInvalidPort)
          ["example.com:-1", "example.com:+80", "example.com:8a", "example.com: 80", "example.com:80 ", "example.com:0x50"])
  , ("RFC 3986 3.2 'authority = [ userinfo \"@\" ] host [ \":\" port ]': two colons outside brackets is invalid"
    , isLeft (parseAuthority "example.com:80:81"))
  , ("RFC 9110 4.2.4 'a recipient SHOULD parse for userinfo and treat its presence as an error'"
    , authErr "user@example.com" == Just AuthorityHasUserinfo)
  , ("RFC 9110 4.2.4 'treat its presence as an error': user:password@host:port"
    , authErr "user:pw@example.com:80" == Just AuthorityHasUserinfo)
  , ("RFC 3986 3.2.1 'userinfo = *( ... )' may be empty: '@host' still has userinfo, which is an error"
    , authErr "@example.com" == Just AuthorityHasUserinfo)
  , ("RFC 9110 4.2.4 userinfo before an IP-literal is still an error"
    , authErr "u@[::1]:443" == Just AuthorityHasUserinfo)
  , ("RFC 9110 4.2.1 'A recipient that processes such a URI reference MUST reject it as invalid': empty host"
    , authErr "" == Just AuthorityEmptyHost)
  , ("RFC 9110 4.2.1 'MUST NOT generate an \"http\" URI with an empty host identifier': ':80' rejected"
    , authErr ":80" == Just AuthorityEmptyHost)
  , ("RFC 9110 4.2.1 empty host: ':' alone rejected as empty host"
    , authErr ":" == Just AuthorityEmptyHost)
  , ("RFC 3986 3.2.2 'This is the only place where square bracket characters are allowed': unbracketed IPv6 rejected"
    , isLeft (parseAuthority "::1") && isLeft (parseAuthority "2001:db8::1"))
  , ("RFC 3986 3.2.2 'IP-literal = \"[\" ( IPv6address / IPvFuture ) \"]\"': an IPv4 address inside brackets is invalid"
    , authErr "[192.0.2.1]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IP-literal: a reg-name inside brackets is invalid"
    , authErr "[example.com]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IP-literal: unterminated bracket rejected"
    , isLeft (parseAuthority "[::1"))
  , ("RFC 3986 3.2 'host [ \":\" port ]': junk after ']' rejected"
    , isLeft (parseAuthority "[::1]x") && isLeft (parseAuthority "[::1]80"))
  , ("RFC 3986 3.2.2 IP-literal: empty brackets rejected"
    , authErr "[]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'IPvFuture = \"v\" 1*HEXDIG \".\" 1*( unreserved / sub-delims / \":\" )'"
    , isFuture "[v1.x]" && isFuture "[vAB.a:b+c]" && isFuture "[v1.:]")
  , ("RFC 3986 3.2.2 'an IP-literal that starts with \"v\" (case-insensitive)': V1.x is IPvFuture"
    , isFuture "[V1.x]")
  , ("RFC 3986 3.2.2 IPvFuture '1*HEXDIG': missing version rejected"
    , authErr "[v.x]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IPvFuture '1*HEXDIG': non-hex version rejected"
    , authErr "[vg.x]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IPvFuture '\".\" 1*( ... )': empty address part rejected"
    , authErr "[v1.]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IPvFuture requires '.' after the version"
    , authErr "[v1x]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IPvFuture '1*( unreserved / sub-delims / \":\" )': no pct-encoded, no '/'"
    , authErr "[v1.a%41]" == Just AuthorityInvalidHost && authErr "[v1.a/b]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IPvFuture with a port"
    , portOf "[v1.x]:8080" == Right (Just 8080))
  ]

------------------------------------------------------------------------
-- IPv6

ipv6Tests :: [(String, Bool)]
ipv6Tests =
  [ ("RFC 3986 3.2.2 '[ *5( h16 \":\" ) h16 ] \"::\" h16': ::1 is the loopback address"
    , ipv6 "[::1]" == Just (v6 [0,0,0,0,0,0,0,1]))
  , ("RFC 3986 3.2.2 '[ *6( h16 \":\" ) h16 ] \"::\"' with empty prefix: :: is all zeros"
    , ipv6 "[::]" == Just (v6 [0,0,0,0,0,0,0,0]))
  , ("RFC 3986 3.2.2 '6( h16 \":\" ) ls32' with 'ls32 = ( h16 \":\" h16 )': eight explicit pieces"
    , ipv6 "[1:2:3:4:5:6:7:8]" == Just (v6 [1,2,3,4,5,6,7,8]))
  , ("RFC 3986 3.2.2 'A 128-bit IPv6 address is divided into eight 16-bit pieces': seven pieces without :: rejected"
    , authErr "[1:2:3:4:5:6:7]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'eight 16-bit pieces': nine pieces rejected"
    , authErr "[1:2:3:4:5:6:7:8:9]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 '[ *6( h16 \":\" ) h16 ] \"::\"': seven pieces then :: is valid"
    , ipv6 "[1:2:3:4:5:6:7::]" == Just (v6 [1,2,3,4,5,6,7,0]))
  , ("RFC 3986 3.2.2 '\"::\" 5( h16 \":\" ) ls32': :: then seven pieces is valid"
    , ipv6 "[::2:3:4:5:6:7:8]" == Just (v6 [0,2,3,4,5,6,7,8]))
  , ("RFC 3986 3.2.2 '[ *3( h16 \":\" ) h16 ] \"::\" h16 \":\" ls32': seven pieces split around :: valid"
    , ipv6 "[1:2:3:4::5:6:7]" == Just (v6 [1,2,3,4,0,5,6,7]))
  , ("RFC 3986 3.2.2 'one or more consecutive zero-valued 16-bit pieces ... may be elided': eight explicit pieces plus trailing :: rejected"
    , authErr "[1:2:3:4:5:6:7:8::]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'one or more ... may be elided': :: plus eight explicit pieces rejected"
    , authErr "[::1:2:3:4:5:6:7:8]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'one or more ... may be elided': eight explicit pieces with :: in the middle rejected"
    , authErr "[1::2:3:4:5:6:7:8]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'leaving exactly two consecutive colons in their place': two :: rejected"
    , authErr "[1::2::3]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'exactly two consecutive colons': ::: rejected"
    , authErr "[:::]" == Just AuthorityInvalidHost && authErr "[1:::2]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IPv6address grammar: single leading colon rejected"
    , authErr "[:1::]" == Just AuthorityInvalidHost && authErr "[:1:2:3:4:5:6:7:8]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IPv6address grammar: single trailing colon rejected"
    , authErr "[::1:]" == Just AuthorityInvalidHost && authErr "[1:2:3:4:5:6:7:8:]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'h16 = 1*4HEXDIG': four hex digits accepted"
    , ipv6 "[1234::]" == Just (v6 [0x1234,0,0,0,0,0,0,0]))
  , ("RFC 3986 3.2.2 'h16 = 1*4HEXDIG': five hex digits rejected"
    , authErr "[12345::]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'h16 = 1*4HEXDIG': five digits rejected even with a leading zero"
    , authErr "[01234::]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'h16 = 1*4HEXDIG': five digits in the last piece rejected"
    , authErr "[1:2:3:4:5:6:7:10000]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 '(leading zeroes are permitted)': fully padded loopback"
    , ipv6 "[0000:0000:0000:0000:0000:0000:0000:0001]" == Just (v6 [0,0,0,0,0,0,0,1]))
  , ("RFC 3986 3.2.2 'case-insensitive hexadecimal'"
    , ipv6 "[ABCD::ef]" == Just (v6 [0xabcd,0,0,0,0,0,0,0xef]))
  , ("RFC 3986 3.2.2 'h16 = 1*4HEXDIG': ffff in every piece"
    , ipv6 "[ffff:ffff:ffff:ffff:FFFF:FFFF:FFFF:FFFF]" == Just (IPv6 maxBound maxBound))
  , ("RFC 3986 3.2.2 HEXDIG: 'g' is not a hex digit"
    , authErr "[::g]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 '[ *3( h16 \":\" ) h16 ] \"::\" h16 \":\" ls32' with 'ls32 = ( h16 \":\" h16 ) / IPv4address': IPv4-mapped"
    , ipv6 "[::ffff:192.0.2.1]" == Just (v6 [0,0,0,0,0,0xffff,0xc000,0x0201]))
  , ("RFC 3986 3.2.2 '[ *4( h16 \":\" ) h16 ] \"::\" ls32': ::192.0.2.1"
    , ipv6 "[::192.0.2.1]" == Just (v6 [0,0,0,0,0,0,0xc000,0x0201]))
  , ("RFC 3986 3.2.2 '6( h16 \":\" ) ls32' with IPv4 ls32"
    , ipv6 "[1:2:3:4:5:6:1.2.3.4]" == Just (v6 [1,2,3,4,5,6,0x0102,0x0304]))
  , ("RFC 3986 3.2.2 'the least-significant two pieces may instead be represented in IPv4' : 255.255.255.255 as ls32"
    , ipv6 "[1:2:3:4:5:6:255.255.255.255]" == Just (v6 [1,2,3,4,5,6,0xffff,0xffff]))
  , ("RFC 3986 3.2.2 '[ *4( h16 \":\" ) h16 ] \"::\" ls32': five pieces, ::, IPv4 is valid"
    , ipv6 "[1:2:3:4:5::1.2.3.4]" == Just (v6 [1,2,3,4,5,0,0x0102,0x0304]))
  , ("RFC 3986 3.2.2 'IPv4 ... counts as two pieces': six pieces, ::, IPv4 is too many"
    , authErr "[1:2:3:4:5:6::1.2.3.4]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 '6( h16 \":\" ) ls32': five pieces then IPv4 without :: is too few"
    , authErr "[1:2:3:4:5:1.2.3.4]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 '6( h16 \":\" ) ls32': seven pieces then IPv4 is too many"
    , authErr "[1:2:3:4:5:6:7:1.2.3.4]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'ls32 ; least-significant 32 bits': IPv4 not at the end rejected"
    , authErr "[::1.2.3.4:5]" == Just AuthorityInvalidHost && authErr "[1.2.3.4::]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 dec-octet '\"25\" %x30-35': 256 in an embedded IPv4 rejected"
    , authErr "[::256.1.1.1]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 dec-octet: leading zero in embedded IPv4 rejected (no reg-name fallback inside brackets)"
    , authErr "[::01.2.3.4]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IPv4address needs four dec-octets: ::1.2.3 rejected"
    , authErr "[::1.2.3]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 'This syntax does not support IPv6 scoped addressing zone identifiers'"
    , authErr "[fe80::1%25eth0]" == Just AuthorityInvalidHost && authErr "[fe80::1%eth0]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2.2 IP-literal: whitespace inside brackets rejected"
    , authErr "[ ::1]" == Just AuthorityInvalidHost)
  , ("RFC 3986 3.2 'host [ \":\" port ]': IPv6 with port"
    , parseAuthority "[::1]:8080" == Right (Authority (HostIPv6 (v6 [0,0,0,0,0,0,0,1])) (Just 8080)))
  , ("RFC 9110 4.2.1 'port subcomponent is empty': IPv6 with empty port"
    , portOf "[::1]:" == Right Nothing)
  , ("RFC 9110 4.2.1 (POLICY > 65535 rejected): IPv6 with port 65536"
    , authErr "[::1]:65536" == Just AuthorityInvalidPort)
  ]

------------------------------------------------------------------------
-- Rendering

renderTests :: [(String, Bool)]
renderTests =
  [ ("RFC 3986 3.2.2 'dotted-decimal form': renderHost IPv4"
    , renderHost (HostIPv4 0xC0000201) == "192.0.2.1")
  , ("RFC 3986 3.2.2 dec-octet '0-9' and '250-255': renderHost of 0 and of all-ones"
    , renderHost (HostIPv4 0) == "0.0.0.0" && renderHost (HostIPv4 0xFFFFFFFF) == "255.255.255.255")
  , ("RFC 3986 3.2.2 'IP-literal = \"[\" ... \"]\"' and RFC 5952: loopback renders as [::1]"
    , r6 [0,0,0,0,0,0,0,1] == "[::1]")
  , ("RFC 5952 (doc: '::' replaces the longest run): unspecified address renders as [::]"
    , r6 [0,0,0,0,0,0,0,0] == "[::]")
  , ("RFC 5952 (doc: 'lower case hex, no leading zeros'): 2001:db8::1"
    , r6 [0x2001,0xdb8,0,0,0,0,0,1] == "[2001:db8::1]")
  , ("RFC 5952 (doc: 'the first, if there is a tie'): 2001:db8:0:0:1:0:0:1 renders 2001:db8::1:0:0:1"
    , r6 [0x2001,0xdb8,0,0,1,0,0,1] == "[2001:db8::1:0:0:1]")
  , ("RFC 5952 (doc: 'the longest run'): a later, longer run is the one compressed"
    , r6 [1,0,0,2,0,0,0,3] == "[1:0:0:2::3]")
  , ("RFC 5952 (doc: 'run of two or more zero pieces'): a single zero piece is not compressed"
    , r6 [0x2001,0xdb8,0,1,1,1,1,1] == "[2001:db8:0:1:1:1:1:1]")
  , ("RFC 5952 (doc: 'two or more'): a single trailing zero piece is not compressed"
    , r6 [1,2,3,4,5,6,7,0] == "[1:2:3:4:5:6:7:0]")
  , ("RFC 5952 (doc: 'two or more'): alternating single zeros are never compressed"
    , r6 [0,1,0,1,0,1,0,1] == "[0:1:0:1:0:1:0:1]")
  , ("RFC 5952: trailing run compressed to 'x::'"
    , r6 [1,0,0,0,0,0,0,0] == "[1::]")
  , ("RFC 5952: the longest run wins over an earlier shorter one at the start"
    , r6 [0,0,1,0,0,0,0,0] == "[0:0:1::]")
  , ("RFC 5952: no zeros, no compression"
    , r6 [1,2,3,4,5,6,7,8] == "[1:2:3:4:5:6:7:8]")
  , ("RFC 3986 3.2.2 'producers and normalizers should use lowercase for ... hexadecimal addresses': ABCD:0:... renders [abcd::]"
    , renderedHost "[ABCD:0:0:0:0:0:0:0]" == Right "[abcd::]")
  , ("RFC 3986 3.2.2 '(leading zeroes are permitted)' on input; RFC 5952 strips them on output"
    , renderedHost "[2001:0DB8:0000:0000:0000:0000:0000:0001]" == Right "[2001:db8::1]")
  , ("RFC 3986 3.2.2 'should use lowercase for registered names': renderHost of a reg-name"
    , renderedHost "WWW.Example.COM" == Right "www.example.com")
  , ("RFC 3986 3.2.2 IPvFuture round-trips through renderHost with its brackets"
    , renderedHost "[v1.fe80::a+en1]" == Right "[v1.fe80::a+en1]")
  , ("RFC 3986 3.2 'authority = ... host [ \":\" port ]': reg-name without port"
    , rendered "example.com" == Right "example.com")
  , ("RFC 3986 3.2 'host [ \":\" port ]': reg-name with port"
    , rendered "example.com:8080" == Right "example.com:8080")
  , ("RFC 3986 3.2 'URI producers and normalizers should omit the \":\" delimiter ... if the port component is empty'"
    , rendered "example.com:" == Right "example.com")
  , ("RFC 3986 3.2.3 'port = *DIGIT' is a decimal number: leading zeros dropped when rendering"
    , rendered "example.com:0080" == Right "example.com:80")
  , ("RFC 3986 3.2.3 port 0 is a real port value and is rendered"
    , rendered "example.com:0" == Right "example.com:0")
  , ("RFC 3986 3.2.2 'IP-literal' keeps brackets in an authority with port"
    , rendered "[0:0::1]:443" == Right "[::1]:443")
  , ("RFC 3986 3.2.2 IPv4 with port renders dotted-decimal"
    , rendered "192.0.2.1:8080" == Right "192.0.2.1:8080")
  , ("RFC 3986 3.2.2 'host is case-insensitive' : mixed-case IPv6 with port normalized"
    , rendered "[2001:DB8::A]:65535" == Right "[2001:db8::a]:65535")
  , ("RFC 3986 3.2.2 renderAuthority on a constructed Authority"
    , renderAuthority (Authority (HostIPv4 0x7F000001) (Just 80)) == "127.0.0.1:80")
  ]

------------------------------------------------------------------------
-- Paths

pathTests :: [(String, Bool)]
pathTests =
  [ ("RFC 9110 4.2.1 'http-URI = \"http\" \"://\" authority path-abempty': the empty path is valid"
    , isJust (mkPath ""))
  , ("RFC 3986 3.3 'path-abempty = *( \"/\" segment )': '/' is valid"
    , fmap pathBytes (mkPath "/") == Just "/")
  , ("RFC 9110 4.1 'absolute-path = 1*( \"/\" segment )' allows paths that begin with '//'"
    , fmap pathBytes (mkPath "//a") == Just "//a")
  , ("RFC 3986 3.3 'segment = *pchar': empty segments in the middle and at the end"
    , isJust (mkPath "/a//b/"))
  , ("RFC 3986 3.3 'pchar = unreserved / pct-encoded / sub-delims / \":\" / \"@\"': every pchar symbol allowed"
    , isJust (mkPath "/:@!$&'()*+,;=-._~aZ09"))
  , ("RFC 3986 3.3 pchar includes pct-encoded, in either case"
    , isJust (mkPath "/%41%2f%2F%e9"))
  , ("RFC 3986 3.2 'If a URI contains an authority component, then the path component must either be empty or begin with a slash'"
    , isNothing (mkPath "a/b"))
  , ("RFC 9110 7.1 asterisk is a distinct request-target form, not a path: '*' is not a valid path"
    , isNothing (mkPath "*"))
  , ("RFC 3986 2.1 'pct-encoded = \"%\" HEXDIG HEXDIG': truncated and bare % rejected"
    , isNothing (mkPath "/a%2") && isNothing (mkPath "/a%") && isNothing (mkPath "/%"))
  , ("RFC 3986 2.1 pct-encoded needs HEXDIG: /a%g1 and /a%1g rejected"
    , isNothing (mkPath "/a%g1") && isNothing (mkPath "/a%1g"))
  , ("RFC 3986 3.3 'The path is terminated by the first question mark (\"?\") or number sign (\"#\")': ? and # not allowed in a path"
    , isNothing (mkPath "/a?b") && isNothing (mkPath "/a#b"))
  , ("RFC 3986 3.2.2 'This is the only place where square bracket characters are allowed in the URI syntax': [ ] not allowed in a path"
    , isNothing (mkPath "/a[b") && isNothing (mkPath "/a]b"))
  , ("RFC 3986 3.3 pchar: SP, '\"', '<', '>', '\\\\', '^', '`', '{', '|', '}' are not allowed"
    , all (isNothing . mkPath) ["/a b", "/a\"b", "/a<b", "/a>b", "/a\\b", "/a^b", "/a`b", "/a{b", "/a|b", "/a}b"])
  , ("RFC 3986 2 'A URI is composed from a limited set of characters': raw non-ASCII and controls rejected"
    , all (isNothing . mkPath) ["/caf\xc3\xa9", "/a\NULb", "/a\tb", "/a\DEL"])
  , ("RFC 9110 4.2.3 'all other components are compared in a case-sensitive manner' (POLICY: kept as received): path case and encoding preserved"
    , fmap pathBytes (mkPath "/A/%7esmith/%2f") == Just "/A/%7esmith/%2f")
  ]

segmentTests :: [(String, Bool)]
segmentTests =
  [ ("RFC 9110 4.2.3 'an empty path component is equivalent to an absolute path of \"/\"' (doc example): '/' has no segments"
    , segs "/" == Just (Right []))
  , ("RFC 3986 3.3 'path-abempty = *( \"/\" segment )': the empty path has zero segments"
    , segs "" == Just (Right []))
  , ("RFC 3986 3.3 'A path consists of a sequence of path segments separated by a slash'"
    , segs "/a/b" == Just (Right ["a", "b"]))
  , ("RFC 3986 3.3 'segment = *pchar': a trailing slash yields a final empty segment"
    , segs "/a/" == Just (Right ["a", ""]))
  , ("RFC 3986 3.3 'segment = *pchar': '//' is two empty segments"
    , segs "//" == Just (Right ["", ""]))
  , ("RFC 3986 3.3 'segment = *pchar': interior empty segment kept"
    , segs "/a//b" == Just (Right ["a", "", "b"]))
  , ("RFC 3986 3.3 'segment = *pchar': leading empty segment from '//a'"
    , segs "//a" == Just (Right ["", "a"]))
  , ("RFC 3986 2.2 'URIs that differ in the replacement of a reserved character with its corresponding percent-encoded octet are not equivalent': %2F does not split"
    , segs "/a/b%2Fc/" == Just (Right ["a", "b/c", ""]))
  , ("RFC 3986 2.1 'uppercase hexadecimal digits ... are equivalent to the lowercase': %2f decoded too"
    , segs "/a%2fb" == Just (Right ["a/b"]))
  , ("RFC 3986 2.3 'percent-encoded octets in the ranges of ... tilde (%7E) ... should be decoded'"
    , segs "/%7Esmith" == Just (Right ["~smith"]))
  , ("RFC 3986 3.3 sub-delims are data: '+' in a path is a plus, not a space"
    , segs "/a+b" == Just (Right ["a+b"]))
  , ("RFC 3986 2.1 '\"%20\" is the percent-encoding for ... space'"
    , segs "/a%20b" == Just (Right ["a b"]))
  , ("RFC 3986 2.4 'Implementations must not percent-encode or decode the same string more than once': %2541 decodes to %41"
    , segs "/%2541" == Just (Right ["%41"]))
  , ("RFC 3986 2.3 '%2E' is an encoded period, so %2e%2e decodes to '..' (doc example; faithful decoding)"
    , segs "/a/%2e%2e/b" == Just (Right ["a", "..", "b"]))
  , ("RFC 3986 3.3 dot-segments: pathSegments keeps '.' and '..' (doc: faithful decoding)"
    , segs "/a/./../b" == Just (Right ["a", ".", "..", "b"]))
  , ("RFC 3986 7.3 'the \"%00\" percent-encoding (NUL) ... should be rejected if the application is not expecting to receive raw data'"
    , segs "/a%00" == Just (Left SegmentContainsNUL))
  , ("RFC 3986 7.3 '%00 ... should be rejected': NUL in a middle segment"
    , segs "/a/b%00c/d" == Just (Left SegmentContainsNUL))
  , ("RFC 3986 3.2.2/2.5 UTF-8 percent-encoded octets decode to raw bytes"
    , segs "/caf%C3%A9" == Just (Right ["caf\xc3\xa9"]))
  , ("RFC 3986 3.3 'the semicolon (\";\") and equals (\"=\") ... often used to delimit parameters': kept as data"
    , segs "/name;v=1.1/x,y" == Just (Right ["name;v=1.1", "x,y"]))
  ]

------------------------------------------------------------------------
-- Query

queryTests :: [(String, Bool)]
queryTests =
  [ ("RFC 3986 3.4 'query = *( pchar / \"/\" / \"?\" )': empty query valid"
    , fmap queryBytes (mkQuery "") == Just "")
  , ("RFC 3986 3.4 query: typical key=value pairs"
    , fmap queryBytes (mkQuery "a=b&c=d") == Just "a=b&c=d")
  , ("RFC 3986 3.4 'The characters slash (\"/\") and question mark (\"?\") may represent data within the query'"
    , fmap queryBytes (mkQuery "a=/b?c?") == Just "a=/b?c?")
  , ("RFC 3986 3.4 query includes pchar ':' '@' and sub-delims"
    , isJust (mkQuery ":@!$&'()*+,;=-._~"))
  , ("RFC 9110 4.2.3 'all other components are compared in a case-sensitive manner' (POLICY: kept as received)"
    , fmap queryBytes (mkQuery "Q=%7e+X") == Just "Q=%7e+X")
  , ("RFC 3986 3.4 'terminated by a number sign (\"#\")': '#' not allowed in a query"
    , isNothing (mkQuery "a#b"))
  , ("RFC 3986 3.4 query: SP not allowed"
    , isNothing (mkQuery "a b"))
  , ("RFC 3986 2.1 'pct-encoded = \"%\" HEXDIG HEXDIG' in a query"
    , isNothing (mkQuery "a%") && isNothing (mkQuery "a%4") && isNothing (mkQuery "a%xy"))
  , ("RFC 3986 3.2.2 square brackets only in IP-literal: '[' ']' not allowed in a query"
    , isNothing (mkQuery "a[0]=1"))
  , ("RFC 3986 3.4 query: '\"', '{', '|', '\\\\', '^', '`' and non-ASCII not allowed"
    , all (isNothing . mkQuery) ["a\"b", "a{b}", "a|b", "a\\b", "a^b", "a`b", "caf\xe9"])
  ]

------------------------------------------------------------------------
-- percentDecode

percentTests :: [(String, Bool)]
percentTests =
  [ ("RFC 3986 2.1 percent-encoding: empty input decodes to empty"
    , percentDecode "" == Just "")
  , ("RFC 3986 2.1: no percent signs, unchanged"
    , percentDecode "abc/def" == Just "abc/def")
  , ("RFC 3986 2.1 'two hexadecimal digits representing that octet's numeric value': %41 is A"
    , percentDecode "%41" == Just "A")
  , ("RFC 3986 2.1 'The uppercase hexadecimal digits ... are equivalent to the lowercase digits': %4a = %4A = J"
    , percentDecode "%4a" == Just "J" && percentDecode "%4A" == Just "J")
  , ("RFC 3986 2.4 'it must be percent-encoded as \"%25\" for that octet to be used as data': %25 is %"
    , percentDecode "%25" == Just "%")
  , ("RFC 3986 2.4 'must not percent-encode or decode the same string more than once': %2541 is %41"
    , percentDecode "%2541" == Just "%41")
  , ("RFC 3986 2.1: '+' is not an encoding of SP"
    , percentDecode "a+b" == Just "a+b")
  , ("RFC 3986 2.1 '\"%20\" is the percent-encoding for the binary octet \"00100000\"'"
    , percentDecode "a%20b" == Just "a b")
  , ("RFC 3986 2.1 percent-encoding represents any octet: %00 and %FF"
    , percentDecode "%00%FF" == Just "\NUL\xff")
  , ("RFC 3986 2.1 'pct-encoded = \"%\" HEXDIG HEXDIG': bare % is malformed"
    , isNothing (percentDecode "%") && isNothing (percentDecode "a%"))
  , ("RFC 3986 2.1 'pct-encoded = \"%\" HEXDIG HEXDIG': one hex digit is malformed"
    , isNothing (percentDecode "%4") && isNothing (percentDecode "a%4"))
  , ("RFC 3986 2.1 HEXDIG: non-hex digits after % malformed"
    , isNothing (percentDecode "%G1") && isNothing (percentDecode "%1G"))
  , ("RFC 3986 2.1 HEXDIG: sign or space is not a hex digit"
    , isNothing (percentDecode "%-1") && isNothing (percentDecode "%+1") && isNothing (percentDecode "% 1"))
  , ("RFC 3986 2.1: '%%41' is malformed (the first % is not followed by two hex digits)"
    , isNothing (percentDecode "%%41"))
  ]

------------------------------------------------------------------------
-- Target

targetTests :: [(String, Bool)]
targetTests =
  [ ("RFC 9110 9.3.6 'consisting of only the host and port number of the tunnel destination'"
    , connectTarget (auth "server.example.com:443") == Just (TargetConnect (hostOf "server.example.com") 443))
  , ("RFC 9110 9.3.6 'There is no default port': CONNECT without a port rejected"
    , isNothing (connectTarget (auth "server.example.com")))
  , ("RFC 9110 9.3.6 'A server MUST reject a CONNECT request that targets an empty ... port number'"
    , isNothing (connectTarget (auth "server.example.com:")))
  , ("RFC 9110 9.3.6 'reject a CONNECT request that targets an ... invalid port number' (POLICY: port 0 invalid)"
    , isNothing (connectTarget (auth "server.example.com:0")))
  , ("RFC 9110 9.3.6 CONNECT: highest port 65535 accepted"
    , connectTarget (auth "server.example.com:65535") == Just (TargetConnect (hostOf "server.example.com") 65535))
  , ("RFC 9110 9.3.6 CONNECT to an IPv6 literal"
    , connectTarget (auth "[::1]:8443") == Just (TargetConnect (HostIPv6 (v6 [0,0,0,0,0,0,0,1])) 8443))
  , ("RFC 9110 9.3.6 CONNECT to an IPv4 address (port restriction is a proxy concern, not syntax)"
    , connectTarget (auth "192.0.2.1:25") == Just (TargetConnect (HostIPv4 0xC0000201) 25))
  , ("RFC 9110 7.1 'For CONNECT ... the request target is the host name and port number': valid with CONNECT"
    , validForMethod CONNECT (TargetConnect (hostOf "example.com") 443))
  , ("RFC 9110 7.1 'These forms MUST NOT be used with other methods': CONNECT form with GET, POST, OPTIONS invalid"
    , not (any (\m -> validForMethod m (TargetConnect (hostOf "example.com") 443)) [GET, POST, OPTIONS, HEAD]))
  , ("RFC 9110 7.1 'These forms MUST NOT be used with other methods' + 9.1 'case-sensitive': lowercase 'connect' cannot use CONNECT form"
    , not (validForMethod (meth "connect") (TargetConnect (hostOf "example.com") 443)))
  , ("RFC 9110 7.1 'For OPTIONS ... the request target can be a single asterisk'"
    , validForMethod OPTIONS TargetAsterisk)
  , ("RFC 9110 7.1 'These forms MUST NOT be used with other methods': asterisk with GET, HEAD, CONNECT, TRACE invalid"
    , not (any (\m -> validForMethod m TargetAsterisk) [GET, HEAD, CONNECT, TRACE, POST]))
  , ("RFC 9110 7.1 + 9.1 'The method token is case-sensitive': lowercase 'options' cannot use asterisk form"
    , not (validForMethod (meth "options") TargetAsterisk))
  , ("RFC 9110 7.1 'These forms MUST NOT be used with other methods': unknown method with asterisk invalid"
    , not (validForMethod (meth "FOO") TargetAsterisk))
  , ("RFC 9110 9.3.7 'If the request target is not an asterisk, the OPTIONS request applies to ... the target resource': OPTIONS with a URI valid"
    , validForMethod OPTIONS (uriT "/a" Nothing))
  , ("RFC 9110 7.1: ordinary target URI is valid for GET, POST and extension methods"
    , validForMethod GET (uriT "/" Nothing) && validForMethod POST (uriT "/x" (Just "y")) && validForMethod (meth "FOO") (uriT "/" Nothing))
  , ("RFC 9110 7.1 (POLICY: extended CONNECT, RFC 8441/9220): CONNECT with an ordinary URI is allowed"
    , validForMethod CONNECT (uriT "/chat" Nothing))
  , ("RFC 9110 4.2.3 'an empty path component is equivalent to an absolute path of \"/\", so the normal form is to provide a path of \"/\"'"
    , normalizeEmptyPath GET (uriT "" Nothing) == uriT "/" Nothing)
  , ("RFC 9110 4.2.3 empty path to '/': query preserved"
    , normalizeEmptyPath GET (uriT "" (Just "a=b")) == uriT "/" (Just "a=b"))
  , ("RFC 9110 4.2.3 'When not being used as the target of an OPTIONS request': OPTIONS empty path left alone"
    , normalizeEmptyPath OPTIONS (uriT "" Nothing) == uriT "" Nothing)
  , ("RFC 9110 4.2.3 OPTIONS exception + 9.1 'case-sensitive': lowercase 'options' is not OPTIONS, so path becomes '/'"
    , normalizeEmptyPath (meth "options") (uriT "" Nothing) == uriT "/" Nothing)
  , ("RFC 9110 4.2.3: empty path normalized for POST, CONNECT and unknown methods too"
    , all (\m -> normalizeEmptyPath m (uriT "" Nothing) == uriT "/" Nothing) [POST, CONNECT, HEAD, meth "FOO"])
  , ("RFC 9110 4.2.3 'all other components are compared in a case-sensitive manner': non-empty path untouched"
    , normalizeEmptyPath GET (uriT "/A/%7esmith" Nothing) == uriT "/A/%7esmith" Nothing)
  , ("RFC 3986 6.2.3 'Normalization should not remove delimiters when their associated component is empty': empty query kept distinct from absent query"
    , normalizeEmptyPath GET (uriT "/" (Just "")) == uriT "/" (Just ""))
  , ("RFC 3986 3.4: absent query stays absent"
    , normalizeEmptyPath GET (uriT "/" Nothing) == uriT "/" Nothing)
  , ("RFC 9110 7.1: asterisk form unchanged by normalization"
    , normalizeEmptyPath OPTIONS TargetAsterisk == TargetAsterisk)
  , ("RFC 9110 9.3.6: CONNECT form unchanged by normalization"
    , normalizeEmptyPath CONNECT (TargetConnect (hostOf "example.com") 443) == TargetConnect (hostOf "example.com") 443)
  , ("RFC 9110 4.2.1 default port is 80 for http, so :443 on http is not a default and must be kept"
    , normalizeEmptyPath GET (TargetURI HTTP (auth "example.com:443") (path "/") Nothing)
        == TargetURI HTTP (auth "example.com:443") (path "/") Nothing)
  , ("RFC 9110 4.2.2 default port is 443 for https, so :80 on https is not a default and must be kept"
    , normalizeEmptyPath GET (TargetURI HTTPS (auth "example.com:80") (path "") Nothing)
        == TargetURI HTTPS (auth "example.com:80") (path "/") Nothing)
  ]

------------------------------------------------------------------------
-- SafePath

safePathTests :: [(String, Bool)]
safePathTests =
  [ ("RFC 3986 3.3 'The path segments \".\" and \"..\", also known as dot-segments': '.' unsafe"
    , safeSeg "." == Left DotSegment)
  , ("RFC 9110 17.3 'use \"..\" as a path component to indicate a directory level above the current one': '..' unsafe"
    , safeSeg ".." == Left DotSegment)
  , ("RFC 3986 3.3 dot-segments are only complete segments '.' or '..': '...' is an ordinary name (doc: POSIX scope)"
    , safeSeg "..." == Right "...")
  , ("RFC 3986 3.3 'complete path segments': '.a', 'a.', 'a..b', '..a' are not dot-segments"
    , all (\s -> safeSeg s == Right s) [".a", "a.", "a..b", "..a", ".hidden"])
  , ("RFC 3986 3.3 'segment = *pchar' (POLICY: empty segment allowed)"
    , safeSeg "" == Right "")
  , ("RFC 9110 17.3 'avoid accessing names that have a special significance': a segment containing '/' is unsafe"
    , safeSeg "a/b" == Left ContainsSlash && safeSeg "/" == Left ContainsSlash)
  , ("RFC 9110 17.3 / doc example: '../secret' as one segment is unsafe because it contains '/'"
    , safeSeg "../secret" == Left ContainsSlash)
  , ("RFC 3986 7.3 '\"%00\" percent-encoding (NUL) may require special handling and should be rejected'"
    , safeSeg "a\NULb" == Left ContainsNUL)
  , ("RFC 3986 7.3 lists '\\\\' as special but doc limits SafePath to POSIX (POLICY): backslash allowed"
    , safeSeg "a\\b" == Right "a\\b")
  , ("RFC 9110 17.3 (doc example): /static/%2E%2E%2Fsecret is rejected"
    , safeSegs "/static/%2E%2E%2Fsecret" == Just (Left ContainsSlash))
  , ("RFC 9110 17.3 '\"..\" as a path component': literal /a/../b rejected"
    , safeSegs "/a/../b" == Just (Left DotSegment))
  , ("RFC 3986 2.3 '%2E' is equivalent to '.': /a/%2e%2E/b rejected"
    , safeSegs "/a/%2e%2E/b" == Just (Left DotSegment))
  , ("RFC 3986 2.3 '%2E' is equivalent to '.': mixed /a/.%2e/b rejected"
    , safeSegs "/a/.%2e/b" == Just (Left DotSegment))
  , ("RFC 3986 3.3 '.' dot-segment: /a/%2E/b rejected"
    , safeSegs "/a/%2E/b" == Just (Left DotSegment))
  , ("RFC 3986 3.3 trailing '..' segment rejected"
    , safeSegs "/a/.." == Just (Left DotSegment))
  , ("RFC 3986 2.2 '%2F' is a reserved character as data: lowercase %2f in a segment rejected"
    , safeSegs "/a%2fb" == Just (Left ContainsSlash))
  , ("RFC 3986 7.3 '%00 ... should be rejected'"
    , safeSegs "/a%00" == Just (Left ContainsNUL))
  , ("RFC 3986 2.4 'must not ... decode the same string more than once': %252e%252e decodes once to the safe name '%2e%2e'"
    , safeSegs "/%252e%252e" == Just (Right ["%2e%2e"]))
  , ("RFC 3986 3.3: '...' via percent-encoding is an ordinary name"
    , safeSegs "/%2e%2e%2e" == Just (Right ["..."]))
  , ("RFC 3986 3.3 'A path consists of a sequence of path segments separated by a slash'"
    , safeSegs "/a/b" == Just (Right ["a", "b"]))
  , ("RFC 9110 4.2.3 '/' equivalent to empty path (doc example): no segments"
    , safeSegs "/" == Just (Right []) && safeSegs "" == Just (Right []))
  , ("RFC 3986 3.3 'segment = *pchar' (POLICY: empty segments allowed)"
    , safeSegs "/a//b/" == Just (Right ["a", "", "b", ""]))
  , ("RFC 3986 2.1 percent-encoded backslash decodes to a POSIX-safe name (POLICY: POSIX only)"
    , safeSegs "/a%5Cb" == Just (Right ["a\\b"]))
  ]
