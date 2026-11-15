{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module URITests (uriTests, expectedAuthority, ipv6FromPieces) where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit, toLower, toUpper)
import Data.Either (isRight)
import Data.List (intercalate)
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Word (Word16, Word8)
import Hyperdrive.Semantics.Method
import Hyperdrive.Semantics.Target
import Hyperdrive.Semantics.URI
import Numeric (showHex)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import qualified URIOracle as O

uriTests :: TestTree
uriTests = testGroup "URI"
  [ testGroup "Authority"
    [ testProperty "parseAuthority agrees with the ABNF"  prop_authorityGrammar
    , testProperty "IPv6 addresses parse to their value"   prop_ipv6Value
    , testProperty "IPv4 addresses parse to their value"   prop_ipv4Value
    , testProperty "IPv4address wins over reg-name"        prop_ipv4FirstMatch
    , testProperty "render then parse is the identity"     prop_authorityRoundTrip
    , testProperty "host is case-insensitive"              prop_hostCaseInsensitive
    , testProperty "examples"                              prop_authorityExamples
    , testProperty "IPv6 is rendered as in RFC 5952"       prop_renderIPv6
    ]
  , testGroup "Scheme"
    [ testProperty "mkScheme agrees with the ABNF"         prop_schemeGrammar
    , testProperty "scheme is case-insensitive"            prop_schemeCaseInsensitive
    ]
  , testGroup "Path"
    [ testProperty "mkPath agrees with the ABNF"           prop_pathGrammar
    , testProperty "pathSegments decodes segments"         prop_pathSegments
    , testProperty "dot-segments are rejected"             prop_dotSegments
    ]
  , testGroup "Query"
    [ testProperty "mkQuery agrees with the ABNF"          prop_queryGrammar
    ]
  , testGroup "Target"
    [ testProperty "special forms only with their method"  prop_validForMethod
    , testProperty "CONNECT needs a non-zero port"         prop_connectTarget
    , testProperty "empty path is / except for OPTIONS"    prop_normalizeTarget
    ]
  ]

------------------------------------------------------------------------------
-- generators
------------------------------------------------------------------------------

-- | characters that matter to URI syntax, and some that are never
-- allowed
uriAlphabet :: String
uriAlphabet = "0123456789abcdefABCDEFvVxyz:.[]%@-_~!$&'()*+,;=/? #\"<>{}|^`\DEL\128"

junk :: Gen String
junk = listOf (elements uriAlphabet)

-- | delete, insert or replace one character
mutate :: String -> Gen String
mutate s = do
  i <- choose (0, length s)
  c <- elements uriAlphabet
  let (a, b) = splitAt i s
  elements [ a ++ drop 1 b, a ++ c : b, a ++ c : drop 1 b ]

-- | valid text, junk, or valid text with a small mistake in it
mixed :: Gen String -> Gen String
mixed valid = frequency [ (4, valid), (2, junk), (3, valid >>= mutate) ]

hexPiece :: Word16 -> Gen String
hexPiece p = do
  let h = showHex p ""
  zeros <- choose (0, 4 - length h)
  cased <- traverse (\c -> elements [toLower c, toUpper c]) h
  pure (replicate zeros '0' ++ cased)

-- | an IPv6 address and some valid textual form of it
genIPv6 :: Gen (String, [Word16])
genIPv6 = do
  ps <- vectorOf 8 (frequency [(3, pure 0), (1, arbitrary)])
  ipv4Tail <- arbitrary
  -- choose a run of zeros (possibly empty) to elide
  let zeroRuns = [ (i, n) | i <- [0 .. 7], n <- [1 .. 8 - i], all (== 0) (take n (drop i ps)) ]
  elide <- if null zeroRuns then pure Nothing else frequency [(1, pure Nothing), (3, Just <$> elements zeroRuns)]
  let render qs
        | ipv4Tail && length qs >= 2 = do
            front <- traverse hexPiece (take (length qs - 2) qs)
            pure (front ++ [dotted (qs !! (length qs - 2)) (last qs)])
        | otherwise = traverse hexPiece qs
      dotted a b = intercalate "." (map show [a `div` 256, a `mod` 256, b `div` 256, b `mod` 256])
  txt <- case elide of
    Nothing -> intercalate ":" <$> render ps
    Just (i, n) -> do
      -- an IPv4 tail can only be in the right hand part
      l <- traverse hexPiece (take i ps)
      r <- render (drop (i + n) ps)
      pure (intercalate ":" l ++ "::" ++ intercalate ":" r)
  pure (txt, ps)

-- | IPv6-like text with an arbitrary number of pieces, with or without
-- "::", which is mostly not valid
genIPv6Shape :: Gen String
genIPv6Shape = do
  l <- choose (0, 9)
  r <- choose (0, 9)
  elide <- arbitrary
  ipv4Tail <- frequency [(4, pure False), (1, pure True)]
  ls <- vectorOf l (arbitrary >>= hexPiece)
  rs <- vectorOf r (arbitrary >>= hexPiece)
  let rs' = if ipv4Tail then rs ++ ["1.2.3.4"] else rs
  pure (if elide then intercalate ":" ls ++ "::" ++ intercalate ":" rs'
                 else intercalate ":" (ls ++ rs'))

genIPv4 :: Gen (String, [Word8])
genIPv4 = do
  os <- vectorOf 4 arbitrary
  pure (intercalate "." (map show os), os)

genRegName :: Gen String
genRegName = concat <$> listOf (frequency
  [ (8, (: []) <$> elements "abcxyzABCXYZ0123456789-._~")
  , (2, (: []) <$> elements "!$&'()*+,;=")
  , (1, do a <- elements "0123456789abcdefABCDEF"; b <- elements "0123456789abcdefABCDEF"; pure ['%', a, b])
  ])

genIPvFuture :: Gen String
genIPvFuture = do
  v <- elements "vV"
  ver <- listOf1 (elements "0123456789abcdefABCDEF")
  rest <- listOf1 (elements "abcXYZ019-._~!$&'()*+,;=:")
  pure ("[" ++ v : ver ++ "." ++ rest ++ "]")

genHost :: Gen String
genHost = oneof
  [ (\(t, _) -> "[" ++ t ++ "]") <$> genIPv6
  , (\t -> "[" ++ t ++ "]") <$> genIPv6Shape
  , fst <$> genIPv4
  , genRegName
  , genIPvFuture
  ]

genAuthority :: Gen String
genAuthority = mixed $ do
  h <- genHost
  p <- frequency [ (2, pure Nothing), (3, Just <$> listOf (elements "0123456789")) ]
  pure (h ++ maybe "" (':' :) p)

pack :: String -> B.ByteString
pack = C.pack

-- | for test values that are valid by construction
unsafeJust :: Maybe a -> a
unsafeJust = maybe (error "unsafeJust: Nothing") id

unsafeRight :: Show e => Either e a -> a
unsafeRight = either (error . ("unsafeRight: " ++) . show) id

------------------------------------------------------------------------------
-- Authority
------------------------------------------------------------------------------

-- | An independent statement of what parseAuthority should accept: the
-- RFC 9110 Host grammar, minus an empty host (RFC 9110 Section 4.2.1)
-- and minus ports that do not fit in 16 bits.
expectedAuthority :: String -> Bool
expectedAuthority s =
  O.matches O.hostPort s && not (null hostPart) && portFits
  where
    -- a ":" can only appear in the host inside "[...]"
    (hostPart, portPart) = case s of
      '[' : _ -> let (h, r) = break (== ']') s in (h ++ take 1 r, drop 1 (drop 1 r))
      _       -> let (h, r) = break (== ':') s in (h, drop 1 r)
    portFits = all isDigit portPart && (null portPart || (read portPart :: Integer) <= 65535)

prop_authorityGrammar :: Property
prop_authorityGrammar = checkCoverage $ forAll genAuthority $ \s ->
  let expected = expectedAuthority s
  in cover 30 expected "valid" $
     cover 20 (not expected) "invalid" $
     cover 5 (expected && take 1 s == "[") "valid IP-literal" $
     counterexample (show (parseAuthority (pack s))) $
       isRight (parseAuthority (pack s)) === expected

ipv6FromPieces :: [Word16] -> IPv6
ipv6FromPieces ps =
  let w64 = L.foldl' (\acc p -> (acc `shiftL` 16) .|. fromIntegral p) 0
  in IPv6 (w64 (take 4 ps)) (w64 (drop 4 ps))

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # A 128-bit IPv6 address is divided into eight 16-bit pieces.
prop_ipv6Value :: Property
prop_ipv6Value = forAll genIPv6 $ \(txt, ps) ->
  counterexample txt $
    parseAuthority (pack ("[" ++ txt ++ "]"))
      === Right (Authority (HostIPv6 (ipv6FromPieces ps)) Nothing)

prop_ipv4Value :: Property
prop_ipv4Value = forAll genIPv4 $ \(txt, os) ->
  parseAuthority (pack txt)
    === Right (Authority (HostIPv4 (L.foldl' (\acc o -> acc * 256 + fromIntegral o) 0 os)) Nothing)

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # If host matches the rule for IPv4address, then it should be
-- # considered an IPv4 address literal and not a reg-name.
prop_ipv4FirstMatch :: Property
prop_ipv4FirstMatch = checkCoverage $
  -- no ":", so that the whole string is the host
  forAll (filter (/= ':') <$> mixed (oneof [fst <$> genIPv4, genRegName, (\(t, _) -> t ++ "0") <$> genIPv4])) $ \s ->
    let isIPv4 = case parseAuthority (pack s) of
                   Right (Authority (HostIPv4 _) _) -> True
                   _                                -> False
    in cover 20 isIPv4 "IPv4" $ isIPv4 === O.matches O.ipv4address s

prop_authorityRoundTrip :: Property
prop_authorityRoundTrip = forAll genAuthority $ \s ->
  case parseAuthority (pack s) of
    Left _  -> discard
    Right a -> parseAuthority (renderAuthority a) === Right a

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # The host subcomponent is case-
-- # insensitive.
prop_hostCaseInsensitive :: Property
prop_hostCaseInsensitive = forAll genAuthority $ \s ->
  parseAuthority (pack (map toUpper s)) === parseAuthority (pack (map toLower s))

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.2.2
-- # If host matches the rule for IPv4address, then it should be
-- # considered an IPv4 address literal and not a reg-name.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.4
-- # Before making use of an "http" or "https" URI reference received from
-- # an untrusted source, a recipient SHOULD parse for userinfo and treat
-- # its presence as an error

-- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.1
-- # A recipient that processes such a URI reference MUST
-- # reject it as invalid.
prop_authorityExamples :: Property
prop_authorityExamples = conjoin
  [ fmap host' (parseAuthority "1.2.3.4")     === Right "HostIPv4 16909060"
  , fmap host' (parseAuthority "1.2.3.256")   === Right "HostRegName (RegName \"1.2.3.256\")"
  , fmap host' (parseAuthority "01.2.3.4")    === Right "HostRegName (RegName \"01.2.3.4\")"
  , fmap renderAuthority (parseAuthority "Example.COM%2f:")  === Right "example.com%2F"
  , fmap authorityPort (parseAuthority "x:0080")  === Right (Just 80)
  , fmap authorityPort (parseAuthority "x:65535") === Right (Just 65535)
  , parseAuthority "x:65536"        === Left AuthorityInvalidPort
  , parseAuthority "user@x"         === Left AuthorityHasUserinfo
  , parseAuthority ""               === Left AuthorityEmptyHost
  , parseAuthority ":80"            === Left AuthorityEmptyHost
  , parseAuthority "[::1"           === Left AuthorityInvalidHost
  , parseAuthority "[::1]x"         === Left AuthorityInvalidHost
  , fmap renderAuthority (parseAuthority "[::FFFF:1.2.3.4]:443") === Right "[::ffff:102:304]:443"
  ]
  where host' = show . authorityHost

-- | RFC 5952, Section 4.2: the longest run of zeros is elided, the first
-- if there is a tie, and a single zero is never elided.
prop_renderIPv6 :: Property
prop_renderIPv6 = conjoin
  [ r "[2001:db8:0:0:1:0:0:1]"   === Right "[2001:db8::1:0:0:1]"
  , r "[2001:db8:0:1:1:1:1:1]"   === Right "[2001:db8:0:1:1:1:1:1]"
  , r "[2001:0:0:1:0:0:0:1]"     === Right "[2001:0:0:1::1]"
  , r "[0:0:0:0:0:0:0:0]"        === Right "[::]"
  , r "[2001:DB8::0001]"         === Right "[2001:db8::1]"
  ]
  where r = fmap renderAuthority . parseAuthority

------------------------------------------------------------------------------
-- Scheme
------------------------------------------------------------------------------

prop_schemeGrammar :: Property
prop_schemeGrammar = checkCoverage $ forAll (mixed (elements ["http", "https", "coap+tcp", "a.b-c"])) $ \s ->
  let expected = O.matches O.scheme s
  in cover 30 expected "valid" $ cover 15 (not expected) "invalid" $
     isJust (mkScheme (pack s)) === expected

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.1
-- # Although schemes are case-
-- # insensitive, the canonical form is lowercase
prop_schemeCaseInsensitive :: Property
prop_schemeCaseInsensitive = conjoin
  [ mkScheme "HTTP" === Just HTTP
  , mkScheme "hTTpS" === Just HTTPS
  ]

------------------------------------------------------------------------------
-- Path
------------------------------------------------------------------------------

genPath :: Gen String
genPath = mixed (concat <$> listOf (('/' :) <$> genSegment))
  where genSegment = filter (`notElem` ("[]" :: String)) <$> genRegName

prop_pathGrammar :: Property
prop_pathGrammar = checkCoverage $ forAll genPath $ \s ->
  let expected = O.matches O.pathAbempty s
  in cover 30 expected "valid" $ cover 15 (not expected) "invalid" $
     isJust (mkPath (pack s)) === expected

-- | percent-encode a segment, encoding everything except unreserved
-- characters, and sometimes those too
encodeSegment :: B.ByteString -> Gen String
encodeSegment = fmap concat . traverse enc . B.unpack
  where
    enc w
      | C.elem (toEnum (fromIntegral w)) "abcxyzABCXYZ0123456789-._~" =
          frequency [(4, pure [toEnum (fromIntegral w)]), (1, pure (pct w))]
      | otherwise = pure (pct w)
    pct w = '%' : map toUpper (if w < 16 then '0' : showHex w "" else showHex w "")

prop_pathSegments :: Property
prop_pathSegments =
  forAll (listOf (B.pack <$> listOf (frequency [(4, elements (B.unpack "abc.%/")), (1, arbitrary)]))) $ \segs ->
    not (any (`elem` [".", ".."]) segs) && segs /= [""] ==>
      forAll (traverse encodeSegment segs) $ \encoded ->
        let p = unsafeJust (mkPath (pack (concatMap ('/' :) encoded)))
        in pathSegments p === Right segs

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.3
-- # The path segments "." and "..", also known as dot-segments, are
-- # defined for relative reference within the path name hierarchy.
prop_dotSegments :: Property
prop_dotSegments =
  forAll (elements [".", "..", "%2e", "%2E", ".%2e", "%2E%2e"]) $ \dot ->
  forAll (listOf (elements ["a", "b", ""])) $ \before ->
  forAll (listOf (elements ["a", "b", ""])) $ \rest ->
    let p = unsafeJust (mkPath (pack (concatMap ('/' :) (before ++ [dot] ++ rest))))
    in pathSegments p === Left DotSegment

------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

prop_queryGrammar :: Property
prop_queryGrammar = checkCoverage $ forAll (mixed (concat <$> listOf (elements ["a=b", "&", "/", "?", "%20", ":@"]))) $ \s ->
  let expected = O.matches O.query s
  in cover 30 expected "valid" $ cover 15 (not expected) "invalid" $
     isJust (mkQuery (pack s)) === expected

------------------------------------------------------------------------------
-- Target
------------------------------------------------------------------------------

exampleURI :: B.ByteString -> Target
exampleURI p =
  TargetURI HTTP (unsafeRight (parseAuthority "example.com")) (unsafeJust (mkPath p)) Nothing

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.1
-- # These forms MUST
-- # NOT be used with other methods.
prop_validForMethod :: Property
prop_validForMethod =
  forAll (elements [GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE]) $ \m ->
    let c = unsafeJust (connectTarget (unsafeRight (parseAuthority "example.com:443")))
    in conjoin
       [ validForMethod m c              === (m == CONNECT)
       , validForMethod m TargetAsterisk === (m == OPTIONS)
       , validForMethod m (exampleURI "/") === True
       ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.3.6
-- # A server MUST reject a CONNECT request that targets an empty or
-- # invalid port number
prop_connectTarget :: Property
prop_connectTarget = conjoin
  [ fmap connectTarget (parseAuthority "example.com")    === Right Nothing
  , fmap connectTarget (parseAuthority "example.com:")   === Right Nothing
  , fmap connectTarget (parseAuthority "example.com:0")  === Right Nothing
  , fmap (fmap (const ())) (fmap connectTarget (parseAuthority "example.com:443")) === Right (Just ())
  ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.3
-- # When not being used as the target of an OPTIONS request, an empty
-- # path component is equivalent to an absolute path of "/"
prop_normalizeTarget :: Property
prop_normalizeTarget = conjoin
  [ normalizeTarget GET     (exampleURI "")   === exampleURI "/"
  , normalizeTarget OPTIONS (exampleURI "")   === exampleURI ""
  , normalizeTarget GET     (exampleURI "/a") === exampleURI "/a"
  ]
