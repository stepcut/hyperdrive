{-# LANGUAGE OverloadedStrings #-}
-- | Tests for inputs that are valid by the RFCs but dangerous at the
-- boundary with some other system: a proxy, a resolver, an allow-list.
-- Each is a regression test for an attack found by reviewing the
-- library; see docs/design-notes.md.
module SecurityTests (securityTests) where

import Control.Monad (replicateM, void)
import Data.Bits ((.|.))
import qualified Data.ByteString.Char8 as C
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit)
import Data.Word (Word32, Word64)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.FieldSyntax
import Hyperdrive.Semantics.URI
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Text.ParserCombinators.ReadP

securityTests :: TestTree
securityTests = testGroup "Security"
  [ testGroup "Repeated singleton fields"
    [ testProperty "lookupSingleton rejects a repeated field" prop_lookupSingleton
    ]
  , testGroup "Repeated parameters"
    [ testProperty "lookupParameter rejects a repeated name" prop_lookupParameter
    ]
  , testGroup "Equivalent hosts"
    [ testProperty "IPv4-mapped IPv6 is the IPv4 address"    prop_mappedIsIPv4
    , testProperty "canonicalHost examples"                 prop_canonicalHostExamples
    , testProperty "canonicalHost is idempotent"            prop_canonicalIdempotent
    , testProperty "authorityKey examples"                  prop_authorityKey
    ]
  , testGroup "Numeric look-alike hosts"
    [ testProperty "anything inet_aton reads as an address is IPv4 or flagged (exhaustive)"
        prop_inetAtonCovered
    , testProperty "real host names are not flagged"        prop_realNamesNotFlagged
    ]
  , testGroup "DNS host names"
    [ testProperty "isDnsHostName agrees with the RFC 1035 grammar (exhaustive)" prop_dnsHostNameExhaustive
    , testProperty "examples"                                prop_dnsHostNameExamples
    , testProperty "length limits"                           prop_dnsHostNameLengths
    ]
  ]

field :: C.ByteString -> FieldName
field = maybe (error "invalid field name") id . mkFieldName

value :: C.ByteString -> FieldValue
value = either (error "invalid field value") id . mkFieldValue

host :: C.ByteString -> Host
host = either (error . ("invalid host: " ++) . show) authorityHost . parseAuthority

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.3
-- # Recipients often
-- # attempt to handle this error by using the last syntactically valid
-- # member of the list, leading to potential interoperability and
-- # security issues if different implementations have different error
-- # handling behaviors.
prop_lookupSingleton :: Property
prop_lookupSingleton = conjoin
  [ lookupSingleton hHost (fs [])                                   === Right Nothing
  , lookupSingleton hHost (fs [("Host", "a")])                      === Right (Just (value "a"))
  , lookupSingleton hHost (fs [("Host", "a"), ("X", "b")])          === Right (Just (value "a"))
  , lookupSingleton hHost (fs [("Host", "good"), ("Host", "evil")]) === Left (RepeatedField hHost)
  , lookupSingleton hHost (fs [("Host", "a"), ("HOST", "a")])       === Left (RepeatedField hHost)
  -- lookupFirst keeps its documented, unsafe behaviour
  , lookupFirst hHost (fs [("Host", "good"), ("Host", "evil")])     === Just (value "good")
  ]
  where fs ls = fieldsFromList [ (field n, value v) | (n, v) <- ls ]

prop_lookupParameter :: Property
prop_lookupParameter = conjoin
  [ (parseParameters ";charset=utf-8" >>= lookupParameter "charset")                === Right (Just "utf-8")
  , (parseParameters ";charset=utf-8" >>= lookupParameter "CharSet")                === Right (Just "utf-8")
  , (parseParameters ";q=1" >>= lookupParameter "charset")                          === Right Nothing
  , (parseParameters ";charset=utf-8;charset=utf-7" >>= lookupParameter "charset")  === Left (DuplicateParameter "charset")
  , (parseParameters ";charset=utf-8;Charset=\"utf-8\"" >>= lookupParameter "charset") === Left (DuplicateParameter "charset")
  ]

mapped :: Word32 -> Host
mapped a = HostIPv6 (IPv6 0 (0x0000FFFF00000000 .|. (fromIntegral a :: Word64)))

prop_mappedIsIPv4 :: Word32 -> Property
prop_mappedIsIPv4 a = canonicalHost (mapped a) === HostIPv4 a

prop_canonicalHostExamples :: Property
prop_canonicalHostExamples = conjoin
  [ canonicalHost (host "[::ffff:127.0.0.1]") === host "127.0.0.1"
  , canonicalHost (host "[::FFFF:7f00:1]")    === host "127.0.0.1"
  , canonicalHost (host "admin.example.com.") === host "admin.example.com"
  , canonicalHost (host "1.2.3.4.")           === host "1.2.3.4"
    -- more than one trailing dot is an empty label, not the same name,
    -- and "." itself is left alone
  , canonicalHost (host "example.com..")      === host "example.com.."
  , canonicalHost (host ".")                  === host "."
    -- not mapped: IPv4-translated, IPv4-compatible and other IPv6
  , canonicalHost (host "[::ffff:0:1.2.3.4]") === host "[::ffff:0:1.2.3.4]"
  , canonicalHost (host "[::1.2.3.4]")        === host "[::1.2.3.4]"
  , canonicalHost (host "[::1]")              === host "[::1]"
  , canonicalHost (host "example.com")        === host "example.com"
  ]

-- | Exhaustive over every host up to length 6 that can spell trailing
-- dots, IPv4 and IPv6 addresses (including IPv4-mapped ones) and names.
prop_canonicalIdempotent :: Property
prop_canonicalIdempotent = once $
  let hosts    = [ h | s <- concatMap (`replicateM` "1.:[]f") [1 .. 6]
                     , Right (Authority h Nothing) <- [parseAuthority (C.pack s)] ]
      failures = [ h | h <- hosts, canonicalHost (canonicalHost h) /= canonicalHost h ]
  in counterexample ("not idempotent: " ++ show (take 10 failures)) (null failures)
     .&&. counterexample "vacuous" (length hosts > 1000)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.3
-- # If the port is equal to the default port for a scheme, the normal
-- # form is to omit the port subcomponent.
prop_authorityKey :: Property
prop_authorityKey = conjoin
  [ key HTTP  "example.com:80"    === key HTTP  "Example.COM."
  , key HTTPS "example.com:443"   === key HTTPS "example.com"
  , key HTTP  "example.com:443"   =/= key HTTP  "example.com"
  , key HTTPS "example.com:80"    =/= key HTTPS "example.com"
  , key HTTP  "[::ffff:10.0.0.1]" === key HTTP  "10.0.0.1:80"
  , key HTTP  "example.com:"      === key HTTP  "example.com"
  ]
  where key s a = either (error "invalid authority") (authorityKey s) (parseAuthority a)

-- | A model of what inet_aton accepts, independent of the
-- implementation: one to four parts separated by ".", each a number in
-- decimal, in octal with a leading 0, or in hex with a leading 0x.
-- Values are not range checked, so this accepts more than inet_aton.
inetAtonLike :: String -> Bool
inetAtonLike s =
  let parts = splitOn s
  in length parts >= 1 && length parts <= 4 && all number parts
  where
    splitOn str = case break (== '.') str of
      (a, [])       -> [a]
      (a, _ : rest) -> a : splitOn rest
    number ('0' : x : hex) | x `elem` ("xX" :: String) = all isHexDigit hex
    number ('0' : oct)                                 = all isOctDigit oct
    number dec                                         = not (null dec) && all isDigit dec

-- = https://www.rfc-editor.org/rfc/rfc3986#section-7.4
-- # However, they
-- # can become a security concern if an application attempts to filter
-- # access to resources based on the IP address in string literal format.

-- | Every string up to length 7 over digits, "x", "f" and ".", with and
-- without a trailing dot: if a resolver could read it as an IPv4
-- address, then it is either an IPv4 address to us or a flagged
-- registered name. It is never an ordinary registered name that an
-- allow-list would compare as text.
prop_inetAtonCovered :: Property
prop_inetAtonCovered = once $
  let candidates = [ s' | s <- concatMap (`replicateM` "0179xf.") [1 .. 7]
                        , inetAtonLike s
                        , s' <- [s, s ++ "."] ]
      covered s = case parseAuthority (C.pack s) of
        Right (Authority (HostIPv4 _) _)    -> True
        Right (Authority (HostRegName r) _) -> isNumericLookalike r
        _                                   -> True -- not a host at all
      failures = filter (not . covered) candidates
  in counterexample ("not covered: " ++ show (take 10 failures)) (null failures)
     .&&. counterexample "vacuous" (length candidates > 10000)

-- RFC 1123, Section 2.1 (not checked by duvet, which cannot parse that
-- RFC's section headings): "However, a valid host name can never have
-- the dotted-decimal form #.#.#.#, since at least the highest-level
-- component label will be alphabetic."
prop_realNamesNotFlagged :: Property
prop_realNamesNotFlagged = conjoin
  [ counterexample (show n) (flagged n === False)
  | n <- [ "example.com", "example.com.", "1password.com", "123.example.com"
         , "127.0.0.1.nip.io", "a1", "0xdead.beef.com", "xn--bcher-kva.example" ] ]
  where
    flagged n = case host n of
      HostRegName r -> isNumericLookalike r
      _             -> True

-- | The preferred name syntax of RFC 1035 Section 2.3.1, with the first
-- character relaxed to a letter or digit by RFC 1123 Section 2.1, as a
-- ReadP grammar: labels of letters, digits and hyphens that start and
-- end with a letter or digit, separated by dots, with an optional
-- trailing dot. Length limits are checked separately.
dnsNameO :: String -> Bool
dnsNameO s = not (null [ () | ((), "") <- readP_to_S (name <* eof) s ])
  where
    name  = ldhLabel >> many (char '.' >> ldhLabel) >> optional (char '.')
    ldhLabel = letDig >> optional (many letDigHyp >> letDig)
    letDig    = void (satisfy (\c -> isAsciiLower c || isAsciiUpper c || isDigit c))
    letDigHyp = letDig +++ void (char '-')

-- | Every registered name up to length 7 over an alphabet that can spell
-- labels, hyphens, empty labels, numeric labels, percent-encodings and
-- underscores: isDnsHostName holds exactly when the grammar matches and
-- the name is not a numeric look-alike.
prop_dnsHostNameExhaustive :: Property
prop_dnsHostNameExhaustive = once $
  let names = [ r | s <- concatMap (`replicateM` "a1-.%_x") [1 .. 7]
                  , Right (Authority (HostRegName r) Nothing) <- [parseAuthority (C.pack s)] ]
      expected r = dnsNameO (C.unpack (regNameBytes r)) && not (isNumericLookalike r)
      failures = [ r | r <- names, isDnsHostName r /= expected r ]
      valid = length (filter expected names)
  in counterexample ("disagrees on: " ++ show (take 10 failures)) (null failures)
     .&&. counterexample "vacuous" (valid > 1000 && valid < length names)

regName :: C.ByteString -> RegName
regName bs = case host bs of
  HostRegName r -> r
  h             -> error ("not a registered name: " ++ show h)

prop_dnsHostNameExamples :: Property
prop_dnsHostNameExamples = conjoin
  [ counterexample (show n) (isDnsHostName (regName n) === ok)
  | (n, ok) <-
      [ ("example.com", True), ("example.com.", True), ("localhost", True)
      , ("xn--bcher-kva.example", True), ("1password.com", True), ("a-b.c", True)
      , ("EXAMPLE.com", True), ("a", True), ("3com.com", True)
      -- percent-encoding, including encoded delimiters and controls
      , ("ho%00st", False), ("ho%40st", False), ("ho%2Fst", False), ("a%C2%ADb", False)
      -- other characters RFC 3986 allows in a reg-name
      , ("a_b.com", False), ("a!b", False), ("a~b", False), ("a=b", False)
      -- hyphens and empty labels
      , ("-a.com", False), ("a-.com", False), ("a..b", False), (".", False)
      , (".a", False), ("example.com..", False)
      -- numeric look-alikes
      , ("127.1", False), ("0x7f.0.0.1", False), ("2130706433", False), ("1.2.3.4.", False)
      ] ]

prop_dnsHostNameLengths :: Property
prop_dnsHostNameLengths = conjoin
  [ isDnsHostName (regName (labelOf 63))              === True
  , isDnsHostName (regName (labelOf 64))              === False
  , isDnsHostName (regName (name 253))                === True
  , isDnsHostName (regName (name 253 <> "."))         === True
  , isDnsHostName (regName (name 254))                === False
  ]
  where
    labelOf n = C.replicate n 'a' <> ".com"
    -- labels of 63 joined by dots, cut to exactly n characters, ending in
    -- a letter
    name n = C.take n (C.intercalate "." (replicate 5 (C.replicate 63 'a')))
