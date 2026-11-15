-- | Bounded exhaustive tests.
--
-- Random tests only find corner cases that the generators happen to
-- produce. These tests instead try /every/ string up to some length
-- over a small alphabet chosen to contain a representative of each
-- character class that matters, and compare the result with an
-- independent oracle. Where a structure is too long to reach by
-- enumerating characters, such as an IPv6 address, the enumeration is
-- over its shape instead.
module ExhaustiveTests (exhaustiveTests) where

import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as C
import Data.Char (chr, digitToInt, isHexDigit, ord, toLower, toUpper)
import Data.Either (isRight)
import Data.List (intercalate)
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Word (Word16)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Method
import Hyperdrive.Semantics.Syntax (isTChar)
import Hyperdrive.Semantics.URI
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Text.ParserCombinators.ReadP
import qualified URIOracle as O
import URITests (expectedAuthority, ipv6FromPieces)

allOctets :: String
allOctets = map chr [0 .. 255]

authorityOK :: String -> Bool
authorityOK = isRight . parseAuthority . C.pack

exhaustiveTests :: TestTree
exhaustiveTests = testGroup "Exhaustive"
  [ testGroup "Authority"
    [ agrees "IP-literal syntax"  "[]:.0a%@v" 6 authorityOK expectedAuthority
    , agrees "IPv4 and ports"     "0129.:5"   7 authorityOK expectedAuthority
    , agrees "h16 contents"       "0aG:"      7
        (\s -> authorityOK ("[" ++ s ++ "::]")) (\s -> expectedAuthority ("[" ++ s ++ "::]"))
    , exhaustive "reg-name normal form" "aA%2fF.:" 6 $ \s ->
        case parseAuthority (C.pack s) of
          Right a@(Authority (HostRegName _) _) -> C.unpack (renderAuthority a) == normalRegName s
          _                                     -> True
    , testProperty "every IPv6 shape" $ once prop_ipv6Shapes
    , exhaustive "dec-octet"          "0123456789" 4 $ \s ->
        let a = s ++ ".0.0.0"
        in case parseAuthority (C.pack a) of
             Right (Authority (HostIPv4 v) _) ->
               O.matches O.ipv4address a && v == fromIntegral (read s :: Int) * 2 ^ (24 :: Int)
             Right (Authority (HostRegName _) _) -> not (O.matches O.ipv4address a)
             _ -> False
    , exhaustive "port value"         "0356"      7 $ \s ->
        case parseAuthority (C.pack ("x:" ++ s)) of
          Right (Authority _ p) ->
            if null s then p == Nothing else fmap fromIntegral p == Just (read s :: Integer)
          Left AuthorityInvalidPort -> (read s :: Integer) > 65535
          Left _ -> False
    ]
  , testGroup "Every octet"
    [ agrees "reg-name"             allOctets 1 authorityOK expectedAuthority
    , agrees "IP-literal"           allOctets 1
        (\s -> authorityOK ("[v1." ++ s ++ "]")) (\s -> expectedAuthority ("[v1." ++ s ++ "]"))
    , agrees "path"                 allOctets 2 (isJust . mkPath . C.pack)  (O.matches O.pathAbempty)
    , agrees "query"                allOctets 2 (isJust . mkQuery . C.pack) (O.matches O.query)
    , agrees "scheme"               allOctets 2 (isJust . mkScheme . C.pack) (O.matches O.scheme)
    , agrees "field value"          allOctets 2
        (isRight . mkFieldValue . C.pack) (fieldValueOracle . stripOWS)
    ]
  , testGroup "Scheme, path and query"
    [ agrees "scheme"       "a1+-.:"    6 (isJust . mkScheme . C.pack) (O.matches O.scheme)
    , agrees "path"         "/%aF.?:2 " 6 (isJust . mkPath . C.pack)   (O.matches O.pathAbempty)
    , agrees "query"        "/%aF?#: "  6 (isJust . mkQuery . C.pack)  (O.matches O.query)
    , exhaustive "pathSegments" "/.%20eE"   7 $ \s ->
        case mkPath (C.pack s) of
          Nothing -> True
          Just p  -> fmap (map C.unpack) (pathSegments p) == expectedSegments s
    ]
  , testGroup "Fields and methods"
    [ agrees "field value"  " \t\r\n\0a\DEL\128" 6
        (isRight . mkFieldValue . C.pack) (fieldValueOracle . stripOWS)
    , exhaustive "field value error" "\r\n\0\1\DEL a" 6 $ \s ->
        case mkFieldValue (C.pack s) of
          Left ContainsCRLFNUL -> any (`elem` "\r\n\0") s
          Left ContainsCTL     -> not (any (`elem` "\r\n\0") s)
          Right _              -> True
    , agrees "field name"   "aA-:( \128" 5 (isJust . mkFieldName . C.pack) tokenOracle
    , agrees "method"       "aA-:( \128" 5 (isJust . mkMethod . C.pack)    tokenOracle
    ]
  ]

-- | all strings over the alphabet of length at most n
stringsUpTo :: String -> Int -> [String]
stringsUpTo alphabet n = concatMap (`replicateM` alphabet) [0 .. n]

describe :: String -> String -> Int -> String
describe name alphabet n =
  name ++ " (all " ++ show size ++ " strings of length <= " ++ show n ++ " over " ++ alphabetName ++ ")"
  where
    size = sum [ length alphabet ^ k | k <- [0 .. n] ]
    alphabetName | length alphabet == 256 = "all octets"
                 | otherwise              = show alphabet

-- | Every string over the alphabet of length at most n satisfies the
-- predicate. Reports the first few that fail.
exhaustive :: String -> String -> Int -> (String -> Bool) -> TestTree
exhaustive name alphabet n ok = testProperty (describe name alphabet n) $ once $
  let failures = filter (not . ok) (stringsUpTo alphabet n)
  in counterexample ("failed for: " ++ show (take 10 failures)) (null failures)

-- | The implementation accepts exactly the strings the oracle accepts.
-- So that the test cannot pass vacuously, the oracle must accept some
-- strings and reject others.
agrees :: String -> String -> Int -> (String -> Bool) -> (String -> Bool) -> TestTree
agrees name alphabet n impl oracle = testProperty (describe name alphabet n) $ once $
  let xs        = stringsUpTo alphabet n
      failures  = filter (\x -> impl x /= oracle x) xs
      accepted  = length (filter oracle xs)
  in counterexample ("disagrees on: " ++ show (take 10 failures)) (null failures)
     .&&. counterexample ("vacuous: the oracle accepted " ++ show accepted ++ " of " ++ show (length xs))
            (accepted > 0 && accepted < length xs)

------------------------------------------------------------------------------
-- IPv6 shapes
------------------------------------------------------------------------------

-- | Every combination of: 0 to 9 pieces before the "::", 0 to 9 after,
-- with or without the "::", and an IPv4 address in no position or in
-- any one position. Checks acceptance against the oracle and, when
-- accepted, the value.
prop_ipv6Shapes :: Property
prop_ipv6Shapes =
  let shapes = [ (l, r, elide, v4)
               | l <- [0 .. 9], r <- [0 .. 9], elide <- [False, True]
               , v4 <- Nothing : map Just [0 .. l + r - 1] ]
      failures = filter (not . check) shapes
  in counterexample ("failed for: " ++ show (take 10 failures)) (null failures)
  where
    check (l, r, elide, v4) =
      let tokens = [ if Just i == v4 then "1.2.3.4" else "1" | i <- [0 .. l + r - 1] ]
          (ls, rs) = splitAt l tokens
          txt | elide     = intercalate ":" ls ++ "::" ++ intercalate ":" rs
              | otherwise = intercalate ":" tokens
          a = "[" ++ txt ++ "]"
          value ts = concatMap (\t -> if t == "1" then [1] else [0x0102, 0x0304]) ts :: [Word16]
          pieces | elide     = value ls ++ replicate (8 - length (value ls ++ value rs)) 0 ++ value rs
                 | otherwise = value tokens
      in case parseAuthority (C.pack a) of
           Right (Authority (HostIPv6 v) Nothing) ->
             expectedAuthority a && v == ipv6FromPieces pieces
           Right _ -> False
           Left _  -> not (expectedAuthority a)

------------------------------------------------------------------------------
-- independent oracles
------------------------------------------------------------------------------

-- | Split on "/" and percent-decode each segment, rejecting NUL,
-- written without reference to the implementation.
expectedSegments :: String -> Either PathSegmentError [String]
expectedSegments s =
  case splitOn '/' s of
    ("" : segs) | segs /= [""] ->
      let decoded = map decode segs
      in if any ('\0' `elem`) decoded then Left SegmentContainsNUL else Right decoded
    _ -> Right []
  where
    splitOn c xs = case break (== c) xs of
      (a, [])     -> [a]
      (a, _ : bs) -> a : splitOn c bs
    decode ('%' : a : b : rest)
      | isHexDigit a && isHexDigit b = chr (digitToInt a * 16 + digitToInt b) : decode rest
    decode (c : rest) = c : decode rest
    decode [] = []

-- | RFC 3986, Section 3.2.2: lower case, except for the hex digits of
-- percent-encodings, which are upper case. The port, if any, is left
-- alone apart from dropping an empty one.
normalRegName :: String -> String
normalRegName s =
  let (h, p) = break (== ':') s
      port = case drop 1 p of
               "" -> ""
               ds -> ':' : show (read ds :: Int)
  in go h ++ port
  where
    go ('%' : a : b : rest) = '%' : toUpper a : toUpper b : go rest
    go (c : rest)           = toLower c : go rest
    go []                   = []

stripOWS :: String -> String
stripOWS = L.dropWhileEnd (`elem` " \t") . dropWhile (`elem` " \t")

-- | RFC 9110, Section 5.5, transcribed into 'ReadP':
--
-- > field-value    = *field-content
-- > field-content  = field-vchar [ 1*( SP / HTAB / field-vchar ) field-vchar ]
-- > field-vchar    = VCHAR / obs-text
fieldValueOracle :: String -> Bool
fieldValueOracle s = not (null (readP_to_S (fieldValue <* eof) s))
  where
    fieldValue   = many fieldContent
    fieldContent = do
      _ <- fieldVChar
      optional (many1 (fieldVChar +++ char ' ' +++ char '\t') >> fieldVChar)
    fieldVChar = satisfy (\c -> (ord c >= 0x21 && ord c <= 0x7E) || ord c >= 0x80)

-- | @token = 1*tchar@, using the byte level 'isTChar', which is itself
-- checked exhaustively against the RFC's alternative definition
tokenOracle :: String -> Bool
tokenOracle s = not (null s) && all (isTChar . fromIntegral . ord) s
