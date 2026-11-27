{-# LANGUAGE OverloadedStrings #-}
-- | The httpwg's structured-field-tests
-- (github.com/httpwg/structured-field-tests): every type of RFC 9651,
-- valid and invalid, parsed and serialized. Its path comes from
-- SF_TESTS, which the flake sets.
module StructuredFieldTests (structuredFieldTests) where

import Control.Monad (forM)
import Data.Aeson (Value(..), eitherDecodeFileStrict)
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (toList)
import Data.List (isSuffixOf, sort)
import Data.Scientific (Scientific, base10Exponent, coefficient)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hyperdrive.Semantics.StructuredField
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.QuickCheck hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- | A test of the corpus, as its README describes them.
data Case = Case
  { caseName      :: String
  , caseRaw       :: Maybe [ByteString]
  , caseType      :: String
  , caseExpected  :: Maybe Value
  , caseMustFail  :: Bool
  , caseCanFail   :: Bool
  , caseCanonical :: Maybe [ByteString]
  }

-- A parsed value of any of the three types.
data Value3 = VList List | VDictionary Dictionary | VItem Item
  deriving (Eq, Show)

toCase :: Value -> Either String Case
toCase (Object o) = do
  name <- field "name" >>= text
  raw <- traverse lines' (KM.lookup "raw" o)
  typ <- field "header_type" >>= text
  canonical <- traverse lines' (KM.lookup "canonical" o)
  pure Case
    { caseName = T.unpack name, caseRaw = raw, caseType = T.unpack typ
    , caseExpected = KM.lookup "expected" o
    , caseMustFail = KM.lookup "must_fail" o == Just (Bool True)
    , caseCanFail = KM.lookup "can_fail" o == Just (Bool True)
    , caseCanonical = canonical }
  where
    field k = maybe (Left ("no " ++ show k)) Right (KM.lookup k o)
    lines' (Array a) = mapM (fmap utf8 . text) (toList a)
    lines' v = Left ("not lines: " ++ show v)
toCase v = Left ("not a test: " ++ show v)

text :: Value -> Either String T.Text
text (String t) = Right t
text v = Left ("not a string: " ++ show v)

utf8 :: T.Text -> ByteString
utf8 = T.encodeUtf8

-- | The corpus's JSON form of a value of the given type.
fromJSON3 :: String -> Value -> Either String Value3
fromJSON3 "list" (Array a) = VList <$> mapM member (toList a)
fromJSON3 "dictionary" (Array a) = VDictionary <$> mapM pair (toList a)
  where
    pair (Array p) | [String k, m] <- toList p = (,) (utf8 k) <$> member m
    pair v = Left ("not a dictionary member: " ++ show v)
fromJSON3 "item" v = VItem <$> item v
fromJSON3 t v = Left ("no " ++ t ++ ": " ++ show v)

member :: Value -> Either String Member
member (Array a) | [Array items, params] <- toList a =
  InnerList <$> mapM item (toList items) <*> parameters params
member v = MemberItem <$> item v

item :: Value -> Either String Item
item (Array a) | [b, params] <- toList a = Item <$> bare b <*> parameters params
item v = Left ("not an item: " ++ show v)

parameters :: Value -> Either String Parameters
parameters (Array a) = mapM pair (toList a)
  where
    pair (Array p) | [String k, b] <- toList p = (,) (utf8 k) <$> bare b
    pair v = Left ("not a parameter: " ++ show v)
parameters v = Left ("not parameters: " ++ show v)

bare :: Value -> Either String BareItem
bare (Number n) = Right (number n)
bare (String s) = Right (SFString (utf8 s))
bare (Bool b) = Right (SFBoolean b)
bare (Object o) = case (KM.lookup "__type" o, KM.lookup "value" o) of
  (Just (String "token"), Just (String t)) -> Right (SFToken (utf8 t))
  (Just (String "binary"), Just (String b)) -> SFByteSequence <$> base32Decode (utf8 b)
  (Just (String "date"), Just (Number n)) | SFInteger i <- number n -> Right (SFDate i)
  (Just (String "displaystring"), Just (String s)) -> Right (SFDisplayString (utf8 s))
  _ -> Left ("not a bare item: " ++ show o)
bare v = Left ("not a bare item: " ++ show v)

-- "a Decimal is written as a JSON number, and its value is the number's
-- decimal digits as written" (the README): written with a point, its
-- exponent is negative
number :: Scientific -> BareItem
number n
  | base10Exponent n < 0 = SFDecimal (toRational n)
  | otherwise = SFInteger (coefficient n * 10 ^ base10Exponent n)

-- | Base32 (RFC 4648, Section 6), as the corpus writes byte sequences.
base32Decode :: ByteString -> Either String ByteString
base32Decode input = do
  values <- mapM value (B.unpack (B.takeWhile (/= 0x3D) input))
  pure (B.pack (bytes (concatMap bits values)))
  where
    value c
      | c >= 0x41 && c <= 0x5A = Right (c - 0x41)
      | c >= 0x32 && c <= 0x37 = Right (c - 0x32 + 26)
      | otherwise = Left ("not base32: " ++ show input)
    bits v = [ v `shiftR` i .&. 1 | i <- [4, 3, 2, 1, 0] ]
    bytes bs = case splitAt 8 bs of
      (byte, rest) | length byte == 8 -> foldl (\acc b -> acc `shiftL` 1 .|. b) 0 byte : bytes rest
      _ -> []

parse3 :: String -> ByteString -> Maybe Value3
parse3 "list" = fmap VList . parseList
parse3 "dictionary" = fmap VDictionary . parseDictionary
parse3 _ = fmap VItem . parseItem

serialize3 :: Value3 -> Maybe ByteString
serialize3 (VList l) = serializeList l
serialize3 (VDictionary d) = serializeDictionary d
serialize3 (VItem i) = serializeItem i

loadCorpus :: IO [(FilePath, [Case])]
loadCorpus = do
  root <- lookupEnv "SF_TESTS"
    >>= maybe (fail "SF_TESTS is not set: run the tests in `nix develop`") pure
  let dirs = [root, root </> "serialisation-tests"]
  fmap concat $ forM dirs $ \dir -> do
    files <- sort . filter (".json" `isSuffixOf`) <$> listDirectory dir
    forM files $ \f -> do
      json <- eitherDecodeFileStrict (dir </> f) >>= either fail pure
      cases <- case json of
        Array a -> either fail pure (mapM toCase (toList a))
        _ -> fail (f ++ " is not a list of tests")
      pure (if dir == root then f else "serialisation-tests" </> f, cases)

structuredFieldTests :: IO TestTree
structuredFieldTests = do
  files <- loadCorpus
  pure $ testGroup "Structured fields (RFC 9651)"
    [ testGroup "The structured-field-tests corpus"
        [ testProperty f $ once $
            counterexample "no tests" (not (null cases))
            .&&. conjoin (map runCase cases)
        | (f, cases) <- files ]
    , testProperty "what the corpus does not test" $ once beyondCorpus
    , testProperty "what serializes parses back to the same value" prop_roundTrip
    ]

-- = https://www.rfc-editor.org/rfc/rfc9651#section-1.2
-- # When parsing from HTTP fields, implementations MUST have behavior
-- # that is indistinguishable from following the algorithms.
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-4.2
-- # When generating input_bytes, parsers MUST combine all field lines in
-- # the same section (header or trailer) that case-insensitively match
-- # the field name into one comma-separated field-value, as per
-- # Section 5.2 of [HTTP]; this assures that the entire field value is
-- # processed correctly.
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-3.3.3
-- # Furthermore, only DQUOTE and "\" can be escaped; other characters
-- # after "\" MUST cause parsing to fail.
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-3.1.2
-- # Parameters whose value is Boolean (see Section 3.3.6) true MUST omit
-- # that value when serialized.
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-3.2
-- # Members whose value is Boolean (see Section 3.3.6) true MUST omit
-- # that value when serialized.
runCase :: Case -> Property
runCase c = counterexample (caseName c) $ case caseRaw c of
  Just raw -> parsing (combineFieldLines raw)
  Nothing -> serializing
  where
    typ = caseType c
    expected = traverse (fromJSON3 typ) (caseExpected c)
    -- = https://www.rfc-editor.org/rfc/rfc9651#section-4.1
    -- # If the structure is a Dictionary or List and its value is empty
    -- # (i.e., it has no members), do not serialize the field at all
    -- # (i.e., omit both the field-name and field-value).
    --
    -- The corpus's canonical form of one is no lines: the empty string.
    canonical = combineFieldLines <$> caseCanonical c
    parsing input = case (parse3 typ input, expected) of
      (_, Left err) -> counterexample err False
      (Nothing, _) -> counterexample "did not parse" (caseMustFail c || caseCanFail c)
      (Just v, Right e)
        | caseMustFail c -> counterexample ("parsed as " ++ show v) False
        | otherwise ->
            maybe (property True) (v ===) e
            -- what parses serializes to the canonical form, or to the
            -- input if that is canonical
            .&&. serialize3 v === Just (maybe input id canonical)
    serializing = case expected of
      Left err -> counterexample err (caseMustFail c)
      Right Nothing -> counterexample "no expected value" False
      Right (Just v)
        | caseMustFail c -> serialize3 v === Nothing
        | otherwise -> serialize3 v === canonical


-- Byte sequences that are not base64 at any padding, and display
-- strings that are not UTF-8 in ways the corpus does not try.
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-4.2.7
-- # This specification does not relax the requirements in Sections 3.1
-- # and 3.3 of [RFC4648]; therefore, parsers MUST fail on characters
-- # outside the base64 alphabet and on line feeds in encoded data.
beyondCorpus :: Property
beyondCorpus = conjoin
  [ counterexample (show input) (parseItem input === Nothing)
  | input <-
      [ ":a:", ":aGVsb:", ":aGVsbG8===:", ":aGV=sbG8:"
      , "%\"%ed%a0%80\"", "%\"%c0%80\"", "%\"%f4%90%80%80\"", "%\"%80\""
      , "%\"%e2%82\"" ] ]
  .&&. parseItem ":aGVsbG8:" === Just (Item (SFByteSequence "hello") [])
  .&&. parseItem "%\"%f4%8f%bf%bf\"" === Just (Item (SFDisplayString "\xf4\x8f\xbf\xbf") [])
  .&&. serializeItem (Item (SFDisplayString "\xed\xa0\x80") []) === Nothing

prop_roundTrip :: Property
prop_roundTrip = forAll (oneof [VList <$> genList, VDictionary <$> genDictionary, VItem <$> genItem]) $ \v ->
  case serialize3 v of
    Nothing -> counterexample "did not serialize" False
    Just bytes -> counterexample (show bytes) $
      parse3 (kind v) bytes === Just v
  where
    kind (VList _) = "list"
    kind (VDictionary _) = "dictionary"
    kind (VItem _) = "item"

genList :: Gen List
genList = scale (`div` 4) (listOf genMember)

genDictionary :: Gen Dictionary
genDictionary = scale (`div` 4) (genKeyed genMember)

genMember :: Gen Member
genMember = oneof
  [ MemberItem <$> genItem
  , InnerList <$> scale (`div` 2) (listOf genItem) <*> genParameters ]

genItem :: Gen Item
genItem = Item <$> genBare <*> genParameters

genParameters :: Gen Parameters
genParameters = scale (`div` 3) (genKeyed genBare)

-- keys each once, as parsing leaves them
genKeyed :: Gen a -> Gen [(ByteString, a)]
genKeyed g = do
  pairs <- listOf ((,) <$> genKey <*> g)
  pure (foldr (\(k, a) acc -> (k, a) : filter ((/= k) . fst) acc) [] pairs)

genKey :: Gen ByteString
genKey = B.pack <$> ((:) <$> elements (0x2A : lc) <*> listOf (elements (lc ++ digits ++ B.unpack "_-.*")))
  where
    lc = [0x61 .. 0x7A]
    digits = [0x30 .. 0x39]

genBare :: Gen BareItem
genBare = oneof
  [ SFInteger <$> choose (-999999999999999, 999999999999999)
  , SFDecimal . (/ 1000) . fromInteger <$> choose (-999999999999999, 999999999999999)
  , SFString . B.pack <$> listOf (choose (0x20, 0x7E))
  , SFToken . B.pack <$> ((:) <$> elements (0x2A : alpha) <*> listOf (elements tokenChars))
  , SFByteSequence . B.pack <$> listOf arbitrary
  , SFBoolean <$> arbitrary
  , SFDate <$> choose (-62135596800, 253402214400)
  , SFDisplayString . utf8 . T.pack <$> listOf (arbitrary `suchThat` (\ch -> ch < '\xD800' || ch > '\xDFFF'))
  ]
  where
    alpha = [0x41 .. 0x5A] ++ [0x61 .. 0x7A]
    tokenChars = alpha ++ [0x30 .. 0x39] ++ B.unpack "!#$%&'*+-.^_`|~:/"
