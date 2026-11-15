{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower, toUpper)
import Data.Maybe (isJust, isNothing)
import Data.Word (Word8)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Method
import Hyperdrive.Semantics.Status
import Hyperdrive.Semantics.Syntax
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import ComplexityTests (complexityTests)
import ExhaustiveTests (exhaustiveTests)
import SafePathTests (safePathTests)
import URITests (uriTests)

main :: IO ()
main = defaultMain $ testGroup "hyperdrive-semantics"
  [ testGroup "Syntax"
    [ testProperty "tchar is VCHAR except delimiters" prop_tcharDelimiters
    , testProperty "isToken"                          prop_isToken
    ]
  , testGroup "Method"
    [ testProperty "mkMethod accepts exactly tokens"  prop_mkMethodToken
    , testProperty "methods are case-sensitive"       prop_methodCaseSensitive
    , testProperty "safe and idempotent methods"      prop_safeIdempotent
    ]
  , testGroup "Field"
    [ testProperty "field names are case-insensitive" prop_fieldNameCaseInsensitive
    , testProperty "mkFieldName accepts exactly tokens" prop_mkFieldNameToken
    , testProperty "OWS is stripped from values"      prop_valueStripsOWS
    , testProperty "CR, LF and NUL are rejected"      prop_valueRejectsCRLFNUL
    , testProperty "accepted values match field-value" prop_valueGrammar
    , testProperty "combined values match field-value" prop_combineGrammar
    , testProperty "values are combined with comma SP" prop_combineCommaSP
    , testProperty "field line order is preserved"    prop_fieldOrder
    ]
  , testGroup "Status"
    [ testProperty "only 100..599 are valid"          prop_statusRange
    , testProperty "class is the first digit"         prop_statusClass
    ]
  , uriTests
  , complexityTests
  , exhaustiveTests
  , safePathTests
  ]

------------------------------------------------------------------------------
-- generators
------------------------------------------------------------------------------

-- | bytes, weighted towards the ones that matter to HTTP syntax
genByte :: Gen Word8
genByte = frequency
  [ (6, elements (B.unpack "abcXYZ019!#$%&'*+-.^_`|~"))
  , (2, elements [0x20, 0x09])
  , (1, elements (B.unpack "\"(),/:;<=>?@[\\]{}"))
  , (1, elements [0x00, 0x0A, 0x0D, 0x01, 0x7F, 0x80, 0xFF])
  , (1, arbitrary)
  ]

newtype Bytes = Bytes ByteString deriving Show

instance Arbitrary Bytes where
  arbitrary = Bytes . B.pack <$> listOf genByte
  shrink (Bytes bs) = Bytes . B.pack <$> shrink (B.unpack bs)

newtype Token = Token ByteString deriving Show

instance Arbitrary Token where
  arbitrary = Token . B.pack <$> listOf1 (elements tchars)
    where tchars = filter isTChar [minBound .. maxBound]

------------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.2
-- # ; any VCHAR, except delimiters

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.2
-- # Delimiters are chosen from the set of US-ASCII visual characters not
-- # allowed in a token (DQUOTE and "(),/:;<=>?@[\]{}").
prop_tcharDelimiters :: Property
prop_tcharDelimiters =
  conjoin [ counterexample (show w) (isTChar w === (isVChar w && not (B.elem w delimiters)))
          | w <- [minBound .. maxBound] ]
  where delimiters = "\"(),/:;<=>?@[\\]{}"

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.2
-- # token          = 1*tchar
prop_isToken :: Bytes -> Property
prop_isToken (Bytes bs) =
  isToken bs === (B.length bs >= 1 && B.all isTChar bs)

------------------------------------------------------------------------------
-- Method
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.1
-- # method = token
prop_mkMethodToken :: Bytes -> Property
prop_mkMethodToken (Bytes bs) =
  isJust (mkMethod bs) === isToken bs

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.1
-- # The method token is case-sensitive
prop_methodCaseSensitive :: Property
prop_methodCaseSensitive =
  conjoin [ mkMethod (C.map toLower (methodBytes m)) =/= Just m
          | m <- [GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE] ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.2.1
-- # Of the request methods defined by this specification, the GET, HEAD,
-- # OPTIONS, and TRACE methods are defined to be safe.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.2.2
-- # Of the request methods
-- # defined by this specification, PUT, DELETE, and safe request methods
-- # are idempotent.
prop_safeIdempotent :: Property
prop_safeIdempotent = conjoin
  [ map isSafe       standard === [True,  True,  False, False, False, False, True, True]
  , map isIdempotent standard === [True,  True,  False, True,  True,  False, True, True]
  ]
  where standard = [GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE]

------------------------------------------------------------------------------
-- Field
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.1
-- # Field names are case-insensitive
prop_fieldNameCaseInsensitive :: Token -> Property
prop_fieldNameCaseInsensitive (Token bs) =
  mkFieldName (C.map toUpper bs) === mkFieldName (C.map toLower bs)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.1
-- # field-name     = token
prop_mkFieldNameToken :: Bytes -> Property
prop_mkFieldNameToken (Bytes bs) =
  isJust (mkFieldName bs) === isToken bs

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # a field parsing implementation MUST exclude such whitespace
-- # prior to evaluating the field value.
prop_valueStripsOWS :: Bytes -> Property
prop_valueStripsOWS (Bytes bs) =
  forAll (listOf (elements [0x20, 0x09])) $ \pre ->
  forAll (listOf (elements [0x20, 0x09])) $ \post ->
    fmap fieldValueBytes (mkFieldValue (B.pack pre <> bs <> B.pack post))
      === fmap fieldValueBytes (mkFieldValue bs)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # a recipient of CR, LF, or NUL within
-- # a field value MUST either reject the message or replace each of those
-- # characters with SP before further processing or forwarding of that
-- # message.
prop_valueRejectsCRLFNUL :: Bytes -> Bytes -> Property
prop_valueRejectsCRLFNUL (Bytes a) (Bytes b) =
  forAll (elements [0x00, 0x0A, 0x0D]) $ \bad ->
    mkFieldValue (a <> B.singleton bad <> b) === Left ContainsCRLFNUL

-- | an independent check of the @field-value@ grammar
matchesFieldValue :: ByteString -> Bool
matchesFieldValue bs =
  B.null bs ||
    (  isFieldVChar (B.head bs)
    && isFieldVChar (B.last bs)
    && B.all (\w -> isFieldVChar w || w == 0x20 || w == 0x09) bs )

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # field-value    = *field-content
-- # field-content  = field-vchar
-- # [ 1*( SP / HTAB / field-vchar ) field-vchar ]
prop_valueGrammar :: Bytes -> Property
prop_valueGrammar (Bytes bs) =
  case mkFieldValue bs of
    Right v -> property (matchesFieldValue (fieldValueBytes v))
    Left _  -> property (not (matchesFieldValue (B.dropWhile isOWS (B.dropWhileEnd isOWS bs))))

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.3
-- # For
-- # consistency, use comma SP.
prop_combineCommaSP :: Property
prop_combineCommaSP = once $ conjoin
  [ combined ["a"]           === Just "a"
  , combined ["a", "b"]      === Just "a, b"
  , combined ["a", "b, c"]   === Just "a, b, c"
  , combined ["a", ""]       === Just "a,"
  , combined ["", "a"]       === Just ", a"
  , combined []              === Nothing
  ]
  where
    combined vs = fmap fieldValueBytes (combineValues [ v | Right v <- map mkFieldValue vs ])

prop_combineGrammar :: [Bytes] -> Property
prop_combineGrammar bss =
  let vs = [ v | Bytes bs <- bss, Right v <- [mkFieldValue bs] ]
  in case combineValues vs of
       Nothing -> property (null vs)
       Just v  -> property (matchesFieldValue (fieldValueBytes v))

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.3
-- # The order in which field lines with the same name are received is
-- # therefore significant to the interpretation of the field value
prop_fieldOrder :: [Bytes] -> Property
prop_fieldOrder bss =
  let vs = [ v | Bytes bs <- bss, Right v <- [mkFieldValue bs] ]
      fs = foldl (\acc v -> appendField hHost v (appendField hConnection v acc)) emptyFields vs
  in lookupAll hHost fs === vs .&&. map snd (fieldsToList fs) === concatMap (\v -> [v, v]) vs

------------------------------------------------------------------------------
-- Status
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15
-- # All valid status codes are within the
-- # range of 100 to 599, inclusive.
prop_statusRange :: Property
prop_statusRange = once $ conjoin
  [ counterexample (show c) $
      isJust (mkStatus c) === (c >= 100 && c <= 599)
        .&&. (isNothing (mkStatus c) || fmap statusCode (mkStatus c) == Just c)
  -- every code that could plausibly be near the boundaries, and the extremes
  | c <- [-1 .. 1000] ++ [minBound, maxBound] ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15
-- # The first digit of the status code defines the class of response.
prop_statusClass :: Property
prop_statusClass = forAll (choose (100, 599)) $ \c ->
  fmap statusClass (mkStatus c) === Just (toEnum (c `div` 100 - 1))
