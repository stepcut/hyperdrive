-- | QuickCheck generators for ABNF syntax trees. Every tree they
-- generate satisfies the invariants documented in
-- "Hyperdrive.ABNF.Types".
module Generators
  ( genRuleList
  , genRule
  , genRuleName
  , genAlternation
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Hyperdrive.ABNF.Types
import Numeric.Natural (Natural)
import Test.QuickCheck

genRuleList :: Gen RuleList
genRuleList = listOf1 genRule

genRule :: Gen Rule
genRule = Rule <$> genRuleName <*> elements [minBound .. maxBound] <*> genAlternation

genRuleName :: Gen RuleName
genRuleName = do
  c  <- elements alpha
  cs <- resize 8 (listOf (elements (alpha ++ ['0' .. '9'] ++ "-")))
  pure (fromJust (mkRuleName (C.pack (c : cs))))
  where alpha = ['a' .. 'z'] ++ ['A' .. 'Z']

genAlternation :: Gen Alternation
genAlternation = sized $ \n -> alternationOf (min n 12)

alternationOf :: Int -> Gen Alternation
alternationOf n = Alternation <$> nonEmpty n (concatenationOf n)

concatenationOf :: Int -> Gen Concatenation
concatenationOf n = Concatenation <$> nonEmpty n (repetitionOf n)

repetitionOf :: Int -> Gen Repetition
repetitionOf n = Repetition <$> oneof [pure Nothing, Just <$> genRepeat] <*> elementOf n

genRepeat :: Gen Repeat
genRepeat = do
  kind <- elements [minBound .. maxBound]
  lo   <- genCount
  hi   <- oneof [pure Nothing, Just <$> genCount]
  pure (Repeat kind lo hi)
  where genCount = fromInteger . getNonNegative <$> arbitrary

elementOf :: Int -> Gen Element
elementOf n = frequency $
  [ (4, RuleRef <$> genRuleName)
  , (2, CharVal <$> elements [minBound .. maxBound] <*> bytesIn charValBytes)
  , (2, NumVal <$> elements [minBound .. maxBound] <*> genNumValBody)
  , (1, ProseVal <$> bytesIn proseValBytes)
  ] ++
  [ (k, c <$> alternationOf (n `div` 3)) | n > 0, (k, c) <- [(1, Group), (1, Option)] ]
  where
    charValBytes  = [0x20 .. 0x21] ++ [0x23 .. 0x7E]
    proseValBytes = [0x20 .. 0x3D] ++ [0x3F .. 0x7E]
    bytesIn ws = B.pack <$> resize 6 (listOf (elements ws))

genNumValBody :: Gen NumValBody
genNumValBody = oneof
  [ Values <$> nonEmpty 4 genValue
  , Range <$> genValue <*> genValue
  ]
  where
    genValue :: Gen Natural
    genValue = oneof
      [ fromInteger . getNonNegative <$> arbitrary
      , fromInteger <$> choose (0, 0x10FFFF)
      , fromInteger <$> choose (0, 2 ^ (80 :: Int))
      ]

nonEmpty :: Int -> Gen a -> Gen (NonEmpty a)
nonEmpty n g = do
  k <- choose (0, min 3 n)
  (:|) <$> g <*> vectorOf k g
