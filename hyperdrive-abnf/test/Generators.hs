{-# LANGUAGE OverloadedStrings #-}
-- | QuickCheck generators for ABNF syntax trees. Every tree they
-- generate satisfies the invariants documented in
-- "Hyperdrive.ABNF.Types".
module Generators
  ( genRuleList
  , genRule
  , genRuleName
  , genAlternation
  , genGrammar
  , shortInputs
  ) where

import Data.ByteString (ByteString)
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

-- | Random grammars whose rules only refer to rules defined before
-- them, over a small alphabet.
genGrammar :: Gen [Rule]
genGrammar = do
  n <- choose (1, 4)
  mapM (\i -> Rule (ruleN i) Basic <$> genAlt i (3 :: Int)) [0 .. n - 1]
  where
    ruleN i = fromJust (mkRuleName (C.pack ("r" ++ show (i :: Int))))
    genAlt i d = Alternation <$> some' (Concatenation <$> some' (genRep i d))
    some' g = do { k <- choose (0, 2); (:|) <$> g <*> vectorOf k g }
    genRep i d = do
      e <- genEl i d
      frequency
        [ (4, pure (Repetition Nothing e))
        , (2, do lo <- elements [0, 1, 2]; hi <- elements [Nothing, Just 0, Just 1, Just 2, Just 3]
                 pure (Repetition (Just (Repeat Star lo hi)) e))
        , (2, do lo <- elements [0, 1, 2]; hi <- elements [Nothing, Just 2, Just 3]
                 pure (Repetition (Just (Repeat Hash lo (max lo <$> hi))) e)) ]
    genEl i d = frequency $
      [ (3, CharVal <$> elements [CaseSensitive, CaseInsensitive] <*> elements ["a", "b", "ab", "A", ""])
      , (2, NumVal Hexadecimal <$> elements [Values (0x61 :| []), Values (0x61 :| [0x62]), Range 0x61 0x62, Range 0x20 0x2C])
      ] ++
      [ (2, RuleRef . ruleN <$> choose (0, i - 1)) | i > 0 ] ++
      [ (1, Group <$> genAlt i (d - 1)) | d > 0 ] ++
      [ (1, Option <$> genAlt i (d - 1)) | d > 0 ]

-- | every string of up to four characters over the alphabet the
-- random grammars use
shortInputs :: [ByteString]
shortInputs = concatMap strings [0 .. 4]
  where strings k = map C.pack (sequence (replicate k "aAb, "))

