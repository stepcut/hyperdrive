{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower, toUpper)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Hyperdrive.ABNF.Types
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import ParserTests (parserTests)
import QQTests (qqTests)
import RFCTests (rfcTests)

main :: IO ()
main = do
 parser <- parserTests
 qq <- qqTests
 rfc <- rfcTests
 defaultMain $ testGroup "hyperdrive-abnf"
  [ testGroup "RuleName"
    [ testProperty "mkRuleName accepts exactly rulename" prop_mkRuleName
    , testProperty "examples"                            prop_ruleNameExamples
    , testProperty "names are case-insensitive"          prop_ruleNameCase
    , testProperty "the spelling is kept"                prop_ruleNameSpelling
    ]
  , parser
  , qq
  , rfc
  ]

------------------------------------------------------------------------------
-- generators
------------------------------------------------------------------------------

-- | bytes, weighted towards the ones allowed in rule names
genByte :: Gen Word8
genByte = frequency
  [ (6, elements (B.unpack "aZq09-"))
  , (1, elements (B.unpack "_ .@`[{/:"))
  , (1, arbitrary)
  ]

newtype Bytes = Bytes ByteString deriving Show

instance Arbitrary Bytes where
  arbitrary = Bytes . B.pack <$> listOf genByte
  shrink (Bytes bs) = Bytes . B.pack <$> shrink (B.unpack bs)

newtype Name = Name ByteString deriving Show

instance Arbitrary Name where
  arbitrary = do
    c  <- elements alpha
    cs <- listOf (elements (alpha ++ ['0' .. '9'] ++ "-"))
    pure (Name (C.pack (c : cs)))
    where alpha = ['a' .. 'z'] ++ ['A' .. 'Z']

------------------------------------------------------------------------------
-- RuleName
------------------------------------------------------------------------------

-- | an independent check of the @rulename@ grammar, by byte value
isRuleName :: ByteString -> Bool
isRuleName bs = case B.unpack bs of
  (w : ws) -> alpha w && all (\x -> alpha x || digit x || x == 0x2D) ws
  []       -> False
  where
    alpha w = (w >= 0x41 && w <= 0x5A) || (w >= 0x61 && w <= 0x7A)
    digit w = w >= 0x30 && w <= 0x39

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # rulename       =  ALPHA *(ALPHA / DIGIT / "-")
prop_mkRuleName :: Bytes -> Property
prop_mkRuleName (Bytes bs) = isJust (mkRuleName bs) === isRuleName bs

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.1
-- # The name of a rule is simply the name itself, that is, a sequence of
-- # characters, beginning with an alphabetic character, and followed by a
-- # combination of alphabetics, digits, and hyphens (dashes).
prop_ruleNameExamples :: Property
prop_ruleNameExamples = once $ conjoin
  [ counterexample (show bs) (isJust (mkRuleName bs) === ok)
  | (bs, ok) <- [ ("a", True), ("Z", True), ("field-name", True)
                , ("IPv6address", True), ("h16", True), ("a-", True)
                , ("", False), ("1a", False), ("-a", False), ("a_b", False)
                , ("a b", False), ("<a>", False), ("a\0", False)
                , ("\xC3\xA9", False) ] ]

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.1
-- # The names <rulename>, <Rulename>, <RULENAME>, and <rUlENamE> all
-- # refer to the same rule.
prop_ruleNameCase :: Name -> Name -> Property
prop_ruleNameCase (Name a) (Name b) =
  conjoin
    [ mkRuleName (C.map toUpper a) === mkRuleName (C.map toLower a)
    , (mkRuleName a == mkRuleName b) === (C.map toLower a == C.map toLower b)
    , compare (mkRuleName a) (mkRuleName b)
        === compare (C.map toLower a) (C.map toLower b)
    ]

prop_ruleNameSpelling :: Name -> Property
prop_ruleNameSpelling (Name a) = fmap ruleNameBytes (mkRuleName a) === Just a
