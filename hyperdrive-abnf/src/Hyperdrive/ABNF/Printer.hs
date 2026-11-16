{-# LANGUAGE OverloadedStrings #-}
-- | Print ABNF in the syntax 'Hyperdrive.ABNF.Parser' reads, so that
-- parsing what is printed gives back the same grammar.
--
-- Each rule is printed on one line ending in CRLF. Hexadecimal
-- values are printed in upper case with at least two digits, as the
-- RFCs write them.
module Hyperdrive.ABNF.Printer
  ( printRuleList
  , printRule
  , printAlternation
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as L
import Data.Char (intToDigit, toUpper)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Hyperdrive.ABNF.Types
import Numeric (showIntAtBase)
import Numeric.Natural (Natural)

printRuleList :: RuleList -> ByteString
printRuleList = build . foldMap rule

printRule :: Rule -> ByteString
printRule = build . rule

printAlternation :: Alternation -> ByteString
printAlternation = build . alternation

build :: BB.Builder -> ByteString
build = L.toStrict . BB.toLazyByteString

rule :: Rule -> BB.Builder
rule (Rule name definedAs alt) =
  BB.byteString (ruleNameBytes name)
    <> (case definedAs of Basic -> " = "; Incremental -> " =/ ")
    <> alternation alt <> "\r\n"

alternation :: Alternation -> BB.Builder
alternation (Alternation cs) = sepBy " / " (concatenation <$> cs)

concatenation :: Concatenation -> BB.Builder
concatenation (Concatenation rs) = sepBy " " (repetition <$> rs)

repetition :: Repetition -> BB.Builder
repetition (Repetition r e) = maybe mempty repeat_ r <> element e

repeat_ :: Repeat -> BB.Builder
repeat_ (Repeat Star n (Just m)) | n == m = BB.string7 (show n)
repeat_ (Repeat kind n m) =
  (if n == 0 then mempty else BB.string7 (show n))
    <> (case kind of Star -> "*"; Hash -> "#")
    <> maybe mempty (BB.string7 . show) m

element :: Element -> BB.Builder
element e = case e of
  RuleRef name              -> BB.byteString (ruleNameBytes name)
  Group alt                 -> "(" <> alternation alt <> ")"
  Option alt                -> "[" <> alternation alt <> "]"
  CharVal CaseInsensitive s -> quoted s
  CharVal CaseSensitive s   -> "%s" <> quoted s
  NumVal base body          -> "%" <> numVal base body
  ProseVal s                -> "<" <> BB.byteString s <> ">"
  where quoted s = "\"" <> BB.byteString s <> "\""

numVal :: Base -> NumValBody -> BB.Builder
numVal base body = BB.char7 letter <> case body of
  Values vs   -> sepBy "." (digits <$> vs)
  Range lo hi -> digits lo <> "-" <> digits hi
  where
    (letter, digits) = case base of
      Binary      -> ('b', inBase 2 1)
      Decimal     -> ('d', inBase 10 1)
      Hexadecimal -> ('x', inBase 16 2)

-- | digits in a base, padded with zeros to a minimum width
inBase :: Natural -> Int -> Natural -> BB.Builder
inBase radix width n =
  let ds = map toUpper (showIntAtBase radix intToDigit n "")
  in BB.string7 (replicate (width - length ds) '0' ++ ds)

sepBy :: BB.Builder -> NonEmpty BB.Builder -> BB.Builder
sepBy s = mconcat . intersperse s . NE.toList
