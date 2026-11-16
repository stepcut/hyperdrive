{-# LANGUAGE TemplateHaskell #-}
-- | A quasiquoter for ABNF, so a grammar can be copied from an RFC
-- into Haskell source as it is written:
--
-- > example :: RuleList
-- > example = [abnf|
-- >   example-list      = 1#example-list-elmt
-- >   example-list-elmt = token ; see Section 5.6.2
-- >   |]
--
-- The indentation shared by the lines that are not blank is removed,
-- and line endings become CRLF, so the rules may be indented to suit
-- the surrounding code. The grammar is parsed at compile time, and a
-- syntax error is a compile error at its line and column.
module Hyperdrive.ABNF.QQ
  ( abnf
  , parseQuoted
  ) where

import qualified Data.ByteString.Char8 as C
import Data.Char (isAscii)
import Hyperdrive.ABNF.Parser (ParseError(..), parseRuleList)
import Hyperdrive.ABNF.RFC (toCRLF)
import Hyperdrive.ABNF.Types (RuleList)
import Language.Haskell.TH (Loc(..), location, sigE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)

-- | A 'RuleList' expression.
abnf :: QuasiQuoter
abnf = QuasiQuoter
  { quoteExp  = \src -> case parseQuoted src of
      Right rules -> sigE (lift rules) [t| RuleList |]
      Left err    -> do
        loc <- location
        fail (message loc err)
  , quotePat  = unsupported "patterns"
  , quoteType = unsupported "types"
  , quoteDec  = unsupported "declarations"
  }
  where
    unsupported what _ = fail ("abnf: the quasiquoter cannot be used for " ++ what)
    message loc (ParseError line column expected) =
      let (startLine, startColumn) = loc_start loc
          -- the location is where the quoted text starts
          column' | line == 1 = startColumn + column - 1
                  | otherwise = column
      in concat
           [ "abnf: syntax error at ", loc_filename loc, ":"
           , show (startLine + line - 1), ":", show column'
           , case expected of
               [] -> ""
               _  -> "; expected " ++ commaOr expected ]
    commaOr [x]    = x
    commaOr [x, y] = x ++ " or " ++ y
    commaOr (x:xs) = x ++ ", " ++ commaOr xs
    commaOr []     = ""

-- | Parse the text of a quotation as 'abnf' does. Lines and columns
-- in errors count in the quoted text, before indentation is removed.
-- A quotation with nothing but whitespace in it is an empty rule list
-- (which RFC 5234's @rulelist@ cannot express).
parseQuoted :: String -> Either ParseError RuleList
parseQuoted src =
  case [ (l, c) | (l, line) <- zip [1 ..] ls, (c, ch) <- zip [1 ..] line, not (isAscii ch) ] of
    (l, c) : _ -> Left (ParseError l c ["ASCII"])
    [] -> case [ length (takeWhile (== ' ') l) | l <- ls, not (all (== ' ') l) ] of
      [] -> Right []
      indents ->
        let indent = minimum indents
        in case parseRuleList (toCRLF (map (C.pack . drop indent) ls)) of
             Right rules -> Right rules
             Left (ParseError l c expected) -> Left (ParseError l (c + indent) expected)
  where
    ls = map (\l -> if not (null l) && last l == '\r' then init l else l) (lines src)
