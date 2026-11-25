-- | Data read from the text of RFC 7541 itself (test/data/rfc7541.txt),
-- so that the tests compare the code with the published text rather
-- than with a copy typed by hand.
module RFC7541
  ( rfc7541
  , HuffmanRow(..)
  , huffmanTable
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)
import Numeric (readHex)

rfc7541 :: IO String
rfc7541 = readFile "test/data/rfc7541.txt"

-- | One row of Appendix B: the symbol, its code as bits (MSB first) and
-- as hex (aligned on the LSB), and its length.
data HuffmanRow = HuffmanRow
  { rowSymbol :: Int
  , rowBits   :: String
  , rowHex    :: Integer
  , rowLength :: Int
  } deriving (Show)

-- | The rows of Appendix B, in order. Rows look like
--
-- >     ' ' ( 32)  |010100                                        14  [ 6]
-- >   EOS (256)  |11111111|11111111|11111111|111111      3fffffff  [30]
huffmanTable :: String -> [HuffmanRow]
huffmanTable text = [r | l <- appendixB, Just r <- [row l]]
  where
    appendixB = takeWhile (not . ("Appendix C." `isPrefixOf`))
              $ dropWhile (not . ("Appendix B." `isPrefixOf`)) (lines text)
    -- the symbol's number is in the first parentheses holding only
    -- digits and spaces (the ASCII form before it can be '(', ')' or '|')
    row l = case symbolNumber l of
      Just (sym, rest') ->
        case words rest' of
          (bits@('|' : _) : hex : len) | [(h, "")] <- readHex hex ->
            Just HuffmanRow
              { rowSymbol = read (trim sym)
              , rowBits = filter (/= '|') bits
              , rowHex = h
              , rowLength = read (filter isDigit (concat len)) }
          _ -> Nothing
      Nothing -> Nothing
    symbolNumber s = case s of
      [] -> Nothing
      ('(' : rest) | (inside, ')' : after) <- break (== ')') rest
                   , all (\c -> isDigit c || c == ' ') inside, any isDigit inside
                   -> Just (inside, after)
      (_ : rest) -> symbolNumber rest
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
