-- | Data read from the text of RFC 7541 itself (test/data/rfc7541.txt),
-- so that the tests compare the code with the published text rather
-- than with a copy typed by hand.
module RFC7541
  ( rfc7541
  , HuffmanRow(..)
  , huffmanTable
  , staticTableRows
  , Example(..)
  , appendixC
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Data.Word (Word8)
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
    symbolNumber str = case str of
      [] -> Nothing
      ('(' : rest) | (inside, ')' : after) <- break (== ')') rest
                   , all (\c -> isDigit c || c == ' ') inside, any isDigit inside
                   -> Just (inside, after)
      (_ : rest) -> symbolNumber rest

-- | The rows of Appendix A's table: index, name, value.
staticTableRows :: String -> [(Int, String, String)]
staticTableRows text =
  [ (read (trim ix), trim name, trim value)
  | l <- appendixA
  , ('|' : rest) <- [dropWhile isSpace l]
  , [ix, name, value, ""] <- [splitOn '|' rest]
  , all isDigit (trim ix), not (null (trim ix)) ]
  where
    appendixA = takeWhile (not . ("Appendix B." `isPrefixOf`))
              $ dropWhile (not . ("Appendix A." `isPrefixOf`)) (lines text)

-- | One example of Appendix C.2 to C.6: the encoded block, the field
-- list decoded from it, and the dynamic table after it (each entry's
-- name and value, with the size the RFC gives it, and the table's
-- size).
data Example = Example
  { exampleName      :: String
  , exampleEncoded   :: [Word8]
  , exampleFields    :: [(String, String)]
  , exampleTable     :: [(Int, (String, String))]
  , exampleTableSize :: Int
  } deriving (Show)

-- | The examples of Appendix C.2 to C.6 in groups that share a decoder,
-- each with its maximum table size: each of C.2's examples alone, then
-- the three of each of C.3, C.4 (4,096 octets), C.5 and C.6 (256, as
-- their text says).
appendixC :: String -> [(Int, [Example])]
appendixC text =
     [ (4096, [e]) | e <- inGroup "C.2." ]
  ++ [ (4096, inGroup "C.3."), (4096, inGroup "C.4.")
     , (256, inGroup "C.5."), (256, inGroup "C.6.") ]
  where
    body = filter (not . pageBreak)
         $ takeWhile (not . ("Acknowledgments" `isPrefixOf`))
         $ dropWhile (not . ("C.2.  " `isPrefixOf`)) (lines text)
    pageBreak l = "Peon & Ruellan" `isPrefixOf` l || "RFC 7541 " `isPrefixOf` l || "\f" `isPrefixOf` l
    sections = splitSections body
    inGroup g = [example h ls | (h, ls) <- sections, g `isPrefixOf` h]

-- the sections with numbers of three parts ("C.2.1."), with their lines
splitSections :: [String] -> [(String, [String])]
splitSections [] = []
splitSections (l : ls)
  | isExampleHeading l = let (body, rest) = break isHeading ls in (l, body) : splitSections rest
  | otherwise = splitSections ls
  where
    isHeading h = "C." `isPrefixOf` h
    isExampleHeading h = case words h of
      (n : _) -> "C." `isPrefixOf` n && length (filter (== '.') n) == 3
      []      -> False

example :: String -> [String] -> Example
example heading ls = Example
  { exampleName = trim heading
  , exampleEncoded = concatMap octets (between "Hex dump of encoded data:" "Decoding process:")
  , exampleFields = map field (filter (not . all isSpace) (after "Decoded header list:"))
  , exampleTable = entries (tableLines (between' "Dynamic" "Decoded header list:"))
  , exampleTableSize = case [l | l <- ls, "Table size:" `isPrefixOf` trim l] of
      (l : _) -> read (filter isDigit l)
      []      -> 0
  }
  where
    between a b = takeWhile (not . (b `isInfixOf`)) (after a)
    between' a b = takeWhile (not . (b `isInfixOf`)) (dropWhile (not . (a `isPrefixOf`) . trim) ls)
    after a = drop 1 (dropWhile (not . (a `isInfixOf`)) ls)
    octets l = case words (takeWhile (/= '|') l) of
      ws -> concatMap hexPairs ws
    hexPairs (a : b : rest) | [(o, "")] <- readHex [a, b] = o : hexPairs rest
    hexPairs _ = []
    -- an entry's line starts with "[ n]"; a line that continues it is
    -- joined with a space (the RFC wraps them at spaces)
    tableLines = filter (\l -> not (all isSpace l) && not ("Table size:" `isPrefixOf` trim l)
                               && not ("Dynamic" `isPrefixOf` trim l))
    entries [] = []
    entries (l : rest) =
      let (cont, rest') = break (("[" `isPrefixOf`) . trim) rest
          full = unwords (trim l : map trim cont)
          sizeText = takeWhile (/= ')') (drop 1 (dropWhile (/= '=') full))
          fieldText = drop 2 (dropWhile (/= ')') full)
      in (read (trim sizeText), field fieldText) : entries rest'

-- "name: value", where a pseudo-header's name starts with ':'
field :: String -> (String, String)
field l =
  let t = trim l
      (lead, rest) = splitAt (if take 1 t == ":" then 1 else 0) t
      (name, value) = break (== ':') rest
  in (lead ++ name, trim (drop 1 value))

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, [])       -> [a]
  (a, _ : rest) -> a : splitOn c rest

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
