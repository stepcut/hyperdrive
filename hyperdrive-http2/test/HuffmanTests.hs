{-# LANGUAGE OverloadedStrings #-}
module HuffmanTests (huffmanTests) where

import Data.Bits (complement, shiftL, testBit, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (minusPtr)
import System.IO.Unsafe (unsafePerformIO)
import Hyperdrive.HPACK.Huffman
import Numeric (showHex)
import RFC7541
import Test.QuickCheck hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

huffmanTests :: [HuffmanRow] -> TestTree
huffmanTests rows = testGroup "The Huffman code (RFC 7541 Section 5.2, Appendix B)"
  [ testProperty "the code of every symbol is Appendix B's" (prop_table rows)
  , testProperty "examples from Appendix C.4" prop_examples
  , testProperty "decodes what was encoded" prop_roundTrip
  , testProperty "huffmanLength is the sum of Appendix B's lengths, in whole octets" (prop_length rows)
  , testProperty "pokeHuffman writes exactly huffmanLength octets" prop_pokeExact
  , testProperty "decoding agrees with a bit-by-bit decoder of Appendix B's table, on any input"
      (prop_reference rows)
  , testProperty "padding and EOS, at the boundaries" (prop_padding rows)
  , testProperty "decoding 5-bit codes gives 8 octets for every 5" prop_shortest
  ]

prop_table :: [HuffmanRow] -> Property
prop_table rows = once $
  map rowSymbol rows === [0 .. 256]
  .&&. conjoin
    [ counterexample (show r) $
        fromIntegral (huffmanCode (rowSymbol r)) === rowHex r
        .&&. codeLengths !! rowSymbol r === rowLength r
        .&&. length (rowBits r) === rowLength r
        .&&. showBits (rowLength r) (rowHex r) === rowBits r
    | r <- rows ]

showBits :: Int -> Integer -> String
showBits len v = [if testBit v i then '1' else '0' | i <- [len - 1, len - 2 .. 0]]

hex :: ByteString -> String
hex = concatMap (\w -> (if w < 16 then "0" else "") ++ showHex w "") . B.unpack

prop_examples :: Property
prop_examples = once $ conjoin
  [ hex (encodeHuffman "www.example.com") === "f1e3c2e5f23a6ba0ab90f4ff"   -- C.4.1
  , hex (encodeHuffman "no-cache") === "a8eb10649cbf"                       -- C.4.2
  , decodeHuffman (B.pack [0xf1, 0xe3, 0xc2, 0xe5, 0xf2, 0x3a, 0x6b, 0xa0, 0xab, 0x90, 0xf4, 0xff])
      === Right "www.example.com"
  , encodeHuffman "" === "" , decodeHuffman "" === Right ""
  ]

newtype Octets = Octets ByteString deriving (Show)

instance Arbitrary Octets where
  arbitrary = Octets . B.pack <$> oneof
    [ arbitrary
    , listOf (elements (B.unpack "abcdefghijklmnopqrstuvwxyz0123456789-./:;= "))
    , listOf (elements [0, 1, 0xFF, 0xFE, 127, 128, 10, 13, 22])  -- long codes
    ]
  shrink (Octets b) = Octets . B.pack <$> shrink (B.unpack b)

-- the decoder's output is at most 8/5 of its input: the shortest
-- codes are 5 bits, and the bound is reached
prop_shortest :: Property
prop_shortest = forAll (listOf (elements (B.unpack "012aceiost"))) $ \cs ->
  let s = B.pack cs
      e = encodeHuffman s
  in decodeHuffman e === Right s
     .&&. counterexample "not 8 symbols for 5 octets" (B.length s >= (B.length e * 8) `div` 5 - 1)

prop_roundTrip :: Octets -> Property
prop_roundTrip (Octets b) = decodeHuffman (encodeHuffman b) === Right b

prop_length :: [HuffmanRow] -> Octets -> Property
prop_length rows (Octets b) =
  let bits = sum [length (code rows (fromIntegral c)) | c <- B.unpack b]
      expected = (bits + 7) `div` 8
  in huffmanLength b === expected .&&. B.length (encodeHuffman b) === expected

prop_pokeExact :: Octets -> Property
prop_pokeExact (Octets b) = unsafePerformIO $ do
  let len = huffmanLength b
      size = len + 4
  fp <- mallocForeignPtrBytes size
  withForeignPtr fp $ \p -> do
    fillBytes p 0xAA size
    q <- pokeHuffman b p
    os <- peekArray size p
    pure $ q `minusPtr` p === len
      .&&. drop len os === replicate 4 0xAA
      .&&. decodeHuffman (B.pack (take len os)) === Right b

-- | Appendix B read as a map from bit strings to symbols, and Section
-- 5.2 applied one bit at a time: a code is complete when the bits read
-- are one of the table's; what is left at the end is padding.
reference :: [HuffmanRow] -> ByteString -> Either HuffmanError ByteString
reference rows = go [] "" . concatMap bits . B.unpack
  where
    table = M.fromList [(rowBits r, rowSymbol r) | r <- rows]
    eos = code rows 256
    bits w = [if testBit w i then '1' else '0' | i <- [7, 6 .. 0 :: Int]]
    go out cur [] | length cur > 7 = Left PaddingTooLong
                  | not (cur `isPrefixOf` eos) = Left PaddingNotEOS
                  | otherwise = Right (B.pack (reverse out))
    go out cur (b : rest) = let cur' = cur ++ [b] in
      case M.lookup cur' table of
        Just 256 -> Left ContainsEOS
        Just s   -> go (fromIntegral s : out) "" rest
        Nothing  -> go out cur' rest

-- | Encodings of strings, damaged in the ways that matter: the padding
-- changed or lengthened, EOS appended, octets flipped or cut.
damaged :: [HuffmanRow] -> Gen ByteString
damaged rows = do
  Octets b <- arbitrary
  let enc = encodeHuffman b
  oneof
    [ pure enc
    , B.pack <$> arbitrary
    , (enc <>) . B.pack <$> listOf1 (elements [0xFF, 0xFE, 0x7F, 0x00])
    , pure (B.take (B.length enc - 1) enc)
    , do i <- choose (0, max 0 (B.length enc - 1))
         x <- arbitrary :: Gen Word8
         pure (B.take i enc <> B.singleton x <> B.drop (i + 1) enc)
    , pure (withEOS b)
    , do k <- choose (1, 7 :: Int)  -- the last octet's low bits cleared
         pure (if B.null enc then enc
               else B.init enc <> B.singleton (B.last enc .&. complement ((1 `shiftL` k) - 1)))
    ]
  where
    -- the string, then the EOS code, then padding
    withEOS b = packBits (concat [code rows (fromIntegral c) | c <- B.unpack b] ++ code rows 256)
    packBits bs = B.pack (map toOctet (chunks (bs ++ replicate ((8 - length bs `mod` 8) `mod` 8) '1')))
    chunks [] = []
    chunks xs = take 8 xs : chunks (drop 8 xs)
    toOctet = foldl (\a c -> a * 2 + (if c == '1' then 1 else 0)) 0

-- | The code of a symbol as bits, from Appendix B.
code :: [HuffmanRow] -> Int -> String
code rows s = case [rowBits r | r <- rows, rowSymbol r == s] of
  (c : _) -> c
  []      -> error ("Appendix B has no symbol " ++ show s)

prop_reference :: [HuffmanRow] -> Property
prop_reference rows = forAll (damaged rows) $ \input ->
  decodeHuffman input === reference rows input

-- = https://www.rfc-editor.org/rfc/rfc7541#section-5.2
-- # A padding strictly longer
-- # than 7 bits MUST be treated as a decoding error.  A padding not
-- # corresponding to the most significant bits of the code for the EOS
-- # symbol MUST be treated as a decoding error.  A Huffman-encoded string
-- # literal containing the EOS symbol MUST be treated as a decoding
-- # error.
prop_padding :: [HuffmanRow] -> Property
prop_padding rows = once $ conjoin
  [ -- "0" is 00000 (5 bits): 3 bits of padding, all ones
    decodeHuffman (B.pack [0x07]) === Right "0"
  , decodeHuffman (B.pack [0x06]) === Left PaddingNotEOS
  , decodeHuffman (B.pack [0x03]) === Left PaddingNotEOS
    -- '&' is 11111000: no padding; then a whole octet of ones is too much
  , decodeHuffman (B.pack [0xF8]) === Right "&"
  , decodeHuffman (B.pack [0xF8, 0xFF]) === Left PaddingTooLong
  , decodeHuffman (B.pack [0xFF]) === Left PaddingTooLong
    -- seven ones and a zero: an unfinished code of eight bits
  , decodeHuffman (B.pack [0xFE]) === Left PaddingTooLong
    -- EOS itself: 30 ones, padded with two ones
  , decodeHuffman (B.pack [0xFF, 0xFF, 0xFF, 0xFF]) === Left ContainsEOS
  , counterexample "reference agrees" $
      conjoin [ decodeHuffman (B.pack os) === reference rows (B.pack os)
              | os <- [[0x07], [0x06], [0x03], [0xF8], [0xF8, 0xFF], [0xFF], [0xFE], [0xFF, 0xFF, 0xFF, 0xFF]] ]
  ]
