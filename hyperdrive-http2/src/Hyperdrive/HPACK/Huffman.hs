-- | HPACK's Huffman code for string literals (RFC 7541, Section 5.2 and
-- Appendix B).
--
-- QPACK uses the same code (RFC 9204, Section 4.1.2), so this module
-- depends on nothing of HTTP/2's (docs/http2-core-design.md, D1).
--
-- The code is canonical: ordered by length, codes of one length are
-- consecutive and in the order of their symbols. So the table below
-- holds only each symbol's code length, and the codes are derived from
-- the lengths; the tests check both against Appendix B's text. Decoding
-- looks at the next 32 bits and finds the length of the code they start
-- with by comparing them with the first code of each greater length,
-- then the symbol by its rank among the codes of that length.
module Hyperdrive.HPACK.Huffman
  ( codeLengths
  , huffmanCode
  , HuffmanError(..)
  , decodeHuffman
  , huffmanLength
  , pokeHuffman
  , encodeHuffman
  ) where

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.List (sortOn)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (poke)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | The length in bits of the code of each symbol: the octets 0 to 255,
-- then EOS (256). From RFC 7541 Appendix B.
codeLengths :: [Int]
codeLengths =
  [ 13, 23, 28, 28, 28, 28, 28, 28, 28, 24, 30, 28, 28, 30, 28, 28  -- 0-15
  , 28, 28, 28, 28, 28, 28, 30, 28, 28, 28, 28, 28, 28, 28, 28, 28  -- 16-31
  ,  6, 10, 10, 12, 13,  6,  8, 11, 10, 10,  8, 11,  8,  6,  6,  6  -- 32-47
  ,  5,  5,  5,  6,  6,  6,  6,  6,  6,  6,  7,  8, 15,  6, 12, 10  -- 48-63
  , 13,  6,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7  -- 64-79
  ,  7,  7,  7,  7,  7,  7,  7,  7,  8,  7,  8, 13, 19, 13, 14,  6  -- 80-95
  , 15,  5,  6,  5,  6,  5,  6,  6,  6,  5,  7,  7,  6,  6,  6,  5  -- 96-111
  ,  6,  7,  6,  5,  5,  6,  7,  7,  7,  7,  7, 15, 11, 14, 13, 28  -- 112-127
  , 20, 22, 20, 20, 22, 22, 22, 23, 22, 23, 23, 23, 23, 23, 24, 23  -- 128-143
  , 24, 24, 22, 23, 24, 23, 23, 23, 23, 21, 22, 23, 22, 23, 23, 24  -- 144-159
  , 22, 21, 20, 22, 22, 23, 23, 21, 23, 22, 22, 24, 21, 22, 23, 23  -- 160-175
  , 21, 21, 22, 21, 23, 22, 23, 23, 20, 22, 22, 22, 23, 22, 22, 23  -- 176-191
  , 26, 26, 20, 19, 22, 23, 22, 25, 26, 26, 26, 27, 27, 26, 24, 25  -- 192-207
  , 19, 21, 26, 27, 27, 26, 27, 24, 21, 21, 26, 26, 28, 27, 27, 27  -- 208-223
  , 20, 24, 20, 21, 22, 21, 21, 23, 22, 22, 25, 25, 24, 24, 26, 23  -- 224-239
  , 26, 27, 26, 26, 27, 27, 27, 27, 27, 28, 27, 27, 27, 27, 27, 26  -- 240-255
  , 30                                                              -- EOS
  ]

maxLength :: Int
maxLength = 30

-- symbols in the canonical order: by length, then by symbol
canonical :: [(Int, Int)]  -- (symbol, length)
canonical = sortOn (\(s, l) -> (l, s)) (zip [0 ..] codeLengths)

-- the codes in canonical order: each is the one before it plus one,
-- shifted left by the difference in length
canonicalCodes :: [(Int, Int, Word32)]  -- (symbol, length, code)
canonicalCodes = go 0 0 canonical
  where
    go _ _ [] = []
    go code prevLen ((s, l) : rest) =
      let c = code `shiftL` (l - prevLen)
      in (s, l, c) : go (c + 1) l rest

lengthArray :: UArray Int Word8
lengthArray = listArray (0, 256) (map fromIntegral codeLengths)

codeArray :: UArray Int Word32
codeArray = listArray (0, 256) (map snd (sortOn fst [(s, c) | (s, _, c) <- canonicalCodes]))

-- | The code of a symbol (0 to 256), aligned on its least significant
-- bit, as Appendix B's "code as hex".
huffmanCode :: Int -> Word32
huffmanCode = unsafeAt codeArray

-- For each length L, every code of length L or less, left-aligned in 32
-- bits, is below limitArray ! L; deltaArray ! L, plus a code of length L
-- (aligned on its least significant bit), is that code's rank in
-- canonical order.
limitArray :: UArray Int Word64
limitArray = listArray (0, maxLength) (scanl1 max [limit l | l <- [0 .. maxLength]])
  where
    -- a length with no codes has the limit of the length before it
    limit l = case [c | (_, l', c) <- canonicalCodes, l' == l] of
      [] -> 0
      cs -> (fromIntegral (maximum cs) + 1) `shiftL` (32 - l)

deltaArray :: UArray Int Int
deltaArray = listArray (0, maxLength) [delta l | l <- [0 .. maxLength]]
  where
    delta l = case [(rank, c) | (rank, (_, l', c)) <- zip [0 ..] canonicalCodes, l' == l] of
      []              -> 0
      ((rank, c) : _) -> rank - fromIntegral c

symbolArray :: UArray Int Word16
symbolArray = listArray (0, 256) [fromIntegral s | (s, _, _) <- canonicalCodes]

data HuffmanError
  = PaddingTooLong   -- ^ more than 7 bits after the last code
  | PaddingNotEOS    -- ^ padding bits that are not all ones
  | ContainsEOS      -- ^ the EOS symbol in the string
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc7541#section-5.2
-- # Upon decoding, an incomplete code at the end of the encoded data is
-- # to be considered as padding and discarded.  A padding strictly longer
-- # than 7 bits MUST be treated as a decoding error.  A padding not
-- # corresponding to the most significant bits of the code for the EOS
-- # symbol MUST be treated as a decoding error.  A Huffman-encoded string
-- # literal containing the EOS symbol MUST be treated as a decoding
-- # error.

-- | Decode a Huffman-coded string.
decodeHuffman :: ByteString -> Either HuffmanError ByteString
decodeHuffman bs
  | B.null bs = Right B.empty
  | otherwise = unsafeDupablePerformIO $ do
      -- the shortest code is 5 bits
      let cap = (B.length bs * 8) `div` 5
      fp <- BI.mallocByteString cap
      r <- withForeignPtr fp $ \p -> decodeLoop bs p p 0 0 0
      pure $ case r of
        Left e  -> Left e
        Right n -> Right (B.copy (BI.BS fp n))

-- The input's next octet is at i; acc's low n bits are the bits read
-- but not yet decoded. Filled to more than 56 bits while input lasts,
-- so that a whole code (30 bits at most) is always there unless the
-- input has ended.
decodeLoop :: ByteString -> Ptr Word8 -> Ptr Word8 -> Int -> Word64 -> Int
           -> IO (Either HuffmanError Int)
decodeLoop !bs !start !out !i !acc !n
  | n <= 56, i < B.length bs =
      decodeLoop bs start out (i + 1)
        ((acc `shiftL` 8) .|. fromIntegral (BU.unsafeIndex bs i)) (n + 8)
  | n == 0 = pure (Right (out `minusPtr` start))
  | otherwise =
      let w = if n >= 32 then (acc `shiftR` (n - 32)) .&. 0xFFFFFFFF
                         else (acc `shiftL` (32 - n)) .&. 0xFFFFFFFF
          l = findLength w 5
      in if l > n
           -- the input has ended inside a code: what is left is padding
           then let ones = (1 `shiftL` n) - 1
                in pure $ if n > 7 then Left PaddingTooLong
                          else if acc .&. ones /= ones then Left PaddingNotEOS
                          else Right (out `minusPtr` start)
           else
             let rank = unsafeAt deltaArray l + fromIntegral (w `shiftR` (32 - l))
                 sym = unsafeAt symbolArray rank
             in if sym == 256 then pure (Left ContainsEOS)
                else do
                  poke out (fromIntegral sym :: Word8)
                  decodeLoop bs start (out `plusPtr` 1) i acc (n - l)

findLength :: Word64 -> Int -> Int
findLength !w !l
  | l >= maxLength || w < unsafeAt limitArray l = l
  | otherwise = findLength w (l + 1)

-- | The number of octets of the string once Huffman-coded.
huffmanLength :: ByteString -> Int
huffmanLength bs = (B.foldl' (\t c -> t + fromIntegral (unsafeAt lengthArray (fromIntegral c))) 0 bs + 7)
                   `shiftR` 3

-- | Write the Huffman code of the string, padded with the most
-- significant bits of EOS (all ones); returns the pointer just after
-- it, 'huffmanLength' octets on.
pokeHuffman :: ByteString -> Ptr Word8 -> IO (Ptr Word8)
pokeHuffman bs p0 = encodeLoop bs 0 p0 0 0

-- acc's low n bits (fewer than 8 between octets) are not yet written
encodeLoop :: ByteString -> Int -> Ptr Word8 -> Word64 -> Int -> IO (Ptr Word8)
encodeLoop !bs !i !p !acc !n
  | i < B.length bs = do
      let c = fromIntegral (BU.unsafeIndex bs i)
          l = fromIntegral (unsafeAt lengthArray c)
      flushOctets bs (i + 1) p ((acc `shiftL` l) .|. fromIntegral (unsafeAt codeArray c)) (n + l)
  | n > 0 = do
      poke p (fromIntegral ((acc `shiftL` (8 - n)) .|. (0xFF `shiftR` n)) :: Word8)
      pure (p `plusPtr` 1)
  | otherwise = pure p

-- write acc's whole octets, then go on with the input
flushOctets :: ByteString -> Int -> Ptr Word8 -> Word64 -> Int -> IO (Ptr Word8)
flushOctets !bs !i !p !acc !n
  | n >= 8 = do
      poke p (fromIntegral (acc `shiftR` (n - 8)) :: Word8)
      flushOctets bs i (p `plusPtr` 1) acc (n - 8)
  | otherwise = encodeLoop bs i p acc n

-- | The string, Huffman-coded.
encodeHuffman :: ByteString -> ByteString
encodeHuffman bs = BI.unsafeCreate (huffmanLength bs) $ \p -> () <$ pokeHuffman bs p
