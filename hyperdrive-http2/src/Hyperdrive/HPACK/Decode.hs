-- | Decoding HPACK field blocks (RFC 7541, Sections 3 and 6).
--
-- A field block is decoded whole (HTTP/2 delivers it as HEADERS and
-- its CONTINUATIONs, whose payloads the caller joins), so input that
-- ends inside a representation is an error. Every error is a
-- connection error of type COMPRESSION_ERROR to the caller (RFC 9113
-- Section 4.3): the table is shared by all streams, so a block cannot
-- be dropped on its own.
--
-- The decoder checks only HPACK. Whether the names and values make a
-- valid request is for the mapping of fields to a request
-- (docs/http2-core-design.md, D11). Names and values not added to the
-- table are slices of the input.
module Hyperdrive.HPACK.Decode
  ( Decoder
  , newDecoder
  , decoderTable
  , decoderLimit
  , Decoded(..)
  , DecodeError(..)
  , decodeBlock
  ) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Hyperdrive.HPACK.Huffman (HuffmanError, decodeHuffman)
import Hyperdrive.HPACK.Integer (DecodedInteger(..), decodeInteger)
import Hyperdrive.HPACK.Table

-- | A decoder's state: its dynamic table, and the largest size an
-- update may set it to (in HTTP/2, the SETTINGS_HEADER_TABLE_SIZE the
-- server sent).
data Decoder = Decoder
  { decoderTable :: !DynamicTable
  , decoderLimit :: !Int
  } deriving (Eq, Show)

-- | A decoder whose table starts empty with the given maximum size,
-- which is also the limit for updates.
newDecoder :: Int -> Decoder
newDecoder limit = Decoder (emptyTable limit) limit

data Decoded
  = Fields [(ByteString, ByteString)]
    -- ^ the field list, in the order of the block
  | ListTooLarge
    -- ^ the field list is larger than the limit given; the table was
    -- still updated by the whole block, as the encoder expects
  deriving (Eq, Show)

data DecodeError
  = Truncated                 -- ^ the block ends inside a representation
  | IntegerTooLarge           -- ^ an integer beyond the limits (Section 5.1)
  | IndexZero                 -- ^ index 0 in an indexed field (Section 6.1)
  | IndexOutOfRange !Int      -- ^ an index beyond both tables (Section 2.3.3)
  | BadHuffman !HuffmanError  -- ^ Section 5.2
  | TableSizeAboveLimit !Int  -- ^ a size update above the limit (Section 6.3)
  | TableSizeUpdateLate       -- ^ a size update after a field (Section 4.2)
  | TooManyTableSizeUpdates   -- ^ more than two size updates (Section 4.2)
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc7541#section-3.2
-- # To ensure that the decoding will successfully
-- # produce a header list, a decoder MUST obey the following rules.

-- | Decode one field block, given the largest field list to return,
-- measured as RFC 9113 Section 6.5.2 does (each field's name and value
-- and 32).
decodeBlock :: Int -> Decoder -> ByteString -> Either DecodeError (Decoded, Decoder)
decodeBlock maxList (Decoder table limit) bs = go table 0 2 0 [] 0
  where
    -- i: the next octet; updates: size updates still allowed here;
    -- listSize: the field list's size so far; acc: fields, last first
    go !t !i !updates !listSize acc !count
      | i >= B.length bs =
          -- = https://www.rfc-editor.org/rfc/rfc7541#section-2.1
          -- # A decoder MUST order header fields in the decoded header list
          -- # according to their ordering in the header block.
          let d = if listSize > maxList then ListTooLarge else Fields (reverse acc)
          in Right (d, Decoder t limit)
      | otherwise =
          let b = BU.unsafeIndex bs i
          in if b .&. 0x80 /= 0 then indexed t i listSize acc count
             else if b .&. 0xC0 == 0x40 then literal True 6 t i listSize acc count
             else if b .&. 0xE0 == 0x20 then sizeUpdate t i updates listSize acc count
             else literal False 4 t i listSize acc count

    -- a field decoded: counted, and kept while the list is within the limit
    field t i' listSize acc count n v =
      let listSize' = listSize + entrySize n v
          acc' = if listSize' > maxList then [] else (n, v) : acc
      in go t i' (0 :: Int) listSize' acc' (count + 1 :: Int)

    -- = https://www.rfc-editor.org/rfc/rfc7541#section-6.1
    -- # The index value of 0 is not used.  It MUST be treated as a decoding
    -- # error if found in an indexed header field representation.
    indexed t i listSize acc count = case decodeInteger 7 bs i of
      Decoded 0 _ -> Left IndexZero
      Decoded ix i' -> case lookupIndex ix t of
        Just (n, v) -> field t i' listSize acc count n v
        Nothing     -> Left (IndexOutOfRange ix)
      Incomplete -> Left Truncated
      TooLarge   -> Left IntegerTooLarge

    -- with indexing (6-bit prefix), or without or never indexed (4-bit
    -- prefix, which the decoder treats alike)
    literal adds prefix t i listSize acc count = do
      (n, i1) <- case decodeInteger prefix bs i of
        Decoded 0 i' -> decodeString bs i'
        Decoded ix i' -> case lookupIndex ix t of
          Just (n, _) -> Right (n, i')
          Nothing     -> Left (IndexOutOfRange ix)
        Incomplete -> Left Truncated
        TooLarge   -> Left IntegerTooLarge
      (v, i2) <- decodeString bs i1
      let t' = if adds then insertEntry n v t else t
      field t' i2 listSize acc count n v

    -- = https://www.rfc-editor.org/rfc/rfc7541#section-6.3
    -- # The new maximum size MUST be lower than or equal to the limit
    -- # determined by the protocol using HPACK.  A value that exceeds this
    -- # limit MUST be treated as a decoding error.
    --
    -- = https://www.rfc-editor.org/rfc/rfc7541#section-4.2
    -- # This dynamic table size
    -- # update MUST occur at the beginning of the first header block
    -- # following the change to the dynamic table size.
    --
    -- That requirement is on the encoder; the decoder holds it to it,
    -- and to the same section's "at most two dynamic table size
    -- updates", so that a block of nothing but updates is refused.
    sizeUpdate t i updates listSize acc count
      | count > 0    = Left TableSizeUpdateLate
      | updates <= 0 = Left TooManyTableSizeUpdates
      | otherwise = case decodeInteger 5 bs i of
          Decoded m i'
            | m > limit -> Left (TableSizeAboveLimit m)
            | otherwise -> go (resizeTable m t) i' (updates - 1) listSize acc count
          Incomplete -> Left Truncated
          TooLarge   -> Left IntegerTooLarge

-- | A string literal (Section 5.2) at the offset: the string, and the
-- offset after it.
decodeString :: ByteString -> Int -> Either DecodeError (ByteString, Int)
decodeString bs i = case decodeInteger 7 bs i of
  Decoded len i'
    | len > B.length bs - i' -> Left Truncated
    | otherwise ->
        let raw = BU.unsafeTake len (BU.unsafeDrop i' bs)
            huffman = BU.unsafeIndex bs i .&. 0x80 /= 0
        in if huffman
             then either (Left . BadHuffman) (\s -> Right (s, i' + len)) (decodeHuffman raw)
             else Right (raw, i' + len)
  Incomplete -> Left Truncated
  TooLarge   -> Left IntegerTooLarge
