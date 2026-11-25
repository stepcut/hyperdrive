-- | HPACK's integers with an N-bit prefix (RFC 7541, Section 5.1).
--
-- QPACK uses the same representation (RFC 9204, Section 4.1.1), so
-- this module depends on nothing of HTTP/2's (docs/http2-core-design.md,
-- D1). Its limit is QPACK's, which is the larger: "QPACK
-- implementations MUST be able to decode integers up to and including
-- 62 bits long". HPACK's callers bound the values further (an index by
-- the tables, a length by the input that is left).
module Hyperdrive.HPACK.Integer
  ( maxInteger
  , DecodedInteger(..)
  , decodeInteger
  , integerLength
  , pokeInteger
  , encodeInteger
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)

-- | The largest integer decoded: 2^62 - 1.
maxInteger :: Int
maxInteger = 2 ^ (62 :: Int) - 1

data DecodedInteger
  = Decoded !Int !Int   -- ^ the value, and the offset just after it
  | Incomplete          -- ^ the input ends inside the integer
  | TooLarge            -- ^ above 'maxInteger', or too many octets for it
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc7541#section-5.1
-- # This integer representation allows for values of indefinite size.  It
-- # is also possible for an encoder to send a large number of zero
-- # values, which can waste octets and could be used to overflow integer
-- # values.  Integer encodings that exceed implementation limits -- in
-- # value or octet length -- MUST be treated as decoding errors.

-- | Decode the integer with an @n@-bit prefix (1 to 8) that starts in
-- the octet at the given offset; the bits above the prefix are the
-- caller's. At most nine octets follow the prefix (63 bits of value),
-- so a run of continuation octets that add nothing is refused as soon
-- as it could not end within the limit.
decodeInteger :: Int -> ByteString -> Int -> DecodedInteger
decodeInteger n bs i
  | i >= B.length bs = Incomplete
  | otherwise =
      let mask = (1 `shiftL` n) - 1
          prefix = fromIntegral (BU.unsafeIndex bs i) .&. mask
      in if prefix < mask then Decoded prefix (i + 1)
         else continuation bs (i + 1) prefix 0

-- A top-level loop taking everything as arguments: as a local one it
-- would be a closure allocated on every call.
continuation :: ByteString -> Int -> Int -> Int -> DecodedInteger
continuation bs !i !acc !shift
  | shift > 56 = TooLarge
  | i >= B.length bs = Incomplete
  | otherwise =
      let b = BU.unsafeIndex bs i
          acc' = acc + (fromIntegral (b .&. 0x7F) `shiftL` shift)
      in if acc' > maxInteger || acc' < acc then TooLarge
         else if b .&. 0x80 == 0 then Decoded acc' (i + 1)
         else continuation bs (i + 1) acc' (shift + 7)

-- | The number of octets the integer takes with an @n@-bit prefix.
integerLength :: Int -> Int -> Int
integerLength n v
  | v < mask  = 1
  | otherwise = go 2 (v - mask)
  where
    mask = (1 `shiftL` n) - 1
    go !k r = if r < 128 then k else go (k + 1) (r `shiftR` 7)

-- | Write the integer with an @n@-bit prefix, the bits above the prefix
-- taken from @high@; returns the pointer just after it, 'integerLength'
-- octets on. The value must not be negative.
pokeInteger :: Int -> Word8 -> Int -> Ptr Word8 -> IO (Ptr Word8)
pokeInteger n high v p
  | v < mask = do
      poke p (high .|. fromIntegral v)
      pure (p `plusPtr` 1)
  | otherwise = do
      poke p (high .|. fromIntegral mask)
      pokeRest (v - mask) (p `plusPtr` 1)
  where
    mask = (1 `shiftL` n) - 1

pokeRest :: Int -> Ptr Word8 -> IO (Ptr Word8)
pokeRest !r !p
  | r < 128 = do
      poke p (fromIntegral r :: Word8)
      pure (p `plusPtr` 1)
  | otherwise = do
      poke p (fromIntegral (r .&. 0x7F) .|. 0x80 :: Word8)
      pokeRest (r `shiftR` 7) (p `plusPtr` 1)

-- | The integer with an @n@-bit prefix, the bits above it from @high@.
encodeInteger :: Int -> Word8 -> Int -> ByteString
encodeInteger n high v =
  BI.unsafeCreate (integerLength n v) $ \p -> () <$ pokeInteger n high v p
