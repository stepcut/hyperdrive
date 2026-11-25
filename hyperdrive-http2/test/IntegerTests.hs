module IntegerTests (integerTests) where

import Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.ByteString as B
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (minusPtr)
import System.IO.Unsafe (unsafePerformIO)
import Hyperdrive.HPACK.Integer
import Test.QuickCheck hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

integerTests :: TestTree
integerTests = testGroup "Integers with a prefix (RFC 7541 Section 5.1)"
  [ testProperty "encodes as Section 5.1's pseudocode does, and integerLength is its length" prop_encode
  , testProperty "pokeInteger writes exactly integerLength octets" prop_pokeExact
  , testProperty "Appendix C.1's examples" prop_examples
  , testProperty "decodes what was encoded, whatever the bits above the prefix" prop_roundTrip
  , testProperty "agrees with Section 5.1's pseudocode, with the limits" prop_reference
  , testProperty "limits, at their boundaries" prop_limits
  ]

newtype Prefix = Prefix Int deriving (Show)

instance Arbitrary Prefix where
  arbitrary = Prefix <$> choose (1, 8)

-- | Values near the edges: small, around 2^N - 1, and up to the limit.
value :: Int -> Gen Int
value n = oneof
  [ choose (0, 300)
  , (\d -> max 0 ((1 `shiftL` n) - 1 + d)) <$> choose (-2, 2)
  , choose (0, maxInteger)
  , (\k d -> max 0 ((1 `shiftL` k) + d)) <$> choose (1, 61) <*> choose (-2, 2)
    -- continuation octets of exactly 128 and 127
  , (\k d -> (1 `shiftL` n) - 1 + 128 ^ (k :: Int) + d) <$> choose (1, 8) <*> choose (-1, 0)
  , pure maxInteger
  ]

-- the bits above an n-bit prefix
highBits :: Int -> Gen Word8
highBits n = (\b -> b .&. complementMask) <$> arbitrary
  where complementMask = fromIntegral (0xFF - ((1 `shiftL` n) - 1) :: Int)

-- | Write the integer into a buffer of sentinel octets, longer than it
-- needs: the octets written, and how far the returned pointer moved.
pokeIntoSentinels :: Int -> Int -> ([Word8], Int)
pokeIntoSentinels n v = unsafePerformIO $ do
  let size = integerLength n v + 4
  fp <- mallocForeignPtrBytes size
  withForeignPtr fp $ \p -> do
    fillBytes p 0xAA size
    q <- pokeInteger n 0 v p
    os <- peekArray size p
    pure (os, q `minusPtr` p)

prop_pokeExact :: Prefix -> Property
prop_pokeExact (Prefix n) = forAll (value n) $ \v ->
  let ref = referenceEncode n (fromIntegral v)
      (os, moved) = pokeIntoSentinels n v
  in moved === length ref .&&. os === ref ++ replicate 4 0xAA

prop_examples :: Property
prop_examples = once $ conjoin
  -- C.1.1: 10 with a 5-bit prefix
  [ encodeInteger 5 0 10 === B.pack [0x0A]
  , decodeInteger 5 (B.pack [0x0A]) 0 === Decoded 10 1
  -- C.1.2: 1337 with a 5-bit prefix
  , encodeInteger 5 0 1337 === B.pack [0x1F, 0x9A, 0x0A]
  , decodeInteger 5 (B.pack [0x1F, 0x9A, 0x0A]) 0 === Decoded 1337 3
  -- C.1.3: 42 at an octet boundary
  , encodeInteger 8 0 42 === B.pack [0x2A]
  , decodeInteger 8 (B.pack [0x2A]) 0 === Decoded 42 1
  ]

prop_roundTrip :: Prefix -> Property
prop_roundTrip (Prefix n) =
  forAll (value n) $ \v ->
  forAll (highBits n) $ \h ->
  forAll arbitrary $ \(before, rest) ->
    let enc = encodeInteger n h v
        input = B.pack before <> enc <> B.pack rest
    in B.head enc .&. fromIntegral (0xFF - ((1 `shiftL` n) - 1) :: Int) === h
       .&&. decodeInteger n input (length before) === Decoded v (length before + B.length enc)

-- | Section 5.1's pseudocode for encoding, in unbounded integers.
referenceEncode :: Int -> Integer -> [Word8]
referenceEncode n i
  | i < 2 ^ n - 1 = [fromIntegral i]
  | otherwise = fromIntegral (2 ^ n - 1 :: Integer) : go (i - (2 ^ n - 1))
  where
    go r | r >= 128  = fromIntegral (r `mod` 128 + 128) : go (r `div` 128)
         | otherwise = [fromIntegral r]

prop_encode :: Prefix -> Property
prop_encode (Prefix n) = forAll (value n) $ \v ->
  let ref = referenceEncode n (fromIntegral v)
  in integerLength n v === length ref .&&. B.unpack (encodeInteger n 0 v) === ref

-- | Section 5.1's pseudocode for decoding, in unbounded integers, with
-- the module's limits: at most nine octets after the prefix, and a
-- value of at most 2^62 - 1.
reference :: Int -> [Word8] -> DecodedInteger
reference _ [] = Incomplete
reference n (b : bs)
  | i < mask  = Decoded (fromIntegral i) 1
  | otherwise = go i 0 1 bs
  where
    mask = 2 ^ n - 1 :: Integer
    i = fromIntegral b .&. mask
    go acc m k rest
      | k > 9 = TooLarge
      | otherwise = case rest of
          [] -> Incomplete
          (o : rest') ->
            let acc' = acc + fromIntegral (o .&. 127) * 2 ^ (m :: Int)
            in if acc' > fromIntegral maxInteger then TooLarge
               else if o .&. 128 == 128 then go acc' (m + 7) (k + 1) rest'
               else Decoded (fromIntegral acc') (k + 1)

prop_reference :: Prefix -> Property
prop_reference (Prefix n) = forAll input $ \os ->
  decodeInteger n (B.pack os) 0 === reference n os
  where
    -- a full prefix, then continuation octets, often zeros, sometimes
    -- many, with or without an end
    input = do
      first <- oneof [pure (fromIntegral ((1 `shiftL` n) - 1 :: Int)), arbitrary]
      k <- frequency [(3, choose (0, 9)), (1, choose (10, 14))]
      cont <- vectorOf k (oneof [pure 0x80, pure 0xFF, (.|. 0x80) <$> arbitrary])
      end <- oneof [pure [], (: []) . (.&. 0x7F) <$> arbitrary]
      pure (first : cont ++ end)

-- = https://www.rfc-editor.org/rfc/rfc7541#section-5.1
-- # Integer encodings that exceed implementation limits -- in
-- # value or octet length -- MUST be treated as decoding errors.
prop_limits :: Property
prop_limits = once $ conjoin
  [ decodeInteger 8 (encodeInteger 8 0 maxInteger) 0 === Decoded maxInteger 10
  , decodeInteger 1 (encodeInteger 1 0 maxInteger) 0 === Decoded maxInteger 10
    -- one more than the limit, with every prefix
  , conjoin [decodeInteger n (encodeInteger n 0 (maxInteger + 1)) 0 === TooLarge | n <- [1 .. 8]]
    -- nine continuation octets that add nothing still end; ten do not
  , decodeInteger 8 (B.pack ([0xFF] ++ replicate 8 0x80 ++ [0x00])) 0 === Decoded 255 10
  , decodeInteger 8 (B.pack ([0xFF] ++ replicate 9 0x80 ++ [0x00])) 0 === TooLarge
  , decodeInteger 5 (B.pack [0x1F]) 0 === Incomplete
  , decodeInteger 5 (B.pack [0x1F, 0x9A]) 0 === Incomplete
  , decodeInteger 5 B.empty 0 === Incomplete
  ]
