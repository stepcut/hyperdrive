{-# LANGUAGE OverloadedStrings #-}
-- | Differential tests against nghttp2 (github.com/nghttp2): HPACK (the
-- two decoders on the same blocks, and each side's encoder read by the
-- other's decoder), and frames within whole sessions ("Sessions"). Run
-- with scripts/differential-nghttp2; NGHTTP2_SURVEY=N in front prints
-- every kind of disagreement of the decoders in N sessions, and
-- NGHTTP2_FRAMES_SURVEY=N every kind of disagreement of the sessions,
-- each with a count and an example.
module Main (main) where

import Control.Monad (forM_, replicateM)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import DecodeTests (Model(..), encodeRep, genBlock)
import EncodeTests (genFields)
import Foreign.C.Types (CLong(..), CSize(..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr)
import Hyperdrive.HPACK.Decode
import Hyperdrive.HPACK.Encode
import Hyperdrive.HPACK.Table (tableEntries, tableMaxSize)
import Sessions (frameSurvey, prop_sessions)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

foreign import ccall unsafe "hd_nghttp2_inflate"
  c_inflate :: Ptr Word8 -> Ptr CSize -> Ptr CLong -> CSize -> Ptr Word8 -> CSize -> IO CLong

foreign import ccall unsafe "hd_nghttp2_deflate"
  c_deflate :: Ptr Word8 -> Ptr CLong -> CSize -> CSize -> Ptr Word8 -> CSize -> IO CLong

------------------------------------------------------------------------------
-- running nghttp2
------------------------------------------------------------------------------

-- | Call the shim with an output buffer, larger until it fits.
withOutput :: Int -> (Ptr Word8 -> CSize -> IO CLong) -> IO ByteString
withOutput cap f = do
  fp <- mallocForeignPtrBytes cap
  r <- withForeignPtr fp $ \p -> f p (fromIntegral cap)
  case r of
    -1 -> withOutput (cap * 4) f
    n | n < 0 -> fail ("nghttp2 shim failed: " ++ show n)
      | otherwise -> pure (B.copy (BI.BS fp (fromIntegral n)))

records :: ByteString -> [(Char, ByteString)]
records b
  | B.length b < 5 = []
  | otherwise =
      let n = sum [fromIntegral (B.index b i) `shiftL` (8 * (i - 1)) | i <- [1 .. 4]]
      in (BI.w2c (B.head b), B.take n (B.drop 5 b)) : records (B.drop (5 + n) b)

limitsOf :: [Maybe Int] -> [CLong]
limitsOf = map (maybe (-1) fromIntegral)

-- | nghttp2's decoding of each block, up to and including the first
-- error.
theirDecode :: [(Maybe Int, ByteString)] -> [Either String [(ByteString, ByteString)]]
theirDecode blocks = unsafePerformIO $
  BU.unsafeUseAsCString (B.concat (map snd blocks) <> "\0") $ \input ->
  withArray (map (fromIntegral . B.length . snd) blocks) $ \lengths ->
  withArray (limitsOf (map fst blocks)) $ \limits -> do
    out <- withOutput 65536 $ c_inflate (castPtr input) lengths limits (fromIntegral (length blocks))
    pure (split [] (records out))
  where
    split acc (('N', n) : ('V', v) : rest) = split ((n, v) : acc) rest
    split acc (('E', _) : rest) = Right (reverse acc) : split [] rest
    split _ (('X', e) : _) = [Left (C.unpack e)]
    split _ _ = []

-- | nghttp2's encoding of each field list, its table capped at 4,096.
theirEncode :: [(Maybe Int, [(ByteString, ByteString)])] -> Either String [ByteString]
theirEncode lists = unsafePerformIO $
  BU.unsafeUseAsCString (B.concat (map (serialize . snd) lists) <> "\0") $ \input ->
  withArray (limitsOf (map fst lists)) $ \limits -> do
    out <- withOutput 65536 $ c_deflate (castPtr input) limits (fromIntegral (length lists)) 4096
    pure (blocksOrError (records out))
  where
    serialize fs = word32 (length fs) <> B.concat [word32 (B.length n) <> n <> word32 (B.length v) <> v | (n, v) <- fs]
    word32 x = B.pack [fromIntegral (x `shiftR` (8 * i)) .&. 0xFF | i <- [0 .. 3]]
    blocksOrError rs = case [e | ('X', e) <- rs] of
      (e : _) -> Left (C.unpack e)
      [] -> Right [b | ('B', b) <- rs]

------------------------------------------------------------------------------
-- running ours
------------------------------------------------------------------------------

ourDecode :: [(Maybe Int, ByteString)] -> [Either DecodeError [(ByteString, ByteString)]]
ourDecode = go (newDecoder 4096 4096)
  where
    go _ [] = []
    go d ((limit, block) : rest) =
      case decodeBlock maxBound (maybe d (`setDecoderLimit` d) limit) block of
        Left e -> [Left e]
        Right (Fields fs, d') -> Right fs : go d' rest
        Right (ListTooLarge, _) -> [Left Truncated]  -- not reached: no limit

------------------------------------------------------------------------------
-- sessions
------------------------------------------------------------------------------

-- | Blocks for one connection, each from the state our decoder is in
-- after the ones before it: sometimes a new limit first, sometimes
-- damaged.
genSession :: Gen [(Maybe Int, ByteString)]
genSession = do
  n <- choose (1, 6)
  go (newDecoder 4096 4096) n
  where
    go _ 0 = pure []
    go d k = do
      limit <- frequency [(6, pure Nothing), (1, Just <$> elements [0, 100, 256, 1000, 4096, 8192])]
      let d1 = maybe d (`setDecoderLimit` d) limit
          model = Model (tableMaxSize (decoderTable d1)) (tableEntries (decoderTable d1))
      reps <- genBlock (decoderLimit d1) model
      let block = B.concat (map encodeRep reps)
      block' <- frequency [(9, pure block), (1, damage block)]
      case decodeBlock maxBound d1 block' of
        Right (_, d') -> ((limit, block') :) <$> go d' (k - 1 :: Int)
        Left _ -> pure [(limit, block')]
    damage b = oneof
      [ (\i -> B.take i b) <$> choose (0, B.length b)
      , do i <- choose (0, max 0 (B.length b - 1))
           x <- arbitrary
           pure (B.take i b <> B.singleton x <> B.drop (i + 1) b) ]

-- | What a decoder made of a session, block by block: the fields, or
-- that it failed (the kinds of failure are each side's own).
data Outcome = Decoded [(ByteString, ByteString)] | Failed
  deriving (Eq, Show)

compareDecoders :: [(Maybe Int, ByteString)] -> ([Outcome], [Outcome], String, String)
compareDecoders s =
  ( map (either (const Failed) Decoded) ours, map (either (const Failed) Decoded) theirs
  , last ("" : [show e | Left e <- ours]), last ("" : [e | Left e <- theirs]) )
  where
    ours = ourDecode s
    theirs = theirDecode s

prop_decoders :: Property
prop_decoders = forAll genSession $ \s ->
  let (o, t, oe, te) = compareDecoders s
  in counterexample ("ours: " ++ oe ++ "; nghttp2: " ++ te) (o === t)

data Step = SetPeer Int | Block [(ByteString, ByteString)]
  deriving (Show)

genSteps :: Gen [Step]
genSteps = listOf1 (frequency
  [ (1, SetPeer <$> elements [0, 100, 256, 1000, 4096, 8192])
  , (4, Block <$> genFields) ])

-- the field lists, each with the last setting acknowledged before it
withSettings :: [Step] -> [(Maybe Int, [(ByteString, ByteString)])]
withSettings = go Nothing
  where
    go _ [] = []
    go _ (SetPeer x : rest) = go (Just x) rest
    go p (Block fs : rest) = (p, fs) : go Nothing rest

prop_oursToTheirs :: Property
prop_oursToTheirs = forAll genSteps $ \steps ->
  let lists = withSettings steps
      blocks = encodeAll (newEncoder 4096) lists
  in theirDecode (zip (map fst lists) blocks) === map (Right . snd) lists
  where
    encodeAll _ [] = []
    encodeAll e ((p, fs) : rest) =
      let (b, e') = encodeBlock (maybe e (`setPeerTableSize` e) p) fs in b : encodeAll e' rest

prop_theirsToOurs :: Property
prop_theirsToOurs = forAll genSteps $ \steps ->
  let lists = withSettings steps
  in case theirEncode lists of
       Left e -> counterexample ("nghttp2's encoder: " ++ e) False
       Right blocks -> ourDecode (zip (map fst lists) blocks) === map (Right . snd) lists

------------------------------------------------------------------------------
-- the survey
------------------------------------------------------------------------------

-- | Every kind of disagreement of the decoders in n sessions: how often,
-- and the shortest example.
survey :: Int -> IO ()
survey n = do
  sessions <- generate (replicateM n genSession)
  let diffs = M.fromListWith pick
        [ ((oe, te), (1 :: Int, s))
        | s <- sessions, let (o, t, oe, te) = compareDecoders s, o /= t ]
      pick (a, s1) (b, s2) = (a + b, if size s1 <= size s2 then s1 else s2)
      size = sum . map (B.length . snd)
  putStrLn (show n ++ " sessions, " ++ show (sum (map fst (M.elems diffs))) ++ " with a difference")
  forM_ (sortOn (negate . fst . snd) (M.toList diffs)) $ \((oe, te), (count, s)) -> do
    putStrLn ""
    putStrLn (show count ++ "x  ours: " ++ (if null oe then "decoded" else oe)
              ++ "  nghttp2: " ++ (if null te then "decoded" else te))
    forM_ s $ \(limit, b) ->
      putStrLn ("  " ++ maybe "" (\l -> "(limit " ++ show l ++ ") ") limit ++ hex b)
  where
    hex = concatMap (\w -> [digit (w `shiftR` 4), digit (w .&. 15)]) . B.unpack
    digit x = "0123456789abcdef" !! fromIntegral x

main :: IO ()
main = do
  n <- lookupEnv "NGHTTP2_SURVEY"
  case n of
    Just k -> survey (read k) >> exitSuccess
    Nothing -> pure ()
  f <- lookupEnv "NGHTTP2_FRAMES_SURVEY"
  case f of
    Just k -> frameSurvey (read k) >> exitSuccess
    Nothing -> pure ()
  defaultMain $ testGroup "against nghttp2"
    [ testProperty "the decoders agree on the same blocks" prop_decoders
    , testProperty "nghttp2 decodes what we encode" prop_oursToTheirs
    , testProperty "we decode what nghttp2 encodes" prop_theirsToOurs
    , testProperty "frames: whole sessions get the same verdicts, up to the first known difference" prop_sessions
    ]
