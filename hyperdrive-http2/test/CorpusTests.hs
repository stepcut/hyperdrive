{-# LANGUAGE OverloadedStrings #-}
-- | The hpack-test-case corpus (github.com/http2jp/hpack-test-case):
-- real header sets, encoded by several HPACK implementations. Its
-- path comes from HPACK_TEST_CASE, which the flake sets.
module CorpusTests (corpusTests) where

import Control.Monad (forM)
import Data.Aeson (FromJSON(..), eitherDecodeFileStrict, withObject, (.:), (.:?))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value(..))
import qualified Data.ByteString.Base16 as Hex
import Data.ByteString (ByteString)
import Data.List (isPrefixOf, sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Hyperdrive.HPACK.Decode
import Hyperdrive.HPACK.Encode
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

newtype Story = Story [Case]

data Case = Case
  { caseTableSize :: Maybe Int
  , caseWire      :: Maybe ByteString
  , caseHeaders   :: [(ByteString, ByteString)]
  }

instance FromJSON Story where
  parseJSON = withObject "story" $ \o -> Story <$> o .: "cases"

instance FromJSON Case where
  parseJSON = withObject "case" $ \o -> do
    size <- o .:? "header_table_size"
    wire <- o .:? "wire"
    headers <- o .: "headers"
    pairs <- forM headers $ \h -> case h of
      Object m | [(k, String v)] <- KM.toList m -> pure (utf8 (K.toText k), utf8 v)
      _ -> fail "a header is not one name and one value"
    wire' <- traverse (either fail pure . Hex.decode . utf8) wire
    pure (Case size wire' pairs)
    where utf8 = T.encodeUtf8 :: T.Text -> ByteString

-- | Every story of every implementation's directory, and of raw-data.
loadCorpus :: IO ([(String, [(String, Story)])], [(String, Story)])
loadCorpus = do
  root <- lookupEnv "HPACK_TEST_CASE"
    >>= maybe (fail "HPACK_TEST_CASE is not set: run the tests in `nix develop`") pure
  dirs <- sort <$> listDirectory root
  impls <- fmap concat $ forM dirs $ \d -> do
    isDir <- doesDirectoryExist (root </> d)
    if not isDir || d `elem` ["raw-data", "util"] || "." `isPrefixOf` d then pure []
      else (\s -> [(d, s)]) <$> stories (root </> d)
  raw <- stories (root </> "raw-data")
  pure (impls, raw)
  where
    stories dir = do
      files <- sort . filter ("story_" `isPrefixOf`) <$> listDirectory dir
      forM files $ \f -> do
        s <- eitherDecodeFileStrict (dir </> f) >>= either fail pure
        pure (f, s)

corpusTests :: IO TestTree
corpusTests = do
  (impls, raw) <- loadCorpus
  pure $ testGroup "The hpack-test-case corpus"
    [ testGroup "decoding each implementation's encodings"
        [ testProperty d $ once $
            counterexample "no stories" (not (null ss))
            .&&. conjoin [counterexample f (decodeStory s) | (f, s) <- ss]
        | (d, ss) <- impls ]
    , testProperty "encoding raw-data's header sets, then decoding them" $ once $
        counterexample "no stories" (not (null raw))
          .&&. conjoin [counterexample f (roundTripStory s) | (f, s) <- raw]
    ]

-- the cases of a story share one decoder
decodeStory :: Story -> Property
decodeStory (Story cases) = go (newDecoder 4096 4096) (zip [0 :: Int ..] cases)
  where
    go _ [] = property True
    go d ((i, c) : rest) =
      let d1 = maybe d (`setDecoderLimit` d) (caseTableSize c)
      in case caseWire c of
           Nothing -> counterexample ("case " ++ show i ++ " has no wire") False
           Just wire -> case decodeBlock maxBound d1 wire of
             Left err -> counterexample ("case " ++ show i ++ ": " ++ show err) False
             Right (decoded, d') ->
               counterexample ("case " ++ show i) (decoded === Fields (caseHeaders c))
               .&&. go d' rest

roundTripStory :: Story -> Property
roundTripStory (Story cases) = go (newEncoder 4096) (newDecoder 4096 4096) cases
  where
    go _ _ [] = property True
    go e d (c : rest) =
      let (block, e') = encodeBlock e (caseHeaders c)
      in case decodeBlock maxBound d block of
           Left err -> counterexample (show err) False
           Right (decoded, d') -> decoded === Fields (caseHeaders c) .&&. go e' d' rest
