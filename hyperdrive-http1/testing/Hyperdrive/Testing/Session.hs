{-# LANGUAGE OverloadedStrings #-}
-- | HTTP/1.1 sessions for tests: requests as a client sends them, and
-- scripts saying how the application answers each.
module Hyperdrive.Testing.Session
  ( Req(..)
  , BodySpec(..)
  , bodyBytes
  , render
  , genReq
  , splitInput
  , damage
  , Script(..)
  , genScript
  , readAll
  , allOrNone
  , fields
  , name
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List (sort)
import Data.Maybe (fromJust)
import Hyperdrive.Semantics.Field
import Numeric (showHex)
import Test.QuickCheck
------------------------------------------------------------------------------
-- requests, as a client sends them
------------------------------------------------------------------------------

data Req = Req
  { rMethod  :: ByteString
  , rPath    :: ByteString
  , rMinor   :: Int
  , rFields  :: [(ByteString, ByteString)]  -- ^ fields the application sees
  , rExpect  :: Bool
  , rClose   :: Bool                         -- ^ Connection: close
  , rBody    :: BodySpec
  } deriving (Show)

data BodySpec = NoBodySpec | LengthBody ByteString | ChunkedBody [ByteString] [(ByteString, ByteString)]
  deriving (Show)

bodyBytes :: BodySpec -> ByteString
bodyBytes NoBodySpec          = ""
bodyBytes (LengthBody b)      = b
bodyBytes (ChunkedBody cs _)  = B.concat cs

render :: Req -> ByteString
render r = B.concat $
  [ rMethod r, " ", rPath r, " HTTP/1.", C.pack (show (rMinor r)), "\r\n", "Host: a.example\r\n" ]
  ++ [ n <> ": " <> v <> "\r\n" | (n, v) <- rFields r ]
  ++ [ "Expect: 100-continue\r\n" | rExpect r ]
  ++ [ "Connection: close\r\n" | rClose r ]
  ++ case rBody r of
       NoBodySpec -> ["\r\n"]
       LengthBody b -> ["Content-Length: ", C.pack (show (B.length b)), "\r\n\r\n", b]
       ChunkedBody cs ts ->
         ["Transfer-Encoding: chunked\r\n\r\n"]
         ++ concat [ [C.pack (showHex (B.length c) ""), "\r\n", c, "\r\n"] | c <- cs, not (B.null c) ]
         ++ ["0\r\n"] ++ [ n <> ": " <> v <> "\r\n" | (n, v) <- ts ] ++ ["\r\n"]

genReq :: Gen Req
genReq = do
  minor <- frequency [(5, pure 1), (1, pure 0)]
  method <- elements ["GET", "POST", "PUT", "HEAD", "DELETE", "X-Custom"]
  path <- elements ["/", "/a/b", "/x?y=1", "/%41"]
  fs <- listOf (elements [("Accept", "*/*"), ("X-A", "1"), ("Cookie", "a=b"), ("X-Empty", "")])
  expect <- frequency [(5, pure False), (1, pure True)]
  close <- frequency [(6, pure False), (1, pure True)]
  body <- frequency
    [ (3, pure NoBodySpec)
    , (2, LengthBody <$> bytes)
    , (if minor == 1 then 2 else 0, ChunkedBody <$> listOf bytes <*> elements [[], [("X-T", "1")]]) ]
  pure (Req method path minor fs (expect && minor == 1) close body)
  where bytes = resize 40 (B.pack <$> listOf arbitrary)

-- | The input as the network delivers it: in pieces.
splitInput :: ByteString -> Gen [ByteString]
splitInput s = do
  cuts <- sort <$> listOf (choose (0, B.length s))
  pure (filter (not . B.null) (zipWith (\a b -> B.take (b - a) (B.drop a s)) (0 : cuts) (cuts ++ [B.length s])))

------------------------------------------------------------------------------
-- the application, as a script
------------------------------------------------------------------------------

data Script = Script
  { sReads    :: Maybe Int   -- ^ Nothing: read the whole body; Just k: ask for at most k chunks
  , sStatus   :: Int
  , sContent  :: [ByteString]
  , sDeclared :: Bool        -- ^ give the length in advance
  , sTrailers :: [(ByteString, ByteString)]
  } deriving (Show)

genScript :: Gen Script
genScript = do
  reads' <- frequency [(4, pure Nothing), (1, Just <$> choose (0, 2))]
  status <- elements [200, 200, 201, 204, 304, 404, 500]
  content <- if status `elem` [204, 304] then pure [] else listOf (resize 40 (B.pack <$> listOf arbitrary))
  declared <- arbitrary
  trailers <- if status `elem` [204, 304] then pure [] else frequency [(3, pure []), (1, pure [("X-Checksum", "abc")])]
  pure (Script reads' status content declared trailers)

readAll :: Script
readAll = Script Nothing 200 ["ok"] True []

fields :: [(ByteString, ByteString)] -> Fields
fields fs = fieldsFromList [ (name n, either (error . show) id (mkFieldValue v)) | (n, v) <- fs ]

name :: ByteString -> FieldName
name = fromJust . mkFieldName


-- | How much a script that reads part of a body gets depends on how the
-- body arrived, so these compare scripts that read all or none of it.
allOrNone :: Script -> Script
allOrNone sc = sc { sReads = fmap (const 0) (sReads sc) }

-- | Up to three bytes changed, inserted or removed.
damage :: ByteString -> Gen ByteString
damage s0 = do
  k <- choose (0, 3 :: Int)
  foldr (\_ g -> g >>= once') (pure s0) [1 .. k]
  where
    once' s = do
      i <- choose (0, max 0 (B.length s - 1))
      b <- elements (B.unpack "\r\n :;0aG\t\0\128")
      op <- elements [0, 1, 2 :: Int]
      pure $ case op of
        0 -> B.take i s <> B.singleton b <> B.drop (i + 1) s
        1 -> B.take i s <> B.singleton b <> B.drop i s
        _ -> B.take i s <> B.drop (i + 1) s
