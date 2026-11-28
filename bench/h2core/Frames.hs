{-# LANGUAGE OverloadedStrings #-}
-- Allocation per DATA frame of the HTTP/2 core: S streams, each a GET
-- answered with W writes of 16 KiB, the client's windows wide open, the
-- transport writing everything at once.
module Main (main) where

import qualified Data.ByteString as B
import Data.List (foldl')
import Data.Maybe (fromJust)
import GHC.Conc (getAllocationCounter, setAllocationCounter)
import Hyperdrive.HPACK.Encode
import Hyperdrive.HTTP2.Connection
import Hyperdrive.HTTP2.Frame
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Status (mkStatus)
import System.Environment (getArgs)

main :: IO ()
main = do
  [s, w] <- map read <$> getArgs
  let sids = [1, 3 .. 2 * s - 1] :: [Int]
      blocks = go (newEncoder 4096) sids
      go _ [] = []
      go e (_ : rest) = let (b, e') = encodeBlock e [(":method", "GET"), (":scheme", "http"), (":authority", "a"), (":path", "/")] in b : go e' rest
      client = B.concat (connectionPreface : renderSettings [InitialWindowSize 0x7FFFFFFF, MaxFrameSize 16384]
                         : renderWindowUpdate (StreamId 0) 0x7FFF0000
                         : concat [ renderHeaders 16384 (StreamId (fromIntegral i)) True b | (i, b) <- zip sids blocks ])
      chunk = B.replicate 16384 0x61
      ok = ResponseHead (fromJust (mkStatus 200)) (fieldsFromList []) True Nothing
      feed (c, frames) i = let (c', outs) = step 0 i c
                               n = sum [B.length b | Send bs <- outs, b <- bs]
                               (c'', _) = if n > 0 then step 0 (Sent n) c' else (c', [])
                           in c'' `seq` (c'', frames + length [() | Send bs <- outs, _ <- bs])
      (c0, _) = newConn defaultConfig 0
      (c1, _) = foldl' feed (c0, 0) [Received client]
      (c2, _) = foldl' feed (c1, 0) [Respond (StreamId (fromIntegral i)) ok | i <- sids]
      inputs = concat (replicate w [WriteBody (StreamId (fromIntegral i)) chunk | i <- sids])
  c2 `seq` setAllocationCounter 0
  before <- getAllocationCounter
  let (c3, pieces) = foldl' feed (c2, 0 :: Int) inputs
  c3 `seq` pieces `seq` pure ()
  after <- getAllocationCounter
  let frames = s * w
  putStrLn (show s ++ " streams, " ++ show frames ++ " DATA frames: " ++ show ((before - after) `div` fromIntegral frames) ++ " bytes a frame")
