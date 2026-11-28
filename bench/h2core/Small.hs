{-# LANGUAGE OverloadedStrings #-}
-- Allocation per small request: N GETs one after another on a
-- connection, each answered with 13 octets, the transport writing all.
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
  [n] <- map read <$> getArgs
  let fields = [(":method", "GET"), (":scheme", "http"), (":authority", "localhost"), (":path", "/"), ("user-agent", "h2load")]
      (block0, enc0) = encodeBlock (newEncoder 4096) fields
      block = fst (encodeBlock enc0 fields)   -- all indexed: the same every time after the first
      ok = ResponseHead (fromJust (mkStatus 200)) (fieldsFromList []) True (Just 13)
      feed (c, n') i = let (c', outs) = step 0 i c
                           k = sum [B.length b | Send bs <- outs, b <- bs]
                           n'' = n' + length [() | NewRequest _ _ <- outs]
                       in n'' `seq` if k > 0 then (fst (step 0 (Sent k) c'), n'') else (c', n'')
      request c sid = foldl' feed c
        [ Received (B.concat (renderHeaders 16384 (StreamId sid) True (if sid == 1 then block0 else block)))
        , Respond (StreamId sid) ok, WriteBody (StreamId sid) "Hello, World!", EndBody (StreamId sid) (fieldsFromList [])
        , HandlerDone (StreamId sid) ]
      c0 = feed (fst (newConn defaultConfig { configMaxRequests = 0 } 0), 0) (Received (connectionPreface <> renderSettings [] <> renderWindowUpdate (StreamId 0) 0x7FFF0000))
      c1 = foldl' request c0 [1, 3, 5, 7]
  snd c1 `seq` setAllocationCounter 0
  before <- getAllocationCounter
  let c2 = foldl' request c1 [9, 11 .. fromIntegral (2 * n + 7)]
  snd c2 `seq` pure ()
  after <- getAllocationCounter
  putStrLn (show n ++ " requests, " ++ show (snd c2) ++ " passed on: " ++ show ((before - after) `div` fromIntegral n) ++ " bytes a request")
