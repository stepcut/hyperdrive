{-# LANGUAGE OverloadedStrings #-}
-- | The driver in process, without sockets: N pipelined requests on one
-- connection through 'serveConnection', from a transport that makes
-- them as they are read and counts what is written. Prints the time and
-- the bytes allocated per request; allocation is exact whatever else
-- the machine is doing, time is not.
--
--   bench-driver [N] [browser | chunked]
--
-- The default request is wrk's (41 bytes, Host only); @browser@ sends
-- a browser's (499 bytes, 13 header fields), and @chunked@ a POST with
-- a chunked body of 16 chunks of 1 KiB (which the application does not
-- read, so the driver discards it, decoding every chunk). Pin it to a
-- core for timing: @taskset -c 2 bench-driver 300000 browser@.
module Main (main) where

import qualified Data.ByteString as B
import Data.IORef
import Data.Maybe (fromJust)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter, setAllocationCounter)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Status (mkStatus)
import Hyperdrive.Server
import Network.Socket (SockAddr(..))
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let n = case args of { (a : _) -> read a; _ -> 200000 }
      request = case drop 1 args of
        ["browser"] -> B.concat
          [ "GET /index.html?q=1 HTTP/1.1\r\nHost: www.example.com\r\n"
          , "User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0\r\n"
          , "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
          , "Accept-Language: en-US,en;q=0.5\r\nAccept-Encoding: gzip, deflate, br, zstd\r\n"
          , "Connection: keep-alive\r\nUpgrade-Insecure-Requests: 1\r\nSec-Fetch-Dest: document\r\n"
          , "Sec-Fetch-Mode: navigate\r\nSec-Fetch-Site: none\r\nSec-Fetch-User: ?1\r\nPriority: u=0, i\r\n"
          , "Cookie: session=0123456789abcdef; theme=dark\r\n\r\n" ]
        ["chunked"] -> B.concat
          ( "POST /upload HTTP/1.1\r\nHost: 127.0.0.1:18083\r\nTransfer-Encoding: chunked\r\n\r\n"
          : replicate 16 ("400\r\n" <> B.replicate 1024 0x78 <> "\r\n") ++ ["0\r\n\r\n"] )
        _ -> "GET / HTTP/1.1\r\nHost: 127.0.0.1:18083\r\n\r\n"
      body = fieldsFromList [(fromJust (mkFieldName "content-type"), either (error . show) id (mkFieldValue "text/plain"))]
      app _ _ = either (fail . show) pure (mkResponse (fromJust (mkStatus 200)) body (ContentBytes "Hello, World!"))
      settings = defaultSettings { settingsOnException = print }
  left <- newIORef (n :: Int)
  written <- newIORef (0 :: Int)
  let next = do
        k <- readIORef left
        if k == 0 then pure B.empty else writeIORef left (k - 1) >> pure (B.copy request)
      t = Transport
        { transportRecv = next
        , transportTryRecv = Just <$> next
        , transportSendMany = \bs -> modifyIORef' written (+ sum (map B.length bs))
        , transportOnReadable = \_ -> pure (pure ())
        , transportShutdownWrite = pure ()
        , transportAbort = pure ()
        , transportClose = pure () }
  setAllocationCounter 0
  t0 <- getMonotonicTimeNSec
  withTimers 100000 $ \timers -> serveConnection settings timers t (Context (SockAddrInet 0 0) (SockAddrInet 0 0) False) app
  t1 <- getMonotonicTimeNSec
  a <- getAllocationCounter
  out <- readIORef written
  putStrLn (show (B.length request) ++ "-byte request: " ++ show n ++ " requests, " ++ show (out `div` n) ++ " bytes out each: "
            ++ show (fromIntegral (t1 - t0) / fromIntegral n / 1000 :: Double) ++ " us and "
            ++ show (negate a `div` fromIntegral n) ++ " bytes allocated per request")
