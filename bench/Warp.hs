{-# LANGUAGE OverloadedStrings #-}
-- | The benchmark application on warp: the same routes as Hyperdrive.hs.
module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment (getArgs)

main :: IO ()
main = do
  [port] <- getArgs
  runSettings (setPort (read port) (setHost "127.0.0.1" defaultSettings)) app

large :: L.ByteString
large = L.fromStrict (B.replicate (1024 * 1024) 120)

app :: Application
app req respond = case (requestMethod req, rawPathInfo req) of
  ("GET", "/")      -> respond (responseLBS status200 textPlain "Hello, World!")
  ("GET", "/large") -> respond (responseLBS status200 textPlain large)
  ("POST", "/upload") -> do
    let loop n = do
          c <- getRequestBodyChunk req
          if B.null c then pure n else loop (n + B.length c)
    n <- loop (0 :: Int)
    respond (responseLBS status200 textPlain (L.fromStrict (C.pack (show n))))
  _ -> respond (responseLBS status404 [] "")
  where textPlain = [("Content-Type", "text/plain")]
