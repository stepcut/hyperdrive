{-# LANGUAGE OverloadedStrings #-}
-- | The benchmark application on hyperdrive: GET / is a small response,
-- GET /large 1 MiB, and POST /upload reads the body and answers with
-- its length. A second argument, @pin@, turns 'settingsPinConnections'
-- on. With TLS_CERT and TLS_KEY set (PEM files), it serves TLS
-- (docs/tls-design.md, T8); TLS_GROUPS replaces the key-exchange groups
-- (X25519 alone, say, as warp-tls offers no X25519MLKEM768).
module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Method (methodBytes)
import Hyperdrive.Semantics.Status (mkStatus)
import Hyperdrive.Semantics.Target (Target(..))
import Hyperdrive.Semantics.URI (pathBytes)
import Hyperdrive.Server
import System.Environment (getArgs, lookupEnv)

main :: IO ()
main = do
  port : options <- getArgs
  cert <- lookupEnv "TLS_CERT"
  key <- lookupEnv "TLS_KEY"
  groups <- lookupEnv "TLS_GROUPS"
  let tls = (\t -> maybe t (\g -> t { tlsGroups = C.pack g }) groups)
              <$> (defaultTLSSettings <$> (FromFile <$> cert) <*> (FromFile <$> key))
  runServer defaultSettings
    { settingsPort = fromIntegral (read port :: Int), settingsHost = Just "127.0.0.1"
    , settingsPinConnections = options == ["pin"]
    , settingsTLS = tls } app

large :: B.ByteString
large = B.replicate (1024 * 1024) 120

app :: Application
app _ req = case (methodBytes (requestMethod req), path (requestTarget req)) of
  ("GET", "/")      -> respond (ContentBytes "Hello, World!")
  ("GET", "/large") -> respond (ContentBytes large)
  ("POST", "/upload") -> do
    let loop n = do
          c <- readChunk (requestBody req)
          if B.null c then pure n else loop (n + B.length c)
    n <- loop 0
    respond (ContentBytes (C.pack (show n)))
  _ -> either (fail . show) pure (mkResponse (fromJust (mkStatus 404)) emptyFields (ContentBytes ""))
  where
    path (TargetURI _ _ p _) = pathBytes p
    path _                   = ""
    respond c = either (fail . show) pure (mkResponse (fromJust (mkStatus 200)) textPlain c)
    textPlain = fieldsFromList [(fromJust (mkFieldName "content-type"), either (error . show) id (mkFieldValue "text/plain"))]
