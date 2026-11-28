{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | TLS in the server (docs/tls-design.md, T10): a client from
-- hyperdrive-tls over a real socket.
module TLSTests (tlsTests) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.Maybe (fromJust)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperdrive.Semantics.Field (emptyFields)
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Status (mkStatus)
import Hyperdrive.Server
import qualified Hyperdrive.TLS as TLS
import Network.Socket
import qualified Network.Socket.ByteString as SB
import System.Timeout (timeout)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

tlsTests :: TestTree
tlsTests = testGroup "TLS (docs/tls-design.md)"
  [ testProperty "HTTP/1.1 over TLS; the context says secure" (once prop_http1)
  , testProperty "ALPN h2: HTTP/2 over TLS" (once prop_h2)
  , testProperty "h2 alone, HTTP/2 off: no protocol in common" (once prop_h2Off)
  , testProperty "the handshake timeout is total, from accept (T4)" (once prop_handshakeTimeout)
  , testProperty "plaintext HTTP to a TLS listener: closed, no response; a bad record: an alert" (once prop_plaintext)
  , testProperty "a key that is not the certificate's fails the start" (once prop_badConfig)
  , testProperty "a large response to a slow reader arrives whole" (once prop_large)
  ]

tlsSettings :: Settings
tlsSettings = defaultSettings
  { settingsHost = Just "127.0.0.1", settingsPort = 0
  , settingsTimerResolution = 10000
  , settingsTLS = Just (defaultTLSSettings (FromFile "test/data/ec.pem") (FromFile "test/data/ec.key"))
  , settingsOnException = \_ -> pure () }

-- the body says whether the connection was secure
secureApp :: Application
secureApp ctx _ = either (throwIO . userError . show) pure
  (mkResponse (fromJust (mkStatus 200)) emptyFields (ContentBytes (C.pack ("secure=" ++ show (contextSecure ctx)))))

largeApp :: Application
largeApp _ _ = either (throwIO . userError . show) pure
  (mkResponse (fromJust (mkStatus 200)) emptyFields (ContentBytes (B.replicate 1048576 0x61)))

-- | A TLS client: connect, handshake over the socket.
withTLSClient :: PortNumber -> [ByteString] -> (Socket -> TLS.Session -> IO a) -> IO (Either TLS.TLSError a)
withTLSClient port alpn act = do
  cfg <- TLS.newConfig (TLS.defaultConfigSettings TLS.Client) { TLS.csALPN = alpn }
           >>= either (fail . show) pure
  addr : _ <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just "127.0.0.1") (Just (show port))
  bracket (openSocket addr) close $ \s -> do
    connect s (addrAddress addr)
    sess <- TLS.newSession cfg (Just "localhost") 1048576
    let shake = do
          r <- TLS.handshake sess
          out <- TLS.takeOutput sess
          if B.null out then pure () else SB.sendAll s out
          case r of
            Left e -> pure (Left e)
            Right TLS.Done -> Right <$> act s sess
            Right TLS.WantInput -> do
              b <- SB.recv s 65536 `catch` \(_ :: IOException) -> pure ""
              if B.null b then pure (Left (TLS.TLSError "closed during the handshake"))
                else TLS.feed sess b >>= either (pure . Left) (const shake)
    shake

sendPlain :: Socket -> TLS.Session -> ByteString -> IO ()
sendPlain s sess b = do
  _ <- TLS.send sess b
  TLS.takeOutput sess >>= SB.sendAll s

-- | Plaintext until the server closes (or five seconds pass), reading
-- at the given pace.
recvPlainAll :: Int -> Socket -> TLS.Session -> IO ByteString
recvPlainAll pause s sess = fst <$> recvPlainEnd pause s sess

-- | The same, and whether it ended with the server's close_notify (not
-- just the socket's end).
recvPlainEnd :: Int -> Socket -> TLS.Session -> IO (ByteString, Bool)
recvPlainEnd pause s sess = fmap (maybe ("<timed out>", False) id) $ timeout 5000000 $ go []
  where
    go acc = TLS.receive sess 16384 >>= \r -> case r of
      TLS.Plain b -> go (b : acc)
      TLS.NeedInput -> do
        threadDelay pause
        b <- SB.recv s 65536 `catch` \(_ :: IOException) -> pure ""
        if B.null b then done acc False else TLS.feed sess b >> go acc
      TLS.PeerClosed -> done acc True
      _ -> done acc False
    done acc clean = pure (B.concat (reverse acc), clean)

request :: ByteString
request = "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"

prop_http1 :: Property
prop_http1 = ioProperty $ withServer tlsSettings secureApp $ \server -> do
  r <- withTLSClient (serverPort server) ["http/1.1"] $ \s sess -> do
    a <- TLS.agreed sess
    sendPlain s sess request
    (body, clean) <- recvPlainEnd 0 s sess
    pure (TLS.agreedALPN a, B.take 15 body, "secure=True" `B.isSuffixOf` body, clean)
  -- the server's close_notify before its socket's end (T6)
  pure (r === Right (Just "http/1.1", "HTTP/1.1 200 OK", True, True))

prop_h2 :: Property
prop_h2 = ioProperty $ withServer tlsSettings secureApp $ \server -> do
  r <- withTLSClient (serverPort server) ["h2", "http/1.1"] $ \s sess -> do
    a <- TLS.agreed sess
    -- the client's preface and an empty SETTINGS
    sendPlain s sess ("PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" <> B.pack [0, 0, 0, 4, 0, 0, 0, 0, 0])
    let first = TLS.receive sess 16384 >>= \x -> case x of
          TLS.Plain b -> pure b
          TLS.NeedInput -> SB.recv s 65536 >>= TLS.feed sess >> first
          _ -> pure ""
    b <- first
    -- the server's preface: a SETTINGS frame (type 4)
    pure (TLS.agreedALPN a, B.length b >= 9 && B.index b 3 == 4)
  pure (r === Right (Just "h2", True))

prop_h2Off :: Property
prop_h2Off = ioProperty $ withServer tlsSettings { settingsHTTP2 = Nothing } secureApp $ \server -> do
  r <- withTLSClient (serverPort server) ["h2"] $ \_ _ -> pure ()
  pure (counterexample (show r) (either (const True) (const False) r))

prop_handshakeTimeout :: Property
prop_handshakeTimeout = ioProperty $ do
  let settings = tlsSettings { settingsTimeouts = (settingsTimeouts tlsSettings) { timeoutHandshake = 300000 } }
  withServer settings secureApp $ \server -> do
    -- nothing at all
    (silent, quiet) <- timed $ withPlainClient (serverPort server) recvToClose
    -- a ClientHello trickled an octet every 20 ms: the deadline is total
    hello <- clientHello
    (trickle, slow) <- timed $ withPlainClient (serverPort server) $ \s -> do
      gone <- newIORef False
      let go b | B.null b = pure ()
               | otherwise = do
                   ok <- (SB.sendAll s (B.take 1 b) >> pure True) `catch` \(_ :: IOException) -> pure False
                   if ok then threadDelay 20000 >> go (B.drop 1 b) else writeIORef gone True
      go hello
      readIORef gone
    pure $ counterexample (show (silent, quiet, trickle, slow)) $
      property (quiet >= 0.25 && quiet < 2 && silent == "")
      .&&. property (trickle && slow < 3)

prop_plaintext :: Property
prop_plaintext = ioProperty $ withServer tlsSettings secureApp $ \server -> do
  out <- withPlainClient (serverPort server) $ \s -> SB.sendAll s request >> recvToClose s
  -- a handshake record that does not parse
  bad <- withPlainClient (serverPort server) $ \s ->
    SB.sendAll s (B.pack ([0x16, 0x03, 0x01, 0x00, 0x08] ++ replicate 8 0xEE)) >> recvToClose s
  -- never an HTTP response (AWS-LC recognizes a plaintext request and
  -- closes without an alert); for a bad handshake record, an alert
  pure $ counterexample (show (out, bad)) $
    not ("HTTP/" `B.isInfixOf` out) && B.length out < 64 && B.take 1 bad == "\x15"

prop_badConfig :: Property
prop_badConfig = ioProperty $ do
  r <- try (withServer tlsSettings { settingsTLS = Just (defaultTLSSettings (FromFile "test/data/ec.pem") (FromFile "test/data/other.key")) }
                       secureApp (\_ -> pure ()))
  pure (counterexample (show r) (either (\(_ :: IOException) -> True) (const False) r))

prop_large :: Property
prop_large = ioProperty $ withServer tlsSettings largeApp $ \server -> do
  r <- withTLSClient (serverPort server) ["http/1.1"] $ \s sess -> do
    sendPlain s sess request
    body <- recvPlainAll 2000 s sess
    let (_, rest) = B.breakSubstring "\r\n\r\n" body
    pure (B.length rest - 4)
  pure (r === Right 1048576)

withPlainClient :: PortNumber -> (Socket -> IO a) -> IO a
withPlainClient port act = do
  addr : _ <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just "127.0.0.1") (Just (show port))
  bracket (openSocket addr) close $ \s -> connect s (addrAddress addr) >> act s

recvToClose :: Socket -> IO ByteString
recvToClose s = fmap (maybe "<timed out>" id) $ timeout 5000000 $ go []
  where
    go acc = do
      b <- SB.recv s 65536 `catch` \(_ :: IOException) -> pure ""
      if B.null b then pure (B.concat (reverse acc)) else go (b : acc)

timed :: IO a -> IO (a, Double)
timed act = do
  t0 <- getMonotonicTimeNSec
  a <- act
  t1 <- getMonotonicTimeNSec
  pure (a, fromIntegral (t1 - t0) / 1e9)

-- a client's first flight, from a session that is not used further
clientHello :: IO ByteString
clientHello = do
  cfg <- TLS.newConfig (TLS.defaultConfigSettings TLS.Client) >>= either (fail . show) pure
  sess <- TLS.newSession cfg (Just "localhost") 65536
  _ <- TLS.handshake sess
  TLS.takeOutput sess
