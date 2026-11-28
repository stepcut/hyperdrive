{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- | TLS in the server (docs/tls-design.md): the listener's configuration,
-- the handshake, and a 'Transport' carrying plaintext over the socket's
-- (T1). A session is used only from its connection's I/O thread (T5).
module Hyperdrive.Server.TLS
  ( TLSSettings(..)
  , CertificateSource(..)
  , defaultTLSSettings
  , TLSServer
  , newTLSServer
  , handshakeTLS
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception (bracket_, throwIO)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import GHC.Conc (getNumCapabilities)
import Hyperdrive.Server.Timer
import Hyperdrive.Server.Transport
import Hyperdrive.Server.Wait (waitFor)
import qualified Hyperdrive.TLS as TLS

-- | Where a certificate chain or a key comes from.
data CertificateSource = FromFile !FilePath | FromPEM !ByteString
  deriving (Show)

-- | A TLS listener's settings (T3).
data TLSSettings = TLSSettings
  { tlsCertificates :: !CertificateSource
    -- ^ the certificate chain, PEM, the leaf first
  , tlsKey          :: !CertificateSource
    -- ^ its key, PEM, not encrypted
  , tlsGroups       :: !ByteString
    -- ^ key-exchange groups in our order of preference
  , tlsMinVersion   :: !TLS.Version
  , tlsMaxHandshakeSteps :: !(Maybe Int)
    -- ^ handshake steps run at once (each a @safe@ call doing CPU-bound
    -- work, an RSA signature about 0.36 ms; T4); 'Nothing': two for
    -- each capability
  } deriving (Show)

defaultTLSSettings :: CertificateSource -> CertificateSource -> TLSSettings
defaultTLSSettings certs key = TLSSettings
  { tlsCertificates = certs
  , tlsKey = key
  , tlsGroups = "X25519MLKEM768:X25519:P-256"
  , tlsMinVersion = TLS.TLS12
  , tlsMaxHandshakeSteps = Nothing
  }

-- | A listener's TLS, checked and loaded.
data TLSServer = TLSServer
  { tsConfig :: !TLS.Config
  , tsSteps  :: !QSem
  }

-- | Load and check the listener's TLS configuration; throws if it is
-- wrong (a key that is not the certificate's, an encrypted key). With
-- HTTP/2, @h2@ is offered before @http/1.1@ (T2).
newTLSServer :: TLSSettings -> Bool -> IO TLSServer
newTLSServer s http2 = do
  certs <- load (tlsCertificates s)
  key <- load (tlsKey s)
  r <- TLS.newConfig (TLS.defaultConfigSettings TLS.Server)
         { TLS.csCertificate = Just (certs, key)
         , TLS.csALPN = ["h2" | http2] ++ ["http/1.1"]
         , TLS.csGroups = Just (tlsGroups s)
         , TLS.csVersions = (tlsMinVersion s, TLS.TLS13) }
  config <- either (\(TLS.TLSError e) -> throwIO (userError ("TLS configuration: " ++ e))) pure r
  steps <- maybe ((* 2) <$> getNumCapabilities) pure (tlsMaxHandshakeSteps s)
  TLSServer config <$> newQSem (max 1 steps)
  where
    load (FromFile f) = B.readFile f
    load (FromPEM b) = pure b

-- | Ciphertext received and not yet processed, at most (T1): a receive of
-- up to 64 KiB, and a record.
maxBuffered :: Int
maxBuffered = 131072

-- | Plaintext encrypted in one 'transportTrySend' at most (T1).
maxEncrypted :: Int
maxEncrypted = 65536

-- | The handshake over the socket's transport, within the deadline (the
-- handshake timeout, from accept: T4) and the cap on steps at once:
-- the plaintext transport and the protocol agreed by ALPN, or 'Nothing'
-- if the handshake failed, the client went, the deadline passed or the
-- server is shutting down. A failure's alert is sent, best effort; a
-- timeout closes without one.
handshakeTLS :: TLSServer -> Timer -> MVar Wake -> Transport -> Word64 -> IO (Maybe (Transport, Maybe ByteString))
handshakeTLS server timer wake sock deadline = do
  sess <- TLS.newSession (tsConfig server) Nothing maxBuffered
  let wait readable writable = waitFor timer wake sock readable writable deadline True (pure False)
      -- all of the session's output, without blocking past the deadline
      flush = do
        out <- TLS.takeOutput sess
        unless (B.null out) (sendAll out)
      sendAll out = do
        n <- transportTrySend sock [out]
        when (n < B.length out) $ wait False True >>= \case
          WakeWritable -> sendAll (B.drop n out)
          _ -> ioError (userError "TLS handshake: no progress")
      step = do
        r <- bracket_ (waitQSem (tsSteps server)) (signalQSem (tsSteps server)) (TLS.handshake sess)
        case r of
          Left _ -> Nothing <$ flush
          Right TLS.Done -> do
            flush
            a <- TLS.agreed sess
            t <- tlsTransport sess sock
            pure (Just (t, TLS.agreedALPN a))
          Right TLS.WantInput -> flush >> input
      input = transportTryRecv sock >>= \case
        Just bs | B.null bs -> pure Nothing
                | otherwise -> TLS.feed sess bs >>= either (const (pure Nothing)) (const step)
        Nothing -> wait True False >>= \case
          WakeReadable -> input
          _ -> pure Nothing
  step

-- | The plaintext transport over the socket's, once the handshake is done.
tlsTransport :: TLS.Session -> Transport -> IO Transport
tlsTransport sess sock = do
  -- ciphertext the socket has not taken yet, each piece with the plaintext
  -- it carries; plaintext whose ciphertext has all left, not yet reported
  pending <- newIORef (Seq.empty :: Seq (ByteString, Int))
  reportable <- newIORef (0 :: Int)
  let -- what reads and handshake messages left to send joins the queue
      queueOutput plain = do
        out <- TLS.takeOutput sess
        unless (B.null out) (modifyIORef' pending (|> (out, plain)))
      -- send what the socket takes now; True if nothing is left
      push = do
        q <- readIORef pending
        case viewl q of
          EmptyL -> pure True
          (c, plain) :< rest -> do
            n <- transportTrySend sock [c]
            if n == B.length c
              then writeIORef pending rest >> modifyIORef' reportable (+ plain) >> push
              else do
                when (n > 0) (writeIORef pending ((B.drop n c, plain) Seq.<| rest))
                pure False
      -- the same, waiting for the socket (the driver's write timeout
      -- aborts it if the client does not read)
      pushAll = do
        q <- readIORef pending
        writeIORef pending Seq.empty
        let cs = [ c | (c, _) <- foldr (:) [] q ]
        unless (null cs) (transportSendMany sock cs)
        modifyIORef' reportable (+ sum [ p | (_, p) <- foldr (:) [] q ])
      -- plaintext octets at the front of what the driver offers that are
      -- already encrypted
      ahead = do
        q <- readIORef pending
        r <- readIORef reportable
        pure (r + sum [ p | (_, p) <- foldr (:) [] q ])
      -- one record's worth of plaintext from the session, receiving from
      -- the socket only when it has none (T1)
      tryRecv recvSock = TLS.receive sess 16384 >>= \case
        TLS.Plain b -> pure (Just b)
        TLS.PeerClosed -> pure (Just B.empty)
        TLS.ReceiveFailed _ -> queueOutput 0 >> push >> pure (Just B.empty)
        TLS.NeedInput -> do
          queueOutput 0
          _ <- push
          recvSock >>= \case
            Nothing -> pure Nothing
            Just c | B.null c -> pure (Just B.empty)
                   | otherwise -> TLS.feed sess c >>= \case
                       Left _ -> pure (Just B.empty)
                       Right () -> TLS.receive sess 16384 >>= \case
                         TLS.Plain b -> pure (Just b)
                         TLS.NeedInput -> pure Nothing
                         TLS.PeerClosed -> pure (Just B.empty)
                         TLS.ReceiveFailed _ -> queueOutput 0 >> push >> pure (Just B.empty)
      -- records into one string, up to the cap (the plaintext copied once
      -- from the library, the whole allocation kept: C3)
      tryRecvLarge cap = tryRecv (transportTryRecvLarge sock cap) >>= \case
        Just b | not (B.null b) -> Just . B.concat . (b :) <$> more (cap - B.length b)
        other -> pure other
      more room
        | room < 16384 = pure []
        | otherwise = TLS.receive sess 16384 >>= \case
            TLS.Plain b -> (b :) <$> more (room - B.length b)
            _ -> pure []
      -- encrypt what the driver offers beyond what is already encrypted
      encrypt limit pieces = do
        skip <- ahead
        let fresh = take' limit (drop' skip pieces)
            n = sum (map B.length fresh)
        unless (n == 0) $ do
          mapM_ (\p -> TLS.send sess p >>= either (\(TLS.TLSError e) -> ioError (userError ("TLS: " ++ e))) pure) fresh
          queueOutput n
      trySend pieces = do
        done <- push
        when done $ do
          encrypt maxEncrypted pieces
          _ <- push
          pure ()
        r <- readIORef reportable
        writeIORef reportable 0
        pure r
      sendMany pieces = do
        pushAll
        encrypt maxBound pieces
        pushAll
        writeIORef reportable 0
  pure Transport
    { transportRecv = do
        let loop = tryRecv (Just <$> transportRecv sock) >>= maybe loop pure
        loop
    , transportTryRecv = tryRecv (transportTryRecv sock)
    , transportTryRecvLarge = tryRecvLarge
    , transportSendMany = sendMany
    , transportOnReadable = transportOnReadable sock
    , transportTrySend = trySend
    , transportOnWritable = transportOnWritable sock
    , transportShutdownWrite = do
        -- close_notify, then the socket's half-close (T6)
        _ <- TLS.closeNotify sess
        queueOutput 0
        pushAll
        transportShutdownWrite sock
    , transportAbort = transportAbort sock
    , transportClose = transportClose sock
    }

-- the pieces after the first n octets
drop' :: Int -> [ByteString] -> [ByteString]
drop' _ [] = []
drop' n (b : bs)
  | n <= 0 = b : bs
  | B.length b <= n = drop' (n - B.length b) bs
  | otherwise = B.drop n b : bs

-- the first n octets of the pieces
take' :: Int -> [ByteString] -> [ByteString]
take' _ [] = []
take' n (b : bs)
  | n <= 0 = []
  | B.length b <= n = b : take' (n - B.length b) bs
  | otherwise = [B.take n b]
