{-# LANGUAGE OverloadedStrings #-}
-- | TLS for hyperdrive over AWS-LC (docs/tls-design.md): configurations
-- and sessions that are freed when no longer referenced (on every path,
-- exceptions included), errors as values, ciphertext and plaintext
-- through buffers. A session is not safe to use from two threads at
-- once (T1, T5); a configuration is.
module Hyperdrive.TLS
  ( -- * Configuration
    Role(..)
  , Version(..)
  , ConfigSettings(..)
  , defaultConfigSettings
  , Config
  , newConfig
  , TLSError(..)
    -- * Sessions
  , Session
  , newSession
  , Step(..)
  , handshake
  , feed
  , takeOutput
  , pendingOutput
  , Received(..)
  , receive
  , send
  , closeNotify
  , keyUpdate
  , Agreed(..)
  , agreed
  , resumable
    -- * QUIC
  , quicSetTransportParams
  , quicProvide
  , quicTake
  , Secret(..)
  , quicSecrets
  , quicAlert
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word16)
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import qualified Hyperdrive.TLS.Shim as C

data Role = Server | Client
  deriving (Eq, Show)

data Version = TLS12 | TLS13
  deriving (Eq, Ord, Show)

versionCode :: Version -> CInt
versionCode TLS12 = 0x0303
versionCode TLS13 = 0x0304

-- | A configuration: one for each listener (or each test client).
data ConfigSettings = ConfigSettings
  { csRole        :: !Role
  , csQuic        :: !Bool                         -- ^ TLS 1.3 only, for QUIC
  , csCertificate :: !(Maybe (ByteString, ByteString))
    -- ^ the certificate chain (leaf first) and the key, as PEM; required
    -- for a server; an encrypted key is refused
  , csALPN        :: ![ByteString]
    -- ^ a server's protocols in its order of preference, or a client's offer
  , csGroups      :: !(Maybe ByteString)
    -- ^ key-exchange groups, colon-separated (@X25519MLKEM768:X25519:P-256@);
    -- 'Nothing': AWS-LC's default
  , csVersions    :: !(Version, Version)           -- ^ the lowest and the highest
  , csCiphers     :: !(Maybe ByteString)
    -- ^ TLS 1.2's ciphers; 'Nothing': ECDHE with AEAD only (T2). Only a
    -- test client should set this.
  } deriving (Show)

defaultConfigSettings :: Role -> ConfigSettings
defaultConfigSettings role = ConfigSettings
  { csRole = role
  , csQuic = False
  , csCertificate = Nothing
  , csALPN = []
  , csGroups = Just "X25519MLKEM768:X25519:P-256"
  , csVersions = (TLS12, TLS13)
  , csCiphers = Nothing
  }

newtype Config = Config (ForeignPtr C.Config)

-- | What the library said went wrong.
newtype TLSError = TLSError String
  deriving (Eq, Show)

errorCap :: Int
errorCap = 256

-- a call that fills an error buffer; its message on failure
withError :: (CString -> IO CInt) -> IO (Either TLSError ())
withError call = allocaBytes errorCap $ \buf -> do
  r <- call buf
  if r == 0 then pure (Right ()) else Left . TLSError <$> peekCString buf

-- | Check and load a configuration.
newConfig :: ConfigSettings -> IO (Either TLSError Config)
newConfig cs = allocaBytes errorCap $ \err -> do
  p <- C.configNew (if csRole cs == Server then 1 else 0) (if csQuic cs then 1 else 0) err (fromIntegral errorCap)
  if p == nullPtr then Left . TLSError <$> peekCString err else do
    fp <- newForeignPtr C.p_configFree p
    let alpnWire = B.concat [ B.cons (fromIntegral (B.length a)) a | a <- csALPN cs ]
        bad = any (\a -> B.null a || B.length a > 255) (csALPN cs)
    r <- withForeignPtr fp $ \c -> runAll
      [ case (csRole cs, csCertificate cs) of
          (_, Just (chain, key)) ->
            BU.unsafeUseAsCStringLen chain $ \(cp, cl) -> BU.unsafeUseAsCStringLen key $ \(kp, kl) ->
              withError (\e -> C.configSetCert c (castPtr cp) (fromIntegral cl) (castPtr kp) (fromIntegral kl) e (fromIntegral errorCap))
          (Server, Nothing) -> pure (Left (TLSError "a server needs a certificate and a key"))
          (Client, Nothing) -> pure (Right ())
      , if bad then pure (Left (TLSError "an ALPN protocol is empty or longer than 255 octets"))
        else if B.null alpnWire then pure (Right ())
        else BU.unsafeUseAsCStringLen alpnWire $ \(ap, al) -> do
          ok <- C.configSetAlpn c (castPtr ap) (fromIntegral al)
          pure (if ok == 0 then Right () else Left (TLSError "ALPN list too long"))
      , case csGroups cs of
          Nothing -> pure (Right ())
          Just g -> B.useAsCString g $ \gp -> withError (\e -> C.configSetGroups c gp e (fromIntegral errorCap))
      , let (lo, hi) = csVersions cs
        in if lo > hi || (csQuic cs && lo < TLS13) then pure (Left (TLSError "versions out of order, or below TLS 1.3 for QUIC"))
           else do ok <- C.configSetVersions c (versionCode lo) (versionCode hi)
                   pure (if ok == 0 then Right () else Left (TLSError "versions"))
      , case csCiphers cs of
          Nothing -> pure (Right ())
          Just l -> B.useAsCString l $ \lp -> withError (\e -> C.configSetCiphers c lp e (fromIntegral errorCap))
      ]
    pure (Config fp <$ r)
  where
    runAll [] = pure (Right ())
    runAll (a : rest) = a >>= either (pure . Left) (const (runAll rest))

-- | A session; its configuration is kept alive with it.
data Session = Session !(ForeignPtr C.Session) !Config

-- | A new session: for a client, the server's name (SNI); the bound on
-- ciphertext received and not yet processed (T1).
newSession :: Config -> Maybe ByteString -> Int -> IO Session
newSession cfg@(Config cfp) name maxBuffered = withForeignPtr cfp $ \c -> do
  p <- maybe ($ nullPtr) B.useAsCString name $ \np -> C.sessionNew c np (fromIntegral maxBuffered)
  if p == nullPtr then ioError (userError "TLS session: out of memory") else do
    fp <- newForeignPtr C.p_sessionFree p
    pure (Session fp cfg)

withSession :: Session -> (Ptr C.Session -> IO a) -> IO a
withSession (Session fp (Config cfp)) act =
  -- the configuration's ALPN list is read by a callback during the
  -- handshake: alive for every call
  withForeignPtr cfp $ \_ -> withForeignPtr fp act

lastError :: Ptr C.Session -> IO TLSError
lastError s = TLSError <$> (C.sessionError s >>= peekCString . unConstPtr)

data Step = Done | WantInput
  deriving (Eq, Show)

-- | One step of the handshake (a @safe@ call). Anything it leaves to
-- send is in 'takeOutput'.
handshake :: Session -> IO (Either TLSError Step)
handshake sess = withSession sess $ \s -> do
  r <- C.handshake s
  case r of
    0 -> pure (Right Done)
    1 -> pure (Right WantInput)
    _ -> Left <$> lastError s

-- | Ciphertext received; refused beyond the session's bound.
feed :: Session -> ByteString -> IO (Either TLSError ())
feed sess bs = withSession sess $ \s -> BU.unsafeUseAsCStringLen bs $ \(p, n) -> do
  r <- C.feed s (castPtr p) (fromIntegral n)
  if r == 0 then pure (Right ()) else Left <$> lastError s

pendingOutput :: Session -> IO Int
pendingOutput sess = withSession sess $ \s -> fromIntegral <$> C.pendingOut s

-- | All the ciphertext waiting to be sent (handshake messages, records,
-- alerts, a reply to a KeyUpdate).
takeOutput :: Session -> IO ByteString
takeOutput sess = withSession sess $ \s -> do
  n <- fromIntegral <$> C.pendingOut s
  if n == 0 then pure B.empty
    else BI.createAndTrim n $ \p -> fromIntegral <$> C.drain s p (fromIntegral n)

data Received
  = Plain !ByteString      -- ^ plaintext
  | NeedInput              -- ^ none until more ciphertext is fed
  | PeerClosed             -- ^ the peer's close_notify
  | ReceiveFailed !TLSError -- ^ an alert may be waiting in 'takeOutput'
  deriving (Eq, Show)

-- | At most the given number of octets of plaintext (one record at a
-- time, so at most 16 KiB).
receive :: Session -> Int -> IO Received
receive sess cap = withSession sess $ \s -> do
  fp <- BI.mallocByteString cap
  n <- withForeignPtr fp $ \p -> C.readPlain s p (fromIntegral cap)
  case n of
    _ | n > 0 -> pure (Plain (if fromIntegral n * 2 < cap
                                then B.copy (BI.BS fp (fromIntegral n))
                                else BI.BS fp (fromIntegral n)))
    0 -> pure NeedInput
    -2 -> pure PeerClosed
    _ -> ReceiveFailed <$> lastError s

-- | Encrypt all of it; the ciphertext is in 'takeOutput'.
send :: Session -> ByteString -> IO (Either TLSError ())
send sess bs = withSession sess $ \s -> BU.unsafeUseAsCStringLen bs $ \(p, n) ->
  if n == 0 then pure (Right ()) else do
    r <- C.writePlain s (castPtr p) (fromIntegral n)
    if fromIntegral r == n then pure (Right ()) else Left <$> lastError s

-- | Queue close_notify (T6).
closeNotify :: Session -> IO (Either TLSError ())
closeNotify sess = withSession sess $ \s -> do
  r <- C.shutdown s
  if r == 0 then pure (Right ()) else Left <$> lastError s

-- | Ask the peer for a key update (TLS 1.3; for tests).
keyUpdate :: Session -> IO (Either TLSError ())
keyUpdate sess = withSession sess $ \s -> do
  r <- C.keyUpdate s
  if r == 0 then pure (Right ()) else Left <$> lastError s

data Agreed = Agreed
  { agreedVersion    :: !Version
  , agreedCipher     :: !ByteString
  , agreedGroup      :: !ByteString
  , agreedALPN       :: !(Maybe ByteString)
  , agreedServerName :: !(Maybe ByteString)
  , agreedPeerSignatureAlgorithm :: !Word16   -- ^ for a client: the server's signature
  , agreedPeerCertificates :: !Int             -- ^ for a client: the certificates the server sent
  } deriving (Eq, Show)

-- | What the handshake agreed.
agreed :: Session -> IO Agreed
agreed sess = withSession sess $ \s -> do
  v <- C.version s
  c <- C.cipher s >>= B.packCString . unConstPtr
  g <- C.keyGroup s >>= B.packCString . unConstPtr
  a <- alloca $ \lp -> do
    ConstPtr p <- C.alpn s lp
    n <- peek lp
    if p == nullPtr || n == 0 then pure Nothing else Just <$> B.packCStringLen (castPtr p, fromIntegral n)
  ConstPtr np <- C.servername s
  sn <- if np == nullPtr then pure Nothing else Just <$> B.packCString np
  sig <- C.peerSignatureAlgorithm s
  certs <- C.peerCertificates s
  pure (Agreed (if v == 0x0304 then TLS13 else TLS12) c g a sn (fromIntegral sig) (fromIntegral certs))

-- | For a client: whether the server gave it a way to resume the session
-- (for tests: a server here never does, T7).
resumable :: Session -> IO Bool
resumable sess = withSession sess $ fmap (/= 0) . C.resumable

------------------------------------------------------------------------------
-- QUIC
------------------------------------------------------------------------------

quicSetTransportParams :: Session -> ByteString -> IO (Either TLSError ())
quicSetTransportParams sess bs = withSession sess $ \s -> BU.unsafeUseAsCStringLen bs $ \(p, n) -> do
  r <- C.quicSetTransportParams s (castPtr p) (fromIntegral n)
  if r == 0 then pure (Right ()) else Left <$> lastError s

-- | Handshake octets received at an encryption level (0 initial, 1 early
-- data, 2 handshake, 3 application). Step the handshake after each.
quicProvide :: Session -> Int -> ByteString -> IO (Either TLSError ())
quicProvide sess level bs = withSession sess $ \s -> BU.unsafeUseAsCStringLen bs $ \(p, n) -> do
  r <- C.quicProvide s (fromIntegral level) (castPtr p) (fromIntegral n)
  if r == 0 then pure (Right ()) else Left <$> lastError s

-- | Handshake octets to send, by level, lowest first.
quicTake :: Session -> IO [(Int, ByteString)]
quicTake sess = withSession sess $ \s -> alloca $ \lp -> allocaBytes 65536 $ \buf -> do
  let loop acc = do
        n <- C.quicTake s lp buf 65536
        if n <= 0 then pure (reverse acc) else do
          l <- peek lp
          b <- B.packCStringLen (castPtr buf, fromIntegral n)
          loop ((fromIntegral l, b) : acc)
  loop []

data Secret = Secret { secretLevel :: !Int, secretWrite :: !Bool, secretBytes :: !ByteString }
  deriving (Eq, Show)

-- | The secrets the handshake has made since last asked.
quicSecrets :: Session -> IO [Secret]
quicSecrets sess = withSession sess $ \s -> alloca $ \lp -> alloca $ \dp -> allocaBytes 64 $ \buf -> do
  let loop acc = do
        n <- C.quicSecret s lp dp buf 64
        if n <= 0 then pure (reverse acc) else do
          l <- peek lp
          d <- peek dp
          b <- B.packCStringLen (castPtr buf, fromIntegral n)
          loop (Secret (fromIntegral l) (d == 1) b : acc)
  loop []

quicAlert :: Session -> IO Int
quicAlert sess = withSession sess $ fmap fromIntegral . C.quicAlert

