{-# LANGUAGE CApiFFI, PatternSynonyms #-}
-- | The C shim over AWS-LC (cbits/hd_tls.c), as it is: every call
-- @unsafe@ but the handshake's steps and 'tlsSign' (D3; the TLS spike,
-- docs/tls-spike-design.md). The spike found TLS 1.3's step that
-- answers the ClientHello, with AWS-LC's default X25519MLKEM768, at
-- about 80 µs and up to 110 at the 99th percentile: over the 100 µs an
-- @unsafe@ call may hold its capability, while a @safe@ call costs a
-- fraction of a microsecond more. The library's callbacks only fill or read
-- the session's buffers in C, so none enters Haskell.
module Hyperdrive.TLS.Shim
  ( Config
  , Session
  , Level
    -- * Configuration
  , configServer
  , configClient
  , configLimitVersion
  , configFree
    -- * Sessions
  , sessionNew
  , sessionFree
  , handshake
  , pattern HandshakeDone, pattern WantRead, pattern WantSign, pattern HandshakeError
  , tlsSign
    -- * TCP
  , feed
  , pendingOut
  , drain
  , readPlain
  , writePlain
  , shutdown
    -- * What was agreed
  , alpn
  , servername
  , version
  , cipher
  , signatureAlgorithm
  , keyGroup
  , lastError
    -- * QUIC
  , quicProvide
  , quicTake
  , quicSecret
  , quicAlert
  ) where

import Data.Word (Word16, Word8)
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CInt(..), CLong(..), CSize(..), CUInt(..))
import Foreign.Ptr (Ptr)

data Config
data Session

-- | A QUIC encryption level: 0 initial, 1 early data, 2 handshake, 3
-- application (AWS-LC's @ssl_encryption_level_t@).
type Level = CInt

pattern HandshakeDone, WantRead, WantSign, HandshakeError :: CInt
pattern HandshakeDone = 0
pattern WantRead = 1
pattern WantSign = 2
pattern HandshakeError = -1

foreign import capi unsafe "hd_tls.h hd_tls_config_server"
  configServer :: CString -> CString -> CInt -> IO (Ptr Config)
foreign import capi unsafe "hd_tls.h hd_tls_config_client"
  configClient :: CInt -> IO (Ptr Config)
foreign import capi unsafe "hd_tls.h hd_tls_config_limit_version"
  configLimitVersion :: Ptr Config -> CInt -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_config_free"
  configFree :: Ptr Config -> IO ()

foreign import capi unsafe "hd_tls.h hd_tls_new"
  sessionNew :: Ptr Config -> CInt -> CString -> IO (Ptr Session)
foreign import capi unsafe "hd_tls.h hd_tls_free"
  sessionFree :: Ptr Session -> IO ()
foreign import capi safe "hd_tls.h hd_tls_handshake"
  handshake :: Ptr Session -> IO CInt
-- | The pending signature: @safe@, since an RSA signature takes far
-- longer than an @unsafe@ call may.
foreign import capi safe "hd_tls.h hd_tls_sign"
  tlsSign :: Ptr Session -> IO CInt

foreign import capi unsafe "hd_tls.h hd_tls_feed"
  feed :: Ptr Session -> Ptr Word8 -> CSize -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_pending_out"
  pendingOut :: Ptr Session -> IO CLong
foreign import capi unsafe "hd_tls.h hd_tls_drain"
  drain :: Ptr Session -> Ptr Word8 -> CSize -> IO CLong
foreign import capi unsafe "hd_tls.h hd_tls_read"
  readPlain :: Ptr Session -> Ptr Word8 -> CSize -> IO CLong
foreign import capi unsafe "hd_tls.h hd_tls_write"
  writePlain :: Ptr Session -> Ptr Word8 -> CSize -> IO CLong
foreign import capi unsafe "hd_tls.h hd_tls_shutdown"
  shutdown :: Ptr Session -> IO CInt

foreign import capi unsafe "hd_tls.h hd_tls_alpn"
  alpn :: Ptr Session -> Ptr CUInt -> IO (ConstPtr Word8)
foreign import capi unsafe "hd_tls.h hd_tls_servername"
  servername :: Ptr Session -> IO (ConstPtr CChar)
foreign import capi unsafe "hd_tls.h hd_tls_version"
  version :: Ptr Session -> IO (ConstPtr CChar)
foreign import capi unsafe "hd_tls.h hd_tls_cipher"
  cipher :: Ptr Session -> IO (ConstPtr CChar)
foreign import capi unsafe "hd_tls.h hd_tls_signature_algorithm"
  signatureAlgorithm :: Ptr Session -> IO Word16
foreign import capi unsafe "hd_tls.h hd_tls_group"
  keyGroup :: Ptr Session -> IO (ConstPtr CChar)
foreign import capi unsafe "hd_tls.h hd_tls_error"
  lastError :: CString -> CSize -> IO ()

foreign import capi unsafe "hd_tls.h hd_tls_quic_provide"
  quicProvide :: Ptr Session -> Level -> Ptr Word8 -> CSize -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_quic_take"
  quicTake :: Ptr Session -> Ptr Level -> Ptr Word8 -> CSize -> IO CLong
foreign import capi unsafe "hd_tls.h hd_tls_quic_secret"
  quicSecret :: Ptr Session -> Ptr Level -> Ptr CInt -> Ptr Word8 -> CSize -> IO CLong
foreign import capi unsafe "hd_tls.h hd_tls_quic_alert"
  quicAlert :: Ptr Session -> IO CInt
