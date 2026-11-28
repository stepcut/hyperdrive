{-# LANGUAGE CApiFFI #-}
-- | The C shim over AWS-LC (cbits/hd_tls.c), as it is. Every call is
-- @unsafe@ but the handshake's step (docs/tls-design.md, T5): TLS 1.3's
-- step that answers the ClientHello takes 80 to 110 µs with AWS-LC's
-- default hybrid group, over the 100 µs an @unsafe@ call may hold its
-- capability (the spike, docs/tls-spike-design.md). Use "Hyperdrive.TLS".
module Hyperdrive.TLS.Shim
  ( Config
  , Session
    -- * Configuration
  , configNew
  , configSetCert
  , configSetAlpn
  , configSetGroups
  , configSetVersions
  , configSetCiphers
  , p_configFree
    -- * Sessions
  , sessionNew
  , p_sessionFree
  , sessionError
  , handshake
  , feed
  , pendingOut
  , drain
  , readPlain
  , writePlain
  , shutdown
  , keyUpdate
    -- * What was agreed
  , alpn
  , servername
  , version
  , cipher
  , keyGroup
  , peerSignatureAlgorithm
  , peerCertificates
  , resumable
    -- * QUIC
  , quicSetTransportParams
  , quicProvide
  , quicTake
  , quicSecret
  , quicAlert
  ) where

import Data.Word (Word8)
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CInt(..), CLong(..), CSize(..), CUInt(..))
import Foreign.Ptr (FunPtr, Ptr)

data Config
data Session

foreign import capi unsafe "hd_tls.h hd_tls_config_new"
  configNew :: CInt -> CInt -> CString -> CSize -> IO (Ptr Config)
foreign import capi unsafe "hd_tls.h hd_tls_config_set_cert"
  configSetCert :: Ptr Config -> Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CString -> CSize -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_config_set_alpn"
  configSetAlpn :: Ptr Config -> Ptr Word8 -> CSize -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_config_set_groups"
  configSetGroups :: Ptr Config -> CString -> CString -> CSize -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_config_set_versions"
  configSetVersions :: Ptr Config -> CInt -> CInt -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_config_set_ciphers"
  configSetCiphers :: Ptr Config -> CString -> CString -> CSize -> IO CInt
foreign import capi unsafe "hd_tls.h &hd_tls_config_free"
  p_configFree :: FunPtr (Ptr Config -> IO ())

foreign import capi unsafe "hd_tls.h hd_tls_new"
  sessionNew :: Ptr Config -> CString -> CSize -> IO (Ptr Session)
foreign import capi unsafe "hd_tls.h &hd_tls_free"
  p_sessionFree :: FunPtr (Ptr Session -> IO ())
foreign import capi unsafe "hd_tls.h hd_tls_error"
  sessionError :: Ptr Session -> IO (ConstPtr CChar)
foreign import capi safe "hd_tls.h hd_tls_handshake"
  handshake :: Ptr Session -> IO CInt
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
foreign import capi unsafe "hd_tls.h hd_tls_key_update"
  keyUpdate :: Ptr Session -> IO CInt

foreign import capi unsafe "hd_tls.h hd_tls_alpn"
  alpn :: Ptr Session -> Ptr CUInt -> IO (ConstPtr Word8)
foreign import capi unsafe "hd_tls.h hd_tls_servername"
  servername :: Ptr Session -> IO (ConstPtr CChar)
foreign import capi unsafe "hd_tls.h hd_tls_version"
  version :: Ptr Session -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_cipher"
  cipher :: Ptr Session -> IO (ConstPtr CChar)
foreign import capi unsafe "hd_tls.h hd_tls_group"
  keyGroup :: Ptr Session -> IO (ConstPtr CChar)
foreign import capi unsafe "hd_tls.h hd_tls_peer_signature_algorithm"
  peerSignatureAlgorithm :: Ptr Session -> IO CInt

foreign import capi unsafe "hd_tls.h hd_tls_resumable"
  resumable :: Ptr Session -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_peer_certificates"
  peerCertificates :: Ptr Session -> IO CInt

foreign import capi unsafe "hd_tls.h hd_tls_quic_set_transport_params"
  quicSetTransportParams :: Ptr Session -> Ptr Word8 -> CSize -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_quic_provide"
  quicProvide :: Ptr Session -> CInt -> Ptr Word8 -> CSize -> IO CInt
foreign import capi unsafe "hd_tls.h hd_tls_quic_take"
  quicTake :: Ptr Session -> Ptr CInt -> Ptr Word8 -> CSize -> IO CLong
foreign import capi unsafe "hd_tls.h hd_tls_quic_secret"
  quicSecret :: Ptr Session -> Ptr CInt -> Ptr CInt -> Ptr Word8 -> CSize -> IO CLong
foreign import capi unsafe "hd_tls.h hd_tls_quic_alert"
  quicAlert :: Ptr Session -> IO CInt
