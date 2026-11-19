{-# LANGUAGE ForeignFunctionInterface #-}
-- | What a connection is carried over: a socket now, TLS later.
module Hyperdrive.Server.Transport
  ( Transport(..)
  , socketTransport
  ) where

import Control.Concurrent.STM (STM)
import Control.Exception (IOException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Foreign.C.Error (eAGAIN, eINTR, eWOULDBLOCK, getErrno)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Conc (threadWaitRead, threadWaitReadSTM)
import System.Posix.Types (CSsize(..), Fd(..))
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB

data Transport = Transport
  { transportRecv          :: IO ByteString
    -- ^ what has arrived, or @""@ at the end of input (or if the
    -- connection was reset)
  , transportTryRecv       :: IO (Maybe ByteString)
    -- ^ the same without waiting: 'Nothing' if nothing has arrived yet.
    -- On a busy connection the next request is usually there already,
    -- and trying first saves registering with the event manager and
    -- waiting for it (two system calls per request).
  , transportSendMany      :: [ByteString] -> IO ()
    -- ^ write all of these, in order; throws if the connection is gone
  , transportWaitReadable  :: IO (STM (), IO ())
    -- ^ an STM action that succeeds once 'transportRecv' would not
    -- block, and an action to call afterwards to release what waiting
    -- registered
  , transportShutdownWrite :: IO ()
  , transportAbort         :: IO ()
    -- ^ make a write blocked in another thread fail (shut the socket
    -- down both ways); used when a write times out
  , transportClose         :: IO ()
  }

-- | A connected TCP socket. Input is received into one buffer, kept
-- for the life of the connection, and copied out: a fresh 16 KiB
-- buffer for every receive (as 'Network.Socket.ByteString.recv' makes)
-- was most of the 46 KB a small request allocated, and the collections
-- that caused stopped every capability hundreds of times a second.
socketTransport :: S.Socket -> IO Transport
socketTransport s = do
  buffer <- mallocForeignPtrBytes bufferSize
  pure Transport
    { transportRecv          = maybe B.empty id <$> recvInto s buffer True
    , transportTryRecv       = recvInto s buffer False
    , transportSendMany      = SB.sendMany s
    , transportWaitReadable  = S.withFdSocket s (threadWaitReadSTM . Fd)
    , transportShutdownWrite = S.shutdown s S.ShutdownSend `catch` \e -> const (pure ()) (e :: IOException)
    , transportAbort         = S.shutdown s S.ShutdownBoth `catch` \e -> const (pure ()) (e :: IOException)
    , transportClose         = S.close s
    }

bufferSize :: Int
bufferSize = 16384

-- the socket is non-blocking (network makes it so), so this returns at once
foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSsize

-- | Receive into the connection's buffer and copy out what arrived:
-- @""@ at the end of input or on an error. When not waiting, 'Nothing'
-- if nothing has arrived; when waiting, wait for the socket and retry.
recvInto :: S.Socket -> ForeignPtr Word8 -> Bool -> IO (Maybe ByteString)
recvInto s buffer wait = S.withFdSocket s $ \fd -> loop fd
  where
    loop fd = do
      n <- withForeignPtr buffer $ \p -> c_recv fd p (fromIntegral bufferSize) 0
      if n > 0 then Just <$> withForeignPtr buffer (\p -> B.packCStringLen (castPtr p, fromIntegral n))
      else if n == 0 then pure (Just B.empty)
      else do
        errno <- getErrno
        if errno == eINTR then loop fd
        else if errno == eAGAIN || errno == eWOULDBLOCK then
          if wait then threadWaitRead (Fd fd) >> loop fd else pure Nothing
        else pure (Just B.empty)
