{-# LANGUAGE ForeignFunctionInterface #-}
-- | What a connection is carried over: a socket now, TLS later.
module Hyperdrive.Server.Transport
  ( Transport(..)
  , socketTransport
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (foldM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word8)
import Foreign.C.Error (eAGAIN, eINTR, eWOULDBLOCK, getErrno)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Conc (threadWaitRead)
import GHC.Event (Lifetime(OneShot), evtRead, getSystemEventManager, registerFd, unregisterFd)
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
  , transportOnReadable    :: IO () -> IO (IO ())
    -- ^ call the action, once, when 'transportTryRecv' would have
    -- something (input or the end of it); gives an action that cancels
    -- this. The action runs on the event manager's thread, so it must
    -- be quick and must not block.
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
    , transportSendMany      = sendPieces s
    , transportOnReadable    = onReadable s
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

foreign import ccall unsafe "send"
  c_send :: CInt -> Ptr Word8 -> CSize -> CInt -> IO CSsize

-- | Output up to this size is sent as one piece.
smallSend :: Int
smallSend = 2048

-- | Write all of these. A small response is a head and its content, and
-- writing the two with one vectored write costs more than copying them
-- together and sending once, in the kernel (a check and a copy for
-- each piece) and in marshalling the vector (docs/benchmarks.md). The
-- socket nearly always takes all of something this small; if it does
-- not (or fails), what is left goes the ordinary way, which waits for
-- the socket or throws.
sendPieces :: S.Socket -> [ByteString] -> IO ()
sendPieces s pieces
  | total <= smallSend = do
      sent <- S.withFdSocket s $ \fd -> allocaBytes total $ \buffer -> do
        foldM_ (\at piece -> BU.unsafeUseAsCStringLen piece $ \(p, n) ->
                  copyBytes (buffer `plusPtr` at) (castPtr p) n >> pure (at + n)) 0 pieces
        c_send fd buffer (fromIntegral total) 0
      let done = max 0 (fromIntegral sent)
      when (done < total) (SB.sendAll s (B.drop done (B.concat pieces)))
  | otherwise = SB.sendMany s pieces
  where total = sum (map B.length pieces)

-- | Register a one-shot callback with the event manager of the current
-- capability (as 'threadWaitRead' does).
onReadable :: S.Socket -> IO () -> IO (IO ())
onReadable s callback = S.withFdSocket s $ \fd -> do
  manager <- maybe (ioError (userError "no event manager: the server needs the threaded RTS")) pure
               =<< getSystemEventManager
  key <- registerFd manager (\_ _ -> callback) (Fd fd) evtRead OneShot
  pure (unregisterFd manager key)
