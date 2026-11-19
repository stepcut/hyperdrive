-- | What a connection is carried over: a socket now, TLS later.
module Hyperdrive.Server.Transport
  ( Transport(..)
  , socketTransport
  ) where

import Control.Concurrent.STM (STM)
import Control.Exception (IOException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import GHC.Conc (threadWaitReadSTM)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import System.Posix.Types (Fd(..))

data Transport = Transport
  { transportRecv          :: IO ByteString
    -- ^ what has arrived, or @""@ at the end of input (or if the
    -- connection was reset)
  , transportSendMany      :: [ByteString] -> IO ()
    -- ^ write all of these, in order; throws if the connection is gone
  , transportWaitReadable  :: IO (STM (), IO ())
    -- ^ an STM action that succeeds once 'transportRecv' would not
    -- block, and an action to call afterwards to release what waiting
    -- registered
  , transportShutdownWrite :: IO ()
  , transportClose         :: IO ()
  }

-- | A connected TCP socket.
socketTransport :: S.Socket -> Transport
socketTransport s = Transport
  { transportRecv          = SB.recv s 16384 `catch` \e -> const (pure B.empty) (e :: IOException)
  , transportSendMany      = SB.sendMany s
  , transportWaitReadable  = S.withFdSocket s (threadWaitReadSTM . Fd)
  , transportShutdownWrite = S.shutdown s S.ShutdownSend `catch` \e -> const (pure ()) (e :: IOException)
  , transportClose         = S.close s
  }
