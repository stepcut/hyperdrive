{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The hyperdrive HTTP server: listen, accept, and serve each
-- connection in its own thread (docs/server-design.md).
module Hyperdrive.Server
  ( -- * Running a server
    runServer
  , withServer
  , Server(..)
    -- * Applications and settings
  , Application
  , Context(..)
  , Settings(..)
  , Timeouts(..)
  , defaultSettings
  , defaultTimeouts
  , ConnectionClosed(..)
  , ServerError(..)
    -- * Lower level
  , serveConnection
  , Timers
  , withTimers
  , beginShutdown
  , Transport(..)
  , socketTransport
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, unless, void, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Foreign.C.Error (Errno(..), eMFILE, eNFILE)
import GHC.IO.Exception (IOException(..))
import Hyperdrive.Server.Connection
import Hyperdrive.Server.Timer
import Hyperdrive.Server.Transport
import Network.Socket

-- | A running server.
data Server = Server
  { serverPort     :: PortNumber
    -- ^ the port it listens on (useful with 'settingsPort' 0)
  , serverShutdown :: IO ()
    -- ^ stop: no new connections; idle ones close at once, busy ones
    -- after their current exchange; after 'settingsShutdownGrace',
    -- the rest are killed. Returns when done; calling it again does
    -- nothing.
  }

-- | Serve until the thread is interrupted (Ctrl-C, or an exception from
-- another thread), then shut down.
runServer :: Settings -> Application -> IO ()
runServer settings app = withServer settings app (\_ -> forever (threadDelay maxBound))

-- | Start a server, run the action, and shut the server down when the
-- action returns or throws.
withServer :: Settings -> Application -> (Server -> IO a) -> IO a
withServer settings app act =
  withTimers (settingsTimerResolution settings) $ \timers ->
    bracket (startServer settings timers app) serverShutdown act

startServer :: Settings -> Timers -> Application -> IO Server
startServer settings timers app = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addr : _ <- getAddrInfo (Just hints) (settingsHost settings) (Just (show (settingsPort settings)))
  listener <- bracketOnError (openSocket addr) close $ \s -> do
    setSocketOption s ReuseAddr 1
    bind s (addrAddress addr)
    listen s 1024
    pure s
  port <- socketPort listener
  live <- newTVarIO (0 :: Int)
  threads <- newTVarIO (Map.empty :: Map.Map ThreadId ())
  capabilities <- getNumCapabilities
  accepted <- newIORef (0 :: Int)
  let serve s peer = do
        local <- getSocketName s
        transport <- socketTransport s
        serveConnection settings timers transport (Context peer local False) app
      fork :: Int -> ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
      fork n
        | settingsPinConnections settings = forkOnWithUnmask (n `rem` capabilities)
        | otherwise = forkIOWithUnmask
      acceptLoop = forever $ do
        -- wait below the connection limit, if there is one
        case settingsMaxConnections settings of
          Just n -> atomically (readTVar live >>= check . (< n))
          Nothing -> pure ()
        r <- try (accept listener)
        case r of
          Left (e :: IOException)
            | ioe_errno e `elem` map (Just . (\(Errno c) -> c)) [eMFILE, eNFILE] -> threadDelay 10000
            | otherwise -> threadDelay 1000
          Right (s, peer) -> do
            setSocketOption s NoDelay 1 `catch` \(_ :: IOException) -> pure ()
            mask_ $ do
              atomically (modifyTVar' live (+ 1))
              n <- readIORef accepted
              writeIORef accepted (n + 1)
              void $ fork n $ \unmask -> do
                me <- myThreadId
                atomically (modifyTVar' threads (Map.insert me ()))
                unmask (serve s peer) `catch` (\(_ :: SomeException) -> pure ()) `finally`
                  atomically (modifyTVar' threads (Map.delete me) >> modifyTVar' live (subtract 1))
  acceptor <- forkIOWithUnmask (\unmask -> unmask acceptLoop `catch` \(_ :: SomeException) -> pure ())
  done <- newIORef False
  let stop = do
        already <- atomicModifyIORef' done (\d -> (True, d))
        unless already $ do
          killThread acceptor
          close listener
          -- graceful: idle connections close now, busy ones after their exchange
          beginShutdown timers
          grace <- registerDelay (settingsShutdownGrace settings)
          atomically $ (readTVar live >>= check . (== 0)) `orElse` (readTVar grace >>= check)
          -- the grace period's end: HTTP/2 connections reset their
          -- streams left and send their GOAWAY (D10, V5), briefly
          busy <- (> 0) <$> readTVarIO live
          when busy $ do
            endShutdownGrace timers
            brief <- registerDelay 1000000
            atomically $ (readTVar live >>= check . (== 0)) `orElse` (readTVar brief >>= check)
          -- forced: kill what is left, without waiting on any one thread
          remaining <- Map.keys <$> readTVarIO threads
          mapM_ (\t -> forkIO (killThread t)) remaining
          when (not (null remaining)) $ do
            patience <- registerDelay 1000000
            atomically $ (readTVar live >>= check . (== 0)) `orElse` (readTVar patience >>= check)
  pure (Server port stop)
