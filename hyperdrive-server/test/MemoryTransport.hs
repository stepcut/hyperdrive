-- | A transport in memory: the client's pieces are queued before or
-- while the server reads them, one piece per receive.
module MemoryTransport
  ( Pipe
  , memoryTransport
  , clientSend
  , clientEOF
  , clientReceived
  , serverShutWrite
  , clientTakes
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Hyperdrive.Server (ConnectionClosed(..), Transport(..))

data Pipe = Pipe
  { pipeIn       :: TVar [ByteString]  -- ^ pieces not yet received, oldest first
  , pipeEOF      :: TVar Bool
  , pipeOut      :: TVar [ByteString]  -- ^ written, newest first
  , pipeShutWrite :: TVar Bool
  , pipeClosed   :: TVar Bool
  , pipeTakes    :: TVar Int           -- ^ octets the client will still take (HTTP/2's non-blocking sends)
  }

memoryTransport :: IO (Transport, Pipe)
memoryTransport = do
  p <- Pipe <$> newTVarIO [] <*> newTVarIO False <*> newTVarIO [] <*> newTVarIO False <*> newTVarIO False
            <*> newTVarIO maxBound
  let readable = do
        q <- readTVar (pipeIn p)
        eof <- readTVar (pipeEOF p)
        check (not (null q) || eof)
      t = Transport
        { transportRecv = atomically $ do
            readable
            q <- readTVar (pipeIn p)
            case q of
              (x : xs) -> writeTVar (pipeIn p) xs >> pure x
              []       -> pure B.empty
        , transportTryRecv = atomically $ do
            q <- readTVar (pipeIn p)
            eof <- readTVar (pipeEOF p)
            case q of
              (x : xs) -> writeTVar (pipeIn p) xs >> pure (Just x)
              [] | eof -> pure (Just B.empty)
                 | otherwise -> pure Nothing
        , transportSendMany = \bs -> do
            ok <- atomically $ do
              shut <- (||) <$> readTVar (pipeShutWrite p) <*> readTVar (pipeClosed p)
              if shut then pure False else modifyTVar' (pipeOut p) (reverse bs ++) >> pure True
            if ok then pure () else throwIO ConnectionClosed
        , transportOnReadable = \callback -> do
            t' <- forkIO (atomically readable >> callback)
            pure (killThread t')
        , transportTryRecvLarge = \_ -> atomically $ do
            q <- readTVar (pipeIn p)
            eof <- readTVar (pipeEOF p)
            case q of
              (x : xs) -> writeTVar (pipeIn p) xs >> pure (Just x)
              [] | eof -> pure (Just B.empty)
                 | otherwise -> pure Nothing
        , transportTrySend = \bs -> do
            r <- atomically $ do
              shut <- (||) <$> readTVar (pipeShutWrite p) <*> readTVar (pipeClosed p)
              room <- readTVar (pipeTakes p)
              let b = B.concat bs
                  n = min room (B.length b)
              if shut then pure Nothing else do
                modifyTVar' (pipeOut p) (B.take n b :)
                writeTVar (pipeTakes p) (room - n)
                pure (Just n)
            maybe (ioError (userError "the memory transport is closed")) pure r
        , transportOnWritable = \callback -> do
            t' <- forkIO (atomically (readTVar (pipeTakes p) >>= check . (> 0)) >> callback)
            pure (killThread t')
        , transportShutdownWrite = atomically (writeTVar (pipeShutWrite p) True)
        , transportAbort = atomically (writeTVar (pipeClosed p) True)
        , transportClose = atomically (writeTVar (pipeClosed p) True)
        }
  pure (t, p)

clientSend :: Pipe -> ByteString -> IO ()
clientSend p b = atomically (modifyTVar' (pipeIn p) (++ [b]))

clientEOF :: Pipe -> IO ()
clientEOF p = atomically (writeTVar (pipeEOF p) True)

clientReceived :: Pipe -> IO ByteString
clientReceived p = B.concat . reverse <$> readTVarIO (pipeOut p)

serverShutWrite :: Pipe -> IO Bool
serverShutWrite p = readTVarIO (pipeShutWrite p)

-- | How many more octets the client takes from now on (it stops reading
-- at 0); 'maxBound' to read everything.
clientTakes :: Pipe -> Int -> IO ()
clientTakes p n = atomically (writeTVar (pipeTakes p) n)
