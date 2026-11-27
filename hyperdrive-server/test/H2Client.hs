{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | An HTTP/2 client for the server's tests, over a socket, built from
-- the HTTP/2 package's own frames and HPACK: a reader thread acknowledges
-- SETTINGS, gives back the credit of the DATA it receives, keeps the
-- server's windows, and passes everything else on as events.
module H2Client
  ( Client
  , Event(..)
  , connect
  , connectWith
  , sendRaw
  , sendRequest
  , sendData
  , sendTrailers
  , nextEvent
  , collect
  , Result(..)
  , closeClient
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import Hyperdrive.HPACK.Decode (Decoded(..), decodeBlock, newDecoder)
import Hyperdrive.HPACK.Encode (Encoder, encodeBlock, newEncoder)
import Hyperdrive.HTTP2.Connection (connectionPreface)
import Hyperdrive.HTTP2.Frame
import Network.Socket hiding (Stream, connect)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as SB
import System.Timeout (timeout)

data Client = Client
  { clSock     :: Socket
  , clLock     :: MVar ()                 -- ^ one writer at a time
  , clEncoder  :: IORef Encoder
  , clEvents   :: TChan Event
  , clConnWin  :: TVar Int                -- ^ what we may still send on the connection
  , clInitWin  :: TVar Int                -- ^ the server's INITIAL_WINDOW_SIZE
  , clStreamWin :: TVar (M.Map Word32 Int) -- ^ adjustments to each stream's window
  , clReader   :: ThreadId
  }

data Event
  = Headers Word32 [(ByteString, ByteString)] Bool
  | Data Word32 ByteString Bool
  | Reset Word32 ErrorCode
  | GoAway Word32 ErrorCode
  | Ping ByteString                       -- ^ a PING from the server (not answered unless asked)
  | Closed                                -- ^ the server closed the connection
  deriving (Eq, Show)

-- | Connect, send the preface and empty SETTINGS; PINGs are answered.
connect :: PortNumber -> IO Client
connect port = connectWith port True []

-- | Connect with these SETTINGS, answering the server's PINGs or not.
connectWith :: PortNumber -> Bool -> [Setting] -> IO Client
connectWith port answerPings settings = do
  addr : _ <- getAddrInfo (Just defaultHints { addrSocketType = NS.Stream }) (Just "127.0.0.1") (Just (show port))
  s <- openSocket addr
  connect' s (addrAddress addr)
  lock <- newMVar ()
  events <- newTChanIO
  connWin <- newTVarIO 65535
  initWin <- newTVarIO 65535
  streamWin <- newTVarIO M.empty
  enc <- newIORef (newEncoder 4096)
  let send bs = withMVar lock (\_ -> SB.sendAll s (B.concat bs)) `catch` \(_ :: IOException) -> pure ()
      emit = atomically . writeTChan events
      reader = go B.empty (newDecoder 4096 4096)
      go buf dec = case parseFrame 0xFFFFFF buf of
        Parsed f n -> handle f dec >>= go (B.drop n buf)
        Incomplete _ -> do
          more <- SB.recv s 65536 `catch` \(_ :: IOException) -> pure B.empty
          if B.null more then emit Closed else go (buf <> more) dec
        Refused e -> error ("the server sent a bad frame: " ++ show e)
      handle f dec = case f of
        SettingsFrame _ ss -> do
          atomically $ mapM_ (\x -> case x of
            InitialWindowSize v -> writeTVar initWin (fromIntegral v)
            _ -> pure ()) ss
          send [renderSettingsAck]
          pure dec
        SettingsAckFrame -> pure dec
        WindowUpdateFrame (StreamId 0) n -> atomically (modifyTVar' connWin (+ fromIntegral n)) >> pure dec
        WindowUpdateFrame (StreamId sid) n ->
          atomically (modifyTVar' streamWin (M.insertWith (+) sid (fromIntegral n))) >> pure dec
        DataFrame (StreamId sid) end d plen -> do
          -- the credit comes back at once, so the server can go on
          when (plen > 0) $ send ([renderWindowUpdate (StreamId 0) (fromIntegral plen)]
                                  ++ [renderWindowUpdate (StreamId sid) (fromIntegral plen) | not end])
          emit (Data sid d end)
          pure dec
        HeadersFrame (StreamId sid) end _ block -> case decodeBlock maxBound dec block of
          Right (Fields fs, dec') -> emit (Headers sid fs end) >> pure dec'
          other -> error ("a field block the client cannot decode: " ++ show other)
        RstStreamFrame (StreamId sid) code -> emit (Reset sid code) >> pure dec
        GoAwayFrame (StreamId l) code _ -> emit (GoAway l code) >> pure dec
        PingFrame False d -> do
          if answerPings then send [renderPing True d] else emit (Ping d)
          pure dec
        _ -> pure dec
  tid <- forkIO (reader `catch` \(e :: SomeException) -> unless (isKill e) (atomically (writeTChan events Closed)))
  let c = Client s lock enc events connWin initWin streamWin tid
  sendRaw c [connectionPreface, renderSettings settings]
  pure c
  where
    connect' s a = NS.connect s a
    isKill e = case fromException e of
      Just ThreadKilled -> True
      _ -> False

sendRaw :: Client -> [ByteString] -> IO ()
sendRaw c bs = withMVar (clLock c) (\_ -> SB.sendAll (clSock c) (B.concat bs)) `catch` \(_ :: IOException) -> pure ()

sendRequest :: Client -> Word32 -> [(ByteString, ByteString)] -> Bool -> IO ()
sendRequest c sid fields end = do
  block <- atomicModifyIORef' (clEncoder c) (\e -> let (b, e') = encodeBlock e fields in (e', b))
  sendRaw c (renderHeaders 16384 (StreamId sid) end block)

sendTrailers :: Client -> Word32 -> [(ByteString, ByteString)] -> IO ()
sendTrailers c sid fields = sendRequest c sid fields True

-- | Send content within the server's windows, waiting for credit.
sendData :: Client -> Word32 -> ByteString -> Bool -> IO ()
sendData c sid d end
  | B.null d = sendRaw c (renderData (StreamId sid) end B.empty)
  | otherwise = do
      n <- atomically $ do
        conn <- readTVar (clConnWin c)
        initial <- readTVar (clInitWin c)
        extra <- M.findWithDefault 0 sid <$> readTVar (clStreamWin c)
        let n = minimum [16384, B.length d, conn, initial + extra]
        check (n > 0)
        writeTVar (clConnWin c) (conn - n)
        modifyTVar' (clStreamWin c) (M.insertWith (+) sid (negate n))
        pure n
      let (piece, rest) = B.splitAt n d
      sendRaw c (renderData (StreamId sid) (end && B.null rest) piece)
      unless (B.null rest) (sendData c sid rest end)

-- | The next event, or 'Closed' after five seconds without one.
nextEvent :: Client -> IO Event
nextEvent c = maybe Closed id <$> timeout 5000000 (atomically (readTChan (clEvents c)))

-- | What came back on a stream.
data Result = Result
  { resHeaders  :: [(ByteString, ByteString)]
  , resBody     :: ByteString
  , resTrailers :: [(ByteString, ByteString)]
  , resReset    :: Maybe ErrorCode
  } deriving (Eq, Show)

-- | Read events until each of the streams has ended (or been reset), or
-- the connection closes; the other events seen, in order.
collect :: Client -> [Word32] -> IO (M.Map Word32 Result, [Event])
collect c sids = go (M.fromList [(s, Result [] B.empty [] Nothing) | s <- sids]) (M.fromList [(s, ()) | s <- sids]) []
  where
    go acc open others
      | M.null open = pure (acc, reverse others)
      | otherwise = nextEvent c >>= \e -> case e of
          Headers s fs end | M.member s acc ->
            let r = acc M.! s
                r' = if null (resHeaders r) || isInterim r then r { resHeaders = fs } else r { resTrailers = fs }
            in go (M.insert s r' acc) (if end then M.delete s open else open) others
          Data s d end | M.member s acc ->
            go (M.adjust (\r -> r { resBody = resBody r <> d }) s acc) (if end then M.delete s open else open) others
          Reset s code | M.member s acc -> go (M.adjust (\r -> r { resReset = Just code }) s acc) (M.delete s open) others
          Closed -> pure (acc, reverse (Closed : others))
          _ -> go acc open (e : others)
    isInterim r = maybe False ((== "1") . B.take 1) (lookup ":status" (resHeaders r))

closeClient :: Client -> IO ()
closeClient c = killThread (clReader c) >> close (clSock c)

