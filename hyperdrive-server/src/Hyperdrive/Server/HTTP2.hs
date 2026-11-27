{-# LANGUAGE LambdaCase, OverloadedStrings, PatternSynonyms, ScopedTypeVariables #-}
-- | Serving one HTTP/2 connection: the IO around
-- 'Hyperdrive.HTTP2.Connection.step' (docs/http2-driver-design.md).
--
-- One owner thread holds the connection's state and does all its I/O
-- (D4); it never blocks on a write (V1). Each request's handler runs in
-- a thread of its own, and talks to the owner through a list of inputs
-- beside the connection's 'MVar' (V2); the owner answers each stream
-- through the stream's own box and 'MVar'.
module Hyperdrive.Server.HTTP2
  ( serveHTTP2
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad (forM_, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (toList)
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (catMaybes, isJust)
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Data.Word (Word32, Word64)
import Hyperdrive.HTTP2.Connection (AbortReason(..), Input(..), Output(..), RequestHead(..), ResponseHead(..))
import qualified Hyperdrive.HTTP2.Connection as H2
import Hyperdrive.HTTP2.Frame (StreamId(..))
import Hyperdrive.Semantics.ControlData (RequestControl(..))
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Method (pattern HEAD)
import Hyperdrive.Semantics.Status (pattern ServiceUnavailable)
import Hyperdrive.Semantics.Target (TargetContext(..))
import Hyperdrive.Semantics.URI (pattern HTTP, pattern HTTPS)
import Hyperdrive.Server.Settings
import Hyperdrive.Server.Timer
import Hyperdrive.Server.Transport
import Hyperdrive.Server.Wait
import System.IO (IOMode(..), SeekMode(..), hFileSize, hSeek, withBinaryFile)
import System.Timeout (timeout)

------------------------------------------------------------------------------
-- State
------------------------------------------------------------------------------

data Env = Env
  { envSettings  :: !Settings
  , envTimer     :: !Timer
  , envWake      :: !(MVar Wake)
  , envTransport :: !Transport
  , envContext   :: !Context
  , envApp       :: Application
  , envConn      :: !(IORef H2.Conn)
  , envInbox     :: !(IORef [Input])          -- ^ from handlers, newest first (V2)
  , envOut       :: !(IORef (Seq ByteString)) -- ^ emitted by the core, not yet written
  , envWriteBy   :: !(IORef (Maybe Word64))   -- ^ queued output must progress by then (ms, V1)
  , envStreams   :: !(IORef (IntMap.IntMap (Stream, ThreadId))) -- ^ streams whose handlers run
  , envClosed    :: !(IORef Bool)             -- ^ the core said Close
  , envBroken    :: !(IORef Bool)             -- ^ the transport failed, or a write timed out
  , envShutdowns :: !(IORef Int)              -- ^ 'Shutdown' inputs fed: the second is the forced stage
  , envFull      :: !(IORef Bool)             -- ^ the last receive filled its buffer
  , envMayRead   :: !(IORef Bool)             -- ^ input may have come since a receive found none
  , envYielded   :: !(IORef Bool)             -- ^ yielded since the last progress
  , envPromising :: !(IORef Bool)             -- ^ a handler started or returned since the last yield or wait
  }

-- | What the owner and a stream's handler share (V3).
data Stream = Stream
  { streamWake :: !(MVar ())
  , streamBox  :: !(IORef Box)
  }

-- | What the owner has for a handler.
data Box = Box
  { boxContent  :: !(Seq ByteString)   -- ^ request content not yet read
  , boxEnd      :: !(Maybe Fields)     -- ^ the request's end, with its trailers
  , boxAborted  :: !(Maybe AbortReason)
  , boxWritable :: !Bool               -- ^ the answer to the last write (C5)
  }

------------------------------------------------------------------------------
-- The owner
------------------------------------------------------------------------------

-- | Serve an HTTP/2 connection whose first octets (its client preface,
-- and perhaps more) have been read. Returns when the connection has
-- ended and its handlers have returned or been killed.
serveHTTP2 :: Settings -> H2.Config -> Timer -> MVar Wake -> Transport -> Context -> Application -> ByteString -> IO ()
serveHTTP2 settings config timer wake transport context app first = do
  now <- clockMillis timer
  let target = H2.configTarget config
      config' = config { H2.configTarget = target { contextScheme = if contextSecure context then HTTPS else HTTP } }
      (conn, outs) = H2.newConn config' now
  env <- Env settings timer wake transport context app
    <$> newIORef conn <*> newIORef [] <*> newIORef Seq.empty <*> newIORef Nothing
    <*> newIORef IntMap.empty <*> newIORef False <*> newIORef False <*> newIORef 0 <*> newIORef False
    <*> newIORef True <*> newIORef False <*> newIORef False
  (do r <- try (act env outs >> feed env (Received first) >> loop env)
      case r of
        Right () -> pure ()
        Left e | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
               | Just (_ :: IOException) <- fromException e -> gone env
               | otherwise -> settingsOnException settings e >> gone env
      finish env)
    `finally` killHandlers env

-- | The transport failed or stopped taking output: the connection is
-- over, and the core tells the handlers (C2).
gone :: Env -> IO ()
gone env = do
  writeIORef (envBroken env) True
  feed env PeerClosed

-- | Until the core says Close: everything handlers said, what has
-- arrived, shutdown, time, and what can be written, in turn; then wait,
-- unless something happened.
loop :: Env -> IO ()
loop env = do
  closed <- readIORef (envClosed env)
  unless closed $ do
    a <- takeInbox env
    b <- receive env
    c <- shutdowns env
    d <- clock env
    e <- send env
    closed' <- readIORef (envClosed env)
    unless closed' $ do
      if a || b || c || d || e then writeIORef (envYielded env) False else do
        -- Nothing to do. If a handler has started or returned since the
        -- last yield or wait, let this capability's other threads run
        -- once before waiting: a handler just started is usually among
        -- them, and its response is then in the inbox with no wait; after
        -- one returns, the client's next request has had time to arrive
        -- (the HTTP/1.1 driver's yield, docs/benchmarks.md, 2026-11-20).
        -- Otherwise, the wait, which registers with the event manager:
        -- yielding during an upload, say, cost it 4.5% (2026-11-27).
        yielded <- readIORef (envYielded env)
        promising <- readIORef (envPromising env)
        writeIORef (envPromising env) False
        if yielded || not promising then wait env else yield >> writeIORef (envYielded env) True
        writeIORef (envMayRead env) True
      loop env

-- | Feed the core an input, and act on what it says.
feed :: Env -> Input -> IO ()
feed env input = do
  now <- clockMillis (envTimer env)
  conn <- readIORef (envConn env)
  let (conn', outs) = H2.step now input conn
  writeIORef (envConn env) $! conn'
  act env outs

act :: Env -> [Output] -> IO ()
act env = mapM_ $ \case
  Send bs -> do
    out <- readIORef (envOut env)
    when (Seq.null out) $ do
      -- queued output starts the write timeout (V1)
      now <- clockMillis (envTimer env)
      writeIORef (envWriteBy env) $! Just (now + writeTimeout env)
    writeIORef (envOut env) $! foldl (|>) out (filter (not . B.null) bs)
  NewRequest (StreamId sid) h -> writeIORef (envPromising env) True >> startHandler env sid h
  RequestData (StreamId sid) d -> deliver env sid (\b -> b { boxContent = boxContent b |> d })
  RequestEnd (StreamId sid) t -> deliver env sid (\b -> b { boxEnd = Just t })
  RequestAborted (StreamId sid) r -> deliver env sid (\b -> b { boxAborted = Just r })
  Writable (StreamId sid) -> deliver env sid (\b -> b { boxWritable = True })
  Close -> writeIORef (envClosed env) True
  ProtocolMisuse m -> settingsOnException (envSettings env) (toException (HTTP2DriverBug m))

-- the write timeout, in milliseconds
writeTimeout :: Env -> Word64
writeTimeout env = fromIntegral (timeoutWrite (settingsTimeouts (envSettings env))) `div` 1000

-- | Something for a stream's handler: into its box, and the handler
-- woken. A stream whose handler has returned has no box.
deliver :: Env -> Word32 -> (Box -> Box) -> IO ()
deliver env sid f = do
  streams <- readIORef (envStreams env)
  forM_ (IntMap.lookup (fromIntegral sid) streams) $ \(st, _) -> do
    atomicModifyIORef' (streamBox st) (\b -> (f b, ()))
    void (tryPutMVar (streamWake st) ())

-- | Feed what the handlers have said, in the order they said it.
takeInbox :: Env -> IO Bool
takeInbox env = do
  msgs <- atomicModifyIORef' (envInbox env) (\m -> ([], m))
  forM_ (reverse msgs) $ \m -> do
    case m of
      HandlerDone (StreamId sid) -> do
        writeIORef (envPromising env) True
        modifyIORef' (envStreams env) (IntMap.delete (fromIntegral sid))
      _ -> pure ()
    feed env m
  pure (not (null msgs))

-- | Receive what has arrived, if anything: the end of input is
-- 'PeerClosed'. After a receive that filled the transport's buffer
-- (16 KiB), more is likely waiting, an upload, say: the next takes all
-- that is waiting, up to 64 KiB, into a buffer of its size, kept without
-- a copy, so that fewer DATA frames are split between receives
-- (docs/benchmarks.md, 2026-11-27). Not more than 64 KiB: a DATA frame
-- of 16 KiB is then at least a quarter of its buffer, which the core
-- passes on without copying (C3); from 256 KiB each would be copied.
--
-- A receive that found nothing is not tried again until something may
-- have come: after a wait or a yield. Waiting registers for input at
-- once if there is some, so nothing is missed.
receive :: Env -> IO Bool
receive env = readIORef (envMayRead env) >>= \may -> if not may then pure False else do
  full <- readIORef (envFull env)
  r <- if full then transportTryRecvLarge (envTransport env) largeReceive else transportTryRecv (envTransport env)
  case r of
    Nothing -> writeIORef (envFull env) False >> writeIORef (envMayRead env) False >> pure False
    Just bs -> do
      writeIORef (envFull env) $! B.length bs >= 16384
      feed env (if B.null bs then PeerClosed else Received bs)
      pure True

largeReceive :: Int
largeReceive = 65536

-- | Shutdown, once when it begins and once when its grace period ends
-- (V5); each is fed once.
shutdowns :: Env -> IO Bool
shutdowns env = do
  n <- readIORef (envShutdowns env)
  stopping <- isShuttingDown (envTimer env)
  over <- isGraceOver (envTimer env)
  if n == 0 && stopping || n == 1 && over
    then writeIORef (envShutdowns env) (n + 1) >> feed env Shutdown >> pure True
    else pure False

-- | The core's deadline, and the write timeout.
clock :: Env -> IO Bool
clock env = do
  now <- clockMillis (envTimer env)
  conn <- readIORef (envConn env)
  writeBy <- readIORef (envWriteBy env)
  if maybe False (<= now) writeBy then gone env >> pure True
  else if maybe False (<= now) (H2.nextDeadline conn) then feed env TimedOut >> pure True
  else pure False

-- | Write what the socket takes now, and tell the core (C5). Progress
-- restarts the write timeout.
send :: Env -> IO Bool
send env = do
  out <- readIORef (envOut env)
  if Seq.null out then pure False else do
    n <- transportTrySend (envTransport env) (toList out)
    if n == 0 then pure False else do
      let rest = dropOctets n out
      writeIORef (envOut env) $! rest
      now <- clockMillis (envTimer env)
      writeIORef (envWriteBy env) $! if Seq.null rest then Nothing else Just (now + writeTimeout env)
      feed env (Sent n)
      pure True

-- the octets after the first n
dropOctets :: Int -> Seq ByteString -> Seq ByteString
dropOctets n q = case viewl q of
  EmptyL -> q
  b :< rest
    | B.length b <= n -> dropOctets (n - B.length b) rest
    | otherwise -> B.drop n b Seq.<| rest

-- | Nothing to do: wait for the socket (and its writability, with output
-- queued), the earliest deadline, shutdown, or a handler.
wait :: Env -> IO ()
wait env = do
  conn <- readIORef (envConn env)
  writeBy <- readIORef (envWriteBy env)
  out <- readIORef (envOut env)
  let deadline = case catMaybes [H2.nextDeadline conn, writeBy] of
        [] -> maxBound
        ds -> minimum ds * 1000000
  void $ waitFor (envTimer env) (envWake env) (envTransport env) True (not (Seq.null out)) deadline False (pending env)

-- | Work that does not wake the owner by itself being seen: a handler's
-- message, or a shutdown not yet fed.
pending :: Env -> IO Bool
pending env = do
  msgs <- readIORef (envInbox env)
  n <- readIORef (envShutdowns env)
  stopping <- isShuttingDown (envTimer env)
  over <- isGraceOver (envTimer env)
  pure (not (null msgs) || n == 0 && stopping || n == 1 && over)

-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.8
-- # A GOAWAY frame might not immediately precede closing of the
-- # connection; a receiver of a GOAWAY that has no more use for the
-- # connection SHOULD still send a GOAWAY frame before terminating the
-- # connection.

-- | The core has said Close: send what is left and close in stages (as
-- HTTP/1.1's driver does, so that a reset does not destroy the GOAWAY
-- before the client reads it), then give the handlers, which were told,
-- the grace period to return (C2, V4), and kill those that have not.
-- At the end of a shutdown's grace period, the handlers are killed at
-- once and there is no lingering.
finish :: Env -> IO ()
finish env = do
  forced <- (>= 2) <$> readIORef (envShutdowns env)
  broken <- readIORef (envBroken env)
  unless broken (closeInStages env forced)
  transportClose (envTransport env)
  grace <- if forced then pure 0 else pure (settingsHandlerGrace (envSettings env))
  deadline <- deadlineIn (envTimer env) grace
  let waitHandlers = do
        _ <- takeInbox env
        streams <- readIORef (envStreams env)
        unless (IntMap.null streams) $ do
          w <- waitFor (envTimer env) (envWake env) (envTransport env) False False deadline False
                 (not . null <$> readIORef (envInbox env))
          case w of
            WakeTimer _ -> pure ()
            _ -> waitHandlers
  unless forced waitHandlers

closeInStages :: Env -> Bool -> IO ()
closeInStages env forced = do
  out <- readIORef (envOut env)
  writeIORef (envOut env) Seq.empty
  r <- try $ do
    unless (Seq.null out) $ do
      armWrite (envTimer env) =<< deadlineIn (envTimer env) (timeoutWrite (settingsTimeouts (envSettings env)))
      transportSendMany (envTransport env) (toList out) `finally` disarm (envTimer env)
    unless forced $ do
      transportShutdownWrite (envTransport env)
      deadline <- deadlineIn (envTimer env) (timeoutLinger (settingsTimeouts (envSettings env)))
      let drain (n :: Int) = when (n < 1048576) $ transportTryRecv (envTransport env) >>= \case
            Just bs -> unless (B.null bs) (drain (n + B.length bs))
            Nothing -> waitFor (envTimer env) (envWake env) (envTransport env) True False deadline False (pure False) >>= \case
              WakeReadable -> drain n
              _ -> pure ()
      drain 0
  case r of
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure ()
    Right () -> pure ()

-- | Kill every handler still running, without waiting on any one.
killHandlers :: Env -> IO ()
killHandlers env = do
  streams <- readIORef (envStreams env)
  forM_ streams $ \(_, tid) -> forkIO (killThread tid)

------------------------------------------------------------------------------
-- Handlers
------------------------------------------------------------------------------

-- | Tell the owner something, and wake it (V2).
post :: Env -> [Input] -> IO ()
post env inputs = do
  atomicModifyIORef' (envInbox env) (\m -> (reverse inputs ++ m, ()))
  void (tryPutMVar (envWake env) WakeHandler)

startHandler :: Env -> Word32 -> RequestHead -> IO ()
startHandler env sid0 h = do
  let sid = fromIntegral sid0
  st <- Stream <$> newEmptyMVar <*> newIORef (Box Seq.empty Nothing Nothing False)
  tid <- mask_ $ forkIOWithUnmask $ \unmask ->
    unmask (runHandler env sid0 st h) `finally` post env [HandlerDone (StreamId sid0)]
  modifyIORef' (envStreams env) (IntMap.insert sid (st, tid))

runHandler :: Env -> Word32 -> Stream -> RequestHead -> IO ()
runHandler env sid0 st h = do
  let sid = StreamId sid0
      rc = requestControl h
      hasContent = requestHasContent h
      body = RequestBody
        { bodyLength   = if hasContent then controlContentLength rc else Just 0
        , readChunk    = if hasContent then readChunkFrom env sid st else pure B.empty
        , readTrailers = if hasContent then readTrailersFrom env sid st else pure emptyFields }
  case mkRequest HTTP_2 (controlMethod rc) (controlTarget rc) (controlFields rc) body of
    -- the core's 500 answers it when the handler is done
    Left e -> report (InvalidRequestFromCore e)
    Right req0 -> do
      let req = setAcceptsTrailers (controlAcceptsTrailers rc) req0
      started <- newIORef False
      r <- try (limited (envApp env (envContext env) req >>= sendResponse env sid st req started))
      hasStarted <- readIORef started
      case r of
        Right (Just ()) -> pure ()
        -- the handler time limit expired: 503, or the response cut off;
        -- on HTTP/2 only this stream ends (V4)
        Right Nothing
          | hasStarted -> post env [Abort sid]
          | otherwise -> do
              date <- dateNow (envTimer env)
              post env [ Respond sid (ResponseHead ServiceUnavailable (withDate date emptyFields) True (Just 0))
                       , EndBody sid emptyFields ]
        Left e
          | Just ConnectionClosed <- fromException e -> pure ()
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | otherwise -> do
              settingsOnException (envSettings env) e
              -- S3: the core answers 500 before the response starts;
              -- after, the stream is reset
              when hasStarted (post env [Abort sid])
  where
    report = settingsOnException (envSettings env) . toException
    limited act' = case settingsHandlerTimeLimit (envSettings env) of
      Nothing -> Just <$> act'
      Just t  -> timeout t act'

data Taken = Chunk !ByteString | Ended | Failed !AbortReason | Nothing'

-- | The next chunk of the request's content (V3, D6): what the owner has
-- passed on, its credit returned with 'Consumed'; with nothing there,
-- 'WantContent' and a wait. After the end, @""@. A stream that ended
-- early throws: 'IncompleteBody' if its content was not as declared,
-- 'ConnectionClosed' otherwise (D5).
readChunkFrom :: Env -> StreamId -> Stream -> IO ByteString
readChunkFrom env sid st = go False
  where
    go asked = do
      taken <- atomicModifyIORef' (streamBox st) $ \b -> case boxAborted b of
        Just r -> (b, Failed r)
        Nothing -> case viewl (boxContent b) of
          d :< rest -> (b { boxContent = rest }, Chunk d)
          EmptyL | isJust (boxEnd b) -> (b, Ended)
                 | otherwise -> (b, Nothing')
      case taken of
        Chunk d -> post env [Consumed sid (B.length d)] >> pure d
        Ended -> pure B.empty
        Failed ContentNotAsDeclared -> throwIO IncompleteBody
        Failed _ -> throwIO ConnectionClosed
        Nothing' -> do
          unless asked (post env [WantContent sid])
          takeMVar (streamWake st)
          go True

-- | Read and drop what is left of the content, then give its trailers.
readTrailersFrom :: Env -> StreamId -> Stream -> IO Fields
readTrailersFrom env sid st = do
  chunk <- readChunkFrom env sid st
  if not (B.null chunk) then readTrailersFrom env sid st else
    maybe emptyFields id . boxEnd <$> readIORef (streamBox st)

-- | Write response content and wait for its 'Writable' (C5): every
-- write gets exactly one, or the stream is aborted.
write :: Env -> StreamId -> Stream -> ByteString -> IO ()
write env sid st b = do
  post env [WriteBody sid b]
  let go = do
        r <- atomicModifyIORef' (streamBox st) $ \x -> case boxAborted x of
          Just _ -> (x, Just False)
          Nothing | boxWritable x -> (x { boxWritable = False }, Just True)
                  | otherwise -> (x, Nothing)
        case r of
          Just True -> pure ()
          Just False -> throwIO ConnectionClosed
          Nothing -> takeMVar (streamWake st) >> go
  go

sendResponse :: Env -> StreamId -> Stream -> Request -> IORef Bool -> Response -> IO ()
sendResponse env sid st req started resp0 = do
  resp <- either (throwIO . InvalidResponse) pure (checkResponseFor req resp0)
  date <- dateNow (envTimer env)
  let fields = withDate date (responseHeaders resp)
      begin hasContent len = do
        writeIORef started True
        pure (Respond sid (ResponseHead (responseStatus resp) fields hasContent len))
      aborted = isJust . boxAborted <$> readIORef (streamBox st)
  case responseContent resp of
    ContentNone -> begin False Nothing >>= \r -> post env [r]
    -- the head, the content and its end together: the content's
    -- Writable is not waited for, since nothing is written after it
    ContentBytes b -> begin True (Just (fromIntegral (B.length b))) >>= \r ->
      post env [r, WriteBody sid b, EndBody sid emptyFields]
    ContentFile path range -> withBinaryFile path ReadMode $ \hdl -> do
      size <- fromIntegral <$> hFileSize hdl
      (offset, len) <- case range of
        Nothing -> pure (0, size)
        Just r | rangeOffset r + rangeLength r <= size -> pure (rangeOffset r, rangeLength r)
               | otherwise -> throwIO FileRangeOutside
      begin True (Just len) >>= \r -> post env [r]
      -- a response to HEAD has no content to send; frames carry 16 KiB
      -- at most, so the file is read in pieces of that size (D5)
      unless (requestMethod req == HEAD) $ do
        hSeek hdl AbsoluteSeek (fromIntegral offset)
        let copy 0 = pure ()
            copy n = do
              chunk <- B.hGetSome hdl (fromIntegral (min n 16384))
              when (B.null chunk) (throwIO FileShrank)
              write env sid st chunk
              copy (n - fromIntegral (B.length chunk))
        copy (len :: Word64)
      post env [EndBody sid emptyFields]
    ContentStream len _ produce -> do
      begin True len >>= \r -> post env [r]
      written <- newIORef (0 :: Word64)
      let write' b = do
            gone' <- aborted
            when gone' (throwIO ConnectionClosed)
            modifyIORef' written (+ fromIntegral (B.length b))
            write env sid st b
      -- V3: the core sends what a write gives it as soon as it can, so
      -- there is nothing held back to flush
      trailers <- produce write' (pure ())
      n <- readIORef written
      case len of
        Just declared | declared /= n -> throwIO (StreamLengthMismatch declared n)
        _ -> pure ()
      checked <- either (throwIO . InvalidTrailers) pure (checkTrailers resp trailers)
      post env [EndBody sid checked]
