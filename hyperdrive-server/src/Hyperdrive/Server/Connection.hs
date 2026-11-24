{-# LANGUAGE OverloadedStrings, PatternSynonyms, LambdaCase, BangPatterns #-}
-- | Serving one HTTP/1.1 connection: the IO around
-- 'Hyperdrive.HTTP1.Connection.step' (docs/server-design.md).
--
-- Everything for a connection happens in one thread, the application's
-- handler included, so the connection's state is an 'IORef' that only
-- that thread touches.
module Hyperdrive.Server.Connection
  ( -- * Applications
    Application
  , Context(..)
    -- * Settings
  , Settings(..)
  , Timeouts(..)
  , defaultSettings
  , defaultTimeouts
    -- * Serving
  , serveConnection
  , ConnectionClosed(..)
  , ServerError(..)
  ) where

import Control.Concurrent (yield)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Hyperdrive.HTTP1.Connection
import Hyperdrive.HTTP1.Target (TargetContext(..))
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Method (pattern HEAD)
import Hyperdrive.Semantics.Status (Status, pattern InternalServerError, pattern ServiceUnavailable)
import Hyperdrive.Semantics.URI (pattern HTTP, pattern HTTPS)
import Hyperdrive.Server.Timer
import Hyperdrive.Server.Transport
import Network.Socket (HostName, PortNumber, SockAddr)
import System.IO (IOMode(..), SeekMode(..), hFileSize, hSeek, withBinaryFile)
import System.Timeout (timeout)

------------------------------------------------------------------------------
-- Applications and settings
------------------------------------------------------------------------------

-- | An application answers each request with a response (S2).
type Application = Context -> Request -> IO Response

-- | What the server knows about the connection a request came on
-- (design note M8).
data Context = Context
  { contextPeer   :: !SockAddr
  , contextLocal  :: !SockAddr
  , contextSecure :: !Bool
  } deriving (Show)

-- | Timeouts by phase, in microseconds (D8, S7).
data Timeouts = Timeouts
  { timeoutIdle    :: !Int  -- ^ between requests, until the first byte of the next
  , timeoutHead    :: !Int  -- ^ from the first byte of a head to its end, in total
  , timeoutBody    :: !Int  -- ^ while reading a body, since bytes last arrived
  , timeoutDiscard :: !Int  -- ^ discarding an unread body, in total
  , timeoutWrite   :: !Int  -- ^ each write, while the client does not read
  , timeoutLinger  :: !Int  -- ^ after half-closing, before closing
  } deriving (Eq, Show)

defaultTimeouts :: Timeouts
defaultTimeouts = Timeouts
  { timeoutIdle = 60 * second, timeoutHead = 30 * second, timeoutBody = 30 * second
  , timeoutDiscard = 5 * second, timeoutWrite = 30 * second, timeoutLinger = 2 * second }
  where second = 1000000

data Settings = Settings
  { settingsHost             :: !(Maybe HostName)  -- ^ 'Nothing': every interface
  , settingsPort             :: !PortNumber        -- ^ 0: any free port
  , settingsConfig           :: !Config            -- ^ the protocol core's limits (D4, D6)
  , settingsTimeouts         :: !Timeouts
  , settingsShutdownGrace    :: !Int               -- ^ microseconds before remaining connections are killed
  , settingsHandlerTimeLimit :: !(Maybe Int)       -- ^ microseconds a handler may run; 'Nothing': no limit
  , settingsMaxConnections   :: !(Maybe Int)
  , settingsPinConnections   :: !Bool
    -- ^ give each connection a capability in turn when it is accepted,
    -- and keep its thread there. For small, even requests this saves
    -- waking threads across capabilities (7% more of the smallest
    -- requests a second, docs/benchmarks.md). But the scheduler can no
    -- longer move a connection's thread to an idle capability, so a
    -- handler that computes for long delays the others on its
    -- capability: off unless handlers are short and alike.
  , settingsTimerResolution  :: !Int
    -- ^ microseconds between checks of the connections' timers: a
    -- timeout fires up to twice this late, not early, and the @Date@
    -- field is up to this much behind (see "Hyperdrive.Server.Timer")
  , settingsOnException      :: SomeException -> IO ()
    -- ^ told of exceptions from applications, and of bugs; not of
    -- clients going away
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingsHost = Nothing
  , settingsPort = 8080
  , settingsConfig = defaultConfig
  , settingsTimeouts = defaultTimeouts
  , settingsShutdownGrace = 30000000
  , settingsHandlerTimeLimit = Nothing
  , settingsMaxConnections = Nothing
  , settingsPinConnections = False
  , settingsTimerResolution = 100000
  , settingsOnException = \e -> putStrLn ("hyperdrive: " ++ displayException e)
  }

-- | Thrown by a 'ContentStream''s write and flush, and by writes in
-- general, once the connection is closed or the client has gone.
data ConnectionClosed = ConnectionClosed
  deriving (Eq, Show)

instance Exception ConnectionClosed

-- | What the driver reports to 'settingsOnException' besides the
-- application's own exceptions.
data ServerError
  = DriverBug !Misuse                     -- ^ the driver fed the core an input out of order
  | InvalidRequestFromCore !RequestError  -- ^ the core handed over a head 'mkRequest' rejects
  | InvalidResponse !ResponseError        -- ^ the response is not allowed for the request
  | InvalidTrailers !TrailerError         -- ^ a stream returned trailers it may not send
  | StreamLengthMismatch !Word64 !Word64  -- ^ a stream wrote other than the length it declared
  | FileRangeOutside                      -- ^ a file range beyond the end of the file
  | FileShrank                            -- ^ a file ended before its announced length
  deriving (Show)

instance Exception ServerError

------------------------------------------------------------------------------
-- State
------------------------------------------------------------------------------

data Env = Env
  { envSettings :: !Settings
  , envWake     :: !(MVar Wake)     -- ^ filled by whatever wakes the connection
  , envTransport :: !Transport
  , envContext  :: !Context
  , envApp      :: Application
  , envTimer    :: !Timer
    -- The connection's state, each part in a reference of its own: as
    -- one record in one reference, every change (a dozen a request)
    -- allocated the whole record again. Each is written evaluated:
    -- written lazily, a write leaves a thunk for the next read to run
    -- (ticky counted about ten a request), and one that refers to the
    -- value before it keeps every earlier one alive.
  , envConn     :: !(IORef Conn)
  , envPending  :: !(IORef [Output])      -- ^ outputs not yet acted on, oldest first
  , envOut      :: !(IORef [ByteString])  -- ^ to write, newest first
  , envOutBytes :: !(IORef Int)
  , envWrote    :: !(IORef Bool)          -- ^ something was written since the last wait for input
  , envClosed   :: !(IORef Bool)          -- ^ the core said Close
  , envBroken   :: !(IORef Bool)          -- ^ a write failed: the client has gone
  , envShutdownSeen :: !(IORef Bool)
  , envTimerFor :: !(IORef TimerFor)      -- ^ the phase the timer is for, and its deadline
  }

-- | Strict, and the deadline evaluated before it is stored: a lazy
-- deadline that referred to the previous state kept every earlier
-- state alive for as long as the connection lasted.
data TimerFor = TimerFor !Phase !Word64

data BodyState = BodyOpen | BodyEnded !Fields | BodyFailed

------------------------------------------------------------------------------
-- The connection
------------------------------------------------------------------------------

-- | Serve one connection until it closes. Its timeouts, and the signal
-- to shut down ('beginShutdown'), come through the given 'Timers' (see
-- 'withTimers'). The transport is closed at the end whatever happens;
-- asynchronous exceptions (the thread being killed) are rethrown after
-- that.
serveConnection :: Settings -> Timers -> Transport -> Context -> Application -> IO ()
serveConnection settings timers transport context app = do
  wake <- newEmptyMVar
  withTimer timers (void . tryPutMVar wake) (transportAbort transport) $ \timer -> do
    let config = settingsConfig settings
        target = configTarget config
        config' = config { configTarget = target { contextScheme = if contextSecure context then HTTPS else HTTP } }
    env <- Env settings wake transport context app timer
      <$> newIORef (newConn config') <*> newIORef [] <*> newIORef [] <*> newIORef 0
      <*> newIORef False <*> newIORef False <*> newIORef False <*> newIORef False
      <*> newIORef (TimerFor Closed 0)
    (do r <- try (mainLoop env)
        case r of
          -- what the core had to say before closing (a 400, say) is still sent
          Left e | Just ConnectionClosed <- fromException e -> linger env
                 | Just (_ :: IOException) <- fromException e -> pure ()
                 | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
                 | otherwise -> settingsOnException settings e
          Right () -> linger env)
      `finally` transportClose transport

mainLoop :: Env -> IO ()
mainLoop env = do
  closed <- readIORef (envClosed env)
  unless closed $ do
    pending <- readIORef (envPending env)
    case pending of
      [] -> waitInput env
      (o : rest) -> do
        writeIORef (envPending env) $! rest
        case o of
          NeedInput    -> waitInput env
          NewRequest h -> serveRequest env h
          _            -> pure ()
    mainLoop env

-- | Feed the core an input, and act on what it says: writes are
-- buffered, Close is noted, misuse reported, and the rest queued for
-- whoever is waiting for it.
feed :: Env -> Input -> IO ()
feed env input = do
  conn <- readIORef (envConn env)
  let (conn', outs) = step input conn
  writeIORef (envConn env) $! conn'
  mapM_ act outs
  buffered <- readIORef (envOutBytes env)
  when (buffered > 16384) (flush env)
  where
    act = \case
      Send bs -> do
        modifyIORef' (envOut env) (reverse bs ++)
        modifyIORef' (envOutBytes env) (+ sum (map B.length bs))
      Close -> writeIORef (envClosed env) True
      ProtocolMisuse m -> settingsOnException (envSettings env) (toException (DriverBug m))
      o -> modifyIORef' (envPending env) (++ [o])

-- | Write what is buffered, in one vectored write. If the client has
-- gone, or does not read within the write timeout, the connection is
-- broken and 'ConnectionClosed' thrown.
flush :: Env -> IO ()
flush env = do
  isBroken <- readIORef (envBroken env)
  when isBroken (throwIO ConnectionClosed)
  out <- readIORef (envOut env)
  unless (null out) $ do
    writeIORef (envOut env) []
    writeIORef (envOutBytes env) 0
    writeIORef (envWrote env) True
    -- a write that does not finish in time is made to fail by the
    -- ticker, which aborts the transport
    armWrite (envTimer env) =<< deadlineIn (envTimer env) (timeoutWrite (settingsTimeouts (envSettings env)))
    r <- try (transportSendMany (envTransport env) (reverse out))
    disarm (envTimer env)
    case r of
      Right () -> pure ()
      Left (_ :: IOException) -> broken
  where
    broken = do
      writeIORef (envBroken env) True
      writeIORef (envClosed env) True
      throwIO ConnectionClosed

-- | Wait for input, the phase's timer, or shutdown, whichever comes
-- first (S4), and feed the core what happened.
waitInput :: Env -> IO ()
waitInput env = do
  flush env  -- what is buffered goes out before waiting
  -- After a response the client's next request takes a round trip, so
  -- a receive straight away nearly always finds nothing, and waiting
  -- for the socket costs an epoll_ctl and a wake-up through the event
  -- manager. Letting this capability's other connections run first
  -- usually gives it time to arrive (warp does the same): with 100
  -- connections, receives that found nothing fell from one per request
  -- to under two per hundred (docs/benchmarks.md).
  wrote <- readIORef (envWrote env)
  when wrote yield
  conn <- readIORef (envConn env)
  shutdownSeen <- readIORef (envShutdownSeen env)
  TimerFor ph' deadline' <- readIORef (envTimerFor env)
  let ph = phase conn
      ts = settingsTimeouts (envSettings env)
      limit = case ph of
        Idle           -> timeoutIdle ts
        ReadingHead    -> timeoutHead ts
        ReadingBody    -> timeoutBody ts
        DiscardingBody -> timeoutDiscard ts
        _              -> timeoutIdle ts
  fresh <- deadlineIn (envTimer env) limit
  let -- the head and discarding timers run from the start of the phase;
      -- the others restart at each wait
      !deadline
        | ph' == ph, ph `elem` [ReadingHead, DiscardingBody] = deadline'
        | otherwise = fresh
  writeIORef (envTimerFor env) $! TimerFor ph deadline
  writeIORef (envWrote env) False
  ready <- transportTryRecv (envTransport env)
  case ready of
    Just bs -> received env bs  -- already there
    Nothing -> waitFor env shutdownSeen deadline

-- | Feed what was received; nothing means the end of input.
received :: Env -> ByteString -> IO ()
received env bs = feed env (if B.null bs then PeerClosed else Received bs)

-- | Nothing has arrived: wait for input, the timer or shutdown.
waitFor :: Env -> Bool -> Word64 -> IO ()
waitFor env shutdownSeen deadline = do
  woken <- wakeOn env deadline (not shutdownSeen)
  case woken of
    WakeReadable -> do
      ready <- transportTryRecv (envTransport env)
      case ready of
        Just bs -> received env bs
        Nothing -> waitFor env shutdownSeen deadline  -- woken for nothing: wait again
    WakeTimer _ -> feed env TimedOut
    WakeShutdown -> do
      writeIORef (envShutdownSeen env) True
      feed env Shutdown

-- | Block until the socket is readable, the timer set for the deadline
-- expires, or (if asked) the server shuts down. Wakings left over from
-- earlier waits are ignored.
wakeOn :: Env -> Word64 -> Bool -> IO Wake
wakeOn env deadline forShutdown = do
  _ <- tryTakeMVar (envWake env)
  gen <- armRead (envTimer env) deadline
  stopping <- isShuttingDown (envTimer env)
  if forShutdown && stopping then disarm (envTimer env) >> pure WakeShutdown else do
    cancel <- transportOnReadable (envTransport env) (void (tryPutMVar (envWake env) WakeReadable))
    let next = takeMVar (envWake env) >>= \case
          WakeTimer g | g /= gen -> next
          WakeShutdown | not forShutdown -> next
          w -> pure w
    w <- next `onException` (cancel >> disarm (envTimer env))
    cancel
    disarm (envTimer env)
    pure w

-- = https://www.rfc-editor.org/rfc/rfc9112#section-9.6
-- # If a server performs an immediate close of a TCP connection, there is
-- # a significant risk that the client will not be able to read the last
-- # HTTP response.

-- | Close in stages: send what is left, half-close, and read and drop
-- what the client still sends until it closes or the linger timer
-- expires (RFC 9112 Section 9.6).
linger :: Env -> IO ()
linger env = do
  r <- try (flush env)
  case r of
    Left ConnectionClosed -> pure ()
    Right () -> do
      transportShutdownWrite (envTransport env)
      deadline <- deadlineIn (envTimer env) (timeoutLinger (settingsTimeouts (envSettings env)))
      let drain (n :: Int) = when (n < 1048576) $ do
            ready <- transportTryRecv (envTransport env)
            case ready of
              Just bs -> unless (B.null bs) (drain (n + B.length bs))
              Nothing -> wakeOn env deadline False >>= \case
                WakeReadable -> drain n
                _            -> pure ()
      drain 0

------------------------------------------------------------------------------
-- Requests
------------------------------------------------------------------------------

serveRequest :: Env -> RequestHead -> IO ()
serveRequest env h = do
  bodyRef <- newIORef (if reqBodyLength h == Just 0 then BodyEnded emptyFields else BodyOpen)
  let body = RequestBody
        { bodyLength   = reqBodyLength h
        , readChunk    = readChunkFrom env bodyRef
        , readTrailers = readTrailersFrom env bodyRef }
  case mkRequest (reqVersion h) (reqMethod h) (reqTarget h) (reqFields h) body of
    Left e -> do
      report (InvalidRequestFromCore e)
      respondWithStatus env InternalServerError
    Right req0 -> do
      let req = setAcceptsTrailers (reqAcceptsTrailers h) req0
      started <- newIORef False
      r <- try (limited (envApp env (envContext env) req >>= sendResponse env req started))
      hasStarted <- readIORef started
      case r of
        Right (Just ()) -> pure ()
        -- the handler time limit expired: 503 and close, or cut the response off
        Right Nothing
          | hasStarted -> feed env Abort
          | otherwise -> feed env Shutdown >> respondWithStatus env ServiceUnavailable
        Left e
          | Just ConnectionClosed <- fromException e -> throwIO ConnectionClosed
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | otherwise -> do
              settingsOnException (envSettings env) e
              -- S3: 500 before the response starts, cut off after
              if hasStarted then feed env Abort else respondWithStatus env InternalServerError
  modifyIORef' bodyRef $ \case
    BodyOpen -> BodyFailed
    b        -> b
  where
    report = settingsOnException (envSettings env) . toException
    limited act = case settingsHandlerTimeLimit (envSettings env) of
      Nothing -> Just <$> act
      Just t  -> timeout t act

-- | The next chunk of the body: feed WantBody, and wait for the answer.
readChunkFrom :: Env -> IORef BodyState -> IO ByteString
readChunkFrom env bodyRef = readIORef bodyRef >>= \case
  BodyEnded _ -> pure B.empty
  BodyFailed  -> throwIO IncompleteBody
  BodyOpen    -> feed env WantBody >> answer
  where
    answer = do
      pending <- readIORef (envPending env)
      closed <- readIORef (envClosed env)
      let rest' outs = writeIORef (envPending env) $! outs
      case pending of
        (RequestData b : RequestEnd t : outs) -> rest' outs >> writeIORef bodyRef (BodyEnded t) >> pure b
        (RequestData b : outs) -> rest' outs >> pure b
        (RequestEnd t : outs) -> rest' outs >> writeIORef bodyRef (BodyEnded t) >> pure B.empty
        (RequestAborted : outs) -> rest' outs >> failed
        (NeedInput : outs) | not closed -> rest' outs >> waitInput env >> answer
        _ -> failed
    failed = writeIORef bodyRef BodyFailed >> throwIO IncompleteBody

-- | Read and drop what is left of the body, then give its trailers.
readTrailersFrom :: Env -> IORef BodyState -> IO Fields
readTrailersFrom env bodyRef = do
  chunk <- readChunkFrom env bodyRef
  if not (B.null chunk) then readTrailersFrom env bodyRef else readIORef bodyRef >>= \case
    BodyEnded t -> pure t
    _           -> throwIO IncompleteBody

------------------------------------------------------------------------------
-- Responses
------------------------------------------------------------------------------

-- | If the server began shutting down while the application worked,
-- tell the core now, so that the response says the connection closes.
noticeShutdown :: Env -> IO ()
noticeShutdown env = do
  seen <- readIORef (envShutdownSeen env)
  unless seen $ do
    stopping <- isShuttingDown (envTimer env)
    when stopping $ do
      writeIORef (envShutdownSeen env) True
      feed env Shutdown

-- | A response with no content, for errors the driver answers itself.
respondWithStatus :: Env -> Status -> IO ()
respondWithStatus env status = do
  noticeShutdown env
  date <- dateNow (envTimer env)
  feed env (Respond (ResponseHead status (withDate date emptyFields) True (Just 0) []))
  feed env (EndBody emptyFields)
  flush env

withDate :: FieldValue -> Fields -> Fields
withDate date fs
  | null (lookupAll hDate fs) = appendField hDate date fs
  | otherwise = fs

hDate :: FieldName
hDate = fromJust (mkFieldName "date")

sendResponse :: Env -> Request -> IORef Bool -> Response -> IO ()
sendResponse env req started resp0 = do
  resp <- either (throwIO . InvalidResponse) pure (checkResponseFor req resp0)
  date <- dateNow (envTimer env)
  let fields = withDate date (responseHeaders resp)
      begin hasContent len trailers = do
        noticeShutdown env
        writeIORef started True
        feed env (Respond (ResponseHead (responseStatus resp) fields hasContent len trailers))
  case responseContent resp of
    ContentNone -> begin False Nothing [] >> feed env (EndBody emptyFields)
    ContentBytes b -> do
      begin True (Just (fromIntegral (B.length b))) []
      feed env (WriteBody b)
      feed env (EndBody emptyFields)
    ContentFile path range -> withBinaryFile path ReadMode $ \hdl -> do
      size <- fromIntegral <$> hFileSize hdl
      (offset, len) <- case range of
        Nothing -> pure (0, size)
        Just r | rangeOffset r + rangeLength r <= size -> pure (rangeOffset r, rangeLength r)
               | otherwise -> throwIO FileRangeOutside
      begin True (Just len) []
      -- a response to HEAD has no content to send (S6: read and write in pieces)
      unless (requestMethod req == HEAD) $ do
        hSeek hdl AbsoluteSeek (fromIntegral offset)
        let copy 0 = pure ()
            copy n = do
              chunk <- B.hGetSome hdl (fromIntegral (min n 65536))
              when (B.null chunk) (throwIO FileShrank)
              feed env (WriteBody chunk)
              copy (n - fromIntegral (B.length chunk))
        copy len
      feed env (EndBody emptyFields)
    ContentStream len names produce -> do
      begin True len names
      written <- newIORef (0 :: Word64)
      let write b = do
            closed <- readIORef (envClosed env)
            when closed (throwIO ConnectionClosed)
            modifyIORef' written (+ fromIntegral (B.length b))
            feed env (WriteBody b)
      trailers <- produce write (flush env)
      n <- readIORef written
      case len of
        Just declared | declared /= n -> throwIO (StreamLengthMismatch declared n)
        _ -> pure ()
      checked <- either (throwIO . InvalidTrailers) pure (checkTrailers resp trailers)
      feed env (EndBody checked)
  flush env
