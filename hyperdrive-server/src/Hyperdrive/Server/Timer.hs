{-# LANGUAGE LambdaCase #-}
-- | Coarse timers for connections: one ticker thread for the whole
-- server, and per connection a deadline that is cheap to set. The same
-- registry carries the shutdown signal to each connection.
--
-- GHC's timer manager is exact, but every registration and
-- cancellation goes through one shared, locked structure; with one per
-- wait and per write, threads queue on it, and a benchmark ran at a
-- seventh of the speed it reached without timers. Here setting a
-- deadline is a write to the connection's own 'IORef', and the ticker
-- looks at every connection once per tick.
--
-- The ticker also keeps the time for the connections: the monotonic
-- clock as of its last tick, which deadlines are counted from
-- ('deadlineIn'), and the @Date@ field's value ('dateNow'). Reading the
-- clocks instead, three times a request (for the wait, the write and
-- the date), cost 0.09 microseconds of a small request's 2
-- (docs/benchmarks.md). A deadline allows for the clock being a tick
-- old, so a timeout is early only by as much as the ticker itself is
-- late, and fires up to two ticks late.
--
-- A connection is woken by calling the action it registered with
-- ('withTimer'), with a 'Wake' saying why. The connection then waits on
-- one variable of its own for whatever wakes it first: its socket,
-- its timer or shutdown. (A transaction over three 'TVar's, the first
-- design, cost a tenth of the time of a small request, and one shared
-- 'TVar' for shutdown was a point of contention.)
module Hyperdrive.Server.Timer
  ( Timers
  , withTimers
  , Timer
  , Wake(..)
  , withTimer
  , armRead
  , armWrite
  , disarm
  , deadlineIn
  , dateNow
  , beginShutdown
  , isShuttingDown
  , endShutdownGrace
  , isGraceOver
  , clockMillis
  ) where

import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Exception (bracket, mask_)
import Control.Monad (forM_, forever, when)
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperdrive.Semantics.Field (FieldValue)
import Hyperdrive.Server.Date

data Timers = Timers
  { timersRegistry :: !(IORef (IntMap.IntMap Timer))
  , timersNext     :: !(IORef Int)
  , timersStopping :: !(IORef Bool)
  , timersGraceOver :: !(IORef Bool)
  , timersClock    :: !(IORef Word64)  -- ^ monotonic nanoseconds at the last tick
  , timersTick     :: !Word64          -- ^ nanoseconds between ticks
  , timersDate     :: !DateCache
  }

-- | Why a connection was woken.
data Wake
  = WakeReadable      -- ^ its socket has input (or the end of it)
  | WakeTimer !Int    -- ^ the read arming with this generation expired
  | WakeShutdown      -- ^ the server is shutting down, or its grace period has ended
  | WakeWritable      -- ^ its socket would take output (HTTP/2's owner, V1)
  | WakeHandler       -- ^ a handler has something for it (HTTP/2's owner, V2)
  deriving (Eq, Show)

-- | A connection's timer: what it is armed for, and until when
-- (monotonic nanoseconds).
data Timer = Timer
  { timerState    :: !(IORef Armed)
  , timerGen      :: !(IORef Int)   -- ^ counts read armings, so that each is told apart
  , timerWake     :: Wake -> IO ()  -- ^ wakes the connection
  , timerAbort    :: IO ()          -- ^ what to do when a write expires
  , timerStopping :: !(IORef Bool)  -- ^ the server is shutting down
  , timerGraceOver :: !(IORef Bool) -- ^ and its grace period has ended
  , timerClock    :: !(IORef Word64) -- ^ the server's 'timersClock'
  , timerTick     :: !Word64
  , timerDate     :: !DateCache
  }

data Armed
  = Disarmed
  | Reading !Int !Word64  -- ^ generation, deadline
  | Writing !Word64       -- ^ deadline

-- | Run with a ticker that checks every connection's timer, and
-- brings the clock and the date up to date, every given number of
-- microseconds, stopping it afterwards.
withTimers :: Int -> (Timers -> IO a) -> IO a
withTimers resolution act = do
  timers <- Timers <$> newIORef IntMap.empty <*> newIORef 0 <*> newIORef False <*> newIORef False
                   <*> (newIORef =<< getMonotonicTimeNSec) <*> pure (fromIntegral resolution * 1000)
                   <*> newDateCache
  let tick = do
        now <- getMonotonicTimeNSec
        writeIORef (timersClock timers) now
        refreshDate (timersDate timers)
        registry <- readIORef (timersRegistry timers)
        forM_ registry $ \t -> do
          fire <- atomicModifyIORef' (timerState t) $ \case
            Reading gen deadline | deadline <= now -> (Disarmed, timerWake t (WakeTimer gen))
            Writing deadline     | deadline <= now -> (Disarmed, timerAbort t)
            s                                      -> (s, pure ())
          fire
  bracket (forkIOWithUnmask (\unmask -> unmask (forever (threadDelay resolution >> tick))))
          killThread
          (const (act timers))

-- | A timer for one connection, for the duration of the action. The
-- first action wakes the connection; it is called on the ticker's
-- thread, or the thread shutting the server down, so it must be quick
-- and must not block. The second runs when a write expires, and should
-- make the blocked write fail, by shutting the socket down.
withTimer :: Timers -> (Wake -> IO ()) -> IO () -> (Timer -> IO a) -> IO a
withTimer timers wake abort act = do
  t <- Timer <$> newIORef Disarmed <*> newIORef 0 <*> pure wake <*> pure abort <*> newIORef False <*> newIORef False
             <*> pure (timersClock timers) <*> pure (timersTick timers) <*> pure (timersDate timers)
  bracket (mask_ (register t)) unregister (const (act t))
  where
    register t = do
      k <- atomicModifyIORef' (timersNext timers) (\n -> (n + 1, n))
      atomicModifyIORef' (timersRegistry timers) (\m -> (IntMap.insert k t m, ()))
      -- either 'beginShutdown' sees this timer, or this sees its flag
      stopping <- atomicModifyIORef' (timersStopping timers) (\b -> (b, b))
      when stopping (atomicWriteIORef (timerStopping t) True)
      over <- atomicModifyIORef' (timersGraceOver timers) (\b -> (b, b))
      when over (atomicWriteIORef (timerGraceOver t) True)
      pure k
    unregister k = atomicModifyIORef' (timersRegistry timers) (\m -> (IntMap.delete k m, ()))

-- | Arm the timer for a wait for input that must end by the deadline,
-- giving the generation its 'WakeTimer' will carry. A 'WakeTimer' with
-- another generation belongs to an earlier wait, and is to be ignored.
armRead :: Timer -> Word64 -> IO Int
armRead t deadline = do
  gen <- atomicModifyIORef' (timerGen t) (\n -> (n + 1, n + 1))
  writeIORef (timerState t) (Reading gen deadline)
  pure gen

-- | Arm the timer for a write that must finish by the deadline.
armWrite :: Timer -> Word64 -> IO ()
armWrite t deadline = writeIORef (timerState t) (Writing deadline)

disarm :: Timer -> IO ()
disarm t = writeIORef (timerState t) Disarmed

-- | A deadline the given number of microseconds from now, in the
-- timers' clock (monotonic nanoseconds). Now is taken to be the last
-- tick plus the time between ticks, which is the latest it can be
-- unless the ticker is late: the deadline is not early.
deadlineIn :: Timer -> Int -> IO Word64
deadlineIn t micros = do
  ticked <- readIORef (timerClock t)
  pure $! ticked + timerTick t + fromIntegral micros * 1000

-- | The value of the @Date@ field, as of the last tick.
dateNow :: Timer -> IO FieldValue
dateNow t = currentDate (timerDate t)

-- | Tell every connection, present and future, that the server is
-- shutting down, and wake each.
beginShutdown :: Timers -> IO ()
beginShutdown timers = do
  atomicModifyIORef' (timersStopping timers) (const (True, ()))
  registry <- readIORef (timersRegistry timers)
  forM_ registry $ \t -> do
    atomicWriteIORef (timerStopping t) True
    timerWake t WakeShutdown -- begun

-- | Whether the server is shutting down. A connection checks this
-- before it waits, so that it does not miss a shutdown that began
-- before it registered its wake action.
isShuttingDown :: Timer -> IO Bool
isShuttingDown t = readIORef (timerStopping t)

-- | Tell every connection that the shutdown's grace period has ended,
-- and wake each: the HTTP/2 driver then resets its streams left and
-- closes (D10's forced stage, V5) before the server kills what is still
-- running. The HTTP/1.1 driver has nothing more to do.
endShutdownGrace :: Timers -> IO ()
endShutdownGrace timers = do
  atomicModifyIORef' (timersGraceOver timers) (const (True, ()))
  registry <- readIORef (timersRegistry timers)
  forM_ registry $ \t -> do
    atomicWriteIORef (timerGraceOver t) True -- told
    timerWake t WakeShutdown -- grace

isGraceOver :: Timer -> IO Bool
isGraceOver t = readIORef (timerGraceOver t)

-- | The timers' clock as of the last tick, in milliseconds: the time
-- the HTTP/2 core is given (its C1).
clockMillis :: Timer -> IO Word64
clockMillis t = (`div` 1000000) <$> readIORef (timerClock t)
