{-# LANGUAGE LambdaCase #-}
-- | Coarse timers for connections: one ticker thread for the whole
-- server, and per connection a deadline that is cheap to set.
--
-- GHC's timer manager is exact, but every registration and
-- cancellation goes through one shared, locked structure; with one per
-- wait and per write, threads queue on it, and a benchmark ran at a
-- seventh of the speed it reached without timers. Here setting a
-- deadline is a write to the connection's own 'IORef', and the ticker
-- looks at every connection once per tick, so a timeout fires up to a
-- tick late.
module Hyperdrive.Server.Timer
  ( Timers
  , withTimers
  , Timer
  , withTimer
  , armRead
  , armWrite
  , disarm
  , deadlineIn
  ) where

import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, mask_)
import Control.Monad (forM_, forever, when)
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

data Timers = Timers
  { timersRegistry :: !(IORef (IntMap.IntMap Timer))
  , timersNext     :: !(IORef Int)
  }

-- | A connection's timer: what it is armed for, until when (monotonic
-- nanoseconds), and which arming has fired.
data Timer = Timer
  { timerState :: !(IORef Armed)
  , timerGen   :: !(IORef Int)    -- ^ counts read armings, so that each is told apart
  , timerFired :: !(TVar Int)     -- ^ the generation of the last read arming that expired
  , timerAbort :: IO ()           -- ^ what to do when a write expires
  }

data Armed
  = Disarmed
  | Reading !Int !Word64  -- ^ generation, deadline
  | Writing !Word64       -- ^ deadline

-- | Run with a ticker that checks every connection's timer every given
-- number of microseconds, stopping it afterwards.
withTimers :: Int -> (Timers -> IO a) -> IO a
withTimers resolution act = do
  timers <- Timers <$> newIORef IntMap.empty <*> newIORef 0
  let tick = do
        now <- getMonotonicTimeNSec
        registry <- readIORef (timersRegistry timers)
        forM_ registry $ \t -> do
          fire <- atomicModifyIORef' (timerState t) $ \case
            Reading gen deadline | deadline <= now -> (Disarmed, atomically (writeTVar (timerFired t) gen))
            Writing deadline     | deadline <= now -> (Disarmed, timerAbort t)
            s                                      -> (s, pure ())
          fire
  bracket (forkIOWithUnmask (\unmask -> unmask (forever (threadDelay resolution >> tick))))
          killThread
          (const (act timers))

-- | A timer for one connection, for the duration of the action. The
-- given action runs, on the ticker's thread, when a write expires; it
-- should make the blocked write fail, by shutting the socket down.
withTimer :: Timers -> IO () -> (Timer -> IO a) -> IO a
withTimer timers abort act = do
  t <- Timer <$> newIORef Disarmed <*> newIORef 0 <*> newTVarIO 0 <*> pure abort
  bracket (mask_ (register t)) unregister (const (act t))
  where
    register t = do
      k <- atomicModifyIORef' (timersNext timers) (\n -> (n + 1, n))
      atomicModifyIORef' (timersRegistry timers) (\m -> (IntMap.insert k t m, ()))
      pure k
    unregister k = atomicModifyIORef' (timersRegistry timers) (\m -> (IntMap.delete k m, ()))

-- | Arm the timer for a wait for input that must end by the deadline,
-- and give the STM action that succeeds once it expires.
armRead :: Timer -> Word64 -> IO (STM ())
armRead t deadline = do
  gen <- atomicModifyIORef' (timerGen t) (\n -> (n + 1, n + 1))
  writeIORef (timerState t) (Reading gen deadline)
  -- an expiry of an earlier arming has an earlier generation
  pure (readTVar (timerFired t) >>= \fired -> when (fired /= gen) retry)

-- | Arm the timer for a write that must finish by the deadline.
armWrite :: Timer -> Word64 -> IO ()
armWrite t deadline = writeIORef (timerState t) (Writing deadline)

disarm :: Timer -> IO ()
disarm t = writeIORef (timerState t) Disarmed

-- | Now, in the timers' clock (monotonic nanoseconds), plus the given
-- number of microseconds.
deadlineIn :: Int -> IO Word64
deadlineIn micros = (+ fromIntegral micros * 1000) <$> getMonotonicTimeNSec
