{-# LANGUAGE LambdaCase #-}
-- | Waiting for whatever wakes a connection: its socket, its timer,
-- shutdown, and (HTTP/2) its handlers.
module Hyperdrive.Server.Wait
  ( waitFor
  ) where

import Control.Concurrent.MVar
import Control.Exception (onException)
import Control.Monad (void)
import Data.Word (Word64)
import Hyperdrive.Server.Timer
import Hyperdrive.Server.Transport

-- | Block until the socket is readable (if asked), writable (if asked),
-- the timer set for the deadline (timers' nanoseconds) expires, the
-- server shuts down, or something else fills the connection's 'MVar'.
-- If asked, a shutdown already begun returns 'WakeShutdown' at once (the
-- caller that has acted on it asks not to be told again). Wakings left over from earlier waits are dropped first; the
-- given check is made after that, so that work that arrived before the
-- wait (a handler's message, whose wake-up was just dropped) is not
-- slept through: if it says there is something to do, the wait returns
-- at once with 'WakeHandler'.
waitFor :: Timer -> MVar Wake -> Transport -> Bool -> Bool -> Word64 -> Bool -> IO Bool -> IO Wake
waitFor timer wake transport readable writable deadline forShutdown pending = do
  _ <- tryTakeMVar wake
  gen <- armRead timer deadline
  stopping <- isShuttingDown timer
  ready <- pending
  if forShutdown && stopping then disarm timer >> pure WakeShutdown
  else if ready then disarm timer >> pure WakeHandler
  else do
    cancelR <- if readable then transportOnReadable transport (void (tryPutMVar wake WakeReadable)) else pure (pure ())
    cancelW <- if writable then transportOnWritable transport (void (tryPutMVar wake WakeWritable)) else pure (pure ())
    let cancel = cancelR >> cancelW >> disarm timer
        next = takeMVar wake >>= \case
          WakeTimer g | g /= gen -> next
          w -> pure w
    w <- next `onException` cancel
    cancel
    pure w
