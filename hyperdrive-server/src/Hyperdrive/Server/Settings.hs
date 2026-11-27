{-# LANGUAGE OverloadedStrings #-}
-- | What the server's drivers share: the application's type, the
-- settings, and the errors they report.
module Hyperdrive.Server.Settings
  ( Application
  , Context(..)
  , Settings(..)
  , Timeouts(..)
  , defaultSettings
  , defaultTimeouts
  , ConnectionClosed(..)
  , ServerError(..)
  , withDate
  , hDate
  ) where

import Control.Exception
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Hyperdrive.HTTP1.Connection (Config, Misuse, defaultConfig)
import qualified Hyperdrive.HTTP2.Connection as H2
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message
import Network.Socket (HostName, PortNumber, SockAddr)

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
    -- ^ connections served at once; at the limit, accepting waits. 10,000 by
    -- default (docs/http2-core-design.md, C10: what bounds the per-connection
    -- budgets and slots together); 'Nothing': no limit
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
  , settingsHTTP2            :: !(Maybe H2.Config)
    -- ^ the HTTP/2 core's configuration, for clients that open with
    -- HTTP/2's preface (h2c with prior knowledge, D2); 'Nothing': HTTP/1.1
    -- only. On by default (docs/http2-driver-design.md, V5).
  , settingsHandlerGrace     :: !Int
    -- ^ microseconds an HTTP/2 connection that has ended waits for its
    -- handlers to return before killing them (C2, V4); 5 s
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
  , settingsMaxConnections = Just 10000
  , settingsPinConnections = False
  , settingsTimerResolution = 100000
  , settingsHTTP2 = Just H2.defaultConfig
  , settingsHandlerGrace = 5000000
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
  | HTTP2DriverBug !H2.Misuse             -- ^ the same, for the HTTP/2 core
  | InvalidRequestFromCore !RequestError  -- ^ the core handed over a head 'mkRequest' rejects
  | InvalidResponse !ResponseError        -- ^ the response is not allowed for the request
  | InvalidTrailers !TrailerError         -- ^ a stream returned trailers it may not send
  | StreamLengthMismatch !Word64 !Word64  -- ^ a stream wrote other than the length it declared
  | FileRangeOutside                      -- ^ a file range beyond the end of the file
  | FileShrank                            -- ^ a file ended before its announced length
  deriving (Show)

instance Exception ServerError

withDate :: FieldValue -> Fields -> Fields
withDate date fs
  | null (lookupAll hDate fs) = appendField hDate date fs
  | otherwise = fs

hDate :: FieldName
hDate = fromJust (mkFieldName "date")

