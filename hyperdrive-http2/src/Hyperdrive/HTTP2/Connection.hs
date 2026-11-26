{-# LANGUAGE OverloadedStrings #-}
-- | The HTTP/2 connection core (docs/http2-core-design.md, "The
-- connection core", C1-C13): a pure 'step' that the driver runs on the
-- connection's owner thread (D4).
--
-- This first part has the connection without streams: the prefaces,
-- SETTINGS, PING, frames of unknown type, connection errors, the
-- connection's timers and D9's nuisance budget. Frames that need
-- streams are not handled yet: they end the connection with
-- INTERNAL_ERROR, so that nothing half-handled passes silently.
module Hyperdrive.HTTP2.Connection
  ( -- * Configuration
    Config(..)
  , defaultConfig
  , Time
    -- * The connection
  , Conn
  , newConn
  , step
  , nextDeadline
  , peerSettings
  , PeerSettings(..)
    -- * Inputs and outputs
  , Input(..)
  , Output(..)
  , AbortReason(..)
  , Misuse(..)
  , connectionPreface
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (nub)
import Data.Word (Word32, Word64)
import Hyperdrive.HTTP2.Frame
import Hyperdrive.Semantics.Field (Fields)

-- | Milliseconds, from the driver's coarse clock (C1).
type Time = Word64

-- | The connection's settings and limits (D6, D9, C5-C10), each with
-- the default the design gives it.
data Config = Config
  { configMaxConcurrentStreams :: !Word32  -- ^ D9: 100
  , configHeaderTableSize      :: !Word32  -- ^ D9: 4,096 (the protocol's default)
  , configMaxFrameSize         :: !Int     -- ^ D9: 16,384 (the protocol's default)
  , configMaxHeaderListSize    :: !Word32  -- ^ D9: 32 KiB
  , configStreamWindow         :: !Word32  -- ^ D6: 1 MiB
  , configConnectionWindow     :: !Word32  -- ^ D6: 1 MiB
  , configHeadTimeout          :: !Time    -- ^ C8: the preface and first SETTINGS, 30 s
  , configSettingsTimeout      :: !Time    -- ^ C8: our SETTINGS unacknowledged, 10 s
  , configBudgetBurst          :: !Word64  -- ^ C10: 1,000
  , configBudgetRate           :: !Word64  -- ^ C10: 100 a second
  , configMaxSettings          :: !Int     -- ^ C10: settings in one SETTINGS frame, 100
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { configMaxConcurrentStreams = 100
  , configHeaderTableSize = 4096
  , configMaxFrameSize = defaultMaxFrameSize
  , configMaxHeaderListSize = 32768
  , configStreamWindow = 1048576
  , configConnectionWindow = 1048576
  , configHeadTimeout = 30000
  , configSettingsTimeout = 10000
  , configBudgetBurst = 1000
  , configBudgetRate = 100
  , configMaxSettings = 100
  }

-- | What the client's SETTINGS have told us so far (Section 6.5.2's
-- initial values until then).
data PeerSettings = PeerSettings
  { peerHeaderTableSize      :: !Word32
  , peerMaxConcurrentStreams :: !(Maybe Word32)
  , peerInitialWindowSize    :: !Word32
  , peerMaxFrameSize         :: !Word32
  , peerMaxHeaderListSize    :: !(Maybe Word32)
  } deriving (Eq, Show)

initialPeerSettings :: PeerSettings
initialPeerSettings = PeerSettings 4096 Nothing 65535 16384 Nothing

------------------------------------------------------------------------------
-- Inputs and outputs (C2)
------------------------------------------------------------------------------

data Input
  = Received !ByteString            -- ^ octets from the transport (the preface first)
  | Sent !Int                       -- ^ the transport has written this many more octets (C5)
  | PeerClosed
  | WriteBody !StreamId !ByteString -- ^ response content (C5)
  | EndBody !StreamId !Fields       -- ^ the end of the content, with trailers
  | Abort !StreamId                 -- ^ the handler failed after responding
  | WantContent !StreamId           -- ^ the handler read with nothing queued (C9)
  | Consumed !StreamId !Int         -- ^ the handler read this much request content (D6)
  | HandlerDone !StreamId           -- ^ the handler returned (C6)
  | TimedOut                        -- ^ 'nextDeadline' has passed
  | Shutdown                        -- ^ graceful shutdown (D10)
  deriving (Show)

-- | Why a stream ended early (C2).
data AbortReason
  = ResetByClient !ErrorCode
  | StreamTimedOut
  | ContentNotAsDeclared            -- ^ the handler's reads throw IncompleteBody
  | ConnectionEnded
  deriving (Eq, Show)

data Output
  = RequestAborted !StreamId !AbortReason
  | Writable !StreamId
  | Send ![ByteString]
  | Close                           -- ^ after a GOAWAY, staged as HTTP/1.1's
  | ProtocolMisuse !Misuse
  deriving (Show)

-- | An input that makes no sense in the connection's state: the
-- driver's bug (as HTTP/1.1's Q4).
data Misuse
  = SentMoreThanEmitted             -- ^ 'Sent' for octets never emitted
  | TimedOutWithoutDeadline         -- ^ 'TimedOut' before 'nextDeadline'
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- State
------------------------------------------------------------------------------

data Phase
  = AwaitingPreface                 -- ^ the client's 24 octets
  | AwaitingSettings                -- ^ the client's first frame, which must be SETTINGS
  | Running
  deriving (Eq, Show)

data Conn = Conn
  { cConfig      :: !Config
  , cPhase       :: !Phase
  , cBuffer      :: !ByteString     -- ^ received and not yet used
  , cPeer        :: !PeerSettings
  , cHeadDeadline :: !(Maybe Time)  -- ^ the preface and first SETTINGS (C8)
  , cSettingsSent :: ![Time]        -- ^ the deadlines of our SETTINGS not yet acknowledged,
                                    -- oldest first; they start when the client's preface and
                                    -- SETTINGS have arrived (C8), the head timeout until then
  , cBudget      :: !Word64         -- ^ nuisance budget left, in thousandths (C10)
  , cBudgetAt    :: !Time           -- ^ when it was last refilled
  , cUnsent      :: !Int            -- ^ octets emitted and not yet reported 'Sent' (C5)
  , cLastStream  :: !Word32         -- ^ the highest client stream opened (C4)
  , cClosed      :: !Bool
  }

peerSettings :: Conn -> PeerSettings
peerSettings = cPeer

-- = https://www.rfc-editor.org/rfc/rfc9113#section-3.4
-- # The client connection preface starts with a sequence of 24 octets,
-- # which in hex notation is:
-- # 0x505249202a20485454502f322e300d0a0d0a534d0d0a0d0a
-- # That is, the connection preface starts with the string "PRI *
-- # HTTP/2.0\r\n\r\nSM\r\n\r\n".
connectionPreface :: ByteString
connectionPreface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

-- = https://www.rfc-editor.org/rfc/rfc9113#section-3.4
-- # The server connection preface consists of a potentially empty
-- # SETTINGS frame (Section 6.5) that MUST be the first frame the server
-- # sends in the HTTP/2 connection.

-- | A new connection at the given time, and the server's preface: our
-- SETTINGS (D9) and the WINDOW_UPDATE that raises the connection window
-- from 65,535 to 'configConnectionWindow' (C11).
newConn :: Config -> Time -> (Conn, [Output])
newConn config now =
  let settings = renderSettings (ourSettings config)
      raise = fromIntegral (configConnectionWindow config) - 65535 :: Int
      update = [renderWindowUpdate (StreamId 0) (fromIntegral raise) | raise > 0]
      out = settings : update
      c = Conn
        { cConfig = config
        , cPhase = AwaitingPreface
        , cBuffer = B.empty
        , cPeer = initialPeerSettings
        , cHeadDeadline = Just (now + configHeadTimeout config)
        , cSettingsSent = [now]
        , cBudget = configBudgetBurst config * 1000
        , cBudgetAt = now
        , cUnsent = sum (map B.length out)
        , cLastStream = 0
        , cClosed = False
        }
  in (c, [Send out])

-- | Our SETTINGS: what differs from the protocol's initial values, and
-- MAX_CONCURRENT_STREAMS and MAX_HEADER_LIST_SIZE, which have none.
-- ENABLE_PUSH is not sent (D7).
ourSettings :: Config -> [Setting]
ourSettings config =
  [ MaxConcurrentStreams (configMaxConcurrentStreams config) ]
  ++ [ HeaderTableSize (configHeaderTableSize config) | configHeaderTableSize config /= 4096 ]
  ++ [ InitialWindowSize (configStreamWindow config) | configStreamWindow config /= 65535 ]
  ++ [ MaxFrameSize (fromIntegral (configMaxFrameSize config)) | configMaxFrameSize config /= defaultMaxFrameSize ]
  ++ [ MaxHeaderListSize (configMaxHeaderListSize config) ]

-- | The earliest time at which something expires (C1).
nextDeadline :: Conn -> Maybe Time
nextDeadline c
  | cClosed c = Nothing
  | otherwise = case maybe [] pure (cHeadDeadline c)
                       ++ (if cPhase c == Running then take 1 (cSettingsSent c) else []) of
      [] -> Nothing
      ds -> Just (minimum ds)

------------------------------------------------------------------------------
-- Stepping
------------------------------------------------------------------------------

step :: Time -> Input -> Conn -> (Conn, [Output])
step _ _ c | cClosed c = result c []
step now input c0 = let c = refill now c0 in case input of
  Received bs -> advance now c { cBuffer = if B.null (cBuffer c) then bs else cBuffer c <> bs } []
  Sent n
    | n > cUnsent c -> misuse SentMoreThanEmitted c
    | otherwise -> result c { cUnsent = cUnsent c - n } []
  PeerClosed -> close c []
  TimedOut -> timedOut now c
  Shutdown -> goAway c NoError
  -- C2: inputs for streams the core does not serve are ignored (the
  -- handler races the owner). This part serves none.
  WriteBody _ _ -> result c []
  EndBody _ _ -> result c []
  Abort _ -> result c []
  WantContent _ -> result c []
  Consumed _ _ -> result c []
  HandlerDone _ -> result c []

-- the budget refilled for the time since it last was (C10)
refill :: Time -> Conn -> Conn
refill now c
  | now <= cBudgetAt c = c
  | otherwise =
      let cfg = cConfig c
          gained = (now - cBudgetAt c) * configBudgetRate cfg
      in c { cBudget = min (configBudgetBurst cfg * 1000) (cBudget c + gained), cBudgetAt = now }

timedOut :: Time -> Conn -> (Conn, [Output])
timedOut now c = case nextDeadline c of
  Just d | d <= now ->
    if maybe False (<= now) (cHeadDeadline c)
      -- the client never sent its preface and SETTINGS: no GOAWAY,
      -- it may not be speaking HTTP/2 at all
      then close c []
      -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.3
      -- # If the sender of a SETTINGS frame does not receive an acknowledgment
      -- # within a reasonable amount of time, it MAY issue a connection error
      -- # (Section 5.4.1) of type SETTINGS_TIMEOUT.
      else goAway c SettingsTimeout
  _ -> misuse TimedOutWithoutDeadline c

-- Use as much of the buffer as makes whole frames.
advance :: Time -> Conn -> [Output] -> (Conn, [Output])
advance now c outs
  | cClosed c = result c outs
  | otherwise = case cPhase c of
      AwaitingPreface
        | B.length (cBuffer c) < B.length connectionPreface ->
            if cBuffer c `B.isPrefixOf` connectionPreface then result c outs else badPreface
        -- = https://www.rfc-editor.org/rfc/rfc9113#section-3.4
        -- # Clients and servers MUST treat an invalid connection preface as a
        -- # connection error (Section 5.4.1) of type PROTOCOL_ERROR.
        | not (connectionPreface `B.isPrefixOf` cBuffer c) -> badPreface
        | otherwise -> advance now c { cPhase = AwaitingSettings
                                     , cBuffer = B.drop (B.length connectionPreface) (cBuffer c) } outs
      _ -> case parseFrame (configMaxFrameSize (cConfig c)) (cBuffer c) of
        Incomplete _ -> result c outs
        Refused (FrameError _ code _) -> prefix outs (goAway c code)
        Parsed f n ->
          let c' = c { cBuffer = B.drop n (cBuffer c) }
          in case cPhase c' of
               -- = https://www.rfc-editor.org/rfc/rfc9113#section-3.4
               -- # This sequence MUST be followed by a
               -- # SETTINGS frame (Section 6.5), which MAY be empty.
               AwaitingSettings -> case f of
                 SettingsFrame _ _ ->
                   let started = map (const (now + configSettingsTimeout (cConfig c'))) (cSettingsSent c')
                   in frame now c' { cPhase = Running, cHeadDeadline = Nothing, cSettingsSent = started } f outs
                 _ -> prefix outs (goAway c' ProtocolError)
               _ -> frame now c' f outs
  where
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-3.4
    -- # A GOAWAY frame (Section 6.8) MAY be omitted in this case, since an
    -- # invalid preface indicates that the peer is not using HTTP/2.
    badPreface = prefix outs (close c [])

-- One frame, then the rest of the buffer.
frame :: Time -> Conn -> Frame -> [Output] -> (Conn, [Output])
frame now c f outs = case f of
  SettingsFrame count ss
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-10.5
    -- # The SETTINGS frame might also be abused to cause a peer to
    -- #    expend additional processing time.  This might be done by
    -- #    pointlessly changing settings, sending multiple undefined
    -- #    settings, or changing the same setting multiple times in the
    -- #    same frame.
    --
    -- Refused outright, as Go and nghttp2 do (C10).
    | count > configMaxSettings (cConfig c) || hasRepeats ss -> prefix outs (goAway c ProtocolError)
    -- one, and one for each setting after the first six (C10)
    | otherwise -> charge (1 + fromIntegral (max 0 (count - 6))) c $ \c1 ->
        -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.3
        -- # Once all values have been processed, the
        -- # recipient MUST immediately emit a SETTINGS frame with the ACK flag
        -- # set.
        emit [renderSettingsAck] c1 { cPeer = foldl apply (cPeer c1) ss } >>> next
  SettingsAckFrame -> case cSettingsSent c of
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.3
    -- # Upon receiving a SETTINGS frame with the ACK flag set, the
    -- # sender of the altered settings can rely on the values from the oldest
    -- # unacknowledged SETTINGS frame having been applied.
    (_ : rest) -> next (c { cSettingsSent = rest }, [])
    -- an acknowledgement of nothing we sent: as Go and nghttp2 (C10)
    [] -> prefix outs (goAway c ProtocolError)
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.7
  -- # Receivers of a PING frame that does not include an ACK flag MUST send
  -- # a PING frame with the ACK flag set in response, with an identical
  -- # frame payload.
  --
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.7
  -- # An endpoint MUST NOT respond to PING frames containing
  -- #    this flag.
  PingFrame False d -> charge 1 c $ \c1 -> emit [renderPing True d] c1 >>> next
  PingFrame True _ -> next (c, [])
  UnknownFrame _ _ _ -> charge 1 c $ \c1 -> next (c1, [])
  PriorityFrame _ -> charge 1 c $ \c1 -> next (c1, [])
  _ -> prefix outs (goAway c InternalError)
  where
    next (c', outs') = advance now c' (outs ++ outs')
    hasRepeats ss = length (nub (map settingId ss)) /= length ss

(>>>) :: a -> (a -> b) -> b
x >>> g = g x
infixl 1 >>>

-- | Spend nuisance budget, or end the connection when it is gone
-- (C10).
charge :: Word64 -> Conn -> (Conn -> (Conn, [Output])) -> (Conn, [Output])
charge n c k
  | cBudget c < n * 1000 = goAway c EnhanceYourCalm
  | otherwise = k c { cBudget = cBudget c - n * 1000 }

settingId :: Setting -> Int
settingId s = case s of
  HeaderTableSize _ -> 1
  EnablePush _ -> 2
  MaxConcurrentStreams _ -> 3
  InitialWindowSize _ -> 4
  MaxFrameSize _ -> 5
  MaxHeaderListSize _ -> 6

apply :: PeerSettings -> Setting -> PeerSettings
apply p s = case s of
  HeaderTableSize v -> p { peerHeaderTableSize = v }
  -- D7: SETTINGS_ENABLE_PUSH is checked by the parser and otherwise
  -- ignored: the server never pushes
  EnablePush _ -> p
  MaxConcurrentStreams v -> p { peerMaxConcurrentStreams = Just v }
  InitialWindowSize v -> p { peerInitialWindowSize = v }
  MaxFrameSize v -> p { peerMaxFrameSize = v }
  MaxHeaderListSize v -> p { peerMaxHeaderListSize = Just v }

-- | Send frames, counting them as emitted (C5).
emit :: [ByteString] -> Conn -> (Conn, [Output])
emit frames c = result c { cUnsent = cUnsent c + sum (map B.length frames) } [Send frames]

-- = https://www.rfc-editor.org/rfc/rfc9113#section-5.4.1
-- # An endpoint that encounters a connection error SHOULD first send a
-- # GOAWAY frame (Section 6.8) with the stream identifier of the last
-- # stream that it successfully received from its peer.

-- | GOAWAY with the code, then close (D10).
goAway :: Conn -> ErrorCode -> (Conn, [Output])
goAway c code =
  let (c', outs) = emit [renderGoAway (StreamId (cLastStream c)) code B.empty] c
  in close c' outs

misuse :: Misuse -> Conn -> (Conn, [Output])
misuse m c = close c [ProtocolMisuse m]

close :: Conn -> [Output] -> (Conn, [Output])
close c outs = result c { cClosed = True } (outs ++ [Close])

prefix :: [Output] -> (Conn, [Output]) -> (Conn, [Output])
prefix outs (c, outs') = result c (outs ++ outs')

-- | The new state, evaluated, and the outputs (as HTTP/1.1's core: a
-- lazily built state is a thunk the driver runs at once).
result :: Conn -> [Output] -> (Conn, [Output])
result !c outs = (c, outs)
{-# INLINE result #-}
