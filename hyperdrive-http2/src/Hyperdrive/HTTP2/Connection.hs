{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | The HTTP/2 connection core (docs/http2-core-design.md, "The
-- connection core", C1-C13): a pure 'step' that the driver runs on the
-- connection's owner thread (D4).
--
-- Built in parts. Part 1: the prefaces, SETTINGS, PING, frames of
-- unknown type, connection errors, the connection's timers and D9's
-- nuisance budget. Part 2: streams and their field blocks, to
-- 'NewRequest' (C4, C6, C7, D10, D11), client resets, trailers, and the
-- responses the core writes itself. Part 3: request content and its
-- flow-control credit (C3, C9). Part 4: responses, the client's
-- windows and the scheduler (C5, C12, D8). Part 5: timers per stream and
-- the idle connection's (C8), graceful shutdown and the client's GOAWAY
-- (D10, C11), and every handler told when the connection ends (C2).
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
  , RequestHead(..)
  , ResponseHead(..)
  , Misuse(..)
  , connectionPreface
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import Data.Sequence (Seq, ViewL(..), viewl, (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word (Word32, Word64)
import Hyperdrive.HPACK.Decode
import Hyperdrive.HPACK.Encode (Encoder, encodeBlock, newEncoder, setPeerTableSize)
import Hyperdrive.HTTP2.Frame
import Hyperdrive.Semantics.ControlData hiding (Refused)
import qualified Hyperdrive.Semantics.ControlData as CD
import Hyperdrive.Semantics.Field (Fields, emptyFields, fieldNameBytes, fieldValueBytes, fieldsToList)
import Hyperdrive.Semantics.Method (pattern HEAD)
import Hyperdrive.Semantics.Status (Status, mkStatus, statusCode, pattern ExpectationFailed, pattern InternalServerError)
import Hyperdrive.Semantics.Target (TargetContext(..))
import Hyperdrive.Semantics.URI (pattern HTTP)

-- | Milliseconds, from the driver's coarse clock (C1).
type Time = Word64

-- | The connection's settings and limits (D6, D9, C5-C10), each with
-- the default the design gives it.
data Config = Config
  { configMaxConcurrentStreams :: !Word32  -- ^ D9: 100
  , configHeaderTableSize      :: !Word32  -- ^ D9: 4,096 (the protocol's default)
  , configMaxFrameSize         :: !Int     -- ^ D9: 16,384 (the protocol's default)
  , configMaxHeaderListSize    :: !Word32  -- ^ D9: 32 KiB
  , configStreamWindow         :: !Word32  -- ^ D6: 1 MiB; at least 65,535 is used
  , configConnectionWindow     :: !Word32  -- ^ D6: 1 MiB; at least 65,535 is used
  , configHeadTimeout          :: !Time    -- ^ C8: the preface and first SETTINGS, 30 s
  , configSettingsTimeout      :: !Time    -- ^ C8: our SETTINGS unacknowledged, 10 s
  , configBudgetBurst          :: !Word64  -- ^ C10: 1,000
  , configBudgetRate           :: !Word64  -- ^ C10: 100 a second
  , configMaxSettings          :: !Int     -- ^ C10: settings in one SETTINGS frame, 100
  , configMaxUnsent            :: !Int     -- ^ C5: octets emitted and not yet written, 1 MiB
  , configMaxHeldStreams       :: !Int     -- ^ C6: reset streams whose handlers still run, 100
  , configMaxFields            :: !Int     -- ^ D9: fields in a request, 100
  , configBlockTimeout         :: !Time    -- ^ C8: a field block left unfinished, 30 s
  , configStreamBuffer         :: !Int     -- ^ C5: response octets buffered per stream, 16 KiB
  , configConnectionBuffer     :: !Int     -- ^ C5: response octets buffered per connection, 256 KiB
  , configPacing               :: !Int     -- ^ C5: DATA emitted and not yet written, 64 KiB
  , configIdleTimeout          :: !Time    -- ^ C8: a connection with no streams, 60 s
  , configPingTimeout          :: !Time    -- ^ C8: the shutdown PING unanswered, 1 s
  , configStreamIdleTimeout    :: !Time    -- ^ C8: a stream the client holds up with no progress, 5 min
  , configMinRate              :: !Word64  -- ^ C8: octets a second such a stream must move, 240
  , configMinRateGrace         :: !Time    -- ^ C8: the minimum rate's grace, 5 s
  , configMaxControlFrames     :: !Int     -- ^ C5: control frames emitted and not yet written, 10,000
  , configMaxRequests          :: !Int     -- ^ D9: requests before a GOAWAY, to rotate connections; 0, off
  , configResetsKept           :: !Int     -- ^ C4: streams we reset that are remembered, 1,000
  , configTarget              :: !TargetContext
    -- ^ the connection's scheme and default authority, and whether its
    -- peer is a trusted gateway (D11)
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
  , configMaxUnsent = 1048576
  , configMaxHeldStreams = 100
  , configMaxFields = 100
  , configBlockTimeout = 30000
  , configStreamBuffer = 16384
  , configConnectionBuffer = 262144
  , configPacing = 65536
  , configIdleTimeout = 60000
  , configPingTimeout = 1000
  , configStreamIdleTimeout = 300000
  , configMinRate = 240
  , configMinRateGrace = 5000
  , configMaxControlFrames = 10000
  , configMaxRequests = 0
  , configResetsKept = 1000
  , configTarget = TargetContext HTTP Nothing False
  }

-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.2
-- # The connection flow-control window can only be changed using
-- # WINDOW_UPDATE frames.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.3
-- # However, the receiver
-- # MUST be prepared to receive data that exceeds this window size, since
-- # the sender might send data that exceeds the lower limit prior to
-- # processing the SETTINGS frame.
--
-- So neither window is ever below the protocol's initial 65,535: the
-- connection's cannot be lowered, and a client may use a stream's
-- before it has our SETTINGS.

-- | The receive windows used: the configured ones, at least 65,535.
streamWindow, connectionWindow :: Config -> Int
streamWindow = max 65535 . fromIntegral . configStreamWindow
connectionWindow = max 65535 . fromIntegral . configConnectionWindow

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
  | Respond !StreamId !ResponseHead -- ^ the handler's response head (C12)
  | WriteBody !StreamId !ByteString -- ^ response content (C5)
  | EndBody !StreamId !Fields       -- ^ the end of the content, with trailers
  | Abort !StreamId                 -- ^ the handler failed after responding
  | WantContent !StreamId           -- ^ the handler read with nothing queued (C9)
  | Consumed !StreamId !Int         -- ^ the handler read this much request content (D6)
  | HandlerDone !StreamId           -- ^ the handler returned (C6)
  | TimedOut                        -- ^ 'nextDeadline' has passed
  | Shutdown                        -- ^ graceful shutdown (D10); given again, the end of
                                    --   its grace period: what is left is reset and closed
  deriving (Show)

-- | Why a stream ended early (C2).
data AbortReason
  = ResetByClient !ErrorCode
  | ResetByServer !ErrorCode        -- ^ a stream error: the request was malformed, say
  | StreamTimedOut
  | ContentNotAsDeclared            -- ^ the handler's reads throw IncompleteBody
  | ConnectionEnded
  deriving (Eq, Show)

-- | A handler's response head (C12), as HTTP/1.1's: the status, the
-- header fields (the driver has added @date@), whether there is
-- content, and its length if known.
data ResponseHead = ResponseHead
  { respStatus     :: !Status
  , respFields     :: !Fields
  , respHasContent :: !Bool
  , respLength     :: !(Maybe Word64)
  } deriving (Show)

-- | A request for a handler: D11's mapping of its field list, and
-- whether content follows.
data RequestHead = RequestHead
  { requestControl    :: !RequestControl
  , requestHasContent :: !Bool
  } deriving (Show)

data Output
  = NewRequest !StreamId !RequestHead
  | RequestData !StreamId !ByteString -- ^ request content (C3)
  | RequestEnd !StreamId !Fields    -- ^ the end of the request's content, with its trailers
  | RequestAborted !StreamId !AbortReason
  | Writable !StreamId               -- ^ the answer to a 'WriteBody': write again (C5);
                                    --   a stream aborted instead gets 'RequestAborted'
  | Send ![ByteString]
  | Close                           -- ^ after a GOAWAY, staged as HTTP/1.1's
  | ProtocolMisuse !Misuse
  deriving (Show)

-- | An input that makes no sense in the connection's state: the
-- driver's bug (as HTTP/1.1's Q4).
data Misuse
  = SentMoreThanEmitted             -- ^ 'Sent' for octets never emitted
  | ConsumedMoreThanQueued          -- ^ 'Consumed' for content never passed on
  | RespondTwice
  | BodyBeforeRespond               -- ^ 'WriteBody' or 'EndBody' before 'Respond'
  | WroteBeforeWritable             -- ^ 'WriteBody' before the last one's 'Writable' (C5)
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

-- | A stream's state (Section 5.1), for a server that never pushes. A
-- stream that is closed is in the map only while its handler still
-- runs after a reset (C6).
data StreamState = Open | HalfClosedRemote | ClosedHeld
  deriving (Eq, Show)

data StreamEntry = StreamEntry
  { sState         :: !StreamState
  , sHandler       :: !Bool          -- ^ its handler runs
  , sExpect100     :: !Bool          -- ^ a 100 (Continue) is owed (C9)
  , sContentLength :: !(Maybe Word64)
  , sReceived      :: !Word64        -- ^ content received
  , sRecvWindow    :: !Int           -- ^ what the client may still send (C3)
  , sUnreturned    :: !Int           -- ^ credit consumed and not yet given back
  , sQueued        :: !Int           -- ^ content passed on and not yet consumed
  , sIsHead        :: !Bool          -- ^ the request's method is HEAD (C12)
  , sResponse      :: !Response
  , sSendWindow    :: !Int           -- ^ what we may still send on the stream (C5)
  , sOut           :: !(Seq ByteString) -- ^ response content not yet sent
  , sOutLength     :: !Int
  , sWaiting       :: !Bool          -- ^ a write waits for its 'Writable' (C5)
  , sWanting       :: !Bool          -- ^ the handler waits for content (C8)
  , sBlocked       :: !Bool          -- ^ the client holds the stream up: its clock runs (C8)
  , sSince         :: !Time          -- ^ when the clock was last run up to
  , sAllowance     :: !Word64        -- ^ the minimum rate's allowance, in thousandths of an octet
  , sIdleLeft      :: !Time          -- ^ what is left of the idle timer
  , sTimer         :: !(Maybe Time)  -- ^ the stream's entry in 'cTimers'
  }

-- | Where a stream's response is (C12).
data Response
  = Awaiting                         -- ^ no response head yet
  -- | the length declared, the content the handler has written, and
  -- the end (with trailers) once the handler has said so
  | Sending !(Maybe Word64) !Word64 !(Maybe Fields)

-- | A field block whose HEADERS has come and whose CONTINUATIONs have
-- not all (C7).
data Block = Block
  { bStream    :: !Word32
  , bEndStream :: !Bool
  , bFragments :: ![ByteString]      -- ^ newest first, joined once at the end
  , bLength    :: !Int
  , bDeadline  :: !Time
  }

data Conn = Conn
  { cConfig      :: !Config
  , cPhase       :: !Phase
  , cBuffer      :: !ByteString     -- ^ received and not yet used, joined
  , cMore        :: ![ByteString]   -- ^ received after it, not yet joined, newest first
  , cMoreLength  :: !Int
  , cPeer        :: !PeerSettings
  , cHeadDeadline :: !(Maybe Time)  -- ^ the preface and first SETTINGS (C8)
  , cSettingsSent :: ![Time]        -- ^ the deadlines of our SETTINGS not yet acknowledged,
                                    -- oldest first; they start when the client's preface and
                                    -- SETTINGS have arrived (C8), the head timeout until then
  , cBudget      :: !Word64         -- ^ nuisance budget left, in thousandths (C10)
  , cBudgetAt    :: !Time           -- ^ when it was last refilled
  , cUnsent      :: !Int            -- ^ octets emitted and not yet reported 'Sent' (C5)
  , cEmitted     :: !Int            -- ^ octets emitted in all
  , cControlEnds :: !(Seq Int)      -- ^ where each control frame not yet written ends, in
                                    --   'cEmitted' octets, oldest first (C5)
  , cRequests    :: !Int            -- ^ requests passed on (D9)
  , cResetByUs   :: !IntSet.IntSet  -- ^ streams we reset, the latest 'configResetsKept' (C4)
  , cResetCount  :: !Int            -- ^ how many that is
  , cLastStream  :: !Word32         -- ^ the highest client stream opened (C4)
  , cStreams     :: !(M.Map Word32 StreamEntry) -- ^ the streams still served (C4)
  , cBlock       :: !(Maybe Block)  -- ^ a field block being received (C7)
  , cDecoder     :: !Decoder
  , cRecvWindow  :: !Int            -- ^ what the client may still send on the connection (C3)
  , cUnreturned  :: !Int            -- ^ connection credit consumed and not yet given back
  , cSource      :: !Int            -- ^ the size of the buffer frames are now read from (C3)
  , cSendWindow  :: !Int            -- ^ what we may still send on the connection (C5)
  , cBuffered    :: !Int            -- ^ response content buffered, all streams (C5)
  , cOrder       :: !(Seq Word32)   -- ^ streams with something to send, in turn (D8)
  , cEncoder     :: !Encoder
  , cNow         :: !Time           -- ^ the latest time a step was given
  , cTimers      :: !(Set (Time, Word32))
    -- ^ the streams' deadlines (C8), one entry for each stream whose
    -- clock has run; an entry may be earlier than its stream's deadline
    -- (progress moves deadlines later), and is then renewed when it expires
  , cTouched     :: ![Word32]       -- ^ streams held up in this step with no deadline yet
  , cIdleDeadline :: !(Maybe Time)  -- ^ the connection has no streams (C8)
  , cEnding      :: !Ending
  , cPeerGoAway  :: !Bool           -- ^ the client sent GOAWAY (C11)
  , cAcceptLimit :: !Word32         -- ^ the last stream of our final GOAWAY, or 2^31-1 (D10)
  , cClosed      :: !Bool
  }

-- | Graceful shutdown (D10).
data Ending
  = Serving
  | Pinging !Time                    -- ^ GOAWAY with 2^31-1 and a PING sent; the PING's deadline
  | Draining                         -- ^ the final GOAWAY sent: the streams finish, then the close
  deriving (Eq, Show)

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
      raise = connectionWindow config - 65535
      update = [renderWindowUpdate (StreamId 0) (fromIntegral raise) | raise > 0]
      out = settings : update
      c = Conn
        { cConfig = config
        , cPhase = AwaitingPreface
        , cBuffer = B.empty
        , cMore = []
        , cMoreLength = 0
        , cPeer = initialPeerSettings
        , cHeadDeadline = Just (now + configHeadTimeout config)
        , cSettingsSent = [now]
        , cBudget = configBudgetBurst config * 1000
        , cBudgetAt = now
        , cUnsent = sum (map B.length out)
        , cEmitted = sum (map B.length out)
        , cControlEnds = Seq.empty
        , cRequests = 0
        , cResetByUs = IntSet.empty
        , cResetCount = 0
        , cLastStream = 0
        , cStreams = M.empty
        , cBlock = Nothing
        , cDecoder = newDecoder 4096 (fromIntegral (configHeaderTableSize config))
        , cRecvWindow = connectionWindow config
        , cUnreturned = 0
        , cSource = 0
        , cSendWindow = 65535
        , cBuffered = 0
        , cOrder = Seq.empty
        , cEncoder = newEncoder 4096
        , cNow = now
        , cTimers = S.empty
        , cTouched = []
        , cIdleDeadline = Nothing
        , cEnding = Serving
        , cPeerGoAway = False
        , cAcceptLimit = maxStreamId
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
  ++ [ InitialWindowSize (fromIntegral (streamWindow config)) | streamWindow config /= 65535 ]
  ++ [ MaxFrameSize (fromIntegral (configMaxFrameSize config)) | configMaxFrameSize config /= defaultMaxFrameSize ]
  ++ [ MaxHeaderListSize (configMaxHeaderListSize config) ]
  -- RFC 9218 Section 2.1: "Servers can send SETTINGS_NO_RFC7540_PRIORITIES
  -- with a value of 1 to indicate that they will ignore HTTP/2 priority
  -- signals sent by clients." (D8, D9)
  ++ [ NoRFC7540Priorities True ]

-- | The earliest time at which something expires (C1).
nextDeadline :: Conn -> Maybe Time
nextDeadline c
  | cClosed c = Nothing
  | otherwise = case maybe [] pure (cHeadDeadline c)
                       ++ (if cPhase c == Running then take 1 (cSettingsSent c) else [])
                       ++ maybe [] (pure . bDeadline) (cBlock c)
                       ++ maybe [] pure (cIdleDeadline c)
                       ++ [t | Pinging t <- [cEnding c]]
                       ++ [t | Just (t, _) <- [S.lookupMin (cTimers c)]] of
      [] -> Nothing
      ds -> Just (minimum ds)

------------------------------------------------------------------------------
-- Stepping
------------------------------------------------------------------------------

step :: Time -> Input -> Conn -> (Conn, [Output])
step _ _ c | cClosed c = result c []
step time input c0 = afterStep $ case input of
  Received bs -> advance now c { cMore = bs : cMore c, cMoreLength = cMoreLength c + B.length bs } []
  Sent n
    | n > cUnsent c -> misuse SentMoreThanEmitted c
    | otherwise -> pump (dropWritten c { cUnsent = cUnsent c - n })
  PeerClosed -> close c []
  TimedOut -> timedOut now c
  Shutdown -> shutdown now c
  -- C2: inputs for streams the core does not serve are ignored (the
  -- handler races the owner)
  Respond (StreamId sid) h -> respondHandler sid h c
  WriteBody (StreamId sid) d -> writeBody sid d c
  EndBody (StreamId sid) tr -> endBody sid tr c
  Abort (StreamId sid) -> abort sid c
  WantContent (StreamId sid) -> wantContent sid c
  Consumed (StreamId sid) n -> consumed sid n c
  HandlerDone (StreamId sid) -> handlerDone sid c
  where
    -- time never runs backwards for the core, whatever it is given
    now = max time (cNow c0)
    c = refill now c0 { cNow = now }

-- | The end of every step: deadlines for the streams held up (C8), a
-- connection that is ending closed once its streams are done (D10,
-- C11), and the idle timer run while there are none (C8).
afterStep :: (Conn, [Output]) -> (Conn, [Output])
afterStep (c0, outs)
  | cClosed c0 = (c0, outs)
  | (cEnding c == Draining || cPeerGoAway c) && isNothing (cBlock c) && all ((== ClosedHeld) . sState) (cStreams c) =
      -- streams reset whose handlers still run need nothing more from
      -- the client; they were told, and the driver ends them (C2)
      if cEnding c == Draining then close c outs
      else let (c', o) = emitUncapped [renderGoAway (StreamId (lastProcessed c)) NoError B.empty] c
           in close c' (outs ++ o)
  | otherwise = result (idleTimer c) outs
  where c = armTimers c0

-- C8: a connection with no streams is closed after the idle timeout
idleTimer :: Conn -> Conn
idleTimer c
  | M.null (cStreams c) && cPhase c == Running && configIdleTimeout (cConfig c) > 0 =
      if isJust (cIdleDeadline c) then c else c { cIdleDeadline = Just (cNow c + configIdleTimeout (cConfig c)) }
  | isJust (cIdleDeadline c) = c { cIdleDeadline = Nothing }
  | otherwise = c

-- the idle timer, if it runs, restarted (C8)
restartIdle :: Time -> Conn -> Conn
restartIdle now c = c { cIdleDeadline = (now + configIdleTimeout (cConfig c)) <$ cIdleDeadline c }

-- the largest stream identifier
maxStreamId :: Word32
maxStreamId = 2147483647

-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.8
-- # Endpoints MUST NOT increase the
-- # value they send in the last stream identifier, since the peers might
-- # already have retried unprocessed requests on another connection.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.7
-- # If frames that are on a stream
-- # are passed to the application layer for any stream, then
-- # REFUSED_STREAM MUST NOT be used for that stream, and a GOAWAY frame
-- # MUST include a stream identifier that is greater than or equal to the
-- # given stream identifier.

-- | The last stream identifier of a GOAWAY: the highest opened, and
-- never above an earlier GOAWAY's.
lastProcessed :: Conn -> Word32
lastProcessed c = min (cLastStream c) (cAcceptLimit c)

-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.8
-- # A server that is attempting to gracefully shut down a
-- # connection SHOULD send an initial GOAWAY frame with the last stream
-- # identifier set to 2^31-1 and a NO_ERROR code.  This signals to the
-- # client that a shutdown is imminent and that initiating further
-- # requests is prohibited.  After allowing time for any in-flight stream
-- # creation (at least one round-trip time), the server MAY send another
-- # GOAWAY frame with an updated last stream identifier.

-- | Graceful shutdown (D10): GOAWAY with 2^31-1 and a PING; the final
-- GOAWAY when the PING is answered or its timer expires; the close when
-- the streams are done ('afterStep'). Given again (the end of the grace
-- period), the streams left are reset with CANCEL and the connection
-- closed.
shutdown :: Time -> Conn -> (Conn, [Output])
shutdown now c = case (cPhase c, cEnding c) of
  -- the client has not started: it may not be speaking HTTP/2
  (AwaitingPreface, _) -> close c []
  (AwaitingSettings, _) -> goAway c NoError
  (_, Serving)
    | configPingTimeout (cConfig c) == 0 -> drain c
    | otherwise ->
        emitUncapped [renderGoAway (StreamId maxStreamId) NoError B.empty, renderPing False shutdownPing]
                     c { cEnding = Pinging (now + configPingTimeout (cConfig c)) }
  _ ->
    let resets = [renderRstStream (StreamId sid) Cancel | (sid, st) <- M.toList (cStreams c), sState st /= ClosedHeld]
        (c', outs) = emitUncapped (resets ++ [renderGoAway (StreamId (lastProcessed c)) NoError B.empty]) c
    in close c' outs

-- the opaque data of the shutdown PING
shutdownPing :: ByteString
shutdownPing = "shutdown"

-- | The final GOAWAY of a graceful shutdown: the last stream is the
-- highest the client has opened, or is opening (its HEADERS came before
-- the PING's answer); streams above it are refused from now on.
drain :: Conn -> (Conn, [Output])
drain c =
  let limit = maybe id (max . bStream) (cBlock c) (cLastStream c)
  in emitUncapped [renderGoAway (StreamId limit) NoError B.empty] c { cEnding = Draining, cAcceptLimit = limit }

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
    if expired (cHeadDeadline c)
      -- the client never sent its preface and SETTINGS: no GOAWAY,
      -- it may not be speaking HTTP/2 at all
      then close c []
      -- a field block left unfinished (C8): the whole connection waits
      -- on it, since nothing may come between its frames
      else if expired (bDeadline <$> cBlock c) then goAway c ProtocolError
      -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.3
      -- # If the sender of a SETTINGS frame does not receive an acknowledgment
      -- # within a reasonable amount of time, it MAY issue a connection error
      -- # (Section 5.4.1) of type SETTINGS_TIMEOUT.
      else if cPhase c == Running && expired (listToMaybe (cSettingsSent c)) then goAway c SettingsTimeout
      -- no streams for the idle timeout (C8)
      else if expired (cIdleDeadline c) then goAway c NoError
      -- the shutdown PING unanswered: on as if it had been (C8)
      else andThen (case cEnding c of
                      Pinging t | t <= now -> drain c
                      _ -> result c [])
                   (expireStreams now)
  _ -> misuse TimedOutWithoutDeadline c
  where expired = maybe False (<= now)

-- | The streams whose timers have expired are reset with CANCEL (C8);
-- an entry earlier than its stream's deadline is renewed.
expireStreams :: Time -> Conn -> (Conn, [Output])
expireStreams now c = case S.minView (cTimers c) of
  Just ((t, sid), rest) | t <= now ->
    let c1 = c { cTimers = rest }
    in case M.lookup sid (cStreams c1) of
         Just st | sBlocked st, Just d <- streamDeadline (cConfig c1) st ->
           if d <= now
             then andThen (streamTimedOut sid st c1) (expireStreams now)
             else expireStreams now c1 { cStreams = M.insert sid st { sTimer = Just d } (cStreams c1)
                                       , cTimers = S.insert (d, sid) rest }
         Just st -> expireStreams now c1 { cStreams = M.insert sid st { sTimer = Nothing } (cStreams c1) }
         Nothing -> expireStreams now c1
  _ -> result c []

-- | A stream's timer expired: RST_STREAM CANCEL, not charged (C10), and
-- the handler told.
streamTimedOut :: Word32 -> StreamEntry -> Conn -> (Conn, [Output])
streamTimedOut sid st c =
  andThen (ended sid st c) $ \c1 ->
    let (c2, outs) = resetStream sid Cancel c1
    in result c2 (RequestAborted (StreamId sid) StreamTimedOut : outs)

-- What has been received and not used.
available :: Conn -> Int
available c = B.length (cBuffer c) + cMoreLength c

-- The chunks received joined into one buffer. Done only once a whole
-- frame (or the preface) is there, so that each octet is copied at most
-- twice however the input is split: joining on every chunk would copy a
-- frame sent an octet at a time once for each octet.
joined :: Conn -> Conn
joined c
  | null (cMore c) = c
  | otherwise =
      -- a lone chunk is used as it is: concatenating it with an empty
      -- buffer would copy every chunk received
      let b = case filter (not . B.null) (cBuffer c : reverse (cMore c)) of
            [x] -> x
            xs -> B.concat xs
      in c { cBuffer = b, cMore = [], cMoreLength = 0, cSource = B.length b }

-- Use as much of the input as makes whole frames. The outputs are
-- collected newest first, a list for each frame, and joined once at the
-- end.
advance :: Time -> Conn -> [[Output]] -> (Conn, [Output])
advance now c acc
  | cClosed c = done c acc
  | otherwise = case cPhase c of
      AwaitingPreface ->
        let c1 = joined c
            buf = cBuffer c1
        in if B.length buf < B.length connectionPreface
             then if buf `B.isPrefixOf` connectionPreface then done c1 acc else badPreface c1
             -- = https://www.rfc-editor.org/rfc/rfc9113#section-3.4
             -- # Clients and servers MUST treat an invalid connection preface as a
             -- # connection error (Section 5.4.1) of type PROTOCOL_ERROR.
             else if not (connectionPreface `B.isPrefixOf` buf) then badPreface c1
             else advance now c1 { cPhase = AwaitingSettings
                                 , cBuffer = B.drop (B.length connectionPreface) buf } acc
      _ -> case parseFrame (configMaxFrameSize (cConfig c)) (cBuffer c) of
        Incomplete n
          | available c >= n && not (null (cMore c)) -> advance now (joined c) acc
          | otherwise -> done c acc
        Refused (FrameError _ code _) -> stop acc (goAway c code)
        Parsed f n ->
          let c' = c { cBuffer = B.drop n (cBuffer c) }
          in case cPhase c' of
               -- = https://www.rfc-editor.org/rfc/rfc9113#section-3.4
               -- # This sequence MUST be followed by a
               -- # SETTINGS frame (Section 6.5), which MAY be empty.
               AwaitingSettings -> case f of
                 SettingsFrame _ _ ->
                   let started = map (const (now + configSettingsTimeout (cConfig c'))) (cSettingsSent c')
                   in frame now c' { cPhase = Running, cHeadDeadline = Nothing, cSettingsSent = started } f acc
                 _ -> stop acc (goAway c' ProtocolError)
               _ -> frame now c' f acc
  where
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-3.4
    -- # A GOAWAY frame (Section 6.8) MAY be omitted in this case, since an
    -- # invalid preface indicates that the peer is not using HTTP/2.
    badPreface c1 = stop acc (close c1 [])

-- the state and the outputs collected so far
done :: Conn -> [[Output]] -> (Conn, [Output])
done c acc = result c (concat (reverse acc))

-- the collected outputs, then those of a last step
stop :: [[Output]] -> (Conn, [Output]) -> (Conn, [Output])
stop acc (c, outs) = done c (outs : acc)

-- One frame, then the rest of the input.
frame :: Time -> Conn -> Frame -> [[Output]] -> (Conn, [Output])
frame now c f acc = case (cBlock c, f) of
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.2
  -- # A HEADERS frame without the END_HEADERS flag set MUST be followed
  -- #    by a CONTINUATION frame for the same stream.  A receiver MUST
  -- #    treat the receipt of any other type of frame or a frame on a
  -- #    different stream as a connection error (Section 5.4.1) of type
  -- #    PROTOCOL_ERROR.
  (Just b, ContinuationFrame (StreamId sid) end frag)
    | sid == bStream b -> continuation now c b end frag acc
  (Just _, _) -> stop acc (goAway c ProtocolError)
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.10
  -- # A CONTINUATION frame MUST be preceded by a HEADERS, PUSH_PROMISE or
  -- # CONTINUATION frame without the END_HEADERS flag set.  A recipient
  -- # that observes violation of this rule MUST respond with a connection
  -- # error (Section 5.4.1) of type PROTOCOL_ERROR.
  (Nothing, ContinuationFrame _ _ _) -> stop acc (goAway c ProtocolError)
  (Nothing, _) -> frameOutsideBlock now c f acc

frameOutsideBlock :: Time -> Conn -> Frame -> [[Output]] -> (Conn, [Output])
frameOutsideBlock now c f acc = case f of
  HeadersFrame (StreamId sid) endStream endHeaders frag
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1.1
    -- # Streams initiated by a client MUST use odd-numbered stream
    -- # identifiers
    | even sid -> stop acc (goAway c ProtocolError)
    | otherwise ->
        let b = Block sid endStream [frag] (B.length frag) (now + configBlockTimeout (cConfig c))
        in if B.length frag > maxEncoded c then stop acc (goAway c EnhanceYourCalm)
           else if endHeaders then fieldBlock now c b acc
           else next (c { cBlock = Just b }, [])
  DataFrame (StreamId sid) end d plen
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.1
    -- # The sender MUST NOT send a flow-controlled frame with a length that
    -- # exceeds the space available in either of the flow-control windows
    -- # advertised by the receiver.
    | plen > cRecvWindow c -> stop acc (goAway c FlowControlError)
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
    -- # Receiving any frame other than HEADERS or PRIORITY on a stream in
    -- #    this state MUST be treated as a connection error (Section 5.4.1)
    -- #    of type PROTOCOL_ERROR.
    | idle sid c -> stop acc (goAway c ProtocolError) -- DATA
    | otherwise ->
        let c1 = c { cRecvWindow = cRecvWindow c - plen }
        in case M.lookup sid (cStreams c1) of
             Just st | sState st == Open -> next (requestData c1 sid st end d plen)
             -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
             -- # If an endpoint receives additional frames, other than
             -- #    WINDOW_UPDATE, PRIORITY, or RST_STREAM, for a stream that is in
             -- #    this state, it MUST respond with a stream error (Section 5.4.2) of
             -- #    type STREAM_CLOSED.
             Just st | sState st == HalfClosedRemote ->
               next (andThen (giveBack plen c1) (\c2 -> streamError c2 sid st StreamClosed))
             -- a closed stream: discarded, its credit given back at once
             -- (C3, C4; Section 6.9)
             _ -> next (giveBack plen c1)
  WindowUpdateFrame (StreamId 0) inc
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.1
    -- # A sender MUST NOT allow a flow-control window to exceed 2^31-1
    -- # octets.  If a sender receives a WINDOW_UPDATE that causes a flow-
    -- # control window to exceed this maximum, it MUST terminate either the
    -- # stream or the connection, as appropriate.
    | cSendWindow c + fromIntegral inc > maxWindow -> stop acc (goAway c FlowControlError)
    | otherwise -> next (pump c { cSendWindow = cSendWindow c + fromIntegral inc })
  WindowUpdateFrame (StreamId sid) inc
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
    -- # Receiving any frame other than HEADERS or PRIORITY on a stream in
    -- #    this state MUST be treated as a connection error (Section 5.4.1)
    -- #    of type PROTOCOL_ERROR.
    | not (idle sid c) -> case M.lookup sid (cStreams c) of
        Just st | sState st /= ClosedHeld ->
          let w = sSendWindow st + fromIntegral inc
          in if w > maxWindow then next (streamError c sid st FlowControlError)
             -- a window left under the DATA floor costs one (C5, C10)
             else (if w < dataFloor then charge 1 c else ($ c)) $ \c1 ->
                    next (pump c1 { cStreams = M.insert sid st { sSendWindow = w } (cStreams c1) })
        -- a closed stream: ignored (C4)
        _ -> next (c, [])
    | otherwise -> stop acc (goAway c ProtocolError)
  RstStreamFrame (StreamId sid) code
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
    -- # Receiving any frame other than HEADERS or PRIORITY on a stream in
    -- #    this state MUST be treated as a connection error (Section 5.4.1)
    -- #    of type PROTOCOL_ERROR.
    | idle sid c -> stop acc (goAway c ProtocolError) -- RST_STREAM
    | otherwise -> case M.lookup sid (cStreams c) of
        Just st | sState st /= ClosedHeld ->
          -- a client's reset before our response ended (rapid reset,
          -- C10); our responses come in part 4
          charge 1 c $ \c1 -> next (andThen (ended sid st c1) (\c2 -> (c2, [RequestAborted (StreamId sid) (ResetByClient code)])))
        -- a stream already closed: ignored (C4)
        _ -> next (c, [])
  SettingsFrame count ss
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-10.5
    -- # The SETTINGS frame might also be abused to cause a peer to
    -- #    expend additional processing time.  This might be done by
    -- #    pointlessly changing settings, sending multiple undefined
    -- #    settings, or changing the same setting multiple times in the
    -- #    same frame.
    --
    -- More than 100 settings are refused outright, as Go refuses them
    -- (C10); a setting named more than once is taken in order, the last
    -- value standing (changed 2026-11-27: the first version refused it,
    -- as Go does, against Section 6.5.3; the cap and the charge below
    -- already bound the work).
    | count > configMaxSettings (cConfig c) -> stop acc (goAway c ProtocolError)
    -- one, and one for each setting after the first six (C10)
    | otherwise -> charge (1 + fromIntegral (max 0 (count - 6))) c $ \c1 ->
        -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.3
        -- # The values in the SETTINGS frame MUST be processed in the order they
        -- # appear, with no other frame processing between values.
        --
        -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5
        -- # Settings are processed in the order in which they
        -- # appear, and a receiver of a SETTINGS frame does not need to maintain
        -- # any state other than the current value of each setting.  Therefore,
        -- # the value of a SETTINGS parameter is the last value that is seen by a
        -- # receiver.
        --
        -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.3
        -- # Once all values have been processed, the
        -- # recipient MUST immediately emit a SETTINGS frame with the ACK flag
        -- # set.
        let peer = foldl apply (cPeer c1) ss
            old = fromIntegral (peerInitialWindowSize (cPeer c1)) :: Int
            delta = fromIntegral (peerInitialWindowSize peer) - old
            streams = M.map (\st -> st { sSendWindow = sSendWindow st + delta }) (cStreams c1)
            -- every window moves by the same amount as each value is
            -- applied in turn: the largest value is the one to check
            highest = maximum (old : [fromIntegral v | InitialWindowSize v <- ss])
            overflows = any ((> maxWindow) . (+ (highest - old)) . sSendWindow) (cStreams c1)
        in
           -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.2
           -- # When the
           -- # value of SETTINGS_INITIAL_WINDOW_SIZE changes, a receiver MUST adjust
           -- # the size of all stream flow-control windows that it maintains by the
           -- # difference between the new value and the old value.
           --
           -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.2
           -- # An endpoint MUST treat a change to SETTINGS_INITIAL_WINDOW_SIZE that
           -- # causes any flow-control window to exceed the maximum size as a
           -- # connection error (Section 5.4.1) of type FLOW_CONTROL_ERROR.
           if overflows then stop acc (goAway c1 FlowControlError)
           else andThen (emitControl [renderSettingsAck] c1 { cPeer = peer, cStreams = streams
                                                    , cEncoder = foldl tableSize (cEncoder c1) ss })
                        pump >>> next
  SettingsAckFrame -> case cSettingsSent c of
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.3
    -- # Upon receiving a SETTINGS frame with the ACK flag set, the
    -- # sender of the altered settings can rely on the values from the oldest
    -- # unacknowledged SETTINGS frame having been applied.
    (_ : rest) -> next (c { cSettingsSent = rest }, [])
    -- an acknowledgement of nothing we sent: as Go and nghttp2 (C10)
    [] -> stop acc (goAway c ProtocolError)
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.7
  -- # Receivers of a PING frame that does not include an ACK flag MUST send
  -- # a PING frame with the ACK flag set in response, with an identical
  -- # frame payload.
  --
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.7
  -- # An endpoint MUST NOT respond to PING frames containing
  -- #    this flag.
  --
  -- PINGs cost no budget (C10): the acknowledgements not yet written are
  -- what is bounded, by the cap on unsent output (C5). A PING restarts
  -- the idle timer, so that a client keeping a quiet connection alive is
  -- not closed (C8).
  PingFrame False d -> emitControl [renderPing True d] (restartIdle now c) >>> next
  -- the answer to the shutdown PING: the final GOAWAY (D10)
  PingFrame True d
    | Pinging _ <- cEnding c, d == shutdownPing -> next (drain c)
    | otherwise -> next (c, [])
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.4
  -- # A client cannot push.  Thus, servers MUST treat the receipt of a
  -- # PUSH_PROMISE frame as a connection error (Section 5.4.1) of type
  -- # PROTOCOL_ERROR.
  PushPromiseFrame _ -> stop acc (goAway c ProtocolError)
  -- the client's GOAWAY (C11): with NO_ERROR, no new streams, and the
  -- close once those open are done ('afterStep'); with an error, the
  -- client is closing the connection (Section 5.4.1), and so do we
  GoAwayFrame _ code _
    | code == NoError -> next (c { cPeerGoAway = True }, [])
    | otherwise -> stop acc (close c [])
  -- PRIORITY_UPDATE (RFC 9218) is free until RFC 9218 is supported (C10)
  UnknownFrame 0x10 _ _ -> next (c, [])
  UnknownFrame _ _ _ -> charge 1 c $ \c1 -> next (c1, [])
  PriorityFrame _ -> charge 1 c $ \c1 -> next (c1, [])
  _ -> stop acc (goAway c InternalError)
  where
    next (c', outs') = advance now c' (outs' : acc)

-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9
-- # A receiver that receives a flow-controlled frame MUST always account
-- # for its contribution against the connection flow-control window,
-- # unless the receiver treats this as a connection error
-- # (Section 5.4.1).  This is necessary even if the frame is in error.

-- | Content for an open stream (C3).
requestData :: Conn -> Word32 -> StreamEntry -> Bool -> ByteString -> Int -> (Conn, [Output])
requestData c sid st end d plen
  -- the stream's window: over it, a stream error, the frame's credit
  -- given back on the connection
  | plen > sRecvWindow st = andThen (giveBack plen c) (\c1 -> streamError c1 sid st FlowControlError)
  | maybe False (< received) (sContentLength st) || end && maybe False (/= received) (sContentLength st) =
      -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1.1
      -- # A request or response is also malformed
      -- # if the value of a content-length header field does not equal the sum
      -- # of the DATA frame payload lengths that form the content, unless the
      -- # message is defined as having no content.
      andThen (giveBack plen c) (\c1 -> streamErrorBecause ContentNotAsDeclared c1 sid st ProtocolError)
  -- an empty DATA that does not end the stream does nothing, and costs
  -- one (C10)
  | len == 0 && not end = charge 1 c $ \c1 -> deliver c1
  | otherwise = deliver c
  where
    len = B.length d
    received = sReceived st + fromIntegral len
    -- a slice under a quarter of its buffer is copied, so that a client
    -- cannot pin more than four times its window (C3)
    d' = if 4 * len < cSource c then B.copy d else d
    deliver c1 =
      let st' = st { sRecvWindow = sRecvWindow st - plen
                   , sReceived = received
                   , sQueued = sQueued st + len
                   -- content arrived: no 100 is owed any more (C9)
                   , sExpect100 = sExpect100 st && len == 0
                   -- a handler waiting has content, or the end (C8)
                   , sWanting = sWanting st && len == 0 && not end
                   , sState = if end then HalfClosedRemote else Open }
          c2 = putStream sid len st' c1
          outs = [RequestData (StreamId sid) d' | len > 0] ++ [RequestEnd (StreamId sid) emptyFields | end]
          -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.1
          -- # The entire
          -- # DATA frame payload is included in flow control, including the Pad
          -- # Length and Padding fields if present.
          --
          -- The padding is credit the handler can never give back: it comes
          -- back at once (C3).
          padding = plen - len
      in if padding == 0 then result c2 outs
         else andThen (giveBackStream sid padding c2) (\c3 -> (c3, outs))

-- | The handler read content: its credit comes back (D6).
consumed :: Word32 -> Int -> Conn -> (Conn, [Output])
consumed sid n c = case M.lookup sid (cStreams c) of
  Just st | sState st /= ClosedHeld ->
    if n > sQueued st then misuse ConsumedMoreThanQueued c
    else giveBackStream sid n c { cStreams = M.insert sid st { sQueued = sQueued st - n } (cStreams c) }
  -- a stream already ended: its credit came back when it ended (C2, C3)
  _ -> result c []

-- | Credit for content on a stream, at both levels: the stream's only
-- while the client may still send on it.
giveBackStream :: Word32 -> Int -> Conn -> (Conn, [Output])
giveBackStream sid n c = case M.lookup sid (cStreams c) of
  Just st | sState st == Open ->
    let unreturned = sUnreturned st + n
        window = streamWindow (cConfig c)
    in if 2 * unreturned >= window
         then let c1 = c { cStreams = M.insert sid st { sUnreturned = 0, sRecvWindow = sRecvWindow st + unreturned } (cStreams c) }
              in andThen (emitControl [renderWindowUpdate (StreamId sid) (fromIntegral unreturned)] c1) (giveBack n)
         else giveBack n c { cStreams = M.insert sid st { sUnreturned = unreturned } (cStreams c) }
  _ -> giveBack n c

-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.1
-- # Receivers are advised to have mechanisms in
-- # place to avoid sending WINDOW_UPDATE frames with very small
-- # increments; see Section 4.2.3.3 of [RFC1122].

-- | Connection credit: a WINDOW_UPDATE once half the window is owed
-- (D6).
giveBack :: Int -> Conn -> (Conn, [Output])
giveBack n c
  | n <= 0 = result c []
  | 2 * unreturned >= window =
      emitControl [renderWindowUpdate (StreamId 0) (fromIntegral unreturned)]
           c { cUnreturned = 0, cRecvWindow = cRecvWindow c + unreturned }
  | otherwise = result c { cUnreturned = unreturned } []
  where
    unreturned = cUnreturned c + n
    window = connectionWindow (cConfig c)

-- one step, then another on its state, the outputs joined
andThen :: (Conn, [Output]) -> (Conn -> (Conn, [Output])) -> (Conn, [Output])
andThen (c, outs) k
  | cClosed c = result c outs
  | otherwise = let (c', outs') = k c in result c' (outs ++ outs')

-- | The handler read with nothing queued: the 100 (Continue), if one is
-- owed, goes now (C9). It never carries END_STREAM (Section 8.1). Until
-- content comes, the client holds the stream up (C8): content passed on
-- and not yet consumed means the handler has not seen it yet, and does
-- not wait.
wantContent :: Word32 -> Conn -> (Conn, [Output])
wantContent sid c = case M.lookup sid (cStreams c) of
  Just st | sState st == Open ->
    let c1 = putStream sid 0 st { sExpect100 = False, sWanting = sQueued st == 0 } c
    in if not (sExpect100 st) then result c1 []
       else
         let (block, enc) = encodeBlock (cEncoder c1) [(":status", "100")]
         in emit (renderHeaders (fromIntegral (peerMaxFrameSize (cPeer c1))) (StreamId sid) False block) c1 { cEncoder = enc }
  _ -> result c []

-- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1.1
-- # Streams
-- # initiated by a client MUST use odd-numbered stream identifiers; those
-- # initiated by the server MUST use even-numbered stream identifiers.

-- | A stream the client has not opened is idle (C4): above the highest
-- it has opened, or even, since even streams are the server's and a
-- server that never pushes (D7) opens none. (Found by the differential
-- test against nghttp2, 2026-11-27: even streams below the highest were
-- taken for closed, so DATA, WINDOW_UPDATE and RST_STREAM on them were
-- discarded instead of a connection error.)
idle :: Word32 -> Conn -> Bool
idle sid c = even sid || sid > cLastStream c

-- the largest encoded field block (C7): four times MAX_HEADER_LIST_SIZE
maxEncoded :: Conn -> Int
maxEncoded c = 4 * fromIntegral (configMaxHeaderListSize (cConfig c))

continuation :: Time -> Conn -> Block -> Bool -> ByteString -> [[Output]] -> (Conn, [Output])
continuation now c b end frag acc
  | len > maxEncoded c = stop acc (goAway c EnhanceYourCalm)
  -- an empty CONTINUATION does nothing, and costs one (C10)
  | B.null frag = charge 1 c $ \c1 -> continue c1
  | otherwise = continue c
  where
    len = bLength b + B.length frag
    b' = b { bFragments = frag : bFragments b, bLength = len }
    continue c1
      | end = fieldBlock now c1 { cBlock = Nothing } b' acc
      | otherwise = advance now c1 { cBlock = Just b' } acc

-- = https://www.rfc-editor.org/rfc/rfc9113#section-4.3
-- # A receiver MUST terminate the connection with a connection error
-- # (Section 5.4.1) of type COMPRESSION_ERROR if it does not decompress a
-- # field block.

-- | A whole field block: decoded, whatever becomes of its stream, so
-- that HPACK's table stays in step (Section 6.8), then given to its
-- stream.
fieldBlock :: Time -> Conn -> Block -> [[Output]] -> (Conn, [Output])
fieldBlock now c b acc =
  case decodeBlock (fromIntegral (configMaxHeaderListSize (cConfig c))) (cDecoder c) block of
    Left _ -> stop acc (goAway c CompressionError)
    Right (decoded, dec) ->
      let c1 = c { cDecoder = dec, cBlock = Nothing }
          (c2, outs) = forStream c1 (bStream b) (bEndStream b) decoded
      in advance now c2 (outs : acc)
  where
    block = B.concat (reverse (bFragments b))

-- A decoded field block, by the state of its stream (C4).
forStream :: Conn -> Word32 -> Bool -> Decoded -> (Conn, [Output])
forStream c sid endStream decoded
  | sid > cLastStream c = newStream c { cLastStream = sid } sid endStream decoded
  | otherwise = case M.lookup sid (cStreams c) of
      Just st -> case sState st of
        -- a second field block on an open stream: its trailers (C7)
        Open -> trailers c sid st endStream decoded
        -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
        -- # If an endpoint receives additional frames, other than
        -- #    WINDOW_UPDATE, PRIORITY, or RST_STREAM, for a stream that is in
        -- #    this state, it MUST respond with a stream error (Section 5.4.2) of
        -- #    type STREAM_CLOSED.
        HalfClosedRemote -> streamError c sid st StreamClosed
        -- reset by us while its handler runs: discarded, as below
        ClosedHeld
          | IntSet.member (fromIntegral sid) (cResetByUs c) -> result c [] -- held
          -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
          -- # An endpoint MAY treat receipt of any other type of frame
          -- #    on a closed stream as a connection error (Section 5.4.1) of type
          -- #    STREAM_CLOSED, except as noted below.
          --
          -- Reset by the client, which sends nothing more on it: the same
          -- answer whether or not its handler has returned
          | otherwise -> goAway c StreamClosed
      -- closed by our reset: minimally processed (decoded above) and
      -- discarded, since the client may have sent it, trailers say,
      -- before it saw the reset (C4)
      Nothing
        | IntSet.member (fromIntegral sid) (cResetByUs c) -> result c [] -- gone
        -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1.1
        -- # An endpoint that
        -- # receives an unexpected stream identifier MUST respond with a
        -- # connection error (Section 5.4.1) of type PROTOCOL_ERROR.
        --
        -- A stream never opened, or one whose request had ended (the
        -- client sends nothing more on it), or that the client reset:
        -- as nine of the ten implementations surveyed (C4)
        | otherwise -> goAway c ProtocolError

newStream :: Conn -> Word32 -> Bool -> Decoded -> (Conn, [Output])
newStream c sid endStream decoded
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.8
  -- # Once the GOAWAY is sent, the sender will ignore frames sent on
  -- # streams initiated by the receiver if the stream has an identifier
  -- # higher than the included last stream identifier.
  --
  -- Above our final GOAWAY's last stream: refused (D10's case 5), not
  -- charged, since the client sent it before it knew
  | sid > cAcceptLimit c = resetStream sid RefusedStream c
  -- after the client's own GOAWAY, no new streams (C11)
  | cPeerGoAway c || active >= configMaxConcurrentStreams cfg || held >= configMaxHeldStreams cfg =
      -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1.2
      -- # An endpoint
      -- # that receives a HEADERS frame that causes its advertised concurrent
      -- # stream limit to be exceeded MUST treat this as a stream error
      -- # (Section 5.4.2) of type PROTOCOL_ERROR or REFUSED_STREAM.
      charge 1 c $ \c1 -> resetStream sid RefusedStream c1
  | otherwise = case decoded of
      ListTooLarge -> refuse tooLarge
      Fields fs
        | length fs > configMaxFields cfg -> refuse tooLarge
        | otherwise -> case requestFromFields (configTarget cfg) fs of
            Left (Malformed _) -> malformed
            Left (CD.Refused status _) -> refuse status
            Right rc -> accept rc
  where
    cfg = cConfig c
    active = fromIntegral (M.size (M.filter ((/= ClosedHeld) . sState) (cStreams c))) :: Word32
    held = M.size (M.filter ((== ClosedHeld) . sState) (cStreams c))
    tooLarge = fromJust (mkStatus 431)
    -- D10's case 3: the response, then NO_ERROR if the request is still
    -- sending; each costs one (C10)
    refuse status = charge 1 c $ \c1 -> respond status sid endStream c1
    -- D10's case 2: the stream error alone
    malformed = charge 1 c $ \c1 -> resetStream sid ProtocolError c1
    accept rc
      | any (/= "100-continue") expectations = refuse ExpectationFailed
      -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1.1
      -- # A request or response is also malformed
      -- # if the value of a content-length header field does not equal the sum
      -- # of the DATA frame payload lengths that form the content, unless the
      -- # message is defined as having no content.
      | endStream, maybe False (/= 0) (controlContentLength rc) = malformed
      | otherwise =
          let st = StreamEntry
                { sState = if endStream then HalfClosedRemote else Open
                , sHandler = True
                , sExpect100 = not endStream && not (null expectations)
                , sContentLength = controlContentLength rc
                , sReceived = 0
                , sRecvWindow = streamWindow cfg
                , sUnreturned = 0
                , sQueued = 0
                , sIsHead = controlMethod rc == HEAD
                , sResponse = Awaiting
                , sSendWindow = fromIntegral (peerInitialWindowSize (cPeer c))
                , sOut = Seq.empty
                , sOutLength = 0
                , sWaiting = False
                , sWanting = False
                , sBlocked = False
                , sSince = cNow c
                , sAllowance = configMinRate cfg * configMinRateGrace cfg
                , sIdleLeft = configStreamIdleTimeout cfg
                , sTimer = Nothing }
              requests = cRequests c + 1
              c1 = c { cStreams = M.insert sid st (cStreams c), cRequests = requests }
              out = [NewRequest (StreamId sid) (RequestHead rc (not endStream))]
          -- D9's optional limit (nginx's keepalive_requests): the last
          -- request accepted, the final GOAWAY at once, so that the
          -- client opens its next requests on a new connection; those it
          -- has sent already are refused, which it may retry (Section 8.7)
          in if configMaxRequests cfg > 0 && requests >= configMaxRequests cfg && cEnding c /= Draining
               then andThen (result c1 out) drain
               else result c1 out
      where
        -- C9: Expect stays in the fields; any expectation but
        -- 100-continue is answered with 417, as HTTP/1.1 answers it
        expectations = [ C.map toLower (fieldValueBytes v)
                       | (n, v) <- fieldsToList (controlFields rc), fieldNameBytes n == "expect" ]

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1
-- # An endpoint that receives a HEADERS frame without the END_STREAM flag
-- # set after receiving the HEADERS frame that opens a request or after
-- # receiving a final (non-informational) status code MUST treat the
-- # corresponding request or response as malformed (Section 8.1.1).
trailers :: Conn -> Word32 -> StreamEntry -> Bool -> Decoded -> (Conn, [Output])
trailers c sid st endStream decoded
  | not endStream = streamError c sid st ProtocolError
  | otherwise = case decoded of
      ListTooLarge -> streamError c sid st ProtocolError
      Fields fs -> case trailersFromFields fs of
        Left _ -> streamError c sid st ProtocolError
        Right tf
          | maybe False (/= sReceived st) (sContentLength st) ->
              -- content shorter than its declared length (Section 8.1.1)
              streamErrorBecause ContentNotAsDeclared c sid st ProtocolError
          | otherwise ->
              result (putStream sid 0 st { sState = HalfClosedRemote, sExpect100 = False, sWanting = False } c)
                     [RequestEnd (StreamId sid) tf]

-- | A stream error (Section 5.4.2): RST_STREAM with the code, the
-- handler told, the stream closed; it costs one (C10).
streamError :: Conn -> Word32 -> StreamEntry -> ErrorCode -> (Conn, [Output])
streamError c sid st code = streamErrorBecause (ResetByServer code) c sid st code

streamErrorBecause :: AbortReason -> Conn -> Word32 -> StreamEntry -> ErrorCode -> (Conn, [Output])
streamErrorBecause reason c sid st code = charge 1 c $ \c1 ->
  andThen (ended sid st c1) $ \c2 ->
    let (c3, outs) = resetStream sid code c2
    in result c3 (RequestAborted (StreamId sid) reason : outs)

-- | A stream ended early: out of the map, or kept closed while its
-- handler still runs (C6). Content passed on and not consumed will never
-- be: its connection credit comes back (C3).
ended :: Word32 -> StreamEntry -> Conn -> (Conn, [Output])
ended sid st c =
  let c0 = (forget sid c) { cBuffered = cBuffered c - sOutLength st }
      c1 | sHandler st = c0 { cStreams = M.insert sid st { sState = ClosedHeld, sQueued = 0
                                                           , sOut = Seq.empty, sOutLength = 0, sWaiting = False
                                                           , sWanting = False, sBlocked = False, sTimer = Nothing } (cStreams c0) }
         | otherwise = c0 { cStreams = M.delete sid (cStreams c0) }
  -- its buffered response freed: other writes may be waiting on the
  -- connection's bound (C5)
  in andThen (giveBack (sQueued st) c1) (if sOutLength st > 0 then writable else (`result` []))

-- | A response the core writes itself (D10's case 3): the status with no
-- content, then RST_STREAM NO_ERROR if the request is still sending.
respond :: Status -> Word32 -> Bool -> Conn -> (Conn, [Output])
respond status sid requestEnded c =
  let fields = [(":status", C.pack (show (statusCode status))), ("content-length", "0")]
      (block, enc) = encodeBlock (cEncoder c) fields
      frames = renderHeaders (fromIntegral (peerMaxFrameSize (cPeer c))) (StreamId sid) True block
  in andThen (emit frames c { cEncoder = enc })
             (if requestEnded then (`result` []) else resetStream sid NoError)

-- | A handler returned. Before its response, it failed: the core
-- answers 500 (C2). While its response is being sent, the stream stays
-- until the response is out, its handler no longer counted.
handlerDone :: Word32 -> Conn -> (Conn, [Output])
handlerDone sid c = case M.lookup sid (cStreams c) of
  Nothing -> result c []
  Just st -> case sState st of
    ClosedHeld -> result c { cStreams = M.delete sid (cStreams c) } []
    s' -> case sResponse st of
      Awaiting -> andThen (giveBack (sQueued st) (forget sid c) { cStreams = M.delete sid (cStreams c) })
                          (respond InternalServerError sid (s' == HalfClosedRemote))
      -- returned without ending its content: as Abort (D10 case 4)
      Sending _ _ Nothing -> abort sid c
      Sending {} -> result c { cStreams = M.insert sid st { sHandler = False } (cStreams c) } []

------------------------------------------------------------------------------
-- Timers per stream (C8)
------------------------------------------------------------------------------

-- A stream's clock runs only while the client is what holds it up. Two
-- timers read it: the idle timer, which content moved restarts, and the
-- minimum rate, an allowance that starts at the grace period's worth
-- (240 octets a second for 5 s), drains at the rate while the clock
-- runs, and grows by each octet moved. The allowance is capped at the
-- idle timeout's worth, so that a fast start cannot bank hours for a
-- slow drip afterwards (an average since the stream began, Kestrel's
-- measure, would let 100 MB moved fast buy 116 hours at one octet a
-- second); a stall is then never allowed longer than the idle timeout.

-- | Whether the client holds the stream up: the handler waits for
-- content, or there is response content (or its end) that the client's
-- windows or its socket have not taken. After every step, buffered
-- content means exactly that, since the scheduler sends what it can.
holdsUp :: StreamEntry -> Bool
holdsUp st = sState st /= ClosedHeld &&
  (sWanting st && sState st == Open || case sResponse st of
     Sending _ _ end -> sOutLength st > 0 || isJust end
     Awaiting -> False)

-- the clock run up to now, if it runs
runClock :: Config -> Time -> StreamEntry -> StreamEntry
runClock cfg now st
  | sBlocked st =
      let e = now - sSince st
      in st { sSince = now
            , sAllowance = sAllowance st - min (sAllowance st) (configMinRate cfg * e)
            , sIdleLeft = sIdleLeft st - min (sIdleLeft st) e }
  | otherwise = st { sSince = now }

-- | When the stream's timers expire if its clock runs on, if either is
-- enabled. It only ever moves later: running the clock leaves it where
-- it is, and content moved or a pause puts it later.
streamDeadline :: Config -> StreamEntry -> Maybe Time
streamDeadline cfg st = case [ sSince st + sAllowance st `div` configMinRate cfg | configMinRate cfg > 0 ]
                          ++ [ sSince st + sIdleLeft st | configStreamIdleTimeout cfg > 0 ] of
  [] -> Nothing
  ds -> Just (minimum ds)

-- | A stream's entry stored, after a change that moved this many octets
-- of content: its clock run, its timers given the progress, and its
-- deadline entered if its clock has started.
putStream :: Word32 -> Int -> StreamEntry -> Conn -> Conn
putStream sid moved st c =
  let cfg = cConfig c
      st1 = runClock cfg (cNow c) st
      st2 | moved > 0 = st1 { sAllowance = min (configMinRate cfg * max (configMinRateGrace cfg) (configStreamIdleTimeout cfg))
                                               (sAllowance st1 + 1000 * fromIntegral moved)
                            , sIdleLeft = configStreamIdleTimeout cfg }
          | otherwise = st1
      st3 = st2 { sBlocked = holdsUp st2 }
  in c { cStreams = M.insert sid st3 (cStreams c)
       , cTouched = if sBlocked st3 && isNothing (sTimer st3) then sid : cTouched c else cTouched c }

-- | At the end of a step, a deadline for each stream still held up that
-- has none: not before, since a write holds its stream up only until
-- the scheduler sends it, in the same step.
armTimers :: Conn -> Conn
armTimers c
  | null (cTouched c) = c
  | otherwise = foldl arm c { cTouched = [] } (cTouched c)
  where
    arm c1 sid = case M.lookup sid (cStreams c1) of
      Just st | sBlocked st, isNothing (sTimer st), Just d <- streamDeadline (cConfig c1) st ->
        c1 { cStreams = M.insert sid st { sTimer = Just d } (cStreams c1), cTimers = S.insert (d, sid) (cTimers c1) }
      _ -> c1

-- | The stream's deadline taken out, as it leaves the map or is closed.
forget :: Word32 -> Conn -> Conn
forget sid c = case M.lookup sid (cStreams c) >>= sTimer of
  Just t -> c { cTimers = S.delete (t, sid) (cTimers c) }
  Nothing -> c

------------------------------------------------------------------------------
-- Responses (C5, C12)
------------------------------------------------------------------------------

-- the largest flow-control window (Section 6.9.1)
maxWindow :: Int
maxWindow = 2147483647

-- = https://www.rfc-editor.org/rfc/rfc9113#section-10.5
-- # Providing tiny increments to flow control in WINDOW_UPDATE frames can
-- #    cause a sender to generate a large number of DATA frames.
--
-- So a WINDOW_UPDATE that leaves a window under this costs nuisance
-- budget (C10), which bounds the tiny DATA frames a client can draw.
-- DATA is sent within whatever window there is, however small, as all
-- ten implementations surveyed do (decided 2026-11-27, after h2spec's
-- first run: a floor held a one-octet window's DATA back for ever,
-- docs/http2-driver-design.md).
dataFloor :: Int
dataFloor = 1024

-- | The handler's response head. A response with no content (HEAD,
-- 204, 304, no content at all) is a HEADERS with END_STREAM, with the
-- declared content-length for HEAD (Section 8.1.1: such a response "MAY
-- have a non-zero content-length header field").
respondHandler :: Word32 -> ResponseHead -> Conn -> (Conn, [Output])
respondHandler sid h c = case M.lookup sid (cStreams c) of
  Just st | sState st /= ClosedHeld -> case sResponse st of
    Awaiting ->
      let headerOnly = not (respHasContent h) || sIsHead st
          fields = responseToFields (respStatus h) (respFields h)
                   ++ [("content-length", C.pack (show n)) | Just n <- [respLength h], respHasContent h]
          (block, enc) = encodeBlock (cEncoder c) fields
          frames = renderHeaders (fromIntegral (peerMaxFrameSize (cPeer c))) (StreamId sid) headerOnly block
          c1 = c { cEncoder = enc }
      in if headerOnly then andThen (emit frames c1) (finished sid st)
         else emit frames c1 { cStreams = M.insert sid st { sResponse = Sending (respLength h) 0 Nothing } (cStreams c1) }
    _ -> misuse RespondTwice c
  -- a stream no longer served: the handler raced a reset (C2)
  _ -> result c []

-- | Response content: queued for the scheduler (C5). Each write is
-- answered by one 'Writable', at once while the stream's and the
-- connection's buffers are under their bounds, else when they are again;
-- the driver writes again only after it. A write of content beyond the
-- declared length aborts the response, as on HTTP/1.1 (C12). A write
-- to a response that is no longer sent is discarded, and answered.
writeBody :: Word32 -> ByteString -> Conn -> (Conn, [Output])
writeBody sid d c = case M.lookup sid (cStreams c) of
  Just st | sState st /= ClosedHeld -> case sResponse st of
    Awaiting -> misuse BodyBeforeRespond c -- WriteBody
    Sending decl written end
      | sWaiting st -> misuse WroteBeforeWritable c
      | isJust end -> result c [Writable (StreamId sid)]
      | maybe False (< written') decl -> abort sid c
      | otherwise ->
          let st' = st { sResponse = Sending decl written' Nothing
                       , sOut = if B.null d then sOut st else sOut st |> d
                       , sOutLength = sOutLength st + B.length d
                       , sWaiting = True }
          in pump (schedule sid (putStream sid 0 st' c { cBuffered = cBuffered c + B.length d }))
      where written' = written + fromIntegral (B.length d)
  -- a header-only response, or a stream no longer served
  _ -> result c [Writable (StreamId sid)]

-- | The end of the content, with trailers. Short of the declared length,
-- the response is aborted (C12).
endBody :: Word32 -> Fields -> Conn -> (Conn, [Output])
endBody sid tr c = case M.lookup sid (cStreams c) of
  Just st | sState st /= ClosedHeld -> case sResponse st of
    Awaiting -> misuse BodyBeforeRespond c -- EndBody
    Sending decl written Nothing
      | maybe False (/= written) decl -> abort sid c
      | otherwise -> pump (schedule sid (putStream sid 0 st { sResponse = Sending decl written (Just tr) } c))
    Sending {} -> result c []
  _ -> result c []

-- | The handler failed after responding, or its content was not as
-- declared: RST_STREAM INTERNAL_ERROR, and the handler told (D10 case 4).
abort :: Word32 -> Conn -> (Conn, [Output])
abort sid c = case M.lookup sid (cStreams c) of
  Just st | sState st /= ClosedHeld ->
    andThen (ended sid st c) $ \c1 ->
      let (c2, outs) = resetStream sid InternalError c1
      in result c2 (RequestAborted (StreamId sid) (ResetByServer InternalError) : outs)
  _ -> result c []

-- | A stream's response is complete. A request still sending is told to
-- stop (D6: RST_STREAM NO_ERROR, which Section 8.1 allows); the stream is
-- closed, its slot freed, the credit of content never consumed given
-- back.
finished :: Word32 -> StreamEntry -> Conn -> (Conn, [Output])
finished sid st c =
  let stillSending = sState st == Open
      c1 = (forget sid c) { cStreams = M.delete sid (cStreams c), cBuffered = cBuffered c - sOutLength st }
  in andThen (if stillSending then resetStream sid NoError c1 else result c1 [])
             (giveBack (sQueued st))

-- | Put a stream in the round, if it is not there (D8).
schedule :: Word32 -> Conn -> Conn
schedule sid c
  | sid `elem` cOrder c = c
  | otherwise = c { cOrder = cOrder c |> sid }

-- the stream's buffer, or the connection's, is full (C5)
full :: StreamEntry -> Conn -> Bool
full st c = sOutLength st >= configStreamBuffer (cConfig c) || cBuffered c >= configConnectionBuffer (cConfig c)

-- | Send what the windows and the transport allow: one frame at a time
-- from each stream that can send, in turn (D8), while the DATA emitted
-- and not yet written is under the pacing bound (C5). Then 'Writable'
-- for the streams waiting whose buffers have room again.
pump :: Conn -> (Conn, [Output])
pump c0 = go c0 [] (Seq.length (cOrder c0))
  where
    go c acc 0 = andThen (result c (concat (reverse acc))) writable
    go c acc k
      | cUnsent c >= configPacing (cConfig c) = go c acc 0
      | otherwise = case viewl (cOrder c) of
          EmptyL -> go c acc 0
          sid :< rest -> case M.lookup sid (cStreams c) of
            Just st | sState st /= ClosedHeld, Sending {} <- sResponse st ->
              case sendOne sid st c { cOrder = rest } of
                -- nothing could be sent: to the back of the round, untouched
                Nothing -> go c { cOrder = rest |> sid } acc (k - 1)
                Just (c', outs) -> if cClosed c' then result c' (concat (reverse (outs : acc)))
                                   else go c' (outs : acc) (Seq.length (cOrder c'))
            -- a stream no longer sending: out of the round
            _ -> go c { cOrder = rest } acc (k - 1)

-- | One frame of a stream's response, if the windows allow: DATA (with
-- END_STREAM when it carries the last of the content and there are no
-- trailers), or the trailers' HEADERS.
sendOne :: Word32 -> StreamEntry -> Conn -> Maybe (Conn, [Output])
sendOne sid st c = case sResponse st of
  Sending _ _ end
    | sOutLength st > 0 ->
        let n = minimum [ sOutLength st, sSendWindow st, cSendWindow c, dataFrameSize c ]
        -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.2
        -- # A sender MUST
        -- # track the negative flow-control window and MUST NOT send new flow-
        -- # controlled frames until it receives WINDOW_UPDATE frames that cause
        -- # the flow-control window to become positive.
        in if n <= 0 then Nothing
           else
             let (pieces, rest) = takeOut n (sOut st)
                 lastOne = n == sOutLength st
                 endHere = lastOne && maybe False isEmptyFields end
                 st' = st { sOut = rest, sOutLength = sOutLength st - n, sSendWindow = sSendWindow st - n }
                 c1 = putStream sid n st' c { cSendWindow = cSendWindow c - n, cBuffered = cBuffered c - n }
                 (c2, outs) = emit (renderDataPieces (StreamId sid) endHere pieces) c1
             in Just $ if endHere then andThen (c2, outs) (finished sid st')
                       else result (if lastOne && isJust end || not lastOne then schedule sid c2 else c2) outs
    | Just tr <- end ->
        if isEmptyFields tr
          -- the content ended with nothing left to send: an empty DATA
          -- with END_STREAM (Section 6.9.1 allows it even with no window)
          then Just (andThen (emit (renderData (StreamId sid) True B.empty) c) (finished sid st))
          else
            let (block, enc) = encodeBlock (cEncoder c) (trailersToFields tr)
                frames = renderHeaders (fromIntegral (peerMaxFrameSize (cPeer c))) (StreamId sid) True block
            in Just (andThen (emit frames c { cEncoder = enc }) (finished sid st))
    | otherwise -> Nothing
  Awaiting -> Nothing

-- | DATA frames carry at most 16 KiB, whatever larger frames the client
-- allows, so that streams take turns finely (D8) and one frame never
-- approaches the unsent-output cap (C5).
dataFrameSize :: Conn -> Int
dataFrameSize c = min 16384 (fromIntegral (peerMaxFrameSize (cPeer c)))

isEmptyFields :: Fields -> Bool
isEmptyFields = null . fieldsToList

-- the first n octets of the queued chunks, as pieces, and the rest
takeOut :: Int -> Seq ByteString -> ([ByteString], Seq ByteString)
takeOut = go []
  where
    go acc 0 q = (reverse acc, q)
    go acc n q = case viewl q of
      EmptyL -> (reverse acc, q)
      b :< rest
        | B.length b <= n -> go (b : acc) (n - B.length b) rest
        | otherwise -> (reverse (B.take n b : acc), B.drop n b Seq.<| rest)

-- | 'Writable' for each write waiting whose stream's and connection's
-- buffers have room (C5).
writable :: Conn -> (Conn, [Output])
writable c =
  let ready = [ sid | (sid, st) <- M.toList (cStreams c), sWaiting st, not (full st c) ]
      streams = foldl (\m sid -> M.adjust (\st -> st { sWaiting = False }) sid m) (cStreams c) ready
  in result c { cStreams = streams } [Writable (StreamId sid) | sid <- ready]

(>>>) :: a -> (a -> b) -> b
x >>> g = g x
infixl 1 >>>

-- | Spend nuisance budget, or end the connection when it is gone
-- (C10).
charge :: Word64 -> Conn -> (Conn -> (Conn, [Output])) -> (Conn, [Output])
charge n c k
  | cBudget c < n * 1000 = goAway c EnhanceYourCalm
  | otherwise = k c { cBudget = cBudget c - n * 1000 }

-- the peer's HEADER_TABLE_SIZE bounds our encoder's table (H4)
tableSize :: Encoder -> Setting -> Encoder
tableSize e (HeaderTableSize v) = setPeerTableSize (fromIntegral v) e
tableSize e _ = e

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
  -- the server ignores RFC 7540's priority signals whatever the client
  -- sends (D8)
  NoRFC7540Priorities _ -> p

-- | Send frames, counting them as emitted (C5). Over the cap on what
-- has been emitted and not yet written, the connection is ended
-- instead: a client that does not read cannot grow the queue (C5).
emit :: [ByteString] -> Conn -> (Conn, [Output])
emit frames c
  | cUnsent c + size > configMaxUnsent (cConfig c) = goAway c EnhanceYourCalm
  | otherwise = emitUncapped frames c
  where size = sum (map B.length frames)

emitUncapped :: [ByteString] -> Conn -> (Conn, [Output])
emitUncapped frames c =
  let n = sum (map B.length frames)
  in result c { cUnsent = cUnsent c + n, cEmitted = cEmitted c + n } [Send frames]

-- | Send control frames that answer the client (acknowledgements,
-- WINDOW_UPDATE, RST_STREAM), which are not paced: those emitted and not
-- yet written are capped by number as well as by size, so that a client
-- that does not read cannot have them pile up (C5, D9; as Go caps them).
emitControl :: [ByteString] -> Conn -> (Conn, [Output])
emitControl frames c
  | Seq.length (cControlEnds c) + length frames > configMaxControlFrames (cConfig c) = goAway c EnhanceYourCalm
  | otherwise =
      let ends = Seq.fromList (drop 1 (scanl (+) (cEmitted c) (map B.length frames)))
      in emit frames c { cControlEnds = cControlEnds c Seq.>< ends }

-- | RST_STREAM for a stream, and the stream remembered (the latest
-- 'configResetsKept', the oldest forgotten first), so that a field block
-- the client sent before it saw the reset is told from an identifier
-- that was never opened (C4; decided 2026-11-27, as Rust's h2 and Kazu
-- Yamamoto's http2 remember their resets).
resetStream :: Word32 -> ErrorCode -> Conn -> (Conn, [Output])
resetStream sid code c = emitControl [renderRstStream (StreamId sid) code] (remember c)
  where
    k = fromIntegral sid
    limit = configResetsKept (cConfig c)
    remember c0
      | limit <= 0 || IntSet.member k (cResetByUs c0) = c0
      | cResetCount c0 >= limit = c0 { cResetByUs = IntSet.insert k (IntSet.deleteMin (cResetByUs c0)) }
      | otherwise = c0 { cResetByUs = IntSet.insert k (cResetByUs c0), cResetCount = cResetCount c0 + 1 }

-- | The control frames the transport has now written leave the count.
dropWritten :: Conn -> Conn
dropWritten c = c { cControlEnds = Seq.dropWhileL (<= cEmitted c - cUnsent c) (cControlEnds c) }

-- = https://www.rfc-editor.org/rfc/rfc9113#section-5.4.1
-- # An endpoint that encounters a connection error SHOULD first send a
-- # GOAWAY frame (Section 6.8) with the stream identifier of the last
-- # stream that it successfully received from its peer.

-- | GOAWAY with the code, then close (D10).
goAway :: Conn -> ErrorCode -> (Conn, [Output])
goAway c code =
  let (c', outs) = emitUncapped [renderGoAway (StreamId (lastProcessed c)) code B.empty] c
  in close c' outs

misuse :: Misuse -> Conn -> (Conn, [Output])
misuse m c = close c [ProtocolMisuse m]

-- | The connection ends: every handler still waiting on a stream is told
-- (C2), so that none waits for ever. Those of streams already reset were
-- told then.
close :: Conn -> [Output] -> (Conn, [Output])
close c outs =
  let aborts = [ RequestAborted (StreamId sid) ConnectionEnded
               | (sid, st) <- M.toList (cStreams c), sHandler st, sState st /= ClosedHeld ]
  in result c { cClosed = True } (outs ++ aborts ++ [Close])

-- | The new state, evaluated, and the outputs (as HTTP/1.1's core: a
-- lazily built state is a thunk the driver runs at once).
result :: Conn -> [Output] -> (Conn, [Output])
result !c outs = (c, outs)
{-# INLINE result #-}
