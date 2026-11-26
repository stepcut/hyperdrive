{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | The HTTP/2 connection core (docs/http2-core-design.md, "The
-- connection core", C1-C13): a pure 'step' that the driver runs on the
-- connection's owner thread (D4).
--
-- Built in parts. Part 1: the prefaces, SETTINGS, PING, frames of
-- unknown type, connection errors, the connection's timers and D9's
-- nuisance budget. Part 2: streams and their field blocks, to
-- 'NewRequest' (C4, C6, C7, D10, D11), client resets, trailers, and the
-- responses the core writes itself. DATA and WINDOW_UPDATE are not
-- handled yet: they end the connection with INTERNAL_ERROR, so that
-- nothing half-handled passes silently.
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
  , Misuse(..)
  , connectionPreface
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Word (Word32, Word64)
import Hyperdrive.HPACK.Decode
import Hyperdrive.HPACK.Encode (Encoder, encodeBlock, newEncoder, setPeerTableSize)
import Hyperdrive.HTTP2.Frame
import Hyperdrive.Semantics.ControlData hiding (Refused)
import qualified Hyperdrive.Semantics.ControlData as CD
import Hyperdrive.Semantics.Field (Fields, fieldNameBytes, fieldValueBytes, fieldsToList)
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
  , configStreamWindow         :: !Word32  -- ^ D6: 1 MiB
  , configConnectionWindow     :: !Word32  -- ^ D6: 1 MiB
  , configHeadTimeout          :: !Time    -- ^ C8: the preface and first SETTINGS, 30 s
  , configSettingsTimeout      :: !Time    -- ^ C8: our SETTINGS unacknowledged, 10 s
  , configBudgetBurst          :: !Word64  -- ^ C10: 1,000
  , configBudgetRate           :: !Word64  -- ^ C10: 100 a second
  , configMaxSettings          :: !Int     -- ^ C10: settings in one SETTINGS frame, 100
  , configMaxUnsent            :: !Int     -- ^ C5: octets emitted and not yet written, 1 MiB
  , configMaxHeldStreams       :: !Int     -- ^ C6: reset streams whose handlers still run, 100
  , configMaxFields            :: !Int     -- ^ D9: fields in a request, 100
  , configBlockTimeout         :: !Time    -- ^ C8: a field block left unfinished, 30 s
  , configTarget               :: !TargetContext
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
  , configTarget = TargetContext HTTP Nothing False
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
  | ResetByServer !ErrorCode        -- ^ a stream error: the request was malformed, say
  | StreamTimedOut
  | ContentNotAsDeclared            -- ^ the handler's reads throw IncompleteBody
  | ConnectionEnded
  deriving (Eq, Show)

-- | A request for a handler: D11's mapping of its field list, and
-- whether content follows.
data RequestHead = RequestHead
  { requestControl    :: !RequestControl
  , requestHasContent :: !Bool
  } deriving (Show)

data Output
  = NewRequest !StreamId !RequestHead
  | RequestEnd !StreamId !Fields    -- ^ the end of the request's content, with its trailers
  | RequestAborted !StreamId !AbortReason
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
  }

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
  , cLastStream  :: !Word32         -- ^ the highest client stream opened (C4)
  , cStreams     :: !(M.Map Word32 StreamEntry) -- ^ the streams still served (C4)
  , cBlock       :: !(Maybe Block)  -- ^ a field block being received (C7)
  , cDecoder     :: !Decoder
  , cEncoder     :: !Encoder
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
        , cMore = []
        , cMoreLength = 0
        , cPeer = initialPeerSettings
        , cHeadDeadline = Just (now + configHeadTimeout config)
        , cSettingsSent = [now]
        , cBudget = configBudgetBurst config * 1000
        , cBudgetAt = now
        , cUnsent = sum (map B.length out)
        , cLastStream = 0
        , cStreams = M.empty
        , cBlock = Nothing
        , cDecoder = newDecoder 4096 (fromIntegral (configHeaderTableSize config))
        , cEncoder = newEncoder 4096
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
                       ++ (if cPhase c == Running then take 1 (cSettingsSent c) else [])
                       ++ maybe [] (pure . bDeadline) (cBlock c) of
      [] -> Nothing
      ds -> Just (minimum ds)

------------------------------------------------------------------------------
-- Stepping
------------------------------------------------------------------------------

step :: Time -> Input -> Conn -> (Conn, [Output])
step _ _ c | cClosed c = result c []
step now input c0 = let c = refill now c0 in case input of
  Received bs -> advance now c { cMore = bs : cMore c, cMoreLength = cMoreLength c + B.length bs } []
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
  HandlerDone (StreamId sid) -> handlerDone sid c

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
      -- a field block left unfinished (C8): the whole connection waits
      -- on it, since nothing may come between its frames
      else if maybe False ((<= now) . bDeadline) (cBlock c) then goAway c ProtocolError
      -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.3
      -- # If the sender of a SETTINGS frame does not receive an acknowledgment
      -- # within a reasonable amount of time, it MAY issue a connection error
      -- # (Section 5.4.1) of type SETTINGS_TIMEOUT.
      else goAway c SettingsTimeout
  _ -> misuse TimedOutWithoutDeadline c

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
  | otherwise = c { cBuffer = B.concat (cBuffer c : reverse (cMore c)), cMore = [], cMoreLength = 0 }

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
  RstStreamFrame (StreamId sid) code
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
    -- # Receiving any frame other than HEADERS or PRIORITY on a stream in
    -- #    this state MUST be treated as a connection error (Section 5.4.1)
    -- #    of type PROTOCOL_ERROR.
    | sid > cLastStream c -> stop acc (goAway c ProtocolError)
    | otherwise -> case M.lookup sid (cStreams c) of
        Just st | sState st /= ClosedHeld ->
          -- a client's reset before our response ended (rapid reset,
          -- C10); our responses come in part 4
          charge 1 c $ \c1 -> next (ended sid st c1, [RequestAborted (StreamId sid) (ResetByClient code)])
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
    -- Refused outright, as Go does (C10).
    | count > configMaxSettings (cConfig c) || hasRepeats ss -> stop acc (goAway c ProtocolError)
    -- one, and one for each setting after the first six (C10)
    | otherwise -> charge (1 + fromIntegral (max 0 (count - 6))) c $ \c1 ->
        -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.3
        -- # Once all values have been processed, the
        -- # recipient MUST immediately emit a SETTINGS frame with the ACK flag
        -- # set.
        emit [renderSettingsAck] c1 { cPeer = foldl apply (cPeer c1) ss
                                    , cEncoder = foldl tableSize (cEncoder c1) ss } >>> next
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
  -- what is bounded, by the cap on unsent output (C5).
  PingFrame False d -> emit [renderPing True d] c >>> next
  PingFrame True _ -> next (c, [])
  -- PRIORITY_UPDATE (RFC 9218) is free until RFC 9218 is supported (C10)
  UnknownFrame 0x10 _ _ -> next (c, [])
  UnknownFrame _ _ _ -> charge 1 c $ \c1 -> next (c1, [])
  PriorityFrame _ -> charge 1 c $ \c1 -> next (c1, [])
  _ -> stop acc (goAway c InternalError)
  where
    next (c', outs') = advance now c' (outs' : acc)
    hasRepeats ss = length (nub (map settingId ss)) /= length ss

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
        ClosedHeld -> result c []
      -- closed: minimally processed (decoded above) and discarded (C4)
      Nothing -> result c []

newStream :: Conn -> Word32 -> Bool -> Decoded -> (Conn, [Output])
newStream c sid endStream decoded
  | active >= configMaxConcurrentStreams cfg || held >= configMaxHeldStreams cfg =
      -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1.2
      -- # An endpoint
      -- # that receives a HEADERS frame that causes its advertised concurrent
      -- # stream limit to be exceeded MUST treat this as a stream error
      -- # (Section 5.4.2) of type PROTOCOL_ERROR or REFUSED_STREAM.
      charge 1 c $ \c1 -> emit [renderRstStream (StreamId sid) RefusedStream] c1
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
    malformed = charge 1 c $ \c1 -> emit [renderRstStream (StreamId sid) ProtocolError] c1
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
                , sReceived = 0 }
          in result c { cStreams = M.insert sid st (cStreams c) }
                    [NewRequest (StreamId sid) (RequestHead rc (not endStream))]
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
              result c { cStreams = M.insert sid st { sState = HalfClosedRemote, sExpect100 = False } (cStreams c) }
                     [RequestEnd (StreamId sid) tf]

-- | A stream error (Section 5.4.2): RST_STREAM with the code, the
-- handler told, the stream closed; it costs one (C10).
streamError :: Conn -> Word32 -> StreamEntry -> ErrorCode -> (Conn, [Output])
streamError c sid st code = streamErrorBecause (ResetByServer code) c sid st code

streamErrorBecause :: AbortReason -> Conn -> Word32 -> StreamEntry -> ErrorCode -> (Conn, [Output])
streamErrorBecause reason c sid st code = charge 1 c $ \c1 ->
  let (c2, outs) = emit [renderRstStream (StreamId sid) code] (ended sid st c1)
  in result c2 (RequestAborted (StreamId sid) reason : outs)

-- | A stream ended early: out of the map, or kept closed while its
-- handler still runs (C6).
ended :: Word32 -> StreamEntry -> Conn -> Conn
ended sid st c
  | sHandler st = c { cStreams = M.insert sid st { sState = ClosedHeld } (cStreams c) }
  | otherwise = c { cStreams = M.delete sid (cStreams c) }

-- | A response the core writes itself (D10's case 3): the status with no
-- content, then RST_STREAM NO_ERROR if the request is still sending.
respond :: Status -> Word32 -> Bool -> Conn -> (Conn, [Output])
respond status sid requestEnded c =
  let fields = [(":status", C.pack (show (statusCode status))), ("content-length", "0")]
      (block, enc) = encodeBlock (cEncoder c) fields
      frames = renderHeaders (fromIntegral (peerMaxFrameSize (cPeer c))) (StreamId sid) True block
               ++ [renderRstStream (StreamId sid) NoError | not requestEnded]
  in emit frames c { cEncoder = enc }

-- | A handler returned. Before its response (which comes in part 4), it
-- failed: the core answers 500 (C2).
handlerDone :: Word32 -> Conn -> (Conn, [Output])
handlerDone sid c = case M.lookup sid (cStreams c) of
  Nothing -> result c []
  Just st -> case sState st of
    ClosedHeld -> result c { cStreams = M.delete sid (cStreams c) } []
    s' -> respond InternalServerError sid (s' == HalfClosedRemote) c { cStreams = M.delete sid (cStreams c) }

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

-- | Send frames, counting them as emitted (C5). Over the cap on what
-- has been emitted and not yet written, the connection is ended
-- instead: a client that does not read cannot grow the queue (C5).
emit :: [ByteString] -> Conn -> (Conn, [Output])
emit frames c
  | cUnsent c + size > configMaxUnsent (cConfig c) = goAway c EnhanceYourCalm
  | otherwise = emitUncapped frames c
  where size = sum (map B.length frames)

emitUncapped :: [ByteString] -> Conn -> (Conn, [Output])
emitUncapped frames c = result c { cUnsent = cUnsent c + sum (map B.length frames) } [Send frames]

-- = https://www.rfc-editor.org/rfc/rfc9113#section-5.4.1
-- # An endpoint that encounters a connection error SHOULD first send a
-- # GOAWAY frame (Section 6.8) with the stream identifier of the last
-- # stream that it successfully received from its peer.

-- | GOAWAY with the code, then close (D10).
goAway :: Conn -> ErrorCode -> (Conn, [Output])
goAway c code =
  let (c', outs) = emitUncapped [renderGoAway (StreamId (cLastStream c)) code B.empty] c
  in close c' outs

misuse :: Misuse -> Conn -> (Conn, [Output])
misuse m c = close c [ProtocolMisuse m]

close :: Conn -> [Output] -> (Conn, [Output])
close c outs = result c { cClosed = True } (outs ++ [Close])

-- | The new state, evaluated, and the outputs (as HTTP/1.1's core: a
-- lazily built state is a thunk the driver runs at once).
result :: Conn -> [Output] -> (Conn, [Output])
result !c outs = (c, outs)
{-# INLINE result #-}
