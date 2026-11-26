{-# LANGUAGE PatternSynonyms #-}
-- | HTTP/2 frames (RFC 9113, Sections 4 and 6): parsing one frame from
-- the start of the received octets, and serializing the frames a server
-- sends.
--
-- The parser makes every check that needs no connection state; the
-- connection core makes the rest (docs/http2-core-design.md, "Frames",
-- F1-F5). Payloads are slices of the input (F5).
module Hyperdrive.HTTP2.Frame
  ( -- * Identifiers and codes
    StreamId(..)
  , ErrorCode(..)
  , pattern NoError, pattern ProtocolError, pattern InternalError
  , pattern FlowControlError, pattern SettingsTimeout, pattern StreamClosed
  , pattern FrameSizeError, pattern RefusedStream, pattern Cancel
  , pattern CompressionError, pattern ConnectError, pattern EnhanceYourCalm
  , pattern InadequateSecurity, pattern HTTP11Required
  , Setting(..)
    -- * Frames
  , Frame(..)
  , frameHeaderLength
  , defaultMaxFrameSize
    -- * Parsing
  , Parsed(..)
  , FrameError(..)
  , Scope(..)
  , Reason(..)
  , parseFrame
    -- * Serializing
  , renderData
  , renderDataPieces
  , renderHeaders
  , renderRstStream
  , renderSettings
  , renderSettingsAck
  , renderPing
  , renderGoAway
  , renderWindowUpdate
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word16, Word32, Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)

-- | A stream identifier: 31 bits; 0 is the connection.
newtype StreamId = StreamId Word32
  deriving (Eq, Ord, Show)

-- = https://www.rfc-editor.org/rfc/rfc9113#section-7
-- # Unknown or unsupported error codes MUST NOT trigger any special
-- # behavior.

-- | An error code (Section 7). A newtype rather than a sum, since a
-- peer may send codes this module does not know.
newtype ErrorCode = ErrorCode Word32
  deriving (Eq, Ord, Show)

pattern NoError, ProtocolError, InternalError, FlowControlError, SettingsTimeout,
  StreamClosed, FrameSizeError, RefusedStream, Cancel, CompressionError,
  ConnectError, EnhanceYourCalm, InadequateSecurity, HTTP11Required :: ErrorCode
pattern NoError            = ErrorCode 0x0
pattern ProtocolError      = ErrorCode 0x1
pattern InternalError      = ErrorCode 0x2
pattern FlowControlError   = ErrorCode 0x3
pattern SettingsTimeout    = ErrorCode 0x4
pattern StreamClosed       = ErrorCode 0x5
pattern FrameSizeError     = ErrorCode 0x6
pattern RefusedStream      = ErrorCode 0x7
pattern Cancel             = ErrorCode 0x8
pattern CompressionError   = ErrorCode 0x9
pattern ConnectError       = ErrorCode 0xa
pattern EnhanceYourCalm    = ErrorCode 0xb
pattern InadequateSecurity = ErrorCode 0xc
pattern HTTP11Required     = ErrorCode 0xd

-- | The settings of Section 6.5.2, with values already checked.
data Setting
  = HeaderTableSize !Word32
  | EnablePush !Bool
  | MaxConcurrentStreams !Word32
  | InitialWindowSize !Word32       -- ^ at most 2^31 - 1
  | MaxFrameSize !Word32            -- ^ from 2^14 to 2^24 - 1
  | MaxHeaderListSize !Word32
  | NoRFC7540Priorities !Bool       -- ^ RFC 9218 Section 2.1 (D8)
  deriving (Eq, Show)

-- | A frame, as the core needs it: flags as fields, unused flags,
-- padding and priority signals gone.
data Frame
  = DataFrame !StreamId !Bool !ByteString !Int
    -- ^ END_STREAM, the data, and the whole payload's length (padding
    -- included), which flow control counts (Section 6.1)
  | HeadersFrame !StreamId !Bool !Bool !ByteString
    -- ^ END_STREAM, END_HEADERS, the field block fragment
  | PriorityFrame !StreamId
    -- ^ deprecated (Section 5.3.2): checked, then ignored (D8)
  | RstStreamFrame !StreamId !ErrorCode
  | SettingsFrame !Int [Setting]
    -- ^ the number of settings in the frame, and those this module
    -- knows, in the order sent (the others are dropped, but counted,
    -- so that the core can bound them; C10)
  | SettingsAckFrame
  | PushPromiseFrame !StreamId
    -- ^ only its stream: a server refuses every one (D7)
  | PingFrame !Bool !ByteString
    -- ^ ACK, and the 8 octets of opaque data
  | GoAwayFrame !StreamId !ErrorCode !ByteString
    -- ^ the last stream identifier, the code, the debug data
  | WindowUpdateFrame !StreamId !Word32
    -- ^ the increment, 1 to 2^31 - 1
  | ContinuationFrame !StreamId !Bool !ByteString
    -- ^ END_HEADERS, the field block fragment
  | UnknownFrame !Word8 !StreamId !Int
    -- ^ a type this module does not know, its stream, its length (F1)
  deriving (Eq, Show)

frameHeaderLength :: Int
frameHeaderLength = 9

-- | 2^14, the initial SETTINGS_MAX_FRAME_SIZE.
defaultMaxFrameSize :: Int
defaultMaxFrameSize = 16384

data Scope = Connection | Stream !StreamId
  deriving (Eq, Show)

-- | Why a frame was refused.
data Reason
  = TooLong              -- ^ longer than our SETTINGS_MAX_FRAME_SIZE (F3)
  | WrongLength          -- ^ a length the frame type does not allow
  | NeedsStream          -- ^ stream 0 for a frame that needs a stream
  | NeedsConnection      -- ^ a stream for a frame that applies to the connection
  | PaddingTooLong       -- ^ the padding leaves no room for the rest
  | AckWithPayload       -- ^ a SETTINGS acknowledgement with settings
  | BadEnablePush        -- ^ SETTINGS_ENABLE_PUSH other than 0 or 1
  | BadInitialWindowSize -- ^ SETTINGS_INITIAL_WINDOW_SIZE above 2^31 - 1
  | BadMaxFrameSize      -- ^ SETTINGS_MAX_FRAME_SIZE outside 2^14 to 2^24 - 1
  | BadNoRFC7540Priorities -- ^ SETTINGS_NO_RFC7540_PRIORITIES other than 0 or 1
  | ZeroIncrement        -- ^ a WINDOW_UPDATE of 0
  deriving (Eq, Show)

-- | A refused frame: the error's scope (which the core may widen, F2),
-- its code, and why.
data FrameError = FrameError !Scope !ErrorCode !Reason
  deriving (Eq, Show)

data Parsed
  = Incomplete !Int
    -- ^ the frame is not all there: the octets it needs in all
  | Parsed !Frame !Int
    -- ^ a frame, and the octets it took
  | Refused !FrameError
  deriving (Eq, Show)

word16 :: ByteString -> Int -> Word16
word16 b i = fromIntegral (BU.unsafeIndex b i) `shiftL` 8 .|. fromIntegral (BU.unsafeIndex b (i + 1))

word32 :: ByteString -> Int -> Word32
word32 b i = fromIntegral (word16 b i) `shiftL` 16 .|. fromIntegral (word16 b (i + 2))

-- = https://www.rfc-editor.org/rfc/rfc9113#section-4.1
-- # Reserved:  A reserved 1-bit field.  The semantics of this bit are
-- #    undefined, and the bit MUST remain unset (0x00) when sending and
-- #    MUST be ignored when receiving.

-- a 31-bit field after a reserved bit
word31 :: ByteString -> Int -> Word32
word31 b i = word32 b i .&. 0x7FFFFFFF

connection :: ErrorCode -> Reason -> Parsed
connection c r = Refused (FrameError Connection c r)

-- | Parse the frame at the start of the input, given our
-- SETTINGS_MAX_FRAME_SIZE.
parseFrame :: Int -> ByteString -> Parsed
parseFrame maxSize input
  | B.length input < frameHeaderLength = Incomplete frameHeaderLength
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-4.2
  -- # An endpoint MUST send an error code of FRAME_SIZE_ERROR if a frame
  -- # exceeds the size defined in SETTINGS_MAX_FRAME_SIZE, exceeds any
  -- # limit defined for the frame type, or is too small to contain
  -- # mandatory frame data.  A frame size error in a frame that could alter
  -- # the state of the entire connection MUST be treated as a connection
  -- # error (Section 5.4.1); this includes any frame carrying a field block
  -- # (Section 4.3) (that is, HEADERS, PUSH_PROMISE, and CONTINUATION), a
  -- # SETTINGS frame, and any frame with a stream identifier of 0.
  --
  -- A connection error for every type (F3), found before the payload is
  -- read.
  | len > maxSize = connection FrameSizeError TooLong
  | B.length input < total = Incomplete total
  | otherwise = case payloadOf typ flags sid payload of
      Right f -> Parsed f total
      Left e  -> Refused e
  where
    len = fromIntegral (BU.unsafeIndex input 0) `shiftL` 16
      .|. fromIntegral (word16 input 1) :: Int
    typ = BU.unsafeIndex input 3
    flags = BU.unsafeIndex input 4
    sid = StreamId (word31 input 5)
    total = frameHeaderLength + len
    payload = BU.unsafeTake len (BU.unsafeDrop frameHeaderLength input)

payloadOf :: Word8 -> Word8 -> StreamId -> ByteString -> Either FrameError Frame
payloadOf typ flags sid p = case typ of
  0x0 -> dataFrame flags sid p
  0x1 -> headersFrame flags sid p
  0x2 -> priorityFrame sid p
  0x3 -> rstStreamFrame sid p
  0x4 -> settingsFrame flags sid p
  0x5 -> Right (PushPromiseFrame sid)
  0x6 -> pingFrame flags sid p
  0x7 -> goAwayFrame sid p
  0x8 -> windowUpdateFrame sid p
  0x9 -> continuationFrame flags sid p
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-4.1
  -- # Implementations MUST ignore and
  -- # discard frames of unknown types.
  --
  -- The core ignores it; it is returned so that the core can refuse
  -- one inside a field block and count it (F1).
  _ -> Right (UnknownFrame typ sid (B.length p))

err :: ErrorCode -> Reason -> Either FrameError a
err c r = Left (FrameError Connection c r)

-- frames that belong to a stream
needsStream :: StreamId -> Either FrameError ()
needsStream (StreamId 0) = err ProtocolError NeedsStream
needsStream _ = Right ()

-- frames that belong to the connection
needsConnection :: StreamId -> Either FrameError ()
needsConnection (StreamId 0) = Right ()
needsConnection _ = err ProtocolError NeedsConnection

flag :: Word8 -> Int -> Bool
flag = testBit

-- | Remove the padding of a PADDED frame (Sections 6.1, 6.2), given how
-- many octets of other fields follow the Pad Length: what is left
-- between the Pad Length and the padding.
unpad :: Word8 -> Int -> ByteString -> Either FrameError ByteString
unpad flags fixed p
  | not (flag flags 3) =
      if B.length p < fixed then err FrameSizeError WrongLength else Right p
  | B.length p < 1 + fixed = err FrameSizeError WrongLength
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.1
  -- # The total number of padding octets is determined by the value of the
  -- # Pad Length field.  If the length of the padding is the length of the
  -- # frame payload or greater, the recipient MUST treat this as a
  -- # connection error (Section 5.4.1) of type PROTOCOL_ERROR.
  --
  -- With HEADERS' priority fields also present, the padding may not
  -- reach into them either.
  | 1 + fixed + padLen > B.length p = err ProtocolError PaddingTooLong
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.1
  -- # Padding octets MUST be set to zero when sending.  A receiver is
  -- #    not obligated to verify padding but MAY treat non-zero padding as
  -- #    a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
  --
  -- The padding's contents are not checked (F4): none of nine
  -- implementations surveyed checks them. The serializer never sends
  -- padding.
  | otherwise = Right (B.take (B.length p - 1 - padLen) (B.drop 1 p))
  where
    padLen = fromIntegral (B.head p)

dataFrame :: Word8 -> StreamId -> ByteString -> Either FrameError Frame
dataFrame flags sid p = do
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.1
  -- # DATA frames MUST be associated with a stream.  If a DATA frame is
  -- # received whose Stream Identifier field is 0x00, the recipient MUST
  -- # respond with a connection error (Section 5.4.1) of type
  -- # PROTOCOL_ERROR.
  needsStream sid
  d <- unpad flags 0 p
  Right (DataFrame sid (flag flags 0) d (B.length p))

headersFrame :: Word8 -> StreamId -> ByteString -> Either FrameError Frame
headersFrame flags sid p = do
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.2
  -- # HEADERS frames MUST be associated with a stream.  If a HEADERS frame
  -- # is received whose Stream Identifier field is 0x00, the recipient MUST
  -- # respond with a connection error (Section 5.4.1) of type
  -- # PROTOCOL_ERROR.
  needsStream sid
  let priority = flag flags 5
  rest <- unpad flags (if priority then 5 else 0) p
  -- the deprecated priority fields (Section 5.3.2) are dropped (D8)
  let fragment = if priority then B.drop 5 rest else rest
  Right (HeadersFrame sid (flag flags 0) (flag flags 2) fragment)

priorityFrame :: StreamId -> ByteString -> Either FrameError Frame
priorityFrame sid p = do
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.3
  -- # The PRIORITY frame always identifies a stream.  If a PRIORITY frame
  -- # is received with a stream identifier of 0x00, the recipient MUST
  -- # respond with a connection error (Section 5.4.1) of type
  -- # PROTOCOL_ERROR.
  needsStream sid
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.3
  -- # A PRIORITY frame with a length other than 5 octets MUST be treated as
  -- # a stream error (Section 5.4.2) of type FRAME_SIZE_ERROR.
  if B.length p /= 5 then Left (FrameError (Stream sid) FrameSizeError WrongLength)
    else Right (PriorityFrame sid)

rstStreamFrame :: StreamId -> ByteString -> Either FrameError Frame
rstStreamFrame sid p = do
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.4
  -- # RST_STREAM frames MUST be associated with a stream.  If a RST_STREAM
  -- # frame is received with a stream identifier of 0x00, the recipient
  -- # MUST treat this as a connection error (Section 5.4.1) of type
  -- # PROTOCOL_ERROR.
  needsStream sid
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.4
  -- # A RST_STREAM frame with a length other than 4 octets MUST be treated
  -- # as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR.
  if B.length p /= 4 then err FrameSizeError WrongLength
    else Right (RstStreamFrame sid (ErrorCode (word32 p 0)))

settingsFrame :: Word8 -> StreamId -> ByteString -> Either FrameError Frame
settingsFrame flags sid p
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5
  -- # The stream identifier for a SETTINGS frame MUST be zero (0x00).  If
  -- # an endpoint receives a SETTINGS frame whose Stream Identifier field
  -- # is anything other than 0x00, the endpoint MUST respond with a
  -- # connection error (Section 5.4.1) of type PROTOCOL_ERROR.
  | sid /= StreamId 0 = err ProtocolError NeedsConnection
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5
  -- # Receipt of a SETTINGS frame with the ACK flag set and a
  -- #    length field value other than 0 MUST be treated as a connection
  -- #    error (Section 5.4.1) of type FRAME_SIZE_ERROR.
  | flag flags 0 = if B.null p then Right SettingsAckFrame else err FrameSizeError AckWithPayload
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5
  -- # A SETTINGS frame with a length other than a multiple of 6 octets MUST
  -- # be treated as a connection error (Section 5.4.1) of type
  -- # FRAME_SIZE_ERROR.
  | B.length p `mod` 6 /= 0 = err FrameSizeError WrongLength
  | otherwise = SettingsFrame (B.length p `div` 6) <$> settings 0
  where
    settings i
      | i >= B.length p = Right []
      | otherwise = do
          s <- setting (word16 p i) (word32 p (i + 2))
          rest <- settings (i + 6)
          Right (maybe rest (: rest) s)

setting :: Word16 -> Word32 -> Either FrameError (Maybe Setting)
setting ident v = case ident of
  0x1 -> Right (Just (HeaderTableSize v))
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.2
  -- # Any value other than 0 or 1 MUST be
  -- #    treated as a connection error (Section 5.4.1) of type
  -- #    PROTOCOL_ERROR.
  0x2 | v > 1 -> err ProtocolError BadEnablePush
      | otherwise -> Right (Just (EnablePush (v == 1)))
  0x3 -> Right (Just (MaxConcurrentStreams v))
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.2
  -- # Values above the maximum flow-control window size of 2^31-1 MUST
  -- #    be treated as a connection error (Section 5.4.1) of type
  -- #    FLOW_CONTROL_ERROR.
  0x4 | v > 0x7FFFFFFF -> err FlowControlError BadInitialWindowSize
      | otherwise -> Right (Just (InitialWindowSize v))
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.2
  -- # The value advertised
  -- #    by an endpoint MUST be between this initial value and the maximum
  -- #    allowed frame size (2^24-1 or 16,777,215 octets), inclusive.
  -- #    Values outside this range MUST be treated as a connection error
  -- #    (Section 5.4.1) of type PROTOCOL_ERROR.
  0x5 | v < 0x4000 || v > 0xFFFFFF -> err ProtocolError BadMaxFrameSize
      | otherwise -> Right (Just (MaxFrameSize v))
  0x6 -> Right (Just (MaxHeaderListSize v))
  -- RFC 9218 Section 2.1 (not in duvet; RFC 9218 is not yet supported, D8):
  --   The value of SETTINGS_NO_RFC7540_PRIORITIES MUST be 0 or 1.  Any
  --   value other than 0 or 1 MUST be treated as a connection error (see
  --   Section 5.4.1 of [HTTP/2]) of type PROTOCOL_ERROR.
  0x9 | v > 1 -> err ProtocolError BadNoRFC7540Priorities
      | otherwise -> Right (Just (NoRFC7540Priorities (v == 1)))
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.2
  -- # An endpoint that receives a SETTINGS frame with any unknown or
  -- # unsupported identifier MUST ignore that setting.
  _ -> Right Nothing

pingFrame :: Word8 -> StreamId -> ByteString -> Either FrameError Frame
pingFrame flags sid p = do
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.7
  -- # PING frames are not associated with any individual stream.  If a PING
  -- # frame is received with a Stream Identifier field value other than
  -- # 0x00, the recipient MUST respond with a connection error
  -- # (Section 5.4.1) of type PROTOCOL_ERROR.
  needsConnection sid
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.7
  -- # Receipt of a PING frame with a length field value other than 8 MUST
  -- # be treated as a connection error (Section 5.4.1) of type
  -- # FRAME_SIZE_ERROR.
  if B.length p /= 8 then err FrameSizeError WrongLength
    else Right (PingFrame (flag flags 0) p)

goAwayFrame :: StreamId -> ByteString -> Either FrameError Frame
goAwayFrame sid p = do
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.8
  -- # An endpoint MUST treat a GOAWAY frame with a stream identifier other
  -- # than 0x00 as a connection error (Section 5.4.1) of type
  -- # PROTOCOL_ERROR.
  needsConnection sid
  -- the last stream identifier and the code are mandatory (Section 4.2:
  -- "too small to contain mandatory frame data")
  if B.length p < 8 then err FrameSizeError WrongLength
    else Right (GoAwayFrame (StreamId (word31 p 0)) (ErrorCode (word32 p 4)) (B.drop 8 p))

windowUpdateFrame :: StreamId -> ByteString -> Either FrameError Frame
windowUpdateFrame sid p
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9
  -- # A WINDOW_UPDATE frame with a length other than 4 octets MUST be
  -- # treated as a connection error (Section 5.4.1) of type
  -- # FRAME_SIZE_ERROR.
  | B.length p /= 4 = err FrameSizeError WrongLength
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9
  -- # A receiver MUST treat the receipt of a WINDOW_UPDATE frame with a
  -- # flow-control window increment of 0 as a stream error (Section 5.4.2)
  -- # of type PROTOCOL_ERROR; errors on the connection flow-control window
  -- # MUST be treated as a connection error (Section 5.4.1).
  | increment == 0 = Left (FrameError (if sid == StreamId 0 then Connection else Stream sid)
                                      ProtocolError ZeroIncrement)
  | otherwise = Right (WindowUpdateFrame sid increment)
  where
    increment = word31 p 0

continuationFrame :: Word8 -> StreamId -> ByteString -> Either FrameError Frame
continuationFrame flags sid p = do
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.10
  -- # CONTINUATION frames MUST be associated with a stream.  If a
  -- # CONTINUATION frame is received with a Stream Identifier field of
  -- # 0x00, the recipient MUST respond with a connection error
  -- # (Section 5.4.1) of type PROTOCOL_ERROR.
  needsStream sid
  Right (ContinuationFrame sid (flag flags 2) p)

------------------------------------------------------------------------------
-- Serializing
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9113#section-4.1
-- # Unused flags are those that have no defined semantics for a
-- #    particular frame type.  Unused flags MUST be ignored on receipt
-- #    and MUST be left unset (0x00) when sending.

-- | A frame header: the payload's length, the type, the flags, the
-- stream (its reserved bit unset).
header :: Int -> Word8 -> Word8 -> StreamId -> ByteString
header len typ flags (StreamId s) = BI.unsafeCreate frameHeaderLength $ \p -> do
  poke p (fromIntegral (len `shiftR` 16) :: Word8)
  poke (p `plusPtr` 1) (fromIntegral (len `shiftR` 8) :: Word8)
  poke (p `plusPtr` 2) (fromIntegral len :: Word8)
  poke (p `plusPtr` 3) typ
  poke (p `plusPtr` 4) flags
  poke32 (p `plusPtr` 5) (s .&. 0x7FFFFFFF)

poke32 :: Ptr Word8 -> Word32 -> IO ()
poke32 p w = do
  poke p (fromIntegral (w `shiftR` 24) :: Word8)
  poke (p `plusPtr` 1) (fromIntegral (w `shiftR` 16) :: Word8)
  poke (p `plusPtr` 2) (fromIntegral (w `shiftR` 8) :: Word8)
  poke (p `plusPtr` 3) (fromIntegral w :: Word8)

bytes32 :: [Word32] -> ByteString
bytes32 ws = BI.unsafeCreate (4 * length ws) $ \p ->
  sequence_ [poke32 (p `plusPtr` (4 * i)) w | (i, w) <- zip [0 ..] ws]

-- | A DATA frame: the stream, END_STREAM, the data (no longer than the
-- peer's SETTINGS_MAX_FRAME_SIZE, which is the caller's to ensure). The
-- header and the data are separate pieces, for a gathering write.
renderData :: StreamId -> Bool -> ByteString -> [ByteString]
renderData sid end d = [header (B.length d) 0x0 (if end then 0x1 else 0) sid, d]

-- | A DATA frame whose data is several pieces, written as they are (a
-- gathering write), so that queued chunks need not be joined.
renderDataPieces :: StreamId -> Bool -> [ByteString] -> [ByteString]
renderDataPieces sid end ds = header (sum (map B.length ds)) 0x0 (if end then 0x1 else 0) sid : ds

-- | A field block as a HEADERS frame and as many CONTINUATION frames as
-- the peer's SETTINGS_MAX_FRAME_SIZE requires: the stream, END_STREAM,
-- the block.
renderHeaders :: Int -> StreamId -> Bool -> ByteString -> [ByteString]
renderHeaders maxSize sid end block =
  header (B.length first) 0x1 (endStream .|. endHeaders (B.null rest)) sid : first : continuations rest
  where
    (first, rest) = B.splitAt maxSize block
    endStream = if end then 0x1 else 0
    endHeaders lastOne = if lastOne then 0x4 else 0
    continuations b
      | B.null b = []
      | otherwise = let (c, r) = B.splitAt maxSize b
                    in header (B.length c) 0x9 (endHeaders (B.null r)) sid : c : continuations r

renderRstStream :: StreamId -> ErrorCode -> ByteString
renderRstStream sid (ErrorCode c) = header 4 0x3 0 sid <> bytes32 [c]

renderSettings :: [Setting] -> ByteString
renderSettings ss = header (6 * length ss) 0x4 0 (StreamId 0) <> B.concat (map one ss)
  where
    one s = let (i, v) = pair s
            in B.pack [fromIntegral (i `shiftR` 8), fromIntegral i] <> bytes32 [v]
    pair :: Setting -> (Word16, Word32)
    pair (HeaderTableSize v)      = (0x1, v)
    pair (EnablePush b)           = (0x2, if b then 1 else 0)
    pair (MaxConcurrentStreams v) = (0x3, v)
    pair (InitialWindowSize v)    = (0x4, v)
    pair (MaxFrameSize v)         = (0x5, v)
    pair (MaxHeaderListSize v)    = (0x6, v)
    pair (NoRFC7540Priorities b)  = (0x9, if b then 1 else 0)

renderSettingsAck :: ByteString
renderSettingsAck = header 0 0x4 0x1 (StreamId 0)

-- | A PING: ACK, and the 8 octets of opaque data.
renderPing :: Bool -> ByteString -> ByteString
renderPing ack d = header 8 0x6 (if ack then 0x1 else 0) (StreamId 0) <> d

renderGoAway :: StreamId -> ErrorCode -> ByteString -> ByteString
renderGoAway (StreamId lastId) (ErrorCode c) debug =
  header (8 + B.length debug) 0x7 0 (StreamId 0) <> bytes32 [lastId .&. 0x7FFFFFFF, c] <> debug

renderWindowUpdate :: StreamId -> Word32 -> ByteString
renderWindowUpdate sid inc = header 4 0x8 0 sid <> bytes32 [inc .&. 0x7FFFFFFF]
