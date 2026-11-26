{-# LANGUAGE OverloadedStrings #-}
module FrameTests (frameTests) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (mapMaybe)
import Data.Word (Word16, Word32, Word8)
import Hyperdrive.HTTP2.Frame
import Test.QuickCheck hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

frameTests :: TestTree
frameTests = testGroup "Frames (RFC 9113 Sections 4 and 6)"
  [ testProperty "every valid frame parses to what it says, whatever its unused flags and reserved bits" prop_valid
  , testProperty "a frame not all there asks for the octets it needs" prop_incomplete
  , testProperty "each rule broken once gives that rule's error" prop_nearMisses
  , testProperty "the boundaries, by example" prop_boundaries
  , testProperty "frames over a maximum other than the default" prop_otherMaxima
  , testProperty "rendered frames parse back" prop_render
  , testProperty "a field block is split into HEADERS and CONTINUATIONs of at most the peer's size" prop_renderHeaders
  ]

------------------------------------------------------------------------------
-- frames on the wire, as Section 4.1 draws them
------------------------------------------------------------------------------

data Wire = Wire
  { wType     :: Word8
  , wFlags    :: Word8
  , wReserved :: Bool
  , wStream   :: Word32
  , wPayload  :: ByteString
  } deriving (Show)

be16 :: Word16 -> ByteString
be16 x = B.pack [fromIntegral (x `shiftR` 8), fromIntegral x]

be32 :: Word32 -> ByteString
be32 x = B.pack [fromIntegral (x `shiftR` s) | s <- [24, 16, 8, 0]]

encodeWire :: Wire -> ByteString
encodeWire w =
  B.pack [fromIntegral (len `shiftR` 16), fromIntegral (len `shiftR` 8), fromIntegral len, wType w, wFlags w]
  <> be32 ((if wReserved w then 0x80000000 else 0) .|. wStream w)
  <> wPayload w
  where len = B.length (wPayload w)

ourMax :: Int
ourMax = defaultMaxFrameSize

------------------------------------------------------------------------------
-- valid frames, each with the frame it should parse to
------------------------------------------------------------------------------

streamId :: Gen Word32
streamId = frequency [(3, choose (1, 20)), (1, choose (1, 0x7FFFFFFF))]

-- the flags given, and any of the others at random (unused, so ignored)
withUnused :: Word8 -> Word8 -> Gen Word8
withUnused defined set = (\r -> set .|. (r .&. complement' defined)) <$> arbitrary
  where complement' d = 0xFF - d

bytes :: Int -> Gen ByteString
bytes n = B.pack <$> vectorOf n arbitrary

someBytes :: Gen ByteString
someBytes = sized $ \s -> choose (0, s * 4) >>= bytes

-- PADDED: a Pad Length, then the rest, then zero padding
padded :: ByteString -> Gen (Bool, ByteString)
padded body = do
  p <- arbitrary
  if not p then pure (False, body) else do
    n <- frequency [(3, choose (0, 10)), (1, choose (0, 255))]
    pure (True, B.singleton (fromIntegral n) <> body <> B.replicate n 0)

genValid :: Gen (Wire, Frame)
genValid = oneof
  [ do -- DATA
      sid <- streamId; end <- arbitrary; d <- someBytes
      (pad, payload) <- padded d
      fl <- withUnused 0x09 ((if end then 0x1 else 0) .|. (if pad then 0x8 else 0))
      r <- arbitrary
      pure (Wire 0x0 fl r sid payload, DataFrame (StreamId sid) end d (B.length payload))
  , do -- HEADERS
      sid <- streamId; end <- arbitrary; endH <- arbitrary; frag <- someBytes
      prio <- arbitrary
      fields <- bytes 5
      (pad, payload) <- padded ((if prio then fields else "") <> frag)
      fl <- withUnused 0x2D ((if end then 0x1 else 0) .|. (if endH then 0x4 else 0)
                             .|. (if pad then 0x8 else 0) .|. (if prio then 0x20 else 0))
      r <- arbitrary
      pure (Wire 0x1 fl r sid payload, HeadersFrame (StreamId sid) end endH frag)
  , do -- PRIORITY
      sid <- streamId; p <- bytes 5; fl <- arbitrary; r <- arbitrary
      pure (Wire 0x2 fl r sid p, PriorityFrame (StreamId sid))
  , do -- RST_STREAM
      sid <- streamId; c <- arbitrary; fl <- arbitrary; r <- arbitrary
      pure (Wire 0x3 fl r sid (be32 c), RstStreamFrame (StreamId sid) (ErrorCode c))
  , do -- SETTINGS, with settings this module does not know mixed in
      ss <- listOf genSetting
      fl <- withUnused 0x01 0; r <- arbitrary
      pure (Wire 0x4 fl r 0 (B.concat [be16 i <> be32 v | (i, v, _) <- ss]), SettingsFrame (length ss) (mapMaybe (\(_, _, s) -> s) ss))
  , do -- SETTINGS acknowledgement
      fl <- withUnused 0x01 0x01; r <- arbitrary
      pure (Wire 0x4 fl r 0 "", SettingsAckFrame)
  , do -- PUSH_PROMISE: only its stream matters, a server refuses it
      sid <- choose (0, 0x7FFFFFFF); p <- someBytes; fl <- arbitrary; r <- arbitrary
      pure (Wire 0x5 fl r sid p, PushPromiseFrame (StreamId sid))
  , do -- PING
      ack <- arbitrary; d <- bytes 8
      fl <- withUnused 0x01 (if ack then 0x1 else 0); r <- arbitrary
      pure (Wire 0x6 fl r 0 d, PingFrame ack d)
  , do -- GOAWAY
      lastId <- choose (0, 0x7FFFFFFF); reservedLast <- arbitrary; c <- arbitrary
      debug <- frequency [(1, pure ""), (2, someBytes)]
      fl <- arbitrary; r <- arbitrary
      pure ( Wire 0x7 fl r 0 (be32 ((if reservedLast then 0x80000000 else 0) .|. lastId) <> be32 c <> debug)
           , GoAwayFrame (StreamId lastId) (ErrorCode c) debug )
  , do -- WINDOW_UPDATE, on a stream or on the connection
      sid <- oneof [pure 0, streamId]; inc <- choose (1, 0x7FFFFFFF); reservedInc <- arbitrary
      fl <- arbitrary; r <- arbitrary
      pure ( Wire 0x8 fl r sid (be32 ((if reservedInc then 0x80000000 else 0) .|. inc))
           , WindowUpdateFrame (StreamId sid) inc )
  , do -- CONTINUATION
      sid <- streamId; endH <- arbitrary; frag <- someBytes
      fl <- withUnused 0x04 (if endH then 0x4 else 0); r <- arbitrary
      pure (Wire 0x9 fl r sid frag, ContinuationFrame (StreamId sid) endH frag)
  , do -- a type this module does not know
      t <- choose (0xA, 0xFF); sid <- choose (0, 0x7FFFFFFF); p <- someBytes; fl <- arbitrary; r <- arbitrary
      pure (Wire t fl r sid p, UnknownFrame t (StreamId sid) (B.length p))
  ]

-- a setting on the wire, and what it parses to (Nothing: unknown, dropped)
genSetting :: Gen (Word16, Word32, Maybe Setting)
genSetting = oneof
  [ (\v -> (0x1, v, Just (HeaderTableSize v))) <$> arbitrary
  , (\b -> (0x2, if b then 1 else 0, Just (EnablePush b))) <$> arbitrary
  , (\v -> (0x3, v, Just (MaxConcurrentStreams v))) <$> arbitrary
  , (\v -> (0x4, v, Just (InitialWindowSize v))) <$> oneof [choose (0, 0x7FFFFFFF), pure 0x7FFFFFFF]
  , (\v -> (0x5, v, Just (MaxFrameSize v))) <$> oneof [choose (0x4000, 0xFFFFFF), elements [0x4000, 0xFFFFFF]]
  , (\v -> (0x6, v, Just (MaxHeaderListSize v))) <$> arbitrary
  , (\b -> (0x9, if b then 1 else 0, Just (NoRFC7540Priorities b))) <$> arbitrary
  , (\i v -> (i, v, Nothing)) <$> oneof [pure 0, choose (0x7, 0x8), choose (0xA, 0xFFFF)] <*> arbitrary
  ]

-- = https://www.rfc-editor.org/rfc/rfc9113#section-4.1
-- # Unused flags are those that have no defined semantics for a
-- #    particular frame type.  Unused flags MUST be ignored on receipt
-- #    and MUST be left unset (0x00) when sending.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-4.1
-- # Implementations MUST ignore and
-- # discard frames of unknown types.
prop_valid :: Property
prop_valid = forAll genValid $ \(w, f) -> forAll someBytes $ \rest ->
  let wire = encodeWire w
  in parseFrame ourMax (wire <> rest) === Parsed f (B.length wire)

prop_incomplete :: Property
prop_incomplete = forAll genValid $ \(w, _) ->
  let wire = encodeWire w
  in forAll (choose (0, B.length wire - 1)) $ \k ->
       parseFrame ourMax (B.take k wire)
         === Incomplete (if k < 9 then 9 else B.length wire)

------------------------------------------------------------------------------
-- near misses: one rule broken, and the error it must give
------------------------------------------------------------------------------

conn :: ErrorCode -> Reason -> Parsed
conn c r = Refused (FrameError Connection c r)

genNearMiss :: Gen (String, ByteString, Parsed)
genNearMiss = oneof
  [ do -- longer than SETTINGS_MAX_FRAME_SIZE: refused from the header alone
      t <- choose (0, 0xFF); sid <- choose (0, 20)
      extra <- frequency [(2, pure 1), (1, choose (1, 100))]
      let w = encodeWire (Wire t 0 False sid (B.replicate (ourMax + extra) 0))
      k <- choose (9, B.length w)
      pure ("too long", B.take k w, conn FrameSizeError TooLong)
  , do -- any 24-bit length above the limit, from the header alone
      len <- frequency [(1, choose (ourMax + 1, 0xFFFFFF)), (1, choose (0x10000, 0xFFFFFF))]
      t <- choose (0, 0xFF)
      let h = B.pack [fromIntegral (len `shiftR` 16), fromIntegral (len `shiftR` 8), fromIntegral (len :: Int), t, 0, 0, 0, 0, 1]
      pure ("too long, header only", h, conn FrameSizeError TooLong)
  , do t <- elements [0x0, 0x1, 0x2, 0x3, 0x9]
       p <- case t of
         0x2 -> bytes 5
         0x3 -> bytes 4
         _ -> someBytes
       pure ("stream 0", encodeWire (Wire t 0 False 0 p), conn ProtocolError NeedsStream)
  , do t <- elements [0x4, 0x6, 0x7]; sid <- streamId
       let p = case t of { 0x6 -> B.replicate 8 0; 0x7 -> B.replicate 8 0; _ -> "" }
       pure ("not stream 0", encodeWire (Wire t 0 False sid p), conn ProtocolError NeedsConnection)
  , do sid <- streamId; n <- lengthOtherThan 5
       pure ("PRIORITY length", encodeWire (Wire 0x2 0 False sid (B.replicate n 0)),
             Refused (FrameError (Stream (StreamId sid)) FrameSizeError WrongLength))
  , do sid <- streamId; n <- lengthOtherThan 4
       pure ("RST_STREAM length", encodeWire (Wire 0x3 0 False sid (B.replicate n 0)), conn FrameSizeError WrongLength)
  , do n <- lengthOtherThan 8; ack <- elements [0, 1]
       pure ("PING length", encodeWire (Wire 0x6 ack False 0 (B.replicate n 0)), conn FrameSizeError WrongLength)
  , do sid <- oneof [pure 0, streamId]; n <- lengthOtherThan 4
       pure ("WINDOW_UPDATE length", encodeWire (Wire 0x8 0 False sid (B.replicate n 1)), conn FrameSizeError WrongLength)
  , do n <- choose (0, 7)
       pure ("GOAWAY too short", encodeWire (Wire 0x7 0 False 0 (B.replicate n 0)), conn FrameSizeError WrongLength)
  , do n <- suchThat (choose (1, 40)) (\x -> x `mod` 6 /= 0)
       pure ("SETTINGS length", encodeWire (Wire 0x4 0 False 0 (B.replicate n 0)), conn FrameSizeError WrongLength)
  , do n <- choose (1, 40)
       pure ("SETTINGS ACK with payload", encodeWire (Wire 0x4 1 False 0 (B.replicate n 0)), conn FrameSizeError AckWithPayload)
  , do (pre, post) <- settingsAround
       v <- frequency [(2, pure 2), (1, choose (2, maxBound))]
       pure ("SETTINGS_ENABLE_PUSH", settingsWire (pre ++ [(0x2, v)] ++ post), conn ProtocolError BadEnablePush)
  , do (pre, post) <- settingsAround
       v <- frequency [(2, pure 2), (1, choose (2, maxBound))]
       pure ("SETTINGS_NO_RFC7540_PRIORITIES", settingsWire (pre ++ [(0x9, v)] ++ post), conn ProtocolError BadNoRFC7540Priorities)
  , do (pre, post) <- settingsAround
       v <- frequency [(2, pure 0x80000000), (1, choose (0x80000000, maxBound))]
       pure ("SETTINGS_INITIAL_WINDOW_SIZE", settingsWire (pre ++ [(0x4, v)] ++ post), conn FlowControlError BadInitialWindowSize)
  , do (pre, post) <- settingsAround
       v <- frequency [(2, elements [0x3FFF, 0x1000000]), (1, choose (0, 0x3FFF)), (1, choose (0x1000000, maxBound))]
       pure ("SETTINGS_MAX_FRAME_SIZE", settingsWire (pre ++ [(0x5, v)] ++ post), conn ProtocolError BadMaxFrameSize)
  , do sid <- streamId; reserved <- arbitrary
       pure ("WINDOW_UPDATE of 0 on a stream", encodeWire (Wire 0x8 0 False sid (be32 (if reserved then 0x80000000 else 0))),
             Refused (FrameError (Stream (StreamId sid)) ProtocolError ZeroIncrement))
  , do reserved <- arbitrary
       pure ("WINDOW_UPDATE of 0 on the connection", encodeWire (Wire 0x8 0 False 0 (be32 (if reserved then 0x80000000 else 0))),
             conn ProtocolError ZeroIncrement)
  , do -- the padding reaches the Pad Length, or into the priority fields
      (t, fl, fixed) <- elements [(0x0, 0x8, 0), (0x1, 0x8, 0), (0x1, 0x28, 5)]
      sid <- streamId; body <- choose (0, 100) >>= bytes
      -- the padding has only the body's octets to fill
      pad <- frequency [(2, pure (B.length body + 1)), (1, choose (B.length body + 1, 255))]
      let payload = B.singleton (fromIntegral pad) <> B.replicate fixed 0 <> body
      pure ("padding too long", encodeWire (Wire t fl False sid payload), conn ProtocolError PaddingTooLong)
  , do (t, fl, fixed) <- elements [(0x0, 0x8, 0), (0x1, 0x8, 0), (0x1, 0x28, 5)]
       sid <- streamId; body <- someBytes; n <- choose (1, 20); i <- choose (0, n - 1); x <- choose (1, 255)
       let padding = B.take i (B.replicate n 0) <> B.singleton x <> B.replicate (n - i - 1) 0
           payload = B.singleton (fromIntegral n) <> B.replicate fixed 0 <> body <> padding
       -- F4: the padding's contents are not checked
       let expected = if t == 0x0 then Parsed (DataFrame (StreamId sid) False body (B.length payload)) (9 + B.length payload)
                      else Parsed (HeadersFrame (StreamId sid) False False body) (9 + B.length payload)
       pure ("padding not zero is accepted (F4)", encodeWire (Wire t fl False sid payload), expected)
  , do -- PADDED without room for the Pad Length (and the priority fields)
      (t, fl, need) <- elements [(0x0, 0x8, 1), (0x1, 0x8, 1), (0x1, 0x28, 6), (0x1, 0x20, 5)]
      sid <- streamId; n <- choose (0, need - 1)
      pure ("too short for its fields", encodeWire (Wire t fl False sid (B.replicate n 0)), conn FrameSizeError WrongLength)
  ]
  where
    lengthOtherThan n = frequency [(2, elements [n - 1, n + 1]), (1, suchThat (choose (0, 20)) (/= n))]
    settingsAround = (,) <$> valid <*> valid
      where valid = map (\(i, v, _) -> (i, v)) <$> listOf genSetting
    settingsWire ss = encodeWire (Wire 0x4 0 False 0 (B.concat [be16 i <> be32 v | (i, v) <- ss]))

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
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.1
-- # The total number of padding octets is determined by the value of the
-- # Pad Length field.  If the length of the padding is the length of the
-- # frame payload or greater, the recipient MUST treat this as a
-- # connection error (Section 5.4.1) of type PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.1
-- # Padding octets MUST be set to zero when sending.  A receiver is
-- #    not obligated to verify padding but MAY treat non-zero padding as
-- #    a connection error (Section 5.4.1) of type PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.1
-- # DATA frames MUST be associated with a stream.  If a DATA frame is
-- # received whose Stream Identifier field is 0x00, the recipient MUST
-- # respond with a connection error (Section 5.4.1) of type
-- # PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.2
-- # HEADERS frames MUST be associated with a stream.  If a HEADERS frame
-- # is received whose Stream Identifier field is 0x00, the recipient MUST
-- # respond with a connection error (Section 5.4.1) of type
-- # PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.3
-- # The PRIORITY frame always identifies a stream.  If a PRIORITY frame
-- # is received with a stream identifier of 0x00, the recipient MUST
-- # respond with a connection error (Section 5.4.1) of type
-- # PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.3
-- # A PRIORITY frame with a length other than 5 octets MUST be treated as
-- # a stream error (Section 5.4.2) of type FRAME_SIZE_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.4
-- # RST_STREAM frames MUST be associated with a stream.  If a RST_STREAM
-- # frame is received with a stream identifier of 0x00, the recipient
-- # MUST treat this as a connection error (Section 5.4.1) of type
-- # PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.4
-- # A RST_STREAM frame with a length other than 4 octets MUST be treated
-- # as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5
-- # The stream identifier for a SETTINGS frame MUST be zero (0x00).  If
-- # an endpoint receives a SETTINGS frame whose Stream Identifier field
-- # is anything other than 0x00, the endpoint MUST respond with a
-- # connection error (Section 5.4.1) of type PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5
-- # Receipt of a SETTINGS frame with the ACK flag set and a
-- #    length field value other than 0 MUST be treated as a connection
-- #    error (Section 5.4.1) of type FRAME_SIZE_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5
-- # A SETTINGS frame with a length other than a multiple of 6 octets MUST
-- # be treated as a connection error (Section 5.4.1) of type
-- # FRAME_SIZE_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.2
-- # Any value other than 0 or 1 MUST be
-- #    treated as a connection error (Section 5.4.1) of type
-- #    PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.2
-- # Values above the maximum flow-control window size of 2^31-1 MUST
-- #    be treated as a connection error (Section 5.4.1) of type
-- #    FLOW_CONTROL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.5.2
-- # The value advertised
-- #    by an endpoint MUST be between this initial value and the maximum
-- #    allowed frame size (2^24-1 or 16,777,215 octets), inclusive.
-- #    Values outside this range MUST be treated as a connection error
-- #    (Section 5.4.1) of type PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.7
-- # PING frames are not associated with any individual stream.  If a PING
-- # frame is received with a Stream Identifier field value other than
-- # 0x00, the recipient MUST respond with a connection error
-- # (Section 5.4.1) of type PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.7
-- # Receipt of a PING frame with a length field value other than 8 MUST
-- # be treated as a connection error (Section 5.4.1) of type
-- # FRAME_SIZE_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.8
-- # An endpoint MUST treat a GOAWAY frame with a stream identifier other
-- # than 0x00 as a connection error (Section 5.4.1) of type
-- # PROTOCOL_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9
-- # A WINDOW_UPDATE frame with a length other than 4 octets MUST be
-- # treated as a connection error (Section 5.4.1) of type
-- # FRAME_SIZE_ERROR.
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9
-- # A receiver MUST treat the receipt of a WINDOW_UPDATE frame with a
-- # flow-control window increment of 0 as a stream error (Section 5.4.2)
-- # of type PROTOCOL_ERROR; errors on the connection flow-control window
-- # MUST be treated as a connection error (Section 5.4.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.10
-- # CONTINUATION frames MUST be associated with a stream.  If a
-- # CONTINUATION frame is received with a Stream Identifier field of
-- # 0x00, the recipient MUST respond with a connection error
-- # (Section 5.4.1) of type PROTOCOL_ERROR.
-- the exact boundaries, which generated cases reach only by chance
prop_boundaries :: Property
prop_boundaries = once $ conjoin
  [ counterexample "a length of 2^16 + 1, from the header alone" $
      parseFrame ourMax (B.pack [1, 0, 1, 0, 0, 0, 0, 0, 1]) === conn FrameSizeError TooLong
  , counterexample "a length of 2^24 - 1" $
      parseFrame ourMax (B.pack [0xFF, 0xFF, 0xFF, 0, 0, 0, 0, 0, 1]) === conn FrameSizeError TooLong
  , counterexample "the largest allowed, 2^14, is waited for" $
      parseFrame ourMax (B.pack [0, 0x40, 0, 0, 0, 0, 0, 0, 1]) === Incomplete (9 + 16384)
  , counterexample "SETTINGS_MAX_FRAME_SIZE of 2^14 - 1" $
      parseFrame ourMax (settingsOne 5 0x3FFF) === conn ProtocolError BadMaxFrameSize
  , counterexample "SETTINGS_MAX_FRAME_SIZE of 2^14" $
      parseFrame ourMax (settingsOne 5 0x4000) === Parsed (SettingsFrame 1 [MaxFrameSize 0x4000]) 15
  , counterexample "SETTINGS_MAX_FRAME_SIZE of 2^24 - 1 and 2^24" $
      parseFrame ourMax (settingsOne 5 0xFFFFFF) === Parsed (SettingsFrame 1 [MaxFrameSize 0xFFFFFF]) 15
      .&&. parseFrame ourMax (settingsOne 5 0x1000000) === conn ProtocolError BadMaxFrameSize
  , counterexample "SETTINGS_ENABLE_PUSH of 1 and 2" $
      parseFrame ourMax (settingsOne 2 1) === Parsed (SettingsFrame 1 [EnablePush True]) 15
      .&&. parseFrame ourMax (settingsOne 2 2) === conn ProtocolError BadEnablePush
  , counterexample "PADDED with one octet too few for the Pad Length and the priority fields" $ conjoin
      [ counterexample (show (t, fl)) $
          parseFrame ourMax (encodeWire (Wire t fl False 1 (B.replicate (need - 1) 0))) === conn FrameSizeError WrongLength
      | (t, fl, need) <- [(0x0, 0x8, 1), (0x1, 0x8, 1), (0x1, 0x28, 6), (0x1, 0x20, 5 :: Int)] ]
  , counterexample "SETTINGS_NO_RFC7540_PRIORITIES of 1 and 2 (RFC 9218 Section 2.1)" $
      parseFrame ourMax (settingsOne 9 1) === Parsed (SettingsFrame 1 [NoRFC7540Priorities True]) 15
      .&&. parseFrame ourMax (settingsOne 9 2) === conn ProtocolError BadNoRFC7540Priorities
  , counterexample "SETTINGS_INITIAL_WINDOW_SIZE of 2^31 - 1 and 2^31" $
      parseFrame ourMax (settingsOne 4 0x7FFFFFFF) === Parsed (SettingsFrame 1 [InitialWindowSize 0x7FFFFFFF]) 15
      .&&. parseFrame ourMax (settingsOne 4 0x80000000) === conn FlowControlError BadInitialWindowSize
  , counterexample "each fixed length, one octet either side" $ conjoin
      [ counterexample (show (t, n)) $ parseFrame ourMax (encodeWire (Wire t 0 False sid (B.replicate n 1))) === expected
      | (t, sid, good) <- [(0x3, 1, 4), (0x6, 0, 8), (0x8, 1, 4), (0x2, 1, 5)]
      , n <- [good - 1, good + 1]
      , let expected = if t == 0x2 then Refused (FrameError (Stream (StreamId 1)) FrameSizeError WrongLength)
                       else conn FrameSizeError WrongLength ]
  , counterexample "GOAWAY of 7 and 8 octets" $
      parseFrame ourMax (encodeWire (Wire 0x7 0 False 0 (B.replicate 7 0))) === conn FrameSizeError WrongLength
      .&&. parseFrame ourMax (encodeWire (Wire 0x7 0 False 0 (B.replicate 8 0)))
             === Parsed (GoAwayFrame (StreamId 0) NoError "") 17
  ]
  where settingsOne i v = encodeWire (Wire 0x4 0 False 0 (be16 i <> be32 v))

-- our SETTINGS_MAX_FRAME_SIZE need not be the default: a frame exactly
-- at it waits for its payload, one octet over is refused
prop_otherMaxima :: Property
prop_otherMaxima = forAll (choose (16384, 0xFFFFFF - 1)) $ \m -> forAll (choose (0, 0xFF)) $ \t ->
  let header len = B.pack [fromIntegral (len `shiftR` 16), fromIntegral (len `shiftR` 8), fromIntegral len, t, 0, 0, 0, 0, 1]
  in parseFrame m (header (m + 1)) === Refused (FrameError Connection FrameSizeError TooLong)
     .&&. parseFrame m (header m) === Incomplete (9 + m)

prop_nearMisses :: Property
prop_nearMisses = forAll genNearMiss $ \(rule, wire, expected) ->
  counterexample rule $ parseFrame ourMax wire === expected

------------------------------------------------------------------------------
-- rendering
------------------------------------------------------------------------------

genRendered :: Gen (ByteString, Frame)
genRendered = oneof
  [ do sid <- streamId; end <- arbitrary; d <- someBytes
       pure (B.concat (renderData (StreamId sid) end d), DataFrame (StreamId sid) end d (B.length d))
  , do sid <- streamId; c <- arbitrary
       pure (renderRstStream (StreamId sid) (ErrorCode c), RstStreamFrame (StreamId sid) (ErrorCode c))
  , do ss <- mapMaybe (\(_, _, s) -> s) <$> listOf genSetting
       pure (renderSettings ss, SettingsFrame (length ss) ss)
  , pure (renderSettingsAck, SettingsAckFrame)
  , do ack <- arbitrary; d <- bytes 8
       pure (renderPing ack d, PingFrame ack d)
  , do lastId <- choose (0, 0x7FFFFFFF); c <- arbitrary; debug <- someBytes
       pure (renderGoAway (StreamId lastId) (ErrorCode c) debug, GoAwayFrame (StreamId lastId) (ErrorCode c) debug)
  , do sid <- oneof [pure 0, streamId]; inc <- choose (1, 0x7FFFFFFF)
       pure (renderWindowUpdate (StreamId sid) inc, WindowUpdateFrame (StreamId sid) inc)
  ]

-- the flags each type defines (Section 6)
definedFlags :: Word8 -> Word8
definedFlags t = case t of
  0x0 -> 0x09
  0x1 -> 0x2D
  0x4 -> 0x01
  0x6 -> 0x01
  0x9 -> 0x04
  _   -> 0x00

prop_render :: Property
prop_render = forAll genRendered $ \(wire, f) ->
  parseFrame ourMax wire === Parsed f (B.length wire)
  .&&. counterexample "an unused flag set, or the reserved bit"
         (B.index wire 4 .&. complement8 (definedFlags (B.index wire 3)) == 0 && B.index wire 5 < 0x80)
  where complement8 x = 0xFF - x

prop_renderHeaders :: Property
prop_renderHeaders =
  forAll (choose (16384, 20000)) $ \peerMax ->
  forAll streamId $ \sid -> forAll arbitrary $ \end ->
  forAll (oneof [someBytes, choose (0, 70000) >>= bytes]) $ \block ->
    let frames = parseAll (B.concat (renderHeaders peerMax (StreamId sid) end block))
    in case frames of
         Just (HeadersFrame s e eh frag : rest) ->
           s === StreamId sid .&&. e === end
           .&&. B.concat (frag : [c | ContinuationFrame _ _ c <- rest]) === block
           .&&. length [() | ContinuationFrame s' _ _ <- rest, s' == StreamId sid] === length rest
           .&&. map endHeaders (HeadersFrame s e eh frag : rest) === (replicate (length rest) False ++ [True])
           .&&. counterexample "a frame longer than the peer allows"
                  (all (<= peerMax) (B.length frag : [B.length c | ContinuationFrame _ _ c <- rest]))
           .&&. counterexample "an empty CONTINUATION" (all (\c -> not (B.null c)) [c | ContinuationFrame _ _ c <- rest])
         other -> counterexample (show other) False
  where
    endHeaders (HeadersFrame _ _ eh _) = eh
    endHeaders (ContinuationFrame _ eh _) = eh
    endHeaders _ = False
    parseAll b
      | B.null b = Just []
      | otherwise = case parseFrame 0xFFFFFF b of
          Parsed f n -> (f :) <$> parseAll (B.drop n b)
          _ -> Nothing
