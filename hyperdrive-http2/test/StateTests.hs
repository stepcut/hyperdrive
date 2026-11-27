{-# LANGUAGE OverloadedStrings #-}
-- | An explicit model of RFC 9113 Section 5.1 for a server that never
-- pushes (C4, C13): for each state a client's stream can be in and each
-- frame type, what the core must do with the frame. Every pair is
-- checked, and generated sequences of frames and handler actions are
-- followed step by step.
module StateTests (stateTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Word (Word32, Word8)
import Hyperdrive.HPACK.Encode (Encoder, encodeBlock, newEncoder)
import Hyperdrive.HTTP2.Connection
import Hyperdrive.HTTP2.Frame
import Hyperdrive.Semantics.Field (emptyFields)
import Hyperdrive.Semantics.Status (mkStatus)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

stateTests :: TestTree
stateTests = testGroup "The connection core: stream states, as RFC 9113 Section 5.1 (C4, C13)"
  [ testProperty "every state and frame type: the model's answer" prop_table
  , testProperty "generated sequences of frames and handler actions follow the model, step by step" prop_sequences
  ]

------------------------------------------------------------------------------
-- the model
------------------------------------------------------------------------------

-- | The states of Section 5.1 a client's stream can be in, as the core
-- tells them apart (C4, C6).
data State
  = IdleAbove        -- ^ above the highest the client has opened
  | IdleEven         -- ^ even: the server's, and a server that never pushes opens none (Section 5.1.1)
  | Open
  | HalfClosedRemote
  | ClosedEnded      -- ^ its request ended and its response is complete; or skipped, never opened
  | ResetByUsHeld    -- ^ we reset it; its handler still runs
  | ResetByUsGone    -- ^ we reset it; its handler returned
  | ResetByClientHeld
  | ResetByClientGone
  deriving (Eq, Show, Enum, Bounded)

data Kind = Data | Headers | Priority | RstStream | WindowUpdate | Continuation | PushPromise
  deriving (Eq, Show, Enum, Bounded)

-- | What became of a frame.
data Outcome
  = Accepted [String]       -- ^ taken, with what the handler was told
  | StreamError ErrorCode   -- ^ RST_STREAM on the stream, the connection kept
  | ConnectionError ErrorCode
  deriving (Eq, Show)

-- | Section 5.1's table, with the core's decisions where the RFC leaves
-- a choice (each named). A HEADERS is a request on an idle stream and
-- trailers (with END_STREAM) on others.
expected :: State -> Kind -> Outcome
expected st k = case k of
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.10
  -- # A CONTINUATION frame MUST be preceded by a HEADERS, PUSH_PROMISE or
  -- # CONTINUATION frame without the END_HEADERS flag set.
  Continuation -> ConnectionError ProtocolError
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.4
  -- # A client cannot push.  Thus, servers MUST treat the receipt of a
  -- # PUSH_PROMISE frame as a connection error (Section 5.4.1) of type
  -- # PROTOCOL_ERROR.
  PushPromise -> ConnectionError ProtocolError
  -- PRIORITY is allowed in every state (Section 5.1), and ignored (D8)
  Priority -> Accepted []
  _ -> case st of
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
    -- # Receiving any frame other than HEADERS or PRIORITY on a stream in
    -- #    this state MUST be treated as a connection error (Section 5.4.1)
    -- #    of type PROTOCOL_ERROR.
    IdleAbove | k == Headers -> Accepted ["request"]
              | otherwise -> ConnectionError ProtocolError
    -- and a HEADERS on an even stream uses the server's identifiers
    -- (Section 5.1.1)
    IdleEven -> ConnectionError ProtocolError
    Open -> case k of
      Data -> Accepted ["content"]
      Headers -> Accepted ["end"]
      RstStream -> Accepted ["aborted"]
      _ -> Accepted []
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
    -- # If an endpoint receives additional frames, other than
    -- #    WINDOW_UPDATE, PRIORITY, or RST_STREAM, for a stream that is in
    -- #    this state, it MUST respond with a stream error (Section 5.4.2) of
    -- #    type STREAM_CLOSED.
    HalfClosedRemote -> case k of
      Data -> StreamError StreamClosed
      Headers -> StreamError StreamClosed
      RstStream -> Accepted ["aborted"]
      _ -> Accepted []
    -- closed: DATA, WINDOW_UPDATE and RST_STREAM minimally processed and
    -- discarded (C4). HEADERS is discarded only where trailers may cross
    -- our reset; otherwise a connection error (C4: Section 5.1.1, and 5.1's
    -- MAY for a stream the client reset whose handler still runs)
    ClosedEnded | k == Headers -> ConnectionError ProtocolError
    ResetByClientHeld | k == Headers -> ConnectionError StreamClosed
    ResetByClientGone | k == Headers -> ConnectionError ProtocolError
    _ -> Accepted []

------------------------------------------------------------------------------
-- the client and the core
------------------------------------------------------------------------------

get', post' :: [(ByteString, ByteString)]
get' = [(":method", "GET"), (":scheme", "http"), (":authority", "example.com"), (":path", "/")]
post' = [(":method", "POST"), (":scheme", "http"), (":authority", "example.com"), (":path", "/up")]

frameOf :: Word8 -> Word8 -> Word32 -> ByteString -> ByteString
frameOf t fl sid payload =
  let len = B.length payload
  in B.pack [ fromIntegral (len `div` 65536), fromIntegral (len `div` 256), fromIntegral len, t, fl
            , fromIntegral (sid `div` 16777216), fromIntegral (sid `div` 65536), fromIntegral (sid `div` 256), fromIntegral sid ]
     <> payload

headersFrame :: Encoder -> Word32 -> Bool -> [(ByteString, ByteString)] -> (ByteString, Encoder)
headersFrame e sid end fields =
  let (b, e') = encodeBlock e fields in (B.concat (renderHeaders 16384 (StreamId sid) end b), e')

-- | The frame of a kind for a stream (a HEADERS: a request if the stream
-- is idle, else trailers), and the client's encoder after it.
frameFor :: Kind -> Bool -> Encoder -> Word32 -> (ByteString, Encoder)
frameFor k idle e sid = case k of
  Data -> (B.concat (renderData (StreamId sid) False "x"), e)
  Headers -> headersFrame e sid True (if idle then get' else [("x-trailer", "1")])
  Priority -> (frameOf 0x2 0 sid (B.pack [0, 0, 0, 0, 16]), e)
  RstStream -> (renderRstStream (StreamId sid) Cancel, e)
  WindowUpdate -> (renderWindowUpdate (StreamId sid) 1, e)
  Continuation -> (frameOf 0x9 0x4 sid "", e)
  PushPromise -> (frameOf 0x5 0x4 sid (B.pack [0, 0, 0, 2]), e)

-- | What the core's outputs say became of a frame on the stream.
judge :: Word32 -> [Output] -> Outcome
judge sid outs =
  let frames = parseAll (B.concat (concat [bs | Send bs <- outs]))
      told = [ "request" | NewRequest _ _ <- outs ] ++ [ "content" | RequestData _ _ <- outs ]
             ++ [ "end" | RequestEnd _ _ <- outs ] ++ [ "aborted" | RequestAborted _ _ <- outs ]
  in case [code | GoAwayFrame _ code _ <- frames] of
       (code : _) -> ConnectionError code
       [] -> case [code | RstStreamFrame (StreamId s) code <- frames, s == sid] of
         (code : _) -> StreamError code
         [] -> Accepted told
  where
    parseAll b
      | B.null b = []
      | otherwise = case parseFrame 0xFFFFFF b of
          Parsed f n -> f : parseAll (B.drop n b)
          other -> error (show other)

-- | A connection past its prefaces and SETTINGS.
start :: Conn
start =
  let (c0, _) = newConn defaultConfig 0
  in fst (step 0 (Received (connectionPreface <> renderSettings [] <> renderSettingsAck)) c0)

------------------------------------------------------------------------------
-- the client's frames and the handlers' actions
------------------------------------------------------------------------------

-- | What the client or a handler does next.
data Act
  = Frame Kind Word32       -- ^ a frame of the kind on the stream
  | OpenPost Word32         -- ^ HEADERS without END_STREAM: a request with content to come
  | DataEnd Word32          -- ^ DATA with END_STREAM
  | Finish Word32           -- ^ its handler responds, and ends the response
  | Done Word32             -- ^ its handler returns
  deriving (Show)

-- | The model's view: the highest stream the client opened, and the
-- state of each it opened.
data Model = Model Word32 (M.Map Word32 State)

stateOf :: Model -> Word32 -> State
stateOf (Model highest m) sid
  | even sid = IdleEven
  | sid > highest = IdleAbove
  | otherwise = M.findWithDefault ClosedEnded sid m

-- | The model's answer to an act (none for a handler's), and its view
-- afterwards.
modelStep :: Model -> Act -> (Maybe Outcome, Model)
modelStep md@(Model highest m) act = case act of
  Frame k sid -> let st = stateOf md sid; o = expected st k in (Just o, after sid st k o)
  OpenPost sid -> case stateOf md sid of
    IdleAbove -> (Just (Accepted ["request"]), Model sid (M.insert sid Open m))
    -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1
    -- # An endpoint that receives a HEADERS frame without the END_STREAM flag
    -- # set after receiving the HEADERS frame that opens a request or after
    -- # receiving a final (non-informational) status code MUST treat the
    -- # corresponding request or response as malformed (Section 8.1.1).
    Open -> (Just (StreamError ProtocolError), set sid ResetByUsHeld)
    st -> let o = expected st Headers in (Just o, after sid st Headers o)
  DataEnd sid -> case stateOf md sid of
    Open -> (Just (Accepted ["content", "end"]), set sid HalfClosedRemote)
    st -> let o = expected st Data in (Just o, after sid st Data o)
  -- the request still sending when the response completes: our
  -- RST_STREAM NO_ERROR (D6), and the stream remembered as reset
  Finish sid -> case stateOf md sid of
    Open -> (Nothing, set sid ResetByUsGone)
    HalfClosedRemote -> (Nothing, set sid ClosedEnded)
    _ -> (Nothing, md)
  -- returned without responding: the core's 500, then NO_ERROR if the
  -- request still sends
  Done sid -> case stateOf md sid of
    Open -> (Nothing, set sid ResetByUsGone)
    HalfClosedRemote -> (Nothing, set sid ClosedEnded)
    ResetByUsHeld -> (Nothing, set sid ResetByUsGone)
    ResetByClientHeld -> (Nothing, set sid ResetByClientGone)
    _ -> (Nothing, md)
  where
    set sid st = Model highest (M.insert sid st m)
    after sid st k o = case (o, st, k) of
      (ConnectionError _, _, _) -> md
      (StreamError _, _, _) -> set sid ResetByUsHeld
      (_, IdleAbove, Headers) -> Model sid (M.insert sid HalfClosedRemote m)
      (_, Open, Headers) -> set sid HalfClosedRemote
      (_, Open, RstStream) -> set sid ResetByClientHeld
      (_, HalfClosedRemote, RstStream) -> set sid ResetByClientHeld
      _ -> md

-- | The core's answer to an act, its state and the client's encoder
-- afterwards.
coreStep :: Model -> Encoder -> Conn -> Act -> (Maybe Outcome, Conn, Encoder)
coreStep md e c act = case act of
  Frame k sid -> let (b, e') = frameFor k (stateOf md sid == IdleAbove) e sid in received b e' sid
  OpenPost sid -> let (b, e') = headersFrame e sid False post' in received b e' sid
  DataEnd sid -> received (B.concat (renderData (StreamId sid) True "x")) e sid
  Finish sid ->
    let respond = Respond (StreamId sid) (ResponseHead (fromJust (mkStatus 200)) emptyFields True (Just 0))
    in (Nothing, fst (step 0 (EndBody (StreamId sid) emptyFields) (fst (step 0 respond c))), e)
  Done sid -> (Nothing, fst (step 0 (HandlerDone (StreamId sid)) c), e)
  where
    received b e' sid = let (c', outs) = step 0 (Received b) c in (Just (judge sid outs), c', e')

-- | Acts that mostly keep the connection alive: chosen with the model in
-- hand, mostly on the streams already opened and the next new one, and
-- one the model says ends the connection only now and then, as the last.
genActs :: Gen [Act]
genActs = sized $ \n -> go (Model 0 M.empty) n
  where
    go _ 0 = pure []
    go md k = do
      a <- genAct md
      case modelStep md a of
        (Just (ConnectionError _), _) -> do
          last' <- frequency [(1, pure True), (30, pure False)]
          if last' then pure [a] else go md k
        (_, md') -> (a :) <$> go md' (k - 1)
    genAct (Model highest m) = do
      let next = if highest == 0 then 1 else highest + 2
      sid <- frequency $ [(6, elements (M.keys m)) | not (M.null m)]
                         ++ [(3, pure next), (1, elements [2, 4, next + 2])]
      frequency
        [ (6, Frame <$> elements [Data, Headers, Priority, RstStream, WindowUpdate] <*> pure sid)
        , (1, Frame <$> elements [Continuation, PushPromise] <*> pure sid)
        , (3, pure (OpenPost sid))
        , (2, pure (DataEnd sid))
        , (2, pure (Finish sid))
        , (2, pure (Done sid)) ]

-- | Each act's outcome, the model's against the core's, until the
-- connection ends.
prop_sequences :: Property
prop_sequences = forAll genActs $ \acts -> go (Model 0 M.empty) (newEncoder 4096) start acts []
  where
    -- how many acts were followed, and how the sequence ended
    followed done how = tabulate "acts followed" [bucket (length done)] . tabulate "ended" [how]
    bucket n | n < 5 = "0-4" | n < 20 = "5-19" | n < 50 = "20-49" | otherwise = "50+"
    go _ _ _ [] done = followed done "acts used up" (property True)
    go md e c (a : rest) done =
      let (want, md') = modelStep md a
          (got, c', e') = coreStep md e c a
      in counterexample (unlines (map show (reverse (a : done)))) $
           case (want, got) of
             (Just w, Just g) | w /= g -> counterexample ("expected " ++ show w ++ ", the core: " ++ show g) False
             (Just (ConnectionError _), _) -> followed (a : done) "a connection error" (property True)
             _ -> go md' e' c' rest (a : done)

------------------------------------------------------------------------------
-- every state, every frame type
------------------------------------------------------------------------------

-- | The acts that bring a stream into a state, and the stream.
setupActs :: State -> ([Act], Word32)
setupActs st = case st of
  IdleAbove -> ([Frame Headers 1], 3)
  IdleEven -> ([Frame Headers 3], 2)
  Open -> ([OpenPost 1], 1)
  HalfClosedRemote -> ([Frame Headers 1], 1)
  ClosedEnded -> ([Frame Headers 1, Finish 1, Done 1], 1)
  -- a second HEADERS without END_STREAM is malformed (Section 8.1): we
  -- reset the stream
  ResetByUsHeld -> ([OpenPost 1, OpenPost 1], 1)
  ResetByUsGone -> ([OpenPost 1, OpenPost 1, Done 1], 1)
  ResetByClientHeld -> ([OpenPost 1, Frame RstStream 1], 1)
  ResetByClientGone -> ([OpenPost 1, Frame RstStream 1, Done 1], 1)

prop_table :: Property
prop_table = once $ conjoin
  [ counterexample (show st ++ ", " ++ show k) $
      let (acts, sid) = setupActs st
          (md, e, c) = foldl (\(m0, e0, c0) a -> let (_, m1) = modelStep m0 a; (_, c1, e1) = coreStep m0 e0 c0 a in (m1, e1, c1))
                             (Model 0 M.empty, newEncoder 4096, start) acts
          (got, _, _) = coreStep md e c (Frame k sid)
      in stateOf md sid === st .&&. got === Just (expected st k)
  | st <- [minBound .. maxBound], k <- [minBound .. maxBound] ]
