{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}
module StreamTests (streamTests, responseTests, endingTests, priorityTests) where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (minusPtr)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Word (Word32, Word64)
import Hyperdrive.HPACK.Decode
import Hyperdrive.HPACK.Encode
import Hyperdrive.HTTP2.Connection
import Hyperdrive.HTTP2.Frame
import Hyperdrive.Semantics.ControlData (RequestControl(..))
import Hyperdrive.Semantics.Field (Fields, emptyFields, fieldNameBytes, fieldValueBytes, fieldsFromList, fieldsToList, mkFieldName, mkFieldValue)
import Hyperdrive.Semantics.Method (methodBytes)
import Hyperdrive.Semantics.Status (mkStatus)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

streamTests :: TestTree
streamTests = testGroup "The connection core: streams and field blocks (C4, C6, C7, D10, D11)"
  [ testProperty "requests, by example" prop_requests
  , testProperty "field blocks: CONTINUATION, contiguity, limits" prop_blocks
  , testProperty "refusals: malformed, refused, too large" prop_refusals
  , testProperty "streams: limits, resets, trailers, closed streams" prop_streams
  , testProperty "a request split into CONTINUATIONs and octets gives the same result" prop_split
  , testProperty "content: DATA, its end, its declared length (C3)" prop_content
  , testProperty "credit: windows, Consumed, WINDOW_UPDATE at half, credit given back (C3, D6)" prop_credit
  , testProperty "100 (Continue) on the first read with nothing queued (C9)" prop_continue
  , testProperty "small slices of a large buffer are copied (C3)" prop_copy
  ]

------------------------------------------------------------------------------
-- a client
------------------------------------------------------------------------------

clientStart :: ByteString
clientStart = connectionPreface <> renderSettings []

base :: [(ByteString, ByteString)]
base = [(":method", "GET"), (":scheme", "http"), (":authority", "example.com"), (":path", "/")]

post :: [(ByteString, ByteString)]
post = [(":method", "POST"), (":scheme", "http"), (":authority", "example.com"), (":path", "/up")]

-- the field blocks of several requests, encoded in order by one encoder
blocks :: [[(ByteString, ByteString)]] -> [ByteString]
blocks = go (newEncoder 4096)
  where
    go _ [] = []
    go e (fs : rest) = let (b, e') = encodeBlock e fs in b : go e' rest

-- a HEADERS frame carrying a whole block
headers :: Word32 -> Bool -> ByteString -> ByteString
headers sid end b = B.concat (renderHeaders 16384 (StreamId sid) end b)

-- a frame of any type and flags, by hand
rawFrame :: Int -> Int -> Word32 -> ByteString -> ByteString
rawFrame t fl sid payload =
  let len = B.length payload
  in B.pack [ fromIntegral (len `div` 65536), fromIntegral (len `div` 256), fromIntegral len
            , fromIntegral t, fromIntegral fl
            , fromIntegral (sid `div` 16777216), fromIntegral (sid `div` 65536)
            , fromIntegral (sid `div` 256), fromIntegral sid ] <> payload

-- HEADERS without END_HEADERS, then CONTINUATIONs, the block cut at the
-- given lengths
split :: Word32 -> Bool -> [Int] -> ByteString -> ByteString
split sid end cuts b =
  let pieces = cut cuts b
      flags i = (if i == 0 && end then 1 else 0) + (if i == length pieces - 1 then 4 else 0)
  in B.concat [ rawFrame (if i == 0 then 1 else 9) (flags i) sid p | (i, p) <- zip [0 :: Int ..] pieces ]
  where
    cut [] r = [r]
    cut (n : ns) r = B.take n r : cut ns (B.drop n r)

rst :: Word32 -> ErrorCode -> ByteString
rst sid code = renderRstStream (StreamId sid) code

run :: [(Time, Input)] -> (Conn, [Output])
run = runWith defaultConfig

runWith :: Config -> [(Time, Input)] -> (Conn, [Output])
runWith config = foldl go (newConn config 0)
  where go (c, outs) (t, i) = let (c', outs') = step t i c in (c', outs ++ outs')

-- the smallest windows (65,535: WINDOW_UPDATE once 32,768 are owed)
small :: Config
small = defaultConfig { configStreamWindow = 65535, configConnectionWindow = 65535 }

-- content as DATA frames of at most 16 KiB, END_STREAM on the last
dataFrames :: Word32 -> Bool -> ByteString -> ByteString
dataFrames sid end b
  | B.length b <= 16384 = B.concat (renderData (StreamId sid) end b)
  | otherwise = B.concat (renderData (StreamId sid) False (B.take 16384 b)) <> dataFrames sid end (B.drop 16384 b)

-- the WINDOW_UPDATEs sent: after the client's SETTINGS were acknowledged
-- (so not the preface's), or in a step's outputs
updatesAfterAck :: [Output] -> [(Word32, Word32)]
updatesAfterAck outs = [ (s, i) | WindowUpdateFrame (StreamId s) i <- drop 1 (dropWhile (/= SettingsAckFrame) (parsed outs)) ]

updates :: [Output] -> [(Word32, Word32)]
updates outs = [ (s, i) | WindowUpdateFrame (StreamId s) i <- parsed outs ]

content :: [Output] -> [(Word32, ByteString)]
content outs = [ (s, d) | RequestData (StreamId s) d <- outs ]

ends :: [Output] -> [Word32]
ends outs = [ s | RequestEnd (StreamId s) _ <- outs ]

misused :: [Output] -> Bool
misused outs = not (null [() | ProtocolMisuse _ <- outs])

received :: ByteString -> [(Time, Input)]
received b = [(0, Received (clientStart <> b))]

-- what the server sent, without its preface's SETTINGS and WINDOW_UPDATE
-- and its SETTINGS acknowledgements: frames, with response field blocks
-- decoded
data Seen
  = SeenHeaders Word32 [(ByteString, ByteString)] Bool
  | SeenReset Word32 ErrorCode
  | SeenGoAway ErrorCode
  | SeenOther Frame
  deriving (Eq, Show)

seen :: [Output] -> [Seen]
seen outs = go (newDecoder 4096 4096) (parseAll (B.concat (concat [bs | Send bs <- outs])))
  where
    go _ [] = []
    go d (SettingsFrame _ _ : rest) = go d rest
    go d (SettingsAckFrame : rest) = go d rest
    go d (WindowUpdateFrame (StreamId 0) _ : rest) = go d rest
    go d (HeadersFrame (StreamId s) end _ b : rest) = case decodeBlock maxBound d b of
      Right (Fields fs, d') -> SeenHeaders s fs end : go d' rest
      other -> error (show other)
    go d (RstStreamFrame (StreamId s) code : rest) = SeenReset s code : go d rest
    go d (GoAwayFrame _ code _ : rest) = SeenGoAway code : go d rest
    go d (f : rest) = SeenOther f : go d rest
    parseAll b
      | B.null b = []
      | otherwise = case parseFrame 0xFFFFFF b of
          Parsed f n -> f : parseAll (B.drop n b)
          other -> error ("the server sent a bad frame: " ++ show other)

requests :: [Output] -> [(Word32, ByteString, ByteString, Bool)]
requests outs = [ (s, methodBytes (controlMethod (requestControl h)), path h, requestHasContent h)
                | NewRequest (StreamId s) h <- outs ]
  where path h = C.pack (show (controlTarget (requestControl h)))

parsed :: [Output] -> [Frame]
parsed outs = go (B.concat (concat [bs | Send bs <- outs]))
  where
    go b
      | B.null b = []
      | otherwise = case parseFrame 0xFFFFFF b of
          Parsed f n -> f : go (B.drop n b)
          other -> error (show other)

aborted :: [Output] -> [(Word32, AbortReason)]
aborted outs = [(s, r) | RequestAborted (StreamId s) r <- outs]

closed :: [Output] -> Bool
closed outs = not (null [() | Close <- outs])

status :: Word32 -> ByteString -> Seen
status s code = SeenHeaders s [(":status", code), ("content-length", "0")] True

------------------------------------------------------------------------------
-- the properties
------------------------------------------------------------------------------

prop_requests :: Property
prop_requests = once $ conjoin
  [ counterexample "a GET: a request, with no content" $
      let [b] = blocks [base]
          outs = snd (run (received (headers 1 True b)))
      in map (\(s, m, _, c) -> (s, m, c)) (requests outs) === [(1, "GET", False)] .&&. seen outs === []
  , counterexample "a POST without END_STREAM: content follows" $
      let [b] = blocks [post]
      in map (\(s, m, _, c) -> (s, m, c)) (requests (snd (run (received (headers 1 False b))))) === [(1, "POST", True)]
  , counterexample "two requests, the second using the table the first filled" $
      let [b1, b2] = blocks [base ++ [("x-custom", "1")], base ++ [("x-custom", "1")]]
          outs = snd (run (received (headers 1 True b1 <> headers 3 True b2)))
      in map (\(s, _, _, _) -> s) (requests outs) === [1, 3]
         .&&. [ [(fieldNameBytes n, fieldValueBytes v) | (n, v) <- fieldsToList (controlFields (requestControl h))]
              | NewRequest _ h <- outs ] === replicate 2 [("x-custom", "1")]
  , counterexample "a handler that returns without responding: 500" $
      let [b] = blocks [base]
          (c, _) = run (received (headers 1 True b))
      in seen (snd (step 0 (HandlerDone (StreamId 1)) c)) === [status 1 "500"]
  ]

prop_blocks :: Property
prop_blocks = once $ conjoin
  [ counterexample "a block in a HEADERS and two CONTINUATIONs" $
      let [b] = blocks [base]
      in map (\(s, m, _, _) -> (s, m)) (requests (snd (run (received (split 1 True [3, 5] b))))) === [(1, "GET")]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.2
  -- # A HEADERS frame without the END_HEADERS flag set MUST be followed
  -- #    by a CONTINUATION frame for the same stream.  A receiver MUST
  -- #    treat the receipt of any other type of frame or a frame on a
  -- #    different stream as a connection error (Section 5.4.1) of type
  -- #    PROTOCOL_ERROR.
  , counterexample "a PING between HEADERS and its CONTINUATION" $
      let [b] = blocks [base]
          (h, rest) = B.splitAt (9 + 3) (split 1 True [3] b)
      in seen (snd (run (received (h <> renderPing False "12345678" <> rest)))) === [SeenGoAway ProtocolError]
  , counterexample "a CONTINUATION on another stream" $
      let [b] = blocks [base]
      in seen (snd (run (received (rawFrame 1 0 1 (B.take 3 b) <> rawFrame 9 4 3 (B.drop 3 b)))))
           === [SeenGoAway ProtocolError]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.10
  -- # A CONTINUATION frame MUST be preceded by a HEADERS, PUSH_PROMISE or
  -- # CONTINUATION frame without the END_HEADERS flag set.  A recipient
  -- # that observes violation of this rule MUST respond with a connection
  -- # error (Section 5.4.1) of type PROTOCOL_ERROR.
  , counterexample "a CONTINUATION with no HEADERS before it" $
      seen (snd (run (received (rawFrame 9 4 1 "")))) === [SeenGoAway ProtocolError]
  , counterexample "an even stream identifier" $
      let [b] = blocks [base] in seen (snd (run (received (headers 2 True b)))) === [SeenGoAway ProtocolError]
  , counterexample "a block over 128 KiB, as it arrives" $
      let big = B.replicate 16000 0
          frames = rawFrame 1 0 1 big <> B.concat (replicate 8 (rawFrame 9 0 1 big))
      in seen (snd (run (received frames))) === [SeenGoAway EnhanceYourCalm]
  , counterexample "an HPACK error: index 0" $
      seen (snd (run (received (headers 1 True (B.pack [0x80]))))) === [SeenGoAway CompressionError]
  , counterexample "a block left unfinished for 30 s" $
      let [b] = blocks [base]
          (c, _) = run (received (renderSettingsAck <> rawFrame 1 0 1 (B.take 3 b)))
      in nextDeadline c === Just 30000 .&&. seen (snd (step 30000 TimedOut c)) === [SeenGoAway ProtocolError]
  , counterexample "empty CONTINUATIONs cost budget" $
      let [b] = blocks [base]
          empties n = rawFrame 1 0 1 b <> B.concat (replicate n (rawFrame 9 0 1 "")) <> rawFrame 9 4 1 ""
      in closed (snd (run (received (empties 998)))) === False
         .&&. seen (snd (run (received (empties 1000)))) === [SeenGoAway EnhanceYourCalm]
  ]

prop_refusals :: Property
prop_refusals = once $ conjoin
  [ counterexample "malformed (an upper-case name): RST_STREAM PROTOCOL_ERROR alone, no request (D10)" $
      let [b] = blocks [base ++ [("X-Upper", "1")]]
          outs = snd (run (received (headers 1 True b)))
      in requests outs === [] .&&. seen outs === [SeenReset 1 ProtocolError]
  , counterexample "refused (https on cleartext): 421, and nothing more when the request has ended" $
      let [b] = blocks [[(":method", "GET"), (":scheme", "https"), (":authority", "example.com"), (":path", "/")]]
      in seen (snd (run (received (headers 1 True b)))) === [status 1 "421"]
  , counterexample "refused while the request is still sending: 421, then NO_ERROR" $
      let [b] = blocks [[(":method", "POST"), (":scheme", "https"), (":authority", "example.com"), (":path", "/")]]
      in seen (snd (run (received (headers 1 False b)))) === [status 1 "421", SeenReset 1 NoError]
  , counterexample "a field list over 32 KiB: 431" $
      let [b] = blocks [base ++ [("x-big", B.replicate 33000 0x61)]]
      in seen (snd (run (received (split 1 True [16000, 16000] b)))) === [status 1 "431"]
  , counterexample "101 fields: 431; 100 are accepted" $
      let fields n = base ++ [(C.pack ("x-" ++ show i), "v") | i <- [1 .. n - 4 :: Int]]
          [b100, b101] = blocks [fields 100, fields 101]
      in seen (snd (run (received (headers 1 True b101)))) === [status 1 "431"]
         .&&. length (requests (snd (run (received (headers 1 True b100))))) === 1
  , counterexample "Expect: 100-continue is accepted; any other expectation gets 417 (C9)" $
      let [b1, b2] = blocks [post ++ [("expect", "100-continue")], post ++ [("expect", "x")]]
      in length (requests (snd (run (received (headers 1 False b1))))) === 1
         .&&. seen (snd (run (received (headers 1 False b2)))) === [status 1 "417", SeenReset 1 NoError]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1.1
  -- # A request or response is also malformed
  -- # if the value of a content-length header field does not equal the sum
  -- # of the DATA frame payload lengths that form the content, unless the
  -- # message is defined as having no content.
  , counterexample "content-length 5 with END_STREAM on the HEADERS: malformed" $
      let [b] = blocks [post ++ [("content-length", "5")]]
      in seen (snd (run (received (headers 1 True b)))) === [SeenReset 1 ProtocolError]
  , counterexample "content-length 0 with END_STREAM is fine" $
      let [b] = blocks [post ++ [("content-length", "0")]]
      in length (requests (snd (run (received (headers 1 True b))))) === 1
  , counterexample "each response the core makes itself costs one too" $
      let https = [(":method", "GET"), (":scheme", "https"), (":authority", "example.com"), (":path", "/")]
          refused n = B.concat [ headers (2 * i + 1) True b | (i, b) <- zip [0 ..] (blocks (replicate n https)) ]
      in closed (snd (run (received (refused 999)))) === False
         .&&. closed (snd (run (received (refused 1000)))) === True
  , counterexample "the client's HEADER_TABLE_SIZE reaches our encoder: our next block starts with an update to 0 (H4)" $
      let https = [(":method", "GET"), (":scheme", "https"), (":authority", "example.com"), (":path", "/")]
          [b] = blocks [https]
          outs = snd (run [(0, Received (connectionPreface <> renderSettings [HeaderTableSize 0] <> headers 1 True b))])
          firstOctets = [B.take 1 blk | HeadersFrame _ _ _ blk <- parsed outs]
      in firstOctets === [B.singleton 0x20]
  , counterexample "each refusal costs one: a thousand are allowed, not more" $
      let malformed n = B.concat [ headers (2 * i + 1) True b | (i, b) <- zip [0 ..] (blocks (replicate n (base ++ [("X", "1")]))) ]
      in closed (snd (run (received (malformed 999)))) === False
         .&&. closed (snd (run (received (malformed 1000)))) === True
  ]

prop_streams :: Property
prop_streams = once $ conjoin
  [ counterexample "100 open streams are served; the 101st is refused (REFUSED_STREAM)" $
      let bs = blocks (replicate 101 post)
          outs = snd (run (received (B.concat [headers (2 * i + 1) False b | (i, b) <- zip [0 ..] bs])))
      in length (requests outs) === 100 .&&. seen outs === [SeenReset 201 RefusedStream]
  , counterexample "each refused stream costs one" $
      let bs = blocks (replicate 1100 post)
          opened = B.concat [headers (2 * i + 1) False b | (i, b) <- zip [0 ..] bs]
          upTo n = B.take (B.length (B.concat [headers (2 * i + 1) False b | (i, b) <- zip [0 ..] (take n bs)])) opened
      in closed (snd (run (received (upTo 1099)))) === False
         .&&. closed (snd (run (received (upTo 1100)))) === True
  , counterexample "a client's reset: the handler is told; the stream keeps its slot until the handler returns" $
      let [b] = blocks [post]
          (c, outs) = run (received (headers 1 False b <> rst 1 Cancel))
          (c', outs') = step 0 (HandlerDone (StreamId 1)) c
      in aborted outs === [(1, ResetByClient Cancel)] .&&. seen outs' === [] .&&. not (closed outs')
         .&&. length (requests (snd (step 0 (Received (headers 3 True (head (blocks [base])))) c'))) === 1
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
  -- # Receiving any frame other than HEADERS or PRIORITY on a stream in
  -- #    this state MUST be treated as a connection error (Section 5.4.1)
  -- #    of type PROTOCOL_ERROR.
  , counterexample "each client reset before our response costs one" $
      let resets n = foldl (\(c, outs) i ->
                              let [b] = blocks [post]
                                  sid = 2 * i + 1
                                  (c1, o1) = step 0 (Received (headers sid False b <> rst sid Cancel)) c
                                  (c2, o2) = step 0 (HandlerDone (StreamId sid)) c1
                              in (c2, outs ++ o1 ++ o2))
                           (run (received ""))
                           [0 .. n - 1]
      in closed (snd (resets (999 :: Word32))) === False .&&. closed (snd (resets 1000)) === True
  , counterexample "RST_STREAM on an idle stream: a connection error" $
      seen (snd (run (received (rst 5 Cancel)))) === [SeenGoAway ProtocolError]
  , counterexample "RST_STREAM on a closed stream is ignored" $
      let [b] = blocks [base]
          (c, _) = run (received (headers 1 True b))
          (c', _) = step 0 (HandlerDone (StreamId 1)) c
      in seen (snd (step 0 (Received (rst 1 Cancel)) c')) === []
  , counterexample "100 reset streams whose handlers still run: the next new stream is refused (C6)" $
      let bs = blocks (replicate 101 post)
          resetAll = B.concat [headers (2 * i + 1) False b <> rst (2 * i + 1) Cancel | (i, b) <- zip [0 .. 99] bs]
          outs = snd (run (received (resetAll <> headers 201 False (last bs))))
          -- one handler returns: its slot is free again
          (c, _) = run (received resetAll)
          (c', _) = step 0 (HandlerDone (StreamId 1)) c
      in length (aborted outs) === 100 .&&. seen outs === [SeenReset 201 RefusedStream]
         .&&. length (requests (snd (step 0 (Received (headers 201 False (last bs))) c'))) === 1
  , counterexample "trailers end the request" $
      let [b, t] = blocks [post, [("x-sum", "abc")]]
          outs = snd (run (received (headers 1 False b <> headers 1 True t)))
      in [ (s, [(fieldNameBytes n, fieldValueBytes v) | (n, v) <- fieldsToList fs]) | RequestEnd (StreamId s) fs <- outs ]
           === [(1, [("x-sum", "abc")])]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1
  -- # An endpoint that receives a HEADERS frame without the END_STREAM flag
  -- # set after receiving the HEADERS frame that opens a request or after
  -- # receiving a final (non-informational) status code MUST treat the
  -- # corresponding request or response as malformed (Section 8.1.1).
  , counterexample "trailers without END_STREAM: malformed" $
      let [b, t] = blocks [post, [("x-sum", "abc")]]
          outs = snd (run (received (headers 1 False b <> headers 1 False t)))
      in aborted outs === [(1, ResetByServer ProtocolError)] .&&. seen outs === [SeenReset 1 ProtocolError]
  , counterexample "trailers with pseudo-header fields: malformed" $
      let [b, t] = blocks [post, [(":path", "/")]]
      in aborted (snd (run (received (headers 1 False b <> headers 1 True t)))) === [(1, ResetByServer ProtocolError)]
  , counterexample "trailers before the declared length has come: content not as declared" $
      let [b, t] = blocks [post ++ [("content-length", "5")], [("x-sum", "abc")]]
      in aborted (snd (run (received (headers 1 False b <> headers 1 True t)))) === [(1, ContentNotAsDeclared)]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
  -- # If an endpoint receives additional frames, other than
  -- #    WINDOW_UPDATE, PRIORITY, or RST_STREAM, for a stream that is in
  -- #    this state, it MUST respond with a stream error (Section 5.4.2) of
  -- #    type STREAM_CLOSED.
  , counterexample "HEADERS on a half-closed (remote) stream: STREAM_CLOSED" $
      let [b, t] = blocks [base, [("x-sum", "abc")]]
          outs = snd (run (received (headers 1 True b <> headers 1 True t)))
      in seen outs === [SeenReset 1 StreamClosed] .&&. aborted outs === [(1, ResetByServer StreamClosed)]
  , counterexample "trailers on a stream we reset are decoded and discarded; the table stays in step" $
      -- content beyond its declared length: we reset stream 1; trailers
      -- the client sent before it saw that come after
      let [b1, t, b3] = blocks [post ++ [("content-length", "1")], [("x-new", "1")], base ++ [("x-new", "1")]]
          (c, _) = run (received (headers 1 False b1 <> dataFrames 1 False "12"))
          (c', _) = step 0 (HandlerDone (StreamId 1)) c
          (_, outs) = step 0 (Received (headers 1 True t <> headers 3 True b3)) c'
          -- the same while the handler still runs
          (_, held) = step 0 (Received (headers 1 True t <> headers 3 True b3)) c
      in map (\(s, _, _, _) -> s) (requests outs) === [3] .&&. seen outs === [] .&&. not (closed outs)
         .&&. map (\(s, _, _, _) -> s) (requests held) === [3] .&&. not (closed held)
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1.1
  -- # An endpoint that
  -- # receives an unexpected stream identifier MUST respond with a
  -- # connection error (Section 5.4.1) of type PROTOCOL_ERROR.
  , counterexample "HEADERS on a lower identifier never opened: PROTOCOL_ERROR (h2spec 5.1.1/2)" $
      let [b5, b3] = blocks [base, base]
      in last (seen (snd (run (received (headers 5 True b5 <> headers 3 True b3))))) === SeenGoAway ProtocolError
  , counterexample "HEADERS on a stream whose request had ended: PROTOCOL_ERROR" $
      let [b1, b1'] = blocks [base, base]
          (c, _) = run (received (headers 1 True b1))
          (c', _) = step 0 (HandlerDone (StreamId 1)) c
      in seen (snd (step 0 (Received (headers 1 True b1')) c')) === [SeenGoAway ProtocolError]
  , counterexample "HEADERS after the client's own reset, its handler still running: STREAM_CLOSED (h2spec 5.1 closed/9)" $
      let [b1, t1] = blocks [post, [("x-sum", "1")]]
      in last (seen (snd (run (received (headers 1 False b1 <> rst 1 Cancel <> headers 1 True t1))))) === SeenGoAway StreamClosed
  , counterexample "only the latest resets are kept: with two kept, the third reset forgets the first" $
      let config = defaultConfig { configResetsKept = 2 }
          malformed = base ++ [("X-Upper", "1")]
          [m1, m3, m5, t1, t5] = blocks [malformed, malformed, malformed, base, base]
          (c, _) = runWith config (received (headers 1 True m1 <> headers 3 True m3 <> headers 5 True m5))
      in seen (snd (step 0 (Received (headers 5 True t5)) c)) === []
         .&&. seen (snd (step 0 (Received (headers 1 True t1)) c)) === [SeenGoAway ProtocolError]
  ]

-- splitting a request into CONTINUATIONs, and its octets into pieces,
-- changes nothing
prop_split :: Property
prop_split = forAll (choose (0, 4)) $ \n -> forAll (vectorOf n (choose (1, 6))) $ \cuts ->
  let [b] = blocks [base ++ [("x-a", "1"), ("x-b", "two")]]
      wire = clientStart <> split 1 True cuts b
      whole = snd (run [(0, Received (clientStart <> headers 1 True b))])
  in forAll (pieces wire) $ \ps ->
       requests (snd (run [(0, Received p) | p <- ps])) === requests whole
  where
    pieces s
      | B.null s = pure []
      | otherwise = do
          k <- choose (1, B.length s)
          (B.take k s :) <$> pieces (B.drop k s)

prop_content :: Property
prop_content = once $ conjoin
  [ counterexample "content in two DATA frames, then the end" $
      let [b] = blocks [post]
          outs = snd (run (received (headers 1 False b <> dataFrames 1 False "hello" <> dataFrames 1 True " world")))
      in content outs === [(1, "hello"), (1, " world")] .&&. ends outs === [1]
  , counterexample "content longer than its content-length: malformed" $
      let [b] = blocks [post ++ [("content-length", "5")]]
          outs = snd (run (received (headers 1 False b <> dataFrames 1 False "123456")))
      in aborted outs === [(1, ContentNotAsDeclared)] .&&. seen outs === [SeenReset 1 ProtocolError] .&&. content outs === []
  , counterexample "content shorter than its content-length at END_STREAM: malformed" $
      let [b] = blocks [post ++ [("content-length", "5")]]
      in aborted (snd (run (received (headers 1 False b <> dataFrames 1 True "123")))) === [(1, ContentNotAsDeclared)]
  , counterexample "content exactly its content-length" $
      let [b] = blocks [post ++ [("content-length", "5")]]
          outs = snd (run (received (headers 1 False b <> dataFrames 1 True "12345")))
      in content outs === [(1, "12345")] .&&. ends outs === [1]
  , counterexample "DATA after the client's END_STREAM on DATA: STREAM_CLOSED" $
      let [b] = blocks [post]
      in seen (snd (run (received (headers 1 False b <> dataFrames 1 True "x" <> dataFrames 1 False "y"))))
           === [SeenReset 1 StreamClosed]
  , counterexample "DATA on a half-closed (remote) stream: STREAM_CLOSED" $
      let [b] = blocks [base]
      in seen (snd (run (received (headers 1 True b <> dataFrames 1 True "x")))) === [SeenReset 1 StreamClosed]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1
  -- # Receiving any frame other than HEADERS or PRIORITY on a stream in
  -- #    this state MUST be treated as a connection error (Section 5.4.1)
  -- #    of type PROTOCOL_ERROR.
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-5.1.1
  -- # Streams
  -- # initiated by a client MUST use odd-numbered stream identifiers; those
  -- # initiated by the server MUST use even-numbered stream identifiers.
  , counterexample "an even stream below the highest opened is idle: DATA, WINDOW_UPDATE, RST_STREAM on it are connection errors" $
      let [b9] = blocks [base]
          opened = headers 9 True b9
      in conjoin [ counterexample (show f) (seen (snd (run (received (opened <> f)))) === [SeenGoAway ProtocolError])
                 | f <- [dataFrames 8 False "x", renderWindowUpdate (StreamId 8) 1, rst 8 Cancel] ]
  , counterexample "DATA on an idle stream: a connection error" $
      seen (snd (run (received (dataFrames 1 True "x")))) === [SeenGoAway ProtocolError]
  , counterexample "empty DATA without END_STREAM costs one" $
      let [b] = blocks [post]
          empties n = headers 1 False b <> B.concat (replicate n (dataFrames 1 False ""))
      in closed (snd (run (received (empties 999)))) === False .&&. closed (snd (run (received (empties 1000)))) === True
  ]

-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.1
-- # The sender MUST NOT send a flow-controlled frame with a length that
-- # exceeds the space available in either of the flow-control windows
-- # advertised by the receiver.
prop_credit :: Property
prop_credit = once $ conjoin
  [ counterexample "over the stream's window: FLOW_CONTROL_ERROR on the stream" $
      let [b] = blocks [post]
          -- a larger connection window, so that only the stream's is exceeded
          config = small { configConnectionWindow = 1048576 }
          outs = snd (runWith config (received (headers 1 False b <> dataFrames 1 False (B.replicate 65536 0x61))))
      in aborted outs === [(1, ResetByServer FlowControlError)] .&&. SeenReset 1 FlowControlError `elem` seen outs
  , counterexample "over the connection's window: FLOW_CONTROL_ERROR on the connection" $
      let [b1, b3] = blocks [post, post]
          outs = snd (runWith small (received (headers 1 False b1 <> headers 3 False b3
                                               <> dataFrames 1 False (B.replicate 40000 0x61)
                                               <> dataFrames 3 False (B.replicate 25536 0x61))))
      in last (seen outs) === SeenGoAway FlowControlError
  , counterexample "windows configured below 65,535 are 65,535: nothing announced, 65,535 accepted" $
      let [b] = blocks [post]
          config = defaultConfig { configStreamWindow = 1000, configConnectionWindow = 1000 }
          outs = snd (runWith config (received (headers 1 False b <> dataFrames 1 False (B.replicate 65535 0x61))))
      in take 1 (parsed outs) === [SettingsFrame 3 [MaxConcurrentStreams 100, MaxHeaderListSize 32768, NoRFC7540Priorities True]]
         .&&. seen outs === [] .&&. sum (map (B.length . snd) (content outs)) === 65535
  , counterexample "credit comes back as the handler reads: a WINDOW_UPDATE once half the window is owed" $
      let [b] = blocks [post]
          (c, outs) = runWith small (received (headers 1 False b <> dataFrames 1 False (B.replicate 40000 0x61)))
          (c1, o1) = step 0 (Consumed (StreamId 1) 30000) c
          (_, o2) = step 0 (Consumed (StreamId 1) 10000) c1
      in updatesAfterAck outs === [] .&&. updates o1 === [] .&&. updates o2 === [(1, 40000), (0, 40000)]
  , counterexample "Consumed for more than was passed on is a misuse" $
      let [b] = blocks [post]
          (c, _) = run (received (headers 1 False b <> dataFrames 1 False "abc"))
      in misused (snd (step 0 (Consumed (StreamId 1) 4) c)) .&&. not (misused (snd (step 0 (Consumed (StreamId 1) 3) c)))
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.1
  -- # The entire
  -- # DATA frame payload is included in flow control, including the Pad
  -- # Length and Padding fields if present.
  , counterexample "padding's credit comes back at once" $
      let [b] = blocks [post]
          padded = rawFrame 0 8 1 (B.singleton 255 <> "abc" <> B.replicate 255 0)
          frames = headers 1 False b <> B.concat (replicate 130 padded)
          outs = snd (runWith small (received frames))
      in length (content outs) === 130 .&&. map fst (updatesAfterAck outs) === [1, 0]
  , counterexample "a reset stream's unread content: its connection credit comes back" $
      let [b] = blocks [post]
          outs = snd (runWith small (received (headers 1 False b <> dataFrames 1 False (B.replicate 40000 0x61) <> rst 1 Cancel)))
      in updatesAfterAck outs === [(0, 40000)]
  , counterexample "DATA on a closed stream: discarded, its credit given back" $
      let [b] = blocks [base]
          (c, _) = runWith small (received (headers 1 True b))
          (c', _) = step 0 (HandlerDone (StreamId 1)) c
          outs = snd (step 0 (Received (dataFrames 1 False (B.replicate 40000 0x61))) c')
      -- three frames: the update goes once half the window (32,768) is owed
      in content outs === [] .&&. updates outs === [(0, 32768)]
  , counterexample "a handler that returns without reading: 500, and the content's credit comes back" $
      let [b] = blocks [post]
          (c, _) = runWith small (received (headers 1 False b <> dataFrames 1 False (B.replicate 40000 0x61)))
          outs = snd (step 0 (HandlerDone (StreamId 1)) c)
      in updates outs === [(0, 40000)] .&&. status 1 "500" `elem` seen outs
  ]

prop_continue :: Property
prop_continue = once $ conjoin
  [ counterexample "100 on the first read, not again" $
      let [b] = blocks [post ++ [("expect", "100-continue")]]
          (c, _) = run (received (headers 1 False b))
          (c1, o1) = step 0 (WantContent (StreamId 1)) c
          (_, o2) = step 0 (WantContent (StreamId 1)) c1
      in seen o1 === [SeenHeaders 1 [(":status", "100")] False] .&&. seen o2 === []
  , counterexample "no 100 once content has arrived" $
      let [b] = blocks [post ++ [("expect", "100-continue")]]
          (c, _) = run (received (headers 1 False b <> dataFrames 1 False "x"))
      in seen (snd (step 0 (WantContent (StreamId 1)) c)) === []
  , counterexample "no 100 without Expect" $
      let [b] = blocks [post]
          (c, _) = run (received (headers 1 False b))
      in seen (snd (step 0 (WantContent (StreamId 1)) c)) === []
  ]

prop_copy :: Property
prop_copy = once $
  let [b] = blocks [post]
      input = clientStart <> headers 1 False b <> dataFrames 1 False "ab" <> dataFrames 1 False (B.replicate 16000 0x61)
      outs = snd (run [(0, Received input)])
      -- whether the slice's octets lie inside the input's
      address bs = let (fp, _) = BI.toForeignPtr0 bs in unsafeForeignPtrToPtr fp
      shares d = let off = address d `minusPtr` address input in off >= 0 && off < B.length input
  in map (shares . snd) (content outs) === [False, True]

------------------------------------------------------------------------------
-- responses and the scheduler
------------------------------------------------------------------------------

responseTests :: TestTree
responseTests = testGroup "The connection core: responses and the scheduler (C5, C12, D8)"
  [ testProperty "responses, by example" prop_responses
  , testProperty "content not as declared, Abort, misuse" prop_responseErrors
  , testProperty "the client's windows: WINDOW_UPDATE, INITIAL_WINDOW_SIZE, tiny windows" prop_sendWindows
  , testProperty "round robin, buffers and Writable, pacing" prop_scheduler
  , testProperty "any writes and window updates: the content arrives whole, in order, within the windows" prop_sendModel
  ]

-- the transport writes everything it is given at once
writeAll :: (Conn, [Output]) -> (Conn, [Output])
writeAll (c, o) = case sum [B.length b | Send bs <- o, b <- bs] of
  0 -> (c, o)
  n -> let (c', o') = writeAll (step 0 (Sent n) c) in (c', o ++ o')

-- a connection whose client sent these SETTINGS and then these octets,
-- its output written at once
serve :: [Setting] -> ByteString -> (Conn, [Output])
serve ss b = feedAll (writeAll (newConn defaultConfig 0)) [Received (connectionPreface <> renderSettings ss <> b)]

-- inputs after a start whose output was written
feedAll :: (Conn, [Output]) -> [Input] -> (Conn, [Output])
feedAll = foldl' feed
  where feed (c, outs) i = let (c', o) = writeAll (step 0 i c) in (c', outs ++ o)

-- what a step after the start sends
from :: (Conn, [Output]) -> [Input] -> [Output]
from (c, _) is = snd (feedAll (c, []) is)

fieldsOf :: [(ByteString, ByteString)] -> Fields
fieldsOf fs = fieldsFromList [ (fromJust (mkFieldName n), either (error . show) id (mkFieldValue v)) | (n, v) <- fs ]

response :: Int -> [(ByteString, ByteString)] -> Maybe Word64 -> ResponseHead
response code fs len = ResponseHead (fromJust (mkStatus code)) (fieldsOf fs) True len

ok :: Maybe Word64 -> ResponseHead
ok = response 200 []

respondOn :: Word32 -> ResponseHead -> Input
respondOn s = Respond (StreamId s)

write :: Word32 -> ByteString -> Input
write s = WriteBody (StreamId s)

endOn :: Word32 -> Input
endOn s = EndBody (StreamId s) emptyFields

-- DATA frames sent: stream, payload, END_STREAM
datas :: [Output] -> [(Word32, ByteString, Bool)]
datas outs = [ (s, d, e) | DataFrame (StreamId s) e d _ <- parsed outs ]

writables :: [Output] -> [Word32]
writables outs = [ s | Writable (StreamId s) <- outs ]

windowUpdate :: Word32 -> Word32 -> Input
windowUpdate s n = Received (renderWindowUpdate (StreamId s) n)

-- two GETs, on streams 1 and 3
twoGets :: ByteString
twoGets = let [b1, b3] = blocks [base, base] in headers 1 True b1 <> headers 3 True b3

prop_responses :: Property
prop_responses = once $ conjoin
  [ counterexample "200 with content-length, two writes, the end: HEADERS, DATA, DATA, empty DATA with END_STREAM" $
      let [b] = blocks [base]
          outs = from (serve [] (headers 1 True b)) [respondOn 1 (ok (Just 5)), write 1 "hel", write 1 "lo", endOn 1]
      in [ x | x@SeenHeaders {} <- seen outs ] === [SeenHeaders 1 [(":status", "200"), ("content-length", "5")] False]
         .&&. datas outs === [(1, "hel", False), (1, "lo", False), (1, "", True)]
         .&&. writables outs === [1, 1] .&&. not (misused outs)
  , counterexample "content of unknown length: no content-length" $
      let [b] = blocks [base]
          outs = from (serve [] (headers 1 True b)) [respondOn 1 (response 200 [("x-a", "1")] Nothing), write 1 "abc", endOn 1]
      in seen outs === [ SeenHeaders 1 [(":status", "200"), ("x-a", "1")] False
                       , SeenOther (DataFrame (StreamId 1) False "abc" 3), SeenOther (DataFrame (StreamId 1) True "" 0) ]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1.1
  -- # A response that is defined to have no content, as described in
  -- # Section 6.4.1 of [HTTP], MAY have a non-zero content-length header
  -- # field, even though no content is included in DATA frames.
  , counterexample "HEAD: HEADERS with the declared content-length and END_STREAM; writes discarded, answered" $
      let [b] = blocks [[(":method", "HEAD"), (":scheme", "http"), (":authority", "example.com"), (":path", "/")]]
          outs = from (serve [] (headers 1 True b)) [respondOn 1 (ok (Just 5)), write 1 "hello", endOn 1]
      in seen outs === [SeenHeaders 1 [(":status", "200"), ("content-length", "5")] True] .&&. writables outs === [1]
         .&&. not (misused outs)
  , counterexample "no content (204): HEADERS with END_STREAM, no content-length" $
      let [b] = blocks [base]
          outs = from (serve [] (headers 1 True b)) [respondOn 1 ((response 204 [] Nothing) { respHasContent = False })]
      in seen outs === [SeenHeaders 1 [(":status", "204")] True]
  , counterexample "trailers: HEADERS with END_STREAM after the content" $
      let [b] = blocks [base]
          outs = from (serve [] (headers 1 True b))
                      [respondOn 1 (ok Nothing), write 1 "abc", EndBody (StreamId 1) (fieldsOf [("x-sum", "1")])]
      in drop 1 (seen outs) === [SeenOther (DataFrame (StreamId 1) False "abc" 3), SeenHeaders 1 [("x-sum", "1")] True]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1
  -- # A server can
  -- # send a complete response prior to the client sending an entire
  -- # request if the response does not depend on any portion of the
  -- # request that has not been sent and received.  When this is true, a
  -- # server MAY request that the client abort transmission of a request
  -- # without error by sending a RST_STREAM with an error code of NO_ERROR
  -- # after sending a complete response (i.e., a frame with the END_STREAM
  -- # flag set).
  , counterexample "a response complete while the request still sends: RST_STREAM NO_ERROR; its slot freed" $
      let [b] = blocks [post]
          (c, outs) = feedAll (serve [] (headers 1 False b)) [respondOn 1 ((response 204 [] Nothing) { respHasContent = False })]
      in drop (length (seen (snd (serve [] (headers 1 False b))))) (seen outs) === [SeenHeaders 1 [(":status", "204")] True, SeenReset 1 NoError]
         .&&. seen (from (c, []) [HandlerDone (StreamId 1), Received (dataFrames 1 False "late")]) === []
  , counterexample "the handler returns after the end: nothing more; the stream's slot is free" $
      let bs = blocks (replicate 101 base)
          (c, _) = serve [] (B.concat [headers (2 * i + 1) True b | (i, b) <- zip [0 .. 99] bs])
          (c', o) = feedAll (c, []) [respondOn 1 (ok (Just 0)), endOn 1, HandlerDone (StreamId 1)]
      in seen o === [SeenHeaders 1 [(":status", "200"), ("content-length", "0")] False, SeenOther (DataFrame (StreamId 1) True "" 0)]
         .&&. length (requests (from (c', []) [Received (headers 201 True (last bs))])) === 1
  , counterexample "the handler returns before the end, after responding: RST_STREAM INTERNAL_ERROR" $
      let [b] = blocks [base]
          outs = from (serve [] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 "abc", HandlerDone (StreamId 1)]
      in last (seen outs) === SeenReset 1 InternalError
  ]

prop_responseErrors :: Property
prop_responseErrors = once $ conjoin
  [ counterexample "more content than declared: RST_STREAM INTERNAL_ERROR, the handler told" $
      let [b] = blocks [base]
          outs = from (serve [] (headers 1 True b)) [respondOn 1 (ok (Just 5)), write 1 "hello!"]
      in last (seen outs) === SeenReset 1 InternalError .&&. aborted outs === [(1, ResetByServer InternalError)]
         .&&. datas outs === []
  , counterexample "less content than declared at the end: the same" $
      let [b] = blocks [base]
          outs = from (serve [] (headers 1 True b)) [respondOn 1 (ok (Just 5)), write 1 "hell", endOn 1]
      in last (seen outs) === SeenReset 1 InternalError .&&. aborted outs === [(1, ResetByServer InternalError)]
         .&&. [e | (_, _, e) <- datas outs] === [False]
  , counterexample "Abort: RST_STREAM INTERNAL_ERROR" $
      let [b] = blocks [base]
          outs = from (serve [] (headers 1 True b)) [respondOn 1 (ok Nothing), Abort (StreamId 1)]
      in last (seen outs) === SeenReset 1 InternalError
  , counterexample "Respond twice, WriteBody or EndBody before Respond: misuse" $
      let [b] = blocks [base]
          start = serve [] (headers 1 True b)
      in misused (from start [respondOn 1 (ok Nothing), respondOn 1 (ok Nothing)])
         .&&. misused (from start [write 1 "x"]) .&&. misused (from start [endOn 1])
  , counterexample "a write before the last one's Writable: misuse" $
      let [b] = blocks [base]
          outs = from (serve [InitialWindowSize 0] (headers 1 True b))
                      [respondOn 1 (ok Nothing), write 1 (B.replicate 16384 0x61), write 1 "x"]
      in writables outs === [] .&&. misused outs
  , counterexample "a response to a stream the client reset: discarded" $
      let [b] = blocks [post]
          outs = from (serve [] (headers 1 False b <> rst 1 Cancel)) [respondOn 1 (ok Nothing), write 1 "abc", endOn 1]
      in seen outs === [] .&&. writables outs === [1] .&&. not (misused outs)
  , counterexample "the client resets a stream with content buffered: the buffer is freed" $
      let bs = blocks (replicate 17 base)
          opened = B.concat [headers (2 * i + 1) True b | (i, b) <- zip [0 ..] bs]
          -- 16 streams fill the connection's 256 KiB with nothing sent
          fill = concat [[respondOn (2 * i + 1) (ok Nothing), write (2 * i + 1) (B.replicate 16384 0x61)] | i <- [0 .. 15]]
          (c, o) = feedAll (serve [InitialWindowSize 0] opened) fill
          o' = from (c, []) [respondOn 33 (ok Nothing), write 33 "x", Received (rst 1 Cancel)]
      in writables o === [] .&&. writables o' === [33]
  ]

prop_sendWindows :: Property
prop_sendWindows = once $ conjoin
  [ counterexample "content beyond the connection's window waits for its WINDOW_UPDATE" $
      let [b] = blocks [base]
          (c, o) = feedAll (serve [InitialWindowSize 200000] (headers 1 True b))
                           [respondOn 1 (ok Nothing), write 1 (B.replicate 100000 0x61), endOn 1]
          o' = from (c, []) [windowUpdate 0 34465]
      in sum [B.length d | (_, d, _) <- datas o] === 65535
         .&&. sum [B.length d | (_, d, _) <- datas o'] === 34465 .&&. [e | (_, _, e) <- datas o'] === [False, False, True]
  , counterexample "the stream's window: what is sent is within it; the rest follows its WINDOW_UPDATE" $
      let [b] = blocks [base]
          (c, o) = feedAll (serve [InitialWindowSize 5000] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 (B.replicate 8000 0x61)]
          o' = from (c, []) [windowUpdate 1 3000]
      in map (\(_, d, _) -> B.length d) (datas o) === [5000] .&&. map (\(_, d, _) -> B.length d) (datas o') === [3000]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-10.5
  -- # Providing tiny increments to flow control in WINDOW_UPDATE frames can
  -- #    cause a sender to generate a large number of DATA frames.
  , counterexample "a window however small: what it allows is sent (h2spec 6.9.1/1)" $
      let [b] = blocks [base]
          (c, o) = feedAll (serve [InitialWindowSize 1023] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 (B.replicate 1500 0x61)]
          o1 = from (c, []) [windowUpdate 1 1]
      in map (\(_, d, _) -> B.length d) (datas o) === [1023] .&&. map (\(_, d, _) -> B.length d) (datas o1) === [1]
         .&&. writables o === [1]
  , counterexample "an empty DATA with END_STREAM needs no window" $
      let [b] = blocks [base]
      in datas (from (serve [InitialWindowSize 0] (headers 1 True b)) [respondOn 1 (ok (Just 0)), endOn 1]) === [(1, "", True)]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.2
  -- # A change to SETTINGS_INITIAL_WINDOW_SIZE can cause the available
  -- # space in a flow-control window to become negative.  A sender MUST
  -- # track the negative flow-control window and MUST NOT send new flow-
  -- # controlled frames until it receives WINDOW_UPDATE frames that cause
  -- # the flow-control window to become positive.
  , counterexample "INITIAL_WINDOW_SIZE lowered below what was sent: negative, then credit restores it" $
      let [b] = blocks [base]
          (c, o) = feedAll (serve [] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 (B.replicate 30000 0x61)]
          -- the window, 35,535, becomes -30,000
          (c1, o1) = feedAll (c, []) [Received (renderSettings [InitialWindowSize 0]), write 1 "x", endOn 1]
          o2 = from (c1, []) [windowUpdate 1 30000]
          o3 = from (c1, []) [windowUpdate 1 30001]
      in map (\(_, d, _) -> B.length d) (datas o) === [16384, 13616] .&&. seen o1 === []
         .&&. datas o2 === [] .&&. datas o3 === [(1, "x", True)]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.2
  -- # When the
  -- # value of SETTINGS_INITIAL_WINDOW_SIZE changes, a receiver MUST adjust
  -- # the size of all stream flow-control windows that it maintains by the
  -- # difference between the new value and the old value.
  , counterexample "INITIAL_WINDOW_SIZE raised: what waited goes" $
      let [b] = blocks [base]
          (c, _) = feedAll (serve [InitialWindowSize 0] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 "abc"]
      in datas (from (c, []) [Received (renderSettings [InitialWindowSize 10])]) === [(1, "abc", False)]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.2
  -- # An endpoint MUST treat a change to SETTINGS_INITIAL_WINDOW_SIZE that
  -- # causes any flow-control window to exceed the maximum size as a
  -- # connection error (Section 5.4.1) of type FLOW_CONTROL_ERROR.
  , counterexample "INITIAL_WINDOW_SIZE raised past 2^31-1 for a stream: FLOW_CONTROL_ERROR on the connection" $
      let [b] = blocks [base]
          (c, _) = serve [] (headers 1 True b <> renderWindowUpdate (StreamId 1) 2147418112)
      in seen (from (c, []) [Received (renderSettings [InitialWindowSize 65536])]) === [SeenGoAway FlowControlError]
         .&&. seen (from (c, []) [Received (renderSettings [InitialWindowSize 65535])]) === []
         -- each value is applied in turn (Section 6.5.3): the first
         -- overflows, though the last would not
         .&&. seen (from (c, []) [Received (renderSettings [InitialWindowSize 65536, InitialWindowSize 65535])])
                === [SeenGoAway FlowControlError]
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.9.1
  -- # A sender MUST NOT allow a flow-control window to exceed 2^31-1
  -- # octets.  If a sender receives a WINDOW_UPDATE that causes a flow-
  -- # control window to exceed this maximum, it MUST terminate either the
  -- # stream or the connection, as appropriate.
  , counterexample "WINDOW_UPDATE past 2^31-1: on the connection, a connection error; on a stream, a stream error" $
      let [b] = blocks [base]
          (c, _) = serve [] (headers 1 True b)
      in seen (from (c, []) [windowUpdate 0 2147418112]) === []
         .&&. seen (from (c, []) [windowUpdate 0 2147418113]) === [SeenGoAway FlowControlError]
         .&&. seen (from (c, []) [windowUpdate 1 2147418112]) === []
         .&&. seen (from (c, []) [windowUpdate 1 2147418113]) === [SeenReset 1 FlowControlError]
  , counterexample "WINDOW_UPDATE on an idle stream: PROTOCOL_ERROR; on a closed one: ignored" $
      let [b] = blocks [base]
          (c, _) = feedAll (serve [] (headers 1 True b)) [HandlerDone (StreamId 1)]
      in seen (from (c, []) [windowUpdate 3 1]) === [SeenGoAway ProtocolError]
         .&&. seen (from (c, []) [windowUpdate 1 1]) === []
  , counterexample "WINDOW_UPDATEs leaving a stream's window under 1 KiB cost one each" $
      let [b] = blocks [base]
          tiny n = B.concat (replicate n (renderWindowUpdate (StreamId 1) 1))
      in closed (snd (serve [InitialWindowSize 0] (headers 1 True b <> tiny 999))) === False
         .&&. closed (snd (serve [InitialWindowSize 0] (headers 1 True b <> tiny 1000))) === True
         .&&. closed (snd (serve [InitialWindowSize 1024] (headers 1 True b <> tiny 1000))) === False
  ]

prop_scheduler :: Property
prop_scheduler = once $ conjoin
  [ counterexample "two streams take turns, frame by frame, 16 KiB at most" $
      let (c, _) = feedAll (serve [InitialWindowSize 0] twoGets)
                           [ respondOn 1 (ok Nothing), write 1 (B.replicate 40000 0x61)
                           , respondOn 3 (ok Nothing), write 3 (B.replicate 40000 0x62) ]
          o = from (c, []) [Received (renderSettings [InitialWindowSize 65535])]
      in [(s, B.length d) | (s, d, _) <- datas o] === [(1, 16384), (3, 16384), (1, 16384), (3, 16383)]
  , counterexample "a stream that cannot send does not hold up one that can" $
      let (c, _) = feedAll (serve [InitialWindowSize 0] twoGets)
                           [respondOn 1 (ok Nothing), write 1 "a", respondOn 3 (ok Nothing), write 3 "b"]
      in datas (from (c, []) [windowUpdate 3 10]) === [(3, "b", False)]
  , counterexample "content and trailers queued behind the window: both go once it opens" $
      let [b] = blocks [base]
          (c, _) = feedAll (serve [InitialWindowSize 0] (headers 1 True b))
                           [respondOn 1 (ok Nothing), write 1 "abc", EndBody (StreamId 1) (fieldsOf [("x-sum", "1")])]
      in seen (from (c, []) [windowUpdate 1 10])
           === [SeenOther (DataFrame (StreamId 1) False "abc" 3), SeenHeaders 1 [("x-sum", "1")] True]
  , counterexample "the client's larger MAX_FRAME_SIZE: DATA still 16 KiB at most" $
      let [b] = blocks [base]
          o = from (serve [MaxFrameSize 1048576] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 (B.replicate 40000 0x61)]
      in map (\(_, d, _) -> B.length d) (datas o) === [16384, 16384, 7232]
  , counterexample "Writable at once while under 16 KiB buffered; at 16 KiB, once it is sent" $
      let [b] = blocks [base]
          (c, o) = feedAll (serve [InitialWindowSize 0] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 (B.replicate 16383 0x61)]
          (c1, o1) = feedAll (c, []) [write 1 "x"]
          o2 = from (c1, []) [windowUpdate 1 1]
      in writables o === [1] .&&. writables o1 === [] .&&. writables o2 === [1]
  , counterexample "the connection's 256 KiB: a stream with room waits too, until any is sent" $
      let bs = blocks (replicate 17 base)
          opened = B.concat [headers (2 * i + 1) True b | (i, b) <- zip [0 ..] bs]
          fill = concat [[respondOn (2 * i + 1) (ok Nothing), write (2 * i + 1) (B.replicate 16384 0x61)] | i <- [0 .. 15]]
          (c, o) = feedAll (serve [InitialWindowSize 0] opened) fill
          (c1, o1) = feedAll (c, []) [respondOn 33 (ok Nothing), write 33 "x"]
          o2 = from (c1, []) [windowUpdate 33 1]
          o3 = from (c1, []) [windowUpdate 1 1024]
      in writables o === [] .&&. writables o1 === [] .&&. writables o2 === [] .&&. writables o3 === [1, 33]
  , counterexample "DATA stops at 64 KiB not yet written, and resumes as the transport writes" $
      let [b] = blocks [base]
          -- the transport writes nothing
          (c, o) = runWith defaultConfig
                     [ (0, Received (connectionPreface <> renderSettings [InitialWindowSize 1000000] <> headers 1 True b
                                     <> renderWindowUpdate (StreamId 0) 1000000))
                     , (0, respondOn 1 (ok Nothing)), (0, write 1 (B.replicate 200000 0x61)) ]
          emitted = sum [B.length x | Send xs <- o, x <- xs]
          o1 = snd (step 0 (Sent emitted) c)
      in counterexample (show emitted) (emitted >= 65536 .&&. emitted < 65536 + 16384 + 9)
         .&&. sum [B.length d | (_, d, _) <- datas o1] === 65536
  ]

-- the reference model: whatever the writes and the window updates, each
-- stream's DATA is what was written, in order, ended once; no frame
-- goes beyond either window; nothing is a misuse
data SendOp = WriteOp Word32 Int | UpdateOp Word32 Word32
  deriving (Show)

prop_sendModel :: Property
prop_sendModel =
  forAll (choose (0, 70000)) $ \w0 ->
  forAll (listOf1 op) $ \ops ->
    let (c0, _) = serve [InitialWindowSize (fromIntegral w0)] twoGets
        (c1, o1) = feedAll (c0, []) [respondOn 1 (ok Nothing), respondOn 3 (ok Nothing)]
        -- the driver writes only after the last write's Writable
        go (c, ss, waiting, ws) o = case o of
          WriteOp s n
            | s `elem` waiting -> (c, ss ++ [[]], waiting, ws)
            | otherwise ->
                let d = B.replicate n (fromIntegral (length ss `mod` 251))
                    (c', o') = writeAll (step 0 (write s d) c)
                in (c', ss ++ [o'], filter (`notElem` writables o') (s : waiting), M.insertWith (flip (<>)) s d ws)
          UpdateOp s n ->
            let (c', o') = writeAll (step 0 (windowUpdate s n) c)
            in (c', ss ++ [o'], filter (`notElem` writables o') waiting, ws)
        (c2, steps, _, written) = foldl' go (c1, [], [], M.empty) ops
        flush = from (c2, []) [endOn 1, endOn 3, windowUpdate 0 1000000, windowUpdate 1 1000000, windowUpdate 3 1000000]
        allOuts = o1 ++ concat steps ++ flush
        on s = [(d, e) | (s', d, e) <- datas allOuts, s' == s]
    in conjoin
         [ counterexample ("stream " ++ show s) $
             B.concat (map fst (on s)) === M.findWithDefault "" s written
             .&&. map snd (on s) === map (const False) (drop 1 (on s)) ++ [True]
         | s <- [1, 3] ]
       .&&. not (misused allOuts) .&&. not (closed allOuts)
       .&&. conjoin (windows (M.fromList [(0, 65535), (1, w0), (3, w0)]) M.empty (zip ops steps))
  where
    op = oneof [ WriteOp <$> elements [1, 3] <*> choose (0, 20000)
               , UpdateOp <$> elements [0, 1, 3] <*> choose (1, 40000) ]
    -- after each step, what was sent on each stream and on the
    -- connection is within the credit given so far
    windows _ _ [] = []
    windows credit sent ((o, outs) : rest) =
      let credit' = case o of
            UpdateOp s n -> M.insertWith (+) s (fromIntegral n) credit
            WriteOp _ _ -> credit
          sent' = foldl' (\m (s, d, _) -> M.insertWith (+) 0 (B.length d) (M.insertWith (+) s (B.length d) m)) sent (datas outs)
      in [ counterexample ("over the window of " ++ show s) (M.findWithDefault 0 s sent' <= M.findWithDefault (0 :: Int) s credit')
         | s <- [0, 1, 3 :: Word32] ] ++ windows credit' sent' rest

------------------------------------------------------------------------------
-- priorities (RFC 9218)
------------------------------------------------------------------------------

priorityTests :: TestTree
priorityTests = testGroup "The connection core: priorities (RFC 9218, P4, P5, P9, P10)"
  [ testProperty "the order responses are sent in, by the client's and the server's signals" prop_priorityOrder
  , testProperty "PRIORITY_UPDATE: errors, the bound on streams not yet open, the budget" prop_priorityUpdates
  ]

-- a PRIORITY_UPDATE frame
priorityUpdate :: Word32 -> ByteString -> ByteString
priorityUpdate target value = rawFrame 0x10 0 0 (B.pack [fromIntegral (target `shiftR` n) | n <- [24, 16, 8, 0]] <> value)

-- GETs on the given streams with these fields added, after the octets
-- given first; responses of 20,000 octets, with these fields, written
-- while the client's windows are 0; then the octets given last and the
-- windows opened: the streams of the DATA frames sent, in order
sendOrder :: ByteString -> [(Word32, [(ByteString, ByteString)])] -> [(Word32, [(ByteString, ByteString)])]
          -> ByteString -> [Word32]
sendOrder first reqs = sendOrderSteps (Raw first : map (uncurry Req) reqs)

-- what the client sends before the windows open: requests, their blocks
-- encoded in order by one encoder, and other octets between them
data ClientStep = Req Word32 [(ByteString, ByteString)] | Raw ByteString

sendOrderSteps :: [ClientStep] -> [(Word32, [(ByteString, ByteString)])] -> ByteString -> [Word32]
sendOrderSteps steps resps lastly =
  let reqs = [ (sid, fs) | Req sid fs <- steps ]
      bs = M.fromList (zip (map fst reqs) (blocks [ base ++ fs | (_, fs) <- reqs ]))
      opened = B.concat [ case st of { Req sid _ -> headers sid True (bs M.! sid); Raw b -> b } | st <- steps ]
      respondAll = concat [ [respondOn sid (response 200 (M.findWithDefault [] sid (M.fromList resps)) Nothing), write sid (B.replicate 20000 0x61)]
                          | (sid, _) <- reqs ]
      (c, _) = feedAll (serve [InitialWindowSize 0] opened) respondAll
  in [ sid | (sid, _, _) <- datas (from (c, []) [Received (lastly <> renderSettings [InitialWindowSize 65535])]) ]

prop_priorityOrder :: Property
prop_priorityOrder = once $ conjoin
  [ counterexample "no signals: urgency 3, incremental, so in turn (P9)" $
      sendOrder "" [(1, []), (3, [])] [] "" === [1, 3, 1, 3]
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-10
  -- # It is RECOMMENDED that, when possible, servers respect the urgency
  -- # parameter (Section 4.1), sending higher-urgency responses before
  -- # lower-urgency responses.
  , counterexample "the request's urgency: the lower first" $
      sendOrder "" [(1, [("priority", "u=5")]), (3, [("priority", "u=1")])] [] "" === [3, 3, 1, 1]
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-10
  -- # Non-incremental responses of the same urgency SHOULD be served by
  -- # prioritizing bandwidth allocation in ascending order of the stream
  -- # ID, which corresponds to the order in which clients make requests.
  , counterexample "non-incremental at one urgency: one after the other, in stream order" $
      sendOrder "" [(1, [("priority", "u=3")]), (3, [("priority", "u=3")])] [] "" === [1, 1, 3, 3]
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-4
  -- # When receiving an HTTP request that does not carry these
  -- # priority parameters, a server SHOULD act as if their default values
  -- # were specified.
  , counterexample "after a signal, a request without one has the defaults: not incremental, so the two kinds alternate" $
      sendOrder "" [(1, [("priority", "u=3, i")]), (3, [])] [] "" === [3, 1, 3, 1]
  , counterexample "a field that is not a Dictionary is no signal" $
      sendOrder "" [(1, [("priority", "u=")]), (3, [])] [] "" === [1, 3, 1, 3]
  , counterexample "a PRIORITY_UPDATE is a signal: later requests without a field have the defaults" $
      sendOrder (priorityUpdate 99 "u=1") [(1, []), (3, [])] [] "" === [1, 1, 3, 3]
      .&&. sendOrderSteps [Req 1 [], Raw (priorityUpdate 1 "u=4"), Req 3 [], Req 5 []] [] "" === [3, 3, 5, 5, 1, 1]
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-7
  -- # Servers SHOULD buffer the most recently received PRIORITY_UPDATE
  -- # frame and apply it once the referenced stream is opened.
  , counterexample "a PRIORITY_UPDATE before its stream opens, applied when it does, over the request's field" $
      sendOrder (priorityUpdate 3 "u=7" <> priorityUpdate 3 "u=0") [(1, [("priority", "u=1")]), (3, [("priority", "u=7")])] [] ""
        === [3, 3, 1, 1]
  , counterexample "a PRIORITY_UPDATE on an open stream moves it" $
      sendOrder "" [(1, [("priority", "u=3")]), (3, [("priority", "u=3")])] [] (priorityUpdate 3 "u=0") === [3, 3, 1, 1]
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-7
  -- # Omitting a priority
  -- # parameter is a signal to use its default value.
  , counterexample "a PRIORITY_UPDATE's parameters not named are the defaults: urgency 3 again, beside stream 1" $
      sendOrder "" [(1, [("priority", "u=3")]), (3, [("priority", "u=0, i")])] [] (priorityUpdate 3 "i") === [1, 3, 1, 3]
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-8
  -- # The absence of a priority parameter in an HTTP response indicates the
  -- # server's disinterest in changing the client-provided value.
  , counterexample "the response's field over the client's, for the parameters it names (P4)" $
      sendOrder "" [(1, [("priority", "u=1, i")]), (3, [("priority", "u=1, i")]), (5, [("priority", "u=2")])]
                   [(1, [("priority", "u=2")])] ""
        === [3, 3, 5, 1, 5, 1]
  , counterexample "an end with nothing left to send, which needs no window, counts as its stream's turn" $
      let bs = blocks [ base ++ [("priority", "u=3")], base ++ [("priority", "u=3, i")]
                      , base ++ [("priority", "u=3")], base ++ [("priority", "u=0")] ]
          opened = B.concat [ headers s True b | (s, b) <- zip [1, 3, 5, 7] bs ]
          (c, _) = feedAll (serve [] opened)
                     [ respondOn 1 (ok Nothing), write 1 "x"
                     -- stream 7 takes all the connection's window
                     , respondOn 7 (ok Nothing), write 7 (B.replicate 65534 0x61)
                     , respondOn 3 (ok Nothing), write 3 (B.replicate 20000 0x62)
                     , respondOn 5 (ok Nothing), write 5 (B.replicate 20000 0x63)
                     -- stream 1's end goes at once: incremental streams next
                     , endOn 1 ]
      in [ s | (s, _, _) <- datas (from (c, []) [windowUpdate 0 100000]) ] === [3, 5, 3, 5]
  , counterexample "the response's field is sent as it is" $
      let [b] = blocks [base]
          outs = from (serve [] (headers 1 True b)) [respondOn 1 (response 200 [("priority", "u=6")] Nothing)]
      in take 1 (seen outs) === [SeenHeaders 1 [(":status", "200"), ("priority", "u=6")] False]
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-13.2
  -- # Backend
  -- # servers SHOULD only schedule based on client priority information
  -- # where that information can be scoped to individual end clients.
  , counterexample "after a request through a proxy, request priority fields are ignored (P10)" $
      sendOrder "" [(1, [("via", "1.1 proxy"), ("priority", "u=6")]), (3, [("priority", "u=0")])] [] "" === [1, 3, 1, 3]
      .&&. sendOrder "" [(1, [("forwarded", "for=192.0.2.1")]), (3, [("priority", "u=0")])] [] "" === [1, 3, 1, 3]
      .&&. sendOrder "" [(1, []), (3, [("x-forwarded-for", "192.0.2.1"), ("priority", "u=0")])] [] "" === [1, 3, 1, 3]
  , counterexample "but PRIORITY_UPDATE still applies" $
      sendOrder "" [(1, [("via", "1.1 proxy")]), (3, [])] [] (priorityUpdate 3 "u=0") === [3, 3, 1, 1]
  ]

prop_priorityUpdates :: Property
prop_priorityUpdates = once $ conjoin
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-7.1
  -- # Servers that
  -- # receive a PRIORITY_UPDATE for a push stream in the "idle" state MUST
  -- # respond with a connection error of type PROTOCOL_ERROR.
  [ counterexample "an even stream: a push stream, idle" $
      seen (snd (serve [] (priorityUpdate 2 "u=1"))) === [SeenGoAway ProtocolError]
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-7.1
  -- # The number of streams that have been prioritized but remain in the
  -- # "idle" state plus the number of active streams (those in the "open"
  -- # state or in either of the "half-closed" states; see Section 5.1.2 of
  -- # [HTTP/2]) MUST NOT exceed the value of the
  -- # SETTINGS_MAX_CONCURRENT_STREAMS parameter.  Servers that receive such
  -- # a PRIORITY_UPDATE MUST respond with a connection error of type
  -- # PROTOCOL_ERROR.
  , counterexample "streams not yet open: 100 kept, the 101st an error" $
      seen (snd (serve [] (idleUpdates 3 100))) === []
      .&&. seen (snd (serve [] (idleUpdates 3 101))) === [SeenGoAway ProtocolError]
  , counterexample "an active stream counts" $
      seen (snd (serve [] (open1 <> idleUpdates 3 99))) === []
      .&&. seen (snd (serve [] (open1 <> idleUpdates 3 100))) === [SeenGoAway ProtocolError]
  , counterexample "a stream updated again counts once" $
      seen (snd (serve [] (idleUpdates 3 100 <> priorityUpdate 3 "u=2"))) === []
  , counterexample "the updates of streams passed over are dropped as a later stream opens" $
      let [b] = blocks [base]
      in seen (snd (serve [] (idleUpdates 3 100 <> headers 301 True b <> idleUpdates 303 99))) === []
  , counterexample "one for a stream already closed is ignored" $
      let [b] = blocks [base]
      in seen (snd (serve [] (headers 3 True b <> priorityUpdate 1 "u=0"))) === []
  -- the budget (P5, C10): the SETTINGS costs one; 999 more are allowed
  -- = https://www.rfc-editor.org/rfc/rfc9218#section-7
  -- # Failure to parse the
  -- # Priority Field Value MAY be treated as a connection error.
  --
  -- Not taken (P5)
  , counterexample "a value that does not parse: ignored, and costs one" $
      closed (snd (serve [] (times 999 (priorityUpdate 1 "u=")))) === False
      .&&. closed (snd (serve [] (times 1000 (priorityUpdate 1 "u=")))) === True
  , counterexample "one for a closed stream costs one" $
      let [b] = blocks [base]
      in closed (snd (serve [] (headers 3 True b <> times 999 (priorityUpdate 1 "u=1")))) === False
         .&&. closed (snd (serve [] (headers 3 True b <> times 1000 (priorityUpdate 1 "u=1")))) === True
  , counterexample "one that changes nothing costs one: open stream" $
      closed (snd (serve [] (open1 <> times 1000 (priorityUpdate 1 "u=1")))) === False
      .&&. closed (snd (serve [] (open1 <> times 1001 (priorityUpdate 1 "u=1")))) === True
  , counterexample "one that changes nothing costs one: a stream not yet open" $
      closed (snd (serve [] (times 1000 (priorityUpdate 5 "u=1")))) === False
      .&&. closed (snd (serve [] (times 1001 (priorityUpdate 5 "u=1")))) === True
  , counterexample "ones that change something cost nothing" $
      closed (snd (serve [] (open1 <> times 2000 (priorityUpdate 1 "u=1" <> priorityUpdate 1 "u=2")))) === False
      .&&. closed (snd (serve [] (times 2000 (priorityUpdate 5 "u=1" <> priorityUpdate 5 "u=2")))) === False
  ]
  where
    times n = B.concat . replicate n
    open1 = let [b] = blocks [base] in headers 1 True b
    idleUpdates :: Word32 -> Word32 -> ByteString
    idleUpdates from' n = B.concat [ priorityUpdate (from' + 2 * i) "u=1" | i <- [0 .. n - 1] ]

------------------------------------------------------------------------------
-- errors, timers and shutdown
------------------------------------------------------------------------------

endingTests :: TestTree
endingTests = testGroup "The connection core: timers, GOAWAY, shutdown (C2, C8, C11, D10)"
  [ testProperty "a stream's timers run only while the client holds it up" prop_streamTimers
  , testProperty "the idle connection" prop_idle
  , testProperty "graceful shutdown: GOAWAY, PING, the final GOAWAY, the close" prop_shutdown
  , testProperty "the client's GOAWAY" prop_peerGoAway
  , testProperty "the end of a connection tells every handler waiting" prop_closing
  , testProperty "D9's limits: an HPACK bomb, the requests a connection serves" prop_limits
  ]

-- a connection with this configuration whose client sent these SETTINGS,
-- acknowledged ours, and sent these octets, its output written at once
serveWith :: Config -> [Setting] -> ByteString -> (Conn, [Output])
serveWith config ss b =
  feedAll (writeAll (newConn config 0)) [Received (connectionPreface <> renderSettings ss <> renderSettingsAck <> b)]

-- inputs at the given times, the output written at once
feedAt :: (Conn, [Output]) -> [(Time, Input)] -> (Conn, [Output])
feedAt = foldl' feed
  where feed (c, outs) (t, i) = let (c', o) = writeAll (step t i c) in (c', outs ++ o)

-- the GOAWAYs sent: the last stream and the code
goAways :: [Output] -> [(Word32, ErrorCode)]
goAways outs = [ (l, code) | GoAwayFrame (StreamId l) code _ <- parsed outs ]

pingAnswer :: Input
pingAnswer = Received (renderPing True "shutdown")

prop_streamTimers :: Property
prop_streamTimers = once $ conjoin
  [ counterexample "a response the client's window holds up: CANCEL when the 5 s grace has gone without progress" $
      let [b] = blocks [base]
          (c, _) = feedAll (serveWith defaultConfig [InitialWindowSize 0] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 "abc"]
          (_, o) = feedAt (c, []) [(5000, TimedOut)]
      in nextDeadline c === Just 5000 .&&. seen o === [SeenReset 1 Cancel] .&&. aborted o === [(1, StreamTimedOut)]
         .&&. misused (snd (step 4999 TimedOut c))
  , counterexample "a stream that waits on its handler has no deadline" $
      let [b] = blocks [post]
          (c, _) = feedAll (serveWith defaultConfig [] (headers 1 False b <> dataFrames 1 False "abc")) [respondOn 1 (ok Nothing), write 1 "x"]
      -- content queued and not consumed: the handler has not seen it yet
      in nextDeadline c === Nothing .&&. nextDeadline (fst (feedAll (c, []) [WantContent (StreamId 1)])) === Nothing
  , counterexample "a handler waiting for content: timed; content moved adds to the allowance; time paused while the handler has it" $
      let [b] = blocks [post]
          (c, _) = feedAt (serveWith defaultConfig [] (headers 1 False b)) [(0, WantContent (StreamId 1))]
          -- 3 s waited (720 of the 1,200 octets' allowance), then 10 octets
          -- (+10), then the handler reads them and waits again at 100 s
          (c1, _) = feedAt (c, []) [ (3000, Received (dataFrames 1 False "0123456789"))
                                   , (3000, Consumed (StreamId 1) 10), (100000, WantContent (StreamId 1)) ]
          -- the timer entry from the first wait, now early, is renewed
          (c2, o2) = feedAt (c1, []) [(5000, TimedOut)]
      in nextDeadline c === Just 5000 .&&. seen o2 === [] .&&. nextDeadline c2 === Just (100000 + 490000 `div` 240)
  , counterexample "the allowance is capped at the idle timeout's worth: a slow drip after a fast start is caught" $
      let [b] = blocks [base]
          -- 165,535 octets go at once (the connection's window), 14,689 wait
          (c, o) = feedAll (serveWith defaultConfig [InitialWindowSize 1000000] (headers 1 True b <> renderWindowUpdate (StreamId 0) 100000))
                           (respondOn 1 (ok Nothing) : replicate 11 (write 1 (B.replicate 16384 0x61)))
          -- the client opens 1,024 octets after 250 s; the timer's entry,
          -- at 300 s since the allowance was capped, is then early
          (c1, o1) = feedAt (c, []) [(250000, windowUpdate 0 1024)]
          (c2, o2) = feedAt (c1, []) [(300000, TimedOut)]
          capped = 72000000 - 240 * 250000 + 1024 * 1000
      in sum [B.length d | (_, d, _) <- datas o] === 165535 .&&. nextDeadline c === Just 300000
         .&&. map (\(_, d, _) -> B.length d) (datas o1) === [1024]
         .&&. seen o2 === [] .&&. nextDeadline c2 === Just (250000 + capped `div` 240)
  , counterexample "with the minimum rate off, the idle timer: 5 minutes without progress" $
      let [b] = blocks [base]
          config = defaultConfig { configMinRate = 0 }
          (c, _) = feedAll (serveWith config [InitialWindowSize 0] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 "abc"]
      in nextDeadline c === Just 300000 .&&. aborted (snd (feedAt (c, []) [(300000, TimedOut)])) === [(1, StreamTimedOut)]
  , counterexample "the idle timer: what has run of it is kept when the clock is read, and progress restarts it" $
      let [b] = blocks [post]
          config = defaultConfig { configMinRate = 0 }
          (c, _) = feedAt (serveWith config [] (headers 1 False b)) [(0, WantContent (StreamId 1))]
          -- an empty DATA at 100 s: no progress, but the clock is read
          stalled = feedAt (c, []) [(100000, Received (dataFrames 1 False "")), (300000, TimedOut)]
          -- content at 100 s: progress; the handler waits again at once
          moving = feedAt (c, []) [ (100000, Received (dataFrames 1 False "x")), (100000, Consumed (StreamId 1) 1)
                                  , (100000, WantContent (StreamId 1)), (300000, TimedOut) ]
      in aborted (snd stalled) === [(1, StreamTimedOut)] .&&. aborted (snd moving) === []
         .&&. nextDeadline (fst moving) === Just 400000
  , counterexample "both off: no deadline" $
      let [b] = blocks [base]
          config = defaultConfig { configMinRate = 0, configStreamIdleTimeout = 0 }
      in nextDeadline (fst (feedAll (serveWith config [InitialWindowSize 0] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 "abc"]))
           === Nothing
  , counterexample "a stream that ends leaves no deadline behind" $
      let [b] = blocks [base]
          (c, _) = feedAll (serveWith defaultConfig [InitialWindowSize 0] (headers 1 True b)) [respondOn 1 (ok Nothing), write 1 "abc"]
          (c1, _) = feedAll (c, []) [Received (rst 1 Cancel)]
          -- or that is sent and finishes: only the idle timer is left
          (c2, _) = feedAll (c, []) [endOn 1, windowUpdate 1 10]
      in nextDeadline c1 === Nothing .&&. nextDeadline c2 === Just 60000
  , counterexample "the trailers, held up by pacing, keep the clock running" $
      let [b] = blocks [base]
          (c, _) = runWith defaultConfig { configPacing = 1 }
                     [ (0, Received (connectionPreface <> renderSettings [] <> renderSettingsAck <> headers 1 True b))
                     , (0, respondOn 1 (ok Nothing)), (0, EndBody (StreamId 1) (fieldsOf [("x-sum", "1")])) ]
      in nextDeadline c === Just 5000
  ]

prop_idle :: Property
prop_idle = once $ conjoin
  [ counterexample "no streams for 60 s: GOAWAY NO_ERROR, then the close" $
      let (c, _) = serveWith defaultConfig [] ""
          o = snd (feedAt (c, []) [(60000, TimedOut)])
      in nextDeadline c === Just 60000 .&&. goAways o === [(0, NoError)] .&&. closed o
  , counterexample "a PING from the client restarts it" $
      nextDeadline (fst (feedAt (serveWith defaultConfig [] "") [(30000, Received (renderPing False "12345678"))])) === Just 90000
  , counterexample "a stream stops it; it starts again when the last stream is done" $
      let [b] = blocks [base]
          (c, _) = feedAt (serveWith defaultConfig [] "") [(10000, Received (headers 1 True b))]
          (c1, _) = feedAt (c, []) [(20000, respondOn 1 (ok (Just 0))), (20000, endOn 1)]
          (c2, _) = feedAt (c1, []) [(25000, HandlerDone (StreamId 1))]
      in nextDeadline c === Nothing .&&. nextDeadline c2 === Just 80000
  , counterexample "off: no deadline" $
      nextDeadline (fst (serveWith defaultConfig { configIdleTimeout = 0 } [] "")) === Nothing
  ]

prop_shutdown :: Property
prop_shutdown = once $ conjoin
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.8
  -- # A server that is attempting to gracefully shut down a
  -- # connection SHOULD send an initial GOAWAY frame with the last stream
  -- # identifier set to 2^31-1 and a NO_ERROR code.  This signals to the
  -- # client that a shutdown is imminent and that initiating further
  -- # requests is prohibited.  After allowing time for any in-flight stream
  -- # creation (at least one round-trip time), the server MAY send another
  -- # GOAWAY frame with an updated last stream identifier.
  [ counterexample "GOAWAY 2^31-1 and a PING; on the answer, GOAWAY with the last stream; the stream finishes; the close" $
      let [b] = blocks [base]
          (c, o) = feedAll (serveWith defaultConfig [] (headers 1 True b)) [Shutdown]
          (c1, o1) = feedAll (c, []) [pingAnswer]
          o2 = from (c1, []) [respondOn 1 (ok (Just 0)), endOn 1]
      in goAways o === [(2147483647, NoError)] .&&. SeenOther (PingFrame False "shutdown") `elem` seen o
         .&&. not (closed o) .&&. goAways o1 === [(1, NoError)] .&&. not (closed o1)
         .&&. datas o2 === [(1, "", True)] .&&. closed o2
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.7
  -- # If frames that are on a stream
  -- # are passed to the application layer for any stream, then
  -- # REFUSED_STREAM MUST NOT be used for that stream, and a GOAWAY frame
  -- # MUST include a stream identifier that is greater than or equal to the
  -- # given stream identifier.
  , counterexample "a stream opened before the answer is served; one after it is refused" $
      let [b1, b3, b5] = blocks [base, base, base]
          (c, _) = feedAll (serveWith defaultConfig [] (headers 1 True b1)) [Shutdown, Received (headers 3 True b3)]
          (c1, o1) = feedAll (c, []) [pingAnswer]
          o2 = from (c1, []) [Received (headers 5 True b5)]
      in goAways o1 === [(3, NoError)] .&&. requests o2 === [] .&&. seen o2 === [SeenReset 5 RefusedStream]
  , counterexample "a field block begun when the PING's timer expires: its stream counts as opened" $
      let [b1, b3] = blocks [base, base]
          (c, _) = feedAll (serveWith defaultConfig [] (headers 1 True b1)) [Shutdown, Received (rawFrame 1 1 3 (B.take 2 b3))]
          (c1, o1) = feedAt (c, []) [(1000, TimedOut)]
      in goAways o1 === [(3, NoError)] .&&. length (requests (from (c1, []) [Received (rawFrame 9 4 3 (B.drop 2 b3))])) === 1
  , counterexample "a field block begun keeps a connection with no streams open until it ends" $
      let [b] = blocks [base]
          (c, o) = feedAt (serveWith defaultConfig [] "") [(0, Shutdown), (0, Received (rawFrame 1 1 1 (B.take 2 b))), (1000, TimedOut)]
      in not (closed o) .&&. length (requests (from (c, []) [Received (rawFrame 9 4 1 (B.drop 2 b))])) === 1
  , counterexample "streams reset whose handlers still run do not hold the close" $
      let [b] = blocks [post]
          o = snd (feedAll (serveWith defaultConfig [] (headers 1 False b <> rst 1 Cancel)) [Shutdown, pingAnswer])
      in closed o .&&. aborted o === [(1, ResetByClient Cancel)]
  , counterexample "the PING unanswered: the final GOAWAY after 1 s" $
      let [b] = blocks [base]
          (c, _) = feedAll (serveWith defaultConfig [] (headers 1 True b)) [Shutdown]
      in nextDeadline c === Just 1000 .&&. goAways (snd (feedAt (c, []) [(1000, TimedOut)])) === [(1, NoError)]
  , counterexample "no streams: closed once the PING is answered" $
      let o = snd (feedAll (serveWith defaultConfig [] "") [Shutdown, pingAnswer])
      in goAways o === [(2147483647, NoError), (0, NoError)] .&&. closed o
  , counterexample "another PING's answer is not the shutdown's" $
      goAways (snd (feedAll (serveWith defaultConfig [] "") [Shutdown, Received (renderPing True "12345678")]))
        === [(2147483647, NoError)]
  , counterexample "with the PING timer off, the final GOAWAY at once" $
      let [b] = blocks [base]
      in goAways (snd (feedAll (serveWith defaultConfig { configPingTimeout = 0 } [] (headers 1 True b)) [Shutdown])) === [(1, NoError)]
  , counterexample "Shutdown again (the grace period's end): CANCEL for what is left, GOAWAY, handlers told, the close" $
      let [b1, b3, b5] = blocks [base, post, post]
          -- stream 5 was reset, and its handler still runs: no second reset
          (c, _) = feedAll (serveWith defaultConfig [] (headers 1 True b1 <> headers 3 False b3 <> headers 5 False b5 <> rst 5 Cancel))
                           [Shutdown]
          o = from (c, []) [Shutdown]
      in seen o === [SeenReset 1 Cancel, SeenReset 3 Cancel, SeenGoAway NoError] .&&. goAways o === [(5, NoError)]
         .&&. aborted o === [(1, ConnectionEnded), (3, ConnectionEnded)] .&&. closed o
  , counterexample "before the client's preface: closed, nothing sent" $
      let (c, _) = newConn defaultConfig 0
          o = snd (step 0 Shutdown c)
      in parsed o === [] .&&. closed o
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-6.8
  -- # Endpoints MUST NOT increase the
  -- # value they send in the last stream identifier, since the peers might
  -- # already have retried unprocessed requests on another connection.
  , counterexample "a connection error after the final GOAWAY does not raise its last stream" $
      let [b1, b3] = blocks [base, base]
          (c, _) = feedAll (serveWith defaultConfig [] (headers 1 True b1)) [Shutdown, pingAnswer, Received (headers 3 True b3)]
      in goAways (from (c, []) [Received (rawFrame 0xA 0 0 "")]) === []
         .&&. goAways (from (c, []) [Received (rawFrame 1 4 2 "")]) === [(1, ProtocolError)]
  ]

prop_peerGoAway :: Property
prop_peerGoAway = once $ conjoin
  [ counterexample "with a stream open: it finishes, then our GOAWAY and the close" $
      let [b] = blocks [base]
          (c, o) = feedAll (serveWith defaultConfig [] (headers 1 True b)) [Received (renderGoAway (StreamId 0) NoError "")]
          o1 = from (c, []) [respondOn 1 (ok (Just 0)), endOn 1]
      in goAways o === [] .&&. not (closed o) .&&. datas o1 === [(1, "", True)] .&&. goAways o1 === [(1, NoError)] .&&. closed o1
  , counterexample "with none: our GOAWAY and the close at once" $
      let o = snd (serveWith defaultConfig [] (renderGoAway (StreamId 0) NoError ""))
      in goAways o === [(0, NoError)] .&&. closed o
  , counterexample "a new stream after it: served, and the close once it is done too" $
      let [b1, b3] = blocks [base, base]
          (c, o) = serveWith defaultConfig [] (headers 1 True b1 <> renderGoAway (StreamId 0) NoError "" <> headers 3 True b3)
          o1 = from (c, []) [respondOn 1 (ok (Just 0)), endOn 1]
          o3 = from (fst (feedAll (c, []) [respondOn 1 (ok (Just 0)), endOn 1]), []) [respondOn 3 (ok (Just 0)), endOn 3]
      in map (\(s, _, _, _) -> s) (requests o) === [1, 3] .&&. seen o === []
         .&&. not (closed o1) .&&. goAways o3 === [(3, NoError)] .&&. closed o3
  , counterexample "with an error: the close at once, the handlers told" $
      let [b] = blocks [base]
          o = snd (serveWith defaultConfig [] (headers 1 True b <> renderGoAway (StreamId 0) InternalError ""))
      in goAways o === [] .&&. aborted o === [(1, ConnectionEnded)] .&&. closed o
  ]

prop_closing :: Property
prop_closing = once $ conjoin
  [ counterexample "PeerClosed: the handlers waiting are told; one already told of its reset is not told again" $
      let [b1, b3] = blocks [post, post]
          (c, _) = serveWith defaultConfig [] (headers 1 False b1 <> headers 3 False b3 <> rst 3 Cancel)
          o = from (c, []) [PeerClosed]
      in aborted o === [(1, ConnectionEnded)] .&&. closed o
  , counterexample "a response being sent after its handler returned: no one to tell" $
      let [b] = blocks [base]
          (c, _) = feedAll (serveWith defaultConfig [InitialWindowSize 0] (headers 1 True b))
                           [respondOn 1 (ok Nothing), write 1 "abc", endOn 1, HandlerDone (StreamId 1)]
      in aborted (from (c, []) [PeerClosed]) === []
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-8.4
  -- # A client cannot push.  Thus, servers MUST treat the receipt of a
  -- # PUSH_PROMISE frame as a connection error (Section 5.4.1) of type
  -- # PROTOCOL_ERROR.
  , counterexample "a connection error (PUSH_PROMISE from the client): GOAWAY, the handlers told" $
      let [b] = blocks [base]
          o = snd (serveWith defaultConfig [] (headers 1 True b <> rawFrame 5 4 1 (B.pack [0, 0, 0, 2]))) 
      in goAways o === [(1, ProtocolError)] .&&. aborted o === [(1, ConnectionEnded)] .&&. closed o
  , counterexample "after the close, a handler's inputs are ignored" $
      let [b] = blocks [base]
          (c, _) = feedAll (serveWith defaultConfig [] (headers 1 True b)) [PeerClosed]
      in null (snd (step 0 (HandlerDone (StreamId 1)) c)) .&&. null (snd (step 0 (respondOn 1 (ok Nothing)) c))
  ]

prop_limits :: Property
prop_limits = once $ conjoin
  [ counterexample "an HPACK bomb: a 4,000-octet field in the table, referred to nine times: 431; the table stays in step" $
      let -- by hand, from the static table: GET, http, /, and :authority
          start = B.pack [0x82, 0x86, 0x84, 0x01, 0x0B] <> "example.com"
          -- x-big: 4,000 octets, added to the table (index 62)
          b1 = start <> B.pack [0x40, 0x05] <> "x-big" <> B.pack [0x7F, 0xA1, 0x1E] <> B.replicate 4000 0x61
          -- x-n added (now 62, x-big 63), then x-big nine times: 36 KiB
          -- decoded from 33 octets
          b2 = start <> B.pack [0x40, 0x03] <> "x-n" <> B.pack [0x01] <> "1" <> B.replicate 9 0xBF
          -- both, which only a table that took b2's addition has there
          b3 = start <> B.pack [0xBE, 0xBF]
          o = snd (serveWith defaultConfig [] (headers 1 True b1 <> headers 3 True b2 <> headers 5 True b3))
          fieldsOn s' = [ [fieldNameBytes n | (n, _) <- fieldsToList (controlFields (requestControl h))]
                        | NewRequest (StreamId s'') h <- o, s'' == s' ]
      in B.length b2 < 40 .&&. map (\(s', _, _, _) -> s') (requests o) === [1, 5] .&&. seen o === [status 3 "431"]
         .&&. fieldsOn 5 === [["x-n", "x-big"]]
  , counterexample "after the configured number of requests, the final GOAWAY; later streams refused; the close once done" $
      let [b1, b3, b5] = blocks [base, base, base]
          config = defaultConfig { configMaxRequests = 2 }
          (c, o) = serveWith config [] (headers 1 True b1 <> headers 3 True b3 <> headers 5 True b5)
          o1 = from (c, []) [ respondOn 1 (ok (Just 0)), endOn 1, HandlerDone (StreamId 1)
                            , respondOn 3 (ok (Just 0)), endOn 3 ]
      in map (\(s, _, _, _) -> s) (requests o) === [1, 3] .&&. goAways o === [(3, NoError)]
         .&&. seen o === [SeenGoAway NoError, SeenReset 5 RefusedStream] .&&. not (closed o) .&&. closed o1
  , counterexample "off by default: a thousand requests on one connection" $
      let bs = blocks (replicate 1000 base)
          o = snd (feedAll (serveWith defaultConfig [] "")
                           (concat [ [Received (headers (2 * i + 1) True b), respondOn (2 * i + 1) (ok (Just 0)), endOn (2 * i + 1)]
                                   | (i, b) <- zip [0 ..] bs ]))
      in length (requests o) === 1000 .&&. goAways o === []
  ]
