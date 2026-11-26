{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-x-partial #-}
module StreamTests (streamTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word (Word32)
import Hyperdrive.HPACK.Decode
import Hyperdrive.HPACK.Encode
import Hyperdrive.HTTP2.Connection
import Hyperdrive.HTTP2.Frame
import Hyperdrive.Semantics.ControlData (RequestControl(..))
import Hyperdrive.Semantics.Field (fieldNameBytes, fieldValueBytes, fieldsToList)
import Hyperdrive.Semantics.Method (methodBytes)
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
run = foldl go (newConn defaultConfig 0)
  where go (c, outs) (t, i) = let (c', outs') = step t i c in (c', outs ++ outs')

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
  , counterexample "a field block on a closed stream is decoded and discarded; the table stays in step" $
      let [b1, b2, b3] = blocks [base, base ++ [("x-new", "1")], base ++ [("x-new", "1")]]
          (c, _) = run (received (headers 1 True b1))
          (c', _) = step 0 (HandlerDone (StreamId 1)) c
          (_, outs) = step 0 (Received (headers 1 True b2 <> headers 3 True b3)) c'
      in map (\(s, _, _, _) -> s) (requests outs) === [3] .&&. seen outs === []
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
