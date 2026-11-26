{-# LANGUAGE OverloadedStrings #-}
module ConnectionTests (connectionTests) where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Hyperdrive.HTTP2.Connection
import Hyperdrive.HTTP2.Frame
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

connectionTests :: TestTree
connectionTests = testGroup "The connection core: prefaces, SETTINGS, PING (C1, C10, C11)"
  [ testProperty "the server's preface: SETTINGS, then the connection window raised" prop_serverPreface
  , testProperty "the client's preface and SETTINGS, in any pieces, are acknowledged" prop_clientPreface
  , testProperty "the outputs do not depend on how the input is split" prop_split
  , testProperty "connection errors, by example" prop_errors
  , testProperty "the nuisance budget, at its boundary and refilled" prop_budget
  , testProperty "timers" prop_timers
  ]

-- run the inputs at the given times, from a new connection at time 0
run :: [(Time, Input)] -> (Conn, [Output])
run = foldl go (newConn defaultConfig 0)
  where go (c, outs) (t, i) = let (c', outs') = step t i c in (c', outs ++ outs')

-- every frame the server sent, and whether it closed
frames :: [Output] -> ([Frame], Bool)
frames outs = (parseAll (B.concat (concat [bs | Send bs <- outs])), any isClose outs)
  where
    isClose Close = True
    isClose _ = False
    parseAll b
      | B.null b = []
      | otherwise = case parseFrame 0xFFFFFF b of
          Parsed f n -> f : parseAll (B.drop n b)
          other -> error ("the server sent a bad frame: " ++ show other)

clientStart :: ByteString
clientStart = connectionPreface <> renderSettings []

-- the server's frames after its preface
replies :: [(Time, Input)] -> ([Frame], Bool)
replies inputs = let (fs, closed) = frames (snd (run inputs)) in (drop 2 fs, closed)

prop_serverPreface :: Property
prop_serverPreface = once $
  -- = https://www.rfc-editor.org/rfc/rfc9113#section-3.4
  -- # The server connection preface consists of a potentially empty
  -- # SETTINGS frame (Section 6.5) that MUST be the first frame the server
  -- # sends in the HTTP/2 connection.
  fst (frames (snd (newConn defaultConfig 0)))
    === [ SettingsFrame 3 [MaxConcurrentStreams 100, InitialWindowSize 1048576, MaxHeaderListSize 32768]
        , WindowUpdateFrame (StreamId 0) (1048576 - 65535) ]

prop_clientPreface :: Property
prop_clientPreface = forAll (pieces clientStart) $ \ps ->
  let (c, outs) = run [(0, Received p) | p <- ps]
  in drop 2 (fst (frames outs)) === [SettingsAckFrame]
     .&&. not (snd (frames outs))
     .&&. peerSettings c === peerSettings (fst (run [(0, Received clientStart)]))

-- a string cut at random points
pieces :: ByteString -> Gen [ByteString]
pieces b
  | B.null b = pure []
  | otherwise = do
      n <- choose (1, B.length b)
      (B.take n b :) <$> pieces (B.drop n b)

-- a client's frames that need no streams, valid ones mostly
genClientFrames :: Gen ByteString
genClientFrames = B.concat <$> listOf (oneof
  [ renderPing False <$> (B.pack <$> vectorOf 8 arbitrary)
  , pure (renderPing True (B.replicate 8 0))
  , renderSettings <$> elements [[], [InitialWindowSize 1000], [MaxFrameSize 20000, HeaderTableSize 0]]
  , pure renderSettingsAck ])

prop_split :: Property
prop_split = forAll genClientFrames $ \fs ->
  let input = clientStart <> fs
  in forAll (pieces input) $ \ps ->
       sendBytes (run [(0, Received p) | p <- ps]) === sendBytes (run [(0, Received input)])
  where sendBytes (_, outs) = (B.concat (concat [bs | Send bs <- outs]), length [() | Close <- outs])

goAwayCode :: ([Frame], Bool) -> Maybe ErrorCode
goAwayCode (fs, closed) = case [c | GoAwayFrame _ c _ <- fs] of
  [c] | closed -> Just c
  _ -> Nothing

prop_errors :: Property
prop_errors = once $ conjoin
  [ counterexample "an HTTP/1.1 request instead of the preface: closed, no GOAWAY" $
      replies [(0, Received "GET / HTTP/1.1\r\n\r\n")] === ([], True)
  , counterexample "a full-length preface that is wrong: closed, no GOAWAY" $
      replies [(0, Received ("PRI * HTTP/2.0\r\n\r\nXX\r\n\r\n" <> renderSettings []))] === ([], True)
  , counterexample "the start of the preface: nothing yet" $
      replies [(0, Received (B.take 10 connectionPreface))] === ([], False)
  , counterexample "a first frame other than SETTINGS" $
      goAwayCode (replies [(0, Received (connectionPreface <> renderPing False (B.replicate 8 0)))]) === Just ProtocolError
  , counterexample "a second acknowledgement" $
      goAwayCode (replies [(0, Received (clientStart <> renderSettingsAck <> renderSettingsAck))]) === Just ProtocolError
  , counterexample "a repeated setting" $
      goAwayCode (replies [(0, Received (clientStart <> renderSettings [InitialWindowSize 1, InitialWindowSize 2]))])
        === Just ProtocolError
  , counterexample "101 settings" $
      goAwayCode (replies [(0, Received (clientStart <> settingsOf 101))]) === Just ProtocolError
  , counterexample "100 settings are accepted" $
      goAwayCode (replies [(0, Received (clientStart <> settingsOf 100))]) === Nothing
  , counterexample "a frame error: a PING of 7 octets" $
      goAwayCode (replies [(0, Received (clientStart <> B.pack [0, 0, 7, 6, 0, 0, 0, 0, 0] <> B.replicate 7 0))])
        === Just FrameSizeError
  , counterexample "replies closing, every input is ignored" $
      let (c, _) = run [(0, Received "garbage, not HTTP/2...")]
      in length (snd (step 1 (Received clientStart) c)) === 0
  , counterexample "a PING is answered with the same payload, an acknowledgement is not" $
      fst (replies [(0, Received (clientStart <> renderPing False "12345678" <> renderPing True "abcdefgh"))])
        === [SettingsAckFrame, PingFrame True "12345678"]
  , counterexample "Sent for more than was emitted is a misuse" $
      let (c, outs) = newConn defaultConfig 0
          emitted = B.length (B.concat (concat [bs | Send bs <- outs]))
      in isMisuse (snd (step 0 (Sent (emitted + 1)) c)) .&&. not (isMisuse (snd (step 0 (Sent emitted) c)))
  ]
  where
    -- settings that are not repeated: unknown ones would be dropped, so
    -- the same known one is not used twice; n of them need distinct ids,
    -- so all but the six known ones are written by hand
    settingsOf n = renderSettingsRaw [(fromIntegral i, 0) | i <- [7 .. 6 + n :: Int]]
    isMisuse outs = not (null [() | ProtocolMisuse _ <- outs])

-- a SETTINGS frame with any identifiers (renderSettings writes only known ones)
renderSettingsRaw :: [(Int, Int)] -> ByteString
renderSettingsRaw ss =
  let payload = B.concat [B.pack [fromIntegral (i `div` 256), fromIntegral i, 0, 0, 0, fromIntegral v] | (i, v) <- ss]
      len = B.length payload
  in B.pack [fromIntegral (len `div` 65536), fromIntegral (len `div` 256), fromIntegral len, 4, 0, 0, 0, 0, 0] <> payload

prop_budget :: Property
prop_budget = once $ conjoin
  [ counterexample "1,000 PINGs at once are answered" $
      goAwayCode (replies [(0, Received (clientStart' <> pings 999))]) === Nothing
  , counterexample "the 1,001st is too many" $
      goAwayCode (replies [(0, Received (clientStart' <> pings 1000))]) === Just EnhanceYourCalm
  , counterexample "a second later, 100 more are allowed" $
      goAwayCode (replies [(0, Received (clientStart' <> pings 999)), (1000, Received (pings 100))]) === Nothing
  , counterexample "but not 101" $
      goAwayCode (replies [(0, Received (clientStart' <> pings 999)), (1000, Received (pings 101))]) === Just EnhanceYourCalm
  , counterexample "a SETTINGS of 100 settings costs 95 (one, and one for each after six)" $
      goAwayCode (replies [(0, Received (clientStart' <> settings100 <> pings 904))]) === Nothing
      .&&. goAwayCode (replies [(0, Received (clientStart' <> settings100 <> pings 905))]) === Just EnhanceYourCalm
  , counterexample "a long quiet time refills the budget only to its burst" $
      goAwayCode (replies [(0, Received clientStart'), (100000, Received (pings 1000))]) === Nothing
      .&&. goAwayCode (replies [(0, Received clientStart'), (100000, Received (pings 1001))]) === Just EnhanceYourCalm
  , counterexample "an unknown frame costs one too" $
      goAwayCode (replies [(0, Received (clientStart' <> pings 999 <> B.pack [0, 0, 0, 0xFA, 0, 0, 0, 0, 0]))])
        === Just EnhanceYourCalm
  ]
  where
    -- the client's SETTINGS costs one: 999 PINGs then use up the burst
    clientStart' = clientStart
    pings n = B.concat (replicate n (renderPing False (B.replicate 8 0)))
    settings100 = renderSettingsRaw [(i, 0) | i <- [7 .. 106]]

prop_timers :: Property
prop_timers = once $ conjoin
  [ counterexample "a new connection waits 30 s for the client's preface and SETTINGS" $
      nextDeadline (fst (newConn defaultConfig 0)) === Just 30000
  , counterexample "the preface never comes: at 30 s, closed without a GOAWAY" $
      replies [(0, Received (B.take 5 connectionPreface)), (30000, TimedOut)] === ([], True)
  , counterexample "once the client has started, 10 s for it to acknowledge our SETTINGS" $
      nextDeadline (fst (run [(5000, Received clientStart)])) === Just 15000
  , counterexample "our SETTINGS unacknowledged after 10 s: SETTINGS_TIMEOUT" $
      goAwayCode (replies [(0, Received clientStart), (10000, TimedOut)]) === Just SettingsTimeout
  , counterexample "acknowledged in time: no deadline left" $
      nextDeadline (fst (run [(0, Received (clientStart <> renderSettingsAck))])) === Nothing
  , counterexample "TimedOut before the deadline is a misuse" $
      let (c, _) = newConn defaultConfig 0
      in not (null [() | ProtocolMisuse _ <- snd (step 9999 TimedOut c)])
  ]
