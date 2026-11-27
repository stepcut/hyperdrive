{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | The HTTP/2 driver (docs/http2-driver-design.md), over loopback
-- sockets with the test client of "H2Client", and in memory where the
-- client must stop reading.
module H2Tests (h2Tests) where

import Control.Concurrent
import Control.Exception hiding (assert)
import Control.Monad (forM_, forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import H2Client
import Hyperdrive.HPACK.Encode (encodeBlock, newEncoder)
import Hyperdrive.HTTP2.Connection (connectionPreface)
import Hyperdrive.HTTP2.Frame
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Method (methodBytes)
import Hyperdrive.Semantics.Status (mkStatus)
import Hyperdrive.Semantics.Target (Target(..))
import Hyperdrive.Semantics.URI (pathBytes)
import Hyperdrive.Server
import MemoryTransport
import Network.Socket (SockAddr(..))
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import System.Timeout (timeout)
import Test.QuickCheck hiding (Result, collect)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

h2Tests :: TestTree
h2Tests = testGroup "HTTP/2 over h2c (docs/http2-driver-design.md)"
  [ testProperty "requests and responses of each kind" prop_kinds
  , testProperty "many streams at once: each request's content comes back on its own stream" prop_concurrent
  , testProperty "handler failures: 500 before the response, RST_STREAM after" prop_failures
  , testProperty "a client's reset: the handler's read throws ConnectionClosed; the connection goes on" prop_reset
  , testProperty "graceful shutdown: GOAWAY, PING, the final GOAWAY, the response, the close" prop_shutdown
  , testProperty "the core's timers: a shutdown PING unanswered, the final GOAWAY after 1 s" prop_coreTimer
  , testProperty "the grace period's end: CANCEL, GOAWAY, the handler killed" prop_forced
  , testProperty "a dead connection's handlers are killed after the grace period (C2)" prop_deadHandlers
  , testProperty "which protocol: the preface in pieces; h2c off" prop_choice
  , testProperty "a client that stops reading: the connection ends after the write timeout" prop_stopReading
  ]

------------------------------------------------------------------------------
-- the application
------------------------------------------------------------------------------

-- What handlers saw, for the tests to look at.
data Log = Log
  { logReadFailure :: IORef (Maybe String)  -- ^ how a read on a reset stream failed
  , logStuck       :: MVar ()               -- ^ filled when a stuck handler starts
  , logKilled      :: MVar ()               -- ^ filled when a stuck handler is ended
  , logFile        :: FilePath
  }

large :: ByteString
large = B.pack (take (1024 * 1024) (cycle [0 .. 250]))

app :: Log -> Application
app lg _ req = case (methodBytes (requestMethod req), path (requestTarget req)) of
  (m, "/") | m `elem` ["GET", "HEAD"] -> respond 200 [] (ContentBytes "hello")
  ("GET", "/large") -> respond 200 [] (ContentBytes large)
  ("POST", "/echo") -> do
    body <- readAll
    respond 200 [] (ContentBytes body)
  ("GET", "/trailers") -> respond 200 [] (ContentStream Nothing [name "x-sum"]
    (\write _ -> write "abc" >> write "def" >> pure (fields [("x-sum", "6")])))
  ("GET", "/file") -> respond 200 [] (ContentFile (logFile lg) Nothing)
  ("GET", "/fail-before") -> throwIO (userError "failed before responding")
  ("GET", "/fail-after") -> respond 200 [] (ContentStream Nothing []
    (\write _ -> write "partial" >> throwIO (userError "failed after responding")))
  ("GET", "/slow") -> threadDelay 300000 >> respond 200 [] (ContentBytes "slow")
  ("GET", "/stuck") -> (putMVar (logStuck lg) () >> forever (threadDelay 1000000))
                         `finally` putMVar (logKilled lg) ()
  ("POST", "/read-until-reset") -> do
    r <- try (forever (readChunk (requestBody req)))
    case r of
      Left (e :: SomeException) -> writeIORef (logReadFailure lg) (Just (show e))
      Right () -> pure ()
    respond 200 [] (ContentBytes "")
  _ -> respond 404 [] (ContentBytes "")
  where
    path (TargetURI _ _ p _) = pathBytes p
    path _ = ""
    readAll = do
      let loop acc = do
            c <- readChunk (requestBody req)
            if B.null c then pure (B.concat (reverse acc)) else loop (c : acc)
      loop []
    respond code hs c = either (throwIO . userError . show) pure (mkResponse (fromJust (mkStatus code)) (fields hs) c)

fields :: [(ByteString, ByteString)] -> Fields
fields fs = fieldsFromList [(name n, either (error . show) id (mkFieldValue v)) | (n, v) <- fs]

name :: ByteString -> FieldName
name = fromJust . mkFieldName

-- | A server with the test application, and what it reported.
withH2 :: (Settings -> Settings) -> (Log -> Server -> IORef [SomeException] -> IO a) -> IO a
withH2 adjust act = do
  tmp <- getTemporaryDirectory
  (file, h) <- openBinaryTempFile tmp "h2-test"
  B.hPut h (B.take 100000 large) >> hClose h
  lg <- Log <$> newIORef Nothing <*> newEmptyMVar <*> newEmptyMVar <*> pure file
  errs <- newIORef []
  let settings = adjust defaultSettings
        { settingsHost = Just "127.0.0.1", settingsPort = 0, settingsTimerResolution = 10000
        , settingsOnException = \e -> atomicModifyIORef' errs (\es -> (e : es, ())) }
  (withServer settings (app lg) $ \srv -> act lg srv errs) `finally` removeFile file

request :: ByteString -> ByteString -> [(ByteString, ByteString)]
request m p = [(":method", m), (":scheme", "http"), (":authority", "localhost"), (":path", p)]

status :: Result -> Maybe ByteString
status = lookup ":status" . resHeaders

------------------------------------------------------------------------------
-- the properties
------------------------------------------------------------------------------

prop_kinds :: Property
prop_kinds = once $ monadicIO $ do
  (rs, errs) <- run $ withH2 id $ \_ srv errs -> do
    c <- connect (serverPort srv)
    sendRequest c 1 (request "GET" "/") True
    sendRequest c 3 (request "HEAD" "/") True
    sendRequest c 5 (request "GET" "/large") True
    sendRequest c 7 (request "POST" "/echo") False
    sendRequest c 9 (request "GET" "/trailers") True
    sendRequest c 11 (request "GET" "/file") True
    sendRequest c 13 (request "GET" "/nothing-here") True
    _ <- forkIO (sendData c 7 (B.replicate 3000000 0x41) True)
    (rs, _) <- collect c [1, 3, 5, 7, 9, 11, 13]
    closeClient c
    (,) rs <$> readIORef errs
  let r s = rs M.! s
  monitor (counterexample (show (M.map (\x -> x { resBody = B.take 40 (resBody x) }) rs, errs)))
  assert (null errs)
  assert (all ((== Nothing) . resReset) (M.elems rs))
  assert (status (r 1) == Just "200" && resBody (r 1) == "hello" && isJust (lookup "date" (resHeaders (r 1))))
  -- HEAD: the length of what GET would send, and no content
  assert (status (r 3) == Just "200" && lookup "content-length" (resHeaders (r 3)) == Just "5" && B.null (resBody (r 3)))
  assert (resBody (r 5) == large)
  assert (resBody (r 7) == B.replicate 3000000 0x41)
  assert (resBody (r 9) == "abcdef" && resTrailers (r 9) == [("x-sum", "6")])
  assert (resBody (r 11) == B.take 100000 large)
  assert (status (r 13) == Just "404")

prop_concurrent :: Property
prop_concurrent = withMaxSuccess 10 $ forAll (vectorOf 30 (choose (0, 70000))) $ \sizes -> monadicIO $ do
  let streams = [ (2 * i + 1, B.replicate n (fromIntegral i)) | (i, n) <- zip [0 ..] sizes ] :: [(Word32, ByteString)]
  (rs, errs) <- run $ withH2 id $ \_ srv errs -> do
    c <- connect (serverPort srv)
    forM_ streams $ \(s, _) -> sendRequest c s (request "POST" "/echo") False
    -- every upload at once, each within the windows
    forM_ streams $ \(s, d) -> forkIO (sendData c s d True)
    (rs, _) <- collect c (map fst streams)
    closeClient c
    (,) rs <$> readIORef errs
  monitor (counterexample (show (M.map (\x -> (B.length (resBody x), resReset x)) rs, errs)))
  assert (null errs)
  assert (and [ fmap (\r -> (resBody r, resReset r)) (M.lookup s rs) == Just (d, Nothing) | (s, d) <- streams ])

prop_failures :: Property
prop_failures = once $ monadicIO $ do
  (rs, errs) <- run $ withH2 id $ \_ srv errs -> do
    c <- connect (serverPort srv)
    sendRequest c 1 (request "GET" "/fail-before") True
    sendRequest c 3 (request "GET" "/fail-after") True
    sendRequest c 5 (request "GET" "/") True
    (rs, _) <- collect c [1, 3, 5]
    closeClient c
    (,) rs <$> readIORef errs
  monitor (counterexample (show (rs, errs)))
  assert (status (rs M.! 1) == Just "500")
  assert (status (rs M.! 3) == Just "200" && resReset (rs M.! 3) == Just InternalError)
  -- the connection serves the other streams
  assert (resBody (rs M.! 5) == "hello")
  assert (length errs == 2)

prop_reset :: Property
prop_reset = once $ monadicIO $ do
  (failure, r3) <- run $ withH2 id $ \lg srv _ -> do
    c <- connect (serverPort srv)
    sendRequest c 1 (request "POST" "/read-until-reset") False
    sendData c 1 "some" False
    threadDelay 50000
    sendRaw c [renderRstStream (StreamId 1) Cancel]
    failure <- waitFor 2000000 (readIORef (logReadFailure lg))
    sendRequest c 3 (request "GET" "/") True
    (rs, _) <- collect c [3]
    closeClient c
    pure (failure, rs M.! 3)
  monitor (counterexample (show (failure, r3)))
  assert (failure == Just "ConnectionClosed")
  assert (resBody r3 == "hello")

-- = https://www.rfc-editor.org/rfc/rfc9113#section-6.8
-- # A server that is attempting to gracefully shut down a
-- # connection SHOULD send an initial GOAWAY frame with the last stream
-- # identifier set to 2^31-1 and a NO_ERROR code.
prop_shutdown :: Property
prop_shutdown = once $ monadicIO $ do
  (first, rs, rest) <- run $ withH2 id $ \_ srv _ -> do
    -- PINGs are not answered by the client: the test answers the one
    -- that matters itself
    c <- connectWith (serverPort srv) False []
    sendRequest c 1 (request "GET" "/slow") True
    threadDelay 50000
    _ <- forkIO (serverShutdown srv)
    e1 <- nextEvent c
    e2 <- nextEvent c
    case e2 of
      Ping d -> sendRaw c [renderPing True d]
      _ -> pure ()
    e3 <- nextEvent c
    (rs, rest) <- collect c [1]
    end <- nextEvent c
    pure ([e1, e2, e3], rs, rest ++ [end])
  monitor (counterexample (show (first, rs, rest)))
  assert (first == [GoAway 2147483647 NoError, Ping "shutdown", GoAway 1 NoError])
  assert (resBody (rs M.! 1) == "slow")
  assert (rest == [Closed])

prop_coreTimer :: Property
prop_coreTimer = once $ monadicIO $ do
  -- a handler that never answers, so that only the timer can bring the
  -- final GOAWAY; the forced stage comes after it
  (events, took) <- run $ withH2 (\s -> s { settingsShutdownGrace = 1500000 }) $ \lg srv _ -> do
    c <- connectWith (serverPort srv) False []
    sendRequest c 1 (request "GET" "/stuck") True
    takeMVar (logStuck lg)
    threadDelay 50000
    _ <- forkIO (serverShutdown srv)
    e1 <- nextEvent c
    e2 <- nextEvent c
    start <- getMonotonicTimeNSec
    e3 <- nextEvent c
    end <- getMonotonicTimeNSec
    pure ([e1, e2, e3], fromIntegral (end - start) / 1e9 :: Double)
  monitor (counterexample (show (events, took)))
  assert (events == [GoAway 2147483647 NoError, Ping "shutdown", GoAway 1 NoError])
  assert (took > 0.8 && took < 2)

prop_forced :: Property
prop_forced = once $ monadicIO $ do
  (events, killed, took) <- run $ withH2 (\s -> s { settingsShutdownGrace = 300000 }) $ \lg srv _ -> do
    c <- connect (serverPort srv)
    sendRequest c 1 (request "GET" "/stuck") True
    takeMVar (logStuck lg)
    start <- getMonotonicTimeNSec
    serverShutdown srv
    end <- getMonotonicTimeNSec
    (rs, before) <- collect c [1]
    after <- sequence [nextEvent c, nextEvent c]
    killed <- timeout 2000000 (readMVar (logKilled lg))
    pure ((before, resReset (rs M.! 1), after), killed, fromIntegral (end - start) / 1e9 :: Double)
  monitor (counterexample (show (events, killed, took)))
  -- the graceful stage's two GOAWAYs, then the stream reset with CANCEL,
  -- a last GOAWAY, and the close
  assert (events == ([GoAway 2147483647 NoError, GoAway 1 NoError], Just Cancel, [GoAway 1 NoError, Closed]))
  assert (killed == Just ())
  assert (took < 2)

prop_deadHandlers :: Property
prop_deadHandlers = once $ monadicIO $ do
  killed <- run $ withH2 (\s -> s { settingsHandlerGrace = 200000 }) $ \lg srv _ -> do
    c <- connect (serverPort srv)
    sendRequest c 1 (request "GET" "/stuck") True
    takeMVar (logStuck lg)
    closeClient c
    timeout 2000000 (readMVar (logKilled lg))
  assert (killed == Just ())

prop_choice :: Property
prop_choice = once $ monadicIO $ do
  (pieces, off) <- run $ do
    -- the preface a few octets at a time is still HTTP/2
    pieces <- withH2 id $ \_ srv _ -> do
      c <- connectRaw (serverPort srv)
      forM_ (chunks 5 connectionPreface) $ \p -> SB.sendAll c p >> threadDelay 20000
      let (block, _) = encodeBlock (newEncoder 4096) (request "GET" "/")
      SB.sendAll c (B.concat (renderSettings [] : renderHeaders 16384 (StreamId 1) True block))
      out <- recvUntilClosed c 500000
      S.close c
      pure out
    -- h2c off: the preface is an HTTP/1.1 request the server refuses
    off <- withH2 (\s -> s { settingsHTTP2 = Nothing }) $ \_ srv _ -> do
      c <- connectRaw (serverPort srv)
      SB.sendAll c (connectionPreface <> renderSettings [])
      out <- recvUntilClosed c 2000000
      S.close c
      pure out
    pure (pieces, off)
  monitor (counterexample (show (B.take 200 pieces, B.take 200 off)))
  -- the server's SETTINGS first, and the response's DATA
  assert (B.index pieces 3 == 0x4 && "hello" `B.isInfixOf` pieces)
  assert ("HTTP/1.1 " `B.isPrefixOf` off)

prop_stopReading :: Property
prop_stopReading = once $ monadicIO $ do
  (ended, took) <- run $ do
    (t, pipe) <- memoryTransport
    let (block, _) = encodeBlock (newEncoder 4096) (request "GET" "/large")
    clientSend pipe (B.concat (connectionPreface : renderSettings [InitialWindowSize 2147483647]
                               : renderWindowUpdate (StreamId 0) 2000000000
                               : renderHeaders 16384 (StreamId 1) True block))
    -- the client takes the server's preface and a little more, then nothing
    clientTakes pipe 1000
    tmp <- getTemporaryDirectory
    lg <- Log <$> newIORef Nothing <*> newEmptyMVar <*> newEmptyMVar <*> pure tmp
    let settings = defaultSettings
          { settingsTimerResolution = 10000, settingsOnException = \_ -> pure ()
          , settingsTimeouts = defaultTimeouts { timeoutWrite = 300000, timeoutLinger = 100000 } }
    start <- getMonotonicTimeNSec
    ended <- withTimers 10000 $ \timers ->
      timeout 5000000 (serveConnection settings timers t (Context (SockAddrInet 0 0) (SockAddrInet 0 0) False) (app lg))
    end <- getMonotonicTimeNSec
    pure (ended, fromIntegral (end - start) / 1e9 :: Double)
  monitor (counterexample (show (ended, took)))
  assert (ended == Just ())
  assert (took < 2)

------------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------------

-- | Poll until the action gives something, or the time (microseconds) runs out.
waitFor :: Int -> IO (Maybe a) -> IO (Maybe a)
waitFor limit act = fmap (maybe Nothing id) $ timeout limit $ let go = act >>= maybe (threadDelay 5000 >> go) (pure . Just) in go

connectRaw :: S.PortNumber -> IO S.Socket
connectRaw port = do
  addr : _ <- S.getAddrInfo (Just S.defaultHints { S.addrSocketType = S.Stream }) (Just "127.0.0.1") (Just (show port))
  s <- S.openSocket addr
  S.connect s (S.addrAddress addr)
  pure s

-- | What the server sends until it closes, or until it has been quiet
-- for the given time (microseconds).
recvUntilClosed :: S.Socket -> Int -> IO ByteString
recvUntilClosed s quiet = go []
  where
    go acc = do
      r <- timeout quiet (SB.recv s 65536 `catch` \(_ :: IOException) -> pure B.empty)
      case r of
        Just b | not (B.null b) -> go (b : acc)
        _ -> pure (B.concat (reverse acc))

chunks :: Int -> ByteString -> [ByteString]
chunks n b
  | B.null b = []
  | otherwise = B.take n b : chunks n (B.drop n b)
