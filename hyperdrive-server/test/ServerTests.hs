{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module ServerTests (serverTests) where

import Control.Concurrent
import Control.Exception hiding (assert)
import Control.Monad (forM_, forever, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import Data.IORef
import Data.Maybe (fromJust)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperdrive.ABNF.HTTP (httpLibrary)
import Hyperdrive.ABNF.Recognize (ListForm(..), matches)
import Hyperdrive.ABNF.Types (mkRuleName)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Method (methodBytes)
import Hyperdrive.Semantics.Status (mkStatus)
import Hyperdrive.Server
import Hyperdrive.Server.Date (formatHttpDate)
import Hyperdrive.Server.Timer
import Hyperdrive.Testing.Client
import Hyperdrive.Testing.Model
import Hyperdrive.Testing.Session
import MemoryTransport
import Network.Socket
import qualified Network.Socket.ByteString as SB
import System.Timeout (timeout)
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

serverTests :: TestTree
serverTests = testGroup "hyperdrive-server"
  [ testProperty "in memory: agrees with the reference model, split at random points" prop_memory
  , testProperty "over loopback sockets: agrees with the reference model" prop_sockets
  , testProperty "timeouts by phase" prop_timeouts
  , testProperty "shutdown: idle at once, busy after the response, stuck ones killed" prop_shutdown
  , testProperty "handler failures and the time limit" prop_failures
  , testProperty "request bodies: 100 (Continue) on the wire, and bodies cut short" prop_bodies
  , testProperty "Date is an IMF-fixdate" prop_date
  , testProperty "coarse timers: expiries, re-arming, write aborts" prop_timers
  ]

------------------------------------------------------------------------------
-- a scripted application
------------------------------------------------------------------------------

-- | Answers each request with the next script, recording what it saw
-- in the reference model's terms.
scripted :: IORef [Script] -> IORef [Seen] -> Application
scripted scriptsRef seenRef _ req = do
  sc <- atomicModifyIORef' scriptsRef $ \ss -> case ss of
    (x : xs) -> (xs, x)
    []       -> ([], readAll)
  let b = requestBody req
  got <- newIORef B.empty
  (end, aborted) <-
    if bodyLength b == Just 0 || sReads sc /= Nothing then pure (Nothing, False) else do
      let loop = do
            c <- readChunk b
            unless (B.null c) (modifyIORef' got (<> c) >> loop)
      r <- try (loop >> readTrailers b)
      pure $ case r of
        Right t -> (Just (pairs t), False)
        Left IncompleteBody -> (Nothing, True)
  body <- readIORef got
  modifyIORef' seenRef (( methodBytes (requestMethod req), show (requestTarget req), requestVersion req
                        , pairs (requestHeaders req), fromIntegral <$> bodyLength b, body, end, aborted ) :)
  either (throwIO . userError . show) pure (responseFor sc)

responseFor :: Script -> Either ResponseError Response
responseFor sc =
  let st = fromJust (mkStatus (sStatus sc))
      hs = fields [("X-Test", "1")]
      len = fromIntegral (sum (map B.length (sContent sc)))
      content
        | sStatus sc `elem` [204, 304] = ContentNone
        | sDeclared sc && null (sTrailers sc) = ContentBytes (B.concat (sContent sc))
        | otherwise = ContentStream (if sDeclared sc then Just len else Nothing) (map (name . fst) (sTrailers sc))
                        (\write _ -> mapM_ write (sContent sc) >> pure (fields (sTrailers sc)))
  in mkResponse st hs content

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.6.1
-- # An origin server with a clock (as defined in Section 5.6.7) MUST
-- # generate a Date header field in all 2xx (Successful), 3xx
-- # (Redirection), and 4xx (Client Error) responses, and MAY generate a
-- # Date header field in 1xx (Informational) and 5xx (Server Error)
-- # responses.

-- | The responses with their Date fields removed, after checking each
-- the application answered has exactly one.
withoutDates :: [Resp] -> Either String [Resp]
withoutDates = traverse $ \r ->
  let dates = [ v | ("date", v) <- rsFields r ]
      rest = [ f | f@(k, _) <- rsFields r, k /= "date" ]
      rejection = lookup "x-test" (rsFields r) == Nothing
  in if rejection || length dates == 1 then Right r { rsFields = rest } else Left ("Date fields: " ++ show dates)

quietSettings :: IORef [SomeException] -> Settings
quietSettings errs = defaultSettings
  { settingsHost = Just "127.0.0.1", settingsPort = 0
  , settingsOnException = \e -> modifyIORef' errs (e :) }

genSession :: Gen ([Script], ByteString)
genSession = do
  session <- resize 4 (listOf1 ((,) <$> genReq <*> (allOrNone <$> genScript)))
  let wire = B.concat (map (render . fst) session)
  w <- frequency [(1, pure wire), (2, damage wire)]
  pure (map snd session, w)

------------------------------------------------------------------------------
-- sessions
------------------------------------------------------------------------------

prop_memory :: Property
prop_memory = withMaxSuccess 500 $ forAll genSession $ \(scripts, wire) -> forAll (splitInput wire) $ \pieces ->
  monadicIO $ do
    (seen, out, errs, shut) <- run $ do
      (t, pipe) <- memoryTransport
      mapM_ (clientSend pipe) pieces
      clientEOF pipe
      scriptsRef <- newIORef scripts
      seenRef <- newIORef []
      errs <- newIORef []
      let ctx = Context (SockAddrInet 0 0) (SockAddrInet 0 0) False
      withTimers 10000 $ \timers -> serveConnection (quietSettings errs) timers t ctx (scripted scriptsRef seenRef)
      (,,,) <$> (reverse <$> readIORef seenRef) <*> clientReceived pipe <*> readIORef errs <*> serverShutWrite pipe
    let (wantSeen, wantResps) = model scripts wire
        heads = [ m == "HEAD" | (m, _, _, _, _, _, _, _) <- wantSeen ]
    monitor (counterexample (unlines [show wire, show seen, show wantSeen, show out, show wantResps, show errs]))
    assert (seen == wantSeen)
    assert ((readResponses heads out >>= withoutDates) == Right wantResps)
    assert (null errs)
    assert shut  -- closed in stages: the server half-closed first


prop_sockets :: Property
prop_sockets = withMaxSuccess 100 $ forAll genSession $ \(scripts, wire) -> forAll (splitInput wire) $ \pieces ->
  monadicIO $ do
    (seen, out, errs) <- run $ do
      scriptsRef <- newIORef scripts
      seenRef <- newIORef []
      errs <- newIORef []
      out <- withServer (quietSettings errs) (scripted scriptsRef seenRef) $ \server ->
        withClient (serverPort server) $ \s -> do
          forM_ pieces $ \p -> SB.sendAll s p >> threadDelay 50
          shutdown s ShutdownSend
          recvAll s
      (,,) <$> (reverse <$> readIORef seenRef) <*> pure out <*> readIORef errs
    let (wantSeen, wantResps) = model scripts wire
        heads = [ m == "HEAD" | (m, _, _, _, _, _, _, _) <- wantSeen ]
    monitor (counterexample (unlines [show wire, show seen, show wantSeen, show out, show wantResps, show errs]))
    assert (seen == wantSeen)
    assert ((readResponses heads out >>= withoutDates) == Right wantResps)
    assert (null errs)

-- | Send, ignoring a server that has already closed.
send' :: Socket -> ByteString -> IO ()
send' s b = SB.sendAll s b `catch` \(_ :: IOException) -> pure ()

withClient :: PortNumber -> (Socket -> IO a) -> IO a
withClient port act = do
  addr : _ <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just "127.0.0.1") (Just (show port))
  bracket (openSocket addr) close $ \s -> connect s (addrAddress addr) >> act s

-- | Everything until the server closes (or five seconds pass).
recvAll :: Socket -> IO ByteString
recvAll s = fmap (maybe "<timed out>" id) $ timeout 5000000 $ do
  let loop acc = do
        b <- SB.recv s 65536 `catch` \(_ :: IOException) -> pure ""
        if B.null b then pure (B.concat (reverse acc)) else loop (b : acc)
  loop []

------------------------------------------------------------------------------
-- timeouts, shutdown and failures
------------------------------------------------------------------------------

fast :: Settings -> Settings
fast s = s { settingsTimerResolution = 10000, settingsTimeouts = defaultTimeouts
  { timeoutIdle = 100000, timeoutHead = 100000, timeoutBody = 100000, timeoutDiscard = 100000, timeoutLinger = 100000 } }

ok :: Application
ok _ _ = either (throwIO . userError . show) pure (mkResponse (fromJust (mkStatus 200)) emptyFields (ContentBytes "ok"))

statusOf :: ByteString -> ByteString
statusOf = B.take 3 . B.drop 9

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.5.9
-- # The 408 (Request Timeout) status code indicates that the server did
-- # not receive a complete request message within the time that it was
-- # prepared to wait.
prop_timeouts :: Property
prop_timeouts = once $ monadicIO $ do
  results <- run $ do
    errs <- newIORef []
    let settings = fast (quietSettings errs)
        slowBody _ req = do
          _ <- try (readChunk (requestBody req) >> readChunk (requestBody req)) :: IO (Either IncompleteBody ByteString)
          ok undefined req
    idle <- withServer settings ok $ \srv -> withClient (serverPort srv) recvAll
    partial <- withServer settings ok $ \srv -> withClient (serverPort srv) $ \s -> SB.sendAll s "GET / HT" >> recvAll s
    -- slowloris: bytes keep arriving, but the head never ends in time;
    -- the 408 comes while the client is still sending (after 0.1 s of
    -- the 1 s it would take)
    trickle <- withServer settings ok $ \srv -> withClient (serverPort srv) $ \s -> do
      _ <- forkIO $ forM_ (C.unpack ("GET /" <> C.replicate 45 'a')) $ \c -> send' s (C.singleton c) >> threadDelay 20000
      start <- getMonotonicSeconds
      r <- recvAll s
      end <- getMonotonicSeconds
      pure (if end - start < 0.6 then r else "too late: " <> r)
    body <- withServer settings slowBody $ \srv -> withClient (serverPort srv) $ \s ->
      SB.sendAll s "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 10\r\n\r\nabc" >> recvAll s
    -- a client that stops reading: the write times out and the
    -- connection is dropped, so only part of 64 MiB ever arrives
    let huge _ _ = either (throwIO . userError . show) pure $
          mkResponse (fromJust (mkStatus 200)) emptyFields
            (ContentStream (Just (64 * 1048576)) [] (\write _ -> mapM_ (const (write (B.replicate 65536 120))) [1 .. 1024 :: Int] >> pure emptyFields))
    stalled <- withServer settings { settingsTimeouts = (settingsTimeouts settings) { timeoutWrite = 200000 } } huge $ \srv ->
      withClient (serverPort srv) $ \s -> do
        SB.sendAll s "GET / HTTP/1.1\r\nHost: a\r\n\r\n"
        threadDelay 1000000
        B.length <$> recvAll s
    pure (idle, partial, trickle, body, stalled)
  let (idle, partial, trickle, body, stalled) = results
  monitor (counterexample (show (idle, partial, trickle, body, stalled)))
  assert (stalled > 0 && stalled < 64 * 1048576)
  assert (idle == "")
  assert (statusOf partial == "408")
  assert (statusOf trickle == "408")
  assert (statusOf body == "408")

prop_shutdown :: Property
prop_shutdown = once $ monadicIO $ do
  results <- run $ do
    errs <- newIORef []
    let settings = (fast (quietSettings errs)) { settingsTimeouts = defaultTimeouts, settingsShutdownGrace = 300000 }
        slow _ req = threadDelay 500000 >> ok undefined req
        stuck _ _ = forever (threadDelay 1000000)
    -- an idle connection closes at once
    idle <- withServer settings { settingsShutdownGrace = 3000000 } ok $ \srv -> withClient (serverPort srv) $ \s -> do
      threadDelay 50000
      _ <- forkIO (serverShutdown srv)
      timeout 1000000 (recvAll s)
    -- a busy one gets its response, with Connection: close
    busy <- withServer settings { settingsShutdownGrace = 3000000 } slow $ \srv -> withClient (serverPort srv) $ \s -> do
      SB.sendAll s "GET / HTTP/1.1\r\nHost: a\r\n\r\n"
      threadDelay 150000
      _ <- forkIO (serverShutdown srv)
      recvAll s
    -- a stuck handler is killed when the grace period ends
    start <- getMonotonicSeconds
    killed <- withServer settings stuck $ \srv -> withClient (serverPort srv) $ \s -> do
      SB.sendAll s "GET / HTTP/1.1\r\nHost: a\r\n\r\n"
      threadDelay 50000
      serverShutdown srv
      recvAll s
    end <- getMonotonicSeconds
    -- a connection that begins after shutdown began closes at once,
    -- rather than waiting out its idle timeout
    late <- do
      (t, _pipe) <- memoryTransport
      withTimers 10000 $ \timers -> do
        beginShutdown timers
        timeout 1000000 (serveConnection settings { settingsTimeouts = defaultTimeouts { timeoutLinger = 100000 } } timers t
                           (Context (SockAddrInet 0 0) (SockAddrInet 0 0) False) ok)
    pure (idle, busy, killed, end - start, late)
  let (idle, busy, killed, took, late) = results
  monitor (counterexample (show results))
  assert (idle == Just "")
  assert (statusOf busy == "200" && "connection: close" `B.isInfixOf` busy)
  assert (late == Just ())
  assert (killed == "")
  assert (took < 2)

getMonotonicSeconds :: IO Double
getMonotonicSeconds = (/ 1e9) . fromIntegral <$> getMonotonicTimeNSec

prop_failures :: Property
prop_failures = once $ monadicIO $ do
  results <- run $ do
    errs <- newIORef []
    calls <- newIORef (0 :: Int)
    let settings = quietSettings errs
        -- the first request fails before responding, the second succeeds
        flaky _ req = do
          n <- atomicModifyIORef' calls (\c -> (c + 1, c))
          if n == 0 then throwIO (userError "boom") else ok undefined req
        broken _ _ = either (throwIO . userError . show) pure $
          mkResponse (fromJust (mkStatus 200)) emptyFields
            (ContentStream Nothing [] (\write _ -> write "partial" >> throwIO (userError "stream failed")))
        short _ _ = either (throwIO . userError . show) pure $
          mkResponse (fromJust (mkStatus 200)) emptyFields
            (ContentStream (Just 10) [] (\write _ -> write "abc" >> pure emptyFields))
        sleepy _ req = threadDelay 2000000 >> ok undefined req
        undeclared _ _ = either (throwIO . userError . show) pure $
          mkResponse (fromJust (mkStatus 200)) emptyFields
            (ContentStream Nothing [] (\write _ -> write "x" >> pure (fields [("X-Late", "1")])))
        twoRequests = "GET / HTTP/1.1\r\nHost: a\r\n\r\nGET / HTTP/1.1\r\nHost: a\r\nConnection: close\r\n\r\n"
    retried <- withServer settings flaky $ \srv -> withClient (serverPort srv) $ \s -> SB.sendAll s twoRequests >> recvAll s
    cut <- withServer settings broken $ \srv -> withClient (serverPort srv) $ \s ->
      SB.sendAll s "GET / HTTP/1.1\r\nHost: a\r\n\r\n" >> recvAll s
    shortened <- withServer settings short $ \srv -> withClient (serverPort srv) $ \s ->
      SB.sendAll s "GET / HTTP/1.1\r\nHost: a\r\n\r\n" >> recvAll s
    limited <- withServer settings { settingsHandlerTimeLimit = Just 100000 } sleepy $ \srv -> withClient (serverPort srv) $ \s ->
      SB.sendAll s "GET / HTTP/1.1\r\nHost: a\r\n\r\n" >> recvAll s
    late <- withServer settings undeclared $ \srv -> withClient (serverPort srv) $ \s ->
      SB.sendAll s "GET / HTTP/1.1\r\nHost: a\r\n\r\n" >> recvAll s
    reported <- length <$> readIORef errs
    pure (retried, cut, shortened, limited, late, reported)
  let (retried, cut, shortened, limited, late, reported) = results
  monitor (counterexample (show results))
  -- S3: a 500, and the connection is kept for the next request
  assert (fmap (map rsStatus) (readResponses [False, False] retried) == Right [500, 200])
  -- after the response started: cut off, never ended normally
  assert ("7\r\npartial\r\n" `B.isSuffixOf` cut && not ("0\r\n\r\n" `B.isSuffixOf` cut))
  assert ("content-length: 10" `B.isInfixOf` shortened && "abc" `B.isSuffixOf` shortened)
  -- the handler time limit: 503 and close
  assert (statusOf limited == "503" && "connection: close" `B.isInfixOf` limited)
  -- trailers not declared: never sent, and the response cut off
  assert ("1\r\nx\r\n" `B.isSuffixOf` late && not ("x-late" `B.isInfixOf` late))
  assert (reported == 4)  -- boom, stream failed, the length mismatch, the trailers

prop_bodies :: Property
prop_bodies = once $ monadicIO $ do
  results <- run $ do
    errs <- newIORef []
    gotRef <- newIORef ""
    let settings = quietSettings errs
        -- reads chunks until the end, and answers with what it read
        echo _ req = do
          let loop acc = do
                c <- readChunk (requestBody req)
                if B.null c then pure acc else loop (acc <> c)
          r <- try (loop "")
          writeIORef gotRef (either (\IncompleteBody -> "incomplete") id r)
          ok undefined req
    -- the client waits for 100 (Continue) before sending the body
    continued <- withServer settings echo $ \srv -> withClient (serverPort srv) $ \s -> do
      SB.sendAll s "POST / HTTP/1.1\r\nHost: a\r\nExpect: 100-continue\r\nContent-Length: 5\r\nConnection: close\r\n\r\n"
      interim <- timeout 2000000 (SB.recv s 4096)
      SB.sendAll s "hello"
      rest <- recvAll s
      body <- readIORef gotRef
      pure (interim, body, statusOf rest)
    -- the client stops part way through the body
    stopped <- withServer settings echo $ \srv -> withClient (serverPort srv) $ \s -> do
      SB.sendAll s "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 10\r\n\r\nabc"
      shutdown s ShutdownSend
      _ <- recvAll s
      readIORef gotRef
    pure (continued, stopped)
  monitor (counterexample (show results))
  let ((interim, body, status), stopped) = results
  assert (interim == Just "HTTP/1.1 100 Continue\r\n\r\n" && body == "hello" && status == "200")
  assert (stopped == "incomplete")

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.7
-- # IMF-fixdate  = day-name "," SP date1 SP time-of-day SP GMT
prop_date :: Property
prop_date = conjoin
  [ once (formatHttpDate 784111777 === "Sun, 06 Nov 1994 08:49:37 GMT")   -- RFC 9110's example
  , forAll (choose (0, 253402300799 :: Int64)) $ \t ->
      counterexample (show (formatHttpDate t)) (isFixdate (formatHttpDate t))
  ]
  where
    isFixdate = either (error . show) id . matches Recipient httpLibrary "HTTP" (fromJust (mkRuleName "IMF-fixdate"))

prop_timers :: Property
prop_timers = once $ monadicIO $ do
  results <- run $ withTimers 5000 $ \timers -> do
    aborted <- newIORef False
    wakes <- newIORef []
    let wake w = atomicModifyIORef' wakes (\ws -> (w : ws, ()))
        woken = reverse <$> readIORef wakes
    withTimer timers wake (writeIORef aborted True) $ \t -> do
      -- a read deadline already passed wakes the connection, with its generation
      first <- armRead t =<< deadlineIn 0
      threadDelay 50000
      afterFirst <- woken
      -- re-armed far ahead: nothing more, and a new generation
      second <- armRead t =<< deadlineIn 10000000
      threadDelay 50000
      afterSecond <- woken
      disarm t
      -- a write past its deadline is aborted
      armWrite t =<< deadlineIn 0
      threadDelay 50000
      wasAborted <- readIORef aborted
      -- shutdown reaches this connection, and one that starts afterwards
      beginShutdown timers
      afterShutdown <- woken
      stopping <- isShuttingDown t
      late <- withTimer timers (const (pure ())) (pure ()) isShuttingDown
      pure (first, second, afterFirst, afterSecond, wasAborted, afterShutdown, stopping, late)
  monitor (counterexample (show results))
  let (first, second, afterFirst, afterSecond, wasAborted, afterShutdown, stopping, late) = results
  assert (afterFirst == [WakeTimer first] && afterSecond == afterFirst && second /= first)
  assert (wasAborted && afterShutdown == afterFirst ++ [WakeShutdown] && stopping && late)
