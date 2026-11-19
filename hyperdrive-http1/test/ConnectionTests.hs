{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module ConnectionTests (connectionTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.List (sort)
import Data.Maybe (fromJust)
import Hyperdrive.HTTP1.Chunked
import Hyperdrive.HTTP1.Connection
import Hyperdrive.HTTP1.Framing (RequestFraming(..), framingErrorStatus, requestFraming)
import Hyperdrive.HTTP1.Head (HeadResult(..), RawRequestHead, defaultLimits, headErrorStatus, parseRequestHead)
import qualified Hyperdrive.HTTP1.Head as Head
import Hyperdrive.HTTP1.Target (reconstructTarget, targetErrorStatus)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.FieldSyntax (Comments(..), splitList)
import Hyperdrive.Semantics.Message (HttpVersion(..), isProtocolField)
import Hyperdrive.Semantics.Method (methodBytes)
import Hyperdrive.Semantics.Status (Status, mkStatus, statusCode, pattern BadRequest)
import Hyperdrive.Semantics.Target (Target(..))
import Hyperdrive.Semantics.URI
import Numeric (readHex, showHex)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

connectionTests :: TestTree
connectionTests = testGroup "Connection (step)"
  [ testProperty "pipelined sessions: each request served, in order, and read back by a client" prop_sessions
  , testProperty "the same however the input is split" prop_splitting
  , testProperty "damaged input: responses stay well formed, and splitting does not matter" prop_damaged
  , testProperty "agrees with a reference model reading the whole input at once" prop_model
  , testProperty "persistence and closing" prop_persistence
  , testProperty "100 (Continue)" prop_continue
  , testProperty "requests the core rejects itself" prop_rejections
  , testProperty "request bodies" prop_bodies
  , testProperty "responses" prop_responses
  , testProperty "timeouts, shutdown and misuse" prop_events
  ]

------------------------------------------------------------------------------
-- requests, as a client sends them
------------------------------------------------------------------------------

data Req = Req
  { rMethod  :: ByteString
  , rPath    :: ByteString
  , rMinor   :: Int
  , rFields  :: [(ByteString, ByteString)]  -- ^ fields the application sees
  , rExpect  :: Bool
  , rClose   :: Bool                         -- ^ Connection: close
  , rBody    :: BodySpec
  } deriving (Show)

data BodySpec = NoBodySpec | LengthBody ByteString | ChunkedBody [ByteString] [(ByteString, ByteString)]
  deriving (Show)

bodyBytes :: BodySpec -> ByteString
bodyBytes NoBodySpec          = ""
bodyBytes (LengthBody b)      = b
bodyBytes (ChunkedBody cs _)  = B.concat cs

render :: Req -> ByteString
render r = B.concat $
  [ rMethod r, " ", rPath r, " HTTP/1.", C.pack (show (rMinor r)), "\r\n", "Host: a.example\r\n" ]
  ++ [ n <> ": " <> v <> "\r\n" | (n, v) <- rFields r ]
  ++ [ "Expect: 100-continue\r\n" | rExpect r ]
  ++ [ "Connection: close\r\n" | rClose r ]
  ++ case rBody r of
       NoBodySpec -> ["\r\n"]
       LengthBody b -> ["Content-Length: ", C.pack (show (B.length b)), "\r\n\r\n", b]
       ChunkedBody cs ts ->
         ["Transfer-Encoding: chunked\r\n\r\n"]
         ++ concat [ [C.pack (showHex (B.length c) ""), "\r\n", c, "\r\n"] | c <- cs, not (B.null c) ]
         ++ ["0\r\n"] ++ [ n <> ": " <> v <> "\r\n" | (n, v) <- ts ] ++ ["\r\n"]

genReq :: Gen Req
genReq = do
  minor <- frequency [(5, pure 1), (1, pure 0)]
  method <- elements ["GET", "POST", "PUT", "HEAD", "DELETE", "X-Custom"]
  path <- elements ["/", "/a/b", "/x?y=1", "/%41"]
  fs <- listOf (elements [("Accept", "*/*"), ("X-A", "1"), ("Cookie", "a=b"), ("X-Empty", "")])
  expect <- frequency [(5, pure False), (1, pure True)]
  close <- frequency [(6, pure False), (1, pure True)]
  body <- frequency
    [ (3, pure NoBodySpec)
    , (2, LengthBody <$> bytes)
    , (if minor == 1 then 2 else 0, ChunkedBody <$> listOf bytes <*> elements [[], [("X-T", "1")]]) ]
  pure (Req method path minor fs (expect && minor == 1) close body)
  where bytes = resize 40 (B.pack <$> listOf arbitrary)

-- | The input as the network delivers it: in pieces.
splitInput :: ByteString -> Gen [ByteString]
splitInput s = do
  cuts <- sort <$> listOf (choose (0, B.length s))
  pure (filter (not . B.null) (zipWith (\a b -> B.take (b - a) (B.drop a s)) (0 : cuts) (cuts ++ [B.length s])))

------------------------------------------------------------------------------
-- the application, as a script
------------------------------------------------------------------------------

data Script = Script
  { sReads    :: Maybe Int   -- ^ Nothing: read the whole body; Just k: ask for at most k chunks
  , sStatus   :: Int
  , sContent  :: [ByteString]
  , sDeclared :: Bool        -- ^ give the length in advance
  , sTrailers :: [(ByteString, ByteString)]
  } deriving (Show)

genScript :: Gen Script
genScript = do
  reads' <- frequency [(4, pure Nothing), (1, Just <$> choose (0, 2))]
  status <- elements [200, 200, 201, 204, 304, 404, 500]
  content <- if status `elem` [204, 304] then pure [] else listOf (resize 40 (B.pack <$> listOf arbitrary))
  declared <- arbitrary
  trailers <- if status `elem` [204, 304] then pure [] else frequency [(3, pure []), (1, pure [("X-Checksum", "abc")])]
  pure (Script reads' status content declared trailers)

readAll :: Script
readAll = Script Nothing 200 ["ok"] True []

------------------------------------------------------------------------------
-- the driver, simulated
------------------------------------------------------------------------------

data Served = Served
  { srvHead    :: RequestHead
  , srvBody    :: ByteString
  , srvEnd     :: Maybe Fields
  , srvAborted :: Bool
  } deriving (Show)

data Sim = Sim
  { simConn    :: Conn
  , simPieces  :: [ByteString]
  , simEOF     :: Bool
  , simScripts :: [Script]
  , simEvents  :: [Output]   -- ^ outputs not yet acted on, oldest first
  , simSent    :: [ByteString]
  , simClosed  :: Bool
  , simServed  :: [Served]   -- ^ newest first
  , simMisuse  :: [Misuse]
  , simAfterClose :: Bool    -- ^ an output followed Close
  }

data Run = Run
  { runServed  :: [Served]
  , runSent    :: ByteString
  , runClosed  :: Bool
  , runMisuse  :: [Misuse]
  , runAfterClose :: Bool
  } deriving (Show)

simulate :: Config -> [ByteString] -> [Script] -> Run
simulate config pieces scripts =
  let s = drive (Sim (newConn config) pieces False scripts [NeedInput] [] False [] [] False)
  in Run (reverse (simServed s)) (B.concat (reverse (simSent s))) (simClosed s) (reverse (simMisuse s)) (simAfterClose s)

stepSim :: Input -> Sim -> Sim
stepSim i s
  | simClosed s = s
  | otherwise = let (c, outs) = step i (simConn s) in foldl apply s { simConn = c } outs
  where
    apply s' o
      | simClosed s' = s' { simAfterClose = True }
      | otherwise = case o of
          Send bs          -> s' { simSent = reverse bs ++ simSent s' }
          Close            -> s' { simClosed = True }
          ProtocolMisuse m -> s' { simMisuse = m : simMisuse s' }
          _                -> s' { simEvents = simEvents s' ++ [o] }

feed :: Sim -> Sim
feed s = case simPieces s of
  (p : ps) -> stepSim (Received p) s { simPieces = ps }
  []       -> stepSim PeerClosed s { simEOF = True }

drive :: Sim -> Sim
drive s
  | simClosed s = s
  | otherwise = case simEvents s of
      (NeedInput : rest) | simEOF s  -> s { simEvents = rest }
                         | otherwise -> drive (feed s { simEvents = rest })
      (NewRequest h : rest) -> drive (serve h s { simEvents = rest })
      (_ : rest) -> drive s { simEvents = rest }
      [] -> s

serve :: RequestHead -> Sim -> Sim
serve h s0 =
  let (script, scripts) = case simScripts s0 of
        (x : xs) -> (x, xs)
        []       -> (readAll, [])
      s1 = s0 { simScripts = scripts, simServed = Served h "" Nothing False : simServed s0 }
      wants = if reqBodyLength h == Just 0 then 0 else maybe maxBound id (sReads script)
      s2 = readBody wants s1
  in respondWith script s2

readBody :: Int -> Sim -> Sim
readBody 0 s = s
readBody k s
  | simClosed s = s
  | otherwise = case awaitBody (stepSim WantBody s) of
      (s', True)  -> s'
      (s', False) -> readBody (k - 1) s'

-- | The answer to a WantBody, and whether the body is finished.
awaitBody :: Sim -> (Sim, Bool)
-- Outputs that came before a Close are still acted on.
awaitBody s = case simEvents s of
      (NeedInput : rest)
        | simEOF s || simClosed s -> (s { simEvents = rest }, True)
        | otherwise -> awaitBody (feed s { simEvents = rest })
      (RequestData b : RequestEnd t : rest) -> (end t (addData b s { simEvents = rest }), True)
      (RequestData b : rest) -> (addData b s { simEvents = rest }, False)
      (RequestEnd t : rest) -> (end t s { simEvents = rest }, True)
      (RequestAborted : rest) -> (modify (\v -> v { srvAborted = True }) s { simEvents = rest }, True)
      _ -> (s, True)
  where
    addData b = modify (\v -> v { srvBody = srvBody v <> b })
    end t = modify (\v -> v { srvEnd = Just t })
    modify f s' = case simServed s' of
      (v : vs) -> s' { simServed = f v : vs }
      []       -> s'

respondWith :: Script -> Sim -> Sim
respondWith sc s =
  let len = fromIntegral (sum (map B.length (sContent sc)))
      hasContent = sStatus sc `notElem` [204, 304]
      h = ResponseHead (fromJust (mkStatus (sStatus sc))) (fields [("X-Test", "1")]) hasContent
            (if sDeclared sc then Just len else Nothing) (map (name . fst) (sTrailers sc))
      s1 = stepSim (Respond h) s
      s2 = foldl (\s' c -> stepSim (WriteBody c) s') s1 (sContent sc)
  in stepSim (EndBody (fields (sTrailers sc))) s2

fields :: [(ByteString, ByteString)] -> Fields
fields fs = fieldsFromList [ (name n, either (error . show) id (mkFieldValue v)) | (n, v) <- fs ]

name :: ByteString -> FieldName
name = fromJust . mkFieldName

------------------------------------------------------------------------------
-- a client reading the responses
------------------------------------------------------------------------------

data Resp = Resp
  { rsStatus   :: Int
  , rsFields   :: [(ByteString, ByteString)]  -- ^ names in lower case
  , rsBody     :: ByteString
  , rsTrailers :: [(ByteString, ByteString)]
  , rsInterim  :: Int                          -- ^ 100 (Continue) responses before it
  } deriving (Eq, Show)

-- | Read responses as RFC 9112 Section 6.3 tells a client to, given
-- whether each answers a HEAD request. Written without the server's
-- code.
readResponses :: [Bool] -> ByteString -> Either String [Resp]
readResponses heads = go heads 0
  where
    go hs interim s
      | B.null s = Right []
      | otherwise = do
          (status, fs, rest) <- readHead s
          if status == 100 then go hs (interim + 1) rest else do
            let isHead = case hs of { (x : _) -> x; [] -> False }
                header n = lookup n fs
            (body, ts, rest') <-
              if isHead || status `elem` [204, 304] then Right ("", [], rest)
              else if fmap (map toLower . C.unpack) (header "transfer-encoding") == Just "chunked" then chunked rest
              else case header "content-length" of
                Just n | [(len, "")] <- reads (C.unpack n) ->
                  if B.length rest < len then Left "short body" else Right (B.take len rest, [], B.drop len rest)
                Just _ -> Left "bad Content-Length"
                Nothing -> Right (rest, [], "")
            (Resp status fs body ts interim :) <$> go (drop 1 hs) 0 rest'
    readHead s = case B.breakSubstring "\r\n\r\n" s of
      (_, r) | B.null r -> Left ("no end of head: " ++ show s)
      (h, r) -> case C.lines (C.filter (/= '\r') h) of
        (line : fls) | Just st <- statusOf line -> Right (st, map field fls, B.drop 4 r)
        _ -> Left ("bad status line: " ++ show h)
    statusOf line = case C.words line of
      ("HTTP/1.1" : code : _) | [(n, "")] <- reads (C.unpack code), " " `B.isPrefixOf` B.drop 12 line -> Just n
      _ -> Nothing
    field l = let (n, v) = C.break (== ':') l in (C.map toLower n, C.dropWhile (== ' ') (B.drop 1 v))
    chunked s = case B.breakSubstring "\r\n" s of
      (line, r) | B.null r -> Left "no chunk size"
                | [(n, "")] <- readHex (C.unpack line) ->
                    if n == 0 then trailers (B.drop 2 r) []
                    else let d = B.drop 2 r in
                      if B.take 2 (B.drop n d) /= "\r\n" then Left "bad chunk end"
                      else (\(b, ts, rest) -> (B.take n d <> b, ts, rest)) <$> chunked (B.drop (n + 2) d)
                | otherwise -> Left ("bad chunk size " ++ show line)
    trailers s acc = case B.breakSubstring "\r\n" s of
      (_, r) | B.null r -> Left "no end of trailers"
      ("", r) -> Right ("", reverse acc, B.drop 2 r)
      (l, r) -> trailers (B.drop 2 r) (field l : acc)

------------------------------------------------------------------------------
-- sessions
------------------------------------------------------------------------------

-- | How many of the requests are served before the connection closes,
-- by D5 and D6, if the client sends all of them at once.
expectedServed :: [(Req, Script)] -> Int
expectedServed = go 1
  where
    go n ((r, sc) : rest)
      | closes r sc || null rest = n
      | otherwise = go (n + 1) rest
    go n [] = n - 1
    closes r sc =
      rMinor r == 0 || rClose r
      || hasBody r && rExpect r && sReads sc == Just 0
      || untilClose r sc
    -- a chunked body may turn out to be empty, but not before it is read
    hasBody r = case rBody r of
      NoBodySpec   -> False
      LengthBody b -> not (B.null b)
      ChunkedBody _ _ -> True
    -- HTTP/1.0 cannot have chunked, so an unknown length is close-delimited
    untilClose r sc = rMinor r == 0 && not (sDeclared sc) && sStatus sc `notElem` [204, 304] && rMethod r /= "HEAD"

prop_sessions :: Property
prop_sessions = withMaxSuccess 1000 $
  forAll (resize 4 (listOf1 ((,) <$> genReq <*> genScript))) $ \session ->
  forAll (splitInput (B.concat (map (render . fst) session))) $ \pieces ->
    let run = simulate defaultConfig pieces (map snd session)
        n = expectedServed session
        served = runServed run
        expected = take n session
        heads = [ rMethod r == "HEAD" | (r, _) <- expected ]
    in counterexample (show (map fst session)) $ counterexample (show run) $
       cover 20 (length session > 1) "pipelined" $
       conjoin
         [ length served === n
         , conjoin (zipWith checkRequest expected served)
         , runClosed run === True
         , runMisuse run === []
         , runAfterClose run === False
         , case readResponses heads (runSent run) of
             Left e -> counterexample e False
             Right rs -> length rs === n .&&. conjoin (zipWith checkResponse expected rs)
         ]

checkRequest :: (Req, Script) -> Served -> Property
checkRequest (r, sc) v =
  let h = srvHead v
      body = bodyBytes (rBody r)
      readsAll = sReads sc == Nothing
  in conjoin
       [ methodBytes (reqMethod h) === rMethod r
       , reqVersion h === HttpVersion 1 (fromIntegral (rMinor r))
       , reqTarget h === targetOf (rPath r)
       , [ (fieldNameBytes k, fieldValueBytes x) | (k, x) <- fieldsToList (reqFields h) ]
           === [ (C.map toLower k, x) | (k, x) <- rFields r ] ++ [ ("expect", "100-continue") | rExpect r ]
       , reqBodyLength h === case rBody r of
           ChunkedBody _ _ -> Nothing
           _               -> Just (fromIntegral (B.length body))
       , if readsAll then srvBody v === body else property (srvBody v `B.isPrefixOf` body)
       , if readsAll && not (B.null body)
           then fmap (map (\(k, x) -> (fieldNameBytes k, fieldValueBytes x)) . fieldsToList) (srvEnd v)
                  === Just (case rBody r of { ChunkedBody _ ts -> [ (C.map toLower k, x) | (k, x) <- ts ]; _ -> [] })
           else property True
       , srvAborted v === False
       ]

targetOf :: ByteString -> Target
targetOf p = let (path, q) = C.break (== '?') p
             in TargetURI HTTP (either (error . show) id (parseAuthority "a.example")) (fromJust (mkPath path))
                  (if B.null q then Nothing else mkQuery (B.drop 1 q))

checkResponse :: (Req, Script) -> Resp -> Property
checkResponse (r, sc) rs =
  let noBody = rMethod r == "HEAD" || sStatus sc `elem` [204, 304]
      chunked = lookup "transfer-encoding" (rsFields rs) == Just "chunked"
  in conjoin
       [ rsStatus rs === sStatus sc
       , lookup "x-test" (rsFields rs) === Just "1"
       , rsBody rs === (if noBody then "" else B.concat (sContent sc))
       , if chunked then rsTrailers rs === [ (C.map toLower k, x) | (k, x) <- sTrailers sc ] else property True
       , counterexample "chunked only for HTTP/1.1" (not chunked || rMinor r == 1)
       , counterexample "trailers sent whenever there are some (D9)"
           (null (sTrailers sc) || noBody || rMinor r == 0 || chunked)
       , counterexample "100 (Continue) only when asked for and the body is read"
           (rsInterim rs == 0 || rExpect r && sReads sc /= Just 0)
       ]

-- | How much a script that reads part of a body gets depends on how the
-- body arrived, so these compare scripts that read all or none of it.
allOrNone :: Script -> Script
allOrNone sc = sc { sReads = fmap (const 0) (sReads sc) }

prop_splitting :: Property
prop_splitting = withMaxSuccess 1000 $
  forAll (resize 4 (listOf1 ((,) <$> genReq <*> (allOrNone <$> genScript)))) $ \session ->
  let wire = B.concat (map (render . fst) session) in
  forAll (splitInput wire) $ \pieces ->
    sameRun (simulate defaultConfig pieces (map snd session)) (simulate defaultConfig [wire] (map snd session))

sameRun :: Run -> Run -> Property
sameRun a b = conjoin
  [ map summary (runServed a) === map summary (runServed b)
  , runSent a === runSent b
  , runClosed a === runClosed b
  , runMisuse a === runMisuse b ]
  where summary v = (show (srvHead v), srvBody v, fmap fieldsToList (srvEnd v), srvAborted v)

-- | Valid sessions with bytes changed, inserted or removed: whatever
-- the core makes of them, it answers each request it saw once, with a
-- well-formed response, and never misbehaves.
prop_damaged :: Property
prop_damaged = withMaxSuccess 1000 $
  forAll (resize 4 (listOf1 ((,) <$> genReq <*> (allOrNone <$> genScript)))) $ \session ->
  forAll (damage (B.concat (map (render . fst) session))) $ \wire ->
  forAll (splitInput wire) $ \pieces ->
    let run = simulate defaultConfig pieces (map snd session)
        whole = simulate defaultConfig [wire] (map snd session)
        heads = [ methodBytes (reqMethod (srvHead v)) == "HEAD" | v <- runServed run ]
    in counterexample (show wire) $
       conjoin
         [ sameRun run whole
         , runMisuse run === []
         , runAfterClose run === False
         , runClosed run === True
         , case readResponses heads (runSent run) of
             Left e -> counterexample e False
             -- a rejection adds one response for a request never served
             Right rs -> property (length rs == length (runServed run) || length rs == length (runServed run) + 1)
         ]

-- | Up to three bytes changed, inserted or removed.
damage :: ByteString -> Gen ByteString
damage s0 = do
  k <- choose (0, 3 :: Int)
  foldr (\_ g -> g >>= once') (pure s0) [1 .. k]
  where
    once' s = do
      i <- choose (0, max 0 (B.length s - 1))
      b <- elements (B.unpack "\r\n :;0aG\t\0\128")
      op <- elements [0, 1, 2 :: Int]
      pure $ case op of
        0 -> B.take i s <> B.singleton b <> B.drop (i + 1) s
        1 -> B.take i s <> B.singleton b <> B.drop i s
        _ -> B.take i s <> B.drop (i + 1) s

------------------------------------------------------------------------------
-- examples
------------------------------------------------------------------------------

-- | Run inputs from a new connection, giving all outputs.
outputs :: Config -> [Input] -> [Output]
outputs config = go (newConn config)
  where go _ [] = []
        go c (i : is) = let (c', os) = step i c in os ++ go c' is

-- | Outputs as text, for comparing.
shown :: [Output] -> [String]
shown = map s
  where
    s o = case o of
      NewRequest h  -> "request " ++ C.unpack (methodBytes (reqMethod h)) ++ " " ++ show (reqBodyLength h)
      RequestData b -> "data " ++ show b
      RequestEnd t  -> "end " ++ show (map (\(k, v) -> (fieldNameBytes k, fieldValueBytes v)) (fieldsToList t))
      Send bs       -> "send " ++ show (B.concat bs)
      other         -> show other

response :: Int -> Maybe Integer -> ResponseHead
response st len = ResponseHead (fromJust (mkStatus st)) emptyFields True (fromIntegral <$> len) []

get :: ByteString
get = "GET / HTTP/1.1\r\nHost: a\r\n\r\n"

-- = https://www.rfc-editor.org/rfc/rfc9112#section-9.3
-- # If the received protocol is HTTP/1.1 (or later), the connection
-- # will persist after the current response

-- = https://www.rfc-editor.org/rfc/rfc9112#section-9.6
-- # A server that receives a "close" connection option MUST initiate
-- # closure of the connection (see below) after it sends the final
-- # response to the request that contained the "close" connection
-- # option.
prop_persistence :: Property
prop_persistence = once $ conjoin
  [ -- HTTP/1.1 persists, and a pipelined request is read once the response is done
    shown (outputs defaultConfig [Received (get <> get), Respond (response 200 (Just 2)), WriteBody "ok", EndBody emptyFields])
      === [ "request GET Just 0", "send \"HTTP/1.1 200 OK\\r\\ncontent-length: 2\\r\\n\\r\\n\""
          , "send \"ok\"", "request GET Just 0" ]
  , shown (outputs defaultConfig [Received "GET / HTTP/1.1\r\nHost: a\r\nConnection: close\r\n\r\n", Respond (response 200 (Just 0)), EndBody emptyFields])
      === [ "request GET Just 0", "send \"HTTP/1.1 200 OK\\r\\ncontent-length: 0\\r\\nconnection: close\\r\\n\\r\\n\"", "Close" ]
  , shown (outputs defaultConfig [Received "GET / HTTP/1.1\r\nHost: a\r\nConnection: keep-alive, CLOSE\r\n\r\n", Respond (response 204 Nothing), EndBody emptyFields])
      === [ "request GET Just 0", "send \"HTTP/1.1 204 No Content\\r\\nconnection: close\\r\\n\\r\\n\"", "Close" ]
    -- HTTP/1.0 always closes (D5); an unknown length is close-delimited
  , shown (outputs defaultConfig [Received "GET / HTTP/1.0\r\nHost: a\r\n\r\n", Respond (response 200 Nothing), WriteBody "ab", EndBody emptyFields])
      === [ "request GET Just 0", "send \"HTTP/1.1 200 OK\\r\\nconnection: close\\r\\n\\r\\n\"", "send \"ab\"", "Close" ]
    -- the client closing between requests, or mid-head
  , shown (outputs defaultConfig [Received get, Respond (response 204 Nothing), EndBody emptyFields, PeerClosed])
      === [ "request GET Just 0", "send \"HTTP/1.1 204 No Content\\r\\n\\r\\n\"", "NeedInput", "Close" ]
  , shown (outputs defaultConfig [Received "GET / HT", PeerClosed]) === ["NeedInput", "Close"]
    -- the client half-closing after its request still gets the response
  , shown (outputs defaultConfig [Received get, PeerClosed, Respond (response 204 Nothing), EndBody emptyFields])
      === [ "request GET Just 0", "send \"HTTP/1.1 204 No Content\\r\\n\\r\\n\"", "Close" ]
  ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1
-- # Upon receiving an HTTP/1.1 (or later) request that has a method,
-- # target URI, and complete header section that contains a 100-continue
-- # expectation and an indication that request content will follow, an
-- # origin server MUST send either:

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1
-- # The origin server MUST NOT wait for the content before sending the
-- # 100 (Continue) response.

-- The application either responds (a final status) or asks for the

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1
-- # *  A server that receives a 100-continue expectation in an HTTP/1.0
-- # request MUST ignore that expectation.
-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1
-- # *  A server that responds with a final status code before reading the
-- # entire request content SHOULD indicate whether it intends to close
-- # the connection (e.g., see Section 9.6 of [HTTP/1.1]) or continue
-- # reading the request content.
prop_continue :: Property
prop_continue = once $ conjoin
  [ -- sent when the body is first asked for
    shown (outputs defaultConfig [Received (expecting "1.1"), WantBody, Received "ab"])
      === [ "request POST Just 2", "send \"HTTP/1.1 100 Continue\\r\\n\\r\\n\"", "NeedInput", "data \"ab\"", "end []" ]
    -- not for HTTP/1.0
  , shown (outputs defaultConfig [Received (expecting "1.0"), WantBody, Received "ab"])
      === [ "request POST Just 2", "NeedInput", "data \"ab\"", "end []" ]
    -- not after a final response, and the connection then closes (the
    -- client may never send the body)
  , shown (outputs defaultConfig [Received (expecting "1.1"), Respond (response 413 (Just 0)), EndBody emptyFields])
      === [ "request POST Just 2", "send \"HTTP/1.1 413 Content Too Large\\r\\ncontent-length: 0\\r\\nconnection: close\\r\\n\\r\\n\"", "Close" ]
    -- not after a final response, even if the body is read after it
  , shown (outputs defaultConfig [Received (expecting "1.1"), Respond (response 200 Nothing), WantBody])
      === [ "request POST Just 2", "send \"HTTP/1.1 200 OK\\r\\ntransfer-encoding: chunked\\r\\nconnection: close\\r\\n\\r\\n\"", "NeedInput" ]
    -- not twice
  , length [ () | Send [b] <- outputs defaultConfig [Received (expecting "1.1" <> "a"), WantBody, WantBody, Received "b"], "100" `B.isInfixOf` b ] === 1
  ]
  where expecting v = "POST / HTTP/" <> v <> "\r\nHost: a\r\nExpect: 100-continue\r\nContent-Length: 2\r\n\r\n"

prop_rejections :: Property
prop_rejections = once $ conjoin
  [ status "GET / HTTP/1.1\r\n\r\n" === "400"                                          -- no Host
  , status "GET / HTTP/1.1\r\nHost: a\r\nHost: b\r\n\r\n" === "400"
  , status "GET / HTTP/2.0\r\nHost: a\r\n\r\n" === "505"
  , status "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 1\r\nTransfer-Encoding: chunked\r\n\r\n" === "400"
  , status "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: gzip, chunked\r\n\r\n" === "501"
  , status "GET https://a/ HTTP/1.1\r\nHost: a\r\n\r\n" === "421"
  , status ("GET /" <> C.replicate 9000 'a' <> " HTTP/1.1\r\n\r\n") === "414"
  , status "GET / HTTP/1.1\r\nHost: a\r\nExpect: 100-continue, x\r\n\r\n" === "417"   -- Q3
  , status "GET / HTTP/1.1\r\nHost: a\r\nExpect: x=1\r\n\r\n" === "417"
  , status "GET / HTTP/1.1\r\nHost: a\r\nConnection: \"close\"\r\n\r\n" === "400"
  , status "CONNECT a:443 HTTP/1.1\r\nHost: a:443\r\nContent-Length: 1\r\n\r\nx" === "400"  -- Q2
  , status "CONNECT a:443 HTTP/1.1\r\nHost: a:443\r\n\r\n" === "request"
  , status "GET / HTTP/1.1\r\nHost: a\r\nExpect: 100-CONTINUE\r\n\r\n" === "request"
    -- a rejection is the whole of the core's answer
  , shown (outputs defaultConfig [Received "GET / HTTP/1.1\r\n\r\n"])
      === ["send \"HTTP/1.1 400 Bad Request\\r\\ncontent-length: 0\\r\\nconnection: close\\r\\n\\r\\n\"", "Close"]
  ]
  where
    status s = case outputs defaultConfig [Received s] of
      (NewRequest _ : _) -> "request"
      (Send [b] : _)     -> C.unpack (B.take 3 (B.drop 9 b))
      os                 -> show (shown os)

prop_bodies :: Property
prop_bodies = once $ conjoin
  [ -- one answer per WantBody, as slices of what arrived
    shown (outputs defaultConfig [Received (post "5" <> "ab"), WantBody, Received "cde", WantBody])
      === [ "request POST Just 5", "data \"ab\"", "data \"cde\"", "end []" ]
    -- nothing is read until the application asks
  , shown (outputs defaultConfig [Received (post "5"), Received "abcde"]) === ["request POST Just 5"]
  , shown (outputs defaultConfig [Received chunked, WantBody, WantBody, WantBody])
      === [ "request POST Nothing", "data \"abc\"", "data \"d\"", "end [(\"x-t\",\"1\")]" ]
    -- the client closing, or an invalid chunked body, aborts the body
  , shown (outputs defaultConfig [Received (post "5" <> "ab"), PeerClosed, WantBody, WantBody])
      === [ "request POST Just 5", "data \"ab\""
          , "RequestAborted", "send \"HTTP/1.1 400 Bad Request\\r\\ncontent-length: 0\\r\\nconnection: close\\r\\n\\r\\n\"", "Close" ]
  , shown (outputs defaultConfig [Received (postChunked <> "3\r\nabcXY"), WantBody, WantBody])
      === [ "request POST Nothing", "data \"abc\""
          , "RequestAborted", "send \"HTTP/1.1 400 Bad Request\\r\\ncontent-length: 0\\r\\nconnection: close\\r\\n\\r\\n\"", "Close" ]
    -- once a response has started, an aborted body cuts it off: no error response inside it
  , shown (outputs defaultConfig [Received (post "5" <> "ab"), Respond (response 200 Nothing), WantBody, WantBody, PeerClosed])
      === [ "request POST Just 5", "send \"HTTP/1.1 200 OK\\r\\ntransfer-encoding: chunked\\r\\n\\r\\n\""
          , "data \"ab\"", "NeedInput", "RequestAborted", "Close" ]
    -- D6: an unread body is discarded after the response, up to the limit
  , shown (outputs defaultConfig [Received (post "5" <> "abcde" <> get), Respond (response 204 Nothing), EndBody emptyFields])
      === [ "request POST Just 5", "send \"HTTP/1.1 204 No Content\\r\\n\\r\\n\"", "request GET Just 0" ]
  , shown (outputs small [Received (post "5" <> "ab"), Respond (response 204 Nothing), EndBody emptyFields])
      === [ "request POST Just 5", "send \"HTTP/1.1 204 No Content\\r\\n\\r\\n\"", "NeedInput" ]
  , shown (outputs small [Received (post "13" <> "ab"), Respond (response 204 Nothing), EndBody emptyFields])
      === [ "request POST Just 13", "send \"HTTP/1.1 204 No Content\\r\\n\\r\\n\"", "Close" ]
  , shown (outputs small [Received (postChunked <> "2\r\nab\r\n0\r\n\r\n" <> get), Respond (response 204 Nothing), EndBody emptyFields])
      === [ "request POST Nothing", "send \"HTTP/1.1 204 No Content\\r\\n\\r\\n\"", "request GET Just 0" ]
  , shown (outputs small [Received (postChunked <> "4\r\nabcd\r\n0\r\n\r\n" <> get), Respond (response 204 Nothing), EndBody emptyFields])
      === [ "request POST Nothing", "send \"HTTP/1.1 204 No Content\\r\\n\\r\\n\"", "Close" ]
  ]
  where
    post n = "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: " <> n <> "\r\n\r\n"
    postChunked = "POST / HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n"
    chunked = postChunked <> "3\r\nabc\r\n1\r\nd\r\n0\r\nX-T: 1\r\n\r\n"
    small = defaultConfig { configDiscardLimit = 12 }

prop_responses :: Property
prop_responses = once $ conjoin
  [ -- chunked, with the Trailer field and the trailer section; an empty write sends nothing
    shown (outputs defaultConfig [Received get, Respond (withTrailers (response 200 Nothing)), WriteBody "ab", WriteBody "", EndBody (fields [("X-C", "1")])])
      === [ "request GET Just 0"
          , "send \"HTTP/1.1 200 OK\\r\\ntransfer-encoding: chunked\\r\\ntrailer: x-c\\r\\n\\r\\n\""
          , "send \"2\\r\\nab\\r\\n\"", "send \"0\\r\\nx-c: 1\\r\\n\\r\\n\"", "NeedInput" ]
    -- HEAD: the length GET would send, and no content
  , shown (outputs defaultConfig [Received "HEAD / HTTP/1.1\r\nHost: a\r\n\r\n", Respond (response 200 (Just 2)), WriteBody "ab", EndBody emptyFields])
      === [ "request HEAD Just 0", "send \"HTTP/1.1 200 OK\\r\\ncontent-length: 2\\r\\n\\r\\n\"", "NeedInput" ]
    -- more or less than declared is cut off, never ended normally
  , shown (outputs defaultConfig [Received get, Respond (response 200 (Just 2)), WriteBody "abc"])
      === [ "request GET Just 0", "send \"HTTP/1.1 200 OK\\r\\ncontent-length: 2\\r\\n\\r\\n\"", "Close" ]
  , shown (outputs defaultConfig [Received get, Respond (response 200 (Just 2)), WriteBody "a", EndBody emptyFields])
      === [ "request GET Just 0", "send \"HTTP/1.1 200 OK\\r\\ncontent-length: 2\\r\\n\\r\\n\"", "send \"a\"", "Close" ]
  , shown (outputs defaultConfig [Received get, Respond (response 200 Nothing), WriteBody "a", Abort])
      === [ "request GET Just 0", "send \"HTTP/1.1 200 OK\\r\\ntransfer-encoding: chunked\\r\\n\\r\\n\"", "send \"1\\r\\na\\r\\n\"", "Close" ]
    -- a status without a reason phrase we know
  , take 1 [ b | Send [b] <- outputs defaultConfig [Received get, Respond (response 299 (Just 0))] ]
      === ["HTTP/1.1 299 \r\ncontent-length: 0\r\n\r\n"]
  ]
  where withTrailers h = h { respTrailers = [name "X-C"] }

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.5.9
-- # The 408 (Request Timeout) status code indicates that the server did
-- # not receive a complete request message within the time that it was
-- # prepared to wait.
prop_events :: Property
prop_events = once $ conjoin
  [ -- timeouts by phase (D8)
    shown (outputs defaultConfig [TimedOut]) === ["Close"]
  , shown (outputs defaultConfig [Received "GET", TimedOut])
      === ["NeedInput", "send \"HTTP/1.1 408 Request Timeout\\r\\ncontent-length: 0\\r\\nconnection: close\\r\\n\\r\\n\"", "Close"]
  , shown (outputs defaultConfig [Received post, WantBody, TimedOut])
      === [ "request POST Just 5", "NeedInput", "RequestAborted"
          , "send \"HTTP/1.1 408 Request Timeout\\r\\ncontent-length: 0\\r\\nconnection: close\\r\\n\\r\\n\"", "Close" ]
  , shown (outputs defaultConfig [Received get, TimedOut])
      === ["request GET Just 0", "ProtocolMisuse TimedOutWithoutTimer", "Close"]
  , phases [Received "GET"] === ReadingHead
  , phases [] === Idle
  , phases [Received post] === WaitingForApplication
  , phases [Received post, WantBody] === ReadingBody
  , phases [Received post, Respond (response 204 Nothing), EndBody emptyFields] === DiscardingBody
  , phases [Received get, Respond (response 204 Nothing), EndBody emptyFields, Shutdown] === Closed
    -- shutdown: now when idle, after the exchange otherwise
  , shown (outputs defaultConfig [Shutdown]) === ["Close"]
  , shown (outputs defaultConfig [Received (get <> get), Shutdown, Respond (response 204 Nothing), EndBody emptyFields])
      === [ "request GET Just 0", "send \"HTTP/1.1 204 No Content\\r\\nconnection: close\\r\\n\\r\\n\"", "Close" ]
  , shown (outputs defaultConfig [Received (get <> get), Respond (response 200 (Just 1)), Shutdown, WriteBody "a", EndBody emptyFields])
      === [ "request GET Just 0", "send \"HTTP/1.1 200 OK\\r\\ncontent-length: 1\\r\\n\\r\\n\"", "send \"a\"", "Close" ]
    -- misuse by the driver (Q4), and nothing after Close
  , shown (outputs defaultConfig [Respond (response 200 Nothing)]) === ["ProtocolMisuse RespondWithoutRequest", "Close"]
  , shown (outputs defaultConfig [Received get, Respond (response 204 Nothing), Respond (response 204 Nothing)])
      === ["request GET Just 0", "send \"HTTP/1.1 204 No Content\\r\\n\\r\\n\"", "ProtocolMisuse RespondTwice", "Close"]
  , shown (outputs defaultConfig [Received get, WriteBody "a"]) === ["request GET Just 0", "ProtocolMisuse BodyWithoutResponse", "Close"]
  , shown (outputs defaultConfig [Received get, WantBody]) === ["request GET Just 0", "ProtocolMisuse WantBodyWithoutBody", "Close"]
  , shown (outputs defaultConfig [Received post, WantBody, WantBody])
      === ["request POST Just 5", "NeedInput", "ProtocolMisuse WantBodyTwice", "Close"]
  , shown (outputs defaultConfig [Shutdown, Received get, PeerClosed, TimedOut]) === ["Close"]
  ]
  where
    post = "POST / HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\n"
    phases is = phase (foldl (\c i -> fst (step i c)) (newConn defaultConfig) is)

------------------------------------------------------------------------------
-- the reference model
------------------------------------------------------------------------------

-- | What the application sees of a request: method, target, version,
-- fields, body length; the body read, its trailers, and whether it was
-- aborted.
type Seen = (ByteString, String, HttpVersion, [(ByteString, ByteString)], Maybe Integer, ByteString, Maybe [(ByteString, ByteString)], Bool)

seen :: Served -> Seen
seen v = let h = srvHead v in
  ( methodBytes (reqMethod h), show (reqTarget h), reqVersion h, pairs (reqFields h), fromIntegral <$> reqBodyLength h
  , srvBody v, pairs <$> srvEnd v, srvAborted v )

pairs :: Fields -> [(ByteString, ByteString)]
pairs fs = [ (fieldNameBytes k, fieldValueBytes v) | (k, v) <- fieldsToList fs ]

-- | The requests the application should see and the responses a client
-- should read, for the whole input at once, followed by the end of the
-- input, with scripts that read all of a body or none of it.
--
-- Each request is taken from the start of what is left: its head with
-- 'parseRequestHead', its framing, target and fields with the functions
-- that are tested on their own, and its body all at once (a
-- Content-Length body is the next n octets; a chunked one is decoded
-- in one go). The rest follows RFC 9112 Section 9 and the design (D2,
-- D3, D5 to D7, Q2, Q3), written out case by case.
model :: [Script] -> ByteString -> ([Seen], [Resp])
model = go
  where
    limit = toInteger (configDiscardLimit defaultConfig)
    go scripts input = case parseRequestHead defaultLimits 0 input of
      HeadIncomplete _ -> ([], [])                                   -- the client closed
      HeadError e -> ([], [rejection (headErrorStatus e)])
      HeadComplete raw used -> case check raw of
        Left st -> ([], [rejection st])
        Right (method, target, version, fs, framing, expects100, closeAsked) ->
          let (sc, scripts') = case scripts of { (x : xs) -> (x, xs); [] -> (readAll, []) }
              rest = B.drop used input
              hasBody = framing /= NoBody && framing /= ContentLength 0
              len = case framing of { NoBody -> Just 0; ContentLength n -> Just (toInteger n); Chunked -> Nothing }
              seenAs body end aborted = (method, show target, version, fs, len, body, end, aborted)
              respondThen = respond sc method version closeAsked expects100 scripts'
          in if not hasBody
               then add (seenAs "" Nothing False) (respondThen False (Just rest))
             else if sReads sc == Nothing
               -- reading the body sends 100 (Continue) first, if expected (D7)
               then case wholeBody framing rest of
                 -- the data before the body was found to be cut short or invalid has been read
                 (partial, Nothing) -> ([seenAs partial Nothing True], [(rejection BadRequest) { rsInterim = fromEnum expects100 }])
                 (body, Just (trailers, k)) ->
                   firstInterim expects100 (add (seenAs body (Just trailers) False) (respondThen False (Just (B.drop k rest))))
               -- D6: a body nobody read is dropped after the response, if
               -- all of it arrives within the limit
               else add (seenAs "" Nothing False) (respondThen True (discarded framing rest))
    add v (served, resps) = (v : served, resps)
    firstInterim e (served, r : rs) = (served, r { rsInterim = fromEnum e } : rs)
    firstInterim _ x = x

    -- the response, then the next request if the connection is kept
    respond sc method version closeAsked expects100 scripts' unread next =
      let code = sStatus sc
          noContent = code `elem` [204, 304]
          isHead = method == "HEAD"
          len = sum (map B.length (sContent sc))
          trailers = not noContent && not (null (sTrailers sc))
          -- framing, as RFC 9112 Section 6 and design note M2 have it
          (framingFs, chunked)
            | noContent = ([], False)
            | isHead = (if sDeclared sc then [("content-length", C.pack (show len))] else [], False)
            | version == HttpVersion 1 1 && (not (sDeclared sc) || trailers) = ([("transfer-encoding", "chunked")], True)
            | sDeclared sc = ([("content-length", C.pack (show len))], False)
            | otherwise = ([], False)
          -- D5, RFC 9112 Section 9.3; and a client waiting for 100 may never send its body
          keepAlive = version == HttpVersion 1 1 && not closeAsked && not (unread && expects100)
          fs = [("x-test", "1")] ++ framingFs
               ++ [ ("trailer", B.intercalate ", " [ C.map toLower n | (n, _) <- sTrailers sc ]) | chunked, trailers ]
               ++ [ ("connection", "close") | not keepAlive ]
          resp = Resp code fs (if noContent || isHead then "" else B.concat (sContent sc))
                      (if chunked then [ (C.map toLower n, v) | (n, v) <- sTrailers sc ] else []) 0
          (served, resps) = case next of
            Just rest | keepAlive -> go scripts' rest
            _                     -> ([], [])
      in (served, resp : resps)

    discarded framing rest = case wholeBody framing rest of
      (body, Just (_, k))
        | toInteger (B.length body) <= limit, toInteger k <= limit || framing /= Chunked -> Just (B.drop k rest)
      _ -> Nothing

    -- the checks on a request head, in the order they are made
    check :: RawRequestHead -> Either Status (ByteString, Target, HttpVersion, [(ByteString, ByteString)], RequestFraming, Bool, Bool)
    check raw = do
      let version = Head.headVersion raw
          method = Head.headMethod raw
          hfs = Head.headFields raw
          lowerAll = map (C.map toLower)
          members n = fmap concat (traverse (either (const Nothing) Just . splitList NoComments) (lookupAll (name n) hfs))
      framing <- either (Left . framingErrorStatus) Right (requestFraming version hfs)
      (target, fields') <- either (Left . targetErrorStatus) Right
                             (reconstructTarget (configTarget defaultConfig) version method (Head.headTarget raw) hfs)
      if methodBytes method == "CONNECT" && framing /= NoBody then Left BadRequest else Right ()
      expects <- case members "Expect" of
        Just ms | all (== "100-continue") (lowerAll ms) -> Right (not (null ms))
        _ -> Left (status 417)
      closeAsked <- case members "Connection" of
        Just os | all (\o -> not (B.null o) && C.all (`elem` tchars) o) os -> Right ("close" `elem` lowerAll os)
        _ -> Left BadRequest
      let hasBody = framing /= NoBody && framing /= ContentLength 0
          fields'' = [ p | p@(k, _) <- pairs fields', not (isProtocolField (name k)) ]
      Right ( methodBytes method, target, version, fields'', framing
            , expects && version == HttpVersion 1 1 && hasBody, closeAsked )
    tchars = "!#$%&'*+-.^_`|~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" :: String

    -- the body's content, and its trailers and length on the wire, or
    -- Nothing if it is cut short or invalid (the content is then what
    -- came before that)
    wholeBody framing rest = case framing of
      ContentLength n | toInteger (B.length rest) >= toInteger n -> (B.take (fromIntegral n) rest, Just ([], fromIntegral n))
                      | otherwise -> (rest, Nothing)
      Chunked -> dechunk startDecoder rest 0 ""
      NoBody -> ("", Just ([], 0))
    dechunk d s used acc = case decodeChunked defaultChunkedLimits d s of
      Data b k d' -> dechunk d' (B.drop k s) (used + k) (acc <> b)
      NeedMore k d' | k > 0 -> dechunk d' (B.drop k s) (used + k) acc
      Finished t k -> (acc, Just (pairs t, used + k))
      _ -> (acc, Nothing)

    rejection st = Resp (statusCode st) [("content-length", "0"), ("connection", "close")] "" [] 0
    status = fromJust . mkStatus

prop_model :: Property
prop_model = withMaxSuccess 2000 $
  forAll (resize 4 (listOf1 ((,) <$> genReq <*> (allOrNone <$> genScript)))) $ \session ->
  forAll (frequency [(1, pure (B.concat (map (render . fst) session))), (2, damage (B.concat (map (render . fst) session)))]) $ \wire ->
  forAll (splitInput wire) $ \pieces ->
    let scripts = map snd session
        run = simulate defaultConfig pieces scripts
        (wantSeen, wantResps) = model scripts wire
        heads = [ m == "HEAD" | (m, _, _, _, _, _, _, _) <- wantSeen ]
    in counterexample (show wire) $
       cover 30 (length wantSeen > 1) "several requests" $
       cover 10 (length wantResps > length wantSeen) "rejected" $
       conjoin
         [ map seen (runServed run) === wantSeen
         , readResponses heads (runSent run) === Right wantResps ]
