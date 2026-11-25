{-# LANGUAGE OverloadedStrings #-}
-- | Differential tests of the HTTP/1.1 core against llhttp, the parser
-- of Node.js: the same octets, split the same way, go to both, and
-- what each makes of them is compared.
--
-- With @LLHTTP_SURVEY=n@ in the environment, n sessions are run and
-- every kind of difference is printed with its count and an example,
-- the expected ones as well.
module Main (main) where

import Control.Monad (forM_, replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Foreign.C.Types (CChar, CLong(..), CSize(..))
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr)
import Hyperdrive.HTTP1.Chunked (Decoded(..), decodeChunked, defaultChunkedLimits, startDecoder)
import Hyperdrive.HTTP1.Connection
import Hyperdrive.HTTP1.Framing (RequestFraming(..), requestFraming)
import Hyperdrive.HTTP1.Head (HeadResult(..), defaultLimits, parseRequestHead)
import qualified Hyperdrive.HTTP1.Head as Head
import Hyperdrive.HTTP1.Target (reconstructTarget)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.FieldSyntax (Comments(..), splitList)
import Hyperdrive.Semantics.Message (HttpVersion(..), isProtocolField)
import Hyperdrive.Semantics.Method (methodBytes)
import Hyperdrive.Semantics.Status (mkStatus)
import Hyperdrive.Testing.Client
import Hyperdrive.Testing.Model (pairs)
import Hyperdrive.Testing.Session (damage, fields, splitInput)
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = do
  survey <- lookupEnv "LLHTTP_SURVEY"
  case survey of
    Just n -> runSurvey (read n)
    Nothing -> defaultMain $ testGroup "hyperdrive-http1 against llhttp"
      [ testProperty "the same messages, and nothing accepted that llhttp refuses" prop_agrees ]

------------------------------------------------------------------------------
-- what each side makes of a connection's input
------------------------------------------------------------------------------

-- | A request that arrived in full.
data Msg = Msg
  { mMethod   :: ByteString
  , mTarget   :: ByteString
    -- ^ as it was sent
  , mVersion  :: (Int, Int)
  , mLines    :: [(ByteString, ByteString)]
    -- ^ every field line of the header section, names in lower case
  , mFields   :: [(ByteString, ByteString)]
    -- ^ names in lower case, without @Host@ and the protocol fields
  , mBody     :: ByteString
  , mTrailers :: [(ByteString, ByteString)]
  } deriving (Eq, Show)

-- | How the input ended, after the requests that arrived in full.
data End
  = Waiting           -- ^ more input is awaited
  | Shut            -- ^ the connection was closed after a response, as asked or as its version has it
  | Upgraded          -- ^ llhttp: stopped at an upgrade or a CONNECT
  | Rejected String   -- ^ a status code, or llhttp's name for the error
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- llhttp
------------------------------------------------------------------------------

foreign import ccall unsafe "hd_llhttp_run"
  c_run :: Ptr CChar -> Ptr CSize -> CSize -> Ptr Word8 -> CSize -> IO CLong

llhttp :: [ByteString] -> ([Msg], End)
llhttp pieces = unsafePerformIO $ do
  let input = B.concat pieces
      capacity = 6 * B.length input + 5 * length pieces + 4096
  out <- BI.createUptoN capacity $ \buffer ->
    BU.unsafeUseAsCString input $ \p ->
    withArrayLen (map (fromIntegral . B.length) pieces) $ \n lengths -> do
      written <- c_run p lengths (fromIntegral n) buffer (fromIntegral capacity)
      if written < 0 then error "llhttp: the record did not fit" else pure (fromIntegral written)
  pure (readRecords (records out))

records :: ByteString -> [(Char, ByteString)]
records s
  | B.null s = []
  | otherwise =
      let n = sum [ fromIntegral (B.index s i) * 256 ^ (i - 1) | i <- [1 .. 4 :: Int] ]
      in (C.head s, B.take n (B.drop 5 s)) : records (B.drop (5 + n) s)

readRecords :: [(Char, ByteString)] -> ([Msg], End)
readRecords = go [] blank
  where
    blank = Partial "" "" "" [] "" [] "" "" False
    go done p rs = case rs of
      [] -> error "llhttp: no end"
      ((tag, s) : rest) -> case tag of
        'B' -> go done blank rest
        'M' -> go done p { pMethod = pMethod p <> s } rest
        'U' -> go done p { pTarget = pTarget p <> s } rest
        'V' -> go done p { pVersion = pVersion p <> s } rest
        'F' -> go done p { pName = pName p <> s } rest
        'H' -> go done p { pValue = pValue p <> s } rest
        -- a field without a value has no end of a value
        'f' -> go done p rest
        'h' | pInTrailers p -> go done p { pTrailers = (pName p, pValue p) : pTrailers p, pName = "", pValue = "" } rest
            | otherwise     -> go done p { pHeaders = (pName p, pValue p) : pHeaders p, pName = "", pValue = "" } rest
        'E' -> go done p { pInTrailers = True } rest
        'D' -> go done p { pBody = pBody p <> s } rest
        'C' -> go (finish p : done) blank rest
        'X' -> (reverse done, end (B.head s) (C.unpack (B.drop 1 s)))
        _   -> go done p rest
    finish p =
      let lower fs = [ (C.map toLower n, trim v) | (n, v) <- reverse fs ]
      in Msg (pMethod p) (pTarget p) (version (pVersion p)) (lower (pHeaders p)) (visible (lower (pHeaders p)))
             (pBody p) (lower (pTrailers p))
    -- llhttp leaves the whitespace after a field's value in it
    trim = C.dropWhileEnd isOWS . C.dropWhile isOWS
    version v = case C.unpack v of
      [a, '.', b] -> (fromEnum a - 48, fromEnum b - 48)
      _ -> (-1, -1)
    end code name' = case code of
      0  -> Waiting
      5  -> Shut    -- HPE_CLOSED_CONNECTION: input after a message that closes the connection
      22 -> Upgraded  -- HPE_PAUSED_UPGRADE
      _  -> Rejected name'

data Partial = Partial
  { pMethod, pTarget, pVersion :: ByteString
  , pHeaders :: [(ByteString, ByteString)]
  , pBody :: ByteString
  , pTrailers :: [(ByteString, ByteString)]
  , pName, pValue :: ByteString
  , pInTrailers :: Bool
  }

-- | The fields the application is given: not @Host@, which is in the
-- target, nor those the protocol layer keeps.
visible :: [(ByteString, ByteString)] -> [(ByteString, ByteString)]
visible fs = [ (n, v) | (n, v) <- fs, n /= "host", maybe True (not . isProtocolField) (mkFieldName n) ]

isOWS :: Char -> Bool
isOWS c = c == ' ' || c == '\t'

------------------------------------------------------------------------------
-- hyperdrive: the core, with an application that reads each body and
-- answers 200
------------------------------------------------------------------------------

data Sim = Sim
  { simConn   :: Conn
  , simPieces :: [ByteString]
  , simEvents :: [Output]
  , simSent   :: [ByteString]   -- ^ newest first
  , simClosed :: Bool
  , simDone   :: [Msg]          -- ^ newest first
  }

hyperdrive :: [ByteString] -> ([Msg], End)
hyperdrive pieces =
  let s = drive (Sim (newConn defaultConfig) pieces [NeedInput] [] False [])
      done = zipWith asSent (reverse (simDone s)) (starts (B.concat pieces))
      -- the application is given a target made from the one sent and
      -- Host, and not every field: what was sent is read again here
      asSent m from = case parseRequestHead defaultLimits 0 from of
        HeadComplete raw _ -> m { mTarget = Head.headTarget raw, mLines = pairs (Head.headFields raw) }
        _ -> m
      sent = B.concat (reverse (simSent s))
      heads = map ((== "HEAD") . mMethod) done ++ [False]
      end | not (simClosed s) = Waiting
          | otherwise = case readResponses heads sent of
              Left e -> Rejected ("unreadable responses: " ++ e)
              Right rs -> case [ rsStatus r | r <- rs, lookup "x-app" (rsFields r) == Nothing ] of
                []      -> Shut
                (c : _) -> Rejected (show c)
  in (done, end)

stepSim :: Input -> Sim -> Sim
stepSim i s
  | simClosed s = s
  | otherwise = let (c, outs) = step i (simConn s) in foldl apply s { simConn = c } outs
  where
    apply s' o = case o of
      Send bs -> s' { simSent = reverse bs ++ simSent s' }
      Close   -> s' { simClosed = True }
      _       -> s' { simEvents = simEvents s' ++ [o] }

drive :: Sim -> Sim
drive s
  | simClosed s = s
  | otherwise = case simEvents s of
      (NeedInput : rest) -> case simPieces s of
        []       -> s
        (p : ps) -> drive (stepSim (Received p) s { simPieces = ps, simEvents = rest })
      (NewRequest h : rest) -> case body h s { simEvents = rest } "" of
        (s', Just (content, trailers)) -> drive (respond (s' { simDone = msg h content trailers : simDone s' }))
        (s', Nothing) -> s'
      (_ : rest) -> drive s { simEvents = rest }
      [] -> s
  where
    msg h content trailers =
      let HttpVersion major minor = reqVersion h
      in Msg (methodBytes (reqMethod h)) "" (fromIntegral major, fromIntegral minor) [] (pairs (reqFields h)) content (pairs trailers)
    respond s0 =
      let h = ResponseHead (fromJust (mkStatus 200)) (fields [("X-App", "1")]) True (Just 0) []
      in stepSim (EndBody emptyFields) (stepSim (Respond h) s0)

-- | The request's body and trailers, or Nothing if they did not arrive
-- in full.
body :: RequestHead -> Sim -> ByteString -> (Sim, Maybe (ByteString, Fields))
body h s0 acc0
  | reqBodyLength h == Just 0 = (s0, Just ("", emptyFields))
  | otherwise = await (stepSim WantBody s0) acc0
  where
    await s acc = case simEvents s of
      (NeedInput : rest) -> case simPieces s of
        _ | simClosed s -> (s, Nothing)
        []       -> (s, Nothing)
        (p : ps) -> await (stepSim (Received p) s { simPieces = ps, simEvents = rest }) acc
      (RequestData b : rest) -> await (stepSim WantBody s { simEvents = rest }) (acc <> b)
      (RequestEnd t : rest)  -> (s { simEvents = rest }, Just (acc, t))
      (RequestAborted : rest) -> (s { simEvents = rest }, Nothing)
      (_ : rest) -> await s { simEvents = rest } acc
      [] -> (s, Nothing)

------------------------------------------------------------------------------
-- comparing
------------------------------------------------------------------------------

-- | A difference between the two in the requests they took in full.
-- What follows the last of those is not compared: one may refuse at
-- once what the other has not seen the end of.
data Difference
  = Differ String
    -- ^ a request both took, read differently: what differs
  | Lenient String (Maybe String)
    -- ^ hyperdrive took a request that llhttp did not: llhttp's
    -- reason, and what in the request accounts for it, if anything
    -- known does
  | Strict String String
    -- ^ llhttp took a request that hyperdrive refused: the status, and why
  | Framing
    -- ^ llhttp took a request of which hyperdrive awaits more
  deriving (Eq, Ord, Show)

compareRuns :: ByteString -> ([Msg], End) -> ([Msg], End) -> Maybe Difference
compareRuns input (hs, hEnd) (ls, lEnd)
  | (h, l) : _ <- [ p | p@(h, l) <- zip hs ls, h /= l ] = Just (Differ (what h l))
  | length hs > length ls = case lEnd of
      Upgraded   -> Nothing  -- llhttp stops there; hyperdrive does not upgrade, and goes on
      Rejected e -> Just (Lenient e (known e (hs !! length ls) (starts input !! length ls)))
      _          -> Just (Lenient (show lEnd) Nothing)
  | length ls > length hs = case hEnd of
      Shut       -> Nothing  -- hyperdrive closes where llhttp would go on, which is safe
      Rejected c -> Just (Strict c (diagnose (starts input !! length hs)))
      _          -> Just Framing
  | otherwise = Nothing
  where
    what h l
      | mMethod h /= mMethod l = "method"
      | mTarget h /= mTarget l = "target"
      | mVersion h /= mVersion l = "version"
      | mLines h /= mLines l = "field lines"
      | mFields h /= mFields l = "fields"
      | mBody h /= mBody l = "body"
      | otherwise = "trailers"

-- | The input from the start of each request in turn, as hyperdrive's
-- own functions frame them.
starts :: ByteString -> [ByteString]
starts input = input : case parseRequestHead defaultLimits 0 input of
  HeadComplete raw used ->
    let rest = B.drop used input
    in case requestFraming (Head.headVersion raw) (Head.headFields raw) of
         Right NoBody -> starts rest
         Right (ContentLength n) -> starts (B.drop (fromIntegral n) rest)
         Right Chunked -> starts (B.drop (sum (map B.length (chunkedParts rest))) rest)
         Left _ -> repeat ""
  _ -> repeat ""

-- | A chunked body in the pieces the decoder takes it in: each is the
-- lines up to a chunk's data and that data, and the last is the rest,
-- to the end of the trailer section.
chunkedParts :: ByteString -> [ByteString]
chunkedParts = go startDecoder
  where
    go d s = case decodeChunked defaultChunkedLimits d s of
      Data _ n d' -> B.take n s : go d' (B.drop n s)
      NeedMore n d' | n > 0 -> B.take n s : go d' (B.drop n s)
      Finished _ n -> [B.take n s]
      _ -> []

-- | Why hyperdrive refuses the request at the start of this input: the
-- name of the error, or of the check.
diagnose :: ByteString -> String
diagnose input = case parseRequestHead defaultLimits 0 input of
  HeadIncomplete _ -> "HeadIncomplete"
  HeadError e -> constructor e
  HeadComplete raw used ->
    let version = Head.headVersion raw
        method = Head.headMethod raw
        hfs = Head.headFields raw
        members n = fmap concat (traverse (either (const Nothing) Just . splitList NoComments) (lookupAll (fromJust (mkFieldName n)) hfs))
    in case requestFraming version hfs of
         Left e -> constructor e
         Right framing -> case reconstructTarget (configTarget defaultConfig) version method (Head.headTarget raw) hfs of
           Left e -> constructor e
           Right _
             | methodBytes method == "CONNECT", framing /= NoBody -> "ConnectWithBody"
             | maybe True (any ((/= "100-continue") . C.map toLower)) (members "expect") -> "Expectation"
             | maybe True (any (\o -> B.null o || not (C.all isTokenChar o))) (members "connection") -> "ConnectionOption"
             | framing == Chunked, Just e <- chunkError startDecoder (B.drop used input) -> e
             | otherwise -> "Unknown"
  where
    constructor :: Show a => a -> String
    constructor = takeWhile (/= ' ') . show
    isTokenChar c = c `elem` ("!#$%&'*+-.^_`|~" :: String) || c >= '0' && c <= '9' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z'
    chunkError d s = case decodeChunked defaultChunkedLimits d s of
      Data _ n d' -> chunkError d' (B.drop n s)
      NeedMore n d' | n > 0 -> chunkError d' (B.drop n s)
      DecodeError e -> Just (constructor e)
      _ -> Nothing

-- | hyperdrive's reasons for refusing what llhttp takes. Each is a
-- check that hyperdrive makes and llhttp does not, or makes less
-- strictly; a reason that is not here, for a request llhttp takes, is
-- a failure.
stricter :: [(String, String)]
stricter =
  [ ("BareLineFeed", "llhttp takes a line feed alone before the request line")
  , ("BareCarriageReturn", "and a carriage return alone there")
  , ("InvalidRequestLine", "llhttp reads a request line without a version as HTTP/0.9")
  , ("UnsupportedVersion", "llhttp takes HTTP/0.9 and HTTP/2.0")
  , ("MissingHost", "llhttp does not look at Host")
  , ("RepeatedHost", "nor at how many there are")
  , ("InvalidHost", "nor at its value")
  , ("NoAuthority", "an HTTP/1.0 request without Host: hyperdrive has no default authority here")
  , ("InvalidTarget", "llhttp checks the octets of a target, not its form")
  , ("InvalidTargetAuthority", "the same")
  , ("TargetFormNotForMethod", "llhttp takes any form with any method")
  , ("MisdirectedScheme", "an https target on a connection without TLS")
  , ("TransferEncodingInHTTP10", "llhttp takes Transfer-Encoding in HTTP/1.0 (RFC 9112 Section 6.1)")
  , ("ChunkedNotFinal", "CONNECT: llhttp does not look at the framing of a request that upgrades")
  , ("InvalidTransferEncoding", "the same")
  , ("ChunkedWithParameters", "the same")
  , ("BothLengthAndEncoding", "the same")
  , ("UnsupportedTransferCoding", "llhttp passes other codings on; hyperdrive implements none (501)")
  , ("InvalidChunkExtension", "llhttp takes an extension with an empty value")
  , ("ConnectWithBody", "CONNECT with a body")
  , ("Expectation", "an expectation that is not 100-continue (417)")
  , ("ConnectionOption", "a Connection field that is not a list of tokens")
  ]

-- | What in a request that hyperdrive took accounts for llhttp
-- refusing it for this reason, if something known does.
--
-- Each is something RFC 9110 or 9112 allows or requires a recipient to
-- take, which llhttp does not; anything else is a failure.
known :: String -> Msg -> ByteString -> Maybe String
known reason h input = case reason of
  "HPE_INVALID_METHOD"
    | mMethod h `notElem` llhttpMethods -> Just "a method llhttp has no name for"
  "HPE_INVALID_VERSION"
    | mVersion h `notElem` [(1, 0), (1, 1)] -> Just "a minor version above 1 (RFC 9110 Section 2.5)"
  "HPE_INVALID_TRANSFER_ENCODING"
    | any (\v -> not (B.null v) && isOWS (C.last v)) (sent "transfer-encoding") -> Just "whitespace after Transfer-Encoding's value"
    | any emptyElement (sent "transfer-encoding") -> Just "an empty element in Transfer-Encoding (RFC 9110 Section 5.6.1.2)"
  "HPE_UNEXPECTED_CONTENT_LENGTH"
    | length (sent "content-length") > 1 -> Just "Content-Length repeated with the same value (RFC 9110 Section 8.6)"
  "HPE_INVALID_CONTENT_LENGTH"
    | any (C.elem ',') (sent "content-length") || length (sent "content-length") > 1 ->
        Just "Content-Length as a list of the same value (RFC 9110 Section 8.6)"
  _ | reason `elem` ["HPE_STRICT", "HPE_INVALID_CHUNK_SIZE"]
    , any (C.any isOWS . fst . Head.breakCRLF . dropCRLF) (chunkedParts (afterHead input)) ->
        Just "whitespace around a chunk extension (BWS, RFC 9112 Section 7.1.1)"
  _ -> Nothing
  where
    -- the field lines as sent, with the whitespace after their values
    sent n = [ C.dropWhile isOWS (B.drop 1 v) | l <- headLines, let (k, v) = C.break (== ':') l, C.map toLower k == n ]
    headLines = drop 1 (C.lines (C.filter (/= '\r') (fst (B.breakSubstring "\r\n\r\n" input))))
    afterHead = B.drop 4 . snd . B.breakSubstring "\r\n\r\n" . C.dropWhile (`elem` ("\r\n" :: String))
    emptyElement v = any (C.all isOWS) (C.split ',' v)
    -- each part after the first begins with the CRLF that ends a chunk's data
    dropCRLF s = if "\r\n" `B.isPrefixOf` s then B.drop 2 s else s

llhttpMethods :: [ByteString]
llhttpMethods =
  [ "DELETE", "GET", "HEAD", "POST", "PUT", "CONNECT", "OPTIONS", "TRACE", "COPY", "LOCK", "MKCOL", "MOVE", "PROPFIND"
  , "PROPPATCH", "SEARCH", "UNLOCK", "BIND", "REBIND", "UNBIND", "ACL", "REPORT", "MKACTIVITY", "CHECKOUT", "MERGE"
  , "M-SEARCH", "NOTIFY", "SUBSCRIBE", "UNSUBSCRIBE", "PATCH", "PURGE", "MKCALENDAR", "LINK", "UNLINK", "SOURCE", "PRI"
  , "DESCRIBE", "ANNOUNCE", "SETUP", "PLAY", "PAUSE", "TEARDOWN", "GET_PARAMETER", "SET_PARAMETER", "REDIRECT", "RECORD"
  , "FLUSH", "QUERY" ]

-- | Whether a difference is one that is known and accounted for.
expected :: Difference -> Bool
expected d = case d of
  Lenient _ (Just _) -> True
  Strict _ why       -> why `elem` map fst stricter
  _                  -> False

------------------------------------------------------------------------------
-- sessions that look for trouble
------------------------------------------------------------------------------

genSession :: Gen ByteString
genSession = do
  n <- frequency [(3, pure 1), (2, pure 2), (1, pure 3)]
  s <- B.concat <$> replicateM n genMessage
  frequency [(3, pure s), (1, damage s)]

genMessage :: Gen ByteString
genMessage = do
  eol <- frequency [(30, pure "\r\n"), (1, pure "\n"), (1, pure "\r"), (1, pure "\r\r\n")]
  before <- frequency [(20, pure ""), (1, pure "\r\n"), (1, pure "\n"), (1, pure " ")]
  method <- frequency
    [ (12, elements ["GET", "POST", "PUT", "HEAD", "DELETE", "OPTIONS", "PATCH"])
    , (1, elements ["get", "X-Custom", "CONNECT", "G\0T", "GE T", "", "G@T"]) ]
  target <- frequency
    [ (8, elements ["/", "/a/b?c=d", "/%41", "/x?y=1"])
    , (2, elements [ "*", "http://a.example/x", "http://b.example:8080/", "a.example:80", "/a b", "/\xff"
                   , "//x", "/#frag", "", "/?", "/\t", "/%zz", "https://a.example/", "/\x7f" ]) ]
  version <- frequency
    [ (10, pure "HTTP/1.1"), (3, pure "HTTP/1.0")
    , (1, elements [ "HTTP/1.2", "HTTP/2.0", "HTTP/0.9", "http/1.1", "HTTP/1.10", "HTTP/11", "HTTP/1.1 "
                   , "HTTP/01.1", "HTTP/1.", "HTTP/3.0" ]) ]
  host <- frequency
    [ (12, pure ["Host: a.example"])
    , (1, elements [ [], ["Host: a.example", "Host: a.example"], ["Host: a.example", "Host: b.example"]
                   , ["Host:"], ["Host: a.example:80"], ["Host: a b"], ["host:a.example"], ["Host: a.example, b.example"] ]) ]
  others <- resize 4 (listOf genField)
  (framing, content) <- genBody eol
  order <- shuffle (host ++ others ++ framing)
  pure (before <> B.concat [ method, " ", target, " ", version, eol ] <> B.concat (map (<> eol) order) <> eol <> content)

genField :: Gen ByteString
genField = frequency
  [ (6, elements ["Accept: */*", "X-A: 1", "Cookie: a=b", "X-Empty:", "Content-Type: text/plain", "X-A:1", "X-Tab:\ta\tb"])
  , (2, elements [ "Connection: close", "Connection: keep-alive", "Connection: Close", "Connection: close, x"
                 , "Connection: x, close", "Connection:", "Connection: ,", "Connection: upgrade"
                 , "Connection: keep-alive, close", "Connection: clos", "Connection: close\t", "Connection: \"close\"" ])
  , (2, elements [ "X-Sp : v", "X-Fold: a\r\n b", "X-Ctl: a\x01\&b", "X-Del: a\x7f", "X-High: \xe9", " X-Lead: v"
                 , "X:Y: z", ": v", "X-Trail: v  \t", "X-Nul: a\0b", "X-NoColon", "X-Tab\t: v", "X-CR: a\rb"
                 , "X-Paren(): v", "X-\xe9: v", "X-LF: a\nb" ])
  , (1, elements [ "Upgrade: websocket", "Expect: 100-continue", "Expect: other", "TE: trailers", "Keep-Alive: timeout=5"
                 , "Proxy-Connection: keep-alive", "Trailer: X-T" ])
  ]

-- | The fields that frame a body, and the body.
genBody :: ByteString -> Gen ([ByteString], ByteString)
genBody eol = frequency
  [ (6, pure ([], ""))
  , (4, do b <- content
           pure (["Content-Length: " <> C.pack (show (B.length b))], b))
  , (4, do c <- chunked
           pure (["Transfer-Encoding: chunked"], c))
  , (2, do b <- content
           let n = C.pack (show (B.length b))
           form <- elements [ "+" <> n, "0" <> n, n <> " ", "0x" <> n, n <> "," <> n, n <> ", " <> n, n <> "\t"
                            , " " <> n, "", "-1", n <> ".0", "99999999999999999999999", n <> ",", n <> " " <> n
                            , n <> ";", "\"" <> n <> "\"", n <> ", 0", n <> "\v" ]
           sep <- elements [": ", ":", ":\t"]
           pure (["Content-Length" <> sep <> form], b))
  , (1, do b <- content
           let n = C.pack (show (B.length b))
           second <- elements [n, C.pack (show (B.length b + 1)), "0", ""]
           pure (["Content-Length: " <> n, "Content-Length: " <> second], b))
  , (2, do c <- chunked
           coding <- elements [ "Chunked", "CHUNKED", "chunked ", "gzip, chunked", "chunked, gzip", "chunked, chunked"
                              , "identity", "gzip", "chunked;a=b", "xchunked", "chunked\t", ",chunked", "chunked,"
                              , "chunked, identity", "identity, chunked", "\"chunked\"", "chunked\v", "chunke", "" ]
           sep <- elements [": ", ":", ":\t"]
           pure (["Transfer-Encoding" <> sep <> coding], c))
  , (1, do c <- chunked
           elements [ (["Transfer-Encoding: gzip", "Transfer-Encoding: chunked"], c)
                    , (["Transfer-Encoding: chunked", "Transfer-Encoding: gzip"], c)
                    , (["Transfer-Encoding: chunked", "Transfer-Encoding: chunked"], c) ])
  , (2, do c <- chunked
           b <- content
           let n = C.pack (show (B.length b))
           payload <- elements [c, b]
           pure (["Transfer-Encoding: chunked", "Content-Length: " <> n], payload))
  ]
  where
    content = resize 12 (B.pack <$> listOf arbitrary)
    chunked = do
      chunks <- resize 3 (listOf (resize 12 (B.pack <$> listOf1 arbitrary)))
      parts <- mapM chunk chunks
      lastSize <- frequency [(8, pure "0"), (1, elements ["00", "0;a=b", "0 ", " 0", "0x0", "-0", "+0", "0;"])]
      trailers <- frequency
        [ (6, pure [])
        , (2, elements [ ["X-T: 1"], ["X-T:1", "X-U: 2"], ["Content-Length: 3"], ["X-Sp : v"], ["X-T: a\r\n b"]
                       , ["Transfer-Encoding: chunked"], ["Host: b.example"], [": v"], ["X-Empty:"] ]) ]
      end <- frequency [(12, pure eol), (1, pure ""), (1, pure "\n")]
      pure (B.concat parts <> lastSize <> eol <> B.concat (map (<> eol) trailers) <> end)
    chunk c = do
      let n = showHex (B.length c) ""
      size <- frequency
        [ (8, pure (C.pack n))
        , (1, elements [ C.pack (map succUpper n), "0" <> C.pack n, "0x" <> C.pack n, " " <> C.pack n, C.pack n <> " "
                       , C.pack n <> "\t", "+" <> C.pack n, C.pack n <> ";a=b", C.pack n <> ";a=\"b c\"", C.pack n <> " ;a"
                       , C.pack n <> ";", C.pack n <> ";a=", C.pack n <> ";=b", C.pack n <> "; a=b", C.pack n <> ";a=\"b\\\"\""
                       , "000000000000000000000" <> C.pack n, C.pack n <> ";a=b\x01", C.pack n <> ",a" ]) ]
      afterSize <- frequency [(15, pure eol), (1, pure "\n"), (1, pure "\r")]
      afterData <- frequency [(15, pure eol), (1, pure ""), (1, pure "\n"), (1, pure "\r"), (1, pure "x\r\n")]
      pure (size <> afterSize <> c <> afterData)
    succUpper ch = if ch >= 'a' && ch <= 'f' then toEnum (fromEnum ch - 32) else ch

------------------------------------------------------------------------------
-- the property, and the survey
------------------------------------------------------------------------------

prop_agrees :: Property
prop_agrees = withMaxSuccess 20000 $
  forAll genSession $ \session ->
  forAll (splitInput session) $ \pieces ->
    let h = hyperdrive pieces
        l = llhttp pieces
    in counterexample ("hyperdrive: " ++ show h) $ counterexample ("llhttp: " ++ show l) $
       case compareRuns session h l of
         Just d | not (expected d) -> counterexample (show d) False
         _ -> property True

runSurvey :: Int -> IO ()
runSurvey n = do
  cases <- generate (replicateM n (genSession >>= splitInput))
  let shorter a b = if B.length (B.concat a) <= B.length (B.concat b) then a else b
      found = M.fromListWith (\(c, e) (c', e') -> (c + c', shorter e e'))
        [ (d, (1 :: Int, pieces)) | pieces <- cases
        , Just d <- [compareRuns (B.concat pieces) (hyperdrive pieces) (llhttp pieces)] ]
  forM_ (M.toList found) $ \(d, (count, pieces)) -> do
    putStrLn (show count ++ "  " ++ (if expected d then "" else "UNEXPECTED ") ++ show d)
    putStrLn ("    " ++ show (B.concat pieces))
    putStrLn ("    hyperdrive: " ++ show (hyperdrive pieces))
    putStrLn ("    llhttp:     " ++ show (llhttp pieces))
  putStrLn (show (sum (map fst (M.elems found))) ++ " of " ++ show n ++ " differ")
