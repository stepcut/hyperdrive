{-# LANGUAGE OverloadedStrings #-}
-- | Frames against nghttp2 (D12 step 6): nghttp2 has no public function
-- that parses one frame, only whole sessions, so the same client octets
-- go to nghttp2's server session and to the connection core, and what
-- each makes of them is compared: whether the connection ended and with
-- which code, which streams were reset and with which codes, and which
-- requests reached the application.
module Sessions
  ( Verdict(..)
  , Fate(..)
  , genSession
  , ours
  , theirs
  , frameSurvey
  , knownCut
  , prop_sessions
  ) where

import Control.Monad (forM_, replicateM)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.List (sort, sortOn, tails)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word (Word32, Word8)
import Foreign.C.Types (CLong(..), CSize(..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import FrameTests (Wire(..), encodeWire, genNearMiss, genSetting, genValid)
import Hyperdrive.HPACK.Encode (encodeBlock, newEncoder)
import Hyperdrive.HTTP2.Connection
import Hyperdrive.HTTP2.Frame
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck hiding ((.&.))

foreign import ccall unsafe "h2_nghttp2_session"
  c_session :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO CLong

-- | What a server made of a client's octets.
data Verdict = Verdict
  { vFate     :: Fate
  , vResets   :: [(Word32, ErrorCode)]   -- ^ streams reset, sorted
  , vRequests :: [Word32]                -- ^ requests passed to the application, sorted
  } deriving (Eq, Show)

data Fate
  = Open                    -- ^ the connection goes on
  | GoneWith ErrorCode      -- ^ a GOAWAY with this code (the first with an error, else NO_ERROR)
  | ClosedQuietly           -- ^ closed without a GOAWAY
  | Fatal String            -- ^ nghttp2's receive gave up with this error
  deriving (Eq, Ord, Show)

-- | The frames a server wrote: the resets and the GOAWAYs.
written :: ByteString -> ([(Word32, ErrorCode)], [ErrorCode])
written = go [] []
  where
    go rs gs b = case parseFrame 0xFFFFFF b of
      Parsed f n -> case f of
        RstStreamFrame (StreamId s) code -> go ((s, code) : rs) gs (B.drop n b)
        GoAwayFrame _ code _ -> go rs (code : gs) (B.drop n b)
        _ -> go rs gs (B.drop n b)
      _ -> (reverse rs, reverse gs)

fateOf :: [ErrorCode] -> Bool -> Fate
fateOf gs closed = case [g | g <- gs, g /= NoError] of
  (g : _) -> GoneWith g
  [] | not (null gs) -> GoneWith NoError
     | closed -> ClosedQuietly
     | otherwise -> Open

-- | The connection core's verdict (time 0, no handler answering).
ours :: ByteString -> Verdict
ours input =
  let (c0, o0) = newConn defaultConfig 0
      (_, o1) = step 0 (Received input) c0
      outs = o0 ++ o1
      (rs, gs) = written (B.concat (concat [bs | Send bs <- outs]))
      closed = not (null [() | Close <- outs])
  in Verdict (fateOf gs closed) (sort rs) (sort [s | NewRequest (StreamId s) _ <- outs])

-- | nghttp2's verdict.
theirs :: ByteString -> (Verdict, [(Word8, Word8, Word32)])
theirs input = unsafePerformIO $ BU.unsafeUseAsCStringLen (input <> "\0") $ \(p, _) -> do
  out <- withOutput 65536 $ c_session (castPtr p) (fromIntegral (B.length input))
  let rs = records out
      (resets, gs) = written (B.concat [b | ('O', b) <- rs])
      fate = case [e | ('X', e) <- rs] of
        (e : _) -> Fatal (C.unpack e)
        [] -> fateOf gs False
      frames = [ (B.index b 0, B.index b 1, le32 (B.drop 2 b)) | ('F', b) <- rs ]
  pure (Verdict fate (sort resets) (sort [le32 b | ('R', b) <- rs]), frames)
  where
    le32 b = sum [fromIntegral (B.index b i) `shiftL` (8 * i) | i <- [0 .. 3]]

withOutput :: Int -> (Ptr Word8 -> CSize -> IO CLong) -> IO ByteString
withOutput cap f = do
  fp <- mallocForeignPtrBytes cap
  r <- withForeignPtr fp $ \p -> f p (fromIntegral cap)
  case r of
    -1 -> withOutput (cap * 4) f
    n | n < 0 -> fail ("nghttp2 shim failed: " ++ show n)
      | otherwise -> pure (B.copy (BI.BS fp (fromIntegral n)))

records :: ByteString -> [(Char, ByteString)]
records b
  | B.length b < 5 = []
  | otherwise =
      let n = sum [fromIntegral (B.index b i) `shiftL` (8 * (i - 1)) | i <- [1 .. 4]]
      in (BI.w2c (B.head b), B.take n (B.drop 5 b)) : records (B.drop (5 + n) b)

------------------------------------------------------------------------------
-- sessions
------------------------------------------------------------------------------

-- | A client's octets: the preface and a SETTINGS, then valid requests
-- (some split into CONTINUATIONs, some with content), frames of every
-- type (valid on their own, wherever they fall), and sometimes, last, a
-- frame that breaks one rule.
genSession :: Gen ByteString
genSession = do
  settings <- listOf (genSetting `suchThat` (\(_, _, s) -> s /= Nothing))
  n <- choose (0, 8)
  items <- go (newEncoder 4096) 1 n
  miss <- frequency [(2, pure []), (1, (\(_, w, _) -> [w]) <$> genNearMiss)]
  let settingsFrame = encodeWire (Wire 0x4 0 False 0 (B.concat [be16 i <> be32 v | (i, v, _) <- settings]))
  pure (B.concat (connectionPreface : settingsFrame : items ++ miss))
  where
    go _ _ 0 = pure []
    go enc sid k = do
      choice <- frequency [(4, pure 'r'), (3, pure 'f')]
      case choice of
        'r' -> do
          method <- elements ["GET", "POST"]
          extra <- elements [[], [("x-a", "1")], [("content-length", "3")], [("te", "trailers")]]
          let fields = [(":method", method), (":scheme", "http"), (":authority", "example.com"), (":path", "/")] ++ extra
              (block, enc') = encodeBlock enc fields
          endStream <- arbitrary
          cut <- choose (0, B.length block)
          let hs = if cut == 0 || cut == B.length block
                     then encodeWire (Wire 0x1 (0x4 + flag endStream) False sid block)
                     else encodeWire (Wire 0x1 (flag endStream) False sid (B.take cut block))
                          <> encodeWire (Wire 0x9 0x4 False sid (B.drop cut block))
          content <- if endStream then pure B.empty else do
            body <- elements ["", "abc", "abcd"]
            end <- arbitrary
            pure (if B.null body && not end then B.empty
                  else encodeWire (Wire 0x0 (flag end) False sid body))
          rest <- go enc' (sid + 2) (k - 1 :: Int)
          pure (hs <> content : rest)
        _ -> do
          (w, _) <- genValid
          -- streams near those in use, not anywhere in 2^31
          s <- if wStream w == 0 then pure 0 else elements [1, 3, 5, 7, sid, sid + 2]
          -- HEADERS, CONTINUATION and PUSH_PROMISE with random field
          -- blocks only test HPACK's errors; the requests above cover them
          if wType w `elem` [0x1, 0x5, 0x9] then go enc sid k
          else (encodeWire w { wStream = s } :) <$> go enc sid (k - 1)
    flag b = if b then 0x1 else 0
    be16 x = B.pack [fromIntegral (x `shiftR` 8), fromIntegral x]
    be32 x = B.pack [fromIntegral (x `shiftR` s) | s <- [24, 16, 8, 0 :: Int]]

------------------------------------------------------------------------------
-- the known differences (docs/http2-core-design.md, "Frames against nghttp2")
------------------------------------------------------------------------------

-- | The session up to the first frame where the core and nghttp2 are
-- known to differ, and which difference that is. What comes after such a
-- frame is not compared: the two connections are in different states.
knownCut :: ByteString -> (ByteString, Maybe String)
knownCut input = go (B.length connectionPreface) initial
  where
    initial = Track 0 S.empty 65535 M.empty Nothing Nothing
    go at t
      | B.length rest < 9 || B.length rest < 9 + len = (input, Nothing)
      -- what the frame's header alone tells, whatever its payload
      | typ == 0x4 && not ack && invalidBeforeLast payload =
          cut "a setting named twice, an earlier value invalid: an error for us (Section 6.5.2), unchecked by nghttp2, which takes only the last"
      | typ == 0x4 && not ack && 8 `elem` ids payload =
          cut "SETTINGS_ENABLE_CONNECT_PROTOCOL: unknown to us (RFC 8441 unsupported), checked by nghttp2"
      | typ == 0x2 && sid == 0 = cut "PRIORITY on stream 0: PROTOCOL_ERROR (Section 6.3), ignored by nghttp2"
      | typ `elem` [0x0, 0x1] && S.member sid (tEnded t) =
          cut "DATA or HEADERS on a half-closed stream: a stream error, which nghttp2 makes a connection error"
      | typ == 0x4 && not ack, Just w <- lookup 4 (pairs payload)
          , any (\(_, sent) -> w + sent > 0x7FFFFFFF) (M.toList (tUpdates t)) =
          cut "a stream window past 2^31-1: a stream error, which nghttp2 makes a connection error"
      | typ == 0x4 && not ack && isJust' (tFirstSettings t)
          && any (\(i, v) -> i == 9 && Just v /= tNoPriorities t) (pairs payload) =
          cut "SETTINGS_NO_RFC7540_PRIORITIES changed: RFC 9218's MAY, taken by nghttp2"
      | typ == 0x0 && B.index rest 4 .&. 0x8 /= 0 && len > 0 && fromIntegral (B.index payload 0) >= len
          && sid /= 0 && sid <= tHighest t && odd sid =
          cut "DATA with padding as long as its payload on a stream opened before: PROTOCOL_ERROR (Section 6.1), discarded unchecked by nghttp2 if the stream is closed"
      | typ == 0x8 && len == 4 && sid /= 0 && sid <= tHighest t && odd sid
          && B.drop 9 (B.take 13 rest) `elem` [B.pack [0, 0, 0, 0], B.pack [0x80, 0, 0, 0]] =
          cut "a WINDOW_UPDATE of 0 on a stream opened before: the frame error (Section 6.9, widened by F2), discarded unchecked by nghttp2 if the stream is closed"
      | otherwise = case parseFrame defaultMaxFrameSize rest of
          Refused (FrameError _ FrameSizeError _)
            | typ `elem` [0x0, 0x1] -> cut "DATA or HEADERS too short for its padding or priority: FRAME_SIZE_ERROR (Section 4.2), nghttp2's PROTOCOL_ERROR"
            | typ == 0x8 && sid /= 0 && (even sid || sid > tHighest t) ->
                cut "a WINDOW_UPDATE of the wrong length on an idle stream: FRAME_SIZE_ERROR (Section 6.9), nghttp2's PROTOCOL_ERROR"
          Parsed f _ -> case f of
            GoAwayFrame {} -> cut "the client's GOAWAY: we close once the streams are done, at once with an error (C11); nghttp2 leaves it to the application"
            SettingsFrame _ ss
              | Just v <- noPriorities ss, Just v /= tNoPriorities t, isJust' (tFirstSettings t) ->
                  cut "SETTINGS_NO_RFC7540_PRIORITIES changed: RFC 9218's MAY, taken by nghttp2"
              | otherwise ->
                  let initial' = foldl (\w x -> case x of InitialWindowSize v -> fromIntegral v; _ -> w) (tInitial t) ss
                  in if any (\(_, sent) -> initial' + sent > 0x7FFFFFFF) (M.toList (tUpdates t))
                       then cut "a stream window past 2^31-1: a stream error, which nghttp2 makes a connection error"
                       else next t { tInitial = initial'
                                   , tFirstSettings = Just ()
                                   , tNoPriorities = maybe (Just (maybe 0 id (noPriorities ss))) Just (tNoPriorities t) }
            HeadersFrame (StreamId s') end endHeaders _
              | not endHeaders && not (blockEnds (B.drop (9 + len) rest) s') -> cut "a field block unfinished at the end: nghttp2 acts on part of it"
              | otherwise -> next t { tHighest = max s' (tHighest t), tEnded = if end then S.insert s' (tEnded t) else tEnded t }
            DataFrame (StreamId s') end _ _ -> next t { tEnded = if end then S.insert s' (tEnded t) else tEnded t }
            WindowUpdateFrame (StreamId s') n | s' /= 0 ->
              let sent = M.findWithDefault 0 s' (tUpdates t) + fromIntegral n
              in if tInitial t + sent > 0x7FFFFFFF
                   then cut "a stream window past 2^31-1: a stream error, which nghttp2 makes a connection error"
                   else next t { tUpdates = M.insert s' sent (tUpdates t) }
            _ -> next t
          _ -> next t
      where
        rest = B.drop at input
        len = sum [fromIntegral (B.index rest i) `shiftL` (8 * (2 - i)) | i <- [0 .. 2]]
        typ = B.index rest 3
        ack = B.index rest 4 .&. 0x1 /= 0
        sid = sum [fromIntegral (B.index rest (5 + i)) `shiftL` (8 * (3 - i)) | i <- [0 .. 3]] .&. 0x7FFFFFFF :: Word32
        payload = B.take len (B.drop 9 rest)
        cut reason = (B.take at input, Just reason)
        next t' = go (at + 9 + len) t'
    ids p = map fst (pairs p)
    pairs p = [ ( fromIntegral (B.index p k) * 256 + fromIntegral (B.index p (k + 1)) :: Int
                , sum [fromIntegral (B.index p (k + 2 + j)) `shiftL` (8 * (3 - j)) | j <- [0 .. 3]] :: Int )
              | k <- [0, 6 .. B.length p - 6] ]
    -- a setting named again later in the frame, with a value here that
    -- Section 6.5.2 refuses
    invalidBeforeLast p = or [ any ((== i) . fst) later && invalid i v | ((i, v), later) <- zip (pairs p) (drop 1 (tails (pairs p))) ]
    invalid i v = (i == 2 && v > 1) || (i == 4 && v > 0x7FFFFFFF) || (i == 5 && (v < 0x4000 || v > 0xFFFFFF)) || (i == 9 && v > 1)
    -- the value that stands: the last (Section 6.5)
    noPriorities ss = case reverse [b | NoRFC7540Priorities b <- ss] of
      (b : _) -> Just (if b then 1 else 0 :: Int)
      [] -> Nothing
    isJust' = maybe False (const True)
    -- whether a CONTINUATION with END_HEADERS on the stream follows
    blockEnds b s' = case parseFrame defaultMaxFrameSize b of
      Parsed (ContinuationFrame (StreamId x) True _) _ -> x == s'
      Parsed (ContinuationFrame (StreamId x) False _) n -> x == s' && blockEnds (B.drop n b) s'
      _ -> False

data Track = Track
  { tHighest       :: Word32
  , tEnded         :: S.Set Word32          -- ^ streams the client has ended
  , tInitial       :: Int                   -- ^ the client's INITIAL_WINDOW_SIZE
  , tUpdates       :: M.Map Word32 Int      -- ^ WINDOW_UPDATE increments by stream
  , tFirstSettings :: Maybe ()
  , tNoPriorities  :: Maybe Int             -- ^ the value of the first SETTINGS
  }

-- | The two verdicts agree on each session up to its first known
-- difference.
prop_sessions :: Property
prop_sessions = forAll (resize 6 genSession) $ \s ->
  let (prefix, reason) = knownCut s
      (o, t) = comparable (ours prefix) (fst (theirs prefix))
  in counterexample (unlines [maybe "no known difference" ("cut at: " ++) reason, "ours:    " ++ show o, "nghttp2: " ++ show t])
       (o === t)

------------------------------------------------------------------------------
-- the survey
------------------------------------------------------------------------------

-- | The verdicts as compared: the resets only while both connections
-- live (nghttp2 drops the RST_STREAMs it had queued when it ends a
-- connection with an error; the core sends them before its GOAWAY).
comparable :: Verdict -> Verdict -> (Verdict, Verdict)
comparable o t
  | lives (vFate o) && lives (vFate t) = (o, t)
  | otherwise = (o { vResets = [] }, t { vResets = [] })
  where
    lives Open = True
    lives (GoneWith NoError) = True
    lives _ = False

-- | The kind of a disagreement: what differs, in words.
kind :: Verdict -> Verdict -> String
kind o0 t0 = unwords $
  [ "fate: ours " ++ show (vFate o) ++ ", nghttp2 " ++ show (vFate t) | vFate o /= vFate t ]
  ++ [ "resets: ours " ++ show (map snd (vResets o)) ++ ", nghttp2 " ++ show (map snd (vResets t))
     | vResets o /= vResets t ]
  ++ [ "requests: ours " ++ show (length (vRequests o)) ++ ", nghttp2 " ++ show (length (vRequests t))
     | vRequests o /= vRequests t ]
  where (o, t) = comparable o0 t0

-- | Every kind of disagreement in n sessions: how often, and the
-- shortest example, with the frames nghttp2 accepted.
frameSurvey :: Int -> IO ()
frameSurvey n = do
  sessions <- generate (replicateM n (resize 6 genSession))
  let cuts = map knownCut sessions
      diffs = M.fromListWith pick
        [ (kind o t, (1 :: Int, s))
        | (s, _) <- cuts, let (o, t) = comparable (ours s) (fst (theirs s)), o /= t ]
      pick (a, s1) (b, s2) = (a + b, if B.length s1 <= B.length s2 then s1 else s2)
      reasons = M.fromListWith (+) [ (r, 1 :: Int) | (_, Just r) <- cuts ]
  putStrLn (show n ++ " sessions, each up to its first known difference:")
  forM_ (sortOn (negate . snd) (M.toList reasons)) $ \(r, k) -> putStrLn ("  " ++ show k ++ "x  " ++ r)
  putStrLn (show (sum (map fst (M.elems diffs))) ++ " with a difference before it")
  forM_ (sortOn (negate . fst . snd) (M.toList diffs)) $ \(k, (count, s)) -> do
    let (t, frames) = theirs s
    putStrLn ""
    putStrLn (show count ++ "x  " ++ k)
    putStrLn ("  ours:    " ++ show (ours s))
    putStrLn ("  nghttp2: " ++ show t)
    putStrLn ("  nghttp2 accepted: " ++ show frames)
    putStrLn ("  octets after the preface: " ++ hex (B.drop (B.length connectionPreface) s))
  where
    hex = concatMap (\w -> [digit (w `shiftR` 4), digit (w .&. 15)]) . B.unpack
    digit x = "0123456789abcdef" !! fromIntegral x
