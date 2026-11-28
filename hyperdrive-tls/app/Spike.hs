{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | The TLS spike (docs/tls-spike-design.md).
--
--   tls-spike serve PORT CERT KEY      handshakes with real clients over TCP
--   tls-spike bench CERT KEY N         N handshakes in process, then records
--   tls-spike quic CERT KEY            a QUIC handshake in process
--
-- Every call into the shim is timed: the handshake's steps (@safe@
-- since the spike's first run) by percentile, against D3's budget of
-- about 100 µs for an @unsafe@ call.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, finally)
import Control.Monad (forever, unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.IORef
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperdrive.TLS.Shim
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import Text.Printf (printf)

main :: IO ()
main = hSetBuffering stdout LineBuffering >> getArgs >>= \args -> case args of
  ["serve", port, cert, key] -> serve port cert key
  ["bench", cert, key, n] -> bench cert key (read n)
  ["quic", cert, key] -> quic cert key
  _ -> putStrLn "tls-spike serve PORT CERT KEY | bench CERT KEY N | quic CERT KEY" >> exitFailure

------------------------------------------------------------------------------
-- timing the calls
------------------------------------------------------------------------------

-- | The longest @unsafe@ call and the time spent in all, in ns.
data Clock = Clock { longest :: IORef Word64, total :: IORef Word64, shakes :: IORef [Word64] }

newClock :: IO Clock
newClock = Clock <$> newIORef 0 <*> newIORef 0 <*> newIORef []

timed :: Clock -> IO a -> IO a
timed clock act = do
  t0 <- getMonotonicTimeNSec
  r <- act
  t1 <- getMonotonicTimeNSec
  modifyIORef' (longest clock) (max (t1 - t0))
  modifyIORef' (total clock) (+ (t1 - t0))
  pure r

------------------------------------------------------------------------------
-- the session's buffers
------------------------------------------------------------------------------

feedBytes :: Clock -> Ptr Session -> ByteString -> IO ()
feedBytes clock t bs = BU.unsafeUseAsCStringLen bs $ \(p, n) -> do
  r <- timed clock (feed t (castPtr p) (fromIntegral n))
  when (r /= 0) (fail "feed")

-- | The ciphertext waiting to be sent.
drainAll :: Clock -> Ptr Session -> IO ByteString
drainAll clock t = do
  n <- fromIntegral <$> timed clock (pendingOut t)
  if n == 0 then pure B.empty
    else BI.create n $ \p -> () <$ timed clock (drain t p (fromIntegral n))

readAll :: Clock -> Ptr Session -> IO (Maybe ByteString)
readAll clock t = allocaBytes 16384 $ \p -> go p []
  where
    go p acc = do
      n <- timed clock (readPlain t p 16384)
      if n > 0 then B.packCStringLen (castPtr p, fromIntegral n) >>= \b -> go p (b : acc)
      else if n == 0 then pure (Just (B.concat (reverse acc)))
      else pure (if null acc then Nothing else Just (B.concat (reverse acc)))

writeAll :: Clock -> Ptr Session -> ByteString -> IO ()
writeAll clock t bs = BU.unsafeUseAsCStringLen bs $ \(p, n) -> do
  r <- timed clock (writePlain t (castPtr p) (fromIntegral n))
  when (r /= fromIntegral n) (fail "write")

-- | One handshake step; a pending signature made in the @safe@ call
-- (timed apart) and the step retried.
step :: Clock -> IORef Word64 -> Ptr Session -> IO CInt
step clock signing t = do
  a0 <- getMonotonicTimeNSec
  r <- timed clock (handshake t)
  a1 <- getMonotonicTimeNSec
  modifyIORef' (shakes clock) ((a1 - a0) :)
  case r of
    WantSign -> do
      t0 <- getMonotonicTimeNSec
      s <- tlsSign t
      t1 <- getMonotonicTimeNSec
      modifyIORef' signing (+ (t1 - t0))
      when (s /= 0) (errorText >>= fail . ("sign: " ++))
      step clock signing t
    _ -> pure r

errorText :: IO String
errorText = allocaBytes 256 $ \p -> lastError p 256 >> peekCString p

agreed :: Ptr Session -> IO (String, String, String, String)
agreed t = do
  g <- keyGroup t >>= peekCString . unConstPtr
  v <- (++ (" " ++ g)) <$> (version t >>= peekCString . unConstPtr)
  c <- cipher t >>= peekCString . unConstPtr
  a <- alloca $ \lp -> do
    p <- unConstPtr <$> alpn t lp
    n <- peek lp
    if p == nullPtr then pure "" else C.unpack <$> B.packCStringLen (castPtr p, fromIntegral n)
  sn <- servername t >>= \(ConstPtr p) -> if p == nullPtr then pure "" else peekCString p
  pure (v, c, a, sn)

------------------------------------------------------------------------------
-- serve: real clients
------------------------------------------------------------------------------

serve :: String -> FilePath -> FilePath -> IO ()
serve port cert key = do
  cfg <- withCString cert $ \c -> withCString key $ \k -> configServer c k 0
  when (cfg == nullPtr) (errorText >>= fail . ("config: " ++))
  addr : _ <- N.getAddrInfo (Just N.defaultHints { N.addrFlags = [N.AI_PASSIVE], N.addrSocketType = N.Stream })
                            (Just "127.0.0.1") (Just port)
  bracket (N.openSocket addr) N.close $ \sock -> do
    N.setSocketOption sock N.ReuseAddr 1
    N.bind sock (N.addrAddress addr)
    N.listen sock 64
    printf "listening on 127.0.0.1:%s\n" port
    forever $ do
      (s, _) <- N.accept sock
      forkIO (connection cfg s `finally` N.close s)

connection :: Ptr Config -> N.Socket -> IO ()
connection cfg s = bracket (sessionNew cfg 1 nullPtr) sessionFree $ \t -> do
  clock <- newClock
  signing <- newIORef 0
  let flush = drainAll clock t >>= \out -> unless (B.null out) (NB.sendAll s out)
      shake = do
        r <- step clock signing t
        flush
        case r of
          HandshakeDone -> pure True
          WantRead -> NB.recv s 16384 >>= \bs ->
            if B.null bs then pure False else feedBytes clock t bs >> shake
          _ -> errorText >>= \e -> putStrLn ("handshake failed: " ++ e) >> pure False
  ok <- shake
  when ok $ do
    (v, c, a, sn) <- agreed t
    sig <- signatureAlgorithm t
    l <- readIORef (longest clock)
    g <- readIORef signing
    printf "%s %s alpn=%s sni=%s signature=0x%04x  longest call %.1f µs, signing %.1f µs\n"
      v c (if null a then "-" else a) (if null sn then "-" else sn) sig
      (fromIntegral l / 1000 :: Double) (fromIntegral g / 1000 :: Double)
    -- for http/1.1 (or no ALPN), one response; for h2, the close
    when (a /= "h2") $ do
      let request acc = do
            bs <- NB.recv s 16384
            if B.null bs then pure acc else do
              feedBytes clock t bs
              more <- readAll clock t
              let acc' = acc <> maybe "" id more
              if "\r\n\r\n" `B.isInfixOf` acc' then pure acc' else request acc'
      _ <- request ""
      writeAll clock t "HTTP/1.1 200 OK\r\ncontent-length: 6\r\nconnection: close\r\n\r\nhello\n"
      flush
    _ <- timed clock (shutdown t)
    flush

------------------------------------------------------------------------------
-- bench: handshakes and records in process
------------------------------------------------------------------------------

-- | A client and a server, octets passed between them until both are
-- done; the server's time and its longest call.
handshakeOnce :: Ptr Config -> Ptr Config -> Clock -> IORef Word64 -> IO (Ptr Session, Ptr Session)
handshakeOnce scfg ccfg sclock signing = do
  srv <- sessionNew scfg 1 nullPtr
  cli <- withCString "localhost" (sessionNew ccfg 0)
  cclock <- newClock
  cSigning <- newIORef 0
  let go n = do
        when (n > 20) (fail "handshake does not finish")
        rc <- step cclock cSigning cli
        drainAll cclock cli >>= \o -> unless (B.null o) (feedBytes sclock srv o)
        rs <- step sclock signing srv
        drainAll sclock srv >>= \o -> unless (B.null o) (feedBytes cclock cli o)
        when (rc == HandshakeError || rs == HandshakeError) (errorText >>= fail . ("handshake: " ++))
        unless (rc == HandshakeDone && rs == HandshakeDone) (go (n + 1 :: Int))
  go 0
  pure (srv, cli)

bench :: FilePath -> FilePath -> Int -> IO ()
bench cert key n = do
  ccfg <- configClient 0
  for12 False ccfg
  for12 True ccfg
  where
    for12 tls12 ccfg = do
      scfg <- withCString cert $ \c -> withCString key $ \k -> configServer c k 0
      when (scfg == nullPtr) (errorText >>= fail . ("config: " ++))
      when tls12 (() <$ configLimitVersion scfg 1)
      sclock <- newClock
      signing <- newIORef 0
      t0 <- getMonotonicTimeNSec
      pairs <- mapM (const (handshakeOnce scfg ccfg sclock signing)) [1 .. n]
      t1 <- getMonotonicTimeNSec
      (srv, cli) : _ <- pure pairs
      (v, c, a, _) <- agreed srv
      servT <- readIORef (total sclock)
      sig <- readIORef signing
      l <- readIORef (longest sclock)
      hs <- sort <$> readIORef (shakes sclock)
      let pct q = fromIntegral (hs !! min (length hs - 1) (length hs * q `div` 1000)) / 1000 :: Double
      printf "%s %s alpn=%s: %d handshakes, server %.1f µs each (signing %.1f µs of it); both sides %.0f a second\n"
        v c a n (fromIntegral (servT + sig) / fromIntegral n / 1000 :: Double)
        (fromIntegral sig / fromIntegral n / 1000 :: Double)
        (fromIntegral n / (fromIntegral (t1 - t0) / 1e9) :: Double)
      printf "  the server's handshake calls (%d): median %.1f µs, 90%% %.1f, 99%% %.1f, 99.9%% %.1f, longest %.1f\n"
        (length hs) (pct 500) (pct 900) (pct 990) (pct 999) (fromIntegral l / 1000 :: Double)
      -- records: 16 KiB encrypted by the server and decrypted by the client
      let record = B.replicate 16384 0x61
          m = 20000 :: Int
      enc <- newClock
      dec <- newClock
      mapM_ (\_ -> do writeAll enc srv record
                      o <- drainAll enc srv
                      feedBytes dec cli o
                      Just p <- readAll dec cli
                      when (B.length p /= 16384) (fail "record")) [1 .. m]
      e <- readIORef (total enc)
      d <- readIORef (total dec)
      el <- readIORef (longest enc)
      printf "  16 KiB records: encrypt and drain %.2f µs, feed and decrypt %.2f µs (%.2f GB/s, %.2f GB/s), longest call %.1f µs\n"
        (fromIntegral e / fromIntegral m / 1000 :: Double) (fromIntegral d / fromIntegral m / 1000 :: Double)
        (16384 * fromIntegral m / fromIntegral e :: Double) (16384 * fromIntegral m / fromIntegral d :: Double)
        (fromIntegral el / 1000 :: Double)
      mapM_ (\(s, k) -> sessionFree s >> sessionFree k) pairs
      configFree scfg

------------------------------------------------------------------------------
-- quic: the handshake by encryption level, in process
------------------------------------------------------------------------------

quic :: FilePath -> FilePath -> IO ()
quic cert key = do
  scfg <- withCString cert $ \c -> withCString key $ \k -> configServer c k 1
  ccfg <- configClient 1
  when (scfg == nullPtr || ccfg == nullPtr) (errorText >>= fail . ("config: " ++))
  srv <- sessionNew scfg 1 nullPtr
  cli <- withCString "localhost" (sessionNew ccfg 0)
  clock <- newClock
  signing <- newIORef 0
  -- each level's octets handed over, the receiver stepped after each: it
  -- takes a level's data only once it has that level's keys, as QUIC
  -- decrypts a level's packets only then
  let pass from to = allocaBytes 65536 $ \buf -> alloca $ \lp -> do
        let loop = do
              n <- timed clock (quicTake from lp buf 65536)
              when (n < 0) (fail "take")
              when (n > 0) $ do
                l <- peek lp
                printf "  %d octets at level %d\n" (fromIntegral n :: Int) (fromIntegral l :: Int)
                r <- timed clock (quicProvide to l buf (fromIntegral n))
                when (r /= 0) (errorText >>= fail . ("provide: " ++))
                rr <- step clock signing to
                when (rr == HandshakeError) (errorText >>= fail . ("handshake: " ++))
                loop
        loop
      go n = do
        when (n > 20) (fail "handshake does not finish")
        rc <- step clock signing cli
        putStrLn "client ->" >> pass cli srv
        rs <- step clock signing srv
        putStrLn "server ->" >> pass srv cli
        when (rc == HandshakeError || rs == HandshakeError) (errorText >>= fail . ("handshake: " ++))
        rc' <- step clock signing cli
        rs' <- step clock signing srv
        unless (rc' == HandshakeDone && rs' == HandshakeDone) (go (n + 1 :: Int))
  go (0 :: Int)
  (v, c, a, _) <- agreed srv
  printf "%s %s alpn=%s\n" v c a
  ss <- secrets srv
  cs <- secrets cli
  -- each level's server write secret is the client's read secret, and the
  -- other way round
  let check :: (Int, Int) -> ByteString -> IO Bool
      check (lvl, dir) s = case M.lookup (lvl, 1 - dir) cs of
        Just s' | s' == s -> printf "  level %d, server %s: agrees (%d octets)\n" lvl (if dir == 1 then "write" else "read" :: String) (B.length s) >> pure True
        _ -> printf "  level %d, server %s: NO MATCH\n" lvl (if dir == 1 then "write" else "read" :: String) >> pure False
  oks <- mapM (uncurry check) (M.toList ss)
  printf "secrets: server %s, client %s; %s\n" (show (sort (M.keys ss))) (show (sort (M.keys cs)))
    (if and oks && M.size ss == M.size cs && not (null oks) then "all agree" else "MISMATCH" :: String)
  l <- readIORef (longest clock)
  printf "longest unsafe call %.1f µs\n" (fromIntegral l / 1000 :: Double)
  where
    secrets t = allocaBytes 64 $ \buf -> alloca $ \lp -> alloca $ \dp -> do
      let loop m = do
            n <- quicSecret t lp dp buf 64
            if n <= 0 then pure m else do
              l <- peek lp
              d <- peek dp
              s <- B.packCStringLen (castPtr buf, fromIntegral n)
              loop (M.insert (fromIntegral l :: Int, fromIntegral d :: Int) s m)
      loop M.empty
