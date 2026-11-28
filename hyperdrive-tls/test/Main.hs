{-# LANGUAGE OverloadedStrings #-}
-- | hyperdrive-tls against itself: a client and a server session in one
-- process, ciphertext passed between them (docs/tls-design.md, T10).
module Main (main) where

import Data.ByteString (ByteString)
import Data.Word (Word16)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Hyperdrive.TLS
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain $ testGroup "hyperdrive-tls"
  [ testProperty "handshakes: each key, each version" (once prop_handshakes)
  , testProperty "a certificate chain" (once prop_chain)
  , testProperty "configurations refused" (once prop_configErrors)
  , testProperty "ALPN" (once prop_alpn)
  , testProperty "groups, in the server's order" (once prop_groups)
  , testProperty "versions and ciphers refused" (once prop_refused)
  , testProperty "no session tickets (T7)" (once prop_noTickets)
  , testProperty "data both ways, ciphertext cut anywhere" prop_data
  , testProperty "close_notify is the end, not an error" (once prop_close)
  , testProperty "garbage after the handshake: an error and an alert" (once prop_garbage)
  , testProperty "input buffered beyond the bound is refused" (once prop_bound)
  , testProperty "a client's KeyUpdate is answered" (once prop_keyUpdate)
  , testProperty "the server's name (SNI)" (once prop_sni)
  , testProperty "QUIC: the handshake by level, secrets agreeing" (once prop_quic)
  ]

------------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------------

load :: FilePath -> IO ByteString
load f = B.readFile ("test/data/" ++ f)

server :: FilePath -> FilePath -> IO ConfigSettings
server cert key = do
  c <- load cert
  k <- load key
  pure (defaultConfigSettings Server) { csCertificate = Just (c, k), csALPN = ["h2", "http/1.1"] }

-- a client offering P-384 as browsers do: in TLS 1.2 an ECDSA
-- certificate's curve must be one the client offers (a P-384 certificate
-- with only P-256 offered fails with BAD_ECC_CERT)
client :: ConfigSettings
client = (defaultConfigSettings Client) { csALPN = ["h2", "http/1.1"], csGroups = Just "X25519MLKEM768:X25519:P-256:P-384" }

config :: ConfigSettings -> IO Config
config cs = newConfig cs >>= either (fail . show) pure

-- | A finished handshake: the server session, the client session, and
-- what the server sent after its last step.
data Pair = Pair { pServer :: Session, pClient :: Session, pAfter :: ByteString }

handshakePair :: ConfigSettings -> ConfigSettings -> Maybe ByteString -> IO (Either TLSError Pair)
handshakePair scs ccs name = do
  sc <- config scs
  cc <- config ccs
  s <- newSession sc Nothing 65536
  c <- newSession cc name 65536
  let go :: Int -> Bool -> Bool -> ByteString -> IO (Either TLSError Pair)
      go n sDone cDone sent
        | sDone && cDone = pure (Right (Pair s c sent))
        | n > 20 = pure (Left (TLSError "the handshake does not finish"))
        | otherwise = do
            rc <- if cDone then pure (Right Done) else handshake c
            co <- takeOutput c
            fc <- if B.null co then pure (Right ()) else feed s co
            rs <- if sDone then pure (Right Done) else handshake s
            so <- takeOutput s
            fs <- if B.null so then pure (Right ()) else feed c so
            case (rc, rs, fc, fs) of
              (Left e, _, _, _) -> pure (Left e)
              (_, Left e, _, _) -> pure (Left e)
              (_, _, Left e, _) -> pure (Left e)
              (_, _, _, Left e) -> pure (Left e)
              (Right a, Right b, _, _) ->
                -- what the server sent in the step that finished its handshake
                let sDone' = b == Done
                in go (n + 1) sDone' (a == Done) (if sDone' && not sDone then so else sent)
  go 0 False False B.empty

pairOrFail :: ConfigSettings -> ConfigSettings -> IO Pair
pairOrFail s c = handshakePair s c (Just "localhost") >>= either (fail . show) pure

-- | Ciphertext from one session into the other, in pieces of these sizes.
pass :: Session -> Session -> [Int] -> IO ()
pass from to cuts = do
  out <- takeOutput from
  let pieces = chop (cycle (filter (> 0) cuts ++ [maxBound])) out
  mapM_ (\p -> feed to p >>= either (fail . show) pure) pieces
  where
    chop _ b | B.null b = []
    chop (k : ks) b = B.take k b : chop ks (B.drop k b)
    chop [] b = [b]

-- | All the plaintext a session has.
receiveAll :: Session -> IO (ByteString, Received)
receiveAll s = go []
  where
    go acc = receive s 16384 >>= \r -> case r of
      Plain b -> go (b : acc)
      other -> pure (B.concat (reverse acc), other)

------------------------------------------------------------------------------
-- the properties
------------------------------------------------------------------------------

prop_handshakes :: Property
prop_handshakes = ioProperty $ do
  results <- mapM one [ (k, v) | k <- ["ec", "p384", "rsa"], v <- [TLS12, TLS13] ]
  -- an Ed25519 key loads; AWS-LC's client, as browsers, does not accept
  -- its signatures unless asked, so no handshake with it here
  ed <- server "ed25519.pem" "ed25519.key" >>= newConfig
  pure (conjoin results .&&. counterexample "an Ed25519 key loads" (either (const False) (const True) ed))
  where
    one (k, v) = do
      scs <- server (k ++ ".pem") (k ++ ".key")
      p <- handshakePair scs client { csVersions = (v, v) } (Just "localhost") >>= either (fail . ((show (k, v) ++ ": ") ++) . show) pure
      a <- agreed (pServer p)
      ca <- agreed (pClient p)
      pure $ counterexample (show (k, v, a, ca)) $
        agreedVersion a === v
        .&&. agreedGroup a === (if v == TLS13 then "X25519MLKEM768" else "X25519")
        .&&. property (agreedCipher a `elem` aead)
        .&&. agreedALPN a === Just "h2"
        -- the signature follows from the key's type (T3)
        .&&. agreedPeerSignatureAlgorithm ca === signature k v
    aead = [ "TLS_AES_128_GCM_SHA256", "TLS_AES_256_GCM_SHA384", "TLS_CHACHA20_POLY1305_SHA256"
           , "ECDHE-ECDSA-AES128-GCM-SHA256", "ECDHE-RSA-AES128-GCM-SHA256" ]
    -- in TLS 1.2 the ECDSA code points do not tie the curve to the hash
    signature k v = case k of
      "ec" -> 0x0403       -- ecdsa_secp256r1_sha256
      "p384" | v == TLS13 -> 0x0503  -- ecdsa_secp384r1_sha384
             | otherwise -> 0x0403
      _ -> 0x0804 :: Word16 -- rsa_pss_rsae_sha256

prop_chain :: Property
prop_chain = ioProperty $ do
  scs <- server "chain.pem" "leaf.key"
  r <- handshakePair scs client (Just "localhost") >>= traverse (agreed . pClient)
  -- the leaf and the CA, both sent
  pure (fmap agreedPeerCertificates r === Right 2)

prop_configErrors :: Property
prop_configErrors = ioProperty $ do
  good <- server "ec.pem" "ec.key"
  mismatch <- server "ec.pem" "other.key"
  encrypted <- server "ec.pem" "encrypted.key"
  garbage <- pure (defaultConfigSettings Server) { csCertificate = Just ("not a certificate", "not a key") }
  results <- mapM (\(name, cs) -> (,) name <$> newConfig cs)
    [ ("a key that is not the certificate's", mismatch)
    , ("an encrypted key", encrypted)
    , ("garbage", garbage)
    , ("no certificate", defaultConfigSettings Server)
    , ("an empty ALPN protocol", client { csALPN = [""] })
    , ("an empty ALPN protocol, for a server", good { csALPN = ["h2", ""] })
    , ("TLS 1.2 for QUIC", client { csQuic = True })
    , ("an unknown group", client { csGroups = Just "nosuchgroup" }) ]
  pure $ conjoin [ counterexample name (either (const True) (const False) r) | (name, r) <- results ]

prop_alpn :: Property
prop_alpn = ioProperty $ do
  scs <- server "ec.pem" "ec.key"
  let with ss offer = do
        r <- handshakePair ss client { csALPN = offer } Nothing
        traverse (agreed . pServer) r
  both <- with scs ["h2", "http/1.1"]
  backwards <- with scs ["http/1.1", "h2"]
  h1 <- with scs ["http/1.1"]
  none <- with scs []
  h3 <- with scs ["h3"]
  h1only <- with scs { csALPN = ["http/1.1"] } ["h2", "http/1.1"]
  noServerAlpn <- with scs { csALPN = [] } ["h2", "http/1.1"]
  pure $ conjoin
    [ counterexample "both: h2" (fmap agreedALPN both === Right (Just "h2"))
    , counterexample "the server's order, not the client's" (fmap agreedALPN backwards === Right (Just "h2"))
    , counterexample "http/1.1 alone" (fmap agreedALPN h1 === Right (Just "http/1.1"))
    , counterexample "no ALPN: the handshake completes" (fmap agreedALPN none === Right Nothing)
    , counterexample "nothing in common: no_application_protocol" (either (const True) (const False) h3)
    , counterexample "a server without h2" (fmap agreedALPN h1only === Right (Just "http/1.1"))
    , counterexample "a server without ALPN: none agreed" (fmap agreedALPN noServerAlpn === Right Nothing) ]

prop_groups :: Property
prop_groups = ioProperty $ do
  scs <- server "ec.pem" "ec.key"
  x <- handshakePair scs { csGroups = Just "X25519" } client Nothing >>= traverse (agreed . pServer)
  order <- handshakePair scs { csGroups = Just "P-256:X25519" } client { csGroups = Just "X25519:P-256" } Nothing
           >>= traverse (agreed . pServer)
  pure $ fmap agreedGroup x === Right "X25519" .&&. fmap agreedGroup order === Right "P-256"

prop_refused :: Property
prop_refused = ioProperty $ do
  scs <- server "rsa.pem" "rsa.key"
  versions <- handshakePair scs { csVersions = (TLS13, TLS13) } client { csVersions = (TLS12, TLS12) } Nothing
  -- a TLS 1.2 client with no ECDHE and AEAD cipher: refused, even for
  -- HTTP/1.1 (T2)
  weak <- handshakePair scs client { csVersions = (TLS12, TLS12), csCiphers = Just "ECDHE-RSA-AES128-SHA", csALPN = ["http/1.1"] } Nothing
  pure $ counterexample "versions" (either (const True) (const False) versions)
    .&&. counterexample "weak ciphers" (either (const True) (const False) weak)

-- no ticket (TLS 1.3, or 1.2), and no TLS 1.2 session ID kept: the
-- client is given no way to resume (T7)
prop_noTickets :: Property
prop_noTickets = ioProperty $ do
  scs <- server "ec.pem" "ec.key"
  rs <- mapM (\v -> do
          p <- pairOrFail scs client { csVersions = (v, v) }
          -- a ticket is read after the handshake, as data is
          _ <- receiveAll (pClient p)
          (,) v <$> resumable (pClient p)) [TLS12, TLS13]
  pure (rs === [(TLS12, False), (TLS13, False)])

prop_data :: Property
prop_data =
  forAll (listOf1 (B.pack <$> listOf (choose (0, 255)))) $ \chunks ->
  forAll (listOf (choose (1, 40000))) $ \cuts ->
  forAll (elements [TLS12, TLS13]) $ \v -> monadicIO $ do
    scs <- run (server "ec.pem" "ec.key")
    p <- run (pairOrFail scs client { csVersions = (v, v) })
    let msg = B.concat chunks
    run (mapM_ (\b -> send (pClient p) b >>= either (fail . show) pure) chunks)
    run (pass (pClient p) (pServer p) cuts)
    (got, end) <- run (receiveAll (pServer p))
    -- and back
    run (send (pServer p) msg >>= either (fail . show) pure)
    run (pass (pServer p) (pClient p) (reverse cuts))
    (back, _) <- run (receiveAll (pClient p))
    assert (got == msg && end == NeedInput && back == msg)

prop_close :: Property
prop_close = ioProperty $ do
  scs <- server "ec.pem" "ec.key"
  p <- pairOrFail scs client
  _ <- send (pClient p) "last"
  _ <- closeNotify (pClient p)
  pass (pClient p) (pServer p) []
  r <- receiveAll (pServer p)
  pure (r === ("last", PeerClosed))

prop_garbage :: Property
prop_garbage = ioProperty $ do
  scs <- server "ec.pem" "ec.key"
  p <- pairOrFail scs client
  -- an application-data record that does not decrypt
  _ <- feed (pServer p) (B.pack ([0x17, 0x03, 0x03, 0x00, 0x20] ++ replicate 32 0x41))
  r <- receive (pServer p) 16384
  alert <- takeOutput (pServer p)
  pure $ counterexample (show r) (case r of { ReceiveFailed _ -> True; _ -> False })
    .&&. counterexample "an alert to send" (not (B.null alert))

prop_bound :: Property
prop_bound = ioProperty $ do
  scs <- server "ec.pem" "ec.key"
  sc <- config scs
  s <- newSession sc Nothing 100
  a <- feed s (B.replicate 100 0x16)
  b <- feed s (B.replicate 1 0x16)
  pure (a === Right () .&&. counterexample (show b) (either (const True) (const False) b))

-- A KeyUpdate goes with its sender's next write (AWS-LC: "The next call
-- to |SSL_write| will cause the message to be sent"); the one that
-- answers it too, as RFC 8446 allows ("prior to sending its next
-- Application Data record")
prop_keyUpdate :: Property
prop_keyUpdate = ioProperty $ do
  scs <- server "ec.pem" "ec.key"
  p <- pairOrFail scs client { csVersions = (TLS13, TLS13) }
  _ <- keyUpdate (pClient p) >>= either (fail . show) pure
  _ <- send (pClient p) "before"
  pass (pClient p) (pServer p) []
  (r, _) <- receiveAll (pServer p)
  _ <- send (pServer p) "after"
  withReply <- takeOutput (pServer p)
  _ <- send (pServer p) "after"
  plain <- takeOutput (pServer p)
  _ <- feed (pClient p) (withReply <> plain)
  (back, _) <- receiveAll (pClient p)
  -- and the client's new keys are understood
  _ <- send (pClient p) "more"
  pass (pClient p) (pServer p) []
  (more, _) <- receiveAll (pServer p)
  pure $ r === "before"
    .&&. counterexample "the reply goes with the next record" (B.length withReply > B.length plain)
    .&&. back === "afterafter" .&&. more === "more"

prop_sni :: Property
prop_sni = ioProperty $ do
  scs <- server "ec.pem" "ec.key"
  r <- handshakePair scs client (Just "example.test") >>= traverse (agreed . pServer)
  pure (fmap agreedServerName r === Right (Just "example.test"))

prop_quic :: Property
prop_quic = ioProperty $ do
  scs <- server "ec.pem" "ec.key"
  sc <- config scs { csQuic = True, csALPN = ["h3"], csVersions = (TLS13, TLS13) }
  cc <- config client { csQuic = True, csALPN = ["h3"], csVersions = (TLS13, TLS13) }
  s <- newSession sc Nothing 65536
  c <- newSession cc (Just "localhost") 65536
  _ <- quicSetTransportParams s "\x01\x02\x40\x64"
  _ <- quicSetTransportParams c "\x01\x02\x40\x64"
  -- each level's octets handed over, the receiver stepped after each
  let passQ from to = quicTake from >>= mapM_ (\(l, b) -> do
        quicProvide to l b >>= either (fail . show) pure
        _ <- handshake to
        pure ())
      go :: Int -> IO ()
      go n = do
        _ <- handshake c
        passQ c s
        _ <- handshake s
        passQ s c
        rc <- handshake c
        rs <- handshake s
        if (rc, rs) == (Right Done, Right Done) || n > 10 then pure () else go (n + 1)
  go 0
  -- handshake data beyond what AWS-LC allows at a level is refused
  big <- newSession sc Nothing 65536
  flood <- quicProvide big 0 (B.replicate 1000000 0x01)
  a <- agreed s
  ss <- quicSecrets s
  cs <- quicSecrets c
  let key sec = (secretLevel sec, secretWrite sec)
      cm = M.fromList [ (key x, secretBytes x) | x <- cs ]
      agree = [ M.lookup (secretLevel x, not (secretWrite x)) cm == Just (secretBytes x) | x <- ss ]
  pure $ agreedALPN a === Just "h3"
    .&&. counterexample "a flood at one level" (either (const True) (const False) flood)
    .&&. agreedVersion a === TLS13
    .&&. counterexample (show (map key ss, map key cs)) (length ss === 4 .&&. property (and agree))
