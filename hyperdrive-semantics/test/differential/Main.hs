{-# LANGUAGE OverloadedStrings #-}
-- | IPv6 addresses compared with another implementation: Python's
-- ipaddress module (run by test/differential/ipv6.py), with RFC 3986's
-- IPv6address rule as a third opinion.
--
-- For each input this checks that we and Python agree on whether it is
-- an address, on its value, and on the text RFC 5952 gives it. Where we
-- disagree, Python is not assumed to be right: each kind of difference
-- was investigated against the RFCs, and only the ones found to be
-- correct on our side are allowed, below. Any other difference fails.
module Main (main) where

import qualified Data.Aeson as A
import Data.Aeson ((.:), (.:?))
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (nub)
import Data.Word (Word16)
import Grammar (nearMiss, ruleBytes, valid)
import Hyperdrive.Semantics.URI
import Numeric (showHex)
import System.Environment (lookupEnv)
import System.Process (readProcess)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Text.Printf (printf)

main :: IO ()
main = defaultMain $ testGroup "Differential"
  [ testProperty "IPv6 addresses agree with Python's ipaddress" $ once $ ioProperty compareIPv6 ]

data Python = Python { pyOk :: Bool, pyValue :: Maybe String, pyText :: Maybe ByteString }

instance A.FromJSON Python where
  parseJSON = A.withObject "result" $ \o ->
    Python <$> o .: "ok" <*> o .:? "value" <*> (fmap C.pack <$> o .:? "text")

-- | our value (as 32 hex digits) and rendering of an address
ours :: ByteString -> Maybe (String, ByteString)
ours s = case parseAuthority ("[" <> s <> "]") of
  Right (Authority h@(HostIPv6 (IPv6 hi lo)) Nothing) ->
    Just (printf "%016x%016x" hi lo, B.init (B.tail (renderHost h)))
  _ -> Nothing

compareIPv6 :: IO Property
compareIPv6 = do
  inputs <- nub . (handPicked ++) . concat <$> mapM generate
    [ vectorOf 4000 (resize 30 (valid "URI" "IPv6address"))
    , vectorOf 4000 (resize 30 (nearMiss "URI" "IPv6address"))
    , vectorOf 3000 (pieces >>= spell) ]
  python <- maybe "python3" id <$> lookupEnv "PYTHON"
  out <- readProcess python ["test/differential/ipv6.py"] (unlines (map hex inputs))
  results <- either fail pure (mapM (A.eitherDecode . L.pack) (lines out))
  let rows = zip inputs results
      problems = concatMap check rows
      zoneIds = [ s | (s, p) <- rows, pyOk p, ours s == Nothing, zoneId s ]
      agreed = length [ () | (s, p) <- rows, pyOk p, ours s /= Nothing ]
  pure $
    counterexample (unlines problems) $
    counterexample (printf "%d inputs, %d addresses both accept" (length rows) agreed) $
    property (null problems && length rows == length results && not (null zoneIds) && agreed > 4000)
  where
    hex = concatMap (printf "%02x") . B.unpack

    check (s, p) =
      let grammar = ruleBytes "URI" "IPv6address" s
      in case (ours s, pyOk p) of
           (Just (v, t), True)
             | Just v /= pyValue p -> [ printf "%s: value %s, Python %s" (show s) v (show (pyValue p)) ]
             | Just t /= pyText p  -> [ printf "%s: renders %s, Python %s" (show s) (show t) (show (pyText p)) ]
             | otherwise           -> []
           (Nothing, False) -> [ printf "%s: the IPv6address rule matches, but we reject" (show s) | grammar ]
           (Just _, False) -> [ printf "%s: we accept, Python rejects (grammar: %s)" (show s) (show grammar) ]
           (Nothing, True)
             | zoneId s && not grammar -> []
             | otherwise -> [ printf "%s: Python accepts, we reject (grammar: %s)" (show s) (show grammar) ]

    -- RFC 4007 writes a zone identifier after "%" (fe80::1%eth0), and
    -- Python accepts that, as an address API should. It is not URI
    -- syntax: RFC 6874 added it to URIs (as %25), but required HTTP
    -- clients to remove it before sending a request, and RFC 9844
    -- (2025) obsoletes RFC 6874, reverting RFC 3986's syntax, because
    -- zone identifiers "must not be sent on the wire". So a Host or
    -- request target with one is rightly rejected.
    zoneId = B.elem 0x25

    -- the pieces of an address, mostly zeros, where compression happens
    pieces :: Gen [Word16]
    pieces = vectorOf 8 (frequency [(5, pure 0), (3, chooseEnum (0, 0xffff)), (1, elements [1, 0xff, 0xffff])])

    -- the same address written in full, in upper case with leading
    -- zeros, and as we render it
    spell ps = elements
      [ C.pack (colons (map (`showHex` "") ps))
      , C.pack (colons (map (printf "%04X") ps))
      , B.init (B.tail (renderHost (HostIPv6 (IPv6 (word64 (take 4 ps)) (word64 (drop 4 ps)))))) ]
    colons = foldr1 (\a b -> a ++ ":" ++ b)
    word64 = foldl (\acc p -> (acc `shiftL` 16) .|. fromIntegral p) 0

    handPicked =
      [ "fe80::1%eth0", "fe80::1%25eth0", "fe80::1%1", "::1/128", " ::1", "::1 ", "::1\n", "\x80::1"
      , "1:2:3:4:5:6:7::", "::2:3:4:5:6:7:8", "1::2:3:4:5:6:7", "1:2:3:4:5:6:7:8:9", "1::2::3", ":::"
      , "::", "1:", ":1", "00000::1", "::0001", "::1.2.3.4", "::ffff:1.2.3.04", "::ffff:01.2.3.4"
      , "::ffff:1.2.3", "::ffff:1.2.3.4.5", "::ffff:255.255.255.255", "::ffff:256.1.1.1"
      , "::FFFF:1.2.3.4", "1:2:3:4:5:6:1.2.3.4", "1:2:3:4:5:6:7:1.2.3.4", "::ffff:0:192.0.2.1"
      , "64:ff9b::192.0.2.33", "2001:db8:0:0:1:0:0:1", "2001:db8::0:1", "v1.x" ]
