{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | Checks that functions scale linearly with the size of their input.
--
-- Every function here can be fed attacker-controlled input, so
-- anything worse than linear is a denial-of-service risk. Timing tests
-- are flaky, so we measure allocation instead: each function is run on
-- an input of size n and of size 8n, and the ratio of the bytes
-- allocated is checked. Linear code gives a ratio of about 8, quadratic
-- code about 64.
module ComplexityTests (complexityTests) where

import Control.Exception (evaluate)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.FieldSyntax
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Method
import Hyperdrive.Semantics.Status (pattern OK)
import Hyperdrive.Semantics.SafePath
import Hyperdrive.Semantics.URI
import System.Mem (getAllocationCounter)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

complexityTests :: TestTree
complexityTests = testGroup "Complexity (allocation is linear in input size)"
  [ linear "appendField"   1000  $ \n ->
      length (fieldsToList (foldl (\fs v -> appendField hHost v fs) emptyFields (values n)))
  , linear "fieldsFromList, lookupAll" 1000 $ \n ->
      length (lookupAll hHost (fieldsFromList [ (hHost, v) | v <- values n ]))
  , linear "combineValues" 1000  $ \n ->
      maybe 0 (B.length . fieldValueBytes) (combineValues (values n))
  , linear "mkFieldName"   10000 $ \n ->
      maybe 0 (B.length . fieldNameBytes) (mkFieldName (C.replicate n 'A'))
  , linear "mkFieldValue"  10000 $ \n ->
      either (const 0) (B.length . fieldValueBytes)
        (mkFieldValue (C.replicate n ' ' <> C.replicate n 'a' <> C.replicate n '\t'))
  , linear "mkMethod"      10000 $ \n ->
      maybe 0 (B.length . methodBytes) (mkMethod (C.replicate n 'G'))
  , linear "mkScheme"      10000 $ \n ->
      maybe 0 (B.length . schemeBytes) (mkScheme ("h" <> C.replicate n '+'))
  , linear "parseAuthority (reg-name)" 10000 $ \n ->
      authority (C.replicate n 'A' <> "%2f")
  , linear "parseAuthority (port)" 10000 $ \n ->
      authority ("x:" <> C.replicate n '0' <> "80")
  , linear "parseAuthority (IPv6 pieces)" 10000 $ \n ->
      authority ("[" <> B.intercalate ":" (replicate n "1") <> "]")
  , linear "parseAuthority (colons)" 10000 $ \n ->
      authority ("[" <> C.replicate n ':' <> "]")
  , linear "mkPath"        10000 $ \n ->
      maybe 0 (B.length . pathBytes) (mkPath (path n))
  , linear "pathSegments"  10000 $ \n ->
      either (const 0) length (pathSegments (fromMaybe (error "invalid path") (mkPath (path n))))
  , linear "safePathSegments" 10000 $ \n ->
      either (const 0) length (safePathSegments (fromMaybe (error "invalid path") (mkPath (path n))))
  , linear "splitList (elements)" 10000 $ \n ->
      either (const 0) length (splitList NoComments (fv (B.intercalate ", " (replicate n "a;q=1"))))
    -- RFC 9110 Section 5.6.1.2: empty list elements must not be usable
    -- for denial of service. See the citation in FieldSyntaxTests.
  , linear "splitList (empty elements)" 10000 $ \n ->
      either (const 0) length (splitList NoComments (fv ("a" <> C.replicate n ',' <> "b")))
  , linear "splitList (quoted-string)" 10000 $ \n ->
      either (const 0) length (splitList NoComments (fv ("\"" <> mconcat (replicate n "a,\\\"") <> "\"")))
  , linear "splitList (nested comments)" 10000 $ \n ->
      either (const 0) length (splitList AllowComments (fv (C.replicate n '(' <> C.replicate n ')')))
  , linear "fieldListElements" 1000 $ \n ->
      either (const 0) length (fieldListElements NoComments hConnection
        (fieldsFromList (replicate n (hConnection, fv "a, b"))))
  , linear "lookupSingleton" 1000 $ \n ->
      either (const 0) (maybe 0 (B.length . fieldValueBytes)) (lookupSingleton hHost
        (fieldsFromList ((hHost, fv "a") : replicate n (hConnection, fv "b"))))
  , linear "lookupParameter" 10000 $ \n ->
      either (const 0) (maybe 0 B.length)
        (lookupParameter "q" (("q", "1") : replicate n ("a", "b")))
  , linear "mkResponse" 1000 $ \n ->
      either (const 0) (length . fieldsToList . responseHeaders)
        (mkResponse OK (fieldsFromList (replicate n (hContentType, fv "a"))) (ContentBytes ""))
  , linear "parseQuotedString" 10000 $ \n ->
      either (const 0) (B.length . fst) (parseQuotedString ("\"" <> mconcat (replicate n "a\\\"") <> "\""))
  , linear "quoteString" 10000 $ \n ->
      maybe 0 B.length (quoteString (mconcat (replicate n "a\"\\")))
  , linear "parseParameters" 10000 $ \n ->
      either (const 0) length (parseParameters (mconcat (replicate n "; a=\"b\"")))
  , linear "mkQuery"       10000 $ \n ->
      maybe 0 (B.length . queryBytes) (mkQuery (C.replicate n '?' <> "%20"))
  , linear "percentDecode" 10000 $ \n ->
      maybe 0 B.length (percentDecode (mconcat (replicate n "a%2F")))
  ]
  where
    values n = [ v | i <- [1 .. n], Right v <- [mkFieldValue (C.pack (show i))] ]
    path n = mconcat (replicate n "/a%2F")
    authority bs = either (const 0) (B.length . renderAuthority) (parseAuthority bs)
    fv bs = either (error "invalid field value") id (mkFieldValue bs)

-- | bytes allocated by evaluating the 'Int' result
allocation :: Int -> IO Int64
allocation x = do
  start <- getAllocationCounter -- counts down as the thread allocates
  _ <- evaluate x
  end <- getAllocationCounter
  pure (start - end)

-- | Run the function on n and 8n and check that allocation grew by a
-- factor of at most 12. The function must fully evaluate its result
-- when the 'Int' it returns is evaluated.
linear :: String -> Int -> (Int -> Int) -> TestTree
linear name n f = testProperty name $ once $ ioProperty $ do
  -- build the inputs inside f, but make sure each call is evaluated
  -- afresh rather than shared
  small <- allocation (f n)
  large <- allocation (f (8 * n))
  let ratio = fromIntegral large / fromIntegral (max 1 small) :: Double
  pure $ counterexample
           (name ++ ": allocated " ++ show small ++ " bytes for n = " ++ show n
                 ++ " and " ++ show large ++ " bytes for 8n (ratio " ++ show ratio ++ ")")
           (ratio <= 12)
