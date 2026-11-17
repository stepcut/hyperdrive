{-# LANGUAGE OverloadedStrings #-}
-- | Each parser against its rule in the RFC, on strings generated from
-- the rule itself: valid ones, and near misses that sit just inside or
-- just outside the language.
module GrammarTests (grammarTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Either (isRight)
import Data.Maybe (isJust)
import Grammar (rule, ruleBytes, validOrNearMiss)
import Hyperdrive.Semantics.Field (mkFieldName, mkFieldValue)
import Hyperdrive.Semantics.FieldSyntax (parseParameters, parseQuotedString)
import Hyperdrive.Semantics.Method (mkMethod)
import Hyperdrive.Semantics.URI
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import URITests (expectedAuthority)

grammarTests :: TestTree
grammarTests = testGroup "Against the RFC grammars, on generated strings"
  [ agrees "mkScheme"          "URI" "scheme"        (isJust . mkScheme)
  , agrees "mkPath"            "URI" "path-abempty"  (isJust . mkPath)
  , agrees "mkQuery"           "URI" "query"         (isJust . mkQuery)
  , agrees "mkMethod"          "HTTP" "method"       (isJust . mkMethod)
  , agrees "mkFieldName"       "HTTP" "field-name"   (isJust . mkFieldName)
  , agrees "parseParameters"   "HTTP" "parameters"   (isRight . parseParameters)
  , agrees "parseQuotedString" "HTTP" "quoted-string" $ \s ->
      case parseQuotedString s of
        Right (_, rest) -> B.null rest
        Left _          -> False
    -- RFC 9110 5.5: OWS around a field value is not part of it
  , testProperty "mkFieldValue: field-value" $ checkCoverage $
      forAll (oneof [validOrNearMiss "HTTP" "field-value", withOWS]) $ \s ->
        let expected = ruleBytes "HTTP" "field-value" (stripOWS s)
        in cover 20 expected "valid" $ cover 8 (not expected) "invalid" $
           isRight (mkFieldValue s) === expected
    -- the Host grammar, less what parseAuthority rejects on purpose
  , testProperty "parseAuthority: Host" $ checkCoverage $
      forAll (validOrNearMiss "HTTP" "Host") $ \s ->
        let expected = expectedAuthority (C.unpack s)
        in cover 20 expected "valid" $ cover 10 (not expected) "invalid" $
           isRight (parseAuthority s) === expected
  , testProperty "parseAuthority: IPv6address" $ checkCoverage $
      forAll (validOrNearMiss "URI" "IPv6address") $ \s ->
        let expected = ruleBytes "URI" "IPv6address" s
            isIPv6 = case parseAuthority ("[" <> s <> "]") of
              Right (Authority (HostIPv6 _) Nothing) -> True
              _                                      -> False
        in cover 20 expected "valid" $ cover 20 (not expected) "invalid" $
           isIPv6 === expected
  ]
  where
    withOWS = do
      v <- validOrNearMiss "HTTP" "field-value"
      a <- elements ["", " ", "\t", "  "]
      b <- elements ["", " ", "\t \t"]
      pure (a <> v <> b)

-- | The parser accepts a string exactly when the rule matches it.
agrees :: String -> String -> String -> (ByteString -> Bool) -> TestTree
agrees what tag r accepts = testProperty (what ++ ": " ++ r) $ checkCoverage $
  forAll (validOrNearMiss tag r) $ \s ->
    let expected = rule tag r (C.unpack s)
    in cover 20 expected "valid" $
       cover 5 (not expected) "invalid" $
       counterexample (show s) $
         accepts s === expected

stripOWS :: ByteString -> ByteString
stripOWS = B.dropWhileEnd ows . B.dropWhile ows
  where ows w = w == 0x20 || w == 0x09
