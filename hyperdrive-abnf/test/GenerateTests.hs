{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module GenerateTests (generateTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Generators (genGrammar, shortInputs)
import Hyperdrive.ABNF.Generate
import Hyperdrive.ABNF.Grammar
import Hyperdrive.ABNF.QQ
import Hyperdrive.ABNF.Recognize
import Hyperdrive.ABNF.Types
import RFCTests (Spec(..), loadSpecs)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

generateTests :: IO TestTree
generateTests = do
  (http, http1, uri) <- loadSpecs
  let lib = library [ ("HTTP", specGrammar http), ("HTTP/1.1", specGrammar http1)
                    , ("URI", specGrammar uri) ]
      everyRule = [ (tag, n) | (tag, s) <- [("HTTP", http), ("HTTP/1.1", http1), ("URI", uri)]
                             , n <- Map.keys (grammarRules (specGrammar s)) ]
  pure $ testGroup "Generator"
    [ testProperty "every RFC rule's strings are recognized" (prop_rfcRules lib everyRule)
    , testProperty "the rules that cannot be generated"   (prop_ungeneratable lib everyRule)
    , testProperty "random grammars' strings are recognized" prop_randomGrammars
    , testProperty "every string of a finite language"      prop_complete
    , testProperty "list forms"                             prop_listForms
    , testGroup "near misses fall on both sides"
        [ testProperty (C.unpack tag ++ " " ++ C.unpack r) (prop_nearMisses lib tag r)
        | (tag, r) <- [ ("HTTP", "token"), ("HTTP", "Host"), ("HTTP", "quoted-string")
                      , ("URI", "URI-reference"), ("URI", "IPv6address"), ("HTTP/1.1", "HTTP-message") ] ]
    ]

name :: ByteString -> RuleName
name = fromJust . mkRuleName

single :: RuleList -> Library
single rules = library [("T", either (error . show) id (mkGrammar rules))]

-- | Generated strings match their rule, for every rule of the three
-- RFCs that can be generated. The recognizer cannot decide for rules
-- that use a rule imported from an RFC outside the library, even when
-- the generator avoided it.
prop_rfcRules :: Library -> [(ByteString, RuleName)] -> Property
prop_rfcRules lib rules = once $ conjoin
  [ counterexample (C.unpack tag ++ " " ++ show n) $
      forAll (resize 30 (vectorOf 25 g)) $ \ss -> conjoin
        [ counterexample (show s) $ case matches Recipient lib tag n s of
            Left (Lookup (UnresolvedImport _ _ i)) -> (importFrom i `elem` ["RFC4647", "RFC5646", "RFC5322"]) === True
            result -> result === Right True
        | s <- ss ]
  | (tag, n) <- rules, Right g <- [validStrings Recipient lib tag n] ]

-- | Only the rules that depend on RFCs outside the library cannot be
-- generated.
prop_ungeneratable :: Library -> [(ByteString, RuleName)] -> Property
prop_ungeneratable lib rules = once $
  Set.fromList [ (tag, n) | (tag, n) <- rules, Left _ <- [validStrings Recipient lib tag n] ]
    === Set.fromList [ ("HTTP", name n) | n <- ["From", "language-range", "language-tag", "mailbox"] ]
  .&&. errorOf (validStrings Recipient lib "HTTP" (name "From")) === Just (Ungeneratable (Tagged "HTTP") (name "From"))
  .&&. errorOf (validStrings Recipient lib "nowhere" (name "x")) === Just (GenerateLookup (UnknownGrammar "nowhere"))
  where errorOf = either Just (const Nothing)

-- | Generated strings match their rule, and a rule can be generated
-- exactly when it matches something (judged by every short input).
prop_randomGrammars :: Property
prop_randomGrammars = withMaxSuccess 500 $
  forAll genGrammar $ \rules ->
  forAll (elements [Sender, Recipient]) $ \form ->
    let top = ruleName (last rules)
        someMatch = any (\i -> matches form (single rules) "T" top i == Right True) shortInputs
    in counterexample (unlines (map show rules)) $
       counterexample (show form) $
         case validStrings form (single rules) "T" top of
           Left err -> counterexample (show err) $
             err === Ungeneratable (Tagged "T") top .&&. counterexample "but it matches" (not someMatch)
           Right g -> forAll (vectorOf 10 g) $ \ss -> conjoin
             [ counterexample (show s) (matches form (single rules) "T" top s === Right True) | s <- ss ]

-- | Small finite languages are generated in full. (Properties run
-- once run at size 0, where repetitions take their minimum, so these
-- set a size.)
prop_complete :: Property
prop_complete = once $ conjoin
  [ language "x" ["a", "b", "ac", "bc", "A", "B", "aC", "bC", "Ac", "Bc", "AC", "BC"]
  , language "y" ["ab", "Ab", "aB", "AB"]
  , language "t" ["ab", "Ab", "aB", "AB"]
  , language "z" ["0", "1", "2", "Q"]
  , language "w" ["", "ab", "abab"]
  , language "v" ["\xFE", "\xFF"]
  , language "u" ["x"]
  ]
  where
    rules = [abnf|
      x = ("a" / "b") ["c"]
      y = "ab"
      t = "Ab"
      z = %x30-32 / %s"Q"
      w = *2%s"ab"
      v = %xFF / %x100 / %x100-1FF / <prose> / %xFE-100
      u = %s"x" / 1*0%s"never" / 2*1%s"y"
      |]
    language :: ByteString -> [ByteString] -> Property
    language r expected = counterexample (C.unpack r) $
      case validStrings Sender (single rules) "T" (name r) of
        Left err -> counterexample (show err) False
        Right g -> forAll (resize 20 (vectorOf 600 g)) $ \ss -> Set.fromList ss === Set.fromList expected

-- | Sender-form lists have no empty elements (RFC 9110, 5.6.1.1), and
-- recipient-form lists sometimes do (5.6.1.2).
prop_listForms :: Property
prop_listForms = once $
  let lib = single [abnf|
              l = 2#3e
              e = %s"e"
              |]
      gen form = either (error . show) id (validStrings form lib "T" (name "l"))
      count = C.count 'e'
  in forAll (resize 20 (vectorOf 400 (gen Sender))) $ \senders ->
     forAll (resize 20 (vectorOf 400 (gen Recipient))) $ \recipients ->
       conjoin
         [ conjoin [ counterexample (show s) (matches Sender lib "T" (name "l") s === Right True) | s <- senders ]
         , conjoin [ counterexample (show s) (matches Recipient lib "T" (name "l") s === Right True) | s <- recipients ]
         , Set.fromList (map count senders) === Set.fromList [2, 3]
         , Set.fromList (map count recipients) === Set.fromList [2, 3]
           -- recipients get lists with empty elements, which senders may not send
         , counterexample "no empty elements" $
             any (\s -> matches Sender lib "T" (name "l") s == Right False) recipients
           -- and lists with whitespace around the commas
         , counterexample "no whitespace" $ any (B.any (\w -> w == 0x20 || w == 0x09)) senders
         ]

-- | Near misses are close to the language: some are still in it, most
-- are not. (checkCoverage only works at the top of a property, so each
-- rule is a test of its own.)
prop_nearMisses :: Library -> ByteString -> ByteString -> Property
prop_nearMisses lib tag r =
  case nearMisses Recipient lib tag (name r) of
    Left err -> counterexample (show err) False
    Right g -> checkCoverage $ withMaxSuccess 400 $ forAll g $ \s ->
      let ok = matches Recipient lib tag (name r) s
      in cover 20 (ok == Right False) "rejected" $
         cover 3 (ok == Right True) "accepted" $
         counterexample (show (s, ok)) (either (const False) (const True) ok)
