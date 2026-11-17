-- | The grammars of RFC 9110, 9112 and 3986, as published, used as
-- oracles: a string is valid exactly when the RFC's rule matches it.
-- See "Hyperdrive.ABNF.HTTP".
module Grammar
  ( rule
  , ruleBytes
  , valid
  , nearMiss
  , validOrNearMiss
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Hyperdrive.ABNF.Generate (nearMisses, validStrings)
import Hyperdrive.ABNF.HTTP (httpLibrary, rfc3986, rfc9110, rfc9112)
import Hyperdrive.ABNF.Recognize (ListForm(..), RecognizeError, matches)
import Hyperdrive.ABNF.Types (RuleName, mkRuleName, ruleNameBytes)
import qualified Hyperdrive.ABNF.Types as R
import qualified Data.Map.Strict as Map
import Test.QuickCheck

-- | Does the rule (of the grammar tagged @HTTP@, @HTTP/1.1@ or @URI@)
-- match the whole string? Characters are bytes, so the string must not
-- contain characters above @\\255@. The RFC appendices write lists in
-- the form senders must use, so the list form makes no difference.
rule :: String -> String -> String -> Bool
rule tag name = ruleBytes tag name . C.pack

ruleBytes :: String -> String -> ByteString -> Bool
ruleBytes tag name s = case Map.lookup (tag, name) matchers of
  Just m  -> either (error . (("Grammar.rule " ++ show (tag, name) ++ ": ") ++) . show) id (m s)
  Nothing -> error ("Grammar.rule: no rule " ++ show (tag, name))

-- | A matcher for every rule of the three grammars, each compiled the
-- first time it is used and then shared by every call.
matchers :: Map.Map (String, String) (ByteString -> Either RecognizeError Bool)
matchers = Map.fromList
  [ ((tag, C.unpack (ruleNameBytes (R.ruleName r))), matches Recipient httpLibrary (C.pack tag) (R.ruleName r))
  | (tag, rules) <- [("HTTP", rfc9110), ("HTTP/1.1", rfc9112), ("URI", rfc3986)], r <- rules ]

-- | strings the rule matches
valid :: String -> String -> Gen ByteString
valid tag name = either (error . show) id (validStrings Recipient httpLibrary (C.pack tag) (ruleName name))

-- | strings close to the rule's language, on either side of it
nearMiss :: String -> String -> Gen ByteString
nearMiss tag name = either (error . show) id (nearMisses Recipient httpLibrary (C.pack tag) (ruleName name))

-- | half valid strings, half near misses
validOrNearMiss :: String -> String -> Gen ByteString
validOrNearMiss tag name = oneof [valid tag name, nearMiss tag name]

ruleName :: String -> RuleName
ruleName = fromJust . mkRuleName . C.pack
