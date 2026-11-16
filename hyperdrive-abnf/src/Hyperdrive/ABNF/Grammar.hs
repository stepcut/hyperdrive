{-# LANGUAGE OverloadedStrings #-}
-- | A rule list as a set of definitions: what each rule name means,
-- which names are used but not defined, and which rules are imported
-- from other specifications.
module Hyperdrive.ABNF.Grammar
  ( Grammar
  , mkGrammar
  , GrammarError(..)
  , grammarRules
  , lookupRule
  , references
  , undefinedReferences
    -- * Rules defined in other specifications
  , Import(..)
  , importOf
  , imports
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (foldlM, toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Hyperdrive.ABNF.Types

-- | Each rule name with its definition, with the alternatives added by
-- @=/@ included.
newtype Grammar = Grammar (Map RuleName Alternation)
  deriving (Eq, Show)

data GrammarError
  = Redefined RuleName
    -- ^ a second @=@ definition of a name
  | IncrementalBeforeDefinition RuleName
    -- ^ @=/@ for a name that has no @=@ definition before it
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.3
-- # That is, an initial rule may match one or more
-- # alternatives, with later rule definitions adding to the set of
-- # alternatives.

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.3
-- # ruleset     =  alt1 / alt2 / alt3 / alt4 / alt5
mkGrammar :: RuleList -> Either GrammarError Grammar
mkGrammar = fmap Grammar . foldlM add Map.empty
  where
    add m (Rule name Basic alt)
      | Map.member name m = Left (Redefined name)
      | otherwise         = Right (Map.insert name alt m)
    add m (Rule name Incremental (Alternation more)) = case Map.lookup name m of
      Nothing                   -> Left (IncrementalBeforeDefinition name)
      Just (Alternation (a :| as)) ->
        Right (Map.insert name (Alternation (a :| as ++ toList more)) m)

grammarRules :: Grammar -> Map RuleName Alternation
grammarRules (Grammar m) = m

lookupRule :: RuleName -> Grammar -> Maybe Alternation
lookupRule name (Grammar m) = Map.lookup name m

-- | the rule names an alternation uses
references :: Alternation -> Set RuleName
references (Alternation cs) = foldMap concatenation cs
  where
    concatenation (Concatenation rs) = foldMap repetition rs
    repetition (Repetition _ e) = case e of
      RuleRef name -> Set.singleton name
      Group a      -> references a
      Option a     -> references a
      _            -> Set.empty

-- | the rule names the grammar uses but does not define
undefinedReferences :: Grammar -> Set RuleName
undefinedReferences (Grammar m) =
  foldMap references m `Set.difference` Map.keysSet m

-- | A rule defined by reference to another specification, written as
-- a prose value: @uri-host = \<host, see [URI], Section 3.2.2\>@.
data Import = Import
  { importName    :: RuleName    -- ^ the name in the other specification
  , importFrom    :: ByteString  -- ^ the citation tag, such as @URI@
  , importSection :: ByteString  -- ^ such as @3.2.2@
  } deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-1.2
-- # The rules below are defined in [HTTP]:

-- = https://www.rfc-editor.org/rfc/rfc9112#section-1.2
-- # The rules below are defined in [URI]:

-- | The import a definition is, if it is nothing but a prose value of
-- the form @\<name, see [TAG], Section N\>@.
importOf :: Alternation -> Maybe Import
importOf (Alternation (Concatenation (Repetition Nothing (ProseVal p) :| []) :| [])) = do
  let (name, rest) = B.breakSubstring ", see [" p
  rest1 <- B.stripPrefix ", see [" rest
  let (tag, rest2) = B.breakSubstring "], Section " rest1
  number <- B.stripPrefix "], Section " rest2
  ruleName' <- mkRuleName name
  if B.null tag || B.null number then Nothing else Just (Import ruleName' tag number)
importOf _ = Nothing

-- | the rules of the grammar that are imports
imports :: Grammar -> Map RuleName Import
imports (Grammar m) = Map.mapMaybe importOf m
