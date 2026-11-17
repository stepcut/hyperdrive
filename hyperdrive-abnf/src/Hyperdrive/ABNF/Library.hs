-- | Grammars that refer to each other, and how a rule name used in one
-- of them is resolved to its definition.
module Hyperdrive.ABNF.Library
  ( Library
  , library
  , libraryHas
  , Namespace(..)
  , LookupError(..)
  , resolve
  ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Hyperdrive.ABNF.Core (coreRules)
import Hyperdrive.ABNF.Grammar
import Hyperdrive.ABNF.Types

-- | Grammars by the tag other grammars cite them with: a rule
-- @uri-host = \<host, see [URI], Section 3.2.2\>@ is looked up as
-- @host@ in the grammar tagged @URI@.
newtype Library = Library (Map ByteString Grammar)

library :: [(ByteString, Grammar)] -> Library
library = Library . Map.fromList

libraryHas :: Library -> ByteString -> Bool
libraryHas (Library grammars) tag = Map.member tag grammars

-- | Where a rule is defined.
data Namespace
  = Core               -- ^ the core rules of RFC 5234, Appendix B.1
  | Tagged ByteString  -- ^ a grammar in the 'Library'
  deriving (Eq, Ord, Show)

data LookupError
  = UnknownGrammar ByteString
    -- ^ no grammar in the library has this tag
  | UndefinedRule Namespace RuleName
    -- ^ a rule is used but defined neither in its grammar nor as a core rule
  | UnresolvedImport Namespace RuleName Import
    -- ^ an import cites a grammar that is not in the library
  deriving (Eq, Show)

-- | The definition of a rule name used in a namespace, with the
-- namespace and name it is defined under (which differ from the ones
-- given when the rule is an import). A grammar's own definitions come first,
-- then the core rules. Imports are followed into the grammar they
-- cite.
resolve :: Library -> Namespace -> RuleName -> Either LookupError (Namespace, RuleName, Alternation)
resolve lib@(Library grammars) ns name
  | Tagged tag <- ns, not (Map.member tag grammars) = Left (UnknownGrammar tag)
  | otherwise = case local of
    Just alt -> case importOf alt of
      Nothing -> Right (ns, name, alt)
      Just i | Map.member (importFrom i) grammars -> resolve lib (Tagged (importFrom i)) (importName i)
             | otherwise -> Left (UnresolvedImport ns name i)
    Nothing -> case lookupRule name coreGrammar of
      Just alt -> Right (Core, name, alt)
      Nothing  -> Left (UndefinedRule ns name)
  where
    local = case ns of
      Core       -> lookupRule name coreGrammar
      Tagged tag -> Map.lookup tag grammars >>= lookupRule name

coreGrammar :: Grammar
coreGrammar = either (error . show) id (mkGrammar coreRules)
