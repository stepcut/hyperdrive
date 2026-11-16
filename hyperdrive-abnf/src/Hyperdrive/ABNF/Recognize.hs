{-# LANGUAGE OverloadedStrings #-}
-- | Decide whether a string matches a rule of an ABNF grammar.
--
-- The recognizer computes, for each element and starting position,
-- the set of every position at which a match of the element can end.
-- Nothing is matched greedily and no alternative is preferred, so
-- ambiguity and repetition mean exactly what RFC 5234 says they mean:
-- a string matches a rule when some way of matching the rule ends at
-- the end of the string. Results are memoised for each rule and
-- position.
--
-- Terminal values are matched against the bytes of the input, so a
-- value above 255 never matches.
module Hyperdrive.ABNF.Recognize
  ( -- * Grammars that refer to each other
    Library
  , library
  , Namespace(..)
    -- * Recognizing
  , ListForm(..)
  , matches
  , matchEnds
  , RecognizeError(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (foldlM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import Hyperdrive.ABNF.Core (coreRules)
import Hyperdrive.ABNF.Grammar
import Hyperdrive.ABNF.Types
import Numeric.Natural (Natural)

-- | Grammars by the tag other grammars cite them with: a rule
-- @uri-host = \<host, see [URI], Section 3.2.2\>@ is looked up as
-- @host@ in the grammar tagged @URI@.
newtype Library = Library (Map ByteString Grammar)

library :: [(ByteString, Grammar)] -> Library
library = Library . Map.fromList

-- | Where a rule is defined.
data Namespace
  = Core               -- ^ the core rules of RFC 5234, Appendix B.1
  | Tagged ByteString  -- ^ a grammar in the 'Library'
  deriving (Eq, Ord, Show)

-- | Which expansion of @#@ lists to use (RFC 9110, Section 5.6.1):
-- senders must not generate empty list elements (5.6.1.1), and
-- recipients must accept some (5.6.1.2). These are obligations on
-- HTTP implementations, not on this module, so they are not cited
-- for duvet here.
data ListForm
  = Sender     -- ^ no empty elements: @element *( OWS \",\" OWS element )@
  | Recipient  -- ^ empty elements allowed: @[ element ] *( OWS \",\" OWS [ element ] )@
  deriving (Eq, Ord, Show, Enum, Bounded)

data RecognizeError
  = UnknownGrammar ByteString
    -- ^ no grammar in the library has this tag
  | UndefinedRule Namespace RuleName
    -- ^ a rule is used but defined neither in its grammar nor as a core rule
  | UnresolvedImport Namespace RuleName Import
    -- ^ an import cites a grammar that is not in the library
  | ProseValue Namespace RuleName ByteString
    -- ^ a prose value had to be matched, which cannot be done
  | LeftRecursion Namespace RuleName
    -- ^ a rule was reached again at the same position, without
    -- consuming input
  deriving (Eq, Show)

-- | Does the whole input match the rule of the grammar with the tag?
matches :: ListForm -> Library -> ByteString -> RuleName -> ByteString -> Either RecognizeError Bool
matches form lib tag name input =
  Set.member (B.length input) <$> matchEnds form lib tag name input

-- | Every position at which a match of the rule, starting at the
-- beginning of the input, can end.
matchEnds :: ListForm -> Library -> ByteString -> RuleName -> ByteString -> Either RecognizeError (Set Int)
matchEnds form lib@(Library grammars) tag name input
  | Map.member tag grammars =
      fst <$> run (rule (Tagged tag) name 0) (Env form lib input) Map.empty
  | otherwise = Left (UnknownGrammar tag)

------------------------------------------------------------------------------
-- the recognizer monad: the environment, a memo table, and errors
------------------------------------------------------------------------------

data Env = Env
  { envForm    :: ListForm
  , envLibrary :: Library
  , envInput   :: ByteString
  }

data Entry = InProgress | Done (Set Int)

type Memo = Map (Namespace, RuleName, Int) Entry

newtype R a = R { run :: Env -> Memo -> Either RecognizeError (a, Memo) }

instance Functor R where
  fmap f (R g) = R $ \e m -> fmap (\(a, m') -> (f a, m')) (g e m)

instance Applicative R where
  pure a = R $ \_ m -> Right (a, m)
  rf <*> ra = do { f <- rf; f <$> ra }

instance Monad R where
  R g >>= k = R $ \e m -> case g e m of
    Left err      -> Left err
    Right (a, m') -> run (k a) e m'

env :: R Env
env = R $ \e m -> Right (e, m)

failWith :: RecognizeError -> R a
failWith err = R $ \_ _ -> Left err

getMemo :: R Memo
getMemo = R $ \_ m -> Right (m, m)

putMemo :: Memo -> R ()
putMemo m = R $ \_ _ -> Right ((), m)

------------------------------------------------------------------------------
-- rules and names
------------------------------------------------------------------------------

-- | Where a rule name used in a namespace is defined, following
-- imports.
resolve :: Namespace -> RuleName -> R (Namespace, Alternation)
resolve ns name = do
  Library grammars <- envLibrary <$> env
  let local = case ns of
        Core       -> lookupRule name coreGrammar
        Tagged tag -> Map.lookup tag grammars >>= lookupRule name
  case local of
    Just alt -> case importOf alt of
      Nothing -> pure (ns, alt)
      Just i | Map.member (importFrom i) grammars -> resolve (Tagged (importFrom i)) (importName i)
             | otherwise -> failWith (UnresolvedImport ns name i)
    Nothing -> case lookupRule name coreGrammar of
      Just alt -> pure (Core, alt)
      Nothing  -> failWith (UndefinedRule ns name)

coreGrammar :: Grammar
coreGrammar = either (error . show) id (mkGrammar coreRules)

rule :: Namespace -> RuleName -> Int -> R (Set Int)
rule ns name pos = do
  (ns', alt) <- resolve ns name
  let key = (ns', name, pos)
  memo <- getMemo
  case Map.lookup key memo of
    Just (Done ends) -> pure ends
    Just InProgress  -> failWith (LeftRecursion ns' name)
    Nothing -> do
      putMemo (Map.insert key InProgress memo)
      ends <- alternation (ns', name) alt pos
      memo' <- getMemo
      putMemo (Map.insert key (Done ends) memo')
      pure ends

------------------------------------------------------------------------------
-- elements
------------------------------------------------------------------------------

-- | the rule being matched, for error messages and name lookup
type Context = (Namespace, RuleName)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.2
-- # Elements separated by a forward slash ("/") are alternatives.
alternation :: Context -> Alternation -> Int -> R (Set Int)
alternation ctx (Alternation cs) pos = Set.unions <$> mapM (\c -> concatenation ctx c pos) cs

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.1
-- # A rule can define a simple, ordered string of values (i.e., a
-- # concatenation of contiguous characters) by listing a sequence of rule
-- # names.
concatenation :: Context -> Concatenation -> Int -> R (Set Int)
concatenation ctx (Concatenation rs) pos = foldlM step (Set.singleton pos) rs
  where step starts r = fromEach starts (repetition ctx r)

-- | the union of the ends reached from each start
fromEach :: Set Int -> (Int -> R (Set Int)) -> R (Set Int)
fromEach starts f = Set.unions <$> mapM f (Set.toList starts)

repetition :: Context -> Repetition -> Int -> R (Set Int)
repetition ctx (Repetition Nothing e) pos = element ctx e pos
repetition ctx (Repetition (Just (Repeat Star lo hi)) e) pos = repeated lo hi (element ctx e) pos
repetition ctx (Repetition (Just (Repeat Hash lo hi)) e) pos = do
  form <- envForm <$> env
  list form lo hi (element ctx e) pos

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.6
-- # <a>*<b>element
-- #
-- # where <a> and <b> are optional decimal values, indicating at least
-- # <a> and at most <b> occurrences of the element.

-- | The ends of at least @lo@ and at most @hi@ matches in sequence.
--
-- The ends after exactly k matches are computed for k = 0, 1, ...
-- Below @lo@ the iteration stops early when the set stops changing
-- (the element matches the empty string). From @lo@ on, only the ends
-- not seen before are extended, since extending the others finds
-- nothing new, so the iteration stops once there are none.
repeated :: Natural -> Maybe Natural -> (Int -> R (Set Int)) -> Int -> R (Set Int)
repeated lo hi match pos
  | maybe False (< lo) hi = pure Set.empty
  | otherwise = below 0 (Set.singleton pos)
  where
    below k current
      | k >= lo = above k current current
      | Set.null current = pure Set.empty
      | otherwise = do
          next <- fromEach current match
          if next == current then above lo next next else below (k + 1) next
    above k frontier seen
      | Set.null frontier || hi == Just k = pure seen
      | otherwise = do
          next <- fromEach frontier match
          let new = next `Set.difference` seen
          above (k + 1) new (seen `Set.union` new)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.1
-- # 1#element => element *( OWS "," OWS element )

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.1
-- # #element => [ 1#element ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.1
-- # <n>#<m>element => element <n-1>*<m-1>( OWS "," OWS element )

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # #element => [ element ] *( OWS "," OWS [ element ] )

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # Empty elements do not contribute to the count of elements present.

-- | A comma-separated list of at least @lo@ and at most @hi@
-- elements. The states are an end position and the number of
-- elements matched so far, which is capped once it can no longer
-- change the outcome, so that there are finitely many states.
list :: ListForm -> Natural -> Maybe Natural -> (Int -> R (Set Int)) -> Int -> R (Set Int)
list form lo hi match pos = do
  firsts <- match pos
  let start = Set.fromList ([ (e, cap 1) | e <- Set.toList firsts ] ++ [ (pos, 0) | form == Recipient ])
  states <- explore start start
  pure (Set.fromList ([ p | (p, n) <- Set.toList states, n >= lo, maybe True (n <=) hi ]
                      ++ [ pos | form == Sender, lo == 0 ]))
  where
    -- one more than the maximum is as bad as any larger count, and
    -- with no maximum, the minimum is as good as any larger count
    cap n = maybe (min n lo) (\m -> min n (m + 1)) hi
    explore frontier seen
      | Set.null frontier = pure seen
      | otherwise = do
          next <- Set.unions <$> mapM continue (Set.toList frontier)
          let new = next `Set.difference` seen
          explore new (seen `Set.union` new)
    continue (p, n) = do
      input <- envInput <$> env
      Set.unions <$> mapM (after n) (separator input p)
    after n q = do
      es <- match q
      pure (Set.fromList ([ (e, cap (n + 1)) | e <- Set.toList es ] ++ [ (q, n) | form == Recipient ]))

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1
-- # each separated by a
-- # single comma (",") and optional whitespace (OWS, defined in
-- # Section 5.6.3).

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.3
-- # OWS            = *( SP / HTAB )

-- | The ends of @OWS \",\" OWS@ starting at a position. The first
-- OWS must be followed by the comma, and OWS is only spaces and tabs,
-- so it must be the longest run. The second may be any length up to
-- the longest, since the element after it might start with whitespace.
separator :: ByteString -> Int -> [Int]
separator input p =
  let afterOWS = p + B.length (B.takeWhile isOWS (B.drop p input))
      comma    = afterOWS + 1
      run      = B.length (B.takeWhile isOWS (B.drop comma input))
  in if B.indexMaybe input afterOWS == Just 0x2C then [comma .. comma + run] else []

isOWS :: Word8 -> Bool
isOWS w = w == 0x20 || w == 0x09

element :: Context -> Element -> Int -> R (Set Int)
element ctx@(ns, _) e pos = case e of
  RuleRef name -> rule ns name pos
  Group alt    -> alternation ctx alt pos
  -- = https://www.rfc-editor.org/rfc/rfc5234#section-3.8
  -- # Square brackets enclose an optional element sequence:
  Option alt   -> Set.insert pos <$> alternation ctx alt pos
  CharVal c s  -> literal c s pos
  NumVal _ (Values vs) -> do
    input <- envInput <$> env
    -- past the end of the input there is no byte, so no value matches
    pure $ if and (zipWith (\i v -> fmap fromIntegral (B.indexMaybe input i) == Just v) [pos ..] (toListNE vs))
             then Set.singleton (pos + length vs) else Set.empty
  NumVal _ (Range lo hi) -> do
    input <- envInput <$> env
    pure $ case B.indexMaybe input pos of
      Just w | fromIntegral w >= lo && fromIntegral w <= hi -> Set.singleton (pos + 1)
      _                                                     -> Set.empty
  ProseVal p -> failWith (uncurry ProseValue ctx p)
  where toListNE = foldr (:) []

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.1
-- # Literal text strings in ABNF may be either case sensitive or case
-- # insensitive.
literal :: Case -> ByteString -> Int -> R (Set Int)
literal c s pos = do
  input <- envInput <$> env
  let candidate = B.take (B.length s) (B.drop pos input)
      same = case c of
        CaseSensitive   -> candidate == s
        CaseInsensitive -> B.map lower candidate == B.map lower s
  pure (if B.length candidate == B.length s && same then Set.singleton (pos + B.length s) else Set.empty)
  where lower w = if w >= 0x41 && w <= 0x5A then w + 0x20 else w
