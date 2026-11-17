{-# LANGUAGE OverloadedStrings #-}
-- | Decide whether a string matches a rule of an ABNF grammar.
--
-- The recognizer computes, for each element and starting position,
-- the set of every position at which a match of the element can end.
-- Nothing is matched greedily and no alternative is preferred, so
-- ambiguity and repetition mean exactly what RFC 5234 says they mean:
-- a string matches a rule when some way of matching the rule ends at
-- the end of the string.
--
-- A rule is first compiled: names are resolved through the library,
-- and the rules and repetitions reachable from it are numbered, so
-- that results can be memoised by number and position. Partially
-- applying 'matches' or 'matchEnds' to a rule compiles it once for
-- any number of inputs.
--
-- Terminal values are matched against the bytes of the input, so a
-- value above 255 never matches.
module Hyperdrive.ABNF.Recognize
  ( -- * Grammars that refer to each other
    Library
  , library
  , Namespace(..)
  , LookupError(..)
    -- * Recognizing
  , ListForm(..)
  , matches
  , matchEnds
  , RecognizeError(..)
    -- * Coverage
  , coverage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad (forM_, unless, when)
import Data.Foldable (foldlM, toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8)
import Hyperdrive.ABNF.Library
import Hyperdrive.ABNF.Types
import Numeric.Natural (Natural)

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
  = Lookup LookupError
    -- ^ a rule could not be found
  | ProseValue Namespace RuleName ByteString
    -- ^ a prose value had to be matched, which cannot be done
  | LeftRecursion Namespace RuleName
    -- ^ a rule was reached again at the same position, without
    -- consuming input
  deriving (Eq, Show)

-- | Does the whole input match the rule of the grammar with the tag?
matches :: ListForm -> Library -> ByteString -> RuleName -> ByteString -> Either RecognizeError Bool
matches form lib tag name =
  let ends = matchEnds' form lib tag name
  in \input -> IntSet.member (B.length input) <$> ends input

-- | Every position at which a match of the rule, starting at the
-- beginning of the input, can end.
matchEnds :: ListForm -> Library -> ByteString -> RuleName -> ByteString -> Either RecognizeError (Set Int)
matchEnds form lib tag name =
  let ends = matchEnds' form lib tag name
  in \input -> Set.fromDistinctAscList . IntSet.toAscList <$> ends input

matchEnds' :: ListForm -> Library -> ByteString -> RuleName -> ByteString -> Either RecognizeError IntSet
matchEnds' form lib tag name
  | libraryHas lib tag =
      let program = compile True lib (Tagged tag) name
      in \input -> fst <$> run (node (programStart program) 0) (Env form program input) emptyMemo
  | otherwise = const (Left (Lookup (UnknownGrammar tag)))

-- | For every rule reachable from the given one, how many alternatives
-- it has (at the top level) and which of them take part in some way of
-- matching one of the inputs in full. Inputs the rule does not match
-- are ignored. This tells which parts of a grammar a set of test inputs
-- exercises.
coverage :: ListForm -> Library -> ByteString -> RuleName -> [ByteString]
         -> Either RecognizeError (Map (Namespace, RuleName) (Int, IntSet))
coverage form lib tag name inputs
  | not (libraryHas lib tag) = Left (Lookup (UnknownGrammar tag))
  | otherwise = do
      used <- foldlM one IntMap.empty inputs
      pure $ Map.fromList
        [ ((ns, n), (alternatives body, IntMap.findWithDefault IntSet.empty i used))
        | (i, (ns, n, body)) <- IntMap.toList (programRules program) ]
  where
    -- without byte classes, so that every rule keeps its alternatives
    program = compile False lib (Tagged tag) name
    alternatives (NAlt as) = length as
    alternatives _         = 1
    one used input = do
      let end = B.length input
          go = do
            ends <- node (programStart program) 0
            when (IntSet.member end ends) (mark (programStart program) 0 end)
      (_, memo) <- run go (Env form program input) emptyMemo
      pure (IntMap.unionWith IntSet.union used (memoUsed memo))

------------------------------------------------------------------------------
-- compiling
------------------------------------------------------------------------------

-- | An element with its names resolved. Rules and repetitions carry
-- numbers to memoise them by.
data Node
  = NAlt [Node]
  | NSeq [Node]
  | NOption Node
  | NRule !Int
  | NRepeat !Int RepeatKind Natural (Maybe Natural) Node
  | NLiteral Case ByteString
  | NBytes [Word8]
  | NClass ByteString        -- ^ one byte, from the set: byte w is in it when index w is 1
  | NNothing                 -- ^ matches nothing: a value above 255
  | NError RecognizeError    -- ^ fails when it has to be matched

data Program = Program
  { programRules :: IntMap (Namespace, RuleName, Node)
  , programStart :: Node
  }

-- | Compile the rule and every rule reachable from it. Names that
-- cannot be resolved and prose values become errors where they are
-- used, so that they only fail a match that reaches them.
compile :: Bool -> Library -> Namespace -> RuleName -> Program
compile classes lib ns0 name0 =
  let (start, st) = runC (ref ns0 name0) (CState Map.empty IntMap.empty [] 0)
      st' = drain st
      program = Program (csCompiled st') start
  in if classes then classify program else program
  where
    -- compile the rules found but not yet compiled
    drain st = case csTodo st of
      [] -> st
      (i, (ns, name, alt)) : rest ->
        let (n, st') = runC (alternation ns name alt) st { csTodo = rest }
        in drain st' { csCompiled = IntMap.insert i (ns, name, n) (csCompiled st') }

    ref ns name = case resolve lib ns name of
      Left err -> pure (NError (Lookup err))
      Right (ns', name', alt) -> C $ \st -> case Map.lookup (ns', name') (csIds st) of
        Just i  -> (NRule i, st)
        Nothing ->
          let i = Map.size (csIds st)
          in (NRule i, st { csIds = Map.insert (ns', name') i (csIds st)
                          , csTodo = csTodo st ++ [(i, (ns', name', alt))] })

    alternation ns name (Alternation cs) = NAlt <$> mapM (concatenation ns name) (toList cs)
    concatenation ns name (Concatenation rs) = NSeq <$> mapM (repetition ns name) (toList rs)
    repetition ns name (Repetition r e) = do
      n <- element ns name e
      case r of
        Nothing -> pure n
        Just (Repeat kind lo hi) -> C $ \st ->
          (NRepeat (csRepeats st) kind lo hi n, st { csRepeats = csRepeats st + 1 })
    element ns name e = case e of
      RuleRef r  -> ref ns r
      Group a    -> alternation ns name a
      Option a   -> NOption <$> alternation ns name a
      CharVal c s -> pure (NLiteral c s)
      NumVal _ (Values vs)
        | all (<= 255) vs -> pure (NBytes (map fromIntegral (toList vs)))
        | otherwise       -> pure NNothing
      NumVal _ (Range lo hi)
        | lo <= 255 && lo <= hi -> pure (NClass (setOf [fromIntegral lo .. fromIntegral (min hi 255)]))
        | otherwise             -> pure NNothing
      ProseVal p -> pure (NError (ProseValue ns name p))

-- | Replace every rule and alternative whose language is a set of
-- single bytes (ALPHA, tchar, "/" and so on) by a lookup table, which
-- is much cheaper than matching it as a rule. Only the cost changes:
-- a table matches exactly the bytes the rule does.
classify :: Program -> Program
classify (Program rules start) = Program (fmap (\(ns, n, b) -> (ns, n, go b)) rules) (go start)
  where
    -- the byte set of each rule that has one, found by iterating to a
    -- fixed point (a rule that refers to itself never has one)
    classes = fixpoint IntMap.empty
    fixpoint known =
      let known' = IntMap.mapMaybe (\(_, _, b) -> byteSet known b) rules
      in if IntMap.keys known' == IntMap.keys known then known' else fixpoint known'
    go n = case byteSet classes n of
      Just set -> NClass set
      Nothing  -> case n of
        NAlt ns -> NAlt (map go ns)
        NSeq ns -> NSeq (map go ns)
        NOption m -> NOption (go m)
        NRepeat i k lo hi m -> NRepeat i k lo hi (go m)
        _ -> n

-- | the set of single bytes a node matches, if that is all it matches
byteSet :: IntMap ByteString -> Node -> Maybe ByteString
byteSet known n = case n of
  NAlt ns -> foldr1 orSets <$> (nonEmpty =<< traverse (byteSet known) ns)
  NSeq [m] -> byteSet known m
  NRule i -> IntMap.lookup i known
  NLiteral CaseSensitive s | B.length s == 1 -> Just (setOf [B.head s])
  NLiteral CaseInsensitive s | B.length s == 1 -> Just (setOf (cases (B.head s)))
  NBytes [w] -> Just (setOf [w])
  NClass set -> Just set
  _ -> Nothing
  where
    nonEmpty [] = Nothing
    nonEmpty xs = Just xs
    orSets a b = B.pack (B.zipWith max a b)
    cases w | w >= 0x41 && w <= 0x5A = [w, w + 0x20]
            | w >= 0x61 && w <= 0x7A = [w, w - 0x20]
            | otherwise              = [w]

-- | the set of the bytes: byte w is in it when index w is 1
setOf :: [Word8] -> ByteString
setOf ws = B.pack [ if w `elem` ws then 1 else 0 | w <- [0 .. 255] ]

inClass :: ByteString -> Word8 -> Bool
inClass set w = B.index set (fromIntegral w) == 1

data CState = CState
  { csIds      :: Map (Namespace, RuleName) Int
  , csCompiled :: IntMap (Namespace, RuleName, Node)
  , csTodo     :: [(Int, (Namespace, RuleName, Alternation))]
  , csRepeats  :: !Int
  }

newtype C a = C { runC :: CState -> (a, CState) }

instance Functor C where
  fmap f (C g) = C $ \s -> let (a, s') = g s in (f a, s')

instance Applicative C where
  pure a = C $ \s -> (a, s)
  cf <*> ca = C $ \s -> let (f, s') = runC cf s; (a, s'') = runC ca s' in (f a, s'')

instance Monad C where
  C g >>= k = C $ \s -> let (a, s') = g s in runC (k a) s'

------------------------------------------------------------------------------
-- the recognizer monad: the environment, a memo table, and errors
------------------------------------------------------------------------------

data Env = Env
  { envForm    :: ListForm
  , envProgram :: Program
  , envInput   :: ByteString
  }

data Entry = InProgress | Done IntSet

-- | Results for rules, and for repetitions, which would otherwise be
-- recomputed from every position an enclosing repetition reaches:
-- nested repetitions would cost the input length to the power of the
-- nesting depth. Both are keyed by number and position.
data Memo = Memo
  { memoRules       :: IntMap (IntMap Entry)
  , memoRepetitions :: IntMap (IntMap IntSet)
    -- | for 'coverage': the alternatives found to take part in a match
  , memoUsed        :: IntMap IntSet
    -- | for 'coverage': the rules already marked, with their spans
  , memoMarked      :: Set (Int, Int, Int)
  }

emptyMemo :: Memo
emptyMemo = Memo IntMap.empty IntMap.empty IntMap.empty Set.empty

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

lookup2 :: Int -> Int -> IntMap (IntMap a) -> Maybe a
lookup2 i pos m = IntMap.lookup i m >>= IntMap.lookup pos

insert2 :: Int -> Int -> a -> IntMap (IntMap a) -> IntMap (IntMap a)
insert2 i pos a = IntMap.insertWith IntMap.union i (IntMap.singleton pos a)

------------------------------------------------------------------------------
-- matching
------------------------------------------------------------------------------

node :: Node -> Int -> R IntSet
node n pos = case n of
  -- = https://www.rfc-editor.org/rfc/rfc5234#section-3.2
  -- # Elements separated by a forward slash ("/") are alternatives.
  NAlt ns -> IntSet.unions <$> mapM (`node` pos) ns
  -- = https://www.rfc-editor.org/rfc/rfc5234#section-3.1
  -- # A rule can define a simple, ordered string of values (i.e., a
  -- # concatenation of contiguous characters) by listing a sequence of rule
  -- # names.
  NSeq ns -> foldlM (\starts m -> fromEach starts (node m)) (IntSet.singleton pos) ns
  -- = https://www.rfc-editor.org/rfc/rfc5234#section-3.8
  -- # Square brackets enclose an optional element sequence:
  NOption m -> IntSet.insert pos <$> node m pos
  NRule i -> rule i pos
  -- a run of single bytes: k repetitions end k bytes on, for every k
  -- between the bounds that the run is long enough for
  NRepeat _ Star lo hi (NClass set) -> do
    input <- envInput <$> env
    let run' = B.length (B.takeWhile (inClass set) (B.drop pos input))
        top  = maybe run' (min run' . fromIntegral) hi
    -- empty when the run is too short, or the maximum is below the minimum
    pure (IntSet.fromDistinctAscList [pos + fromIntegral lo .. pos + top])
  NRepeat i kind lo hi m -> do
    memo <- getMemo
    case lookup2 i pos (memoRepetitions memo) of
      Just ends -> pure ends
      Nothing -> do
        form <- envForm <$> env
        ends <- case kind of
          Star -> repeated lo hi (node m) pos
          Hash -> list form lo hi (node m) pos
        memo' <- getMemo
        putMemo memo' { memoRepetitions = insert2 i pos ends (memoRepetitions memo') }
        pure ends
  NLiteral c s -> literal c s pos
  NBytes ws -> do
    input <- envInput <$> env
    pure $ if B.pack ws == B.take (length ws) (B.drop pos input)
             then IntSet.singleton (pos + length ws) else IntSet.empty
  NClass set -> do
    input <- envInput <$> env
    pure $ case B.indexMaybe input pos of
      Just w | inClass set w -> IntSet.singleton (pos + 1)
      _                      -> IntSet.empty
  NNothing -> pure IntSet.empty
  NError err -> failWith err

rule :: Int -> Int -> R IntSet
rule i pos = do
  memo <- getMemo
  case lookup2 i pos (memoRules memo) of
    Just (Done ends) -> pure ends
    Just InProgress  -> do
      (ns, name, _) <- (IntMap.! i) . programRules . envProgram <$> env
      failWith (LeftRecursion ns name)
    Nothing -> do
      putMemo memo { memoRules = insert2 i pos InProgress (memoRules memo) }
      (_, _, body) <- (IntMap.! i) . programRules . envProgram <$> env
      ends <- node body pos
      memo' <- getMemo
      putMemo memo' { memoRules = insert2 i pos (Done ends) (memoRules memo') }
      pure ends

-- | the union of the ends reached from each start
fromEach :: IntSet -> (Int -> R IntSet) -> R IntSet
fromEach starts f = IntSet.unions <$> mapM f (IntSet.toList starts)

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
repeated :: Natural -> Maybe Natural -> (Int -> R IntSet) -> Int -> R IntSet
repeated lo hi match pos
  | maybe False (< lo) hi = pure IntSet.empty
  | otherwise = below 0 (IntSet.singleton pos)
  where
    below k current
      | k >= lo = above k current current
      | IntSet.null current = pure IntSet.empty
      | otherwise = do
          next <- fromEach current match
          if next == current then above lo next next else below (k + 1) next
    above k frontier seen
      | IntSet.null frontier || hi == Just k = pure seen
      | otherwise = do
          next <- fromEach frontier match
          let new = next `IntSet.difference` seen
          above (k + 1) new (seen `IntSet.union` new)

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
list :: ListForm -> Natural -> Maybe Natural -> (Int -> R IntSet) -> Int -> R IntSet
list form lo hi match pos = do
  firsts <- match pos
  let start = Set.fromList ([ (e, cap 1) | e <- IntSet.toList firsts ] ++ [ (pos, 0) | form == Recipient ])
  states <- explore start start
  pure (IntSet.fromList ([ p | (p, n) <- Set.toList states, n >= lo, maybe True (n <=) hi ]
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
      pure (Set.fromList ([ (e, cap (n + 1)) | e <- IntSet.toList es ] ++ [ (q, n) | form == Recipient ]))

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
      run'     = B.length (B.takeWhile isOWS (B.drop comma input))
  in if B.indexMaybe input afterOWS == Just 0x2C then [comma .. comma + run'] else []

isOWS :: Word8 -> Bool
isOWS w = w == 0x20 || w == 0x09

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.1
-- # Literal text strings in ABNF may be either case sensitive or case
-- # insensitive.
literal :: Case -> ByteString -> Int -> R IntSet
literal c s pos = do
  input <- envInput <$> env
  let candidate = B.take (B.length s) (B.drop pos input)
      same = case c of
        CaseSensitive   -> candidate == s
        CaseInsensitive -> B.map lower candidate == B.map lower s
  pure (if B.length candidate == B.length s && same then IntSet.singleton (pos + B.length s) else IntSet.empty)
  where lower w = if w >= 0x41 && w <= 0x5A then w + 0x20 else w

------------------------------------------------------------------------------
-- coverage
------------------------------------------------------------------------------

-- | Mark the alternatives that take part in some way of matching the
-- node exactly from one position to the other. Only parts that lie on
-- such a match are visited: for a sequence, the positions between its
-- items must both be reachable from the start and able to reach the
-- end, and likewise for repetitions and lists, whose counts are
-- tracked as in matching.
mark :: Node -> Int -> Int -> R ()
mark n s e = case n of
  NAlt as -> forM_ as $ \a -> do
    ends <- node a s
    when (IntSet.member e ends) (mark a s e)
  NSeq ns -> do
    -- the positions after each item reachable from the start
    forward <- scanM (\starts m -> fromEach starts (node m)) (IntSet.singleton s) ns
    -- of those, the ones from which the rest of the sequence can reach
    -- the end, working back from the end
    let back [] acc = pure acc
        back _ [] = pure []
        back ((m, before) : rest) acc@(after : _) = do
          keep <- filterM' (\p -> not . IntSet.null . IntSet.intersection after <$> node m p) before
          back rest (keep : acc)
    alive <- back (reverse (zip ns forward)) [IntSet.singleton e `IntSet.intersection` last forward]
    forM_ (zip3 ns alive (drop 1 alive)) $ \(m, before, after) ->
      forM_ (IntSet.toList before) $ \p -> do
        ends <- node m p
        forM_ (IntSet.toList (ends `IntSet.intersection` after)) (mark m p)
  NOption m -> do
    ends <- node m s
    when (IntSet.member e ends) (mark m s e)
  NRule i -> do
    memo <- getMemo
    unless (Set.member (i, s, e) (memoMarked memo)) $ do
      putMemo memo { memoMarked = Set.insert (i, s, e) (memoMarked memo) }
      (_, _, body) <- (IntMap.! i) . programRules . envProgram <$> env
      case body of
        NAlt as -> forM_ (zip [0 ..] as) $ \(k, a) -> do
          ends <- node a s
          when (IntSet.member e ends) $ do
            memo' <- getMemo
            putMemo memo' { memoUsed = IntMap.insertWith IntSet.union i (IntSet.singleton k) (memoUsed memo') }
            mark a s e
        _ -> mark body s e
  NRepeat _ Star lo hi m -> do
    let cap c = maybe (min c lo) (min c) hi
        initial = [((s, 0), Nothing)]
        -- a repetition that matches nothing only counts towards the
        -- minimum: beyond it, it would make any alternative that can
        -- match the empty string look covered
        stepFrom (p, c)
          | hi == Just c = pure []
          | otherwise = do
              ends <- node m p
              pure [ ((q, cap (c + 1)), Just (p, q)) | q <- IntSet.toList ends, q /= p || c < lo ]
    spans <- onAcceptedPaths initial stepFrom (\(p, c) -> p == e && c >= lo)
    forM_ spans (\(p, q) -> mark m p q)
  NRepeat _ Hash lo hi m -> do
    form <- envForm <$> env
    input <- envInput <$> env
    firsts <- node m s
    let cap c = maybe (min c lo) (\h -> min c (h + 1)) hi
        initial = [ ((q, cap 1), Just (s, q)) | q <- IntSet.toList firsts ]
                  ++ [ ((s, 0), Nothing) | form == Recipient ]
        stepFrom (p, c) = concat <$> mapM (\q -> do
          es <- node m q
          pure ([ ((x, cap (c + 1)), Just (q, x)) | x <- IntSet.toList es ]
                ++ [ ((q, c), Nothing) | form == Recipient ])) (separator input p)
    spans <- onAcceptedPaths initial stepFrom (\(p, c) -> p == e && c >= lo && maybe True (c <=) hi)
    forM_ spans (\(p, q) -> mark m p q)
  _ -> pure ()

-- | The labels of the edges that lie on a path from the initial edges to
-- an accepting state, in a finite graph given by its edges out of each
-- state.
onAcceptedPaths :: [((Int, Natural), Maybe (Int, Int))]
                -> ((Int, Natural) -> R [((Int, Natural), Maybe (Int, Int))])
                -> ((Int, Natural) -> Bool)
                -> R [(Int, Int)]
onAcceptedPaths initial stepFrom accepting = do
  (edges, states) <- explore [ s | (s, _) <- initial ] (Set.fromList [ s | (s, _) <- initial ]) []
  let allEdges = [ (Nothing, to, l) | (to, l) <- initial ] ++ edges
      alive = grow (Set.filter accepting states)
      grow live =
        let live' = live `Set.union` Set.fromList [ f | (Just f, t, _) <- allEdges, Set.member t live ]
        in if Set.size live' == Set.size live then live else grow live'
  pure [ span' | (_, t, Just span') <- allEdges, Set.member t alive ]
  where
    explore [] seen acc = pure (acc, seen)
    explore (x : todo) seen acc = do
      out <- stepFrom x
      let new = [ t | (t, _) <- out, not (Set.member t seen) ]
      explore (new ++ todo) (foldr Set.insert seen new) ([ (Just x, t, l) | (t, l) <- out ] ++ acc)

scanM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanM _ z [] = pure [z]
scanM f z (x : xs) = do
  z' <- f z x
  (z :) <$> scanM f z' xs

filterM' :: Monad m => (Int -> m Bool) -> IntSet -> m IntSet
filterM' p set = IntSet.fromList <$> filterM'' (IntSet.toList set)
  where
    filterM'' [] = pure []
    filterM'' (x : xs) = do
      keep <- p x
      rest <- filterM'' xs
      pure (if keep then x : rest else rest)
