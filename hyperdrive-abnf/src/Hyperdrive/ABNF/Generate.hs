-- | QuickCheck generators for the strings a rule matches, and for
-- strings that nearly do.
--
-- Valid strings are derived from the grammar. Every rule has a
-- minimum derivation depth, the fewest rule expansions needed to
-- produce any string at all; generation is given a depth budget and
-- only chooses alternatives that fit in it, so recursive rules always
-- terminate. Alternatives that cannot produce anything (prose values,
-- values above 255, rules imported from a grammar the library does
-- not have) are never chosen.
--
-- Near misses are valid strings with a few small edits, using bytes
-- the grammar treats specially. Some edits leave the string valid, so
-- near misses must be classified with "Hyperdrive.ABNF.Recognize";
-- what makes them useful is that they sit close to the boundary of the
-- language, where parsers go wrong.
module Hyperdrive.ABNF.Generate
  ( GenerateError(..)
  , validStrings
  , nearMisses
  ) where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as L
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import Hyperdrive.ABNF.Library
import Hyperdrive.ABNF.Recognize (ListForm(..))
import Hyperdrive.ABNF.Types
import Numeric.Natural (Natural)
import Test.QuickCheck

data GenerateError
  = GenerateLookup LookupError
    -- ^ the rule could not be found
  | Ungeneratable Namespace RuleName
    -- ^ every way of deriving the rule needs something that cannot be
    -- generated
  deriving (Eq, Show)

-- | Strings the rule matches, with lists in the given form.
validStrings :: ListForm -> Library -> ByteString -> RuleName -> Either GenerateError (Gen ByteString)
validStrings form lib tag name = do
  (info, key) <- prepare lib tag name
  pure (build <$> generateRule form info key)

-- | Strings near the boundary of the rule's language: valid strings
-- with one to three edits. Classify them with the recognizer.
nearMisses :: ListForm -> Library -> ByteString -> RuleName -> Either GenerateError (Gen ByteString)
nearMisses form lib tag name = do
  (info, key) <- prepare lib tag name
  let bytes = interestingBytes info
  pure $ do
    s <- build <$> generateRule form info key
    n <- choose (1, 3)
    iterateM n (mutate bytes) s
  where
    iterateM :: Int -> (a -> Gen a) -> a -> Gen a
    iterateM 0 _ a = pure a
    iterateM k f a = f a >>= iterateM (k - 1) f

build :: BB.Builder -> ByteString
build = L.toStrict . BB.toLazyByteString

------------------------------------------------------------------------------
-- minimum derivation depths
------------------------------------------------------------------------------

type Key = (Namespace, RuleName)

data Info = Info
  { infoLibrary :: Library
  , infoRules   :: Map Key Alternation  -- ^ every rule reachable from the start
  , infoHeights :: Map Key Int          -- ^ the rules that can be generated
  }

prepare :: Library -> ByteString -> RuleName -> Either GenerateError (Info, Key)
prepare lib tag name = do
  (ns, name', alt) <- either (Left . GenerateLookup) Right (resolve lib (Tagged tag) name)
  let key = (ns, name')
      rules = reachable lib (Map.singleton key alt) [key]
      info = Info lib rules (heights lib rules)
  if Map.member key (infoHeights info) then Right (info, key) else Left (uncurry Ungeneratable key)

-- | where a name used in a namespace is defined, if anywhere
resolveKey :: Library -> Namespace -> RuleName -> Maybe (Key, Alternation)
resolveKey lib ns n = case resolve lib ns n of
  Right (ns', n', alt) -> Just ((ns', n'), alt)
  Left _               -> Nothing

reachable :: Library -> Map Key Alternation -> [Key] -> Map Key Alternation
reachable _ seen [] = seen
reachable lib seen ((ns, n) : todo) =
  let alt = seen Map.! (ns, n)
      new = [ (k, a) | r <- refs alt, Just (k, a) <- [resolveKey lib ns r], not (Map.member k seen) ]
      seen' = foldr (uncurry Map.insert) seen new
  in reachable lib seen' (nub (map fst new) ++ todo)

refs :: Alternation -> [RuleName]
refs (Alternation cs) = concatMap (\(Concatenation rs) -> concatMap rep (NE.toList rs)) (NE.toList cs)
  where
    rep (Repetition _ e) = case e of
      RuleRef n -> [n]
      Group a   -> refs a
      Option a  -> refs a
      _         -> []

-- | The minimum derivation depth of each rule that can be generated,
-- by iterating to a fixed point from "none can be".
heights :: Library -> Map Key Alternation -> Map Key Int
heights lib rules = go Map.empty
  where
    go hs =
      let hs' = Map.mapMaybeWithKey (\(ns, _) alt -> (+ 1) <$> altHeight lib hs ns alt) rules
      in if hs' == hs then hs else go hs'

altHeight :: Library -> Map Key Int -> Namespace -> Alternation -> Maybe Int
altHeight lib hs ns (Alternation cs) = minimumMaybe (mapMaybe (concHeight lib hs ns) (NE.toList cs))

concHeight :: Library -> Map Key Int -> Namespace -> Concatenation -> Maybe Int
concHeight lib hs ns (Concatenation rs) = maximum <$> traverse (repHeight lib hs ns) (NE.toList rs)

repHeight :: Library -> Map Key Int -> Namespace -> Repetition -> Maybe Int
repHeight lib hs ns (Repetition r e) = case r of
  Nothing -> elHeight lib hs ns e
  Just (Repeat _ lo hi)
    | maybe False (< lo) hi -> Nothing
    | lo == 0               -> Just 0
    | otherwise             -> elHeight lib hs ns e

elHeight :: Library -> Map Key Int -> Namespace -> Element -> Maybe Int
elHeight lib hs ns e = case e of
  RuleRef n -> resolveKey lib ns n >>= \(k, _) -> Map.lookup k hs
  Group a   -> altHeight lib hs ns a
  Option _  -> Just 0
  CharVal _ _ -> Just 0
  NumVal _ (Values vs) | all (<= 255) vs -> Just 0
  NumVal _ (Range lo hi) | lo <= hi && lo <= 255 -> Just 0
  _ -> Nothing

minimumMaybe :: [Int] -> Maybe Int
minimumMaybe [] = Nothing
minimumMaybe xs = Just (minimum xs)

------------------------------------------------------------------------------
-- valid strings
------------------------------------------------------------------------------

generateRule :: ListForm -> Info -> Key -> Gen BB.Builder
generateRule form info key = do
  extra <- frequency [(4, pure 0), (3, pure 1), (2, pure 2), (1, pure 4)]
  rule key (infoHeights info Map.! key + extra)
  where
    lib = infoLibrary info
    hs  = infoHeights info

    -- a rule with depth budget b, where its height is at most b
    rule k@(ns, _) b = alternation ns (infoRules info Map.! k) (b - 1)

    alternation ns (Alternation cs) b =
      case [ c | c <- NE.toList cs, maybe False (<= b) (concHeight lib hs ns c) ] of
        []      -> error "Hyperdrive.ABNF.Generate: no alternative fits the budget"
        options -> elements options >>= concatenation ns b

    concatenation ns b (Concatenation rs) = mconcat <$> mapM (repetition ns b) (NE.toList rs)

    repetition ns b (Repetition r e) = case r of
      Nothing -> element ns b e
      Just (Repeat kind lo hi) -> do
        let fits = maybe False (<= b) (elHeight lib hs ns e)
        k <- if fits then count lo hi else pure (fromIntegral lo)
        -- the items share the size, so that nested repetitions do not
        -- multiply it
        let item = scale (`div` max 1 k) (element ns b e)
        case kind of
          Star -> mconcat <$> replicateM k item
          Hash -> list k item

    -- between lo and hi: usually a few more than lo, never more extra
    -- than the size
    count :: Natural -> Maybe Natural -> Gen Int
    count lo hi = sized $ \size -> do
      extra <- frequency [(5, choose (0, min 2 size)), (2, choose (0, min 5 size)), (1, choose (0, min 12 size))]
      let n = lo + fromIntegral (extra :: Int)
      pure (fromIntegral (maybe n (min n) hi))

    list k el = do
      items <- replicateM k el
      items' <- case form of
        Sender    -> pure items
        Recipient -> concat <$> mapM (\i -> do e <- empties; pure (e ++ [i])) items
                       >>= \xs -> (xs ++) <$> empties
      case items' of
        [] -> pure mempty
        (x : xs) -> mconcat . (x :) <$> mapM (\i -> (<> i) <$> separator) xs

    -- empty list elements, for recipients
    empties = frequency [(4, pure []), (1, flip replicate mempty <$> choose (1, 2))]

    -- OWS "," OWS, as in RFC 9110's expansion of lists
    separator = do
      a <- ows
      c <- ows
      pure (a <> BB.char7 ',' <> c)
    ows = mconcat <$> (choose (0, 2 :: Int) >>= \n -> replicateM n (elements [BB.char7 ' ', BB.char7 '\t']))

    element ns b e = case e of
      RuleRef n -> case resolveKey lib ns n of
        Just (k, _) -> rule k b
        Nothing     -> error "Hyperdrive.ABNF.Generate: unresolved rule"
      Group a -> alternation ns a b
      Option a
        | maybe False (<= b) (altHeight lib hs ns a) -> oneof [pure mempty, alternation ns a b]
        | otherwise -> pure mempty
      CharVal CaseSensitive s -> pure (BB.byteString s)
      -- any mixture of upper and lower case
      CharVal CaseInsensitive s -> BB.byteString . B.pack <$> mapM anyCase (B.unpack s)
      NumVal _ (Values vs) -> pure (foldMap (BB.word8 . fromIntegral) vs)
      NumVal _ (Range lo hi) -> BB.word8 . fromIntegral <$> chooseNatural lo (min hi 255)
      ProseVal _ -> error "Hyperdrive.ABNF.Generate: prose"

    anyCase w
      | w >= 0x41 && w <= 0x5A = elements [w, w + 0x20]
      | w >= 0x61 && w <= 0x7A = elements [w, w - 0x20]
      | otherwise              = pure w

chooseNatural :: Natural -> Natural -> Gen Natural
chooseNatural lo hi = fromInteger <$> choose (toInteger lo, toInteger hi)

------------------------------------------------------------------------------
-- near misses
------------------------------------------------------------------------------

-- | The bytes at the edges of the grammar's terminals, and bytes that
-- are often mishandled.
interestingBytes :: Info -> [Word8]
interestingBytes info = nub (special ++ concatMap fromAlt (Map.elems (infoRules info)))
  where
    special = [0x00, 0x09, 0x0A, 0x0D, 0x20, 0x22, 0x2C, 0x7F, 0x80, 0xFF]
    fromAlt (Alternation cs) = concatMap (\(Concatenation rs) -> concatMap fromRep (NE.toList rs)) (NE.toList cs)
    fromRep (Repetition _ e) = case e of
      Group a -> fromAlt a
      Option a -> fromAlt a
      CharVal _ s -> concatMap cases (B.unpack s)
      NumVal _ (Values vs) -> mapMaybe byte (NE.toList vs)
      NumVal _ (Range lo hi) -> mapMaybe byte [lo, hi] ++ mapMaybe byte [hi + 1] ++ [ b | lo > 0, Just b <- [byte (lo - 1)] ]
      _ -> []
    cases w | w >= 0x41 && w <= 0x5A = [w, w + 0x20]
            | w >= 0x61 && w <= 0x7A = [w, w - 0x20]
            | otherwise = [w]
    byte v = if v <= 255 then Just (fromIntegral v) else Nothing

mutate :: [Word8] -> ByteString -> Gen ByteString
mutate bytes s
  | B.null s = B.singleton <$> elements bytes
  | otherwise = do
      i <- choose (0, B.length s - 1)
      let (before, after) = B.splitAt i s
      oneof
        [ (\w -> before <> B.singleton w <> B.drop 1 after) <$> elements bytes  -- replace
        , (\w -> before <> B.singleton w <> after) <$> elements bytes           -- insert
        , pure (before <> B.drop 1 after)                                       -- delete
        , pure before                                                           -- truncate
        , (\n -> before <> B.take n after <> after) <$> choose (1, 4)           -- duplicate
        , pure (case B.unpack (B.take 2 after) of                               -- swap
                  [a, b] -> before <> B.pack [b, a] <> B.drop 2 after
                  _      -> s)
        ]
