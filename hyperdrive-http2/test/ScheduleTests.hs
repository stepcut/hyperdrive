-- | The scheduler (P3) against a reference model, and the order frames
-- go out when every stream can send (P7).
module ScheduleTests (scheduleTests) where

import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Hyperdrive.HTTP2.Schedule (Schedule)
import qualified Hyperdrive.HTTP2.Schedule as Sch
import Hyperdrive.Semantics.Priority
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

scheduleTests :: TestTree
scheduleTests = testGroup "Schedule (RFC 9218)"
  [ testProperty "candidates as the model orders them, after every operation" (withMaxSuccess 2000 prop_model)
  , testProperty "examples of the order frames go out" (once examples)
  , testProperty "no frame of an urgency while a more urgent stream can send" prop_strict
  , testProperty "incremental streams, and the first non-incremental, wait at most a round" prop_noStarvation
  ]

data Op = Insert Int Priority | Delete Int | Sent Int
  deriving Show

genPriority :: Gen Priority
genPriority = fromJust <$> (mkPriority <$> elements [0, 3, 3, 7] <*> arbitrary)

instance Arbitrary Op where
  arbitrary = frequency
    [ (4, Insert <$> key <*> genPriority)
    , (2, Delete <$> key)
    , (5, Sent <$> key) ]
    where key = choose (1, 8)

-- | The model: each stream's priority and a stamp, the time it was
-- inserted or, if incremental, last sent; and for each urgency, whether
-- a non-incremental stream sent last.
data Model = Model
  { mStreams :: M.Map Int (Priority, Int)
  , mIncNext :: M.Map Int Bool
  , mClock   :: Int
  }

modelOp :: Op -> Model -> Model
modelOp op m = tidy $ case op of
  Insert k p -> case M.lookup k (mStreams m) of
    Just (p0, _) | p0 == p -> m
    _ -> tick m { mStreams = M.insert k (p, mClock m) (mStreams m) }
  Delete k -> m { mStreams = M.delete k (mStreams m) }
  Sent k -> case M.lookup k (mStreams m) of
    Nothing -> m
    Just (p, _)
      | priorityIncremental p -> tick m { mStreams = M.insert k (p, mClock m) (mStreams m)
                                        , mIncNext = M.insert (priorityUrgency p) False (mIncNext m) }
      | otherwise -> m { mIncNext = M.insert (priorityUrgency p) True (mIncNext m) }
  where
    tick m' = m' { mClock = mClock m' + 1 }
    -- an urgency with no streams left forgets which kind sent last
    tidy m' = m' { mIncNext = M.filterWithKey (\u _ -> u `elem` urgencies m') (mIncNext m') }
    urgencies m' = [ priorityUrgency p | (p, _) <- M.elems (mStreams m') ]

modelCandidates :: Model -> [Int]
modelCandidates m = concat
  [ if M.findWithDefault False u (mIncNext m) then inc ++ nonInc else nonInc ++ inc
  | u <- [0 .. 7]
  , let at = [ (k, p, t) | (k, (p, t)) <- M.toList (mStreams m), priorityUrgency p == u ]
        nonInc = [ k | (k, p, _) <- at, not (priorityIncremental p) ]
        inc = map (\(k, _, _) -> k) (sortOn (\(_, _, t) -> t) [ s | s@(_, p, _) <- at, priorityIncremental p ]) ]

applyOp :: Op -> Schedule Int -> Schedule Int
applyOp (Insert k p) = Sch.insert k p
applyOp (Delete k) = Sch.delete k
applyOp (Sent k) = Sch.sent k

prop_model :: [Op] -> Property
prop_model = go Sch.empty (Model M.empty M.empty 0)
  where
    go _ _ [] = property True
    go s m (op : ops) =
      let s' = applyOp op s
          m' = modelOp op m
      in counterexample (show op)
           (Sch.candidates s' === modelCandidates m'
            .&&. Sch.null s' === M.null (mStreams m'))
         .&&. go s' m' ops

-- | Streams (key, priority, frames), all able to send: the keys of the
-- frames in the order they go out, the first candidate each time.
run :: [(Int, Priority, Int)] -> [Int]
run streams = go (foldl (\s (k, q, _) -> Sch.insert k q s) Sch.empty streams)
                 (M.fromList [ (k, n) | (k, _, n) <- streams ])
  where
    go s left = case Sch.candidates s of
      [] -> []
      k : _ ->
        let n = left M.! k - 1
        in k : if n == 0 then go (Sch.delete k (Sch.sent k s)) (M.delete k left)
               else go (Sch.sent k s) (M.insert k n left)

prio :: Int -> Bool -> Priority
prio u i = fromJust (mkPriority u i)

-- = https://www.rfc-editor.org/rfc/rfc9218#section-10
-- # Non-incremental responses of the same urgency SHOULD be served by
-- # prioritizing bandwidth allocation in ascending order of the stream
-- # ID, which corresponds to the order in which clients make requests.
--
-- = https://www.rfc-editor.org/rfc/rfc9218#section-10
-- # Incremental responses of the same urgency SHOULD be served by sharing
-- # bandwidth among them.
examples :: Property
examples = conjoin
  [ -- non-incremental, one by one in stream order, whatever the order they came in
    run [(3, prio 3 False, 2), (1, prio 3 False, 2)] === [1, 1, 3, 3]
    -- incremental, in turn
  , run [(1, prio 3 True, 2), (3, prio 3 True, 2), (5, prio 3 True, 1)] === [1, 3, 5, 1, 3]
    -- both kinds at one urgency alternate
  , run [(1, prio 3 False, 3), (3, prio 3 True, 2), (5, prio 3 True, 2)] === [1, 3, 1, 5, 1, 3, 5]
    -- moved within its urgency, a stream leaves the alternation as it was:
    -- after a non-incremental frame, the incremental streams first
  , Sch.candidates (foldl (flip applyOp) Sch.empty
      [Insert 6 (prio 7 False), Sent 6, Insert 6 (prio 7 True), Insert 1 (prio 7 False)]) === [6, 1]
    -- strictly by urgency
  , run [(1, prio 7 True, 2), (3, prio 0 False, 2), (5, prio 3 False, 1)] === [3, 3, 5, 1, 1]
  ]

genStreams :: Gen [(Int, Priority, Int)]
genStreams = do
  n <- choose (1, 8)
  sequence [ (,,) k <$> genPriority <*> choose (1, 5) | k <- [1, 3 .. 2 * n - 1] ]

-- = https://www.rfc-editor.org/rfc/rfc9218#section-10
-- # It is RECOMMENDED that, when possible, servers respect the urgency
-- # parameter (Section 4.1), sending higher-urgency responses before
-- # lower-urgency responses.
prop_strict :: Property
prop_strict = forAll genStreams $ \streams ->
  let order = run streams
      urgencies = M.fromList [ (k, priorityUrgency q) | (k, q, _) <- streams ]
      urgency k = urgencies M.! k
      -- the urgency of each frame, and the most urgent of the streams not yet done
      before = scanl (\left k -> M.update (\x -> if x <= 1 then Nothing else Just (x - 1)) k left)
                     (M.fromList [ (k, n) | (k, _, n) <- streams ]) order
  in conjoin [ counterexample (show k) (urgency k === minimum (map urgency (M.keys left)))
             | (k, left) <- zip order before ]

-- Before an incremental stream's first frame and between two of its
-- frames, at most 2I - 1 frames of its urgency, I the incremental
-- streams of that urgency: one turn of each other incremental stream,
-- and the non-incremental frames alternating with them. For the first
-- non-incremental stream of an urgency, at most one.
--
-- = https://www.rfc-editor.org/rfc/rfc9218#section-10
-- # It is RECOMMENDED that servers avoid such starvation where possible.
prop_noStarvation :: Property
prop_noStarvation = forAll genStreams $ \streams ->
  let order = run streams
      priorities = M.fromList [ (k, q) | (k, q, _) <- streams ]
      priorityOf k = priorities M.! k
      atUrgency u = filter (\k -> priorityUrgency (priorityOf k) == u) order
      incremental u = [ k | (k, q, _) <- streams, priorityUrgency q == u, priorityIncremental q ]
      firstOrdered u = take 1 [ k | (k, q, _) <- sortOn (\(k, _, _) -> k) streams
                                  , priorityUrgency q == u, not (priorityIncremental q) ]
      gaps k = map length (splitOn k (upToLast k (atUrgency (priorityUrgency (priorityOf k)))))
  in conjoin $
       [ counterexample (show (k, order)) (all (<= 2 * length (incremental u) - 1) (gaps k))
       | u <- [0 .. 7], k <- incremental u ]
       ++
       [ counterexample (show (k, order)) (all (<= 1) (gaps k))
       | u <- [0 .. 7], k <- firstOrdered u ]
  where
    upToLast k xs = reverse (dropWhile (/= k) (reverse xs))
    splitOn k xs = case break (== k) xs of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOn k rest
