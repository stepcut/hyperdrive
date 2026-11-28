-- | Which response sends next, by RFC 9218's priorities
-- (docs/priorities-design.md, P2 and P3). Nothing of HTTP/2 here, so
-- that HTTP/3 can use it as it is: the streams are any ordered keys, and
-- the caller says which of them sent.
--
-- Strictly by urgency, 0 first. Within one urgency, the non-incremental
-- streams one by one in ascending order of their keys, the incremental
-- ones in turn, one frame each; and when both kinds are there, they
-- alternate, the first non-incremental stream that can send and the
-- incremental ones in turn (as Go's scheduler does).
module Hyperdrive.HTTP2.Schedule
  ( Schedule
  , empty
  , null
  , member
  , priorityOf
  , insert
  , delete
  , sent
  , done
  , candidates
  ) where

import Prelude hiding (null)
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Hyperdrive.Semantics.Priority

-- | The streams that have something to send, and their priorities.
data Schedule k = Schedule
  { schPriorities :: !(M.Map k Priority)
  , schLevels     :: !(IM.IntMap (Level k))   -- ^ by urgency, none empty
  } deriving (Eq, Show)

-- the streams of one urgency
data Level k = Level
  { lvOrdered         :: !(S.Set k)   -- ^ non-incremental, by key
  , lvTurns           :: !(Seq k)     -- ^ incremental, the next turn first
  , lvIncrementalNext :: !Bool        -- ^ a non-incremental stream sent last
  } deriving (Eq, Show)

empty :: Schedule k
empty = Schedule M.empty IM.empty

null :: Schedule k -> Bool
null = M.null . schPriorities

{-# INLINABLE member #-}
member :: Ord k => k -> Schedule k -> Bool
member k = M.member k . schPriorities

{-# INLINABLE priorityOf #-}
priorityOf :: Ord k => k -> Schedule k -> Maybe Priority
priorityOf k = M.lookup k . schPriorities

-- = https://www.rfc-editor.org/rfc/rfc9218#section-10
-- # Non-incremental responses of the same urgency SHOULD be served by
-- # prioritizing bandwidth allocation in ascending order of the stream
-- # ID, which corresponds to the order in which clients make requests.
--
-- = https://www.rfc-editor.org/rfc/rfc9218#section-10
-- # Incremental responses of the same urgency SHOULD be served by sharing
-- # bandwidth among them.

-- | A stream with something to send, at this priority. One already
-- there at the same priority keeps its place; at another, it moves (an
-- incremental one to the end of its urgency's turns).
{-# INLINABLE insert #-}
insert :: Ord k => k -> Priority -> Schedule k -> Schedule k
insert k p s@(Schedule ps levels)
  -- the usual case: nothing else waiting
  | M.null ps = Schedule (M.singleton k p) (IM.singleton u (put fresh))
  | otherwise = case M.lookup k ps of
      Just p0
        | p0 == p -> s
        -- within one urgency, the level stays, and with it which kind is next
        | priorityUrgency p0 == u -> Schedule ps' (IM.adjust (put . takeOut k p0) u levels)
      Just _ -> add (delete k s)
      Nothing -> add s
  where
    u = priorityUrgency p
    ps' = M.insert k p ps
    add (Schedule ps0 levels0) = Schedule (M.insert k p ps0) (IM.alter (Just . put . maybe fresh id) u levels0)
    fresh = Level S.empty Seq.empty False
    put lv
      | priorityIncremental p = lv { lvTurns = lvTurns lv |> k }
      | otherwise = lv { lvOrdered = S.insert k (lvOrdered lv) }

-- | A stream with nothing more to send, or gone. After its last frame,
-- 'sent' first, so that the frame counts as its turn.
{-# INLINABLE delete #-}
delete :: Ord k => k -> Schedule k -> Schedule k
delete k s@(Schedule ps levels) = case M.lookup k ps of
  Nothing -> s
  Just _ | M.size ps == 1 -> empty -- delete
  Just p -> Schedule (M.delete k ps) (IM.update remove (priorityUrgency p) levels) -- delete
    where
      remove lv = let lv' = takeOut k p lv in
        if S.null (lvOrdered lv') && Seq.null (lvTurns lv') then Nothing else Just lv'

-- a stream out of its urgency's level
{-# INLINABLE takeOut #-}
takeOut :: Ord k => k -> Priority -> Level k -> Level k
takeOut k p lv
  | priorityIncremental p = lv { lvTurns = case Seq.viewl (lvTurns lv) of
                                  -- usually it is first
                                  k' Seq.:< rest | k' == k -> rest -- taken out
                                  _ -> Seq.filter (/= k) (lvTurns lv) }
  | otherwise = lv { lvOrdered = S.delete k (lvOrdered lv) }

-- | A stream sent a frame: an incremental one waits for its next turn,
-- and its urgency's other kind goes first next time.
{-# INLINABLE sent #-}
sent :: Ord k => k -> Schedule k -> Schedule k
sent k s@(Schedule ps levels) = case M.lookup k ps of
  Nothing -> s
  Just p -> Schedule ps (IM.adjust after (priorityUrgency p) levels)
    where
      after lv
        | priorityIncremental p = lv { lvTurns = rotate (lvTurns lv), lvIncrementalNext = False }
        | otherwise = lv { lvIncrementalNext = True }
      -- to the end of the turns; usually it is first
      rotate q = case Seq.viewl q of
        k' Seq.:< rest | k' == k -> rest |> k
        _ -> Seq.filter (/= k) q |> k

-- | A stream sent its last frame: 'sent', then 'delete', in one pass.
{-# INLINABLE done #-}
done :: Ord k => k -> Schedule k -> Schedule k
done k s@(Schedule ps levels) = case M.lookup k ps of
  Nothing -> s
  -- the usual case: nothing else waiting
  Just _ | M.size ps == 1 -> empty -- done
  Just p -> Schedule (M.delete k ps) (IM.update remove (priorityUrgency p) levels)
    where
      remove lv
        | S.null (lvOrdered lv') && Seq.null (lvTurns lv') = Nothing
        | otherwise = Just lv' { lvIncrementalNext = not (priorityIncremental p) }
        where lv' = takeOut k p lv

-- = https://www.rfc-editor.org/rfc/rfc9218#section-10
-- # It is RECOMMENDED that, when possible, servers respect the urgency
-- # parameter (Section 4.1), sending higher-urgency responses before
-- # lower-urgency responses.
--
-- = https://www.rfc-editor.org/rfc/rfc9218#section-10
-- # It is RECOMMENDED that servers avoid such starvation where possible.

-- | The streams in the order to try them: the first that can send
-- should. Lazy, so that taking the first few costs little.
candidates :: Schedule k -> [k]
candidates = concatMap order . IM.elems . schLevels
  where
    order lv
      | lvIncrementalNext lv = toList (lvTurns lv) ++ S.toAscList (lvOrdered lv)
      | otherwise = S.toAscList (lvOrdered lv) ++ toList (lvTurns lv)
