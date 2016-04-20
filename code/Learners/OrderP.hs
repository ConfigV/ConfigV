{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses#-}

module Learners.OrderP where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple

import Debug.Trace

-- for tuning which probabilistic rules to throw out
--  (for some reason we are seeing a lot of rules at 66% and 33% split)
cutoffProb = 0.7
cutoffPerc = 0.1
-- our "strategy" for which rules to believe
filterRuleSet :: OrdMap (Integer, Integer) -> OrdMap (Integer, Integer)
filterRuleSet rs =
  let
    -- make sure rule is above a threshold probability
    filterProbs p (y, n) =
      let
        y' = fromIntegral y
        n' = fromIntegral n
        prob = y' / (y' + n')
      in
        prob >= p
    -- make sure rule has been verified by enough files
    obs = reverse $ L.sort $ map (\(y, n) -> y + n) $ M.elems rs
    cutoffObs = obs !! (round $ cutoffPerc * (fromIntegral.length) obs)
    filterObs (y, n) = y + n >= cutoffObs
  in
    M.filter filterObs $ M.filter (filterProbs cutoffProb) rs

-- should we consider a version of this with (Keyword, Keyword) instead of the whole line?
instance Attribute (M.Map (IRLine,IRLine)) (Integer, Integer) where
  learn f = let
    -- fromList automatically does duplicate removal [(a,b), (a,c)] -> [(a,b)]
    --  (is this the behavior we want?)
     x = M.fromList $ pairs f
   in
     -- keeps counts straight for (a,b) and (b,a) and deletes redundant copy
     M.foldrWithKey removeConflicts x x

  -- MAKE SURE WE TAKE RULESET AND EXPAND OUT ANTIPAIRS BEFORE CHECKING (Ideally both (a,b) and (b,a) should be available for checking)
  check rs f =
   let
     invPairs m = M.fromList $ map (\((l1, l2), (y, n)) -> ((l2, l1), (n, y))) $ M.toList m
     relevantRules = M.filterWithKey (hasRuleFor f) (M.union rs (invPairs rs))
     fRules = learn f :: OrdMap (Integer, Integer)
     -- is this removeConflicts in here for safety?
     fRules' = M.foldrWithKey removeConflicts fRules fRules  :: OrdMap (Integer, Integer)
     diff = traceMe $ filterRuleSet $ relevantRules M.\\ fRules' --the difference between the two rule sets
     x = if M.null diff then Nothing else Just diff
   in
    x

  -- | only delete rules when (foo,bar) && (bar,foo)
  -- NB : wihtout unique names, this is actualyl wrong and makes our tool incomplete over ordering
  -- this is "ok" becuase odering merge does not satisfy the algorithm spec (see paper)
  merge curr new =
    let
      u = M.unionWith (\(a, b) (c, d) -> (a + c, b + d)) curr new
      x = M.foldrWithKey removeConflicts u u
    in
      x
    --L.intersect curr new

-- counting version chucks out mirrored antipairs!
removeConflicts :: (IRLine,IRLine) -> (Integer, Integer) -> OrdMap (Integer,Integer) -> OrdMap (Integer,Integer)
removeConflicts k (vy, vn) old =
  let
    flippedRule = swap k
    conflict =  M.lookup flippedRule old
  in
    case conflict of
    -- Betweeen (a, b) and (b, a), only keeping one copy since it's redundant info
      Just (y, n) -> M.insert k (vy + n, vn + y) $ M.delete flippedRule old
      Nothing -> old -- no need for redundant insert?



-- | is the rule relevant to the file
--   ie have we seen the (keyword, value) pairing before
hasRuleFor :: [IRLine] -> (IRLine,IRLine) -> (Integer, Integer)  -> Bool
hasRuleFor ts (r1,r2) (y,n) =
  let
    yd = fromIntegral y
    nd = fromIntegral n
    prob = yd / (yd + nd)
  in
    elem r1 ts && elem r2 ts

-- generate pairs of every possible elements in the list where (x,y) is a pair iff the element x appeared before y in the list
pairs :: [IRLine]  -> [((IRLine,IRLine),(Integer, Integer))]
pairs [] = []
pairs (l:ls) =
  let
    thisP = map (\x->((l,x),(1, 0))) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s.fst in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf

-- basically like pairs but the reverse relations to keep accurate count
antiPairs :: [IRLine] -> [((IRLine,IRLine),(Integer,Integer))]
antiPairs = 
  let reverseCounts (rp, (y, n)) = (rp, (n, y))
  in (map reverseCounts) . pairs . reverse

--traceMe x = traceShow x x
-- what is this for?!
traceMe x = x
