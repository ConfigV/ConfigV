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

instance Attribute (M.Map (IRLine,IRLine)) (Integer, Integer) where
  learn f = let
     x = M.fromList $ pairs f
   in
     M.foldrWithKey removeConflicts x x

  check rs f =
   let
     relevantRules = M.filterWithKey (hasRuleFor f) (rs)
     fRules = learn f :: OrdMap (Integer, Integer)
     -- ok, we really need to figure out why we have this second removeConflicts here...
     fRules' = M.foldrWithKey removeConflicts fRules fRules  :: OrdMap (Integer, Integer)
     diff = traceMe $ M.filterWithKey (\k v -> filterProbs cutoffProb v) $ relevantRules M.\\ fRules' --the difference between the two rule sets
     x = if M.null diff then Nothing else Just diff
   in
    x

  -- | only delete rules when (foo,bar) && (bar,foo)
  -- NB : wihtout unique names, this is actualyl wrong and makes our tool incomplete over ordering
  -- this is "ok" becuase odering merge does not satisfy the algorithm spec (see paper)
  merge curr new =
    let
      u = M.union curr new -- also I'm pretty sure this doesn't add counts for ones that do stack up
      x = M.foldrWithKey removeConflicts u u
    in
      mergeWhileRemovingConflicts curr new M.empty
    --L.intersect curr new

-- while this works for the non-counting version, running this more than once is iffy on the counting version
--  (as well as across two merged sets) because every time it runs, we exponentially increase (double-ish) the
--  counts on both the rule and its negation, and then do it again on the negation and its rule
-- Ideally, let's restrict this to run exactly once during the learn step!
removeConflicts :: (IRLine,IRLine) -> (Integer, Integer) -> OrdMap (Integer,Integer) -> OrdMap (Integer,Integer)
removeConflicts k (vy, vn) old =
  let
    flippedRule = swap k
    conflict =  M.lookup flippedRule old
  in
    case conflict of -- can't delete because it'll screw up \\
    -- we can only do this when one is from one set and the other is from the other set
    --  (otherwise, if we keep running this on itself, we exponentially blow up counts)
      Just (y, n) -> M.insert k (vy + n, vn + y) $ M.insert flippedRule (y + vn, n + vy) old
      Nothing -> old -- no need for redundant insert?

-- better version that looks across the merging maps and does not exponentially blow up
--mergeWhileRemovingConflicts :: OrdMap (Integer,Integer) -> OrdMap (Integer,Integer) -> OrdMap (Integer, Integer) -> OrdMap (Integer, Integer)
--mergeWhileRemovingConflicts (c:cs) ns seed =
  -- is this even the right approach? Or should we not keep track of duplicates (a,b) and (b,a) and elect to keep just one copy that is perfectly invertible?



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

filterProbs p (y, n) =
  let
    y' = fromIntegral y
    n' = fromIntegral n
    prob = y' / (y' + n')
  in
    prob >= p