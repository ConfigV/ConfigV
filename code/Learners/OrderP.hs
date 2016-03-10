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


instance Attribute (M.Map (IRLine,IRLine)) (Int, Int) where
  learn f = let
     x = M.fromList $ pairs f
   in
     M.foldrWithKey removeConflicts x x

  check rs f =
   let
     relevantRules = M.filterWithKey (hasRuleFor f) (rs)
     fRules = learn f :: OrdMap (Int, Int)
     fRules' = M.foldrWithKey removeConflicts fRules fRules  :: OrdMap (Int, Int)
     diff = traceMe relevantRules M.\\ fRules' --the difference between the two rule sets
     x = if M.null diff then Nothing else Just diff
   in
    x

  -- | only delete rules when (foo,bar) && (bar,foo)
  -- NB : wihtout unique names, this is actualyl wrong and makes our tool incomplete over ordering
  -- this is "ok" becuase odering merge does not satisfy the algorithm spec (see paper)
  merge curr new =
    let
      u = M.union curr new
      x = M.foldrWithKey removeConflicts u u
    in x
    --L.intersect curr new

removeConflicts :: (IRLine,IRLine) -> (Int, Int) -> OrdMap (Int,Int) -> OrdMap (Int,Int)
removeConflicts k (vy, vn) old =
  let
    flippedRule = swap k
    conflict =  M.lookup flippedRule old
  in
    case conflict of -- can't delete because it'll screw up \\
      Just (y, n) -> M.insert k (vy + n, vn + y) $ M.insert flippedRule (y + vn, n + vy) old
      Nothing -> old -- no need for redundant insert?




-- | is the rule relevant to the file
--   ie have we seen the (keyword, value) pairing before
hasRuleFor :: [IRLine] -> (IRLine,IRLine) -> (Int, Int)  -> Bool
hasRuleFor ts (r1,r2) (y,n) =
  let
    yd = fromIntegral y
    nd = fromIntegral n
    prob = yd / (yd + nd)
  in
    elem r1 ts && elem r2 ts && prob > 0.9 -- we should tune this

-- generate pairs of every possible elements in the list where (x,y) is a pair iff the element x appeared before y in the list
pairs :: [IRLine]  -> [((IRLine,IRLine),(Int, Int))]
pairs [] = []
pairs (l:ls) =
  let
    thisP = map (\x->((l,x),(1, 0))) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s.fst in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf

-- basically like pairs but the reverse relations to keep accurate count
antiPairs :: [IRLine] -> [((IRLine,IRLine),(Int,Int))]
antiPairs = 
  let reverseCounts (rp, (y, n)) = (rp, (n, y))
  in (map reverseCounts) . pairs . reverse

--traceMe x = traceShow x x
-- what is this for?!
traceMe x = x
