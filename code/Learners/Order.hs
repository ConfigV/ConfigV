{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses#-}

module Learners.Order where

import Types.Types
import Types.IR

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple

import Debug.Trace


instance Attribute (M.Map (IRLine,IRLine)) Bool where
  learn f = let
     x = M.fromList $ pairs f
   in
     M.foldrWithKey removeConflicts x x

  check rs f =
   let
     relevantRules = M.filterWithKey (hasRuleFor f) (rs)
     fRules = learn f :: OrdMap Bool
     fRules' = M.foldrWithKey removeConflicts fRules fRules  :: OrdMap Bool
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

removeConflicts :: (IRLine,IRLine) -> Bool -> OrdMap Bool -> OrdMap Bool
removeConflicts k v old =
  let
    flippedRule = swap k
    conflict =  M.lookup flippedRule old
  in
    case conflict of
      Just True -> M.insert k False $ M.insert flippedRule False old
      Just False -> old
      Nothing -> M.insert k True old


-- | is the rule relevant to the file
--   ie have we seen the (keyword, value) pairing before
hasRuleFor :: [IRLine] -> (IRLine,IRLine) -> Bool  -> Bool
hasRuleFor ts (r1,r2) rel =
  elem r1 ts && elem r2 ts && rel

pairs :: [IRLine]  -> [((IRLine,IRLine),Bool)]
pairs [] = []
pairs (l:ls) =
  let
    thisP = map (\x->((l,x),True)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s.fst in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf

--traceMe x = traceShow x x
traceMe x = x
