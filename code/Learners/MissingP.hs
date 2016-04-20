{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}

module Learners.MissingP where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O

import Debug.Trace

-- programmed in cutoff for testing?
cutoffProb :: Double
cutoffProb = 0.8
cutoffObsPercentile :: Double
cutoffObsPercentile = 0.1
-- and to make dealing with probs easier
prob :: Eq a => Show a => (a, Int, Int) -> Double
prob (_, y, n) = 
  let
    y' = fromIntegral y
    n' = fromIntegral n
  in y' / (y' + n')
-- and to actually filter the rules
filterRuleSet :: Eq a => Show a => Double -> Double -> [(a, Int, Int)] -> [(a, Int, Int)]
filterRuleSet probCutoff percObsCutoff rs =
  let
    rs' = filter (\x -> prob x > probCutoff) rs
    rulesObs = reverse $ L.sort $ map (\(_, y, n) -> y + n) rs'
    topObsCutoff = rulesObs !! (round $ percObsCutoff * (fromIntegral.length) rulesObs)
    rs'' = filter (\(_, y, n) -> y + n >= topObsCutoff) rs'
  in
    rs''

-- optimization so that we don't keep duplicates
simplify :: Eq a => Show a => [(a, Int, Int)] -> [(a, Int, Int)]
simplify xs = L.map combineCounts $ sortedRules xs
  where
    sameRule (r1, y1, n1) (r2, y2, n2) = r1 == r2
    sortKey (r, y, n) = show r -- super hacky solution, sorry
    mergeTwo = (\(r1, y1, n1) (r2, y2, n2) -> (r1, y1 + y2, n1 + n2))
    combineCounts (r:rs) = foldl mergeTwo r rs
    sortedRules xs = L.groupBy sameRule $ (L.sortBy . O.comparing) sortKey xs

-- instead of just a KVRule, keep track of the rule plus counts for + counts against
instance Attribute [] (MissingKVRule, Int, Int) where
  learn [] = []
  -- for each line in our file, see if any of the following lines have the same 'keyword' and if not, then that's a rule
  learn (l:ls) = simplify $ concatMap (\l' -> if (keyword l == keyword l') then [] else [(MissingKVRule l l', 1, 0)]) ls ++ learn ls

  check rs f =
   let
     fRules = learn f
     fRules' = (filterRuleSet cutoffProb cutoffObsPercentile) fRules
     diff = L.deleteFirstsBy (\(r1, y1, n1) (r2, y2, n2) -> r1 == r2) rs fRules' --the difference between the two rule sets
     x = if null diff then Nothing else Just diff
   in
    x

  -- actually, what we should be doing is treating these lists like a map MissingKVRule -> (Int, Int)
  --  and during the merge step, if one of the maps is missing this key, then we "normalize" it by adding (rule, 0, n)
  --  where n is the number of observations we have seen so far that is missing this rule
  merge curr new = simplify $ curr ++ new
   -- rules that are in curr but not new must have normalized "no" votes from new
   ++ (makeNegations (maxObs new) $ L.filter (hasCounterexample new) $ L.deleteFirstsBy sameRule curr new) 
   -- (and vice versa)
   ++ (makeNegations (maxObs curr) $ L.filter (hasCounterexample curr) $ L.deleteFirstsBy sameRule new curr)
    where
      -- this guards against putting in negative counts where it is not relevant because neither of the keys appear in the ruleset to be merged
      hasCounterexample xs (MissingKVRule l l', _, _) = L.or $ map (\(MissingKVRule k k', _, _) -> k == l || k == l' || k' == l || k' == l') xs
      makeNegations neg = map (\(r, y, n) -> (r, 0, neg))
      sameRule = (\(r1, y1, n1) (r2, y2, n2) -> r1 == r2)
      maxObs = L.maximum . (map (\(r, y, n) -> y + n))

instance Attribute [] (MissingKRule, Int, Int) where
  learn [] = []
  learn (l:ls) = simplify $ concatMap (\l' -> if (keyword l == keyword l') then [] else [(MissingKRule (keyword l) (keyword l'), 1, 0)]) ls ++ learn ls

  check rs f =
   let
     fRules = learn f
     fRules' = (filterRuleSet cutoffProb cutoffObsPercentile) fRules
     diff = L.deleteFirstsBy (\(r1, y1, n1) (r2, y2, n2) -> r1 == r2) rs fRules' --the difference between the two rule sets
     x = if null diff then Nothing else Just diff
   in
     x

  -- same as before
  merge curr new = simplify $ curr ++ new
   ++ (makeNegations (maxObs new) $ L.filter (hasCounterexample new) $ L.deleteFirstsBy sameRule curr new) 
   ++ (makeNegations (maxObs curr) $ L.filter (hasCounterexample curr) $ L.deleteFirstsBy sameRule new curr)
    where
      hasCounterexample xs (MissingKRule l l', _, _) = L.or $ map (\(MissingKRule k k', _, _) -> k == l || k == l' || k' == l || k' == l') xs
      makeNegations neg = map (\(r, y, n) -> (r, 0, neg))
      sameRule = (\(r1, y1, n1) (r2, y2, n2) -> r1 == r2)
      maxObs = L.maximum . (map (\(r, y, n) -> y + n))
