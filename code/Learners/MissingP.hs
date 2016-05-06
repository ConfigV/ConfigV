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
cutoffProb = 0.5
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
-- have settings in the settings file to set cutoff percentage/percentile/hard number of observations
filterRuleSet :: Eq a => Show a => Double -> Double -> [(a, Int, Int)] -> [(a, Int, Int)]
filterRuleSet probCutoff percObsCutoff rs =
  let
    rs' = filter (\x -> prob x > probCutoff) rs
    obs = reverse $ L.sort $ map (\(_, y, n) -> y + n) rs'
    topObsCutoff = obs !! min (round $ percObsCutoff * (fromIntegral.length) obs) (length obs -1)
    rs'' = filter (\(_, y, n) -> y + n >= topObsCutoff) rs'
  in
    rs''

-- when we need to go from MissingKRule to [(MissingKRule, Int, Int)]
--  (hack type matching)
toTuple :: MissingKRule -> (MissingKRule, Int, Int)
toTuple r = (r, 0, 0)
-- optimization so that we don't keep duplicates
simplify :: [(MissingKRule, Int, Int)] -> [(MissingKRule, Int, Int)]
simplify rs = foldr removeInverse [] noDuplicates
  where
    sortKey (r, y, n) = show r -- super hacky solution, sorry
    mergeTwo = (\(r1, y1, n1) (r2, y2, n2) -> (r1, y1 + y2, n1 + n2))
    combineCounts (r:rs) = foldl mergeTwo r rs
    sortedRules xs = L.groupBy sameRule $ (L.sortBy . O.comparing) sortKey xs
    -- fold to combine the counts of (a,b) and (b,a) since they're measuring the same thing (togetherness)
    removeInverse (r, y, n) rest =
      let
        invPair = L.find (\(r', _, _) -> sameRule (toTuple r) (toTuple r')) rest
      in
        case invPair of
          Just (r', y', n') -> (r, y + y', n + n'):(L.delete (r', y', n') rest)
          Nothing -> (r, y, n):rest
    -- first round to combine (a,b) and (a,b) counts
    noDuplicates = L.map combineCounts $ sortedRules rs

-- do two given rules share at least one keyword?
relevantTo :: (MissingKRule, Int, Int) -> (MissingKRule, Int, Int) -> Bool
relevantTo (r, _, _) (r', _, _) = k1 r == k1 r' || k1 r == k2 r' || k2 r == k1 r' || k2 r == k2 r'

-- does the given rule set have at least one keyword in common with the given rule
hasRuleFor :: [(MissingKRule, Int, Int)] -> (MissingKRule, Int, Int) -> Bool
hasRuleFor rs r = L.or $ map (relevantTo r) rs

-- we only keep one copy of the tuple, so if (a,b) is in the rule set, that means (b,a) is not
sameRule :: (MissingKRule, Int, Int) -> (MissingKRule, Int, Int) -> Bool
sameRule = (\(r1, y1, n1) (r2, y2, n2) -> r1 == r2 || r1 == rflip r2)
  where
    rflip (MissingKRule k1 k2) = MissingKRule k2 k1

instance Attribute [] (MissingKRule, Int, Int) where
  learn [] = []
  learn (l:ls) = simplify $ concatMap (\l' -> if (keyword l == keyword l') then [] else [(MissingKRule (keyword l) (keyword l'), 1, 0)]) ls ++ learn ls

  check rs f =
   let
     fRules = learn f
     rs' = (filterRuleSet cutoffProb cutoffObsPercentile) rs
     rs'' = L.filter (hasRuleFor fRules) rs'
     diff = L.deleteFirstsBy (\(r1, y1, n1) (r2, y2, n2) -> r1 == r2) rs'' fRules --the difference between the two rule sets
     x = if null diff then Nothing else Just diff
   in
     x

  -- same as before
  merge curr new = simplify $ curr ++ new ++ (counterexamplesFromOtherSet curr new) ++ (counterexamplesFromOtherSet new curr)
    where
      -- given a rule set, make a copy of the rule with a normalized number of negative votes
      --  (for combinations across sets during merge)
      makeNegations :: [(MissingKRule, Int, Int)] -> [(MissingKRule, Int, Int)] -> [(MissingKRule, Int, Int)]
      makeNegations rs = map (\(r, y, n) -> (r, 0, maxObs r rs))
      -- max obs does not depend on just the max obs of the entire list, but of the ones where at least one of the keywords was seen
      maxObs :: MissingKRule -> [(MissingKRule, Int, Int)] -> Int
      maxObs r' = L.maximum . (map (\(r, y, n) -> y + n)) . (filter (relevantTo (toTuple r')))
      -- rules that are in rs1 but not rs2 must have normalized "no" votes from rs2
      counterexamplesFromOtherSet :: [(MissingKRule, Int, Int)] -> [(MissingKRule, Int, Int)] -> [(MissingKRule, Int, Int)]
      counterexamplesFromOtherSet rs1 rs2 = makeNegations rs2 $ L.filter (hasRuleFor rs2) $ L.deleteFirstsBy sameRule rs1 rs2
