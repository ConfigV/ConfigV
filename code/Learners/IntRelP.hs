{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE MultiWayIf #-}

module Learners.IntRelP where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.Tuple

import Debug.Trace

-- I haven't found a nice package for SMTLIB format in haskell yet, its out there tho, im sure

-- for tuning which probabilistic rules to throw out
cutoffProb = 0.7
cutoffPerc = 0.2

instance Attribute (M.Map (Keyword,Keyword)) FormulaC where
  -- | this has the problem that order is important
  --   (foo,bar,==) is different than (bar,foo,==)
  learn ts =
    let
      ts' = pairs $ filter couldBeInt ts
      rules = foldr (\p rs -> findeqRules p ++ rs) [] ts'
      rs' = M.fromList rules
      rs = M.foldrWithKey removeConflicts rs' rs' -- just to be safe about no duplicates
    in
      rs

  -- we should rewrite this so that we don't have to expand out antipairs (makeFlips), but check them in compRules
  check rs f =
    let
      emptyFC (FormulaC l g e) = g == 0 && l == 0 && e == 0
      relevantRules' = M.filterWithKey (\k v-> (not $ emptyFC v) && hasRuleFor f k) (rs)
      fRules' = learn f :: IntRelMapC
      relevantRules = (filterRuleSet cutoffProb cutoffPerc) relevantRules'
      fRules = forcePairOrientation relevantRules fRules'
      diff = M.differenceWith compRules relevantRules fRules --diff is the rules we should have met, but didnt
    in
      if M.null diff then Nothing else Just diff

  -- merge curr new = foldl combineCounts [] $ L.union curr (traceShow (length curr) new)
  merge curr new =
    let
      u = M.unionWith combineCounts curr new
      cu = M.foldrWithKey removeConflicts u u -- combine/chuck out antipairs
    in
      cu

-- as in the other rules, institute a filter over a top percentile of observations and probability cutoff
filterRuleSet :: Double -> Double -> IntRelMapC -> IntRelMapC
filterRuleSet probCutoff percObsCutoff m =
  let
    totalObs (FormulaC l g e) = l + g - e -- remember that <= and >= intersect on the event ==
    obs = (reverse . L.sort) $ map totalObs $ M.elems m
    obsCutoff = obs !! (round $ percObsCutoff * fromIntegral (length obs))
    prob (FormulaC l g e) =
      let
        total = fromIntegral $ totalObs (FormulaC l g e)
        l' = fromIntegral l
        g' = fromIntegral g
        e' = fromIntegral e
      in
        maximum [l' / total, g' / total, e' / total]
  in
    M.filter (\x -> totalObs x >= obsCutoff) $ M.filter (\x -> prob x >= probCutoff) m

-- counting version chucks out mirrored antipairs!
removeConflicts :: (Keyword, Keyword) -> FormulaC -> IntRelMapC -> M.Map (Keyword, Keyword) FormulaC
removeConflicts k (FormulaC vl vg ve) old =
  let
    flippedRule = swap k
    conflict =  M.lookup flippedRule old
  in
    case conflict of
    -- Betweeen (a, b) and (b, a), only keeping one copy since it's redundant info
      Just (FormulaC l g e) -> M.insert k (FormulaC (vl + g) (vg + l) (ve + e)) $ M.delete flippedRule old
      Nothing -> old -- no need for redundant insert?

-- we assume that each of the IntRelMapC have only one copy of the pair, because we should have removed conflicts at each step
--  then, we want any pairs from the map "new" that have its antipair in the map "standard" to reorient
forcePairOrientation :: IntRelMapC -> IntRelMapC -> IntRelMapC
forcePairOrientation standard new =
  let
    reorient k (FormulaC l g e) old =
      let
        flippedRule = swap k
        standardOrientation =  M.lookup flippedRule standard
      in
        case standardOrientation of
          Just _ -> M.insert flippedRule (FormulaC g l e) $ M.delete k old
          Nothing -> old
  in
    M.foldrWithKey reorient new new

-- THIS IS SUPER IMPORTANT FOR MAKING DIFFERENCES OF RULES
--  (find the maximum operator in the tuple)
mostLikely :: FormulaC -> Maybe String -- better comparator than (Ord a => a -> a -> Bool)
mostLikely (FormulaC l g e) =
  if | l > g && l > e -> Just "<="
     | g > l && g > e -> Just ">="
     | e > l && e > g -> Just "=="
     | otherwise -> Nothing

compRules :: FormulaC -> FormulaC -> Maybe FormulaC
compRules f1 f2 =
  let
    f1' = mostLikely f1 -- set these cutoffs here or somewhere?
    f2' = mostLikely f2
  in
    case (f1', f2') of
      (Nothing, _) -> Nothing -- these are too inconclusive to mark as errors
      (Just "<=", Just "==") -> Nothing -- these are "equality" conditions
      (Just "==", Just "<=") -> Nothing
      (Just ">=", Just "==") -> Nothing
      (Just "==", Just ">=") -> Nothing
      (Just "==", Just "==") -> Nothing
      (Just ">=", Just ">=") -> Nothing
      (Just "<=", Just "<=") -> Nothing
      _ -> Just f1 -- will match if the fRules is too inconclusive, which should be an error

foo s m = trace (s++(show (M.lookup (("port[client]","port[mysqld]")) m))) m
--foo m = traceShow (M.lookup (swap ("max_allowed_packet[wampmysqld]","key_buffer[wampmysqld]")) m) m
--if ((snd r1) == "max_allowed_packet[wampmysqld]") && ((fst r1) == "key_buffer[wampmysqld]") then (trace ((show r1)++(show f)) f) else f
--traceMe x = (

hasRuleFor :: [IRLine] -> (Keyword,Keyword) ->  Bool
hasRuleFor ls (l1,l2) =
  elem l1 (map keyword ls) && elem l2 (map keyword ls)

combineCounts :: FormulaC -> FormulaC -> FormulaC
combineCounts f1 f2 =
  FormulaC {
    lt = lt f1 + lt f2,
    gt = gt f1 + gt f2,
    eq = eq f1 + eq f2
  }
-- | true for rules with mathcing keys, but diff formulas
{-sameKeyRel :: ((Keyword,Keyword), Formula) -> (Keyword,Keyword) -> Formula -> Bool
sameKeyRel (r1, f) r2 f' =
   id (
    (fst r1) == (fst r2) && (snd r1) == (snd r2) && ((foo r1 f) /= (foo r2 f'))
   )
-}

findeqRules :: (IRLine,IRLine) -> [((Keyword,Keyword),FormulaC)]
findeqRules (l1,l2) =
  let
    -- string to int convertsion taking into account some ints will have 'M' attached at the end
    getI l =
      if (T.last$value l) == 'M' then (read. T.unpack. T.init. value) l else (read. T.unpack. value) l
    i1 = getI l1 :: Int
    i2 = getI l2 :: Int
    -- determine whether a number marks data size or just an int
    bothSame = (isSize (value l1) && isSize (value l2)) || ((isInt (value l1)) && (isInt (value l2)))
    strToOp "==" = (==) -- hack to make pattern-matching on operators work
    strToOp "<=" = (<=)
    strToOp ">=" = (>=)
    makeR f =
      if bothSame && (strToOp f) i1 i2
      then
        case f of
          -- we should think of lt and gt as really "less than or equals to" and "greater than or equals to"
          -- (I think this means we don't want the count-tuples to be completely independent but measure the
          --  events we want, even if they overlap. We will need to provide a way to extract a total count of
          --  observations, though.)
          "==" -> [((keyword l1, keyword l2), FormulaC { lt = 1, gt = 1, eq = 1})]
          "<=" -> [((keyword l1, keyword l2), FormulaC { lt = 1, gt = 0, eq = 0})]
          ">=" -> [((keyword l1, keyword l2), FormulaC { lt = 0, gt = 1, eq = 0})]
          _    -> []
      else []
    all = makeR "==" ++ makeR "<=" ++ makeR ">="
  in
    if | length all == 1 -> all
       | not bothSame -> []
       | length all /= 1 && bothSame -> makeR "=="

-- only for comparators! careful!
--instance Eq (Int -> Int -> Bool) where
--  (==) f1 f2 =
--    (f1 1 1) == (f2 1 1) &&
--    (f1 0 1) == (f2 0 1) &&
--    (f1 1 0) == (f2 1 0)

couldBeInt :: IRLine -> Bool
couldBeInt IRLine{..} =
  isInt value || isSize value
--  (T.all isDigit value && (T.length value >0)) ||
--  ((T.length value >1) && T.all isDigit (T.init value)  && T.last value == 'M')

isInt v =
  (T.all isDigit v && (T.length v >0))
isSize v =
  ((T.length  v >1) && T.all isDigit (T.init v)  && (T.last v) == 'M')

pairs :: [IRLine]  -> [(IRLine,IRLine)]
pairs [] = []
pairs (l:ls) =
  filter (\(l1,l2) -> ((isSize $value l1)&&(isSize $value l2)) || ((isInt $value l1)&&(isInt $ value l2)) && (keyword l1/=keyword l2)) $ map (l,) ls ++ pairs ls


--L.nubBy modOrder $ filter (\(l1,l2) -> (keyword l1/=keyword l2)) $ map (l,) ls ++ pairs ls
-- we dont need foo,bar,<= and bar,foo,>=
modOrder (r11,r12) (r21,r22) =
  (keyword r11 == keyword r22) &&
  (keyword r12 == keyword r21)
