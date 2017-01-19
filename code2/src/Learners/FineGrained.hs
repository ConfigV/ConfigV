{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Learners.FineGrained where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable
import qualified Types.Rules as R
import Learners.Common


import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Char as C
import           Data.Maybe 
import           System.Directory

import Debug.Trace

-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable R.FineGrained MultiFormula where

  buildRelations f = let
    rs = mapMaybe intLike f 
    --dont want anything with these
    xs = ["dir","socket","port"]
    rs' = triples $ filter (\ir -> not $ any (\k -> T.isInfixOf k (keyword ir)) xs) rs
    eqs = map toFineGrained rs'
   in
    M.fromList $ eqs

  -- unionsWith work by Ord, so just providing a custom instance of Eq wont work, also need Ord
  -- ord is too sensitive, since traversal might miss an EQ
  -- instead just rebuild the whole map with combineFlips (only happens once so shouldnt be too bad
  merge rs = let
    rs' = M.unionsWith add rs
   in
    rs'
--    M.foldlWithKey combineFlips M.empty rs'
 
  check _ r1 r2 = if
    | gt r1 > 3 && lt r1 > 3 -> Nothing --ignore rules if they dont have a clear tendancy
    | eq r2 == 1 && (lt r1 > 3 || gt r1 > 3) && eq r1 < 3 -> Just r1
    | lt r2 == 1 && gt r1 > 3 && lt r1 < 2-> Just r1
    | gt r2 == 1 && lt r1 > 3 && gt r1 < 2-> Just r1
    | otherwise -> Nothing
    
    
  toError fname ((IntRel k1 k2),rd) = Error{
     errLocs = map (\x->(fname, x)) [k1,k2]
    ,errIdent = INTREL
    ,errMsg = "INTEGER RELATION ERROR: Expected "++(show k1)++(show rd)++(show k2)
    }

--all IRLines must have only ints as values
toFineGrained :: (IRLine,IRLine) -> (FineGrained,MultiFormula)
toFineGrained (IRLine k1 v1,IRLine k2 v2,IRLine k3 v3) = let
 asInt v = (read $ T.unpack (fst $ T.partition C.isNumber v)) * (fromMaybe 1 $ units v)
 formula v1 v2 = if
   | v1*v2 > v3  -> MultiFormula {gt=0,eq=0,lt=1}
   | v1*v2 == v3 -> MultiFormula {gt=0,eq=1,lt=0}
   | v1*v2 > v3  -> MultiFormula {gt=1,eq=0,lt=0}
 in
   ((FineGrained k1 k2 k3), formula (asInt v1) (asInt v2) (asInt v3))

combineFlips :: M.Map IntRel Formula -> IntRel -> Formula -> M.Map IntRel Formula
combineFlips old (IntRel k1 k2) v = if 
  | M.member (IntRel k1 k2) old -> M.adjust (add v) (IntRel k1 k2) old
  | M.member (IntRel k2 k1) old -> M.adjust (add (flipped v)) (IntRel k2 k1) old
  | otherwise -> M.insert (IntRel k1 k2) v old

flipped :: Formula -> Formula
flipped f = Formula {
   gt = lt f
  ,eq = eq f
  ,lt = gt f}

 

--TODO from Xinyu below here, no idea if it works
triples xs = permuation xs $ combination 2 xs

-- newly added : select n items from xs, combinations
-- this one has the original input [IRLine]
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- L.tails xs, ys <- combinations (n-1) xs']

-- newly added : generate permutaion of three elements (l1, l2, l3), (l1, l2) is generated using "combination 2 [IRLine]"
-- l3 is selected from [IRLine], which keyword is different from l1 and l2
permutation :: [IRLine] -> [[IRLine]] -> [(IRLine, IRLine, IRLine)]
permutation _ [] = []
permutation xs (l@(l1 : l2 :[]) : ls) = filter (\(l1, l2, l3) -> (keyword l1/=keyword l3 && keyword l2 /= keyword l3)) $ map (l1, l2, ) xs ++ permutation xs ls

