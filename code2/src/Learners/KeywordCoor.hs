{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 

module Learners.KeywordCoor where

import Types.IR
import Types.Common
import Types.Errors
import Types.Rules 
import Types.Countable

import qualified Types.Rules as R

import qualified Data.Map as M
import qualified Data.Bits as B
import           System.Directory

import Learners.Common

import Debug.Trace


minTrue = 7
maxFalse = 1

instance Learnable R.KeywordCoor AntiRule where

  buildRelations f = let
    keyCounts :: M.Map Keyword Int 
    keyCounts = foldl (\rs ir-> M.insertWith (+) (keyword ir) 1 rs) M.empty f
    --order pairs consistently
    toKC (ir1,ir2) = 
      if keyword ir1 > keyword ir2
      then KeywordCoor (keyword ir1, keyword ir2) 
      else KeywordCoor (keyword ir2, keyword ir1) 
    irPairs = pairs' f
    -- tot = # times x + # times y
    totalTimes = embedWith keyCounts $ M.fromList $ embedOnce $ map toKC irPairs 
   in
    totalTimes

  merge rs = let 
      rsAdded = M.unionsWith add rs
      -- false = total - (true *2) b/c total counted both ks
      rsWithFalse = M.map (\r -> r{fls=(tot r)-((tru r)*2)}) rsAdded
      validRule r = (tru r)>=minTrue && (fls r)<=maxFalse
    in
      M.filter validRule rsWithFalse
      --rsUpdated


  --   should we report the relation r2 found in the target file
  --   as in conflict with the learned rule r1
  check _ rd1 rd2 = let
     agrees r1 r2 = 
       if tru r2 ==1
       then tru r1 > fls r1
       else True--fls r1 > tru r1
   in
     if (not $agrees rd1 rd2)
     then Just rd1
     else Nothing

  toError fname ((KeywordCoor (k1,k2)),rd) = Error{
     errLocs= [(fname,k1),(fname,k2)]
    ,errIdent = MISSING
    ,errMsg = "KEYWORD ERROR: Expected "++(show k1)++" WITH "++(show k2) ++ " CONF. = " ++ (show rd)}

pairs' :: [IRLine]  -> [(IRLine,IRLine)]
pairs' [] = []
pairs' (l:ls) =
  let
    thisP = map (\x->(l,x)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf

embedWith :: M.Map Keyword Int -> M.Map KeywordCoor AntiRule -> M.Map KeywordCoor AntiRule
embedWith counts rules =
  M.mapWithKey (addCount counts) rules

addCount :: M.Map Keyword Int -> KeywordCoor -> AntiRule -> AntiRule
addCount counts (KeywordCoor (k1,k2)) rd = let
  kcount k = M.findWithDefault 0 k counts
  addTotal :: Int -> AntiRule -> AntiRule
  addTotal i r = r{tot=i}
 in
  addTotal (kcount k1 + kcount k2) rd
  
