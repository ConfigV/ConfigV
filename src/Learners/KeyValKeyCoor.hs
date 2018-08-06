{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Learners.KeyValKeyCoor where

import Types.IR
import Types.Common
import Types.Errors
import Types.Rules 
import Types.Countable

import Settings

import qualified Types.Rules as R

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Bits as B
import           System.Directory

import Learners.Common

import Debug.Trace


minTrue = Settings.keyValKeyCoorSupport
maxFalse = Settings.keyValKeyCoorConfidence

buildRelations' keyCounts f = let
  rs = buildRelations f
 in
  embedWith keyCounts rs

-- | TODO these are undirected coorelation
instance Learnable R.KeyValKeyCoor AntiRule where

  buildRelations f = let
    --order pairs consistently
    toKVKCoors (ir1,ir2) = 
      if keyword ir1 > keyword ir2
      then KeyValKeyCoor (keyword ir1, value ir1, keyword ir2) 
      else KeyValKeyCoor (keyword ir2, value ir2, keyword ir1) 
    irPairs = pairs' f
    isRelevant (KeyValKeyCoor (k1,v,k2)) = 
        not (
             T.isSuffixOf ".Type" k1 
          || T.isSuffixOf "Ref" k1 
          || T.isSuffixOf "Description" k1 
          || T.isInfixOf "Fn::" k1)
    -- tot = # times x + # times y
    totalTimes = M.fromList $ embedOnce $ filter isRelevant $ map toKVKCoors irPairs 
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

  toError ir fname ((KeyValKeyCoor (k1,v,k2)),rd) = Error{
     errLocs= [(fname,k1),(fname,k2)]
    ,errIdent = MISSING
    ,errMsg = "(Key,Val) => Val ERROR: Given "++(show k1)++" WITH "++(show v) ++ ", expected to see a keyowrd with value CONF. = " ++ (show rd)
    ,errSupport = tru rd + fls rd}

pairs' :: [IRLine]  -> [(IRLine,IRLine)]
pairs' [] = []
pairs' (l:ls) =
  let
    thisP = map (\x->(l,x)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf

embedWith :: M.Map Keyword Int -> M.Map KeyValKeyCoor AntiRule -> M.Map KeyValKeyCoor AntiRule
embedWith counts rules =
  M.mapWithKey (addCount counts) rules

addCount :: M.Map Keyword Int -> KeyValKeyCoor -> AntiRule -> AntiRule
addCount counts (KeyValKeyCoor (k1,v,k2)) rd = let
  kcount k = M.findWithDefault 0 k counts
 in
  rd{tot=(kcount k1 + kcount k2)}
  
