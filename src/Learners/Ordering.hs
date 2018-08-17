{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TupleSections #-} 

module Learners.Ordering where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable

import Settings 

import Prelude hiding (Ordering)
import qualified Data.Map as M

import Learners.Common
-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file

minTrue = Settings.orderSupport
maxFalse = Settings.orderConfidence

instance Learnable Ordering AntiRule where

  buildRelations f = let
    toOrdering (ir1,ir2) = Ordering (keyword ir1, keyword ir2) 
    irPairs = filter (sameConfigModule) $ pairs f
   in
     M.fromList $ embedAsTrueAntiRule $ map toOrdering $ irPairs 
    --M.foldrWithKey removeConflicts x x

  --since AntiRules are built with different keyword pais
  --we need to generate the counts of false evidence
  --ths is nice since we will maintain both (k1,k2) and (k2,k1) rules
  merge rs' = let 
      rs = M.unionsWith add rs'
      findOp (Ordering (k1,k2)) = M.findWithDefault (AntiRule 0 0 0) (Ordering (k2,k1)) rs
      adjWithOp (AntiRule tru fls t) (AntiRule truOp flsOp tOp) = 
        AntiRule tru truOp (t+tOp) 
      updateWithOp k v = combine v $ findOp k
      validRule r = (tru r)>=minTrue && (fls r)<=maxFalse
    in
      M.filter validRule $ M.mapWithKey updateWithOp rs

  -- | does the r2 we found in the target file
  --   agree with the learned r1
  check _ rd1 rd2 = let
     agrees r1 r2 = 
       if tru r2 ==1
       then tru r1 > fls r1
       else fls r1 > tru r1
   in
     if (not $agrees rd1 rd2)
     then Just rd1
     else Nothing

  toError ir fname ((Ordering (k1,k2)),rd) = Error{
     errLocs = map (fname,) [k1,k2]
    ,errIdent = ORDERING
    ,errMsg = "ORDERING ERROR: Expected "++(show k1)++" BEFORE "++(show k2)++" w/ confidence "++(show rd)
    ,errSupport = tru rd + fls rd}

