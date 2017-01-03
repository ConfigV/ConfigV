{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, OverloadedStrings #-}

module Learners.Ordering where

import Types.IR
import Types.Errors
import Types.Rules 

import Prelude hiding (Ordering)
import qualified Data.Map as M
import           System.Directory
import qualified Data.Text as T
import qualified Data.Bits as B

import Learners.Common
-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable Ordering where

  buildRelations :: IRConfigFile -> RuleDataMap Ordering
  buildRelations f = let
    toOrdering (ir1,ir2) = Ordering (keyword ir1, keyword ir2) 
    --according to Ennan socket and port do not interact with anything except themselves
    sameConfigModule (ir1,ir2) = not (
      (substr "socket" (keyword ir1) ||
       substr "port"   (keyword ir1)) `B.xor`
      (substr "socket" (keyword ir2) ||
       substr "port"   (keyword ir2)))
    substr = T.isInfixOf
    irPairs = filter sameConfigModule $ pairs f
   in
     M.fromList $ embedOnce $ map toOrdering irPairs 
    --M.foldrWithKey removeConflicts x x

  resolve :: RuleDataMap Ordering -> RuleDataMap Ordering
  resolve rs = let
    rs' = genFalseEvidence rs
    condition r@(RuleData tru fls t e) = 
      --tru>0 && fls==0 -- ConfigC 
      tru>=3 && fls<=1  -- ConfigV
   in
    M.map (\r@(RuleData tru fls t e) -> if condition r then RuleData tru fls t True else r) rs'

  -- | NB it seems this was over IRLines in ConfigC
  --   looked like an error, but double check this spot if strange results
  isRelevant :: IRConfigFile -> Ordering -> Bool
  isRelevant cf (Ordering (k1,k2)) = let
     ks = map keyword cf
   in
     elem k1 ks && elem k2 ks

  toError :: FilePath -> (Ordering,RuleData) -> Error
  toError fname ((Ordering (k1,k2)),rd) = Error{
     errLoc1 = (fname,k1)
    ,errLoc2 = (fname,k2)
    ,errIdent = ORDERING
    ,errMsg = "ORDERING ERROR: Expected "++(show k1)++" BEFORE "++(show k2)++" \n   w/ confidence "++(show rd)}

genFalseEvidence :: RuleDataMap Ordering -> RuleDataMap Ordering
genFalseEvidence rs = let
   findOp :: Ordering -> RuleData
   findOp (Ordering (k1,k2)) = M.findWithDefault (RuleData 0 0 0 True) (Ordering (k2,k1)) rs
   adjWithOp r@(RuleData tru fls t e) rOpData@(RuleData truOp flsOp tOp eOp) = 
     RuleData tru truOp (t+tOp) (e&&eOp)
   updateWithOp k v = adjWithOp v $ findOp k
  in
   M.mapWithKey updateWithOp rs

pairs :: [IRLine]  -> [(IRLine,IRLine)]
pairs [] = []
pairs (l:ls) =
  let
    thisP = map (\x->(l,x)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf
