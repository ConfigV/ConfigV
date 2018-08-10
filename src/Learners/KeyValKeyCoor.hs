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
instance Learnable R.KeyValKeyCoor NontrivRule where

  buildRelations f = let
    --order pairs consistently
    toKVKCoors (ir1,ir2) = 
      if keyword ir1 > keyword ir2
      then KeyValKeyCoor (keyword ir1, value ir1, keyword ir2) 
      else KeyValKeyCoor (keyword ir2, value ir2, keyword ir1) 
    irPairs = pairs' f
    -- this is specific to CFN, this should be moved to preprocesing
    isRelevant (KeyValKeyCoor (k1,v,k2)) = 
        not (
             T.isSuffixOf ".Type" k1 
          || T.isSuffixOf "Ref" k1 
          || T.isSuffixOf "Description" k1 
          || T.isInfixOf "Fn::" k1)
    -- tot = # times x + # times y
    totalTimes = M.fromList $ embedAsNontriv $ filter isRelevant $ map toKVKCoors irPairs 
   in
    totalTimes

-- For NontrivRules, when we merge, we need to keep an eye out for evidence on nontriviality in other rules we have constructed
-- this is a bit tricky
  merge rs = let 
      antiRuleCounts = mergeAsAntiRules rs
      nonTrivs = addNontrivEvidence antiRuleCounts
    in
      nonTrivs

  --   should we report the relation r2 found in the target file
  --   as in conflict with the learned rule r1
  check _ existingLearnedRule incomingRuleToCheck = let
     rd1' = antiRuleData existingLearnedRule
     rd2' = antiRuleData incomingRuleToCheck
     agrees r1 r2 = 
       if tru r2 ==1
       then tru r1 > fls r1
       else True
   in
     if (not $agrees rd1' rd2')
     then Just existingLearnedRule
     else Nothing

  toError ir fname ((KeyValKeyCoor (k1,v,k2)),rd) = Error{
     errLocs= [(fname,k1),(fname,k2)]
    ,errIdent = KEYVALKEY
    ,errMsg = "(Key,Val) => Val ERROR: Given "++(show k1)++" WITH "++(show v) ++ ", expected to see a keyword "++(show k2)++" with CONF. = " ++ (show rd)
    ,errSupport = (tru $ antiRuleData rd) + (fls $ antiRuleData rd)}

-- | does a simple embedding into a Nontriv rule, treating it as a wrapper for Antirule 
--   we do not put any nontriv evidence in during the build stage, we need to do that in the merge stage
embedAsNontriv :: [a] -> [(a, NontrivRule)]
embedAsNontriv = map (\r -> (r, (NontrivRule {nontrivialityEvidence = 0, antiRuleData = AntiRule {tru=1, fls=0, tot=1}})))

-- | we need to count how many times we see k1, v1', k2 where v1' != v1
--   if this count is low (ideally 0) this is evidence that the coorlations are due to the keyword,value pair
--   and not just that the two keyword appear together, and v1 is a common value for k1
addNontrivEvidence :: RuleDataMap KeyValKeyCoor NontrivRule -> RuleDataMap KeyValKeyCoor NontrivRule
addNontrivEvidence rs = let
  updateNontriv r = undefined
 in
  M.map updateNontriv rs

mergeAsAntiRules :: [RuleDataMap KeyValKeyCoor NontrivRule] -> RuleDataMap KeyValKeyCoor NontrivRule
mergeAsAntiRules rs = let
    rsAdded = M.unionsWith add rs
    -- false = total - (true *2) b/c total counted both ks (why did I count both keys in the first place?)
    rsWithFalse = M.map (\r -> r{antiRuleData = (antiRuleData r){fls=(tot $ antiRuleData r)-((tru $ antiRuleData r)*2)}}) rsAdded
    validRule r = (tru $ antiRuleData r)>=minTrue && (fls$ antiRuleData r)<=maxFalse
    combinedAntiRules = M.filter validRule rsWithFalse
  in
    combinedAntiRules

pairs' :: [IRLine]  -> [(IRLine,IRLine)]
pairs' [] = []
pairs' (l:ls) =
  let
    thisP = map (\x->(l,x)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf

embedWith :: M.Map Keyword Int -> M.Map KeyValKeyCoor NontrivRule -> M.Map KeyValKeyCoor NontrivRule 
embedWith counts rules =
  M.mapWithKey (addCount counts) rules

addCount :: M.Map Keyword Int -> KeyValKeyCoor -> NontrivRule -> NontrivRule
addCount counts (KeyValKeyCoor (k1,v,k2)) rd = let
  kcount k = M.findWithDefault 0 k counts
 in
  -- TODO is this correct? I thought it should only increment if both keys are present
  -- this increments by 1 for each key present
  rd { antiRuleData = (antiRuleData rd) { tot=(kcount k1 + kcount k2)}}
  
