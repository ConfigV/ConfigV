{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Learners.KeyValKeyCoor where

import Types.IR
import Types.Common
import Types.Errors
import Types.Rules 
import qualified Types.Rules as R
import Types.Countable

import Learners.Common

import Settings

import qualified Data.Map as M
import qualified Data.Text as T

import Data.Interned


minTrue = Settings.keyValKeyCoorSupport
maxFalse = Settings.keyValKeyCoorConfidence

-- | TODO these are undirected coorelation
instance Learnable R.KeyValKeyCoor NontrivRule where

  buildRelations f = let
    --since order matters, we take both pairs
    toKVKCoors (ir1,ir2) = 
      [KeyValKeyCoor {k1= keyword ir1, v1=value ir1, k2=keyword ir2}
      ,KeyValKeyCoor {k1= keyword ir2, v1=value ir2, k2=keyword ir1}]
    irPairs = pairs' f
    -- this is specific to CFN, this should be moved to preprocesing
    isRelevant (KeyValKeyCoor {..}) = 
        let 
          k1' = unintern k1
          k2' = unintern k2
          v'  = unintern v1
        in
          not (
               T.isSuffixOf ".Type" k1'
            || T.isSuffixOf ".Version" k1'
            || T.isSuffixOf "Description" k1' 
            || T.isSuffixOf ".Type" k2'
            || T.isSuffixOf ".Version" k2'
            || T.isSuffixOf "Description" k2'
            -- ignore refs and parameters for now
            || T.isInfixOf "Ref" v'
            || T.isInfixOf "Parameters" k1' 
            || T.isInfixOf "Parameters" k2')
    totalTimes = M.fromList $ embedAsNontriv $ filter isRelevant $ concatMap toKVKCoors irPairs 
   in
    totalTimes

-- For NontrivRules, when we merge, we need to keep an eye out for evidence on nontriviality in other rules we have constructed
-- this is a bit tricky
  merge rs = let 
      antiRuleCounts = mergeAsAntiRules rs
      --Need to calculate nontriviality based on the full set of rules we observed
      nonTrivs = addTrivEvidence (M.unionsWith add rs) antiRuleCounts
    in
      M.filter (\r-> trivialityEvidence r <= Settings.trivEvidenceThreshold) 
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

  toError ir fname ((KeyValKeyCoor {..}), rd) = Error{
     errLocs= [(fname,k1),(fname,k2)]
    ,errIdent = KEYVALKEY
    ,errMsg = "(Key,Val) => Val ERROR: Given "++(show k1)++" WITH "++(show v1) ++ ", expected to see a keyword "++(show k2)++" with CONF. = " ++ (show rd)
    ,errSupport = (tru $ antiRuleData rd) + (fls $ antiRuleData rd)}

-- | does a simple embedding into a Nontriv rule, treating it as a wrapper for Antirule 
--   we do not put any nontriv evidence in during the build stage, we need to do that in the merge stage
embedAsNontriv :: [a] -> [(a, NontrivRule)]
embedAsNontriv = map (\r -> (r, (NontrivRule {trivialityEvidence = 0, antiRuleData = AntiRule {tru=1, fls=0, tot=1}})))

-- | we need to count how many times we see k1, v1', k2 where v1' != v1
--   if this count is high (or really anything greater than 1) this is evidence that the key,val is not important to k2
--   intuition: a rule is trivial if we if we used a differnt v1 for k1, and still saw  k2
addTrivEvidence :: RuleDataMap KeyValKeyCoor NontrivRule -> RuleDataMap KeyValKeyCoor NontrivRule -> RuleDataMap KeyValKeyCoor NontrivRule
addTrivEvidence fullRs rs = let
  updateTriv k r = let
    isTrivEvidence otherKey _ =
      (k1 k == k1 otherKey) &&
      (k2 k == k2 otherKey) &&
      (v1 k /= v1 otherKey)
    count = M.size $ M.filterWithKey isTrivEvidence fullRs
   in
    r {trivialityEvidence = count} 
 in
  M.mapWithKey updateTriv rs

mergeAsAntiRules :: [RuleDataMap KeyValKeyCoor NontrivRule] -> RuleDataMap KeyValKeyCoor NontrivRule
mergeAsAntiRules rMaps = let
    -- for a single rule, the number of time the rule was false is equal to how many files had a rule with k1,v1, but not a rule with k1,v1,k2
    findOpp k r = let
      ruleOverlap otherKey _ =
        (k1 k == k1 otherKey) &&
        (v1 k == v1 otherKey)
      count = length $ filter (\rMap -> (not $ M.member k rMap) && ((M.size $ M.filterWithKey ruleOverlap rMap) > 0)) rMaps
     in
      r { antiRuleData = (antiRuleData r){fls=count}}
    rsWithFalse = map (M.mapWithKey $ findOpp ) rMaps
    
    -- with correct tru and fls counts, we can combine rules as antirules
    rsAdded = M.unionsWith add rsWithFalse

    -- tot is just false + true
    rsWithTotal = M.map (\r -> r{antiRuleData = (antiRuleData r){tot=(fls $ antiRuleData r)+(tru $ antiRuleData r)}}) rsAdded

    -- finally we filter based on Assoc Rule Learning Thresholds
    validRule r = (tru $ antiRuleData r)>=minTrue && (fls$ antiRuleData r)<=maxFalse
    combinedAntiRules = M.filter validRule rsWithTotal
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
addCount counts (KeyValKeyCoor {..}) rd = let
  kcount k = M.findWithDefault 0 k counts
 in
  -- TODO is this correct? I thought it should only increment if both keys are present
  -- this increments by 1 for each key present
  rd { antiRuleData = (antiRuleData rd) { tot=(kcount k1 + kcount k2)}}
  
