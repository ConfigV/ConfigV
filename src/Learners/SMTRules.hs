{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Learners.SMTRules where

import Types.IR
import Types.Common
import Types.Errors
import Types.Rules 
import qualified Types.Rules as R
import Types.Countable

import Learners.Common

import qualified Data.Map as M
import qualified Data.Text as T

import Data.Interned

import Settings.Config
import Control.Monad.Reader

import Utils

instance Learnable R.SMTRule Antirule where

  -- | With SMT formulas, I am going to learn many formulas in the same loop
  --   so when I build relations, I want to build a relation for every possible formula
  --   this is not so ideal actually since then i cannot iteratively learn...
  --   maybe i can use laziness somehow?
  buildRelations f = let
    checkSMTRule potentialSMTRule = 
      if evalSMT potentialSMTRule 
      then Just $ embedAsTrueAntiRule potentialSMTRule
      else Nothing
    assignSMTs ps smtTemplate = mapMaybe (\p -> checkSMTRule $ toSMTRule smtTemplate p) ps 
    smtTemplates = undefined -- TODO [...]
   in
    return $ M.fromList $ concatMap (assignSMTs $ pairs f) smtTemplates

  merge rs = do
    settings <- ask
    antiRuleCounts <- mergeAsAntiRules rs
      --Need to calculate nontriviality based on the full set of rules we observed
    let nonTrivs = addTrivEvidence (M.unionsWith add rs) antiRuleCounts
    return $ M.filter (\r-> trivialityEvidence r <= (trivEvidenceThreshold $ thresholdSettings settings)) nonTrivs

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
addTrivEvidence ::  
   RuleDataMap KeyValKeyCoor NontrivRule -> 
   RuleDataMap KeyValKeyCoor NontrivRule -> 
   RuleDataMap KeyValKeyCoor NontrivRule
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

-- | treat these rules similarly to antirules, but a little different??? 
mergeAsAntiRules :: [RuleDataMap KeyValKeyCoor NontrivRule] -> Reader ConfigVConfiguration (RuleDataMap KeyValKeyCoor NontrivRule)
mergeAsAntiRules rMaps = do
    settings <- ask
    let
      minTrue = keyValKeyCoorSupport $ thresholdSettings settings
      maxFalse = keyValKeyCoorConfidence $ thresholdSettings settings
    -- for a single rule, the number of time the rule was false is equal to how many files had a rule with k1,v1, but not a rule with k1,v1,k2
      findOpp k r = let
         --this is the support calculation for KVK rules, which indeed only take the keywords - I will need to redefine support?
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
    return combinedAntiRules

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
  
