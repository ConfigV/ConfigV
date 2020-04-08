{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE OverloadedStrings #-} 

module ConfigV.Learners.SMTRules where

import ConfigV.Types
import qualified ConfigV.Types.Locatable as R

import ConfigV.Learners.Common

import qualified Data.Map.Strict as M

import Data.Maybe
import Data.List

import Control.Monad.Reader
import Control.Monad.Omega


import ConfigV.Settings.Config
import ConfigV.Utils
import Debug.Trace

instance Learnable SMTFormula AntiRule where

  buildRelations f = {-# SCC "SMT" #-} do
    settings <- ask
    let 
						checkSMTRule :: SMTFormula -> Maybe (SMTFormula, AntiRule)
						checkSMTRule potentialSMTRule = 
								-- TODO all rules are correct by construction, until we add IntRel
								if True -- evalSMT potentialSMTRule
								then Just $ head $ embedAsTrueAntiRule [potentialSMTRule]
								else Nothing
						assignSMTs1 irs template =  {-# SCC "assignSMTs1" #-} mapMaybe (\p -> checkSMTRule $ template p) irs
						assignSMTs2 irPairs template = {-# SCC "assignSMTs2" #-} mapMaybe (\p -> checkSMTRule $ (uncurry template) p) irPairs
						--filterForms = {-# SCC "filterForms" #-} filter (smtFilterCriteria $ options settings)
						allRules = {-# SCC "allRules" #-} smts1 ++ smts2
						smts1 = {-# SCC "smts1" #-} flcm (assignSMTs1 f) $ formulasSize1
						smts2 = {-# SCC "smts2" #-} flcm (assignSMTs2 $ orderPreservingPairs $ sort f) $ runOmega formulasSize2
						flcm f xs = {-# SCC "fromList-concatMap" #-} concatMap f xs
    return $ 
      M.fromList $ 
--      filterForms 
        allRules

  merge rs = do
    settings <- ask
    let 
      --TODO need specialized support and conf
      minTrue = smtSupport $ thresholdSettings settings
      maxFalse = smtConfidence $ thresholdSettings settings
      rsAdded = M.unionsWith add rs
      rsWithFalse = M.mapWithKey (addFalse rs) rsAdded
      filteredRules = filterByThresholds minTrue maxFalse rsWithFalse

    return $ 
      filteredRules 
      --M.map snd $ trace (unlines . (map show) . M.toList $ M.map fst implicationLattice) implicationLattice


  --   should we report the relation r2 found in the target file
  --   as in conflict with the learned rule r1
  check _ existingLearnedRule incomingRuleToCheck = let
     rd1' = existingLearnedRule
     rd2' = incomingRuleToCheck
     agrees r1 r2 = 
       if tru r2 ==1
       then tru r1 > fls r1
       else True
   in
     if (not $agrees rd1' rd2')
     then Just existingLearnedRule
     else Nothing

  toError ir fname (smtF, rd) = Error{
     errLocs= zip (cycle [fname]) $ R.keys smtF
    ,errIdent = KEYVALKEY
    ,errMsg = "TODO"
    ,errSupport = (tru rd) + (fls rd)}

-- false is equal to how many files had a rule with the same antecedent clause, but not the consequent clause
addFalse:: [M.Map SMTFormula AntiRule] -> SMTFormula -> AntiRule -> AntiRule
addFalse ruleSets smtRule rd = let
    
    fCount = 
      sum $ map (\rmap -> fromEnum $ f smtRule rmap)
                ruleSets
 in
  rd{fls= fCount, tot = (tot rd) + fCount}

f :: SMTFormula -> M.Map SMTFormula AntiRule -> Bool
f r rs =
    (not $ M.member r rs) &&
    containsKeyBy r (\r' -> antecedent r' == antecedent r && consequent r' /= consequent r) rs

-- | if the file we are comparing to has support for this rule, and we dont see the rule itself, then this file is 1 unit of evidence that the rule does not hold
containsKeyBy :: k -> (k -> Bool) -> M.Map k v -> Bool
containsKeyBy r p rs = 
   M.foldrWithKey (\k v res -> res || p k) False rs
