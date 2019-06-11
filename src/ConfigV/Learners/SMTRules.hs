{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE OverloadedStrings #-} 

module ConfigV.Learners.SMTRules where

import ConfigV.Types
import qualified ConfigV.Types.Locatable as R

import ConfigV.Learners.Common

import qualified Data.Map as M

import Data.Maybe
import Data.List

import Control.Monad.Reader
import Control.Monad.Omega

import Algebra.PartialOrd

import ConfigV.Settings.Config
import ConfigV.Utils
import Debug.Trace

instance Learnable SMTFormula AntiRule where

  buildRelations f = let
    checkSMTRule :: SMTFormula -> Maybe (SMTFormula, AntiRule)
    checkSMTRule potentialSMTRule = 
      -- TODO all rules are correct by construction, until we add IntRel
      if True -- evalSMT potentialSMTRule
      then Just $ head $ embedAsTrueAntiRule [potentialSMTRule]
      else Nothing
    assignSMTs1 irs template = mapMaybe (\p -> checkSMTRule $ template p) irs
    assignSMTs2 irPairs template = mapMaybe (\p -> checkSMTRule $ (uncurry template) p) irPairs
    allRules = M.union 
       (M.fromList $ concatMap (assignSMTs1 f) $ formulasSize1)
       (M.fromList $ concatMap (assignSMTs2 $ orderPreservingPairs $ sort f) $ runOmega formulasSize2)
   in
    return allRules
       

-- filter (\(s,_) -> containsIsSetTo s) $ 

  merge rs = do
    settings <- ask
    let 
      --TODO need specialized support and conf
      minTrue = smtSupport $ thresholdSettings settings
      maxFalse = smtConfidence $ thresholdSettings settings
      rsAdded = M.unionsWith add rs
      rsWithFalse = M.mapWithKey (addFalse rs) rsAdded
      filteredRules = filterByThresholds minTrue maxFalse rsWithFalse
      
      implicationLattice = buildImplGraph filteredRules
    return $ M.map snd $ trace (unlines . (map show) . M.toList $ M.map fst implicationLattice)implicationLattice


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


instance PartialOrd SMTFormula where
  leq r1 r2 = 
    antecedent r1 `implies` antecedent r2 &&
    consequent r2 `implies` consequent r1


-- | build a graph based on partial order of rules
buildImplGraph :: M.Map SMTFormula AntiRule -> M.Map SMTFormula ([SMTFormula], AntiRule)
buildImplGraph rs =
  M.mapWithKey 
     (\r a -> (M.keys $ M.filterWithKey (\k _ -> comparable k r && r `leq` k) rs, a))
     rs


-- false is equal to how many files had a rule with the same antecedent clause, but not the consequent clause
addFalse:: [M.Map SMTFormula AntiRule] -> SMTFormula -> AntiRule -> AntiRule
addFalse allRules smtRule rd = let
    
    fCount = 
      sum $ map (\rmap -> fromEnum $
                            (not $ M.member smtRule rmap) &&
                            (not $ M.null $ M.filterWithKey (\r' _ -> antecedent r' == antecedent smtRule && consequent r' /= consequent smtRule) rmap))
                allRules
 in
  rd{fls= fCount, tot = (tot rd) + fCount}
  
