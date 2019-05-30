{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Learners.SMTRules where

import Types.IR
import Types.Common
import Types.Errors
import Types.Rules 
import Types.SMTRules 
import qualified Types.Rules as R
import Types.Countable
import qualified Types.Locatable as R

import Learners.Common

import qualified Data.Map as M
import qualified Data.Text as T

import Data.Interned
import Data.Maybe

import Settings.Config
import Control.Monad.Reader
import Control.Monad.Omega

import Utils

instance Learnable SMTFormula AntiRule where

  buildRelations f = let
    checkSMTRule :: SMTFormula -> Maybe (SMTFormula, AntiRule)
    checkSMTRule potentialSMTRule = 
      -- TODO all rules are correct by construction, until we add IntRel
      if True -- evalSMT potentialSMTRule
      then Just $ head $ embedAsTrueAntiRule [potentialSMTRule]
      else Nothing
    assignSMTs irPairs template = mapMaybe (\p -> checkSMTRule $ (uncurry template) p) irPairs
   in
    return $ M.fromList $ concatMap (assignSMTs $ pairs f) $ runOmega templatesArity2

  merge rs = do
    settings <- ask
    let 
      minTrue = keywordCoorSupport $ thresholdSettings settings
      maxFalse = keywordCoorConfidence $ thresholdSettings settings
      rsAdded = M.unionsWith add rs
      rsWithFalse = M.mapWithKey (addFalse rs) rsAdded
    return $ filterByThresholds minTrue maxFalse rsWithFalse


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
addFalse allRules smtRule rd = let
    
    fCount = 
      sum $ map (\rmap -> fromEnum $
                            (not $ M.member smtRule rmap) &&
                            (not $ M.null $ M.filterWithKey (\r' _ -> antecedent r' == antecedent smtRule && consequent r' /= consequent smtRule) rmap))
                allRules
 in
  rd{fls= fCount}
  
