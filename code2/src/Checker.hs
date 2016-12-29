module Checker where

import Types.IR
import Types.Rules
import Types.Errors

import LearningEngine
import Convert (convert)
import qualified Data.Map.Strict as M

verifyOn :: RuleSet -> ConfigFile Language -> ErrorReport
verifyOn rs configFile =
  genErrReport $ check rs configFile

-- | fitler out the rules that won't be in the error report
check :: RuleSet -> ConfigFile Language -> RuleSet
check rs f =
   let
     fRules = learnRules [f]
     relevantRules = getRelevant rs (convert f)
     diff = ruleDiff relevantRules fRules --the difference between the two rule sets
   in
     diff

--TODO apply to each rule type in a rule set individually
--TODO make Ruleset an instance of functor or something? fmap M.//
ruleDiff :: RuleSet -> RuleSet -> RuleSet
ruleDiff rs1 rs2 = RuleSet {
  order = (order rs1) M.\\ (order rs2)
  ,missing = (missing rs1) M.\\ (missing rs2)
  ,intRel = (intRel rs1) M.\\ (intRel rs2)
 }
-- TODO, again the RuleSet structure is pain 
getRelevant :: RuleSet -> IRConfigFile -> RuleSet
getRelevant rs f = RuleSet {
    order = M.filterWithKey (\k _ -> isRelevant f k) (order rs)
  , missing = M.filterWithKey (\k _ -> isRelevant f k) (missing rs)
  , intRel  = M.filterWithKey (\k _ -> isRelevant f k) (intRel rs)
  }

-- | basically, the show instance for rules
genErrReport :: RuleSet -> ErrorReport
genErrReport rs = 
--  if M.null rs then Nothing else Just rs
  --map show rs
  undefined
