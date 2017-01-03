module Checker where

import Types.IR
import Types.Rules
import Types.Errors

import LearningEngine
import Convert (convert)

import qualified Data.Map.Strict as M
import           System.Directory

verifyOn :: RuleSet -> ConfigFile Language -> ErrorReport
verifyOn rs configFile@(fp,_,_)  =
  genErrReport fp $ check rs configFile

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
    order   = M.filterWithKey (\k v -> isRelevant f k && enabled v) (order rs)
  , missing = M.filterWithKey (\k v -> isRelevant f k) (missing rs)
  , intRel  = M.filterWithKey (\k v -> isRelevant f k && enabled v) (intRel rs)
  }

-- | basically, the show instance for rules
-- this was the biggest mess last time, can this somehow be simpiler
genErrReport :: FilePath -> RuleSet -> ErrorReport
genErrReport fname rs = 
  let  
    f :: Learnable a => (RuleSet -> M.Map a RuleData) -> [Error]
    f classOfErr= map (toError fname) (M.toList $ classOfErr rs)
  in 
    f order ++ f missing ++ f intRel


