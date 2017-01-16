module Checker where

import Types.IR
import Types.Rules
import Types.Errors
import Types.Common

import LearningEngine
import Convert (convert)

import qualified Data.Map.Strict as M
import           System.Directory

import Debug.Trace

verifyOn :: RuleSet -> ConfigFile Language -> ErrorReport
verifyOn rs configFile@(fp,_,_)  =
  genErrReport fp $ checkFile rs configFile

traceMe x = trace (show x) x
--traceMe = id
-- | fitler out the rules that won't be in the error report
checkFile :: RuleSet -> ConfigFile Language -> RuleSet
checkFile rs f =
   let
     fRules = buildAllRelations $ convert f
     fKs = map keyword $ convert f
     rs' = filterRules fKs rs
     diff = ruleDiff rs' fRules --the difference between the two rule sets
   in
     diff

filterRules :: [Keyword] -> RuleSet -> RuleSet
filterRules fKs rs = RuleSet {
       order   = f order
      ,missing = f' missing 
      ,intRel  = f intRel 
      ,typeErr = f typeErr 
     }
   where
     f classOfErr  =  M.filterWithKey (\e _ -> keysMatch (keys e)) (classOfErr rs)
     f' classOfErr =  M.filterWithKey (\e _ -> oneKeyMatch (keys e)) (classOfErr rs)
     keysMatch ks   = and $ map (\k->elem k fKs) ks
     oneKeyMatch ks = or $ map (\k->elem k fKs) ks

--TODO apply to each rule type in a rule set individually
--TODO make Ruleset an instance of functor or something? 
ruleDiff :: RuleSet -> RuleSet -> RuleSet
ruleDiff rs1 rs2 = RuleSet {
       order   = f order 
      ,missing = f missing
      ,intRel  = f' intRel
      ,typeErr = f typeErr
     }
   where
     --only use the key to resolve type ambiguity
     f classOfErr = M.differenceWithKey check (classOfErr rs1) (classOfErr rs2)
     f' classOfErr = M.differenceWithKey check (classOfErr rs1) (classOfErr rs2)
     

-- | basically, the show instance for rules
-- this was the biggest mess last time, can this somehow be simpiler
genErrReport :: FilePath -> RuleSet -> ErrorReport
genErrReport fname rs = 
  let  
    f :: Learnable a b => (RuleSet -> RuleDataMap a b) -> [Error]
    f classOfErr= map (\rd -> toError fname rd) $ M.toList $ classOfErr rs
  in 
    f order ++ 
    f missing ++ 
    f intRel ++
    f typeErr


