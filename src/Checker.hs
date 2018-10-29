{-# LANGUAGE ScopedTypeVariables #-}
module Checker where

import Types.IR
import Types.Rules
import Types.Errors
import Types.Common

import LearningEngine
import Convert (convert)

import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import Data.Interned

import qualified Data.Text as T

verifyOn :: RuleSet -> ConfigFile Language -> ErrorReport
verifyOn rs configFile  =
  genErrReport configFile $ checkFile rs configFile

--traceMe = id
-- | fitler out the rules that won't be in the error report
checkFile :: RuleSet -> ConfigFile Language -> RuleSet
checkFile rs f =
   let
     keyCounts :: M.Map Keyword Int 
     keyCounts = foldl (\rs ir-> M.insertWith (+) (keyword ir) 1 rs) M.empty (convert f)
     {-fRules = buildAllRelations keyCounts $ convert f --TODO what in gods name is this doing here?
     fKs = map keyword $ convert f
     rs' = filterRules fKs rs
     diff = ruleDiff rs' fRules --the difference between the two rule sets-}
   in
     emptyRuleSet --diff

-- what is this doing? is this calculating support? no i dont think so, maybe this really is just the checking procdure - still not sure what is happenign here tho
filterRules :: [Keyword] -> RuleSet -> RuleSet
filterRules fKs rs = RuleSet {
       order   = f order
      ,missing = f' missing 
      ,keyvalkey = f' keyvalkey --why only require one key match?
      ,intRel  = f intRel 
      ,typeErr = f'' typeErr 
      ,fineInt = f fineInt
     }
   where
     f classOfErr  =  M.filterWithKey (\e _ -> keysMatch (keys e)) (classOfErr rs)
     f' classOfErr =  M.filterWithKey (\e _ -> oneKeyMatch (keys e)) (classOfErr rs)
     f'' classOfErr=  M.filterWithKey (\(e::TypeErr) _ -> keyPartMatch (map unintern $ keys e)) (classOfErr rs)
     keysMatch ks   = and $ map (\k->elem k fKs) ks
     oneKeyMatch ks = or $ map (\k->elem k fKs) ks
     keyPartMatch ks = elem (T.takeWhile (/='[') $ head ks) (map (T.takeWhile (/='[').unintern) fKs) 


--TODO apply to each rule type in a rule set individually
--TODO make Ruleset an instance of functor or something? 
ruleDiff :: RuleSet -> RuleSet -> RuleSet
ruleDiff rs1 rs2 = RuleSet {
       order   = f order 
      ,missing = f missing
      ,keyvalkey = f keyvalkey
      ,intRel  = f' intRel
      ,typeErr = f typeErr
      ,fineInt = f' fineInt
     }
   where
     --only use the key to resolve type ambiguity
     f classOfErr = M.differenceWithKey check (classOfErr rs1) (classOfErr rs2)
     --intersect, and return formula tht will never match when Nothing
     f' classOfErr = M.merge M.dropMissing M.dropMissing (M.zipWithMaybeMatched check) (classOfErr rs1) (classOfErr rs2)
     

-- | basically, the show instance for rules
-- this was the biggest mess last time, can this somehow be simpiler
genErrReport :: ConfigFile Language -> RuleSet -> ErrorReport
genErrReport cf@(fname,_,_) rs = 
  let  
    f :: Learnable a b => (RuleSet -> RuleDataMap a b) -> [Error]
    f classOfErr= map (\rd -> toError (convert cf) fname rd) $ M.toList $ classOfErr rs
  in 
    f order ++ 
    f missing ++ 
    f keyvalkey ++ 
    f intRel ++
    f typeErr ++
    f fineInt


