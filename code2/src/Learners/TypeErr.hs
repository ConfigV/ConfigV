{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, OverloadedStrings #-}

module Learners.ConfigTypeErr where

import Types.IR
import Types.Errors
import Types.Rules 

import Prelude hiding (ConfigTypeErr)
import qualified Data.Map as M
import           System.Directory
import qualified Data.Text as T
import qualified Data.Bits as B

import Learners.Common
-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable ConfigTypeErr where

  buildRelations :: IRConfigFile -> RuleDataMap ConfigTypeErr
  buildRelations f = let
    getQType :: String -> QType
    getQType v =
      undefined --regex, which types are possible
    toConfigTypeErr :: IRLine -> ConfigTypeErr
    toConfigTypeErr ir = ConfigTypeErr ((keyword ir,getQType $ val ir))
   in
     M.fromList $ embedOnce $ map toConfigTypeErr f

  --NB this doesnt use the the rule/anti rule scheme
  --only loko at the tru field of ruleData
  resolve :: RuleDataMap ConfigTypeErr -> RuleDataMap ConfigTypeErr
  resolve rs = let
    condition r@(RuleData tru fls t e) = 
      --tru>0 && fls==0 -- ConfigC 
      tru>=3 && fls<=1  -- ConfigV
   in
    M.map (\r@(RuleData tru fls t e) -> if condition r then RuleData tru fls t True else r) rs'

  isRelevant :: IRConfigFile -> ConfigTypeErr -> Bool
  isRelevant cf (ConfigTypeErr (k,_)) = let
     ks = map keyword cf
   in
     elem k ks

  toError :: FilePath -> (ConfigTypeErr,RuleData) -> Error
  toError fname ((ConfigTypeErr(k,t)),rd) = Error{
     errLoc1 = (fname,k)
    ,errLoc2 = (fname,k) --redundant, change Error tyoe to errLoc :: [(File,Key)]
    ,errIdent = TYPE
    ,errMsg = "TYPE ERROR: Expected a "++(show ty)++" for "++(show k)}
    --".Found value " ++(show $ findVal f' $ fst x) ++ " of type " ++ (show $ assignProbs $ findVal f' $ fst x) }
