{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}

module Learners.KeywordCoor where

import Types.IR
import Types.Errors
import Types.Rules 
import qualified Types.Rules as R

import qualified Data.Map as M
import           System.Directory

instance Learnable R.KeywordCoor where

  buildRelations :: IRConfigFile -> RuleDataMap a
  buildRelations f = M.empty
  -- | is a rule relevant to the file we want to verify
  --   this used to be check
  resolve :: RuleDataMap a -> RuleDataMap a
  resolve = id
  isRelevant :: IRConfigFile -> a -> Bool
  isRelevant f r = False
  
  toError :: FilePath -> (KeywordCoor,RuleData) -> Error
  toError fname ((KeywordCoor (k1,k2)),rd) = Error{
     errLoc1 = (fname,k1)
    ,errLoc2 = (fname,k2)
    ,errIdent = MISSING
    ,errMsg = "KEYWORD ERROR: Expected "++(show k1)++" WITH "++(show k2)}
