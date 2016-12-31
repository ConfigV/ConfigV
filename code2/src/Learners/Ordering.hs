{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}

module Learners.Ordering where

import Types.IR
import Types.Errors
import Types.Rules 

import Prelude hiding (Ordering)
import qualified Data.Map as M
import           System.Directory

-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable Ordering where

  buildRelations :: IRConfigFile -> RuleDataMap a
  buildRelations f = M.empty
  -- | is a rule relevant to the file we want to verify
  --   this used to be check
  resolve :: RuleDataMap a -> RuleDataMap a
  resolve = id
  isRelevant :: IRConfigFile -> a -> Bool
  isRelevant f r = False
  
  toError :: FilePath -> (Ordering,RuleData) -> Error
  toError fname ((Ordering (k1,k2)),rd) = Error{
     errLoc1 = (fname,k1)
    ,errLoc2 = (fname,k2)
    ,errIdent = ORDERING
    ,errMsg = "ORDERING ERROR: Expected "++(show k1)++" BEFORE "++(show k2)}

