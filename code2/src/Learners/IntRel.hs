{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}

module Learners.IntRel where

import Types.IR
import Types.Errors
import Types.Rules 
import qualified Types.Rules as R

import qualified Data.Map as M
import           System.Directory

-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable R.IntRel where

  buildRelations :: IRConfigFile -> RuleDataMap a
  buildRelations f = M.empty
  -- | is a rule relevant to the file we want to verify
  --   this used to be check
  resolve :: RuleDataMap a -> RuleDataMap a
  resolve = id
  isRelevant :: IRConfigFile -> a -> Bool
  isRelevant f r = False
    
  toError :: FilePath -> (IntRel,RuleData) -> Error
  toError fname ((IntRel (l1,l2,formula)),rd) = Error{
     errLoc1 = (fname,keyword l1)
    ,errLoc2 = (fname,keyword l2)
    ,errIdent = INTREL
    ,errMsg = "INTEGER RELATION ERROR: Expected "++(show l1)++(show formula)++(show l2)
    }
