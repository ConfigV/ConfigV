{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module Learners.IntRel where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable
import qualified Types.Rules as R

import qualified Data.Map as M
import           System.Directory

-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable R.IntRel Formula where

  buildRelations f = M.empty
  -- | is a rule relevant to the file we want to verify
  --   this used to be check
  
  merge = id
  
  check _ r1 r2 = Nothing
    
  toError fname ((IntRel (l1,l2)),rd) = Error{
     errLocs = map (\x->(fname,keyword x)) [l1,l2]
    ,errIdent = INTREL
    ,errMsg = "INTEGER RELATION ERROR: Expected "++(show l1)++(show rd)++(show l2)
    }
