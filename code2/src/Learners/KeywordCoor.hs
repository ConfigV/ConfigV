{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module Learners.KeywordCoor where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable

import qualified Types.Rules as R

import qualified Data.Map as M
import           System.Directory

instance Learnable R.KeywordCoor AntiRule where

  buildRelations f = M.empty
  -- | is a rule relevant to the file we want to verify
  --   this used to be check
  merge= id
  check _ r1 r2 = Nothing
  
  toError fname ((KeywordCoor (k1,k2)),rd) = Error{
     errLocs= [(fname,k1),(fname,k2)]
    ,errIdent = MISSING
    ,errMsg = "KEYWORD ERROR: Expected "++(show k1)++" WITH "++(show k2)}
