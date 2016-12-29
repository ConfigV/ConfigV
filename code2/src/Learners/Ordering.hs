{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Learners.Ordering where

import Types.Rules 
import qualified Types.Rules as R

-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable R.Ordering where

  buildRelations = undefined

  -- | this is where we choose cutoff values
  resolve = undefined
