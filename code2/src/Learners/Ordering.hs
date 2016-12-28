{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Learners.Ordering where

import Types.Rules 
import qualified Types.Rules as R

instance Learnable R.Ordering where
  buildRelations = undefined
--  merge = undefined
  check = undefined
