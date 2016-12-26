{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Learners.Ordering where

import Types.Rules 
import qualified Types.Rules as R

instance Learnable [] R.Ordering where
  buildRelations = undefined
  merge = undefined

instance Checkable R.Ordering where
  check = undefined
