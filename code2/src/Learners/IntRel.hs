{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Learners.IntRel where

import Types.Rules

instance Learnable [] ordering where
  buildRelations = undefined
  merge = undefined

instance Checkable ordering where
  check = undefined
