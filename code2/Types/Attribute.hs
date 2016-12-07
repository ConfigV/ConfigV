module Attribute where

class Foldable t => Learnable t a where
  -- | build a set of rules from a IR
  learn :: IRConfigFile -> t a
  -- | how do we merge two rules
  merge :: a -> a -> t a
  -- | how to check if a rule holds on a file
  check :: a -> IRConfigFile -> Bool

data Rule a = ...
  
