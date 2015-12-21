module Usertime where

import Types

usertime :: (RuleSet,a) -> ConfigFile a -> Maybe Error
usertime r c =
  d = convert c
  e = verifyOn r d
  return e

