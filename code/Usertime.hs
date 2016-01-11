module Usertime where

import Types

usertime :: RuleSet -> ConfigFile Language -> Maybe Error
usertime r c = 
 let
  d = convert c
  e = verifyOn r d
 in
  e

verifyOn :: RuleSet -> ConfigFile Common -> Maybe Error
verifyOn r f = Nothing
