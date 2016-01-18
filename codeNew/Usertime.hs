module Usertime where

import Types

import Learners
import Convert

usertime :: RuleSet -> ConfigFile Language -> Bool --Maybe Error
usertime r c = 
 let
  d = convert c
  e = verifyOn r d
 in
  e

verifyOn :: RuleSet -> IRConfigFile -> Bool -- -> Maybe Error
verifyOn r f = 
  let
    l = check (order r) f
    i = check (intRel r) f
    all = l && i
  in
    all 
