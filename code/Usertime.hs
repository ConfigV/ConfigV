module Usertime where

import Lexical
import Values
import Syntax
import Types

usertime :: RuleSet -> ConfigFile Language -> Bool --Maybe Error
usertime r c = 
 let
  d = convert c
  e = verifyOn r d
 in
  e

verifyOn :: RuleSet -> ConfigFile Common -> Bool -- -> Maybe Error
verifyOn r f = 
  let
    l = checkLex (lexical r) f
    s = checkSyn (syntax r) f
    v = checkVal (value r) f
    all = l && s && v
  in
    all 
