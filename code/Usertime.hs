module Usertime where

import Lexical
import Values
import Order
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
    l = check (lexical r) f
    s = check (order r ) f
    v = check (value r) f
    all = l && s && v
  in
    all 
