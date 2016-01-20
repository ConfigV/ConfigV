module Usertime where

import Types

import Learners
import Convert

import Control.Monad


verifyOn :: RuleSet -> TypeMap -> ConfigFile Language -> Error
verifyOn r tyMap f = 
  let
    f' = convert tyMap f 
    orderingError = check (order r) f'
    intRelError   = check (intRel r) f'
    all = mplus orderingError intRelError
  in
    all 
