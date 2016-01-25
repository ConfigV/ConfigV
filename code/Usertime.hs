module Usertime where

import Types

import Learners
import Convert

import Control.Monad
import Data.Maybe

verifyOn :: RuleSet -> TypeMap -> ConfigFile Language -> [String]
verifyOn r tyMap f = 
  let
    f' = convert tyMap f 
    s = fromMaybe ""
    orderingError = s$ check (order r) f'
    intRelError   = s$ check (intRel r) f'
    missingError   = s$ check (missing r) f'
    all = [missingError,  orderingError, intRelError]
  in
    all 
