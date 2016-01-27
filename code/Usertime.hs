module Usertime where

import Types

import Learners
import Convert

import Control.Monad
import Data.Maybe

verifyOn :: RuleSet -> ConfigFile Language -> [String]
verifyOn r f = 
  let
    f' = convert f 
    s = fromMaybe ""
    orderingError = s$ check (order r) f'
    intRelError   = s$ check (intRel r) f'
    missingError   = s$ check (missing r) f'
    typeError = s$ check (typeErr r) f'
    all = [
        typeError
      , orderingError
      , intRelError
      , missingError
      ]
  in
    all 
