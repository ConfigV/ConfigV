module Usertime where

import Types

import Learners
import Convert

import Control.Monad
import Data.Maybe
import qualified Data.Map as M

verifyOn :: RuleSet -> ConfigFile Language -> [String]
verifyOn r f = 
  let
    f' = convert f 
    orderingError =  check (order r) f'
    intRelError   =  check (intRel r) f'
    missingError  =  check (missing r) f'
    typeError     =  check (typeErr r) f'
    all = [
        show typeError
      , show orderingError
      , show intRelError
      , show missingError
      ]
    count = 
      (maybe 0 M.size typeError) +
      (length $ fromMaybe [] orderingError) +
      (length $ fromMaybe [] missingError) +
      (length $ fromMaybe [] intRelError) 
  in
    all ++ [show count]
    
