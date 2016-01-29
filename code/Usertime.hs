module Usertime where

import Types

import Learners
import Convert

import Control.Monad
import Data.Maybe
import qualified Data.Map as M

import Data.Foldable

verifyOn :: RuleSet -> ConfigFile Language -> [String]
verifyOn r f = 
  let
    f' = convert f 
    orderingError =  check (order r) f'
    intRelError   =  check (intRel r) f'
    missingError  =  check (missing r) f'
    typeError     =  check (typeErr r) f'
    typeShow = maybe "" (\x->"TYPE ERR "++(unlines.(map show).M.toList) x) typeError
    all = [
        typeShow
      , maybe "" (\x->"ORDER ERR "++(unlines.(map show).M.toList) x) orderingError
      , maybe "" (\x->"INT REL ERR "++(unlines.(map show).M.toList) x) intRelError
      , maybe "" (\x-> "MISSING ERR "++(unlines.(map show)) x) missingError
      ]
    sizeErr = maybe 0 length
    typeSize = (maybe 0 M.size typeError) 
    count = typeSize +
      (maybe 0 M.size orderingError) +
      (sizeErr missingError) +
      (maybe 0 M.size intRelError) 
  in
    if typeSize >0 then ["\n"] ++ [typeShow]++ [show typeSize] else ["\n"]++all ++ [show count]
    
