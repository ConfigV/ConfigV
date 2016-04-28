{-# LANGUAGE OverloadedStrings #-}

module Usertime where

import qualified Settings

import Learners
import Convert
import Types

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import Control.Applicative
import Data.Foldable (Foldable)

import Debug.Trace
--import Data.Foldable


instance Show Error where
  show e = "Error between "++(show$ errLoc1 e)++" and "++(show$ errLoc2 e)++" of type: "++(show $errIdent e)++"\n"

verifyOn :: RuleSet -> ConfigFile Language -> FilePath -> ErrorReport
verifyOn r f fname =
  let
    f' = convert f
    orderingError =  check (order r) f'
    intRelError   =  check (intRel r) f'
    missingError  =  check (missing r) f'
    typeError     =  check (typeErr r) f'

    showErr :: (Show k, Show v) => Maybe (M.Map k v) -> ((k,v) -> Error) -> [Error]
    showErr rawEs printFxn =
      let
        es = maybe [] M.toList rawEs
      in
        map printFxn es


    typeErrMsg x =
      --"TYPE ERROR: Expected a "++(show$snd x)++" for "++(show$fst x)
      Error {errLoc1 = (fname,fst x)
            ,errLoc2 = (fname,fst x)
            ,errIdent = "TYPE" }
    typeShow =
      showErr typeError typeErrMsg

    orderingErrMsg x =
      --"ORDERING ERROR: Expected "++(show$fst $fst x)++" BEFORE "++(show$snd$fst  x)
      Error {errLoc1 = (fname,keyword$snd$ fst x)
            ,errLoc2 = (fname,keyword$fst$ fst x)
            ,errIdent = "ORDERING"}
    orderingShow =
      showErr orderingError orderingErrMsg

    intRelErrMsg x =
      --"INTEGER RELATION ERROR: Expected "++(show$fst $fst x)++(show$fromJust$snd x)++(show$snd$fst x)
      Error {errLoc1 = (fname,snd$fst x)
            ,errLoc2 = (fname,fst $fst x)
            ,errIdent = "INTREL"}
    intRelShow =
      showErr intRelError intRelErrMsg


    missingShow =
      let
        es = fromMaybe [] missingError
        --f = (\x->"MISSING KEYWORD ERROR: Expected "++(show$k1 x)++" in the same file as: "++(show$k2 x))
        f x =  Error {errLoc1 = (fname,k1 x)
                     ,errLoc2 = (fname,k2 x)
                     ,errIdent = "MISSING"}
      in
        map f es



    all =
        [typeShow, orderingShow, intRelShow, missingShow]

    typeSize = (maybe 0 M.size typeError)
  in
    --if typeSize >0 then typeShow else filter (/="") $ concat all
    if typeSize >0 then typeShow else concat all
