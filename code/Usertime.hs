module Usertime where

import Types

import Learners
import Convert

import Control.Monad
import Data.Maybe
import qualified Data.Map as M

--import Data.Foldable

verifyOn :: RuleSet -> ConfigFile Language -> [String]
verifyOn r f = 
  let
    f' = convert f 
    orderingError =  check (order r) f'
    intRelError   =  check (intRel r) f'
    missingError  =  check (missing r) f'
    typeError     =  check (typeErr r) f'
    missingErrorP =  check (missingP r) f'
   
    --the dreaded monomorphism restriction. i tihnk there is way to turn it off tho
    typeShow = 
      let 
        es = maybe [] M.toList typeError
        f = (\x->"TYPE ERROR: Expected a "++(show$snd x)++" for "++(show$fst x)++"\n")
      in
        concatMap f es

    orderingShow =
      let 
        es = maybe [] M.toList orderingError
        f = (\x->"ORDERING ERROR: Expected "++(show$fst $fst x)++" BEFORE "++(show$snd$fst  x)++"\n")
      in
        concatMap f es
   
    intRelShow = 
      let  
        es = maybe [] M.toList intRelError
        f = (\x->"INTEGER RELATION ERROR: Expected "++(show$fst $fst x)++(show$fromJust$snd x)++(show$snd$fst x)++"\n")
      in
        concatMap f es
    missingShow = 
      let  
        es = fromMaybe [] missingError
        f = (\x->"MISSING KEYWORD ERROR: Expected "++(show$k1 x)++" in the same file as: "++(show$k2 x)++"\n")
      in
        concatMap f es
    missingShowP =
      let  
        es = fromMaybe [] missingErrorP
        f = (\(x, y, n)->"MISSING KEYWORD ERROR (PROB): Expected "++(show$k1 x)++" in the same file as: "++(show$k2 x)++" with probability "++(show $ (fromIntegral y) / (fromIntegral (y + n)))++"\n")
      in
        concatMap f es
    all = [
        typeShow
      , orderingShow
      , intRelShow
      , missingShow
      , missingShowP
      ]
    sizeErr = maybe 0 length
    typeSize = (maybe 0 M.size typeError) 
    count = typeSize +
      (maybe 0 M.size orderingError) +
      (sizeErr missingError) +
      (maybe 0 M.size intRelError) +
      (sizeErr missingErrorP)
  in
    if typeSize >0 then ["\n"] ++ [typeShow]++ [show typeSize] else ["\n"]++all ++ [show count]
    
