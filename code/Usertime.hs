module Usertime where

import Types

import Learners
import Convert

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L

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
    orderingErrorP = check (orderP r) f'
   
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

    orderingShowP =
      let 
        es = maybe [] M.toList orderingErrorP
        f = (\x->"ORDERING ERROR (PROB): Expected "++(show$fst $fst x)++" BEFORE "++(show$snd$fst  x)++" WITH PROB "++(showP $ snd x)++"\n")
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
    ---------- probabilistic missing keywords debug ----------
    -- (why are there so many of them?! Why aren't they getting filtered out?!)
    missingShowP =
      let  
        es = fromMaybe [] missingErrorP
        f = (\(x, y, n)->"MISSING KEYWORD ERROR (PROB): Expected "++(show$k1 x)++" in the same file as: "++(show$k2 x)++
          " with probability "++(show $ (fromIntegral y) / (fromIntegral (y + n)))++" "++(show y)++", "++(show n)++" \n")
      in
        concatMap f es
    missingErrorProbs = map (\(x, y, n) -> (fromIntegral y) / (fromIntegral (y + n))) $ fromMaybe [] missingErrorP
    mean x = (sum x) / (fromIntegral $ length x)
    median x = (L.sort x) !! ((length x) `div` 2)
    meanV = mean missingErrorProbs
    medianV = median missingErrorProbs
    missingErrorPDebug = ["\n----------PROBABILISTIC MISSING KEYWORDS DEBUG----------",
                          "\nmean: " ++ (show meanV),
                          "\nmedian: " ++ (show medianV),
                          "\nnumber of 0%: " ++ (show $ length $ filter (== 0) missingErrorProbs),
                          "\nnumber of 100%: " ++ (show $ length $ filter (== 1) missingErrorProbs),
                          "\nnumber of probabilistic keyword errors: " ++ (show $ maybe 0 length missingErrorP),
                          "\n--------------------------------------------------------"]
    ---------- probabilistic missing keywords debug end ----------
    orderingShowPC = "Probabilistic ordering errors: " ++ (show $ length $ maybe [] M.toList orderingErrorP)
    orderingShowNPC = "Non-Probabilistic ordering errors: " ++ (show $ length $ maybe [] M.toList orderingError)
    all = [
        typeShow
      , orderingShow
      , orderingShowP
      , intRelShow
      , missingShow
      --, missingShowP
      , orderingShowPC
      , orderingShowNPC
      ]
    sizeErr = maybe 0 length
    typeSize = (maybe 0 M.size typeError) 
    count = typeSize +
      (maybe 0 M.size orderingError) +
      (sizeErr missingError) +
      (maybe 0 M.size intRelError) +
      (sizeErr missingErrorP)
  in
    if typeSize >0 then ["\n"] ++ [typeShow]++ [show typeSize] else ["\n"]++all ++ [show count] ++ missingErrorPDebug
    
-- utility printing method for our probability tuples
showP (y, n) =
  let 
    y' = fromIntegral y
    n' = fromIntegral n
    p = y' / (y' + n')
  in
    (show p) ++ " WITH COUNTS " ++ (show (y, n))