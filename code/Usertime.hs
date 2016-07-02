{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds, MultiParamTypeClasses #-}


module Usertime where

import           Convert
import           Learners
import           Types

import           Control.Monad
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Text           as T

import           Learners.IntRelP
import           Learners.TypeMapper (assignProbs, findVal)

import           Control.Applicative
import           Data.Foldable       (Foldable)

import           Debug.Trace
--import Data.Foldable


import qualified Settings



showProbRules :: RuleSet -> [String]
showProbRules r =
  let
    delimiter = "|"
    m = filter (\(_,y,n) -> y+n>20 && n==0) $ missingP r
    o = orderP r
    i = intRelP r
    m' = missing r
    o' = order r
    showMissingP (r, y, n) =
      "Expected " ++ (show $ k1 r) ++ " with " ++ (show $ k2 r)
        ++ delimiter ++ (show y)
        ++ delimiter ++ (show n)
        ++ delimiter ++ (show $ elem r m')
    showOrderP (r, (y, n)) =
          "Expected " ++ (show $ fst r) ++ " before " ++ (show $ snd r)
          ++ delimiter ++ (show y)
          ++ delimiter ++ (show n)
          ++ delimiter ++ (show $ M.findWithDefault False r o')
    showIntRelP (r, f) =
      (show $ fst r) ++ " <=> " ++ (show $ snd r)
        ++ delimiter ++ (show $ lt f)
        ++ delimiter ++ (show $ eq f)
        ++ delimiter ++ (show $ gt f)
      --  ++ delimiter ++ (show $ findOrFindInverse r)
      {-where
        inverseIntRel x =
          case (show x) of -- hack to make case matching on operators work, sorry...
            "Just <=" -> Just (>=)
            "Just >=" -> Just (<=)
            "Just ==" -> Just (==)
            _         -> Nothing
        findOrFindInverse (x,y) =
          M.findWithDefault Nothing (x,y) i <|> inverseIntRel (M.findWithDefault Nothing (y,x) i)-}
  in
    case Settings.pROBRULES of
      Settings.Prob ->
        (["\n\nrule" ++ delimiter ++ "yes" ++ delimiter ++ "no" ++ delimiter ++ "valid"] ++ (map showMissingP m)) ++
        (["\n\nrule|yes|no|valid"] ++ (map showOrderP $ filter (\(r, (y, n)) -> y+n>50 && n==0) $ M.toList o)) ++
        (["\n\nordering|less_than|equals|greater_than|answer"] ++ (map showIntRelP $ filter (\(r, f) ->lt f==0 && eq f==0 && gt f > 35) $ M.toList i))
      _ ->
        (map (\x -> "Expected " ++ (show $ k1 x) ++ " with " ++ (show $ k2 x)) m') ++
        (map (\(x,y) -> "Expected " ++ (show $ fst x) ++ " before " ++ (show $ snd x)) $ filter snd $ M.toList o')


verifyOn :: RuleSet -> ConfigFile Language -> FilePath -> ErrorReport
verifyOn r f fname =
  let
    f' = convert f
    getErrors :: (Attribute t a, Foldable t) => (RuleSet -> t a) -> Maybe (t a)
    getErrors ruleSelect = check (ruleSelect r) $ convert f
    orderingError  = getErrors order
    missingError   = getErrors missing
    typeError      = getErrors typeErr
    missingErrorP  = getErrors missingP
    orderingErrorP = getErrors orderP
    intRelErrorP   = getErrors intRelP

    showErr :: (Show k, Show v) => Maybe (M.Map k v) -> ((k,v) -> Error) -> [Error]
    showErr rawEs printFxn =
      let
        es = maybe [] M.toList rawEs
      in
        map printFxn es

    -- this should be a ConvertToError typeclass
    typeErrMsg x =
      Error {errLoc1 = (fname,fst x)
            ,errLoc2 = (fname,fst x)
            ,errIdent = TYPE
            ,errMsg = "TYPE ERROR: Expected a "++(show$snd x)++" for "++(show$fst x)++ ". Found value " ++(show $ findVal f' $ fst x) ++ " of type " ++ (show $ assignProbs $ findVal f' $ fst x) }
    typeShow =
      showErr typeError typeErrMsg

    orderingErrMsg x =
      Error {errLoc1 = (fname,keyword$snd$ fst x)
            ,errLoc2 = (fname,keyword$fst$ fst x)
            ,errIdent = ORDERING
            ,errMsg = "ORDERING ERROR: Expected "++(show$fst $fst x)++" BEFORE "++(show$snd$fst  x)}
    orderingShow =
      showErr orderingError orderingErrMsg

    orderingPErrMsg x =
      Error {errLoc1 = (fname,keyword$snd$ fst x)
            ,errLoc2 = (fname,keyword$fst $fst x)
            ,errIdent = ORDERING
            ,errMsg = "ORDERING ERROR (PROB): Expected "++(show$fst $fst x)++" BEFORE "++(show$snd$fst  x)++" WITH PROB "++(showP $ snd x)}
    orderingShowP =
      showErr orderingErrorP orderingPErrMsg

    intRelPErrMsg :: ((Keyword,Keyword),FormulaC) -> Error
    intRelPErrMsg x =
      let
        es = maybe [] M.toList intRelErrorP
        fc x = show $ snd x
      in
        Error {errLoc1 = (fname,fst$fst x)
              ,errLoc2 = (fname,snd$fst x)
              ,errIdent = INTREL
              ,errMsg = "INTEGER RELATION ERROR (PROB): Expected "++(show$fst$fst x)++(fc x)++(show$snd$fst x)}
    intRelShowP =
      showErr intRelErrorP intRelPErrMsg


    {-intRelErrMsg x =
      --"INTEGER RELATION ERROR: Expected "++(show$fst $fst x)++(show$fromJust$snd x)++(show$snd$fst x)
      Error {errLoc1 = (fname,snd$fst x)
            ,errLoc2 = (fname,fst $fst x)
            ,errIdent = "INTREL"}
    intRelShow =
      showErr intRelError intRelErrMsg-}


    missingShow =
      let
        es = fromMaybe [] missingError
        f x =  Error {errLoc1 = (fname,k1 x)
                     ,errLoc2 = (fname,k2 x)
                     ,errIdent = MISSING
                     ,errMsg = "MISSING KEYWORD ERROR: Expected "++(show$k1 x)++" in the same file as: "++(show$k2 x)}
      in
        map f es


    ---------- probabilistic missing keywords debug ----------
    -- (why are there so many of them?! Why aren't they getting filtered out?!)
    missingShowP =
      let
        es = fromMaybe [] missingErrorP
        -- f = (\(x, y, n)->"MISSING KEYWORD ERROR (PROB): Expected "++(show$k1 x)++" in the same file as: "++(show$k2 x)++
        --  " with probability "++(show $ (fromIntegral y) / (fromIntegral (y + n)))++" "++(show y)++", "++(show n))
        f' (x, y, n) =
           Error {errLoc1 = (fname,k1 x)
                  ,errLoc2 = (fname,k2 x)
                  ,errIdent = MISSING --(PROB)" y:" ++ (show y) ++ " n:" ++ (show n)"
                  ,errMsg = "MISSING (PROB): Expected "++(show$k1 x)++" in the same file as: "++(show$k2 x)}
      in
        map f' es

    missingErrorProbs = map (\(x, y, n) -> fromIntegral y / fromIntegral (y + n)) $ fromMaybe [] missingErrorP
    mean x = sum x / (fromIntegral $ length x)
    median x = (L.sort x) !! (length x `div` 2)
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
    intRelShowPC = "Probabilistic integer relation errors: " ++ (show $ length $ maybe [] M.toList intRelErrorP)
    sizeErr = maybe 0 length
    all =
      case Settings.pROBRULES of
        Settings.Prob -> [typeShow, orderingShowP, intRelShowP, missingShowP]
        Settings.NonProb -> [typeShow, orderingShow, missingShow]
        Settings.Test -> [typeShow, orderingShow, missingShow, orderingShowP, intRelShowP, missingShowP]



    typeSize = (maybe 0 M.size typeError)
    count = typeSize +
      maybe 0 M.size orderingError +
      sizeErr missingError +
      sizeErr missingErrorP
  in
    --if typeSize >0 then typeShow else filter (/="") $ concat all
    --if typeSize >0 then typeShow else concat all
    concat all



-- utility printing method for our probability tuples
showP (y, n) =
  let
    y' = fromIntegral y
    n' = fromIntegral n
    p = y' / (y' + n')
  in
    show p ++ " WITH COUNTS " ++ show (y, n)
--  in
    --if typeSize >0 then typeShow else filter (/="") $ concat all
  --  if typeSize >0 then typeShow else concat all
