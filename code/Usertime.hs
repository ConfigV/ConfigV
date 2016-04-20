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
import Learners.IntRelP
import Control.Applicative
import Data.Foldable (Foldable)

--import Data.Foldable

showProbRules :: RuleSet -> [String]
showProbRules r =
  let
    delimiter = "|"
    m = missingP r
    o = orderP r
    i = intRelP r
    m' = missing r
    o' = order r
    i' = intRel r
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
        ++ delimiter ++ (show $ findOrFindInverse r)
      where
        inverseIntRel x =
          case (show x) of -- hack to make case matching on operators work, sorry...
            "Just <=" -> Just (>=)
            "Just >=" -> Just (<=)
            "Just ==" -> Just (==)
            _         -> Nothing
        findOrFindInverse (x,y) =
          M.findWithDefault Nothing (x,y) i' <|> inverseIntRel (M.findWithDefault Nothing (y,x) i')
  in
    --["rule" ++ delimiter ++ "yes" ++ delimiter ++ "no" ++ delimiter ++ "valid"] ++ (map showMissingP m)
    ["rule|yes|no|valid"] ++ (map showOrderP $ M.toList o)
    --["ordering|less_than|equals|greater_than|answer"] ++ (map showIntRelP $ M.toList i)
    --map (\x -> "Expected " ++ (show $ k1 x) ++ " with " ++ (show $ k2 x)) m'
    --map (\(x,y) -> "Expected " ++ (show $ fst x) ++ " before " ++ (show $ snd x)) $ filter snd $ M.toList o'
    --map show $ M.toList i'



verifyOn :: RuleSet -> ConfigFile Language -> FilePath -> ErrorReport
verifyOn r f fname =
  let
    f' = convert f
    orderingError =  check (order r) f'
    intRelError   =  check (intRel r) f'
    missingError  =  check (missing r) f'
    typeError     =  check (typeErr r) f'
    missingErrorP =  check (missingP r) f'
    orderingErrorP = check (orderP r) f'
    intRelErrorP   =  check (intRelP r) f'

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

    orderingPErrMsg x =
      --"ORDERING ERROR (PROB): Expected "++(show$fst $fst x)++" BEFORE "++(show$snd$fst  x)++" WITH PROB "++(showP $ snd x)
      Error {errLoc1 = (fname,keyword$snd$ fst x)
            ,errLoc2 = (fname,keyword$fst $fst x)
            ,errIdent = "ORDERING(PROB)"}
    orderingShowP =
      showErr orderingErrorP orderingPErrMsg

    intRelErrMsg x =
      --"INTEGER RELATION ERROR: Expected "++(show$fst $fst x)++(show$fromJust$snd x)++(show$snd$fst x)
      Error {errLoc1 = (fname,snd$fst x)
            ,errLoc2 = (fname,fst $fst x)
            ,errIdent = "INTREL"}
    intRelShow =
      showErr intRelError intRelErrMsg

    intRelPErrMsg x =
      let
        es = maybe [] M.toList intRelErrorP
        k1 x = show $ fst $ fst x
        fc x = show $ snd x
        k2 x = show $ snd $ fst x
      in
        --"INTEGER RELATION ERROR (PROB): Expected "++(k1 x)++(fc x)++(k2 x)
        Error {errLoc1 = (fname,T.pack$k1 x)
              ,errLoc2 = (fname,T.pack$k2 x)
              ,errIdent = "INTREL(PROB)"}
    intRelShowP =
      showErr intRelErrorP intRelPErrMsg

    missingShow =
      let
        es = fromMaybe [] missingError
        --f = (\x->"MISSING KEYWORD ERROR: Expected "++(show$k1 x)++" in the same file as: "++(show$k2 x))
        f x =  Error {errLoc1 = (fname,k1 x)
                     ,errLoc2 = (fname,k2 x)
                     ,errIdent = "MISSING"}
      in
        map f es


    ---------- probabilistic missing keywords debug ----------
    -- (why are there so many of them?! Why aren't they getting filtered out?!)
    missingShowP =
      let
        es = fromMaybe [] missingErrorP
        -- f = (\(x, y, n)->"MISSING KEYWORD ERROR (PROB): Expected "++(show$k1 x)++" in the same file as: "++(show$k2 x)++
        --  " with probability "++(show $ (fromIntegral y) / (fromIntegral (y + n)))++" "++(show y)++", "++(show n))
        f' (x, y, n) = Error {errLoc1 = (fname,k1 x)
                     ,errLoc2 = (fname,k2 x)
                     ,errIdent = "MISSING(PROB)"}
      in
        map f' es

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
    intRelShowPC = "Probabilistic integer relation errors: " ++ (show $ length $ maybe [] M.toList intRelErrorP)
    intRelShowNPC = "Non-Probabilistic integer relation errors: " ++ (show $ length $ maybe [] M.toList intRelError)


    all =
      if Settings.pROBRULES
        then [typeShow, orderingShowP, intRelShowP, missingShowP]
        else [typeShow, orderingShow, intRelShow, missingShow]

    sizeErr = maybe 0 length
    typeSize = (maybe 0 M.size typeError)
    count = typeSize +
      (maybe 0 M.size orderingError) +
      (sizeErr missingError) +
      (maybe 0 M.size intRelError) +
      (sizeErr missingErrorP)
  in
    --if typeSize >0 then typeShow else filter (/="") $ concat all
    if typeSize >0 then typeShow else concat all

-- utility printing method for our probability tuples
showP (y, n) =
  let
    y' = fromIntegral y
    n' = fromIntegral n
    p = y' / (y' + n')
  in
    (show p) ++ " WITH COUNTS " ++ (show (y, n))
