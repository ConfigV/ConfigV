{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Learners.Common where

-- I expect that the resolve stage of all the learners will have a lot of repitition
-- that can all go here

import Debug.Trace

import qualified Data.Bits as B
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Maybe

import Types.IR
import Types.Rules
import Types.Countable

{-
showErr :: (Show k, Show v) => Maybe (M.Map k v) -> ((k,v) -> Error) -> [Error]
showErr rawEs printFxn =
  let
    es = maybe [] M.toList rawEs
  in
    map printFxn es
-}

--embedOnce :: Learnable a => [a] -> [(a,RuleData AntiRule)]
embedOnce = map (\r -> (r, (AntiRule {tru=1, fls=0, tot=1})))

combine :: AntiRule -> AntiRule -> AntiRule
combine (AntiRule tru fls tot) (AntiRule truOp flsOp totOp)
  = AntiRule tru truOp (tot+totOp) 

traceMe x = trace (show x) x

pairs :: [IRLine]  -> [(IRLine,IRLine)]
pairs [] = []
pairs (l:ls) =
  let
    thisP = map (\x->(l,x)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf

-- For weeding out IRLine with values that do not resemble integer formats (IntRel and FineGrained)
-- TODO use validAsInt and validAsSize
intLike :: IRLine -> Maybe IRLine
intLike (IRLine k v) = let
  hasInt v = (all C.isNumber$ T.unpack v) || ((isJust $ units v) && (any C.isNumber (T.unpack v)))
 in
  if hasInt v && (v/="")
  then Just $ IRLine k v
  else Nothing

-- For converting values in the form 100M to an actual Int with proper scaling (IntRel and FineGrained)
units v = if
  | T.length v == 0 -> Nothing
  | all C.isNumber (T.unpack $ T.init v) -> Just $ scale $ T.last v
  | otherwise -> Nothing
scale c = if
  | C.toUpper c == 'K' -> 1 
  | C.toUpper c == 'M' -> 1000
  | C.toUpper c == 'G' -> 1000000
  | otherwise -> 1 

emptyMod ir= (==) "" $ (snd$ T.breakOn c$ keyword ir)
  where
    c = if T.isInfixOf "_" $ keyword ir then "_" else "-"
getMod ir = fst$ T.breakOn c $ keyword ir
  where
    c = if T.isInfixOf "_" $ keyword ir then "_" else "-"
getContext = snd. T.breakOn "[". keyword

-- does the pair contain just no socket/port, or both
sockport (ir1,ir2) = not $
 (T.isInfixOf "socket" (getMod ir1) ||
  T.isInfixOf "port"   (getMod ir1)) `B.xor`
 (T.isInfixOf "socket" (getMod ir2) ||
  T.isInfixOf "port"   (getMod ir2))

--according to Ennan modules do not interact with anything except themselves for Ordering
--so take the string before the first '_' as the module and only compare those that have equal modules
--if no modules, the keys are in the same module
--Additoianlly restict the context, eg [mysql], to be the same
--Are these lines in the same moudle
sameConfigModule :: (IRLine,IRLine) -> Bool
sameConfigModule (ir1,ir2) = let
  sameMod = ((emptyMod ir1 && emptyMod ir2) || (getMod ir1 == getMod ir2)) && (getContext ir1 == getContext ir2)
  --special case for socket and port
  --cant be the same module if only one is socket or port
 in
  (sockport (ir1,ir2)) && sameMod


--TYPES
validAsString v = (((T.length $ T.takeWhile C.isAlpha v) > 1) || (T.length v>=2 && T.head v == '"' && T.last v == '"')) || mysqlvar v
validAsPath v  = ((T.isInfixOf "/" v) || (T.isInfixOf "." v)) || mysqlvar v
validAsBool v = (any (==v) ["","on","off","ON","OFF","0","1"]) || mysqlvar v--flag keywords have no values 
validAsInt v = ((all C.isNumber $T.unpack $T.filter (/='.') v)&& (T.length v>0)) && (T.length v <=1+(T.length $ T.filter (/='.') v))|| mysqlvar v
validAsSize v = ((or $ map (\x-> T.isSuffixOf x v) ["G","g","M","m","K","k"]) &&
                 (T.length $ T.takeWhile C.isNumber v) == (T.length v -1) ) || mysqlvar v
--a var can have any type
mysqlvar v = T.isInfixOf "{{" v
