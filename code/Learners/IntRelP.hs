{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE MultiWayIf #-}

module Learners.IntRelP where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.Tuple

import Debug.Trace

-- I haven't found a nice package for SMTLIB format in haskell yet, its out there tho, im sure

instance Attribute (M.Map (Keyword,Keyword)) FormulaC where
  -- | this has the problem that order is important
  --   (foo,bar,==) is different than (bar,foo,==)
  learn ts =
    let
      ts' = pairs $ filter couldBeInt ts
      rules = foldr (\p rs -> findeqRules p ++ rs) [] ts'
    in
      M.fromList rules

  check rs f =
    let
     emptyFC (FormulaC g l e) = g == 0 && l == 0 && e == 0
     relevantRules' = M.filterWithKey (\k v-> (not $ emptyFC v) && hasRuleFor f k) (rs)
     relevantRules = M.foldrWithKey makeFlips relevantRules' relevantRules' -- why are we doing "makeFlips" here?
     fRules' = learn f :: IntRelMap
     fRules = M.foldrWithKey makeFlips fRules' fRules' -- why do we do "makeFlips" here?
     diff = M.differenceWith compRules relevantRules fRules --diff is the rules we should have met, but didnt
    in
     if M.null diff then Nothing else Just diff


  -- merge curr new = foldl removeConflicts [] $ L.union curr (traceShow (length curr) new)
  merge curr new =
    let
      u = M.unionWith removeConflicts curr new
    in u

compRules :: FormulaC -> FormulaC -> Maybe FormulaC
compRules f1 f2 =
  if | (f1==f2) -> Nothing
     | ((f1==Just (==)) && (f2==Just(<=))) || ((f1==Just(<=)) && (f2==Just(==))) -> Nothing
     | ((f1==Just (==)) && (f2==Just(>=))) || ((f1==Just(>=)) && (f2==Just(==))) -> Nothing
     | True -> Just f1

foo s m = trace (s++(show (M.lookup (("port[client]","port[mysqld]")) m))) m
--foo m = traceShow (M.lookup (swap ("max_allowed_packet[wampmysqld]","key_buffer[wampmysqld]")) m) m
--if ((snd r1) == "max_allowed_packet[wampmysqld]") && ((fst r1) == "key_buffer[wampmysqld]") then (trace ((show r1)++(show f)) f) else f
--traceMe x = (
makeFlips :: (Keyword,Keyword) -> FormulaC -> IntRelMap -> IntRelMap
makeFlips (k1,k2) (FormulaC l g e) old = let
    f' = FormulaC {
      lt = g,
      gt = l,
      eq = e
    }
  in
    M.insert (k2,k1) f' old

hasRuleFor :: [IRLine] -> (Keyword,Keyword) ->  Bool
hasRuleFor ls (l1,l2) =
  elem l1 (map keyword ls) && elem l2 (map keyword ls)

removeConflicts :: FormulaC -> FormulaC -> FormulaC
removeConflicts f1 f2 =
  if | (f1==f2) -> f1
     | ((f1==Just (==)) && (f2==Just(<=))) || ((f1==Just(<=)) && (f2==Just(==))) -> Just (<=)
     | ((f1==Just (==)) && (f2==Just(>=))) || ((f1==Just(>=)) && (f2==Just(==))) -> Just (>=)
     | True -> Nothing
-- | true for rules with mathcing keys, but diff formulas
{-sameKeyRel :: ((Keyword,Keyword), Formula) -> (Keyword,Keyword) -> Formula -> Bool
sameKeyRel (r1, f) r2 f' =
   id (
    (fst r1) == (fst r2) && (snd r1) == (snd r2) && ((foo r1 f) /= (foo r2 f'))
   )
-}

findeqRules :: (IRLine,IRLine) -> [((Keyword,Keyword),FormulaC)]
findeqRules (l1,l2) =
  let
    -- string to int convertsion taking into account some ints will have 'M' attached at the end
    getI l =
      if (T.last$value l) == 'M' then (read. T.unpack. T.init. value) l else (read. T.unpack. value) l
    i1 = getI l1 :: Int
    i2 = getI l2 :: Int
    -- determine whether a number marks data size or just an int
    bothSame = (isSize (value l1) && isSize (value l2)) || ((isInt (value l1)) && (isInt (value l2)))
    makeR f =
      if bothSame && f i1 i2
      then
        case f of
          (<=) -> [((keyword l1, keyword l2), FormulaC { lt = 1, gt = 0, eq = 0})]
          (>=) -> [((keyword l1, keyword l2), FormulaC { lt = 0, gt = 1, eq = 0})]
          (<=) -> [((keyword l1, keyword l2), FormulaC { lt = 0, gt = 0, eq = 1})]
          _    -> [((keyword l1, keyword l2), FormulaC { lt = 0, gt = 0, eq = 0})]
      else []
    all = makeR (==) ++ makeR (<=) ++ makeR (>=)
  in
    if | length all == 1 -> all
       | not bothSame -> []
       | length all /= 1 && bothSame -> makeR (==)

-- only for comparators! careful!
instance Eq (Int -> Int -> Bool) where
  (==) f1 f2 =
    (f1 1 1) == (f2 1 1) &&
    (f1 0 1) == (f2 0 1) &&
    (f1 1 0) == (f2 1 0)

couldBeInt :: IRLine -> Bool
couldBeInt IRLine{..} =
  isInt value || isSize value
--  (T.all isDigit value && (T.length value >0)) ||
--  ((T.length value >1) && T.all isDigit (T.init value)  && T.last value == 'M')

isInt v =
  (T.all isDigit v && (T.length v >0))
isSize v =
  ((T.length  v >1) && T.all isDigit (T.init v)  && (T.last v) == 'M')

pairs :: [IRLine]  -> [(IRLine,IRLine)]
pairs [] = []
pairs (l:ls) =
  filter (\(l1,l2) -> ((isSize $value l1)&&(isSize $value l2)) || ((isInt $value l1)&&(isInt $ value l2)) && (keyword l1/=keyword l2)) $ map (l,) ls ++ pairs ls


--L.nubBy modOrder $ filter (\(l1,l2) -> (keyword l1/=keyword l2)) $ map (l,) ls ++ pairs ls
-- we dont need foo,bar,<= and bar,foo,>=
modOrder (r11,r12) (r21,r22) =
  (keyword r11 == keyword r22) &&
  (keyword r12 == keyword r21)
