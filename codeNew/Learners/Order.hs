{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Order where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L

import Debug.Trace


-- this all needs to be somehow translated from set operations on lists of pairs to SMT formulas
-- I haven't found a nice package for SMTLIB format in haskell yet, might need to build it from scratch
instance Attribute OrdRule where
  learn (t,c) = allLinePairs $ T.lines t

  check rs f = 
   let
     fileAsLines = T.lines (fst f)
     diff = (filter (hasRuleFor fileAsLines) rs)  L.\\ learn f --the difference between the two rule sets
     x = if diff == (learn f) then True else trace (show diff) False
   in 
    x
  
  merge curr new = L.intersect curr new


hasRuleFor :: [T.Text] -> OrdRule -> Bool
hasRuleFor ts r = 
  elem (snd r) ts && elem (fst r) ts

allLinePairs :: [T.Text]  -> [OrdRule]
allLinePairs [] = []
allLinePairs (l:ls) = map (l,) ls ++ allLinePairs ls
