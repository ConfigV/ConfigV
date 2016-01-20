{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Order where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Map as M

import Debug.Trace


instance Attribute OrdRule where
  learn f = allLinePairs f

  check rs f = 
   let
     relevantRules = filter (hasRuleFor f) rs
     fRules = learn f
     diff = relevantRules L.\\ fRules --the difference between the two rule sets
     x = if null diff then Nothing else Just $ "Error in ordering on "++(show diff)
   in 
    x
  
  merge curr new = L.intersect curr new

-- | is the rule relevant to the file
--   ie have we seen the (keyword, value) pairing before
hasRuleFor :: [IRLine] -> OrdRule -> Bool
hasRuleFor ts r = 
  elem (fst r) ts && elem (snd r) ts

allLinePairs :: [IRLine]  -> [OrdRule]
allLinePairs [] = []
allLinePairs (l:ls) = map (l,) ls ++ allLinePairs ls

traceMe x = traceShow x x
