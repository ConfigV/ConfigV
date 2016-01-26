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
  learn f = pairs f

  check rs f = 
   let
     relevantRules = filter (hasRuleFor f) (rs)
     fRules = learn f
     diff = traceMe relevantRules L.\\ fRules --the difference between the two rule sets
     x = if null diff then Nothing else Just $ "Error in ordering on \n"++(show diff)
   in 
    x
  
  merge curr new = L.intersect curr new

-- | is the rule relevant to the file
--   ie have we seen the (keyword, value) pairing before
hasRuleFor :: [IRLine] -> OrdRule -> Bool
hasRuleFor ts r = 
  --this has problems if the (keyword, value) is repeated
  elem (fst r) ts && elem (snd r) ts

allLinePairs :: [IRLine]  -> [OrdRule]
allLinePairs [] = []
allLinePairs (l:ls) = map (l,) ls ++ allLinePairs ls

pairs :: [IRLine]  -> [(IRLine, IRLine)]
pairs [] = []
pairs (l:ls) = filter (\(l1,l2) -> (keyword l1/=keyword l2)) $ map (l,) ls ++ pairs ls

--traceMe x = traceShow x x
traceMe x = x
