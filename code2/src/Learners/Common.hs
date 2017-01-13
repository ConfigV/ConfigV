module Learners.Common where

-- I expect that the resolve stage of all the learners will have a lot of repitition
-- that can all go here

import qualified Data.Map as M
import Types.IR
import Types.Rules
import Types.Countable

import Debug.Trace

{-
showErr :: (Show k, Show v) => Maybe (M.Map k v) -> ((k,v) -> Error) -> [Error]
showErr rawEs printFxn =
  let
    es = maybe [] M.toList rawEs
  in
    map printFxn es
-}


--embedOnce :: Learnable a => [a] -> [(a,RuleData AntiRule)]
embedOnce = map (\r -> (r, (AntiRule 1 0 1)))

combine :: AntiRule -> AntiRule -> AntiRule
combine (AntiRule tru fls tot) (AntiRule truOp flsOp totOp)
  = AntiRule tru truOp (tot+totOp) 

traceMe x = trace (show x) x
