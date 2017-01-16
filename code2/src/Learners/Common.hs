{-# LANGUAGE OverloadedStrings #-}
module Learners.Common where

-- I expect that the resolve stage of all the learners will have a lot of repitition
-- that can all go here

import qualified Data.Map as M
import Types.IR
import Types.Rules
import Types.Countable

import Debug.Trace

import qualified Data.Text as T
import qualified Data.Bits as B

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



pairs :: [IRLine]  -> [(IRLine,IRLine)]
pairs [] = []
pairs (l:ls) =
  let
    thisP = map (\x->(l,x)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf


getMod = fst. T.breakOn "_". keyword

-- does the pair contain just no socket/port, or both
sockport (ir1,ir2) = not $
 (T.isInfixOf "socket" (getMod ir1) ||
  T.isInfixOf "port"   (getMod ir1)) `B.xor`
 (T.isInfixOf "socket" (getMod ir2) ||
  T.isInfixOf "port"   (getMod ir2))

--according to Ennan modules do not interact with anything except themselves for Ordering
--so take the string before the first '_' as the module and only compare those that have equal modules
--if no modules, the keys are in the same module
--Are these lines in the same moudle
sameConfigModule :: (IRLine,IRLine) -> Bool
sameConfigModule (ir1,ir2) = let
  emptyMod x = (==) "" $ (snd$ T.breakOn "_"$ keyword x)
  sameMod = (emptyMod ir1 && emptyMod ir2) || (getMod ir1 == getMod ir2)
  --special case for socket and port
  --cant be the same module if only one is socket or port
 in
  (sockport (ir1,ir2)) && sameMod

