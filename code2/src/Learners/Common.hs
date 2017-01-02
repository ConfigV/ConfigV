module Learners.Common where

-- I expect that the resolve stage of all the learners will have a lot of repitition
-- that can all go here

import qualified Data.Map as M
import Types.IR
import Types.Rules

{-
showErr :: (Show k, Show v) => Maybe (M.Map k v) -> ((k,v) -> Error) -> [Error]
showErr rawEs printFxn =
  let
    es = maybe [] M.toList rawEs
  in
    map printFxn es
-}

embedOnce :: Learnable a => [a] -> [(a,RuleData)]
embedOnce = map (\r -> (r, RuleData 1 0 1 False))

