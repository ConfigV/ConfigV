module Learners.Common where

-- I expect that the resolve stage of all the learners will have a lot of repitition
-- that can all go here
import Types.Error

import qualified Data.Map as M

showErr :: (Show k, Show v) => Maybe (M.Map k v) -> ((k,v) -> Error) -> [Error]
showErr rawEs printFxn =
  let
    es = maybe [] M.toList rawEs
  in
    map printFxn es
