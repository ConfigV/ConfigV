module Preproc where

import Lexical
import Values
import Order
import Types

import qualified Data.Text.IO as T

-- | learn rules based on a set of files
preproc :: [ConfigFile Language] -> RuleSet
preproc cs = let
  ds = map convert cs 
  rules = learnOn ds
 in
  rules


-- | call each of the learning modules
genConstraints :: ConfigFile Common -> RuleSet
genConstraints f = RuleSet
  { lexical = learn f
  , order   = learn f
  , value   = learn f}

-- | collect contraints from each file indepentantly
-- this should be parmap
learnOn :: [ConfigFile Common] -> RuleSet
learnOn cs = let
  rs = map genConstraints cs 
 in
  foldl1 mergeRules rs

-- | reconcile all the information we have learned
-- later, think about merging this step with genConstraints
-- no more parallel, but might be faster
mergeRules :: RuleSet -> RuleSet -> RuleSet
mergeRules rs rs' = RuleSet
  { lexical = merge (lexical rs) (lexical rs')
  , order  = merge (order rs) (order rs')
  , value   = merge (value rs)  (value rs')}

