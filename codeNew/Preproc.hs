module Preproc where

import Types

import Learners
import Convert

import qualified Data.Text.IO as T

-- | learn rules based on a set of files
preproc :: [ConfigFile Language] -> RuleSet
preproc cs = let
  ds = map convert cs 
  rules = learnRules ds
 in
  rules

-- | call each of the learning modules
findAllRules :: IRConfigFile -> RuleSet
findAllRules f = RuleSet
  { order   = learn f
  , intRel = learn f}

-- | collect contraints from each file indepentantly
-- this should be parmap
learnRules :: [IRConfigFile] -> RuleSet
learnRules cs = let
  rs = map findAllRules cs 
 in
  foldl1 mergeRules rs

-- | reconcile all the information we have learned
-- later, think about merging this step with findAllRules
-- no more parallel, but might be faster
mergeRules :: RuleSet -> RuleSet -> RuleSet
mergeRules rs rs' = RuleSet
  { order  = merge (order rs) (order rs')
  , intRel = merge (intRel rs) (intRel rs')}

