
module Preproc where

import Types

import Learners
import Convert

import Control.Parallel.Strategies

import qualified Data.Text.IO as T

-- | collect contraints from each file indepentantly
-- this should be parmap
learnRules :: [ConfigFile Language] -> RuleSet
learnRules fs = let
  fs' = parMap rseq convert fs
  rs = parMap rdeepseq findAllRules fs'
 in
  foldl1 mergeRules rs

-- | call each of the learning modules
findAllRules :: IRConfigFile -> RuleSet
findAllRules f = RuleSet
  { order   = learn f
  , intRel = learn f
  , missing = learn f
  , typeErr = learn f
  }

-- | reconcile all the information we have learned
-- later, think about merging this step with findAllRules
-- no more parallel, but might be faster
mergeRules :: RuleSet -> RuleSet -> RuleSet
mergeRules rs rs' = RuleSet
  { order  = merge (order rs) (order rs')
  , intRel = merge (intRel rs) (intRel rs')
  , missing = merge (missing rs) (missing rs')
  , typeErr= merge (typeErr rs) (typeErr rs')
  }

