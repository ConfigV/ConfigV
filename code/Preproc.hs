
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
  --fs' = parMap rseq convert fs
  --rs = parMap rdeepseq findAllRules fs'
  fs' = map convert fs
  rs = map findAllRules fs'
 in
  foldl1 mergeRules rs

-- | call each of the learning modules
findAllRules :: IRConfigFile -> RuleSet
findAllRules f = RuleSet
  { order   = learn f
  , missing = learn f
  , typeErr = learn f
  , missingP = learn f
  , orderP = learn f
  , intRelP = learn f
  }

-- | reconcile all the information we have learned
-- later, think about merging this step with findAllRules
-- no more parallel, but might be faster
mergeRules :: RuleSet -> RuleSet -> RuleSet
mergeRules rs rs' = RuleSet
  { order  = merge (order rs) (order rs')
  , missing = merge (missing rs) (missing rs')
  , typeErr= merge (typeErr rs) (typeErr rs')
  , missingP = merge (missingP rs) (missingP rs')
  , orderP = merge (orderP rs) (orderP rs')
  , intRelP = merge (intRelP rs) (intRelP rs')
  }
