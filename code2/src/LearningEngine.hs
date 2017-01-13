
module LearningEngine where

import Types.IR
import Types.Rules
import Types.Common
import Types.Countable

import Learners
import Convert

import Control.Parallel.Strategies

import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M

-- | collect contraints from each file indepentantly
-- this should be parmap
learnRules :: [ConfigFile Language] -> RuleSet
learnRules fs = let
  --fs' = parMap rseq convert fs
  --rs = parMap rdeepseq buildAllRelations fs'
  fs' = map convert fs
  rs = map buildAllRelations fs'
 in
  resolveRules rs

-- | use the learning module instances to decide probabiliity cutoff and the sort
resolveRules :: [RuleSet] -> RuleSet
resolveRules rs = RuleSet
  { order   = f order
  , missing = f missing
  , intRel  = f intRel
  , typeErr = f typeErr
  }
 where
  f classOfErr = merge $ map classOfErr rs 

-- | call each of the learning modules
buildAllRelations :: IRConfigFile -> RuleSet
buildAllRelations f = RuleSet
  { order   = buildRelations f
  , missing = buildRelations f 
  , intRel  = buildRelations f
  , typeErr = buildRelations f
  }

