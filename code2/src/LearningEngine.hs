
module LearningEngine where

import Types.IR
import Types.Rules
import Types.Common


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
  --rs = parMap rdeepseq findAllRules fs'
  fs' = map convert fs
  rs = map findAllRules fs'
 in
  mergeRules rs

-- | call each of the learning modules
findAllRules :: IRConfigFile -> RuleSet
findAllRules f = RuleSet
  { order   = buildRelations f
  , missing = buildRelations f 
  , intRel = buildRelations f
  --, typeErr = learn f
  }

-- | reconcile all the information we have learned
-- later, think about merging this step with findAllRules
-- no more parallel, but might be faster
mergeRules :: [RuleSet] -> RuleSet
mergeRules rs =
  RuleSet
    { order  = f order 
    , missing = f missing
    , intRel = f intRel 
    --, typeErr= merge (typeErr rs) (typeErr rs')
    }
 where
  f x = M.unionsWith combineRuleData (map x rs)
  combineRuleData :: RuleData -> RuleData -> RuleData
  combineRuleData (x,y,z) (x',y',z') = (x+x',y+y',z+z') 

