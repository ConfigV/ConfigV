
module LearningEngine where

import Types.IR
import Types.Rules
import Types.Common
import Types.Countable

import Learners
import Learners.KeywordCoor
import Convert

import Control.Parallel.Strategies
import Control.DeepSeq

import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M

import Debug.Trace

-- | collect contraints from each file indepentantly
-- this should be parmap
learnRules :: [ConfigFile Language] -> RuleSet
learnRules fs = let
  --fs' = parMap rseq convert fs
  --rs = parMap rdeepseq buildAllRelations fs'
  fs' = map convert fs
  --rs = parMap rseq buildAllRelations fs'
  keyCounts :: M.Map Keyword Int 
  keyCounts = foldl (\rs ir-> M.insertWith (+) (keyword ir) 1 rs) M.empty (concat fs')
  rs = map (buildAllRelations keyCounts) fs'
 in
  resolveRules rs `using` parRuleSet

-- | use the learning module instances to decide probabiliity cutoff and the sort
resolveRules :: [RuleSet] -> RuleSet
resolveRules rs = RuleSet
  { order   = f "o" order
  , missing = f "m" missing
  , intRel  = f "i" intRel
  , typeErr = f "t" typeErr
  , fineInt = f "f"fineInt
  }
 where
  f s classOfErr =  trace s $ merge (map classOfErr rs)

-- | call each of the learning modules
buildAllRelations :: M.Map Keyword Int -> IRConfigFile -> RuleSet
buildAllRelations ks f = RuleSet
  { order   = buildRelations f
  , missing = buildRelations' ks f 
  , intRel  = buildRelations f
  , typeErr = buildRelations f
  , fineInt = buildRelations f
  }


 
parRuleSet :: Strategy RuleSet
parRuleSet rs = do
  o' <- rpar $ force $order rs
  m' <- rpar $ force $missing rs
  i' <- rpar $ force $intRel rs
  t' <- rpar $ force $typeErr rs
  f' <- rpar $ force $fineInt rs
  let newRs = RuleSet { order = o', missing = m', intRel = i', typeErr = t', fineInt = f'}
  return newRs
