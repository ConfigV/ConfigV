module LearningEngine where

import Types.IR
import Types.Rules
import Types.Common
import Types.Countable

import Learners
import qualified Learners.KeywordCoor as K
import qualified Learners.KeyValKeyCoor as KV
import Convert

import Control.Parallel.Strategies
import Control.DeepSeq

import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M

import Settings
import Utils

import Debug.Trace

-- | collect contraints from each file indepentantly
-- this should be parmap
learnRules :: [ConfigFile Language] -> RuleSet
learnRules fs = let
  --fs' = parMap rseq convert fs
  --rs = parMap rdeepseq buildAllRelations fs'
  configLines = map convert (take Settings.learnFileLimit fs)
  --rs = parMap rseq buildAllRelations fs'
  keyCounts :: M.Map Keyword Int 
  keyCounts = foldl (\rs ir-> M.insertWith (+) (keyword ir) 1 rs) M.empty $ concat configLines
  rs = map (buildAllRelations keyCounts) configLines
 in
  resolveRules rs --`using` parRuleSet

-- | use the learning module instances to decide probabiliity cutoff and the sort
resolveRules :: [RuleSet] -> RuleSet
resolveRules rs = RuleSet
  { order     = applyThresholds "order" order
  , missing   = applyThresholds "missing" missing
  , keyvalkey = applyThresholds "keyvalkey" keyvalkey
  , intRel    = applyThresholds "coarse grain" intRel
  , typeErr   = applyThresholds "type" typeErr
  , fineInt   = applyThresholds "fine grain" fineInt
  }
 where
  applyThresholds templateName classOfErr =  trace ("resolving rules for "++templateName) $ merge (map classOfErr rs)

-- | call each of the learning modules
buildAllRelations :: M.Map Keyword Int -> IRConfigFile -> RuleSet
buildAllRelations ks f = RuleSet
  { order     = if Settings.enableOrder then buildRelations f else emptyRuleMap
  , missing   = if Settings.enableMissing then K.buildRelations' ks f else emptyRuleMap
  , keyvalkey = if Settings.enableKeyvalkey then KV.buildRelations' ks f else emptyRuleMap
  , intRel    = if Settings.enableCoarseGrain then buildRelations f else emptyRuleMap
  , fineInt   = if Settings.enableFineGrain then buildRelations f else emptyRuleMap
  , typeErr   = if Settings.enableTypesRules then buildRelations f else emptyRuleMap
  }

parRuleSet :: Strategy RuleSet
parRuleSet rs = do
  o' <- rpar $ force $order rs
  m' <- rpar $ force $missing rs
  k' <- rpar $ force $keyvalkey rs
  i' <- rpar $ force $intRel rs
  t' <- rpar $ force $typeErr rs
  f' <- rpar $ force $fineInt rs
  let newRs = RuleSet { order = o', missing = m', intRel = i', typeErr = t', fineInt = f', keyvalkey = k'}
  return newRs
