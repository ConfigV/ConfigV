{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonoPatBinds #-}

module LearningEngine where

import Types.IR
import Types.Rules
import Types.Common

import Learners()
import qualified Learners.KeywordCoor as K
import Convert

import Control.Parallel.Strategies
import Control.DeepSeq

import qualified Data.Map.Strict as M

import Settings.Config
import Control.Monad.Reader

import Debug.Trace

-- | collect contraints from each file indepentantly
--   this should be parmap
learnRules :: ConfigVConfiguration -> [ConfigFile Language] -> RuleSet
learnRules configVconfig fs = let
  configLines = map convert (take (learnFileLimit $ optionsSettings configVconfig) fs)
  keyCounts :: M.Map Keyword Int 
  keyCounts = foldl (\rs ir-> M.insertWith (+) (keyword ir) 1 rs) M.empty $ concat configLines
  rs = map (buildAllRelations configVconfig keyCounts) configLines
 in
  resolveRules configVconfig rs --`using` parRuleSet

-- | use the learning module instances to decide probabiliity cutoff and the sort
resolveRules :: ConfigVConfiguration -> [RuleSet] -> RuleSet
resolveRules configVconfig rs = RuleSet
  { order     = applyThresholds "order" order
  , missing   = applyThresholds "missing" missing
  , keyvalkey = applyThresholds "keyvalkey" keyvalkey
  , intRel    = applyThresholds "coarse grain" intRel
  , typeErr   = applyThresholds "type" typeErr
  , fineInt   = applyThresholds "fine grain" fineInt
  }
 where
  applyThresholds templateName classOfErr =
     trace ("resolving rules for "++templateName) $ runReader (merge $! (map classOfErr rs)) configVconfig

-- | call each of the learning modules
buildAllRelations :: ConfigVConfiguration -> M.Map Keyword Int -> IRConfigFile -> RuleSet
buildAllRelations configVconfig ks f = let
  getRuleOpt sel = sel $ optionsSettings configVconfig
  getRules :: Learnable a b => (Options -> Bool) -> RuleDataMap a b
  getRules sel = if getRuleOpt sel 
                 then runReader (buildRelations f) configVconfig 
                 else emptyRuleMap
 in
  RuleSet
    { order     = getRules enableOrder
    , keyvalkey = getRules enableKeyvalkey
    , intRel    = getRules enableCoarseGrain
    , fineInt   = getRules enableFineGrain
    , typeErr   = getRules enableTypeRules
    , missing   = getRules enableMissing  
    }

mapOverRuleSet :: (forall a. a -> a) -> RuleSet -> RuleSet
mapOverRuleSet f r = 
  r {
      order = f (order r)
    , keyvalkey = f (keyvalkey r)
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
