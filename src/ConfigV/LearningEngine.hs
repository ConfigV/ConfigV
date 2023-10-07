{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module ConfigV.LearningEngine where

import ConfigV.Types

import ConfigV.Learners()
import ConfigV.Convert

import Control.Parallel.Strategies
import Control.DeepSeq

import ConfigV.Settings.Config
import Control.Monad.Reader

import Debug.Trace

-- | collect contraints from each file indepentantly
--   this should be parmap
learnRules :: ConfigVConfiguration -> [ConfigFile Language] -> RuleSet
learnRules configVconfig fs = let
  configLines = map convert (take (learnFileLimit $ optionsSettings configVconfig) fs)
  rs = map (buildAllRelations configVconfig) configLines
 in
  resolveRules configVconfig rs --`using` parRuleSet

-- | use the learning module ConfigV.instances to decide probabiliity cutoff and the sort
resolveRules :: ConfigVConfiguration -> [RuleSet] -> RuleSet
resolveRules configVconfig rs = RuleSet
  { order     = applyThresholds "order" order
  , intRel    = applyThresholds "coarse grain" intRel
  , typeErr   = applyThresholds "type" typeErr
  , fineInt   = applyThresholds "fine grain" fineInt
  , smtRules  = applyThresholds "smtRule" smtRules
  }
 where
  applyThresholds templateName classOfErr =
     trace ("resolving rules for "++templateName) $ runReader (merge $! (map classOfErr rs)) configVconfig

-- | call each of the learning modules
buildAllRelations :: ConfigVConfiguration -> IRConfigFile -> RuleSet
buildAllRelations configVconfig f = let
  getRuleOpt sel = sel $ optionsSettings configVconfig
  getRules :: Learnable a b => (Options -> Bool) -> RuleDataMap a b
  getRules sel = if getRuleOpt sel 
                 then runReader (buildRelations f) configVconfig 
                 else emptyRuleMap
 in
  RuleSet
    { order     = getRules enableOrder
    , intRel    = getRules enableCoarseGrain
    , fineInt   = getRules enableFineGrain
    , typeErr   = getRules enableTypeRules
    , smtRules  = getRules enableSMT
    }

mapOverRuleSet :: (forall a. a -> a) -> RuleSet -> RuleSet
mapOverRuleSet f r = 
  r {
      order = f (order r)
    }

parRuleSet :: Strategy RuleSet
parRuleSet rs = do
  o' <- rpar $ force $order rs
  i' <- rpar $ force $intRel rs
  t' <- rpar $ force $typeErr rs
  f' <- rpar $ force $fineInt rs
  let newRs = RuleSet { order = o', intRel = i', typeErr = t', fineInt = f'}
  return newRs
