{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE PartialTypeSignatures #-} 

module ConfigV.Learners.KeywordCoor where

import Types.IR
import Types.Common
import Types.Errors
import Types.Rules 
import Types.Countable

import qualified Types.Rules as R

import qualified Data.Map as M

import Learners.Common

import Settings.Config
import Control.Monad.Reader
import Debug.Trace

-- | these are directed coorelations, so we can learn Y=>X, but this does not necessarily mean X=>Y
instance Learnable R.KeywordCoor AntiRule where

  buildRelations f = let
    toKC (ir1,ir2) = 
      KeywordCoor (keyword ir1, keyword ir2) 
    irPairs = pairs f
    totalTimes = M.fromList $ embedAsTrueAntiRule $ map toKC irPairs 
   in
    return totalTimes

  merge rs = do
    settings <- ask
    let 
      minTrue = keywordCoorSupport $ thresholdSettings settings
      maxFalse = keywordCoorConfidence $ thresholdSettings settings
      rsAdded = M.unionsWith add rs
      rsWithFalse = M.mapWithKey (addFalse rs) rsAdded
    return $ filterByThresholds minTrue maxFalse rsWithFalse

  --   should we report the relation r2 found in the target file
  --   as in conflict with the learned rule r1
  check _ rd1 rd2 = let
     agrees r1 r2 = 
       if tru r2 ==1
       then tru r1 > fls r1
       else True--fls r1 > tru r1
   in
     if (not $agrees rd1 rd2)
     then Just rd1
     else Nothing

  toError ir fname ((KeywordCoor (k1,k2)),rd) = Error{
     errLocs= [(fname,k1),(fname,k2)]
    ,errIdent = MISSING
    ,errMsg = "MISSING ERROR: Expected "++(show k1)++" WITH "++(show k2) ++ " CONF. = " ++ (show rd)
    ,errSupport = tru rd + fls rd}

-- false is equal to how many times we saw k1 but not k2
addFalse:: [M.Map KeywordCoor AntiRule] -> KeywordCoor -> AntiRule -> AntiRule
addFalse allRules (KeywordCoor (k1,k2)) rd = let
    fCount = 
      sum $ map (\r -> fromEnum $
                  (not $ M.member (KeywordCoor (k1,k2)) r) &&
                  (not $ M.null $ M.filterWithKey (\(KeywordCoor (k1',k2')) v-> k1==k1' && k2/=k2') r))
                allRules
 in
  rd{tot=fCount+(tru rd),fls= fCount}
  
