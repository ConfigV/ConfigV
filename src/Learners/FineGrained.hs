{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Learners.FineGrained where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable
import qualified Types.Rules as R
import Learners.Common

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Bits as B
import           Data.Maybe 
import Data.Interned

import Settings.Config
import Control.Monad.Reader


import Control.Parallel
import Utils

-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable R.FineGrained Formula where

  buildRelations f = do
    settings <- ask
    let
      rs = mapMaybe intLike f
      --dont want anything with these
      xs = ["dir","socket","port"]
      tris = triples $ filter (\ir -> not $ any (\k -> T.isInfixOf k (unintern $ keyword ir)) xs) rs
      -- TODO should use learning result from TypeErr module
      wellTypedTris = 
          filter 
          (\(ir1,ir2,ir3) -> 
             ((validAsSize $ unintern $ value ir1) `B.xor` 
              (validAsSize $ unintern $ value ir2) && 
              (validAsSize $ unintern $ value ir3)) ||
             all (validAsInt.unintern.value) [ir1,ir2,ir3]) 
          tris
      eqs = map toFineGrained $ if (enableProbTypeInference $ optionsSettings settings) then wellTypedTris else traceMe tris
    return $ M.fromList $ eqs

  merge rs = do
    settings <- ask
    let
      rs' = M.unionsWith add rs
      --merged = M.mapKeysWith add (\fg@(FineGrained k1 k2 k3)-> if k2 > k1 then (FineGrained k2 k1 k3) else fg) rs'
      validRule r = (gt r + lt r + eq r)>(fineGrainSupport $ thresholdSettings settings) &&  
                    ((gt r <= (fineGrainConfidence $ thresholdSettings settings)) || 
                     (lt r <= (fineGrainConfidence $ thresholdSettings settings)))
      merged = M.foldlWithKey combineFlips M.empty rs'
    return $ M.filter validRule merged
 
  check _ r1 r2 = if
    | eq r2 == 1 && (lt r1 > 3 || gt r1 > 3) && eq r1 < 3 -> Just r1
    {- | lt r2 == 1 && gt r1 > Settings.fineGrainSupport && lt r1 <= Settings.fineGrainConfidence -> Just r1
    | gt r2 == 1 && lt r1 > Settings.fineGrainSupport && gt r1 <= Settings.fineGrainConfidence-> Just r1 -}
    | otherwise -> Nothing

  toError ir fname ((FineGrained k1 k2 k3), rd) = Error{
     errLocs = map (\x->(fname, x)) [k1, k2, k3]
    ,errIdent = FINEGRAINED
    ,errMsg = "FINE GRAINED ERROR: Expected "++(show k1)++" * "++(show k2)++(show rd)++(show k3)++" \n Found values: "
              ++(show $ map (\x-> (show $ keyword x)++"="++(show $ value x)) $ filter (\x->keyword x==k1 || keyword x==k2 || keyword x==k3) ir)
    ,errSupport = gt rd + lt rd + eq rd}

{-
TODO this doesnt help at all, whyyyy 
paralleize the merge?a-}
pfold full  = let
  ms' = M.splitRoot full
  ms = map pfold ms'
  mappend m1 m2 = M.foldlWithKey combineFlips m1 m2
 in
  if M.size full < 50 
  then M.empty `mappend` full
  else (head ms `par` last ms) `pseq` (foldl1 mappend ms) 

--all IRLines must have only ints as values (filtered out by this point)
toFineGrained :: (IRLine,IRLine,IRLine) -> (FineGrained,Formula)
toFineGrained (IRLine k1 v1, IRLine k2 v2, IRLine k3 v3) = let
    asInt v = ((either (\x->0) fst $T.decimal v) * (fromMaybe 1 $ units v)) ::Int
    formula v1 v2 v3 = if
     | v1*v2 < v3  -> Formula {gt=0,eq=0,lt=1}
     | v1*v2 == v3 -> Formula {gt=0,eq=1,lt=0}
     | v1*v2 > v3  -> Formula {gt=1,eq=0,lt=0}
  in
    ((FineGrained k1 k2 k3), formula (asInt $ unintern v1) (asInt $ unintern v2) (asInt $ unintern v3))

-- | Combine k1 k2 k3 with k2 k1 k3 rules
-- unionsWith work by Ord, so just providing a custom instance of Eq wont work, also need Ord
-- ord is too sensitive, since traversal might miss an EQ
-- instead just rebuild the whole map with combineFlips (only happens once so shouldnt be too bad
combineFlips :: RuleDataMap FineGrained Formula -> FineGrained -> Formula -> RuleDataMap FineGrained Formula
combineFlips old (FineGrained k1 k2 k3) v = if 
  -- if we already have a rule in this form, we increment the count
  | M.member (FineGrained k1 k2 k3) old -> M.adjust (add v) (FineGrained k1 k2 k3) old
  -- if we are already counting this rule as k1 k2, any time we see k2 k1, we can throw it away
  -- since we are guaranteed to add the k1 k2 version
  | M.member (FineGrained k2 k1 k3) old -> old
  | otherwise -> M.insert (FineGrained k1 k2 k3) v old

-- Aaron's implementation
triples :: [IRLine] -> [(IRLine, IRLine, IRLine)]
triples xs = concat $ map tripleWithElem xs
  where
    -- we need a guarantee that x /= y /= z
    -- we also need a guarantee that for all x, y, z, iff (x, y, z) is in there (y, x, z) is not.
    --   (The things that are not z do not matter in order.)
    --   (But order does matter somewhat, since it matters which of the three is the z.)
    --   (Hence, for any collection of {a, b, c}, there are three possible triples: one where a is the z, b is the z, c is the z.)
    --   (That bit is why it's so hard to write a generalized combination :: Int -> [IRLine] -> [[IRLine]] function.)
    -- sadly, I don't think this is super efficient :(
    tripleWithElem z = -- this function
      map (\(x, y) -> (x, y, z)) $ pairs $ filter (\x -> keyword x /= keyword z) xs
