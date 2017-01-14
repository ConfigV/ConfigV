{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 

module Learners.KeywordCoor where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable

import qualified Types.Rules as R

import qualified Data.Map as M
import           System.Directory

import Learners.Common

import Debug.Trace

instance Learnable R.KeywordCoor AntiRule where

  buildRelations f = let
    toKC (ir1,ir2) = KeywordCoor (keyword ir1, keyword ir2) 
    irPairs = pairs f
   in
     M.fromList $ embedOnce $ map toKC irPairs 
  
  merge rs = let 
      addEvi k rd rs = if 
          | M.member k rs || M.member (flipped k) rs -> addT rd
          | hasKey k rs -> addF rd
          | otherwise -> rd
      updateExisting newRs sumRs = M.mapWithKey (\k rd -> addEvi k rd newRs) sumRs

      -- if the rule does not have an entry already
      -- add it
      addNewR oldRs rs k rd  = 
        if not $ hasKey k oldRs
        then M.insert k rd rs
        else rs
      addNewRs newRs sumRs = M.foldlWithKey (addNewR sumRs) sumRs newRs

      rsUpdated = foldl (\sumRs newRs -> addNewRs newRs $ updateExisting newRs sumRs) M.empty rs
      
      validRule r = (tru r)>=6 && (fls r)<=1
    in
      M.filter validRule rsUpdated
      --rsUpdated


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

  toError fname ((KeywordCoor (k1,k2)),rd) = Error{
     errLocs= [(fname,k1),(fname,k2)]
    ,errIdent = MISSING
    ,errMsg = "KEYWORD ERROR: Expected "++(show k1)++" WITH "++(show k2) ++ " CONF. = " ++ (show rd)}

pairs :: [IRLine]  -> [(IRLine,IRLine)]
pairs [] = []
pairs (l:ls) =
  let
    thisP = map (\x->(l,x)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf

addT :: AntiRule -> AntiRule
addT (AntiRule t f tot) =
  AntiRule (t+1) f (tot+1)
addF :: AntiRule -> AntiRule
addF (AntiRule t f tot) =
  AntiRule t (f+1) (tot+1)

hasKey :: KeywordCoor -> RuleDataMap KeywordCoor AntiRule -> Bool
hasKey (KeywordCoor (k1,k2)) rs = let
  matches = map (\((KeywordCoor (k1',k2')), _) -> k1==k1' || k1==k2' || k2==k1' || k2==k2') $M.toList rs
 in
  or matches 

--TODO this should just be part of a custom Eq instance
flipped :: KeywordCoor -> KeywordCoor
flipped (KeywordCoor (k1,k2)) =
  KeywordCoor (k2,k1)
