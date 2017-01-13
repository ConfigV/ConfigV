{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE TupleSections #-} 

module Learners.Ordering where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable

import Prelude hiding (Ordering)
import qualified Data.Map as M
import           System.Directory
import qualified Data.Text as T
import qualified Data.Bits as B

import Learners.Common
-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable Ordering AntiRule where

  buildRelations f = let
    toOrdering (ir1,ir2) = Ordering (keyword ir1, keyword ir2) 
    irPairs = filter (not. sameConfigModule) $ pairs f
   in
     M.fromList $ embedOnce $ map toOrdering $ irPairs 
    --M.foldrWithKey removeConflicts x x

  --since AntiRules are built with different keyword pais
  --we need to generate the counts of false evidence
  --ths is nice since we will maintain both (k1,k2) and (k2,k1) rules
  merge rs' = let 
      rs = M.unionsWith add rs'
      findOp (Ordering (k1,k2)) = M.findWithDefault (AntiRule 0 0 0) (Ordering (k2,k1)) rs
      adjWithOp (AntiRule tru fls t) (AntiRule truOp flsOp tOp) = 
        AntiRule tru truOp (t+tOp) 
      updateWithOp k v = combine v $ findOp k
      validRule r = (tru r)>=1 && (fls r)<=1
    in
      M.filter validRule $ M.mapWithKey updateWithOp rs

  -- | does the r2 we found in the target file
  --   agree with the learned r1
  check _ rd1 rd2 = let
     agrees r1 r2 = 
       if tru r2 ==1
       then tru r1 > fls r1
       else fls r1 > tru r1
   in
     if (not $agrees rd1 rd2)
     then Just rd1
     else Nothing

  toError fname ((Ordering (k1,k2)),rd) = Error{
     errLocs = map (fname,) [k1,k2]
    ,errIdent = ORDERING
    ,errMsg = "ORDERING ERROR: Expected "++(show k1)++" BEFORE "++(show k2)++" \n   w/ confidence "++(show rd)}

--according to Ennan modules do not interact with anything except themselves
--so take the string before the first '_' as the module and only compare those
sameConfigModule (ir1,ir2) = let
  getMod = fst. T.breakOn "_". keyword
  emptyMod x = (==) "" $ (snd$ T.breakOn "_"$ keyword x)
  --special case for socket and port
  sockport = 
   (T.isInfixOf "socket" (getMod ir1) ||
    T.isInfixOf "port"   (getMod ir1)) `B.xor`
   (T.isInfixOf "socket" (getMod ir2) ||
    T.isInfixOf "port"   (getMod ir2))
  sameMod = (not (emptyMod ir1 && emptyMod ir2)) && (getMod ir1 == getMod ir2)
 in
  sockport || sameMod

pairs :: [IRLine]  -> [(IRLine,IRLine)]
pairs [] = []
pairs (l:ls) =
  let
    thisP = map (\x->(l,x)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf

