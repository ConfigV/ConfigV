{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module Learners.KeywordCoor where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable

import qualified Types.Rules as R

import qualified Data.Map as M
import           System.Directory

import Learners.Common

instance Learnable R.KeywordCoor AntiRule where

  buildRelations f = 
    M.empty
 {-let
    toKC (ir1,ir2) = KeywordCoor (keyword ir1, keyword ir2) 
    irPairs = pairs f
   in
     M.fromList $ embedOnce $ map toKC irPairs -}
  
  merge= id

  check _ r1 r2 = Nothing
  
  toError fname ((KeywordCoor (k1,k2)),rd) = Error{
     errLocs= [(fname,k1),(fname,k2)]
    ,errIdent = MISSING
    ,errMsg = "KEYWORD ERROR: Expected "++(show k1)++" WITH "++(show k2)}

pairs :: [IRLine]  -> [(IRLine,IRLine)]
pairs [] = []
pairs (l:ls) =
  let
    thisP = map (\x->(l,x)) ls
    theRest = pairs ls
    noSelf = filter (\r -> let f s= keyword.s in (f fst r)/=(f snd r)) (thisP++theRest)
  in
    noSelf
