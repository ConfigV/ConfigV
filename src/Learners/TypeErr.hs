{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Learners.TypeErr where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable

import Prelude hiding (TypeErr)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Interned

import Learners.Common

import Settings.Config
import Control.Monad.Reader

-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable TypeErr QType where

--  buildRelations :: IRConfigFile -> RuleDataMap TypeErr QType
  buildRelations f = let
    getQType :: T.Text -> QType
    getQType v =
      QType {
         string = fromEnum $ validAsString v 
        ,path = fromEnum $ validAsPath v 
        ,bool = fromEnum $ validAsBool v 
        ,int = fromEnum $ validAsInt v 
        ,size = fromEnum $ validAsSize v
      }
    -- on takeWhile, types of keywords do not depend on context
    -- this creates a problem with the Chekcer tho...
    toTypeErr :: IRLine -> (TypeErr,QType)
    --toTypeErr ir = (TypeErr (keyword ir),getQType $ value ir)
    toTypeErr ir = (TypeErr (intern $ T.takeWhile (/= '[') $ unintern $ keyword ir), getQType $ unintern $ value ir)
    untypable = ["set-variable"] -- some keywords are too hard to type, make sure they can take any type
    f' = filter (\ir -> not $ any (\k -> T.isInfixOf k (unintern $ keyword ir)) untypable) f
   in
     return $ M.fromList $ map toTypeErr f'

  merge rules = do
     settings <- ask
     -- this is support for all possible types, not per file
     return $ M.filter (\rd -> totalC rd > (typeSupport $ thresholdSettings settings)) $ M.unionsWith add rules

  -- 
  check _ rd1 rd2 = let
     toProb x = if (totalC rd1) > 0 --Settings.typeSupport --TODO necessary?
       then (fromIntegral .x)rd1 / (fromIntegral $totalC rd1)
       else 1
     cutoff = 0 --typeConfidence
   in
     if 
       (string rd2 == 1 && toProb string > cutoff) ||
       (path   rd2 == 1 && (toProb string+ toProb path) > cutoff) ||
       (bool   rd2 == 1 && (toProb bool + toProb int) > cutoff) || --TODO only allow ints of value 0/1 as bool, rn any int is ok
       (int    rd2 == 1 && ((toProb int + toProb size > cutoff) || (toProb int + toProb bool > cutoff))) ||
       (size   rd2 == 1 && toProb size> cutoff) ||
       totalC rd1 == 0
     then Nothing
     else Just rd1

  toError ir fname (TypeErr k,rd1) = Error{
     errLocs = [(fname,k)]
    ,errIdent = TYPE
    ,errMsg = "TYPE ERROR: Expected a "++(show rd1)++" for "++(show k)++" \n Found value:"++(show $ map value $ filter (\x-> (T.takeWhile (/= '[') $ unintern $ keyword x) == unintern k) ir)
    ,errSupport = totalC rd1}
    --".Found value " ++(show $ findVal f' $ fst x) ++ " of type " ++ (show $ assignProbs $ findVal f' $ fst x) }

totalC rd1 = sum [string rd1, int rd1, size rd1, bool rd1, path rd1] 

p = QType {
  string =1
 ,path =1
 ,int =1
 ,bool =1
 ,size =1
}


i = QType {
  string =2
 ,path =0
 ,int =37
 ,bool =7
 ,size =0
}

test = check (TypeErr "F") i p
