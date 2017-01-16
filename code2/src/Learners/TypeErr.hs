{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Learners.TypeErr where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable

import Prelude hiding (TypeErr)
import qualified Data.Map as M
import           System.Directory
import qualified Data.Text as T
import qualified Data.Bits as B
import qualified Data.Char as C

import Debug.Trace
import Learners.Common
-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable TypeErr QType where

  buildRelations :: IRConfigFile -> RuleDataMap TypeErr QType
  buildRelations f = let
    --TODO fill out
    getQType :: T.Text -> QType
    getQType v =
      QType {
         string = fromEnum ((T.length $ T.takeWhile C.isAlpha v) > 1)
        ,path = fromEnum ((T.isInfixOf "/" v) || (T.isInfixOf "." v))
        ,int = fromEnum (all C.isNumber $T.unpack v)
        ,bool = fromEnum (v == "")--flag keywords have no values
        ,size = fromEnum ((or $ map (\x-> T.isSuffixOf x v) ["G","g","M","m","K","k"]) && (C.isNumber $ T.head v) ) --TODO are lower case allowed?
      }
    --NB on takeWhile, types of keywords do not depend on context
    -- this creates a problem with the Chekcer tho...
    toTypeErr :: IRLine -> (TypeErr,QType)
    toTypeErr ir = (TypeErr (keyword ir),getQType $ value ir)
    --toTypeErr ir = (TypeErr (T.takeWhile (/= '[') $ keyword ir),getQType $ value ir)
   in
     M.fromList $ map toTypeErr  f

  merge rs = 
     M.unionsWith add rs

  -- 
  check _ rd1 rd2 = let
     tot = fromIntegral $ sum [string rd1, int rd1, size rd1] 
     toProb x = (fromIntegral .x)rd1 / tot 
   in
     if 
       string rd2 == 1 && toProb string > 0.7 ||
       path   rd2 == 1 && toProb path> 0.5 ||
       bool   rd2 == 1 && toProb bool > 0.5 ||
       int    rd2 == 1 && (toProb int> 0.5 || toProb size>0.5) ||
       size   rd2 == 1 && toProb size> 0.6 ||
       tot == 0
     then Nothing
     else Just rd1

  toError fname (TypeErr k,rd) = Error{
     errLocs = [(fname,k)]
    ,errIdent = TYPE
    ,errMsg = "TYPE ERROR: Expected a "++(show rd)++" for "++(show k)}
    --".Found value " ++(show $ findVal f' $ fst x) ++ " of type " ++ (show $ assignProbs $ findVal f' $ fst x) }
    
