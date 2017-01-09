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
         string = fromEnum (all C.isAlphaNum $T.unpack v)
      --,path = fromEnum (not $ all C.isAlphaNum $T.unpack v)
        ,int = fromEnum (all C.isNumber $T.unpack v)
        ,size = fromEnum (T.isSuffixOf "M" v) 
      }
      --undefined --regex, which types are possible
    toTypeErr :: IRLine -> (TypeErr,QType)
    toTypeErr ir = (TypeErr (keyword ir),getQType $ value ir)
   in
     M.fromList $ map toTypeErr  f

  -- not needed since we keywords are always the same
  merge :: RuleDataMap TypeErr QType -> RuleDataMap TypeErr QType
  merge = id 

  check _ rd1 rd2 = let
     tot = fromIntegral $ sum [string rd1, int rd1, size rd1] 
     strProb  = (fromIntegral .string)rd1 / tot 
     intProb  = (fromIntegral .int) rd1 / tot 
     sizeProb = (fromIntegral .size) rd1 / tot 
   in
     if True -- string rd2 == 1 && strProb > 0.5
     then Just rd1
     else Nothing

  toError fname (TypeErr k,rd) = Error{
     errLocs = [(fname,k)]
    ,errIdent = TYPE
    ,errMsg = "TYPE ERROR: Expected a "++(show rd)++" for "++(show k)}
    --".Found value " ++(show $ findVal f' $ fst x) ++ " of type " ++ (show $ assignProbs $ findVal f' $ fst x) }
    
