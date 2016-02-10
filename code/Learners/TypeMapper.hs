{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE TypeSynonymInstances#-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses#-}

module Learners.TypeMapper where

import Types
import Convert

import Data.Char
import qualified Data.Text as T
import qualified Data.Map as M

import Debug.Trace

instance Attribute (M.Map Keyword) ConfigQType where
  learn f =
    foldl addTypeToMap M.empty f

  check rs f =
    let
      fRules = learn f
      mismatches = M.differenceWith typeConflict fRules rs
    in
      if M.null mismatches then Nothing else Just mismatches

  merge oldMap newMap = M.unionWithKey (updateProbs) oldMap newMap

traceMe x = trace (concatMap (\x-> show x++"\n")(M.toList x)) x

-- | if types conflict, return Just _, so we can report the error
typeConflict :: ConfigQType -> ConfigQType -> Maybe ConfigQType
typeConflict t1 t2 =
    if t1 == t2 then Nothing else Just t2


-- | can use all sorts of nice Data.map fxns (see docs)
addTypeToMap :: TypeMap ConfigQType -> IRLine -> TypeMap ConfigQType
addTypeToMap m IRLine{..} =
  let
    newCType = assignProbs value
  in
    -- will only lookup if a file has duplicate keywords
    snd $ M.insertLookupWithKey updateProbs keyword newCType m

updateProbs :: Keyword -> ConfigQType -> ConfigQType -> ConfigQType
updateProbs k old new =
  new

assignProbs :: Value -> ConfigQType
assignProbs t =
  let
   cint =
     buildConfTy (T.all isDigit t) (read $ T.unpack t :: Int)
   cstring =
     buildConfTy (T.all isAlpha t) (T.unpack t :: String)
   cfilepath =
     buildConfTy
       ((T.isInfixOf "/" t) || (T.isInfixOf "." t) || (T.isInfixOf "\\" t))
       (T.unpack t :: FilePath)
   {-cdirpath =
     buildConfTy
       ((T.isInfixOf "/" t))
       (T.unpack t :: DirPath)-}
  in
   ConfigQType{..}

buildConfTy :: Bool -> a -> Config a
buildConfTy b v =
  if b then (Config 1.0::Config a) else zeroProb

-- | should be dual of attemptCoerce, but not sure how to write that cleanly
--   maybe also want subtyping?
{-checkType :: Value -> ConfigType -> Bool
checkType v t =
  attemptCoerce v == t-}
