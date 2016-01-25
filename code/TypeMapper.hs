{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}

module TypeMapper where

import Types
import Convert

import Data.Char
import qualified Data.Text as T
import qualified Data.Map as M


-- | in this pass we build a mapping from keywords to types
-- to be reused when checking a userfile
-- with a large learning set, concatMap might run out of memory
learnTypes :: [ConfigFile Language] -> TypeMap
learnTypes fs =
 let
    fs' = concatMap parse fs
  in
    foldl addTypeToMap M.empty fs' 

-- | can use all sorts of nice Data.map fxns (see docs)
addTypeToMap :: TypeMap -> (Keyword, Value) -> TypeMap
addTypeToMap m (k,v) =
  let
    newCType = assignProbs v
  in
    snd $ M.insertLookupWithKey updateProbs k newCType m

updateProbs :: Keyword -> ConfigQType -> ConfigQType -> ConfigQType
updateProbs k old new =
  new

assignProbs :: Value -> ConfigQType
assignProbs t = 
  let 
   cint =
     buildConf (T.all isDigit t) (read $ T.unpack t :: Int)
   cstring = 
     buildConf (T.all isAlpha t) (T.unpack t :: String)
   cfilepath =
     buildConf 
       ((T.length $ last $ T.splitOn "." t) <= 4 && 
        (length $ T.splitOn "." t) == 2)
       (T.unpack t :: FilePath)
   cdirpath = 
     buildConf 
       ((T.isInfixOf "/" t) && 
        (not $ T.isInfixOf "." t))
       (T.unpack t :: DirPath)
  in 
   ConfigQType{..}

buildConf :: Bool -> a -> Config a
buildConf b v = 
  if b then (Config 1.0::Config a) else zeroProb

-- | should be dual of attemptCoerce, but not sure how to write that cleanly
--   maybe also want subtyping?
{-checkType :: Value -> ConfigType -> Bool
checkType v t =
  attemptCoerce v == t-}
