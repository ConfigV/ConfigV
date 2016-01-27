{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}

module Convert where

import Types

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad 

import Data.List.Unique as L

import Debug.Trace

convert ::  ConfigFile Language -> IRConfigFile
convert f =
  parse f

-- | why would i want to do this?
{-addConfigType :: TypeMap -> (Keyword,Value) -> IRLine
addConfigType tyMap (keyword,value) = 
  case M.lookup keyword tyMap of
    Just configType -> IRLine{..}
    Nothing -> IRLine{configType = emptyConfigQType,..}-}

-- | If the user wants to give hints to how to parse a config file based on filetype they go here
parse :: ConfigFile Language -> [IRLine] --[(Keyword,Value)]
parse (t, l) = 
  let
    noComments = (map (stripComment l) $ T.lines t)
    noEmpty = filter (not. T.null) noComments
    noEmptyAsKV = map seperateVals noEmpty
    noDups = foldl (\rs r -> makeUniq rs r) [] noEmptyAsKV
  in
    map (\(k,v)-> IRLine{keyword=k,value=v}) noEmptyAsKV
    --noDups

makeUniq :: [(Keyword,Value)] -> (Keyword,Value) -> [(Keyword,Value)]
makeUniq rs (k,v) = 
  let
   c = L.countElem k (map fst rs)
   k' = if c >= 2 then T.append k (T.pack $show c) else k
  in
   rs ++ [(k',v)]
 
stripComment :: Language -> T.Text -> T.Text
stripComment l t = case l of
  MySQL -> T.takeWhile (/='#') t
  HTTPD -> T.takeWhile  (/=';') t

-- | for now we are just using spaces and = to seperate keywords and values
seperateVals :: T.Text -> (Keyword,Value)
seperateVals t = 
  let
    ts = T.split isDelimeter (T.strip t)
    isDelimeter c = (c=='=') || (c==' ')
  in
    (T.strip $ head ts, T.strip $ last ts)


