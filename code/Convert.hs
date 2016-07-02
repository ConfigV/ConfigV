{-# LANGUAGE OverloadedStrings #-}

module Convert where

import Types

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad

--import Data.List.Unique as L

import Debug.Trace

-- | why would i want to do this?
{-addConfigType :: TypeMap -> (Keyword,Value) -> IRLine
addConfigType tyMap (keyword,value) =
case M.lookup keyword tyMap of
Just configType -> IRLine{..}
Nothing -> IRLine{configType = emptyConfigQType,..}-}


    -- | Main function of this file - translate configFile to intermediate rep
-- | If the user wants to give hints to how to parse a config file based on filetype they go here
convert :: ConfigFile Language -> [IRLine] --[(Keyword,Value)]
convert (t, l) =
  let
    noComments = (map (stripComment l) $ T.lines t)
    noEmpty = filter (not. T.null) noComments
    noEmptyAsKV = map seperateVals noEmpty
    noDups = makeUniq l noEmptyAsKV
  in
    map (\(k,v)-> IRLine{keyword=k,value=v}) noDups -- noEmptyAsKV
    --noDups

makeUniq :: Language -> [(Keyword,Value)] -> [(Keyword,Value)]
makeUniq lang ls = case lang of
  MySQL ->
    let
      f _ [] = []
      f header (x:xs) =
        if (fst x==snd x) && T.isInfixOf "[" (fst x) then f x xs else (T.append (fst x) (fst header),snd x) : f header xs
    in
      f (head ls) ls

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
