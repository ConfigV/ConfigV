{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}

module Convert where

import Types

import qualified Data.Text as T
import qualified Data.Map as M


convert ::  TypeMap -> ConfigFile Language -> IRConfigFile
convert tyMap f =
  map (addConfigType tyMap) (parse f)

addConfigType :: TypeMap -> (Keyword,Value) -> IRLine
addConfigType tyMap (keyword,value) = 
  case M.lookup keyword tyMap of
    Just configType -> IRLine{..}
    Nothing -> IRLine{configType = ConfigUnknown,..}

-- | If the user wants to give hints to how to parse a config file based on filetype they go here
parse :: ConfigFile Language -> [(Keyword,Value)]
parse (t, MySQL) = map seperateVals (T.lines t)
parse (t, HTTPD) =  map seperateVals (T.lines t)


-- | for now we are just using spaces and = to seperate keywords and values
seperateVals :: T.Text -> (Keyword,Value)
seperateVals t = 
  let
    ts = T.split isDelimeter (T.strip t)
    isDelimeter c = (c=='=') || (c==' ')
  in
    (head ts, last ts)
