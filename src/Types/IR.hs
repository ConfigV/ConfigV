{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}


module Types.IR where

import           Control.DeepSeq

import           Data.Aeson
import           Data.Data
import qualified Data.Text       as T
import           Data.Typeable
import           GHC.Generics    (Generic)
import           System.Directory

import Types.Common
import Control.DeepSeq

type ConfigFile a = (FilePath,T.Text, a)
data Language = 
     MySQL 
   | HTTPD
   | CSV --when your config file preprocessing happens elsewhere and you just have a key,value csv
     deriving (Eq,Show)

-- | Intermediate Representation stuff
type IRConfigFile = [IRLine]
data IRLine = IRLine {
    keyword :: Keyword
  , value   :: Val } 
 deriving (Eq,Ord, Generic,Data,Typeable, ToJSON, FromJSON,NFData)

instance Show IRLine where
  show IRLine{..} = "k:"++(show keyword) ++ " ==> v:"++(show value)

