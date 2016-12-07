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

import Types.Util

type ConfigFile a = (T.Text, a)
data Language = MySQL | HTTPD



-- | Intermediate Representation stuff
type IRConfigFile = [IRLine]
data IRLine = IRLine {
    keyword :: Keyword
  , value   :: Value } 
 deriving (Eq,Ord, Generic,Data,Typeable, ToJSON, FromJSON)

instance Show IRLine where
  show IRLine{..} = (show keyword) ++ (show value)

