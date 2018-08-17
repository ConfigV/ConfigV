{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE RecordWildCards    #-}


module Types.IR where

import           Control.DeepSeq

import           Data.Aeson
import           Data.Data
import qualified Data.Text       as T
import           GHC.Generics    (Generic)

import Types.Common

import Data.Interned.Internal.Text

type ConfigFile a = (FilePath,T.Text, a)
data Language = 
     MySQL 
   | HTTPD
   | CSV --when your config file preprocessing happens elsewhere and you just have a key,value csv
     deriving (Eq,Show)

deriving instance Data InternedText
deriving instance Generic InternedText
deriving instance ToJSON InternedText
deriving instance FromJSON InternedText
deriving instance NFData InternedText


-- | Intermediate Representation stuff
type IRConfigFile = [IRLine]
data IRLine = IRLine {
    keyword :: Keyword
  , value   :: Val } 
 deriving (Eq,Ord, Generic,Data,Typeable, ToJSON, FromJSON,NFData)


instance Show IRLine where
  show IRLine{..} = "k:"++(show keyword) ++ " ==> v:"++(show value)

