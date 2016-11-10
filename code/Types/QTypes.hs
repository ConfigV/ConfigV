{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Types.QTypes where


--import qualified Data.Aeson (ToJSON, FromJSON) as D
import           Control.Monad
import           Data.Aeson
import           GHC.Generics  (Generic)

import           Data.Data
import           Data.Typeable


--type Config a = (Maybe a, Probability)
type DirPath = String
type Probability = Double --st 0<=x<=1
data Config a = Config {p :: Probability} deriving (Show,Eq, Generic,Data,Typeable, ToJSON, FromJSON)


data ConfigQType = ConfigQType {
    cint      :: Config Int
  , cstring   :: Config String
  , cfilepath :: Config FilePath
  --, cdirpath :: Config DirPath
  }
  deriving (Eq, Generic, Data,Typeable,ToJSON, FromJSON)

instance Show ConfigQType where
 show ConfigQType{..} =
  let
    i = if p cint >0 then "Int with P="++(show (p cint))++" " else ""
    s = if p cstring >0 then "String with P="++(show (p cstring))++" " else ""
    f = if p cfilepath >0 then "Filepath with P="++(show (p cfilepath))++" " else ""
    u = if (i++s++f)=="" then "Unknown Type" else ""
  in
    i++s++f++u

zeroProb:: Config a
zeroProb= Config 0
emptyConfigQType = ConfigQType {
  cint = zeroProb,
  cstring = zeroProb,
  cfilepath = zeroProb
 -- cdirpath = zeroProb
  }
