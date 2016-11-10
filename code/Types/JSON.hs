{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings#-}

module Types.JSON where


import           Data.Aeson
import qualified Data.Map        as M
import           GHC.Generics    (Generic)

import           Types.IR
import           Types.QTypes
import           Types.Types

data RuleSetLists = RuleSetLists
  { orderl    :: [((IRLine,IRLine),Bool)]
  , missingl  :: [MissingKRule]
  , typeErrl  :: [(Keyword, ConfigQType)]
  , missingPl :: [(MissingKRule, Int, Int)]
  , orderPl   :: [((IRLine,IRLine), (Integer, Integer))]
  , intRelPl  :: [((Keyword,Keyword),FormulaC)]
  } deriving (Show, Generic, ToJSON, FromJSON)

toLists :: RuleSet -> RuleSetLists
toLists RuleSet{..}=
  RuleSetLists
    { orderl = M.toList order
    , missingl = missing
    , typeErrl = M.toList typeErr
    , missingPl = missingP
    , orderPl = M.toList orderP
    , intRelPl = M.toList intRelP
    }

fromLists :: RuleSetLists -> RuleSet
fromLists RuleSetLists{..}=
  RuleSet
    { order = M.fromList orderl
    , missing = missingl
    , typeErr = M.fromList typeErrl
    , missingP = missingPl
    , orderP = M.fromList orderPl
    , intRelP = M.fromList intRelPl
    }


{-
instance ToJSON (Int -> Int -> Bool) where
  toJSON f = object ["formula" .= show f]
instance FromJSON (Int-> Int -> Bool) where
  parseJSON (Object v) =
      if | v .: "formula" == "<=" -> return (<=) -- :: (Int -> Int -> Bool)
         | v .: "formula" == ">=" -> return (>=) -- :: (Int -> Int -> Bool)
         | v .: "formula" == "==" -> return (==) -- :: (Int -> Int -> Bool)
  --parseJSON _ = mzero
  -}
