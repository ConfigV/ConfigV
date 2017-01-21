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
import           Types.Countable
import           Types.Rules
import qualified Types.Rules as R
import           Types.Common

data RuleSetLists = RuleSetLists
  { orderl    :: [(R.Ordering,AntiRule)]
  , missingl  :: [(KeywordCoor,AntiRule)]
  , typeErrl  :: [(TypeErr,QType)]
  , intRell  :: [(IntRel,Formula)]
  , finel  :: [(FineGrained,Formula)]
  } deriving (Show, Generic, ToJSON, FromJSON)

toLists :: RuleSet -> RuleSetLists
toLists RuleSet{..}=
  RuleSetLists
    { orderl = M.toList order
    , missingl = M.toList missing
    , typeErrl = M.toList typeErr
    , intRell = M.toList intRel
    , finel = M.toList fineInt
    }

fromLists :: RuleSetLists -> RuleSet
fromLists RuleSetLists{..}=
  RuleSet
    { order = M.fromList orderl
    , missing = M.fromList missingl
    , typeErr = M.fromList typeErrl
    , intRel = M.fromList intRell
    , fineInt = M.fromList finel
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
