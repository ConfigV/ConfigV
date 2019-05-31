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

import           Types.Countable
import           Types.Rules
import qualified Types.Rules as R

data RuleSetLists = RuleSetLists
  { orderl    :: [(R.Ordering,AntiRule)]
  , missingl  :: [(KeywordCoor,AntiRule)]
  , keyvalkeyl :: [(KeyValKeyCoor,NontrivRule)]
  , typeErrl  :: [(TypeErr,QType)]
  , intRell  :: [(IntRel,Formula)]
  , finel  :: [(FineGrained,Formula)]
  , smtRulesl  :: [(SMTFormula, AntiRule)]
  } deriving (Generic, ToJSON, FromJSON)

instance Show RuleSetLists where
  show RuleSetLists{..} = let
      f :: Show a => [a] -> String
      f = unlines. map show
    in
      unlines $
        [ "Order:\n" ++ (f orderl) 
        , "Missing:\n" ++ (f missingl)
        , "KeyValKey:\n" ++ (f keyvalkeyl) 
        , "Type:\n" ++ (f typeErrl) 
        , "IntRel:\n" ++ (f intRell)
        , "Finegrain:\n" ++ (f finel)
        , "SMT:\n" ++ (f smtRulesl)
        ]

    

-- | TODO move this to learnRules?
ruleSizes :: RuleSetLists -> String
ruleSizes RuleSetLists{..} = unlines $ 
  [ "Order " ++ (show $ length orderl) 
  , "Missing: " ++ (show $ length missingl)
  , "KeyValKey: " ++ (show $ length keyvalkeyl) 
  , "Type: " ++ (show $ length typeErrl) 
  , "IntRel: " ++ (show $ length intRell)
  , "Finegrain: " ++ (show $ length finel)
  , "SMT: " ++ (show $ length smtRulesl)
  ]

toLists :: RuleSet -> RuleSetLists
toLists RuleSet{..}=
  RuleSetLists
    { orderl = M.toList order
    , missingl = M.toList missing
    , keyvalkeyl = M.toList keyvalkey
    , typeErrl = M.toList typeErr
    , intRell = M.toList intRel
    , finel = M.toList fineInt
    , smtRulesl = M.toList smtRules
    }

fromLists :: RuleSetLists -> RuleSet
fromLists RuleSetLists{..}=
  RuleSet
    { order = M.fromList orderl
    , missing = M.fromList missingl
    , keyvalkey = M.fromList keyvalkeyl
    , typeErr = M.fromList typeErrl
    , intRel = M.fromList intRell
    , fineInt = M.fromList finel
    , smtRules = M.fromList smtRulesl
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
