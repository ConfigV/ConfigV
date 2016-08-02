{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}


module Types.Types where

import           Control.DeepSeq
import           Data.Foldable
import qualified Data.Map         as M
import qualified Data.Text        as T
import           System.Directory

--import qualified Data.Aeson (ToJSON, FromJSON) as D
import           Control.Monad
import           Data.Aeson
import           GHC.Generics     (Generic)

import           Data.Data
import           Data.Typeable

import           Types.IR
import           Types.QTypes

-- TODO; i think i should be able to remove multiparamtypeclass here
class Foldable t => Attribute t a where
  learn :: IRConfigFile -> t a
  merge :: t a -> t a -> t a --given the spec, this could be reduced to (a -> a -> Merge a)
  check :: t a -> IRConfigFile -> Maybe (t a)

-- | can i replace this with an exstientially quantified type?
-- oh that would be beautiful
-- forall a . Attribute a => [a]
data RuleSet = RuleSet
  { order    :: OrdMap Bool
  , missing  :: [MissingKRule]
  , typeErr  :: TypeMap ConfigQType
  , missingP :: [(MissingKRule, Int, Int)]
  , orderP   :: OrdMap (Integer, Integer)
  , intRelP  :: IntRelMapC
  } deriving (Eq, Show, Generic, Typeable)

instance NFData RuleSet where rnf x = seq x ()

type IntRelMapC = M.Map (Keyword,Keyword) FormulaC
type TypeMap a = M.Map Keyword a
type OrdMap a = M.Map (IRLine,IRLine) a
type IntRelMap = M.Map (Keyword,Keyword) (Maybe (Int->Int->Bool))

-- the types of these rules restricts the search space for learning modules
type Formula = Maybe (Int -> Int -> Bool)
data OrdRule = OrdRule {
  ord_l1  :: IRLine,
  ord_l2  :: IRLine,
  ord_rel :: Bool} deriving (Show,Eq)
--you are missing a key-value pair correlation
data MissingKVRule = MissingKVRule {
  l11 ::IRLine,
  l22 :: IRLine} deriving (Show,Eq, Generic,Data,Typeable, ToJSON, FromJSON)
--you are missing a key correlation
data MissingKRule = MissingKRule {
  k1 :: Keyword,
  k2 :: Keyword} deriving (Show,Eq, Generic, Data,Typeable,ToJSON, FromJSON)
data IntRelRule = IntRelRule {
  l1      :: Keyword,
  l2      :: Keyword,
  formula :: Maybe (Int->Int->Bool)} deriving (Show)
-- a formula count, basically a triple of integers for instances of the pair
--  with ordering <=, >=, or ==
data FormulaC = FormulaC {
  lt :: Int,
  gt :: Int,
  eq :: Int
  } deriving (Show, Eq, Generic, Data,Typeable,ToJSON, FromJSON)


instance Show (Int-> Int -> Bool) where
  show f1 =
    if | (f1 0 1)  -> "<="
       | (f1 1 0) -> ">="
       | (f1 1 1) -> "=="
-- only for comparators! careful!
instance Eq (Int -> Int -> Bool) where
 (==) f1 f2 =
   (f1 1 1) == (f2 1 1) &&
   (f1 0 1) == (f2 0 1) &&
   (f1 1 0) == (f2 1 0)
