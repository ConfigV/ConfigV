{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE MultiWayIf#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}

module Types where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Foldable
import Control.DeepSeq

-- | can i replace this with an exstientially quantified type?
-- oh that would be beautiful
-- forall a . Attribute a => [a]
data RuleSet = RuleSet
  { order :: OrdMap Bool
  , intRel :: IntRelMap
  , missing :: [MissingKRule]
  , typeErr :: TypeMap ConfigQType
  , missingP :: [(MissingKRule, Int, Int)]
  , orderP :: OrdMap (Int, Int)
  }

instance NFData RuleSet where rnf x = seq x ()
class Foldable t => Attribute t a where
  learn :: IRConfigFile -> t a
  merge :: t a -> t a -> t a --given the spec, this could be reduced to (a -> a -> Bool)
  check :: t a -> IRConfigFile -> Maybe (t a)


type ConfigFile a = (T.Text, a)
data Language = MySQL | HTTPD

--(rule broken, expected value, actual value)
--data Error a = (Maybe a,
--
-- the types of these rules restricts the search space for learning modules
data OrdRule = OrdRule {
  ord_l1 :: IRLine,
  ord_l2 :: IRLine,
  ord_rel :: Bool} deriving (Show,Eq)
--you are missing a key-value pair correlation
data MissingKVRule = MissingKVRule {
  l11 ::IRLine,
  l22 :: IRLine} deriving (Show,Eq)
--you are missing a key correlation
data MissingKRule = MissingKRule {
  k1 :: Keyword,
  k2 :: Keyword} deriving (Show,Eq)
data IntRelRule = IntRelRule {
  l1 :: Keyword,
  l2 :: Keyword,
  formula :: Maybe (Int -> Int -> Bool)} deriving (Show)

type TypeMap a = M.Map Keyword a
type OrdMap a = M.Map (IRLine,IRLine) a
type IntRelMap = M.Map (Keyword,Keyword) (Maybe (Int->Int->Bool))
type Formula = Maybe (Int->Int->Bool)
instance Show (Int-> Int -> Bool) where
  show f1 =
    if | (f1 0 1)  -> "<="
       | (f1 1 0) -> ">="
       | (f1 1 1) -> "=="
-- a formula count, basically a triple of integers for instances of the pair
--  with ordering <=, >=, or ==
data FormulaC = FormulaC {
  lt :: Int,
  gt :: Int,
  eq :: Int
} deriving (Show, Eq)


-- | Intermediate Representation stuff
type IRConfigFile = [IRLine]
data IRLine = IRLine {
  keyword :: Keyword,
  value :: Value } deriving (Eq,Ord)
instance Show IRLine where
  show IRLine{..} = (show keyword) ++ (show value)
type Keyword = T.Text
type Value = T.Text

--type Config a = (Maybe a, Probability)
type DirPath = String
type Probability = Double --st 0<=x<=1
data Config a = Config {p :: Probability} deriving (Show,Eq)

data ConfigQType = ConfigQType {
    cint :: Config Int
  , cstring :: Config String
  , cfilepath :: Config FilePath
  --, cdirpath :: Config DirPath
  }
  deriving (Eq)

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
