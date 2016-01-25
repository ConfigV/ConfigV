{-# LANGUAGE RecordWildCards#-}
{-# LANGUAGE MultiWayIf#-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import qualified Data.Text as T
import qualified Data.Map as M

-- | can i replace this with an exstientially quantified type?
-- oh that would be beautiful
-- forall a . Attribute a => [a]
data RuleSet = RuleSet 
  { order :: [OrdRule]
  , intRel :: [IntRelRule]
  , missing :: [MissingKRule]
  }

class Attribute a where
  learn :: IRConfigFile -> [a]
  merge :: [a] -> [a] -> [a]
  check :: [a] -> IRConfigFile -> Error

type Error = Maybe String

type ConfigFile a = (T.Text, a)
data Language = MySQL | HTTPD

-- the types of these rules restricts the search space for learning modules
type OrdRule = (IRLine,IRLine)
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
  formula :: (Int -> Int -> Bool)} deriving (Show)

instance Show (Int-> Int -> Bool) where
  show f1 = 
    if | (f1 0 1)  -> "<="
       | (f1 1 0) -> ">="
       | (f1 1 1) -> "=="


-- | Intermediate Representation stuff
type IRConfigFile = [IRLine]
data IRLine = IRLine {
  keyword :: Keyword,
  configType :: ConfigQType,
  value :: Value } deriving (Eq)
instance Show IRLine where
  show IRLine{..} = (show keyword) ++ (show value)
type Keyword = T.Text
type Value = T.Text

--type Config a = (Maybe a, Probability)
type DirPath = String
type Probability = Double --st 0<=x<=1
data Config a = Config {p :: Probability} deriving (Show,Eq)

data ConfigQType = ConfigQType {
  cint :: Config Int ,
  cstring :: Config String ,
  cfilepath :: Config FilePath , 
  cdirpath :: Config DirPath }
  deriving (Show, Eq)

zeroProb:: Config a
zeroProb= Config 0  
emptyConfigQType = ConfigQType {
  cint = zeroProb,
  cstring = zeroProb,
  cfilepath = zeroProb,
  cdirpath = zeroProb}   


type TypeMap = M.Map Keyword ConfigQType

