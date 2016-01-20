module Types where


import qualified Data.Text as T
import qualified Data.Map as M


data RuleSet = RuleSet 
  { order :: [OrdRule]
  , intRel :: [IntRelRule]
  }

class Attribute a where
  learn :: IRConfigFile -> [a]
  merge :: [a] -> [a] -> [a]
  check :: [a] -> IRConfigFile -> Error

-- the types of these rules restricts the search space for learning modules
type OrdRule = (IRLine,IRLine)
type IntRelRule = Int -> Int -> Int

type ConfigFile a = (T.Text, a)
data Language = MySQL | HTTPD

type IRConfigFile = [IRLine]
data IRLine = IRLine {
  keyword :: Keyword,
  configType :: ConfigType,
  value :: Value } deriving (Show, Eq)
type Keyword = T.Text
type Value = T.Text
data ConfigType = 
  ConfigInt | 
  ConfigString | 
  ConfigUnknown deriving (Show, Eq)

type Error = Maybe String


type TypeMap = M.Map Keyword ConfigType

