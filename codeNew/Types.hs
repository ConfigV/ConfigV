module Types where


import Data.Text as T


data RuleSet = RuleSet 
  { order :: [OrdRule]
  , intRel :: [IntRelRule]
  }

class Attribute a where
  learn :: IRConfigFile -> [a]
  merge :: [a] -> [a] -> [a]
  check :: [a] -> IRConfigFile -> Bool

type OrdRule = (T.Text,T.Text)
type IntRelRule = Int -> Int -> Int
-- the types of these rules restricts the search space for learning modules

type ConfigFile a = (T.Text, a)
data Language = MySQL | HTTPD

type IRConfigFile = (Keyword, Value)
type Keyword = T.Text
type Value = T.Text

type Error = String

