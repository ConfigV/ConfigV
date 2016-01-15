module Types where


import Data.Text as T




data RuleSet = RuleSet {
  lexical :: [LexRule], 
  order :: [SynRule], 
  value :: [Clause],
  intRels :: [IntRelRule]}

class Attribute a where
  learn :: ConfigFile Common -> [a]
  merge :: [a] -> [a] -> [a]
  check :: [a] -> ConfigFile Common -> Bool

type SynRule = (T.Text,T.Text)
type LexRule = (T.Text,T.Text,Int)
type IntRelRule = Int -> Int -> Int
type Clause = T.Text
-- the types of these rules restricts the search space for learning modules




type ConfigFile a = (T.Text, a)
data Language = MySQL | HTTPD
data Common = Common

type Error = String

-- | for now just id
convert :: ConfigFile Language -> ConfigFile Common
convert (t, MySQL) = (t,Common)
convert (t, HTTPD) = (t,Common)
