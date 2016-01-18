module Types where


import Data.Text as T

-- | for now just id
--
-- I think we actually need learning here
-- the type of ConfigFile Common will be something like [(VariableName, Variable Value)]
convert :: ConfigFile Language -> ConfigFile Common
convert (t, MySQL) = (t,Common)
convert (t, HTTPD) = (t,Common)



data RuleSet = RuleSet {
  lexical :: [LexRule], 
  order :: [SynRule], 
  value :: [Clause],
  intRels :: [IntRelRule]}



-- | collect contraints from each file indepentantly


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

