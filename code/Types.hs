module Types where


import Data.Text as T




data RuleSet = RuleSet {
  lexical :: [LexRule], 
  syntax :: [SynRule], 
  value :: [Clause]}

type Clause = T.Text

type SynRule = (T.Text,T.Text)
type LexRule = (T.Text,T.Text)

type ConfigFile a = (T.Text, a)
data Language = MySQL | HTTPD
data Common = Common

type Error = String

-- | for now just id
convert :: ConfigFile Language -> ConfigFile Common
convert (t, MySQL) = (t,Common)
convert (t, HTTPD) = (t,Common)
