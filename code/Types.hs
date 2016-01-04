module Types where

convert :: ConfigFile a -> ConfigFile Common
getType :: [ConfigFile a] -> a

data RuleSet = RuleSet {Lexical :: [Clause], Syntax :: [Clause], Value :: [Clause]}
data ConfigFile a = (Filepath, a)
