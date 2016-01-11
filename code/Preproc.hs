module Preproc where

import Lexical
import Values
import Syntax
import Types

import qualified Data.Text.IO as T
import System.IO.Unsafe

-- | learn rules based on a set of files
preproc :: [ConfigFile Language] -> RuleSet
preproc cs = let
  ds = map convert cs 
  rules = learnOn ds
 in
  rules

-- | collect contraints from each file indepentantly
-- this should be parmap
learnOn :: [ConfigFile Common] -> RuleSet
learnOn cs = let
  rs = map genConstraints cs :: [RuleSet]
 in
  foldl1 mergeRules rs

-- | call each of the learning modules
genConstraints :: ConfigFile Common -> RuleSet
genConstraints (p,c) = RuleSet
  { lexical = learnLexicalConstraints (p,c)
  , syntax  = learnSyntaxConstraints (p)
  , value   = learnValueConstraints (p,c)}

-- | reconcile all the information we have learned
-- later, think about merging this step with genConstraints
-- no more parallel, but might be faster
mergeRules :: RuleSet -> RuleSet -> RuleSet
mergeRules rs rs' = RuleSet
  { lexical = mergeLex (lexical rs) (lexical rs')
  , syntax  = mergeSyn (syntax rs) (syntax rs')
  , value   = mergeVal (value rs)  (value rs')}

