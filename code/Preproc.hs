module Preproc where

preproc :: [ConfigFile a] -> (RuleSet,a)
preproc cs =
  ds = map convert cs 
  rules = learnOn ds
  fileType = getType cs
  return (rules,fileType)

learnOn :: [ConfigFile Common] -> RuleSet
learnOn cs = 
  rs = map genConstraints cs :: [RuleSet]
  foldl1 mergeRules rs

genConstraints :: ConfigFile Common -> RuleSet
genConstraints c =
  r1 = learnLexicalConstraints c
  r2 = learnSyntaxConstraints c
  r3 = learnValueConstraints c
  return RuleSet {r1, r2, r3}

mergeRules :: RuleSet -> RuleSet -> RuleSet
mergeRules rs rs' =
  return RuleSet
    { mergeLex (Lexical rs) (Lexical rs')
    , mergeSyn (Syntax rs)  (Syntax rs')
    , mergeVal (Value rs)   (Value rs')}

