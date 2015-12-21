module Preproc where

preproc :: [ConfigFile a] -> (RuleSet,a)
preproc cs =
  ds = map convert cs 
  rules = learnOn ds
  fileType = getType cs
  return (rules,fileType)

learnOn :: [ConfigFile Common] -> RuleSet
learnOn cs = 
  rs = map genConstraints cs
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

learnLexicalConstraints :: ConfigFile Common -> [Clause]
learnLexicalConstraints =
  

mergeLex =
  strisynth?

learnSyntaxConstraints :: ConfigFile Common -> [Clause]
learnSyntaxConstraints c =
  ls = lines c
  concatMap (makeOrderPairs ls) ls
  
makeOrderPairs :: [Text] -> Text -> [(Text,Text)]
makeOrderPairs ls l = 
  map (,l) ls

mergeSyn :: [Clause] -> [Clause] -> [Clause]
mergeSyn curr new =
  combined = intersect curr new
  return combined

learnValueConstraints :: Config Common -> RuleSet
learnValueConstraints c =
