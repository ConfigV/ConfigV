module Syntax where

-- essential we reduce the problem to finding ordering pairs that are always true
-- pairs that hold over the whole learning set become rules at usertime
-- [[A,B,C],[A,C,B]] -> [(A,B),(A,C)]

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

