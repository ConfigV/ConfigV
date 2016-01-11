{-# LANGUAGE TupleSections #-}

module Syntax where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L

-- essential we reduce the problem to finding ordering pairs that are always true
-- pairs that hold over the whole learning set become rules at usertime
-- [[A,B,C],[A,C,B]] -> [(A,B),(A,C)]

learnSyntaxConstraints :: T.Text -> [Clause]
learnSyntaxConstraints t = 
  let ls = T.lines t
  in ls
--  in concatMap (makeOrderPairs ls) ls

  
makeOrderPairs :: [T.Text] -> T.Text -> [(T.Text,T.Text)]
makeOrderPairs ls l = 
  map (,l) ls

mergeSyn :: [Clause] -> [Clause] -> [Clause]
mergeSyn curr new = let
  combined = L.intersect curr new
 in
  combined

