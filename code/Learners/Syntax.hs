{-# LANGUAGE TupleSections #-}

-- {-# LANGUAGE LambdaCase #-}

module Syntax where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L

import Debug.Trace

-- essential we reduce the problem to finding ordering pairs that are always true
-- pairs that hold over the whole learning set become rules at usertime
-- [[A,B,C],[A,C,B]] -> [(A,B),(A,C)]

learnSyntaxConstraints :: T.Text -> [SynRule]
learnSyntaxConstraints t = 
  makeOrderPairs $ T.lines t


-- | honestly, i thought this would be approximately correct
-- but by pure luck seems to be exactly correct
checkSyn :: [SynRule] -> ConfigFile Common -> Bool
checkSyn rs f = let
  f' = makeOrderPairs $ T.lines (fst f)
  new =  rs L.\\ f' --the difference between the two rule sets
  x = if new == f' then True else trace (show new) False
  in x
  
makeOrderPairs :: [T.Text]  -> [SynRule]
makeOrderPairs [] = []
makeOrderPairs (l:ls) = map (l,) ls ++ makeOrderPairs ls
 {- makeOrderPairs  = \case 
  [] -> []
  (l:ls) -> map (l,) ls ++ makeOrderPairs ls-}

mergeSyn :: [SynRule] -> [SynRule] -> [SynRule]
mergeSyn curr new = let
  combined = L.intersect curr new
 in
  combined

