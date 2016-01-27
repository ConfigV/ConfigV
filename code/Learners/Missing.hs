{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Missing where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Map as M

import Debug.Trace


instance Attribute [MissingKVRule] where
  learn [] = []
  learn (l:ls) = concatMap (\l' -> if (keyword l == keyword l') then [] else [MissingKVRule l l']) ls ++ learn ls

  check rs f = 
   let
     fRules = learn f
     diff = rs L.\\ fRules --the difference between the two rule sets
     x = if null diff then Nothing else Just $ "Error in Missing Keyword-value Entry on \n "++(show diff)
   in 
    x
  
  merge curr new = L.intersect curr new

  
instance Attribute [MissingKRule] where
  learn [] = []
  learn (l:ls) = concatMap (\l' -> if (keyword l == keyword l') then [] else [MissingKRule (keyword l) (keyword l')]) ls ++ learn ls

  check rs f = 
   let
     fRules = learn f
     rs' = L.nub rs
     diff = rs' L.\\ fRules --the difference between the two rule sets
     x = if null diff then Nothing else Just $ "Error in Missing Keyword Entry on \n"++(show diff)
   in 
     x
  
  merge curr new = L.intersect curr new

