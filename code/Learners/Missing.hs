{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}

module Learners.Missing where

import Types.Types
import Types.IR

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Map as M

import Debug.Trace

instance Attribute [] MissingKRule where
  learn [] = []
  learn (l:ls) = concatMap (\l' -> if (keyword l == keyword l') then [] else [MissingKRule (keyword l) (keyword l')]) ls ++ learn ls

  check rs f =
   let
     fRules = learn f
     rs' = L.nub rs
     diff = rs' L.\\ fRules --the difference between the two rule sets
     x = if null diff then Nothing else Just diff
   in
     x

  merge curr new = L.intersect curr new
