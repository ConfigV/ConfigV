{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module IntRel where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.Map as M

import Debug.Trace

-- I haven't found a nice package for SMTLIB format in haskell yet, its out there tho, im sure

instance Attribute IntRelRule where
  learn ts = [(+)]

  check rs f = Nothing
  
  merge curr new = [(+)]


