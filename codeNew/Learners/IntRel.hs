{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module IntRel where

import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L

import Debug.Trace

instance Attribute IntRelRule where
  learn (t,c) = [(+)]

  check rs f = True
  
  merge curr new = [(+)]


