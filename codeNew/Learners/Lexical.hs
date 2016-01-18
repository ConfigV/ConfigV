{-# LANGUAGE FlexibleInstances #-}

module Lexical where
import Types


instance Attribute LexRule where
  learn f = []
  check cs f = True
  merge c1 c2 = c1
