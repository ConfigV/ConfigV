{-# LANGUAGE FlexibleInstances #-}

module Values where

import Types
-- similar to Order.hs, we reduce the problem to 
-- finding relations that always hold 

-- [[(A,1),(B,2)],[(A,1),(B,3)] -> [(A==1),(2<b<3)]



instance Attribute Clause where
  learn f = []
  check cs f = True
  merge c1 c2 = c1
