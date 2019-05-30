module Types.Locatable where

import Types.Common

class Locatable a where
  keys :: a -> [Keyword]

