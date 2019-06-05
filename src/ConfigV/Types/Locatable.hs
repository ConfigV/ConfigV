module ConfigV.Types.Locatable where

import ConfigV.Types.Common

class Locatable a where
  keys :: a -> [Keyword]

