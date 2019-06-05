module ConfigV.Types.Common where

import Data.Interned.Text 

--just some simple types used by multiple type defs

type Keyword = InternedText
type Val = InternedText

type Probability = Double

data ConfigType =
    Flag -- (0 || 1)
  | Int  
  | String
--  etc..
