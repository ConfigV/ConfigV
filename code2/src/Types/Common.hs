module Types.Common where

import qualified Data.Text       as T

--just some simple types used by multiple type defs

type Keyword = T.Text
type Val = T.Text

type Probability = Double

data ConfigType =
    Flag -- (0 || 1)
  | Int  
  | String
--  etc..
