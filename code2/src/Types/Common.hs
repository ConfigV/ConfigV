module Types.Common where

--just some simple types used by multiple type defs

type Keyword = T.Text
type Value = T.Text

type Probability = Dobule

data ConfigType =
  | Flag -- (0 || 1)
  | Int  
  | String
--  etc..
