module Types.QTypes where

import Types.Common

import qualified Data.Map        as M

-- | A single QType is the collection of possible types and the probs
data QType =
  Set (ConfigType, Probability)  

-- | type inference will construct a map 
--   which is updated as we see more examples of values/use
--   might even be useful in real langauge, to learn more specific "subtypes" (or refinement types?)
--   https://arxiv.org/abs/1505.02878
type TypeInference = M.Map Keyword QType
