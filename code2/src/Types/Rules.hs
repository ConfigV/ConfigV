{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}

module Types.Rules where

import Types.Common
import Types.IR

import Prelude hiding (Ordering)
import           Data.Aeson
import           GHC.Generics     (Generic)

--TODO Strict or Lazy maps?
import qualified Data.Map.Strict as M

-- | every instance of Learnable is a template of rules we can learn
--   instance provided in the Learners dir
class (Eq a, Show a) => Learnable a where
  -- | build a set of rules from a IR
  -- these are not probabalistic
  buildRelations :: IRConfigFile -> RuleDataMap a
  -- | how to check if a rule holds on a file
  check :: a -> IRConfigFile -> Bool

-- | A rule is the structure for tracking and merging evidence relations
--   We need to track how much evidence we have for the rule, against the rule, and how often we have seen the rule
type RuleData = (NumEvidenceTrue, NumEvidenceFalse, TotalTimes) 
type RuleDataMap a = M.Map a RuleData

data RuleSet = RuleSet
  { missing  :: RuleDataMap KeywordCoor
  , order    :: RuleDataMap Ordering
  , intRel :: RuleDataMap IntRel
--  , typeErr  :: TypeMap ConfigQType
  } --deriving (Eq, Show, Generic)--, Typeable)

------------
-- The specific types of relations we want to learn
-- go below here
-----------

-- | these keywords should appear in the same file
-- TODO give explicit Eq instances to be used in merging
data KeywordCoor = KeywordCoor (Keyword,Keyword) 
  deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON)

data Ordering = Ordering (Keyword,Keyword)
  deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON)

data IntRel = IntRel (IRLine,IRLine,Formula)
  deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON)

-- TODO actual formula here
type Formula = Int

type NumEvidenceTrue = Int
type NumEvidenceFalse= Int
type TotalTimes = Int
type AppearsThereshold = Int
type FrequencyThreshold = Double --between 0 and 1
