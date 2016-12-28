{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Rules where

import Types.Common
import Types.IR

--TODO Strict or Lazy maps?
import qualified Data.Map.Strict as M

-- | every instance of Learnable is a template of rules we can learn
--   instance provided in the Learners dir
class Learnable a where
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
  { order    :: RuleDataMap Types.Rules.Ordering
  , missing  :: RuleDataMap KeywordCoor
  , intRel :: RuleDataMap IntRel
--  , typeErr  :: TypeMap ConfigQType
  } deriving (Eq, Show) --, Generic, Typeable)

------------
-- The specific types of relations we want to learn
-- go below here
-----------

-- | these keywords should appear in the same file
-- TODO give explicit Eq instances to be used in merging
-- TODO is overloading a constructor bad practice?
data KeywordCoor = KeywordCoor (Keyword,Keyword) deriving (Eq, Show,Ord)

type Ordering = (Keyword,Keyword)

type IntRel = (IRLine,IRLine,Formula)

-- TODO actual formula here
type Formula = Int

type NumEvidenceTrue = Int
type NumEvidenceFalse= Int
type TotalTimes = Int
type AppearsThereshold = Int
type FrequencyThreshold = Double --between 0 and 1
