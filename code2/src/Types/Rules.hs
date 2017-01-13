{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE AllowAmbiguousTypes #-} 

module Types.Rules where

import Types.Common
import Types.IR
import Types.Errors
import Types.Countable

import Prelude hiding (Ordering)
import           Data.Aeson
import           GHC.Generics     (Generic)
import           System.Directory

--TODO Strict or Lazy maps?
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

-- | every instance of Learnable is a template of rules we can learn
--   instance provided in the Learners dir
class (Eq a, Show a, Ord a, Countable b) => Learnable a b where
  -- | build a set of rules from a single IR
  -- these rules will have TotalTimes=1
  -- this used to be learn
  buildRelations :: IRConfigFile -> RuleDataMap a b
  -- | once you have all the evidence from indiviual files, merge into one rule
  --   sometimes this can be 'id' if the countable data is isolated (See typeErr)
  merge :: Countable b => [RuleDataMap a b] -> RuleDataMap a b
  --given a rule on keywords a, 
  --check if the first (learned) rule is violated by the second rule from the target verification file
  check :: a -> b -> b -> Maybe b
  -- | How to convert a rule to an error message
  toError :: FilePath -> (a, b) -> Error

class Locatable a where
  keys :: a -> [Keyword]

-- | A rule is the structure for tracking and merging evidence relations
--   We need to track how much evidence we have for the rule, against the rule, and how often we have seen the rule
type RuleDataMap a b = M.Map a b

data RuleSet = RuleSet
  { missing  :: RuleDataMap KeywordCoor AntiRule
  , order    :: RuleDataMap Ordering AntiRule
  , intRel   :: RuleDataMap IntRel Formula
  , typeErr  :: RuleDataMap TypeErr QType
  } --deriving (Eq, Show, Generic)--, Typeable)

emptyRuleSet = RuleSet
  { missing  = M.empty
  , order    = M.empty
  , intRel   = M.empty
  , typeErr  = M.empty}

------------
-- The specific types of relations we want to learn
-- go below here
-----------

-- | these keywords should appear in the same file
-- TODO give explicit Eq instances to be used in merging
data KeywordCoor = KeywordCoor (Keyword,Keyword) 
  deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON)
instance Locatable KeywordCoor where
  keys (KeywordCoor (k1,k2)) = [k1,k2]

data Ordering = Ordering (Keyword,Keyword)
  deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON)
instance Locatable Ordering where
  keys (Ordering  (k1,k2)) = [k1,k2]

data IntRel = IntRel (IRLine,IRLine)
  deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON)
instance Locatable IntRel where
  keys (IntRel (k1,k2)) = [keyword k1,keyword k2]

data TypeErr = TypeErr (Keyword)
  deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON)
instance Locatable TypeErr where
  keys (TypeErr (k1)) = [k1]



