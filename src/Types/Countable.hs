{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Countable where

import Types.Common

import qualified Data.Map        as M
import           Data.Aeson
import           GHC.Generics     (Generic)

import Control.DeepSeq
-- | type inference will construct a map 
--   which is updated as we see more examples of values/use
--   might even be useful in real langauge, to learn more specific "subtypes" (or refinement types?)
--   https://arxiv.org/abs/1505.02878
type TypeInference = M.Map Keyword QType

-- | A single QType is the collection of counts of possible types
data QType = QType {
  string :: Int
 ,path :: Int
 ,int :: Int  
 ,bool :: Int  
 ,size :: Int --mb/kb
} deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON,NFData)

-- For relations over partial orders (usually numeric values, e.g. RAM size)
data Formula = Formula {
  gt :: Int
 ,lt :: Int
 ,eq :: Int
 }
  deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON,NFData)

-- Rules that can only be true or false
data AntiRule = AntiRule {
   tru :: Int
  ,fls :: Int
  ,tot :: Int
  } deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON,NFData)

-- Rules that can only be true or false, but also need some other evidence to hold to prove it is a non-trivial
-- TODO not so sure how to describ/generalize this one
data NontrivRule = NontrivRule {
   antiRuleData :: AntiRule
  ,trivialityEvidence :: Int
  } deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON,NFData)

class Countable a where 
  add :: a -> a -> a

-- used the KeywordCoor, tot counts how many times either keyword appeared
-- this computation is handled seperatly, so when adding rules dont touch tot
instance Countable AntiRule where
  add (AntiRule tru fls tot) (AntiRule tru' fls' tot') =
    AntiRule (tru+tru') (fls+fls') (tot+tot')

-- used in KeyValKey, tot counts how many times k1 and v1 appear
-- this computation is handled seperatly, so when adding rules dont touch tot
instance Countable NontrivRule where
  add (NontrivRule antiR otherEvidence) (NontrivRule antiR' otherEvidence') =
    NontrivRule (add antiR antiR') (otherEvidence+otherEvidence')

instance Countable Formula where
  add (Formula gt lt eq) (Formula gt' lt' eq') = 
    Formula (gt+gt') (lt+lt') (eq+eq')

instance Countable QType where
  add (QType f1 f2 f3 f4 f5) (QType f1' f2' f3' f4' f5') =
    QType (f1+f1') (f2+f2') (f3+f3') (f4+f4') (f5+f5')
