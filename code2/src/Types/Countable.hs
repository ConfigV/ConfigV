{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Countable where

import Types.Common

import qualified Data.Map        as M
import           Data.Aeson
import           GHC.Generics     (Generic)

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
} deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON)

-- TODO actual formula here
type Formula = Int
data AntiRule = AntiRule {
   tru :: Int
  ,fls :: Int
  ,tot :: Int
  } deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON)

class Countable a where 
  add :: a -> a -> a

instance Countable AntiRule where
  add (AntiRule tru fls tot) (AntiRule tru' fls' tot') =
    AntiRule (tru+tru') (fls+fls') (tot+tot')

instance Countable Formula where
  add f1 f2 = f1

instance Countable QType where
  add (QType f1 f2 f3 f4 f5) (QType f1' f2' f3' f4' f5') =
    QType (f1+f1') (f2+f2') (f3+f3') (f4+f4') (f5+f5')
