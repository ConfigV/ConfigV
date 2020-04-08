{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE LambdaCase #-} 

module ConfigV.Types.SMTRules where

import ConfigV.Types.Common
import ConfigV.Types.IR
import ConfigV.Types.Locatable

import Control.Monad.Omega

import           Data.Aeson
import           GHC.Generics     (Generic)
import Control.DeepSeq

import Algebra.PartialOrd
import Data.Interned.Text

-- TODO add IntRels
data SMTFormula = SMTFormula {
         antecedent :: SMTSubFormula -- ^ called source in ARL
       , consequent :: SMTSubFormula -- ^ called target in ARL
       }
  deriving (Eq, Ord,Generic,ToJSON,FromJSON,NFData)

data SMTSubFormula = 
    And SMTSubFormula SMTSubFormula
  | IsSet Keyword 
  | IsSetTo Keyword Val
  | STrue
  deriving (Eq, Ord,Generic,ToJSON,FromJSON,NFData)

instance Show SMTFormula where
  show SMTFormula{..} = show antecedent ++ " => " ++ show consequent

instance Show SMTSubFormula where
  show = \case
    And s1 s2 -> show s1 ++ " /\\ " ++ show s2
    IsSet k -> "isSet(" ++ show k ++ ")"
    IsSetTo k v -> "isSetTo(" ++ show k ++ ", " ++ show v ++ ")"
    STrue -> "True"

shortShow :: SMTSubFormula -> String
shortShow = \case
    And s1 s2 -> shortShow s1 ++ " /\\ " ++ shortShow s2
    IsSet k -> "isSet(" ++ (s k) ++ ")"
    IsSetTo k v -> "isSetTo(" ++ (s k) ++ ", " ++ (s v) ++ ")"
    STrue -> "T"
  where 
   s = show. internedTextId

instance Locatable SMTFormula where
  keys SMTFormula{..} = keys antecedent ++ keys consequent

instance Locatable SMTSubFormula where
  keys = \case
    And s1 s2     -> keys s1 ++ keys s2
    IsSet k       -> [k]
    IsSetTo k _   -> [k]
    STrue         -> []

instance PartialOrd SMTFormula where
  leq r1 r2 = 
    antecedent r1 `implies` antecedent r2 &&
    consequent r2 `implies` consequent r1

-- Util functions for SMTFormula - TODO move to seperate module
constructImplication :: Omega (SMTSubFormula -> SMTSubFormula -> SMTFormula)
constructImplication = each [
     (\s1 s2 -> SMTFormula {antecedent = s1, consequent = s2})
   , (\s1 s2 -> SMTFormula {antecedent = s2, consequent = s1})
   , (\s1 s2 -> SMTFormula {antecedent = STrue, consequent = And s1 s2})
   ]

-- TODO add IntRels
terms1 :: [IRLine -> SMTSubFormula]
terms1 = [ \IRLine{..} -> IsSet keyword
         , \IRLine{..} -> IsSetTo keyword value]

formulasSize1 :: [IRLine -> SMTFormula]
formulasSize1 =
  map (\t ir -> SMTFormula {antecedent = STrue, consequent = t ir}) terms1

formulasSize2 :: Omega (IRLine -> IRLine -> SMTFormula)
formulasSize2 = do
  c <- constructImplication
  (ts1, ts2) <- each [(t1,t2) | t1 <- terms1, t2 <- terms1]
  return $ \ir1 ir2 -> c (ts1 ir1) (ts2 ir2)

appliers :: Omega (SMTSubFormula -> SMTSubFormula -> SMTSubFormula)
appliers = each [And] --TODO allow Or?

templatesArity2 :: Omega (IRLine -> IRLine -> SMTSubFormula)
templatesArity2 = do 
  (ts1, ts2) <- each [(t1,t2) | t1 <- terms1, t2 <- terms1]
  as <- appliers
  return $ (\ir1 ir2 -> as (ts1 ir1) (ts2 ir2)) 

-- TODO make arity parameterized by Int?
templatesArity3 :: [IRLine -> IRLine -> IRLine -> SMTSubFormula]
templatesArity3 = undefined 

{-runOmega $ do
  ts  <- each terms1
  ts2 <- templatesArity2
  as  <- appliers
  each [\ir1 ir2 ir3 -> as (ts ir1) (ts2 ir2 ir3)
       ,\ir1 ir2 ir3 -> as (ts2 ir1 ir2) (ts ir3)]
-}

containsIsSetTo :: SMTFormula -> Bool
containsIsSetTo SMTFormula{..} = containsIsSetTo' antecedent || containsIsSetTo' consequent

containsIsSetTo' :: SMTSubFormula -> Bool
containsIsSetTo' (IsSetTo _ _) = True
containsIsSetTo' (And s1 s2) = containsIsSetTo' s1 || containsIsSetTo' s2
containsIsSetTo' _ = False

-- TODO this would be much nicer to check with an SMT solver, even though it is just constants
-- there are just so many ways to mess this up
implies :: SMTSubFormula-> SMTSubFormula -> Bool
implies s1 s2 
  | s1 == s2 = True
  | STrue <- s1 = True
  | (And s2left s2right) <- s2 = --we need to split s2 Ands before splitting s1 Ands
       s1 `implies` s2left && s1 `implies` s2right
  | (And s1left s1right) <- s1 = 
       s1left `implies` s2 || s1right `implies` s2 ||
       (And s1right s1left) == s2
  | (IsSetTo k1 _) <- s1, 
    (IsSet k2)     <- s2 = k1 == k2
  | otherwise = False

