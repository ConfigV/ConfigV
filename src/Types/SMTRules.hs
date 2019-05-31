{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE LambdaCase #-} 

module Types.SMTRules where

import Types.Common
import Types.IR
import Types.Locatable

import Control.Monad.Omega

import           Data.Aeson
import           GHC.Generics     (Generic)
import Control.DeepSeq

-- TODO add IntRels
terms1 :: [IRLine -> SMTFormula]
terms1 = [ \IRLine{..} -> IsSet keyword
         , \IRLine{..} -> IsSetTo keyword value]

appliers :: Omega (SMTFormula -> SMTFormula -> SMTFormula)
appliers = each [And, Implies]

templatesArity2 :: Omega (IRLine -> IRLine -> SMTFormula)
templatesArity2 = do 
  (ts1, ts2) <- each [(t1,t2) | t1 <- terms1, t2 <- terms1]
  as <- appliers
  return $ (\ir1 ir2 -> as (ts1 ir1) (ts2 ir2)) 

-- TODO make arity parameterized by Int?
templatesArity3 :: [IRLine -> IRLine -> IRLine -> SMTFormula]
templatesArity3 = undefined 
--TODO allow Implies only at top level
{-runOmega $ do
  ts  <- each terms1
  ts2 <- templatesArity2
  as  <- appliers
  each [\ir1 ir2 ir3 -> as (ts ir1) (ts2 ir2 ir3)
       ,\ir1 ir2 ir3 -> as (ts2 ir1 ir2) (ts ir3)]
-}
antecedent :: SMTFormula -> Maybe SMTFormula
antecedent = \case
   Implies s1 s2 -> Just s1
   _ -> Nothing

consequent :: SMTFormula -> SMTFormula
consequent = \case
   Implies s1 s2 -> s2
   x -> x

containsIsSetTo :: SMTFormula -> Bool
containsIsSetTo (IsSetTo _ _) = True
containsIsSetTo (IsSet _) = False
containsIsSetTo (And s1 s2) = containsIsSetTo s1 || containsIsSetTo s2
containsIsSetTo (Implies s1 s2) = containsIsSetTo s1 || containsIsSetTo s2

-- TODO must have implication at top level ; enforce with GADTs?
-- TODO add IntRels
data SMTFormula =
    And SMTFormula SMTFormula
  | Implies SMTFormula SMTFormula
  | IsSet Keyword 
  | IsSetTo Keyword Val
  deriving (Eq, Ord,Generic,ToJSON,FromJSON,NFData)

-- TODO should have data SMTFormula a = ...?
-- instance Foldable SMTFormula where

instance Show SMTFormula where
  show = \case
    And s1 s2 -> show s1 ++ " /\\ " ++ show s2
    Implies s1 s2 -> show s1 ++ " => " ++ show s2
    IsSet k -> "isSet(" ++ show k ++ ")"
    IsSetTo k v -> "isSetTo(" ++ show k ++ ", " ++ show v ++ ")"

instance Locatable SMTFormula where
  keys = \case
    And s1 s2     -> keys s1 ++ keys s2
    Implies s1 s2 -> keys s1 ++ keys s2
    IsSet k       -> [k]
    IsSetTo k _   -> [k]

