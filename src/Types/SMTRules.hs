{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE LambdaCase #-} 

module Types.SMTRules where

import Types.Common
import Types.IR
import Types.Countable
import Types.Locatable

import Control.Applicative
import Control.Monad.Omega
import Control.Monad

import           Data.Aeson
import           GHC.Generics     (Generic)
import Control.DeepSeq

terms1 :: [IRLine -> SMTFormula]
terms1 = [\IRLine{..} -> IsSet keyword, \IRLine{..} -> IsSetTo keyword value]

appliers :: Omega (SMTFormula -> SMTFormula -> SMTFormula)
appliers = each [And, Implies]

terms2 :: Omega (IRLine -> IRLine -> SMTFormula)
terms2 = do 
  ts <- each terms1
  as <- appliers
  return $ (\ir1 ir2 -> as (ts ir1) (ts ir2)) 


terms3 :: [IRLine -> IRLine -> IRLine -> SMTFormula]
terms3 = runOmega $ do
  ts  <- each terms1
  ts2 <- terms2
  as  <- appliers
  return $ each [\ir1 ir2 ir3 -> as (ts ir1) (ts2 ir2 ir3)
                ,\ir1 ir2 ir3 -> as (ts2 ir1 ir2) (ts ir3)]

data SMTFormula =
    And SMTFormula SMTFormula
  | Implies SMTFormula SMTFormula
  | IsSet Keyword 
  | IsSetTo Keyword Val
  deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON,NFData)

data SMTRule = SMTRule
  { smtFormula :: SMTFormula
  } deriving (Eq, Show,Ord,Generic,ToJSON,FromJSON,NFData)

instance Locatable SMTRule where
  keys (SMTRule {..}) = keys smtFormula

instance Locatable SMTFormula where
  keys = \case
    And s1 s2     -> keys s1 ++ keys s2
    Implies s1 s2 -> keys s1 ++ keys s2
    IsSet k       -> [k]
    IsSetTo k _   -> [k]

