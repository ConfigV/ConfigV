{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}

module Settings.Options where
import System.Console.CmdArgs

import Types.IR
import Control.Monad
import System.Exit

data Options
  = Learning {
    learnTarget :: FilePath
  , language :: Language
  , enableOrder :: Bool
  , enableMissing :: Bool
  , enableKeyvalkey :: Bool
  , enableCoarseGrain :: Bool
  , enableFineGrain :: Bool 
  , enableTypeRules :: Bool
  , enableProbTypeInference :: Bool
  , cacheLocation :: FilePath
  , thresholdsPath :: FilePath
  , learnFileLimit :: Int
  , verbose :: Bool
  }

  | Verification {
    verifyTarget :: FilePath
  , cacheLocation :: String
  , sortWithRuleGraph :: Bool
  , language :: Language
  , verbose :: Bool
  } deriving (Show, Data, Typeable)

mode = cmdArgsMode $ modes [learnConfig, verifyConfig] &= program "ConfigV" &= summary "ConfigV v0.0.1"

checkSettings Learning{..} = do
  when (learnTarget == "") $ die "ConfigV: No learning target provided. See help."

learnConfig = Learning {
    learnTarget = "" &= help "The files to learn from" &= typDir
  , language = CSV &= help ("The language of the verification files, select from "++(show allLanguages))
  , enableOrder = False
  , enableMissing = False
  , enableKeyvalkey = False
  , enableCoarseGrain = False
  , enableFineGrain = False
  , enableTypeRules = False
  , enableProbTypeInference = False
  , cacheLocation = "cachedRules.json" &= help "The location where the cache of learned rules wll be written" &= typFile
  , thresholdsPath = def &= help "The location from which to read threshold values (unsupported)" &= typFile
  , learnFileLimit = 9999 &= help "the limit for files to be used in learning. useful for benchmarking learning times" &= typ "INT"
  , verbose = False
  } &=
    help "Use Predicate Rule Learning to learn rules"

verifyConfig = Verification {
    verifyTarget = def &= help "The files to verify." &= typDir
  , cacheLocation = "cachedRule.json" &= help "The location of the cache file to read rules from"
  , sortWithRuleGraph = def &= help "Turn on rule graph sorting, rerun the rule graph builder for each training set for best results (see graphAnalysis/README)"
  , language = CSV &= help ("The language of the verification files, select from "++(show allLanguages))
  , verbose = False
  } &=
    help "Verify Configuration files from a set of learned rules"

