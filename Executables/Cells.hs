module Main where

import           ConfigV
import qualified GHC.IO.Encoding       as G
import           System.IO
import           System.Exit

main = do
  G.setLocaleEncoding utf8
  G.setFileSystemEncoding utf8
  G.setForeignEncoding utf8  
 
  executeLearning settings (Left csvThresholds)
  actualResults   <- readFile cachedRulesDefaultLoc
  return ()
--  putStrLn actualResults

settings = learnConfig {
        learnTarget = "Datasets/rahul/ConfigVExperiments/exp2/test1/"
      , learnFileLimit = 999
      , enableMissing = True
      , enableCoarseGrain = True
      , enableFineGrain = True
      , verbose = True
      }

csvThresholds = defaultThresholds {
        keywordCoorSupport = 4
      , keywordCoorConfidence = 0
      , intRelSupport = 5
      , intRelConfidence = 0
      , fineGrainSupport = 6
      , fineGrainConfidence = 0
      }


