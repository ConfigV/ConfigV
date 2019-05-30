module Main where

import           ConfigV
import qualified GHC.IO.Encoding       as G
import           System.IO
import           System.Exit

main = do
  G.setLocaleEncoding utf8
  G.setFileSystemEncoding utf8
  G.setForeignEncoding utf8  
 
  executeLearning settings (Right pthresholds)
  actualResults   <- readFile cachedRulesDefaultLoc
--  putStrLn actualResults

  executeVerification verifyConfig { verifyTarget = "Datasets/rahul/ConfigVExperiments/exp2/test2/MS121_A2__10X_17_grch38_CTGCGGAAGTCGATAAx.csv" }
 
  return ()

settings = learnConfig {
        learnTarget = "Datasets/rahul/ConfigVExperiments/exp2/train/"
      , learnFileLimit = 999
      , enableMissing = True
      , enableCoarseGrain = True
      , enableFineGrain = True
      , verbose = True
      }

pthresholds = defaultPercentageThresholds {
        intRelSupport_P = 0.01
      , intRelConfidence_P = 0.95
      , fineGrainSupport_P = 0.01
      , fineGrainConfidence_P = 0.95
      , keywordCoorSupport_P = 0.01
      , keywordCoorConfidence_P = 0.95
      , orderSupport_P = 0.01
      , orderConfidence_P = 0.95
      }

csvThresholds = defaultThresholds {
        keywordCoorSupport = 4
      , keywordCoorConfidence = 1
      , intRelSupport = 5
      , intRelConfidence = 1
      , fineGrainSupport = 6
      , fineGrainConfidence = 1
      }


