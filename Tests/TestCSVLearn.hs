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
  expectedResults <- readFile "Tests/TestCSVLearn_results.json"
  actualResults   <- readFile cachedRulesDefaultLoc

  if expectedResults == actualResults
    then return ()
    else putStrLn ("Generated unexpected rule set for CSV Test: \n\n"++ (show actualResults)) >> exitFailure


settings = learnConfig {
        learnTarget = "benchmarks/CSVTest/"
      , enableOrder = True
      , enableMissing = True
      , enableKeyvalkey = True
      , enableCoarseGrain = True
      , enableFineGrain = True
      , verbose = True
      }

csvThresholds = RawThresholds {
        intRelSupport = 1
      , intRelConfidence = 0
      , fineGrainSupport = 3
      , fineGrainConfidence = 0
      , keywordCoorSupport = 2
      , keywordCoorConfidence = 1
      , keyValKeyCoorSupport = 2
      , keyValKeyCoorConfidence = 0 
      , trivEvidenceThreshold = 0
      , orderSupport = 2
      , orderConfidence = 1 
      , typeSupport = 1
      , typeConfidence = 1 
      }


