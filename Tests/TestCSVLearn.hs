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
  expectedResults <- readFile "benchmarks/CSVTest/TestCSVLearn_results.json"
  actualResults   <- readFile cachedRulesDefaultLoc

  if expectedResults == actualResults
    then return ()
    else putStrLn ("Generated unexpected rule set for CSV Test: \n\n"++ (actualResults)) >> exitFailure


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
        intRelSupport = 1 --min true
      , intRelConfidence = 0 --max false
      , fineGrainSupport = 1
      , fineGrainConfidence = 0
      , keywordCoorSupport = 3
      , keywordCoorConfidence = 1
      , keyValKeyCoorSupport = 2
      , keyValKeyCoorConfidence = 0 
      , trivEvidenceThreshold = 0
      , orderSupport = 3
      , orderConfidence = 1 
      , typeSupport = 1
      , typeConfidence = 1 
      }


