module Main where

import           ConfigV
import qualified GHC.IO.Encoding       as G
import           System.IO
import           System.Exit
import Data.Char

main = do
  G.setLocaleEncoding utf8
  G.setFileSystemEncoding utf8
  G.setForeignEncoding utf8  
 
  executeLearning settings (Left csvThresholds)
  expectedResults <- readFile "Datasets/benchmarks/CSVTest/TestCSVLearn_results.json"
  actualResults   <- readFile cachedRulesDefaultLoc

  let trim = takeWhile (not. isSpace)
  if (trim expectedResults) == (trim actualResults)
    then return ()
    else do
             putStrLn ("Generated unexpected rule set for CSVTest: \n\n"++ (actualResults))
             putStrLn ("expected rule set: \n\n"++ (expectedResults)) >> exitFailure



settings = learnConfig {
        learnTarget = "Datasets/benchmarks/CSVTest/"
      , enableOrder = True
      , enableMissing = True
      , enableKeyvalkey = True
      , enableCoarseGrain = True
      , enableFineGrain = True
      , enableSMT = True
      , verbose = True
      }

csvThresholds = RawThresholds {
        intRelSupport = 1 --min true
      , intRelConfidence = 0 --max false
      , fineGrainSupport = 1
      , fineGrainConfidence = 0
      , keywordCoorSupport = 3
      , keywordCoorConfidence = 0
      , keyValKeyCoorSupport = 2
      , keyValKeyCoorConfidence = 0 
      , trivEvidenceThreshold = 0
      , orderSupport = 3
      , orderConfidence = 1 
      , typeSupport = 1
      , typeConfidence = 1 
      }


