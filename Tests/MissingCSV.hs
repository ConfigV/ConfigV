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
  expectedResults <- readFile "Datasets/benchmarks/MissingCSV/Missing_results.json"
  actualResults   <- readFile cachedRulesDefaultLoc

  if (takeWhile (not. isSpace) expectedResults) == actualResults
    then verify 
    else do
             putStrLn ("Generated unexpected rule set for CSV-Missing Test: \n\n"++ (actualResults))
             putStrLn ("expected rule set: \n\n"++ (expectedResults)) >> exitFailure

verify = do
  executeVerification verifyConfig { verifyTarget = "Datasets/benchmarks/MissingCSV" }

settings = learnConfig {
        learnTarget = "Datasets/benchmarks/MissingCSV/"
      , enableSMT = True
      , verbose = True
      }

csvThresholds = defaultThresholds {
        smtSupport = 3
      , smtConfidence = 1
      }


