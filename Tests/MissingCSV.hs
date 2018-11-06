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
  expectedResults <- readFile "benchmarks/MissingCSV/Missing_results.json"
  actualResults   <- readFile cachedRulesDefaultLoc

  if expectedResults == actualResults
    then return ()
    else putStrLn ("Generated unexpected rule set for CSV Test: \n\n"++ (actualResults)) >> exitFailure


settings = learnConfig {
        learnTarget = "benchmarks/MissingCSV/"
      , enableMissing = True
      , verbose = True
      }

csvThresholds = defaultThresholds {
        keywordCoorSupport = 0
      , keywordCoorConfidence = 1
      }


