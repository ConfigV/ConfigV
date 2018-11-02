module Main where

import           ConfigV
import qualified GHC.IO.Encoding       as G
import           System.IO
import           System.Exit

main = do
  G.setLocaleEncoding utf8
  G.setFileSystemEncoding utf8
  G.setForeignEncoding utf8  
 
  let settings = learnConfig {
          learnTarget = "benchmarks/CSVTest/"
        , enableOrder = True
        , enableMissing = True
        , enableKeyvalkey = True
        , enableCoarseGrain = True
        , enableFineGrain = True
        }

  executeLearning settings
  expectedResults <- readFile "Tests/TestCSVLearn_results.json"
  actualResults   <- readFile cachedRulesDefaultLoc

  if expectedResults == actualResults
    then return ()
    else putStrLn ("Generated unexpected rule set for CSV Test: \n\n"++ (show actualResults)) >> exitFailure


