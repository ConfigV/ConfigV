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
 
  putStrLn actualResults

settings = learnConfig {
        learnTarget = "Datasets/antonDumpCSV"
      , enableMissing = True
      , verbose = True
      }

csvThresholds = defaultThresholds {
        keywordCoorSupport = 10
      , keywordCoorConfidence = 0
      }


