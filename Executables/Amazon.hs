module Main where

import           ConfigV
import qualified GHC.IO.Encoding       as G
import           System.IO
import           System.Exit

main = do
  G.setLocaleEncoding utf8
  G.setFileSystemEncoding utf8
  G.setForeignEncoding utf8  
 
  executeLearning settings csvThresholds
  actualResults   <- readFile cachedRulesDefaultLoc
 
  putStrLn actualResults

settings = learnConfig {
        learnTarget = "Datasets/antonDumpCSV/"
      , enableSMT = True
      , verbose = True
      , learnFileLimit = 80
      }

csvThresholds = Left $ defaultThresholds {
        keywordCoorSupport = 15
      , keywordCoorConfidence = 2
      }


