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
      , enableKeyvalkey = True
      , verbose = True
      , learnFileLimit = 10
      }

csvThresholds = Left $ defaultThresholds {
        keywordCoorSupport = 3
      , keywordCoorConfidence = 1
      , keyValKeyCoorSupport = 4
      , keyValKeyCoorConfidence = 1
      }


