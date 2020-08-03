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

--  executeVerification verifyConfig { verifyTarget = "Datasets/rahul/ConfigVExperiments/exp2/test2/MS121_A2__10X_17_grch38_CTGCGGAAGTCGATAAx.csv" }
 
  return ()

settings = learnConfig {
        learnTarget = "../../COVID19-Data/normalized_pos_input_files/HALLMARK_ADIPOGENESIS/B/HC1/"
      , learnFileLimit = 50
      , enableCoarseGrain = True
      , enableFineGrain = True
      , enableOrder = False
      , verbose = True
      }

pthresholds = defaultPercentageThresholds {
        intRelSupport_P = 0.01
      , intRelConfidence_P = 0.95
      , fineGrainSupport_P = 0.1
      , fineGrainConfidence_P = 0.9
      }
