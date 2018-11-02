module Main where

import System.Console.CmdArgs
import ConfigV
import qualified GHC.IO.Encoding       as G
import           System.IO

main = do
  G.setLocaleEncoding utf8
  G.setFileSystemEncoding utf8
  G.setForeignEncoding utf8  

  settings <- cmdArgsRun mode
  case settings of
    Learning {} -> do 
      executeLearning settings

    Verification {} -> do
      executeVerification settings
      return ()


