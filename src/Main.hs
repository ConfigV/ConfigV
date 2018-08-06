{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import qualified Data.List             as L
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Data.Time.Clock.POSIX

import           Control.Applicative
import           Control.Monad
import           System.IO
import           System.Environment
import qualified GHC.IO.Encoding       as G
import           Text.Printf

import qualified Benchmarks as Bench
import           Convert
import qualified LearningEngine
import           Checker
import           Utils
import qualified Settings
import           OutputPrinter

import Types.Rules
import Types.IR
import Types.Common
import Types.JSON
import Types.Errors

main = do
  G.setLocaleEncoding utf8
  G.setFileSystemEncoding utf8
  G.setForeignEncoding utf8  
  
  degrees <- ((M.fromList. fromJust. decode) <$> B.readFile "graphAnalysis/sorted_degrees.json") :: IO (M.Map Keyword Double)
 
  vFiles <- mapM T.readFile Bench.vFilePaths :: IO [T.Text]
  let vTargets = zip3 
                   Bench.vFilePaths 
                   vFiles
                   (repeat Settings.language) :: [ConfigFile Language]

  rules <- getRules 
  --checkCache rules

  --fitness <- runVerify rules degrees vTargets
  return ()

getRules :: IO RuleSet
getRules = do
  let learnedRules = (toLists $ LearningEngine.learnRules Bench.learnTarget) :: RuleSetLists
  unless Settings.useCache $ B.writeFile Settings.cacheLocation $ encode learnedRules 
  unless Settings.useCache $ putStrLn $ "Learned rules: \n"++(ruleSizes learnedRules)
  (fromLists. fromJust. decode) <$> B.readFile Settings.cacheLocation

runVerify ::  RuleSet -> M.Map Keyword Double -> [ConfigFile Language] -> IO Int
runVerify rules ds vTargets  = do
  let errors = map (verifyOn rules) vTargets
  fitnesses <- 
    if Settings.benchmarks
    then zipWithM reportBenchmarkPerformance Bench.benchmarks errors 
    else mapM (reportUserPerformance ds) $ L.sortOn (\(f,es) -> length es) (zip (map (\(x,y,z)->x) vTargets) errors) 
  printSummary errors fitnesses
  return $ sum fitnesses


-- | to check integrity of cache, rerun learning, even if useCache is on, and compare
--   TODO, only allow this to run if useCache is on?
checkCache cache = do
  let freshRules = LearningEngine.learnRules Bench.learnTarget
  when (freshRules /= cache) (fail  "error in json cache")

