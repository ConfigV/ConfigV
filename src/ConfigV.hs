{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-cse #-}
module ConfigV (
  executeLearning,
  executeVerification,
  module Settings.Config
  ) where


import qualified Data.Aeson            as A
import qualified Data.ByteString.Lazy  as B
import qualified Data.List             as L
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.IO          as T

import           System.IO
import qualified GHC.IO.Encoding       as G

import qualified Benchmarks as Bench
import qualified LearningEngine
import           Checker
import           OutputPrinter
import Utils

import Types.Rules
import Types.IR
import Types.Common

import Types.JSON

import System.Console.CmdArgs
import Settings.Config

import           System.Directory

executeLearning :: Options -> Either RawThresholds PercentageThresholds -> IO()
executeLearning settings userThresholds = do
  checkSettings settings
  thresholds <- 
    case userThresholds of
      Left rawThresholds -> return rawThresholds
      Right percentageThresholds -> calcThresholds settings percentageThresholds
  let configVconfig = ConfigVConfiguration { 
                        optionsSettings = settings, 
                        thresholdSettings = thresholds}
  targets <- gatherLearnTargets settings
  let learnedRules = 
        (toLists $ LearningEngine.learnRules configVconfig targets) :: RuleSetLists

  B.writeFile (cacheLocation settings) $ A.encode learnedRules 
  putStrLn $ "Learned rules: \n"++(ruleSizes learnedRules)

executeVerification settings = do
  (fromLists. fromJust. A.decode) <$> (B.readFile $ cacheLocation settings)
  degrees <- ((M.fromList. fromJust. A.decode) <$> B.readFile "graphAnalysis/sorted_degrees.json") :: IO (M.Map Keyword Double)
  vFiles <- mapM T.readFile (vFilePaths settings) :: IO [T.Text]
  let vTargets = zip3 
               (vFilePaths settings) --the names of the files
               vFiles -- the file data
               (repeat $ language settings) :: [ConfigFile Language]
  return ()
  --fitness <- runVerify rules degrees vTargets


gatherLearnTargets :: Options -> IO [ConfigFile Language]
gatherLearnTargets Learning{..} = do
  fs' <- listDirectory learnTarget
  let fs = map (learnTarget++) fs'
  fContents <- mapM T.readFile fs
  let cs = zipWith 
             (\fName fc -> (fName, fc, language)) 
             fs 
             fContents
  return cs

vFilePaths :: Options -> [FilePath]
vFilePaths Verification{..} = 
    map ((verifyTarget++"/")++) $ u $ listDirectory verifyTarget
    --benchmarkFiles = map getFileName $ concat benchmarks --TODO unused atm

runVerify ::  RuleSet -> M.Map Keyword Double -> [ConfigFile Language] -> IO Int
runVerify rules ds vTargets  = do  
  print "Under construction"
  return 0
  {-
  let errors = map (verifyOn rules) vTargets
  fitnesses <- 
    if False --Settings.benchmarks
    then undefined --zipWithM reportBenchmarkPerformance Bench.benchmarks errors 
    else mapM (reportUserPerformance ds) $ L.sortOn (\(f,es) -> length es) (zip (map (\(x,y,z)->x) vTargets) errors) 
  printSummary errors fitnesses
  return $ sum fitnesses
  -}

-- | to check integrity of cache, rerun learning, even if useCache is on, and compare
--   TODO, only allow this to run if useCache is on?
{-
checkCache settings cache = do
  let freshRules = LearningEngine.learnRules settings Bench.learnTarget
  when (freshRules /= cache) (fail  "error in json cache")
-}
