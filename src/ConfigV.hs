{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-cse #-}

module ConfigV (
  executeLearning,
  executeVerification,
  module ConfigV.Settings.Config
  ) where


import qualified Data.Aeson            as A
import qualified Data.ByteString.Lazy  as B
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import Data.List

import qualified ConfigV.LearningEngine as L
import           ConfigV.Checker
import           ConfigV.OutputPrinter
import ConfigV.Utils
import ConfigV.BuildImplGraph

import ConfigV.Types.Rules
import ConfigV.Types.IR
import ConfigV.Types.Common

import ConfigV.Types.JSON

import System.Console.CmdArgs()
import ConfigV.Settings.Config

import           System.Directory

executeLearning :: Options -> Either RawThresholds PercentageThresholds -> IO()
executeLearning settings userThresholds = do
  checkSettings settings
  putStrLn $ "Learning on directoty: \n"++(learnTarget settings)
  putStrLn $ "Using file limit: \n"++(show $ learnFileLimit settings)
  thresholds <- 
    case userThresholds of
      Left rawThresholds -> return rawThresholds
      Right percentageThresholds -> calcThresholds settings percentageThresholds
  let configVconfig = ConfigVConfiguration { 
                        optionsSettings = settings, 
                        thresholdSettings = thresholds}
  targets <- gatherLearnTargets settings
  let learnedRules = L.learnRules configVconfig targets
      learnedRulesL = toLists learnedRules :: RuleSetLists
  B.writeFile (cacheLocation settings) $ A.encode learnedRulesL 
  writeFile ((cacheLocation settings)++".pretty") $ show learnedRulesL 
  putStrLn $ "Learned rules: \n"++(ruleSizes learnedRulesL)
  print learnedRulesL
 
  putStrLn $ "Building Implication Lattice: "
  implGraph $ smtRules learnedRules
  
executeVerification :: Options -> _
executeVerification settings = do
  rules <- (fromLists. fromJust. A.decode) <$> (B.readFile $ cacheLocation settings)
  degrees <- ((M.fromList. fromJust. A.decode) <$> B.readFile "graphAnalysis/sorted_degrees.json") :: IO (M.Map Keyword Double)
  vFiles <- mapM T.readFile (vFilePaths settings) :: IO [T.Text]
  let vTargets = zip3 
               (vFilePaths settings) --the names of the files
               vFiles -- the file data
               (repeat $ language settings) :: [ConfigFile Language]
  fitness <- runVerify settings rules degrees vTargets
  return ()


gatherLearnTargets :: Options -> IO [ConfigFile Language]
gatherLearnTargets Learning{..} = do
  fs'' <- listDirectory learnTarget
  let fs' = case language of
             CSV -> filter (isSuffixOf "csv") fs''
             _   -> fs''
  let fs = map (learnTarget++) fs'
  fContents <- mapM T.readFile fs
  let cs = zipWith 
             (\fName fc -> (fName, fc, language)) 
             fs 
             fContents
  return cs

vFilePaths :: Options -> [FilePath]
vFilePaths Verification{..} = 
    if '.' `elem` verifyTarget
    then [verifyTarget]
    else map ((verifyTarget++"/")++) $ u $ listDirectory verifyTarget

runVerify :: _ -> RuleSet -> M.Map Keyword Double -> [ConfigFile Language] -> IO Int
runVerify settings rules ds vTargets  = do  
 
  let errors = map (verifyOn settings rules) vTargets
  fitnesses <- 
    mapM print $ sortOn (\(f,es) -> length es) (zip (map (\(x,y,z)->x) vTargets) errors) 
  --printSummary errors fitnesses
  return 0 -- $ sum fitnesses
  

-- | to check integrity of cache, rerun learning, even if useCache is on, and compare
--   TODO, only allow this to run if useCache is on?
{-
checkCache settings cache = do
  let freshRules = LearningEngine.learnRules settings Bench.learnTarget
  when (freshRules /= cache) (fail  "error in json cache")
-}
