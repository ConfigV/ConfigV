{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}

module Main where

import Preproc
import Usertime
import Convert

import Types

import Benchmarks

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import Control.Monad

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe

import Control.Applicative

import Debug.Trace

import qualified Settings

main = do
 bs <- mapM T.readFile benchmarkFiles :: IO [T.Text]
 let bs' = zip bs (replicate (length bs) MySQL)
 unless Settings.uSE_CACHE $ B.writeFile "cachedRules.json" $ encode (toLists rules:: RuleSetLists)
 cached <- B.readFile "cachedRules.json"
 fitness <- runVerify bs' (if Settings.uSE_CACHE then (fromLists. fromJust $ decode cached) else rules)
 putStrLn ("FITNESS : "++show fitness)
 return ()

rules =
  learnRules $ case Settings.pROBRULES of
  Settings.Test -> testLearnSet
  Settings.NonProb -> learningSet
  Settings.Prob -> bigLearningSet ++ learningSet

verifyCache c = when (rules /= (fromLists. fromJust $ decode c)) (fail  "error in json cache")

runVerify :: [ConfigFile Language] -> RuleSet -> IO Int
runVerify bs' rules = do
 let errors =  zipWith (verifyOn rules) bs' benchmarkFiles
 when Settings.vERBOSE $ mapM_ putStrLn $ showProbRules rules

 --mapM putStrLn (zipWith (++) benchmarks (map unlines errors))
 fitnesses <- zipWithM reportBenchmarkPerformance benchmarks errors
 return $ sum fitnesses

 -- | compare the original benchark spec to the generated one
reportBenchmarkPerformance :: ErrorReport -> ErrorReport -> IO Int
reportBenchmarkPerformance spec foundErrs =
  let
    truePos = head spec `elem` foundErrs
    falsePos = filter ((/=) $ head spec) foundErrs
    -- try to min fitness
    fitness =
      100 * (fromEnum $ not truePos) --large penalty for not passing
      + length falsePos
  in do
    putStrLn (getFileName $ head spec)
    putStrLn $ "    Passing: " ++ show truePos
    putStrLn $ "    False Positives: "++ show (length falsePos)
    when Settings.vERBOSE $ putStrLn $ "Specification :   \n" ++ show spec
    when Settings.vERBOSE $ putStrLn $ "Found Errors :    \n" ++ unlines (map show foundErrs)
    when Settings.vERBOSE $ putStrLn $ "False Positives : \n" ++ unlines (map show falsePos)
    putStrLn ""
    return fitness
