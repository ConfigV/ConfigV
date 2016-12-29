{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import qualified Data.List             as L
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Data.Time.Clock.POSIX

import           Control.Applicative
import           Control.Monad
import           System.IO

import           Debug.Trace

import           Benchmarks
import           Convert
import           LearningEngine

import           Checker

import qualified Settings
import Types.Rules
import Types.IR
import Types.JSON
import Types.Errors

rules = learnRules learnTarget

main = do
  bs <- mapM T.readFile verificationTargets :: IO [T.Text]
  let vTargets = zip bs (replicate (length bs) MySQL)

  unless Settings.uSE_CACHE $ B.writeFile "cachedRules.json" $ encode ((toLists $ learnRules learnTarget):: RuleSetLists)
  cached <- (fromLists. fromJust. decode) <$> B.readFile "cachedRules.json"

  --to check integrity of cache
  --when (rules /= (fromLists. fromJust $ decode c)) (fail  "error in json cache")

  fitness <- runVerify cached vTargets
  return ()


runVerify ::  RuleSet -> [ConfigFile Language] -> IO Int
runVerify rules vTargets  = do
  let errors =  map (verifyOn rules) vTargets
  fitnesses <- 
    if Settings.bENCHMARKS 
    then zipWithM reportBenchmarkPerformance benchmarks errors 
    else mapM print errors >> return [0]
  return $ sum fitnesses

 -- | compare the original benchark spec to the generated one
reportBenchmarkPerformance :: ErrorReport -> ErrorReport -> IO Int
reportBenchmarkPerformance spec foundErrs =
  let
    truePos = head spec `elem` foundErrs
    falsePos = filter ((/=) $ head spec) foundErrs
    -- larger fitness is worse performance over benchmarks
    fitness =
      100 * fromEnum (not truePos) --large penalty for not passing
      + length falsePos
  in do
    --startT <- getPOSIXTime --this is the right place to put it because of lazy eval
    putStrLn (getFileName $ head spec)
    putStrLn $ "    Passing: " ++ show truePos
    putStrLn $ "    False Positives: "++ show (length falsePos)
    when Settings.vERBOSE $ putStrLn $ "Specification :   \n" ++ show spec
    when Settings.vERBOSE $ putStrLn $ "True Errors :    \n" ++ unlines (map show $ filter ((==) $ head spec) foundErrs)
    when (Settings.vERBOSE && (length falsePos < 10)) $ putStrLn $ "False Positives : \n" ++ unlines (map show falsePos)
    putStrLn ""
    --endT <- liftM2 (-) getPOSIXTime (return startT)
    --print endT
    --hFlush stdout
    return fitness
