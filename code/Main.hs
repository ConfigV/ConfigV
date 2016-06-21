{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Maybe
import Data.Time.Clock.POSIX

import Control.Monad
import Control.Applicative
import System.IO

import Debug.Trace

import Preproc
import Usertime
import Convert
import Types
import Benchmarks
import qualified Settings

verificationTarget =
  if Settings.bENCHMARKS
    then benchmarkFiles
    else userFiles

main = do
 bs <- mapM T.readFile verificationTarget :: IO [T.Text]
 let bs' = zip bs (replicate (length bs) MySQL)

 --mapM putStrLn (zipWith (++) benchmarks (map unlines errors))
 --when Settings.bENCHMARKS $ zipWithM_ reportBenchmarkPerformance benchmarks errors
 --when (not Settings.bENCHMARKS) $ mapM_ putStrLn $ concat $ zipWith (\x y -> [show x,show y]) userFiles

 let rules = learnRules (learningSet)
 unless Settings.uSE_CACHE $ B.writeFile "cachedRules.json" $ encode (toLists rules:: RuleSetLists)
 cached <- B.readFile "cachedRules.json"
 --(putStrLn . show . listsLength) ((fromJust $ decode cached) :: RuleSetLists)


 fitness <- runVerify bs' (if Settings.uSE_CACHE then (fromLists. fromJust $ decode cached) else rules)
 --putStrLn ("FITNESS : "++show fitness)
 return ()

listsLength :: RuleSetLists -> Int
listsLength RuleSetLists{..} =
  sum [
    length orderl
    ,length missingl
    ,length typeErrl
    ,length missingPl
    ,length orderPl
    ,length intRelPl
    ]

--mapM_ printTiming [150, 200]
printTiming x = do
  startT <- getPOSIXTime
  print $ length $ show $ learnRules $ take x $ cycle bigLearningSet
  endT <- liftM2 (-) getPOSIXTime (return startT)
  print endT
  hFlush stdout

rules =
  learnRules $ case Settings.pROBRULES of
  Settings.Test -> testLearnSet
  Settings.NonProb -> learningSet
  Settings.Prob -> bigLearningSet

verifyCache c = when (rules /= (fromLists. fromJust $ decode c)) (fail  "error in json cache")

runVerify :: [ConfigFile Language] -> RuleSet -> IO Int
runVerify bs' rules = do
 let errors =  zipWith (verifyOn rules) bs' verificationTarget
 --when Settings.vERBOSE $ mapM_ putStrLn $ showProbRules rules
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


--lsDir = "dataset/correctMySQL/"
lsDir = "dataset/correctMySQL2/"
