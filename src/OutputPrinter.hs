{-# LANGUAGE MultiWayIf #-}
module OutputPrinter where

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

import           Debug.Trace

import qualified Benchmarks as Bench
import           Convert
import           LearningEngine
import           Checker
import           Utils
import qualified Settings

import Types.Rules
import Types.IR
import Types.Common
import Types.JSON
import Types.Errors


printSummary :: [ErrorReport] -> [Int] -> IO ()
printSummary ers fitnesses = do
  putStrLn "------------"
  putStrLn $ "Found "++(show $sum fitnesses)++" errors in "++(show $ length $ filter (>0) fitnesses)++"/"++(show $ length fitnesses)++" files."
  let numOfClass x = (show x)++", "++(show $ length $ filter (\e-> errIdent e == x) (concat ers))
  putStrLn "------------"
  putStrLn "Table of results"
  putStrLn "Error Class, Number, Support Threshold, Confidence Threshold"
  putStrLn $ numOfClass TYPE++", "++(roundToStr 2 (Settings.typeSupport `percent` Settings.totalFiles))++"%, "++(roundToStr 2 (100*Settings.typeConfidence))++"%"
  putStrLn $ numOfClass INTREL++", "++(roundToStr 2 (Settings.intRelSupport `percent` Settings.totalFiles))++"%, "++(roundToStr 2 ((Settings.intRelSupport -Settings.intRelConfidence) `percent` Settings.intRelSupport))++"%"
  putStrLn $ numOfClass FINEGRAINED++", "++(roundToStr 2 (Settings.fineGrainSupport `percent` Settings.totalFiles))++"%, "++(roundToStr 2 ((Settings.fineGrainSupport -Settings.fineGrainConfidence) `percent` Settings.fineGrainSupport))++"%"
  putStrLn $ numOfClass MISSING++", "++(roundToStr 2 (Settings.keywordCoorSupport `percent` Settings.totalFiles))++"%, "++(roundToStr 2 ((Settings.keywordCoorSupport -Settings.keywordCoorConfidence) `percent` Settings.keywordCoorSupport))++"%"
  putStrLn $ numOfClass ORDERING++", "++(roundToStr 2 (Settings.orderSupport `percent` Settings.totalFiles))++"%, "++(roundToStr 2 ((Settings.orderSupport -Settings.orderConfidence) `percent` Settings.orderSupport))++"%"
  putStrLn $ "Total, "++(show $sum fitnesses)++", -, -"
  putStrLn "------------"
  putStrLn "Histogram of Errors"
  print fitnesses

-- print and how many errs
reportUserPerformance :: M.Map Keyword Double -> (FilePath,ErrorReport) -> IO Int
reportUserPerformance ds (f,es) = do
  print f
  when (Settings.sortingStyle == Settings.Support)  $ print $ L.sortBy supportComp es
  when (Settings.sortingStyle == Settings.RuleGraphDegree) $ print $ L.sortBy (degreeComp ds) es
  return $ length es

degreeComp :: M.Map Keyword Double -> Error -> Error -> Prelude.Ordering
degreeComp ds e1 e2 = let
  findK k = M.findWithDefault 0 k ds
  calcD e = (sum $ map (\ks -> findK $ snd ks) $ errLocs e) / ((fromIntegral $ length $ errLocs e))
  e1s = calcD e1
  e2s = calcD e2
 in
  if
   | e1s > e2s -> GT
   | e1s < e2s -> LT
   | otherwise -> EQ

supportComp e1 e2 = if
  | errSupport e1 > errSupport e2 -> LT
  | errSupport e1 < errSupport e2 -> GT
  | otherwise -> EQ


 -- | compare the original benchark spec to the generated one
reportBenchmarkPerformance :: ErrorReport -> ErrorReport -> IO Int
reportBenchmarkPerformance spec foundErrs =
  let
    truePos = filter ((==) $ head spec) foundErrs
    passing = (length truePos) > 0
    falsePos = filter ((/=) $ head spec) foundErrs
    -- larger fitness is worse performance over benchmarks
    fitness =
      100 * fromEnum (not passing) --large penalty for not passing
      + length falsePos
  in do
    --startT <- getPOSIXTime --this is the right place to put it because of lazy eval
    putStrLn (getFileName $ head spec)
    putStrLn $ "    Passing: " ++ show passing
    putStrLn $ "    True Positives: "++ show (length truePos)
    putStrLn $ "    False Positives: "++ show (length falsePos)
    when Settings.verbose $ putStrLn $ "Specification :   \n" ++ show spec
    when Settings.verbose $ putStrLn $ "True Errors :    \n" ++ unlines (map show $ take 10 truePos)
    when Settings.verbose $ putStrLn $ "False Positives : \n" ++ unlines (map show $ take 10 falsePos)
    putStrLn ""
    --endT <- liftM2 (-) getPOSIXTime (return startT)
    --print endT
    --hFlush stdout
    return fitness
