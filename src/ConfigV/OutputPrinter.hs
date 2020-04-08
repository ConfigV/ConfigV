{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module ConfigV.OutputPrinter where

import qualified Data.List             as L
import qualified Data.Map             as M

import           Control.Monad

import           ConfigV.Utils
import ConfigV.Settings.Config

import ConfigV.Types.Common
import ConfigV.Types.Errors


printSummary :: RawThresholds -> [ErrorReport] -> [Int] -> IO ()
printSummary RawThresholds{..} ers fitnesses = do
  putStrLn "------------"
  putStrLn $ "Found "++(show $sum fitnesses)++" errors in "++(show $ length $ filter (>0) fitnesses)++"/"++(show $ length fitnesses)++" files."
  let numOfClass x = (show x)++", "++(show $ length $ filter (\e-> errIdent e == x) (concat ers))
  putStrLn "------------"
  putStrLn "Table of results"
  putStrLn "Error Class, Number, Support Threshold, Confidence Threshold"
{-  putStrLn $ numOfClass TYPE++", "++(roundToStr 2 (typeSupport `percent` totalFiles))++"%, "++(roundToStr 2 (100*typeConfidence))++"%"
  putStrLn $ numOfClass INTREL++", "++(roundToStr 2 (intRelSupport `percent` totalFiles))++"%, "++(roundToStr 2 ((intRelSupport -intRelConfidence) `percent` intRelSupport))++"%"
  putStrLn $ numOfClass FINEGRAINED++", "++(roundToStr 2 (fineGrainSupport `percent` totalFiles))++"%, "++(roundToStr 2 ((fineGrainSupport -fineGrainConfidence) `percent` fineGrainSupport))++"%"
  putStrLn $ numOfClass MISSING++", "++(roundToStr 2 (keywordCoorSupport `percent` totalFiles))++"%, "++(roundToStr 2 ((keywordCoorSupport -keywordCoorConfidence) `percent` keywordCoorSupport))++"%"
  putStrLn $ numOfClass ORDERING++", "++(roundToStr 2 (orderSupport `percent` totalFiles))++"%, "++(roundToStr 2 ((orderSupport -orderConfidence) `percent` orderSupport))++"%"
  putStrLn $ "Total, "++(show $sum fitnesses)++", -, -"-}
  putStrLn "------------"
  putStrLn "Histogram of Errors"
  print fitnesses

-- print and how many errs
reportUserPerformance :: Options -> M.Map Keyword Double -> (FilePath,ErrorReport) -> IO Int
reportUserPerformance Verification{..} ds (f,es) = do
  print f
  when (not sortWithRuleGraph )  $ print $ L.sortBy supportComp es
  when (sortWithRuleGraph) $ print $ L.sortBy (degreeComp ds) es
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
reportBenchmarkPerformance :: Options -> ErrorReport -> ErrorReport -> IO Int
reportBenchmarkPerformance Verification{..} spec foundErrs =
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
    when verbose $ putStrLn $ "Specification :   \n" ++ show spec
    when verbose $ putStrLn $ "True Errors :    \n" ++ unlines (map show $ take 10 truePos)
    when verbose $ putStrLn $ "False Positives : \n" ++ unlines (map show $ take 10 falsePos)
    putStrLn ""
    --endT <- liftM2 (-) getPOSIXTime (return startT)
    --print endT
    --hFlush stdout
    return fitness
