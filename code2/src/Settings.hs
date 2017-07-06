module Settings where

  import System.IO.Unsafe 
  import System.Directory
  import Debug.Trace

  import Learners.Common
  -- enable debugging logs
  vERBOSE = True

  verificationTarget = 
    "user"
    --"githubFiles" 
    --"caseStudies"

  
  trainingTarget = Test
  data ModeSetting = NonProb | Prob | Test

  -- You can set support and confidence as percentages, or as a # of files (usually specific for a trainging set)
  (intRelSupport,intRelConfidence) = 
    thresholds(0.105,0.96) --support and confidence 
    --(27,1) :: (Int,Int) --min # evidenced file and max # contradictory files

  (fineGrainSupport, fineGrainConfidence) = 
    thresholds(0.24,0.91)  
    --(62, 5) :: (Int,Int)

  (keywordCoorSupport, keywordCoorConfidence)  = 
    thresholds(0.028,0.71)  
    --(7,2) :: (Int,Int)

  --minTrue and maxFalse
  (orderSupport, orderConfidence) = 
    --thresholds(0.067,0.94) 
    (17, 1) :: (Int,Int)

  -- TODO - can only be provided as Int for support and percent for confidence
  (typeSupport, typeConfidence) =
    --(15,0.7) ::(Int,Double)
    --(0,0) ::(Int,Double)


  -----------------------
  --                   --
  -- Advanced Settings --
  --                   --
  -----------------------


  --how to sort errors in the report, rerun the rule graph builder for each training set for best results
  --(will still work fairly well if you dont tho)
  sortingStyle  =  RuleGraphDegree
  data SortStyles = Support | RuleGraphDegree deriving (Eq)

  --Turn on/off probablitic type inference
  pROBTYPES = True

  --use the prebuilt cache, if false, will overwrite cache using this run
  uSE_CACHE = False
  
  --verify benchmarks and report # passing or verify files in 'user' dir
  bENCHMARKS = False --TODO not yet implemented, only works on False setting

  totalFiles = case Settings.trainingTarget of
    Settings.Test -> f "testLearn/"
    Settings.NonProb -> f "benchmarkSet/correctMySQL/"
    Settings.Prob -> f "learningSet/MySQL/"
   where
    f = length . unsafePerformIO . listDirectory

  thresholds  :: (Double,Double) -> (Int,Int)
  thresholds (support,conf) = (floor supportAbs,floor confAbs)
   where
    supportAbs = support * (fromIntegral $ totalFiles)
    confAbs = supportAbs- (conf * (support * fromIntegral totalFiles))
  
  --traceMe x = traceShow x x
