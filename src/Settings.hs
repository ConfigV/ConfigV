module Settings where

  import System.IO.Unsafe 
  import System.Directory

  import Types.IR

  -- enable debugging logs
  verbose =  True

  verificationTarget = 
    "../Apriori/configvData"
    --"user"
    --"githubFiles" 
    --"caseStudies"

  trainingTarget = UserSpecified
  data ModeSetting = NonProb | Prob | Test | UserSpecified
  
  -- only used when ModeSetting set to UserSpecified
  userLearnDir = 
       "templates4_2CSV/"
       --"cfn_data/"
       --"cfn_data_custom_auth_api/"
       --"benchmarkSet/CSVTest/" -- should learn (x,3) => y
  cacheLocation = "cachedRules.json"

  language = CSV

  enableOrder = False
  enableMissing = False
  enableKeyvalkey = True
  enableCoarseGrain = False
  enableFineGrain = False
  enableTypesRules = False
  ----------------------------
  --                        --
  -- Support and Confidence --
  --       thresholds       --
  --                        --
  ----------------------------

  -- You can set support and confidence as percentages, or as a # of files (usually specific for a trainging set)
  (intRelSupport,intRelConfidence) = 
    thresholds(0.105,2) --support and confidence 
    --(27,1) :: (Int,Int) --min # evidenced file and max # contradictory files

  (fineGrainSupport, fineGrainConfidence) = 
    thresholds(0.24,2)  
    --(62, 5) :: (Int,Int)

  (keywordCoorSupport, keywordCoorConfidence)  = 
    thresholds(0.1,1)  
--    (1,1) :: (Int,Int)
  
  (keyValKeyCoorSupport, keyValKeyCoorConfidence)  = 
    --thresholds(0.5,1)
    (4,0) :: (Int,Int) --(minTrue,maxFalse)
  trivEvidenceThreshold :: Int 
  trivEvidenceThreshold = 0

  --minTrue and maxFalse
  (orderSupport, orderConfidence) = 
    thresholds(0.067,2) 
    --(17, 1) :: (Int,Int)

  -- TODO - can only be provided as Int for support and percent for confidence
  (typeSupport, typeConfidence) =
   (15,2) ::(Int,Double)
    --(0,0) ::(Int,Double)


  -----------------------
  --                   --
  -- Advanced Settings --
  --                   --
  -----------------------


  --how to sort errors in the report, rerun the rule graph builder for each training set for best results (see graphAnalysis/README)
  --(will still work fairly well if you dont rerun tho)
  sortingStyle  =  RuleGraphDegree
  data SortStyles = Support | RuleGraphDegree deriving (Eq)

  --Turn on/off probablitic type inference
  probtypes = False
 
  --use the prebuilt cache, if false, will overwrite cache using this run
  useCache = False
 
  --the limit for files to be used in learning
  --useful for benchmarking learning times (be sure to turn off use_cache)
  learnFileLimit :: Int
  learnFileLimit = 200

  --verify benchmarks and report # passing or verify files in 'user' dir
  benchmarks = False --TODO not yet implemented, only works on False setting

  totalFiles = case Settings.trainingTarget of
    Settings.Test -> f "testLearn/"
    Settings.NonProb -> f "benchmarkSet/correctMySQL/"
    Settings.Prob -> f "learningSet/MySQL/"
    Settings.UserSpecified -> f userLearnDir
   where
    f = length . unsafePerformIO . listDirectory

  thresholds  :: (Double,Double) -> (Int,Int)
  thresholds (support,conf) = (floor supportAbs,floor confAbs)
   where
    supportAbs = support * (fromIntegral $ totalFiles)
    confAbs = supportAbs- (conf * (support * fromIntegral totalFiles))
  
  --traceMe x = traceShow x x
