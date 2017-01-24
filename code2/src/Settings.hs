module Settings where

  -- enable debugging logs
  vERBOSE = True

  --use the prebuilt cache, if false, will overwrite cache using this run
  uSE_CACHE = False

  --verify benchmarks and report # passing or verify files in 'user' dir
  bENCHMARKS = False

  --what kind of rules should we learn (from correct or incorrect dataset)
  pROBRULES = Prob
  data ModeSetting = NonProb | Prob | Test

  --cutoffs (see https://en.wikipedia.org/wiki/Association_rule_learning#Useful_Concepts)
  -- an abusive of notation here, we use slightly different interprtation of the words - same idea in the end
  --TODO unify numerical comutations to be consistent with true interpretation of support and confidence

  sortingStyle  =  RuleGraphDegree
  data SortStyles = Support | RuleGraphDegree deriving (Eq)

  --TODO this should really be read from training set dir
  totalFiles = 971

  --TODO use confidence instead of maxFalse everytwhere
  thresholds  :: (Double,Double) -> (Int,Int)
  thresholds (s,c) = (support,conf)
   where
    support = floor $ s * (fromIntegral totalFiles)
    conf =  support - (floor $ c*fromIntegral support)

  --support and maxFalse
  intRelSupport = 
    --fst $ thresholds(0.1,0.9) 
    27 :: Int 
  intRelConfidence = 
    --snd $ thresholds(0.1,0.9) 
    1 :: Int

  --support and maxFalse
  fineGrainSupport = 
    -- fst $ thresholds(0.1,0.9)  
    62 :: Int
  fineGrainConfidence = 
    --snd $ thresholds(0.1,0.9)  
    5 :: Int

  --minTrue (actually minTrue is Support, well kind of) and maxFalse
  keywordCoorSupport = 
    --fst $ thresholds(0.1,0.9)  
    7 :: Int 
  keywordCoorConfidence = 
    --snd $ thresholds(0.1,0.9) 
    2 :: Int

{-  
  typeSupport =
  typeConfidence =
-}

  --minTrue and maxFalse
  orderSupport = 
    --fst $ thresholds(0.1,0.9) 
    17  :: Int
  orderConfidence = 
    --snd $ thresholds(0.1,0.9)  
    1 :: Int

