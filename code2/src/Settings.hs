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

  --support and maxFalse
  intRelSupport = 30 :: Int 
  intRelConfidence = 2 :: Int

  --support and maxFalse
  fineGrainSupport = 55 :: Int
  fineGrainConfidence = 1 :: Int

  --minTrue and maxFalse
  keywordCoorSupport =  7 :: Int 
  keywordCoorConfidence = 1 :: Int

{-  
  typeSupport =
  typeConfidence =
-}

  --minTrue and maxFalse
  orderSupport = 30  :: Int
  orderConfidence = 1 :: Int

