module Settings where

  -- enable debugging logs
  vERBOSE = True

  --use the prebuilt cache, if false, will overwrite cache using this run
  uSE_CACHE = False

  --verify benchmarks and report # passing or verify files in 'user' dir
  bENCHMARKS = True

  --what kind of rules should we learn (from correct or incorrect dataset)
  pROBRULES = Test
  data ModeSetting = NonProb | Prob | Test
