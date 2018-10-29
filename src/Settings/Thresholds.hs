module Settings.Thresholds where

import System.Directory
import Settings.Options

data Thresholds = Thresholds {
    intRelSupport :: Int
  , intRelConfidence :: Int
  , fineGrainSupport :: Int
  , fineGrainConfidence :: Int
  , keywordCoorSupport  :: Int
  , keywordCoorConfidence :: Int 
  , keyValKeyCoorSupport :: Int
  , keyValKeyCoorConfidence :: Int 
  , trivEvidenceThreshold :: Int
  , orderSupport :: Int
  , orderConfidence :: Int
  , typeSupport :: Int
  , typeConfidence :: Double
  }

----------------------------
--                        --
-- Support and Confidence --
--       thresholds       --
--                        --
----------------------------
thresholdConvert :: Int -> (Double,Double) -> (Int,Int)
thresholdConvert trainingSetSize (support,conf) = (floor supportAbs,floor confAbs)
 where
  supportAbs = support * (fromIntegral $ trainingSetSize)
  confAbs = supportAbs- (conf * (support * fromIntegral trainingSetSize))
  
-- TODO allow settings to optionally contain values for thresholds
calcThresholds settings = do
  
  fileList <- listDirectory $ learnTarget settings
  let 
    totalFiles = length fileList

  -- You can set support and confidence as percentages, or as a # of files (usually specific for a trainging set)
    (intRelSupport,intRelConfidence) = 
      --thresholdConvert totalFiles (0.105,2) --support and confidence 
      (1,0) :: (Int,Int) --min # evidenced file and max # contradictory files

    (fineGrainSupport, fineGrainConfidence) = 
      thresholdConvert totalFiles (0.24,2)  
      --(62, 5) :: (Int,Int)

    (keywordCoorSupport, keywordCoorConfidence)  = 
      --thresholdConvert totalFiles (0.1,1)  
      (2,1) :: (Int,Int)
    
    (keyValKeyCoorSupport, keyValKeyCoorConfidence)  = 
      --thresholdConvert(0.5,1)
      (2,0) :: (Int,Int) --(minTrue,maxFalse)
    trivEvidenceThreshold = 0

    --minTrue and maxFalse
    (orderSupport, orderConfidence) = 
      --thresholdConvert(0.067,2) 
      (2, 1) :: (Int,Int)

    -- TODO - can only be provided as Int for support and percent for confidence
    (typeSupport, typeConfidence) =
     (15,2) ::(Int,Double)
      --(0,0) ::(Int,Double)

  return $ Thresholds {
        intRelSupport = intRelSupport
      , intRelConfidence = intRelConfidence
      , fineGrainSupport = fineGrainSupport
      , fineGrainConfidence = fineGrainConfidence
      , keywordCoorSupport = keywordCoorSupport
      , keywordCoorConfidence = keywordCoorConfidence
      , keyValKeyCoorSupport = keyValKeyCoorSupport
      , keyValKeyCoorConfidence = keyValKeyCoorConfidence
      , trivEvidenceThreshold = trivEvidenceThreshold
      , orderSupport = orderSupport
      , orderConfidence = orderConfidence
      , typeSupport = typeSupport
      , typeConfidence = typeConfidence
      }

