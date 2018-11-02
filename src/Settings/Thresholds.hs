module Settings.Thresholds where

import System.Directory
import Settings.Options

-- | You can set support and confidence as percentages, or as a # of files (usually specific for a trainging set)
data PercentageThresholds = PercentageThresholds {
    intRelSupport_P :: Double
  , intRelConfidence_P :: Double
  , fineGrainSupport_P :: Double
  , fineGrainConfidence_P :: Double
  , keywordCoorSupport_P :: Double
  , keywordCoorConfidence_P :: Double 
  , keyValKeyCoorSupport_P :: Double
  , keyValKeyCoorConfidence_P :: Double 
  , trivEvidenceThreshold_P :: Int -- ^ This can only be provided as an Int
  , orderSupport_P :: Double
  , orderConfidence_P :: Double
  , typeSupport_P :: Int -- ^ This can only be provided as an Int
  , typeConfidence_P :: Double
  }

  -- support as min true, and confidence as max false
data RawThresholds = RawThresholds {
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
  
-- | If we have been provided percentageSettings by the user,
--   we need to convert to rawThresholds that account for the size of the training set
calcThresholds :: Options -> PercentageThresholds -> IO RawThresholds
calcThresholds settings thresholds = do
  
  fileList <- listDirectory $ learnTarget settings
  let 
    totalFiles = length fileList
    is = (intRelSupport_P thresholds :: Double, intRelConfidence_P thresholds)
    fs = (fineGrainSupport_P thresholds, fineGrainConfidence_P thresholds)
    ks = (keywordCoorSupport_P thresholds, keywordCoorConfidence_P thresholds)
    kvks = (keyValKeyCoorSupport_P thresholds, keyValKeyCoorConfidence_P thresholds)
    os = (orderSupport_P thresholds, orderConfidence_P thresholds)
    f = thresholdConvert totalFiles
  return $ RawThresholds {
        intRelSupport           = fst $ f is
      , intRelConfidence        = snd $ f is

      , fineGrainSupport        = fst $ f fs 
      , fineGrainConfidence     = snd $ f fs 

      , keywordCoorSupport      = fst $ f ks
      , keywordCoorConfidence   = snd $ f ks

      , keyValKeyCoorSupport    = fst $ f kvks
      , keyValKeyCoorConfidence = fst $ f kvks
      , trivEvidenceThreshold   = trivEvidenceThreshold_P thresholds

      , orderSupport            = fst $ f os
      , orderConfidence         = snd $ f os

      , typeSupport             = typeSupport_P thresholds
      , typeConfidence          = typeConfidence_P thresholds
      }

defaultThresholds = let
    (intRelSupport,intRelConfidence) = 
      (1,0) :: (Int,Int) --min # evidenced file and max # contradictory files

    (fineGrainSupport, fineGrainConfidence) = 
      (1, 0) :: (Int,Int)

    (keywordCoorSupport, keywordCoorConfidence)  = 
      (2,1) :: (Int,Int)
    
    (keyValKeyCoorSupport, keyValKeyCoorConfidence)  = 
      (2,0) :: (Int,Int) --(minTrue,maxFalse)
    trivEvidenceThreshold = 0

    (orderSupport, orderConfidence) = 
      (2, 1) :: (Int,Int)

    -- TODO - can only be provided as Int for support and percent for confidence
    (typeSupport, typeConfidence) =
     (15,2) ::(Int,Double)

  in RawThresholds {
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

