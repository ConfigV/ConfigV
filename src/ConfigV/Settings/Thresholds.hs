module ConfigV.Settings.Thresholds where

import System.Directory
import ConfigV.Settings.Options

-- | You can set support and confidence as percentages, or as a # of files (usually specific for a trainging set)
data PercentageThresholds = PercentageThresholds {
    intRelSupport_P :: Double
  , intRelConfidence_P :: Double
  , fineGrainSupport_P :: Double
  , fineGrainConfidence_P :: Double
  , smtSupport_P :: Double
  , smtConfidence_P :: Double 
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
  , smtSupport :: Int
  , smtConfidence :: Int 
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
    ss = (smtSupport_P thresholds, smtConfidence_P thresholds)
    os = (orderSupport_P thresholds, orderConfidence_P thresholds)
    f = thresholdConvert totalFiles
  return $ RawThresholds {
        intRelSupport           = fst $ f is
      , intRelConfidence        = snd $ f is

      , fineGrainSupport        = fst $ f fs 
      , fineGrainConfidence     = snd $ f fs 

      , smtSupport      = fst $ f ss
      , smtConfidence   = snd $ f ss

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

    (smtSupport, smtConfidence)  = 
      (1,0) :: (Int,Int)--(minTrue,maxFalse)
    
    trivEvidenceThreshold = 0

    (orderSupport, orderConfidence) = 
      (1, 0) :: (Int,Int)

    -- TODO - can only be provided as Int for support and percent for confidence
    (typeSupport, typeConfidence) =
     (1, 0) ::(Int,Double)

  in RawThresholds {
        intRelSupport = intRelSupport
      , intRelConfidence = intRelConfidence
      , fineGrainSupport = fineGrainSupport
      , fineGrainConfidence = fineGrainConfidence
      , smtSupport = smtSupport
      , smtConfidence = smtConfidence
      , trivEvidenceThreshold = trivEvidenceThreshold
      , orderSupport = orderSupport
      , orderConfidence = orderConfidence
      , typeSupport = typeSupport
      , typeConfidence = typeConfidence
      }

defaultPercentageThresholds = 
 PercentageThresholds {
        intRelSupport_P = 0.01
      , intRelConfidence_P = 1
      , fineGrainSupport_P = 0.01
      , fineGrainConfidence_P = 1
      , smtSupport_P = 0.01
      , smtConfidence_P = 1 
      , trivEvidenceThreshold_P = 0 
      , orderSupport_P = 0.01
      , orderConfidence_P = 1
      , typeSupport_P = 2
      , typeConfidence_P = 1
      }
