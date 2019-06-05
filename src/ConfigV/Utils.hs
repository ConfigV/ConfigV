{-# LANGUAGE FlexibleContexts #-}

module ConfigV.Utils where

import Text.Printf
import System.IO.Unsafe
import ConfigV.Types.Errors
import Debug.Trace

import ConfigV.Settings.Config
import Control.Monad.Reader

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f" 

percent :: Int -> Int -> Double
x `percent` y = 100 * (fromIntegral x / fromIntegral y)

u = unsafePerformIO
getFileName = fst . head . errLocs

debugPrint x = do
  settings <- ask
  return $
      if verbose $ optionsSettings $ settings
      then traceShow x x
      else x

traceMe x = traceShow x x
