module Utils where

import Text.Printf
import System.IO.Unsafe
import Types.Errors
import qualified Settings
import           Debug.Trace


roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f" 

percent :: Int -> Int -> Double
x `percent` y = 100 * (fromIntegral x / fromIntegral y)

u = unsafePerformIO
getFileName = fst . head . errLocs

debugPrint x = 
  if Settings.verbose
  then traceShow x x
  else x

