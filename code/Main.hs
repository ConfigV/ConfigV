{-# LANGUAGE OverloadedStrings #-}

module Main where

import Preproc
import Usertime
import Convert

import Types

import Benchmarks

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import Control.Monad

import System.IO.Unsafe

import System.Directory


import Control.Applicative

import Debug.Trace

vERBOSE = True

main = do
 bs <- mapM T.readFile benchmarkFiles :: IO [T.Text]
 let bs' = zip bs (replicate (length bs) MySQL)
 let rules = learnRules learningSet
 let bigRules = learnRules bigLearningSet
 let errors =  zipWith (verifyOn rules) bs' benchmarkFiles
 --mapM putStrLn $ showProbRules rules
-- mapM putStrLn $ showProbRules bigRules
 --mapM putStrLn (zipWith (++) benchmarks (map unlines errors))
 zipWithM_ reportBenchmarkPerformance benchmarks errors
 return ()

 -- | compare the original benchark spec to the generated one
reportBenchmarkPerformance :: ErrorReport -> ErrorReport -> IO()
reportBenchmarkPerformance spec foundErrs =
  let
    truePos = head spec `elem` foundErrs
    falsePos = filter ((/=) $ head spec) foundErrs
  in do
    putStrLn (getFileName $ head spec)
    putStrLn $ "    Passing: " ++ show truePos
    putStrLn $ "    False Positives: "++ show (length falsePos)
    when vERBOSE $ putStrLn $ "Specification :   \n" ++ show spec
    when vERBOSE $ putStrLn $ "Found Errors :    \n" ++ unlines (map show foundErrs)
    when vERBOSE $ putStrLn $ "False Positives : \n" ++ unlines (map show falsePos)

    putStrLn ""

lsDir = "dataset/correctMySQL/"
learningSet =
  map (\x -> (u $ T.readFile (lsDir++x), MySQL))
    (u (listDirectory lsDir))
     -- ++ [ (u $ T.readFile ("dataset/group2-entry-missing/error"), MySQL)]

bigLsDir = "dump/MySQL/"
bigLearningSet =
  map (\x -> (u $ T.readFile (bigLsDir++x), MySQL))
    (u (listDirectory bigLsDir))

u = unsafePerformIO

-- | from the newest version of the package, which i cant get for some reason
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where
    isDir = not . u . doesDirectoryExist
    f filename = filename /= "." && filename /= ".." && isDir (path++filename)
