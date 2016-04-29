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

import qualified Settings

main = do
 let verificationTarget = 
       if Settings.bENCHMARKS
       then benchmarkFiles
       else userFiles
 bs <- mapM T.readFile verificationTarget :: IO [T.Text]
 let bs' = zip bs (replicate (length bs) MySQL)
 let rules = learnRules (learningSet)
 let errors = zipWith (verifyOn rules) bs' verificationTarget

 --mapM putStrLn (zipWith (++) benchmarks (map unlines errors))
 when Settings.bENCHMARKS $ zipWithM_ reportBenchmarkPerformance benchmarks errors
 when (not Settings.bENCHMARKS) $ mapM_ putStrLn $ concat $ zipWith (\x y -> [show x,show y]) userFiles errors
 return ()

userFiles = map ("user/"++) $ u$ listDirectory "user"

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
    when Settings.vERBOSE $ putStrLn $ "Specification :   \n" ++ show spec
    when Settings.vERBOSE $ putStrLn $ "Found Errors :    \n" ++ unlines (map show foundErrs)
    when Settings.vERBOSE $ putStrLn $ "False Positives : \n" ++ unlines (map show falsePos)

    putStrLn ""

lsDir = "dataset/correctMySQL/"
learningSet =
  map (\x -> (u $ T.readFile (lsDir++x), MySQL))
    (u (listDirectory lsDir))
     -- ++ [ (u $ T.readFile ("dataset/group2-entry-missing/error"), MySQL)]

u = unsafePerformIO

-- | from the newest version of the package, which i cant get for some reason
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where
    isDir = not . u . doesDirectoryExist
    f filename = filename /= "." && filename /= ".." && isDir (path++filename)
