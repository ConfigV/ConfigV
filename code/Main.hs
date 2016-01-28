{-# LANGUAGE OverloadedStrings #-}

module Main where

import Preproc
import Usertime
import Convert
 
import Types

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List as L
import System.IO.Unsafe
import System.Directory

import Control.Applicative

import Debug.Trace

main = do
 bs <- mapM T.readFile benchmarks :: IO [T.Text]
 let bs' = zip bs (replicate (length bs) MySQL)
 let rules = learnRules learningSet
 let errors =  map (verifyOn rules) bs'
 mapM putStrLn (zipWith (++) benchmarks (map unlines errors))

lsDir = "dataset/correctMySQL/"
learningSet = 
  (map (\x -> (u $ T.readFile (lsDir++x), MySQL))
    (u (listDirectory lsDir)))
    -- ++ [ (u $ T.readFile ("dataset/group5-value-correlation/error5"), MySQL)]

benchmarks = [
    "dataset/group2-entry-missing/error"
  , "dataset/group2-entry-missing/error2"
  , "dataset/group2-entry-missing/error3"
  , "dataset/group2-entry-missing/error4"
  , "dataset/group2-entry-missing/error5"
  , "dataset/group3-path-type/error"
  , "dataset/group3-path-type/error2"
  , "dataset/group3-path-type/error3"
  , "dataset/group3-path-type/error4"
  , "dataset/group4-ordering/error_mysql"
  , "dataset/group4-ordering/error2"
  , "dataset/group4-ordering/error3"
  , "dataset/group4-ordering/error4"
  , "dataset/group5-value-correlation/error"
  , "dataset/group5-value-correlation/error2"
  , "dataset/group5-value-correlation/error3"
  , "dataset/group5-value-correlation/error4"
  , "dataset/group5-value-correlation/error5"
  ]

u = unsafePerformIO

-- | from the newest version of the package, which i cant get for some reason
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
  where
    isDir = not . u . doesDirectoryExist
    f filename = filename /= "." && filename /= ".." && isDir (path++filename)
