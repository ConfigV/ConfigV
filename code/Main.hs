{-# LANGUAGE OverloadedStrings #-}

module Main where

import Preproc
import Usertime
import TypeMapper
import Convert
 
import Types

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO.Unsafe
import System.Directory

import Control.Applicative

import Debug.Trace

main = 
 let
  tyMap = learnTypes learningSet
  rules = learnRules tyMap learningSet

  errors = verifyOn rules tyMap userFile
 in do
   mapM  putStrLn errors

--lsDir = "dataset/correctMySQL/"
--lsDir = "dataset/group2-entry-missing/correct/"
--lsDir = "dataset/group5-value-correlation/correct/"
lsDir = "dataset/group4-ordering/correct/"
learningSet = map (\x -> (u $ T.readFile (lsDir++x), MySQL))
  (u $ listDirectory lsDir)

{-  [ ("learn/file1.txt")
  , ("learn/file2.txt")
  ]-}

userFile =
--  (unsafePerformIO $ T.readFile "dataset/group5-value-correlation/error",MySQL)
--  (unsafePerformIO $ T.readFile "dataset/group2-entry-missing/error",MySQL)
  (unsafePerformIO $ T.readFile "dataset/group4-ordering/error",HTTPD)

u = unsafePerformIO

-- | from the newest version of the package, which i cant get for some reason
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
  where
    isDir = not . u . doesDirectoryExist
    f filename = filename /= "." && filename /= ".." && isDir (path++filename)
