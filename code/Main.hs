{-# LANGUAGE OverloadedStrings #-}

module Main where

import Preproc
import Usertime
import Types
import qualified Data.Text as T

import System.IO.Unsafe

main = 
 let
  r = preproc learningSet
  e = usertime r userFile
 in return e


learningSet = map (\x -> (T.pack . unsafePerformIO $ readFile x, MySQL))
  [ ("learn/file1.txt")
  , ("learn/file2.txt")
  ]

userFile =
  (T.pack $ unsafePerformIO $ readFile "user/order1.txt",MySQL)
