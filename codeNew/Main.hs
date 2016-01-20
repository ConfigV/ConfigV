{-# LANGUAGE OverloadedStrings #-}

module Main where

import Preproc
import Usertime
import TypeMapper
import Convert
 
import Types

import qualified Data.Text as T
import System.IO.Unsafe

main = 
 let
  tyMap = learnTypes learningSet
  rules = learnRules tyMap learningSet

  errors = verifyOn rules tyMap userFile
 in return errors


learningSet = map (\x -> (T.pack . unsafePerformIO $ readFile x, MySQL))
  [ ("learn/file1.txt")
  , ("learn/file2.txt")
  ]

userFile =
  (T.pack $ unsafePerformIO $ readFile "user/order1.txt",MySQL)
