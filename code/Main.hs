{-# LANGUAGE OverloadedStrings #-}

module Main where

import Preproc
import Usertime
import Types
import Data.Text

main = 
 let
  r = preproc learningSet
  e = usertime r userFile
 in return e


learningSet = 
  [ ("learn/file1.txt",MySQL)
  , ("learn/file2.txt",MySQL)
  ]

userFile = ("user/file1.txt",MySQL)
