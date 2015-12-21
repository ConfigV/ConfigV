module Main where

import Preproc
import Usertime

main = 
  r = preproc learningSet
  e = usertime r userFile

