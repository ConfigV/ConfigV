{-# LANGUAGE OverloadedStrings#-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import Control.Exception

import Learners.Common
import Types.IR

import qualified Data.Set as S

ir1 = IRLine{keyword="k1", value="a"}
ir2 = IRLine{keyword="k2", value="b"}

pairTest :: TestTree
pairTest = testCase "pair test" $ 
  (pairsAsSet $ S.fromList [ir1,ir2]) @?= 
  (S.fromList $ [(ir1,ir2), (ir2,ir1)])

main = defaultMain pairTest
  `catch` (\e -> do
    if e == ExitSuccess
      then putStrLn "Yea"
      else putStrLn "Nay"
    throwIO e)

