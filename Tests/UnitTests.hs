{-# LANGUAGE OverloadedStrings#-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import Control.Exception

import ConfigV.Learners.Common
import ConfigV.Types
import ConfigV.BuildImplGraph

import qualified Data.Map as M
import qualified Data.Set as S

-- | are pairs constructed correctly
pairTest :: TestTree
pairTest = testCase "pair test" $ 
  (pairsAsSet $ S.fromList [ir1,ir2]) @?= 
  (S.fromList $ [(ir1,ir2), (ir2,ir1)])
ir1 = IRLine{keyword="k1", value="a"}
ir2 = IRLine{keyword="k2", value="b"}


-- | can an implication graph be rendered without crashing
implicationGraphRender :: TestTree
implicationGraphRender = testGroup "impl graph tests" [implGraphEmpty, implGraphOne]

-- | render an empty graph
implGraphEmpty :: TestTree
implGraphEmpty = testCase "render empty impl graph" $
  implGraph $ M.empty

implGraphTiny:: TestTree
implGraphTiny = testCase "render tiny impl graph" $
  implGraph $ M.fromList [(simpleSmtFormula, simpleAntiRule), (simpleSmtFormula2, simpleAntiRule)]
simpleSmtFormula = SMTFormula {
         antecedent = STrue
       , consequent = STrue
       }
simpleSmtFormula2 = SMTFormula {
         antecedent = And STrue STrue
       , consequent = STrue
       }
simpleAntiRule = AntiRule {
   tru = 1
  ,fls = 0
  ,tot = 1
  } 


main = defaultMain (testGroup "unit tests" [pairTest, implicationGraphRender])
  `catch` (\e -> do
    if e == ExitSuccess
      then putStrLn "Yea"
      else putStrLn "Nay"
    throwIO e)

