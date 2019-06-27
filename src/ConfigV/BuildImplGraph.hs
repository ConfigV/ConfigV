{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ConfigV.BuildImplGraph where

import ConfigV.Types

import qualified Data.Map.Strict as M

import Algebra.PartialOrd

import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz

import Data.GraphViz

implGraph :: M.Map SMTFormula AntiRule -> IO ()
implGraph rs = do 
  let m = buildImplGraph rs
  let g = (uncurry mkGraph) $ asVerteciesAndEdges m
  gr <- layoutGraph Dot g 
  let grRendered = drawGraph
                     (\label loc -> place (formatRule label) loc)
                     (\_ p1 _ p2 _ p -> arrowBetween' (opts p) p1 p2)  
                     gr
      opts p = with & gaps .~ 16 & arrowShaft .~ (unLoc . head $ pathTrails p)
  renderSVG "implicationGraph.svg" (dims2D (fromIntegral $ (M.size m) *40) 400) $ bgFrame 20 white grRendered

formatRule SMTFormula{..} = let
  line1 = shortShow antecedent
  line2 = shortShow consequent
  asDia = fontSizeL 8 . text 
 in
  translateY (8) $ vsep 8 $ map asDia [(line1 ++ " =>"), line2]

-- | build a graph based on partial order of rules
buildImplGraph :: M.Map SMTFormula AntiRule -> M.Map SMTFormula ([SMTFormula], AntiRule)
buildImplGraph rs =
  M.mapWithKey 
     (\r a -> (M.keys $ M.filterWithKey (\k _ -> comparable k r && r `leq` k) rs, a))
     rs

type Edge = (SMTFormula, SMTFormula, ())

asVerteciesAndEdges :: M.Map SMTFormula ([SMTFormula], AntiRule) -> ([SMTFormula], [Edge])
asVerteciesAndEdges rs = let
    edges = concatMap (\(k, (ks,ruleData)) -> [(k, k', ()) | k' <- ks]) $ M.toList rs
    noSelf = filter (\(v1,v2,_) -> v1 /= v2) edges
  in
    (M.keys rs, noSelf)
