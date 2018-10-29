{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE MultiWayIf #-} 
{-# LANGUAGE OverloadedStrings #-} 

module Learners.IntRel where

import Types.IR
import Types.Errors
import Types.Rules 
import Types.Countable
import qualified Types.Rules as R
import Learners.Common

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Char as C
import           Data.Maybe 

import Data.Interned

import Settings.Config
import Control.Monad.Reader


-- | We assume that all IRConfigFiles have a set of unique keywords
--   this should be upheld by the tranlsation from ConfigFile to IRConfigFile
--   this means we cannot derive both (a,b) and (b,a) from one file
instance Learnable R.IntRel Formula where

  buildRelations f = let
    rs = mapMaybe intLike f 
--assume socket and port can only only have int relations with each other
--TODO is this a valid assumtpion?
    rs' = filter (\(ir1,ir2) -> sockport (ir1,ir2) && (not $T.isInfixOf "dir" $ unintern $ keyword ir1) && (not $ T.isInfixOf "dir" $ unintern $ keyword ir2)) $ pairs rs
    eqs = map toIntRel rs'
   in
    return $ M.fromList $ eqs

  -- unionsWith work by Ord, so just providing a custom instance of Eq wont work, also need Ord
  -- ord is too sensitive, since traversal might miss an EQ
  -- instead just rebuild the whole map with combineFlips (only happens once so shouldnt be too bad
  merge rs = do
    settings <- ask
    let 
      rs' = M.unionsWith add rs
      merged = M.foldlWithKey combineFlips M.empty rs'
    --TODO def for when an eq rule has enough conf
      validRule r = (gt r + lt r + eq r)>(intRelSupport $ thresholdSettings settings) &&  
                    (gt r < (intRelConfidence $ thresholdSettings settings)) || 
                    (lt r <= (intRelConfidence $ thresholdSettings settings))
    return $ M.filter validRule merged
 
  check _ r1 r2 = if
    | eq r2 == 1 && (lt r1 > 3 || gt r1 > 3) && eq r1 < 3 -> Just r1 --TODO eq rules require more evidence than others
    | lt r2 == 1 && gt r1 > lt r1 -> Just r1
    | gt r2 == 1 && lt r1 > gt r1 -> Just r1
    | otherwise -> Nothing
    
    
  toError ir fname ((IntRel k1 k2),rd) = Error{
     errLocs = map (\x->(fname, x)) [k1,k2]
    ,errIdent = INTREL
    ,errMsg = "INTEGER RELATION ERROR: Expected "++(show k1)++(show rd)++(show k2)++" \n Found values: "
              ++(show $ map (\x-> (T.unpack$ unintern $ keyword x)++"="++(T.unpack$ unintern $ value x)) $ filter (\x->keyword x==k1 || keyword x==k2) ir)
    ,errSupport = gt rd + lt rd + eq rd
    }

combineFlips :: M.Map IntRel Formula -> IntRel -> Formula -> M.Map IntRel Formula
combineFlips old (IntRel k1 k2) v = if 
  | M.member (IntRel k1 k2) old -> M.adjust (add v) (IntRel k1 k2) old
  | M.member (IntRel k2 k1) old -> M.adjust (add (flipped v)) (IntRel k2 k1) old
  | otherwise -> M.insert (IntRel k1 k2) v old


flipped :: Formula -> Formula
flipped f = Formula {
   gt = lt f
  ,eq = eq f
  ,lt = gt f}

--all IRLines must have only ints as values
toIntRel :: (IRLine,IRLine) -> (IntRel,Formula)
toIntRel (IRLine k1 v1,IRLine k2 v2) = let
 asInt v = (read $ T.unpack (fst $ T.partition C.isNumber v)) * (fromMaybe 1 $ units v)
 formula v1 v2 = if
   | v1 < v2  -> Formula {gt=0,eq=0,lt=1}
   | v1 == v2 -> Formula {gt=0,eq=1,lt=0}
   | v1 > v2  -> Formula {gt=1,eq=0,lt=0}
 in
  if k1>k2 
  then ((IntRel k1 k2), formula (asInt $ unintern v1) (asInt $ unintern v2))
  else ((IntRel k2 k1), formula (asInt $ unintern v2) (asInt $ unintern v1))
  
