{-# LANGUAGE MultiWayIf #-}

module TypeMapper where

import Types
import Convert

import Data.Char
import qualified Data.Text as T
import qualified Data.Map as M


-- | in this pass we build a mapping from keywords to types
-- to be reused when checking a userfile
-- with a large learning set, concatMap might run out of memory
learnTypes :: [ConfigFile Language] -> TypeMap
learnTypes fs =
 let
    fs' = concatMap parse fs
  in
    foldl addTypeToMap M.empty fs' 

-- | can use all sorts of nice Data.map fxns (see docs)
addTypeToMap :: TypeMap -> (Keyword, Value) -> TypeMap
addTypeToMap m (k,v) =
  let
    ty = attemptCoerce v
  in
    M.insert k ty m  

-- | can use all sorts of nice Data.map fxns (see docs)
attemptCoerce :: Value -> ConfigType
attemptCoerce t = if
   | T.all isDigit t -> ConfigInt
   | T.all isAlpha t -> ConfigString
   | otherwise -> ConfigUnknown
