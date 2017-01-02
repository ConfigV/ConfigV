module Types.Errors where

import           Types.IR
import           Types.Common

--printing  stuff
type ErrorReport = [Error]
data ErrorType = INTREL | ORDERING | MISSING | TYPE deriving (Show, Eq)
data Error = Error{
    errLoc1  :: (FilePath, Keyword)
  , errLoc2  :: (FilePath, Keyword)
  , errIdent :: ErrorType
  , errMsg   :: String
}

instance Show Error where
  show e = 
    --"Error between "++(show$ errLoc1 e)++" and "++(show$ errLoc2 e)++" of type: "++(show $errIdent e)++"\n   -->" ++(show $errMsg e)++"\n"
    (show $ errMsg e)++"\n"


-- | as long as we have the correct type of error
--   and have identified one similar fail point the errors are similar enough
--   both will point the user to the item that needs to be fixed
--   NB DEF OF FALSE POSITIVE - THIS IS REALLY IMPORTANT!!!
--   in configC, if we ounda type error, we would not report anything else
--   this reduces false positives and makes sense since type errors tend to be strongest rules and will break other things

instance Eq Error where
  (==) x y =
    let
      exactLocMatch = (errLoc1 x == errLoc1 y) && (errLoc2 x == errLoc2 y)
      justOneMatch = (errLoc1 x == errLoc1 y) || (errLoc2 x == errLoc2 y)
      transitiveLocMatch = (errLoc2 x == errLoc1 y) && (errLoc1 x == errLoc2 y)
      anyMatch =  (errLoc1 x == errLoc1 y) || (errLoc2 x == errLoc2 y) || (errLoc2 x == errLoc1 y) || (errLoc1 x == errLoc2 y)
      identMatch = (errIdent x == errIdent y)
    in
      case errIdent x of
        MISSING  -> justOneMatch && identMatch
        ORDERING -> justOneMatch && identMatch
        INTREL   -> anyMatch && identMatch
        TYPE     -> (exactLocMatch || transitiveLocMatch) && identMatch

