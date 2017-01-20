module Types.Errors where

import           Types.IR
import           Types.Common

--printing  stuff
type ErrorReport = [Error]
data ErrorType = INTREL | ORDERING | MISSING | TYPE | FINEGRAINED deriving (Show, Eq)
data Error = Error{
    errLocs  :: [(FilePath, Keyword)]
  , errIdent :: ErrorType
  , errMsg   :: String
}

instance Show Error where
  show e = 
    --"Error between "++(show$ errLoc1 e)++" and "++(show$ errLoc2 e)++" of type: "++(show $errIdent e)++"\n   -->" ++(show $errMsg e)++"\n"
    errMsg e++"\n"


-- | as long as we have the correct type of error
--   and have identified one similar fail point the errors are similar enough
--   both will point the user to the item that needs to be fixed
--   NB DEF OF FALSE POSITIVE - THIS IS REALLY IMPORTANT!!!
--   in configC, if we ounda type error, we would not report anything else
--   this reduces false positives and makes sense since type errors tend to be strongest rules and will break other things

instance Eq Error where
  (==) e1 e2 =
    let
      exactLocMatch = and $ zipWith (==) (errLocs e1) (errLocs e2)
      justOneMatch = or $ zipWith (==) (errLocs e1) (errLocs e2)
      transitiveLocMatch = and $ zipWith (==) (errLocs e1) (reverse $ errLocs e2)
      anyMatch =  justOneMatch || transitiveLocMatch
      identMatch = (errIdent e1 == errIdent e2)
    in
      case errIdent e1 of
        MISSING  -> justOneMatch && identMatch
        ORDERING -> justOneMatch && identMatch
        INTREL   -> anyMatch && identMatch
        TYPE     -> (exactLocMatch || transitiveLocMatch) && identMatch

