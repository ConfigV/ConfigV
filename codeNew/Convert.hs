module Convert where

import Types

-- | I think we actually need learning here
-- the type of IRConfigFile will be something like [(VariableName, Variable Value)]
convert :: ConfigFile Language -> IRConfigFile
convert (t, MySQL) = (t,t)
convert (t, HTTPD) = (t,t)
