module Values where

import Types
-- similar to Syntax.hs, we reduce the problem to 
-- finding relations that always hold 

-- [[(A,1),(B,2)],[(A,1),(B,3)] -> [(A==1),(2<b<3)]

learnValueConstraints :: ConfigFile Common -> [Clause]
learnValueConstraints c = []

--(FilePath,Common) -> [(Variable,Value)]


-- | combine two sets of rules
mergeVal :: [Clause] -> [Clause] -> [Clause]
mergeVal c1 c2 = c1 
