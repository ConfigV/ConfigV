module Values where

-- similar to Syntax.hs, we reduce the problem to 
-- finding relations that always hold 

-- [[(A,1),(B,2)],[(A,1),(B,3)] -> [(A==1),(2<b<3)]

learnValueConstraints :: Config Common -> RuleSet
learnValueConstraints c =

ConfigFile Common -> [(Variable,Value)]


-- | combine two sets of rules
mergeVal :: RuleSet -> RuleSet -> RuleSet
