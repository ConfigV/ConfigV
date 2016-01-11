module Lexical where
import Types

learnLexicalConstraints :: ConfigFile Common -> [LexRule]
learnLexicalConstraints f = []

mergeLex :: [LexRule] -> [LexRule] -> [LexRule]
mergeLex c1 c2 = c1
--  strisynth

checkLex :: [LexRule] -> ConfigFile Common -> Bool
checkLex cs f = True

