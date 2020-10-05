module Language.ASKEE.Experiments where

import Language.ASKEE.GenLexer
import Language.ASKEE.Lexer
import Language.ASKEE.GenParser
import Language.ASKEE.Syntax


testLex :: FilePath -> IO [Located Token]
testLex fp = do
  Right toks <- lexModel <$> readFile fp
  mapM_ print toks
  pure $ toks

testParse :: FilePath -> IO Model
testParse fp =
  do  toks <- testLex fp
      pure $ parse toks