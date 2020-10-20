module Language.ASKEE.Experiments where

import Language.ASKEE.GenLexer ( lexModel )
import Language.ASKEE.Lexer ( Token, Located )
import Language.ASKEE.GenParser ( parse )
import Language.ASKEE.Syntax ( Model )

import qualified Language.ASKEE.DiffEq.GenLexer as GL
import qualified Language.ASKEE.DiffEq.GenParser as GP
import qualified Language.ASKEE.DiffEq.Lexer as L
import qualified Language.ASKEE.DiffEq.DiffEq as S


{- testLex :: FilePath -> IO [Located Token]
testLex fp = do
  Right toks <- lexModel <$> readFile fp
  mapM_ print toks
  pure $ toks

testParse :: FilePath -> IO Model
testParse fp =
  do  toks <- testLex fp
      pure $ parse toks

 -}

testLex :: FilePath -> IO [L.Located L.Token]
testLex fp = GL.alexScanTokens <$> readFile fp

testParse :: FilePath -> IO [S.DiffEq]
testParse fp = GP.parse <$> testLex fp