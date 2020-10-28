module Language.ASKEE.Experiments where

import Data.Map (toList, Map)
import Data.Text (Text)

import qualified Language.ASKEE.DiffEq.GenLexer as DL
import qualified Language.ASKEE.DiffEq.GenParser as DP
import           Language.ASKEE.DiffEq.DiffEq (DiffEq, EqGen)
import qualified Language.ASKEE.GenLexer as AL
import qualified Language.ASKEE.GenParser as AP
import           Language.ASKEE.Lexer (Token, Located)
import           Language.ASKEE.Syntax (Model)


import System.IO.Unsafe (unsafePerformIO)


testLexModel :: FilePath -> [Located Token]
testLexModel fp = 
  let model = unsafePerformIO $ readFile fp
      Right toks = AL.lexModel model
  in  toks

testParseModel :: FilePath -> Model
testParseModel fp =
  let toks = testLexModel fp
  in  AP.parse toks

dump :: EqGen (Map Text [Double]) -> FilePath -> IO ()
dump (Right m) fp = writeFile fp $ show $ toList m 

sir, sirs, sirVD :: [Char]
sir = "examples/askee/sir.askee"
sirs = "examples/askee/sirs.askee"
sirVD = "examples/askee/sir-vd.askee"

deqs :: String
deqs = unsafePerformIO $ readFile "/Users/sam/Desktop/projects/aske-e/aske-e/equations.txt"

p :: String -> [DiffEq]
p = DP.parse . DL.alexScanTokens