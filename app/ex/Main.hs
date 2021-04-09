{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Main(main) where

import System.Environment
import Control.Exception
import System.IO(hPutStrLn,stderr)
import Data.Text(Text)
import qualified Data.Text.IO as Text
import Data.Foldable(traverse_)
import AlexTools(prettySourceRange)
import Text.Show.Pretty(pPrint)
import qualified Language.ASKEE.Experiment.Parser as P
import qualified Language.ASKEE.Experiment.Syntax as E
import qualified Language.ASKEE.Experiment.Typechecker as TC
import qualified Language.ASKEE.Experiment.TraverseType as TC
import qualified Language.ASKEE.Experiment.EaselAdapter as Ex
import qualified Language.ASKEE as Core

main :: IO ()
main =
  do [mf,ef] <- getArgs
     measureModel mf ef


measureModel :: FilePath -> FilePath -> IO ()
measureModel modelFile experimentFile =
  runTest
  do core  <- Core.loadCoreModel (Core.FromFile modelFile) []
     exper <- loadExperiment experimentFile
     let measures = [ m | E.DMeasure m <- exper ]
         model    = foldr Ex.withMeasure core measures
     pPrint model


--------------------------------------------------------------------------------
runTest :: IO () -> IO ()
runTest m =
  m `catches`
    [ Handler \(P.ParseError r) ->
        hPutStrLn stderr ("Parse error at " ++ prettySourceRange r)
    , Handler \(TCError txt) ->
        Text.hPutStrLn stderr ("Typing error: " <> txt)
    ]

--------------------------------------------------------------------------------
data TCError = TCError Text
  deriving Show

instance Exception TCError

runTC :: TC.TraverseType a => TC.TC a -> IO a
runTC tc =
  case TC.runTC tc of
    Left err -> throwIO (TCError err)
    Right a  -> pure a

loadExperiment :: FilePath -> IO [E.Decl]
loadExperiment file =
  do syn <- P.parseFromFile P.declsP file
     runTC (TC.inferDecls syn)

 -------------------------------------------------------------------------------- 

testExpr :: [(Text, E.Type)] -> Text -> IO ()
testExpr envs source =
  runTest
  do  let Right e = P.parse P.exprP "<unit test>" source
      inferred <- runTC do (uncurry TC.bindVar) `traverse_` envs
                           TC.inferExpr e
      pPrint inferred

testDecl :: FilePath -> IO ()
testDecl path =
  runTest
  do  [E.DMeasure m] <- P.parseFromFile P.declsP path
      m' <- runTC (TC.inferMeasure m)
      pPrint m'

testFile :: FilePath -> IO ()
testFile fp = runTest
  do ds <- loadExperiment fp
     pPrint ds

