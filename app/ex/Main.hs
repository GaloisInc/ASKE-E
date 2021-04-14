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
import qualified Language.ASKEE.Experiment.CodeGen as GenX
import qualified Language.ASKEE as Core
import qualified Language.ASKEE.SimulatorGen as Gen

main :: IO ()
main =
  do  [mf, n, out] <- getArgs
      replicateModel (read n) mf out
  -- do [mf,ef,out] <- getArgs
  --    measureModel mf ef out


replicateModel :: Int -> FilePath -> FilePath -> IO ()
replicateModel n modelFile out =
  runTest
  do  core <- Core.loadCoreModel (Core.FromFile modelFile) []
      let core' = Ex.replicateModel n core
          cpp   = Gen.genModel core'
      pPrint core'
      writeFile out $ show cpp


measureModel :: FilePath -> FilePath -> FilePath -> IO ()
measureModel modelFile experimentFile out =
  runTest
  do core  <- Core.loadCoreModel (Core.FromFile modelFile) []
     exper <- loadExperiment experimentFile
     let measures = [ m | E.DMeasure m <- exper ]
         model    = foldr Ex.withMeasure core measures
         cpp      = Gen.genModel model
     writeFile out $ show cpp

testCodeGen :: FilePath -> IO ()
testCodeGen experimentFile =
  runTest
  do exper <- loadExperiment experimentFile
     mapM_ print [ GenX.compileMeasure m | E.DMeasure m <- exper ]


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

testFile :: FilePath -> IO ()
testFile fp = runTest
  do ds <- loadExperiment fp
     pPrint ds

