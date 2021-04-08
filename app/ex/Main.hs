{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Main(main) where

import System.Environment
import Control.Monad((<=<))
import Control.Exception
import System.IO(hPutStrLn,stderr)
import Data.Text(Text)
import Data.Foldable(traverse_)
import AlexTools(prettySourceRange)
import Text.Show.Pretty(pPrint)
import Language.ASKEE.Experiment.Parser(ParseError(..), parse, parseFromFile, exprP, declsP)
import Language.ASKEE.Experiment.Syntax as E
import Language.ASKEE.Experiment.Typechecker as TC

main :: IO ()
main = (mapM_ (pPrint <=< parseFromFile declsP) =<< getArgs)
  `catches`
    [ Handler \(ParseError r) ->
        hPutStrLn stderr ("Parse error at " ++ prettySourceRange r)
    ]

testExpr :: [(Text, E.Type)] -> Text -> IO ()
testExpr envs source =
  do  let Right e = parse exprP "<unit test>" source
      let inferred =
            TC.runTC do (uncurry TC.bindVar) `traverse_` envs
                        TC.inferExpr e
      pPrint inferred

testDecl :: FilePath -> IO ()
testDecl path =
  do  [E.DMeasure m] <- parseFromFile declsP path
      let m' = runTC (TC.inferMeasure m)
      pPrint m'



