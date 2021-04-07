{-# Language BlockArguments #-}
module Main(main) where

import System.Environment
import Control.Monad((<=<))
import Control.Exception
import System.IO(hPutStrLn,stderr)
import AlexTools(prettySourceRange)
import Text.Show.Pretty(pPrint)
import Language.ASKEE.Experiment.Parser(parseDeclsFromFile, ParseError(..))

main :: IO ()
main = (mapM_ (pPrint <=< parseDeclsFromFile) =<< getArgs)
  `catches`
    [ Handler \(ParseError r) ->
        hPutStrLn stderr ("Parse error at " ++ prettySourceRange r)
    ]

