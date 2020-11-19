{-# Language BlockArguments #-}
module Main(main) where

import Control.Exception(catches, Handler(..))
import System.Exit(exitSuccess,exitFailure)

import Language.ASKEE.Experiments

import Options

main :: IO ()
main =
  do opts <- getOptions
     case command opts of
       OnlyLex   -> mapM_ print =<< testLexModel (modelFile opts)
       OnlyParse -> print =<< testParseModel (modelFile opts)
       DumpCPP   -> genCppRunner (modelFile opts)

  `catches`
  [ Handler  \(GetOptException errs) ->
      case errs of
        [] -> exitSuccess   -- showed help
        _  -> do mapM_ putStrLn errs
                 showHelp
                 exitFailure
  ]
