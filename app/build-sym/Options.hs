{-# Language BlockArguments #-}
module Options
  ( Options(..)
  , Command(..)
  , getOptions
  , GetOptException(..)
  , showHelp
  ) where

import Control.Exception(throwIO)
import Control.Monad(when)

import SimpleGetOpt

data Command =
    OnlyLex
  | OnlyParse

data Options = Options
  { command :: Command
  , modelFile :: FilePath
  , onlyShowHelp :: Bool
  }

options :: OptSpec Options
options = OptSpec
  { progDefaults =
      Options
        { command = OnlyParse
        , modelFile = ""
        , onlyShowHelp = False
        }

  , progOptions =
      [ Option [] ["dbg-only-lex"]
        "Show the list of tokens in the model"
        $ NoArg \s -> Right s { command = OnlyLex }

      , Option [] ["dbg-only-parse"]
        "Show the parse tree."
        $ NoArg \s -> Right s { command = OnlyParse }

      , Option [] ["help"]
        "Show this help"
        $ NoArg \s -> Right s { onlyShowHelp = True }
      ]

  , progParamDocs =
      [ ("FILE",        "File describing the model")
      ]

  , progParams = \p s ->
      if null (modelFile s)
        then Right s { modelFile = p }
        else Left "Multiple model files."
  }


getOptions :: IO Options
getOptions =
  do opts <- getOpts options
     when (onlyShowHelp opts)
       do showHelp
          throwIO (GetOptException [])

     -- extra validation
     when (null (modelFile opts))
       $ throwIO (GetOptException ["Missing model file."])

     pure opts


showHelp :: IO ()
showHelp = dumpUsage options
