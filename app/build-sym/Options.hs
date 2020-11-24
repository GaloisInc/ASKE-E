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
  | DumpCPP
  | SimulateODE Double Double Double

data Options = Options
  { command :: Command
  , modelFile :: FilePath
  , outFile :: FilePath
  , gnuplot :: Bool
  , onlyShowHelp :: Bool
  }

options :: OptSpec Options
options = OptSpec
  { progDefaults =
      Options
        { command = DumpCPP
        , modelFile = ""
        , onlyShowHelp = False
        , gnuplot = False
        , outFile = "output.txt"
        }

  , progOptions =
      [ Option [] ["dbg-only-lex"]
        "Show the list of tokens in the model"
        $ NoArg \s -> Right s { command = OnlyLex }

      , Option [] ["dbg-only-parse"]
        "Show the parse tree."
        $ NoArg \s -> Right s { command = OnlyParse }

      , Option [] ["dbg-dump-cpp"]
        "Dump some c++ code"
        $ NoArg \s -> Right s { command = DumpCPP }

      , Option [] ["sim-ode"]
        "Solve using GSL's ODE solver"
        $ ReqArg "START:STEP:END"
          \a s -> do (start,step,end) <- parseODETimes a
                     Right s { command = SimulateODE start step end }

      , Option [] ["gnuplot"]
        "Generate a GNU plot file"
        $ NoArg \s -> Right s { gnuplot = True }

      , Option [] ["help"]
        "Show this help"
        $ NoArg \s -> Right s { onlyShowHelp = True }
      ]

  , progParamDocs =
      [ ("FILE",        "File describing the model")
      , ("FILE",        "Optional output file")
      ]

  , progParams = \p s ->
      if null (modelFile s)
        then Right s { modelFile = p }
        else Right s { outFile = p }
  }

parseODETimes :: String -> Either String (Double,Double,Double)
parseODETimes xs =
  case [ (start,step,end) | (start,':':ys) <- reads xs
                          , (step,':':zs)  <- reads ys
                          , (end,"")      <- reads zs
                          ] of
    [x] -> Right x
    _   -> Left "Malformed simulation time"

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
