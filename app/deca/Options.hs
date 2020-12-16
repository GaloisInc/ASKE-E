{-# Language BlockArguments #-}
module Options
  ( Options(..)
  , Command(..)
  , getOptions
  , GetOptException(..)
  , showHelp
  ) where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Exception(throwIO)
import Control.Monad(when)

import SimpleGetOpt

data Command =
    OnlyLex
  | OnlyParse
  | OnlyCheck
  | DumpCPP
  | SimulateODE Double Double Double
  | FitModel [Text] (Map Text Double)
  | ComputeError

data Options = Options
  { command :: Command
  , modelFiles :: [FilePath]
  , dataFiles :: [FilePath]
  , deqFiles :: [FilePath]
  , outFile :: FilePath
  , gnuplot :: Bool
  , onlyShowHelp :: Bool
  }

options :: OptSpec Options
options = OptSpec
  { progDefaults =
      Options
        { command = DumpCPP
        , modelFiles = []
        , dataFiles = []
        , deqFiles = []
        , onlyShowHelp = False
        , gnuplot = False
        , outFile = ""
        }

  , progOptions =
      [ Option [] ["dbg-only-lex"]
        "Show the list of tokens in the model"
        $ NoArg \s -> Right s { command = OnlyLex }

      , Option [] ["dbg-only-parse"]
        "Show the parse tree."
        $ NoArg \s -> Right s { command = OnlyParse }

      , Option [] ["dbg-only-check"]
        "Scope- and type-check the model."
        $ NoArg \s -> Right s { command = OnlyCheck }

      , Option [] ["dbg-dump-cpp"]
        "Dump some c++ code"
        $ NoArg \s -> Right s { command = DumpCPP }

      , Option [] ["sim-ode"]
        "Solve a model using GSL's ODE solver"
        $ ReqArg "START:STEP:END"
          \a s -> do (start,step,end) <- parseODETimes a
                     Right s { command = SimulateODE start step end }

      , Option [] ["fit"]
        "Fit model parameters with optional residual scaling"
        $ ReqArg "PNAME"
          \a s -> let newCommand = case command s of
                                     FitModel ps mp ->
                                        FitModel (Text.pack a:ps) mp
                                     _ -> FitModel [Text.pack a] Map.empty
                  in Right s { command = newCommand }

      , Option [] ["fit-scale"]
        "Scaling when fitting this variable"
        $ ReqArg "VNAME*SCALE"
          \a s -> do (x,d) <- parseScale a
                     let newCmd = case command s of
                                    FitModel ps mp ->
                                         FitModel ps (Map.insert x d mp)
                                    _ -> FitModel [] (Map.singleton x d)
                     Right s { command = newCmd }

      , Option [] ["error-ode"]
        "Compute difference between model and data"
        $ NoArg \s -> Right s { command = ComputeError }

      , Option [] ["gnuplot"]
        "Generate a GNU plot file"
        $ NoArg \s -> Right s { gnuplot = True }

      , Option ['m'] ["model"]
        "Use this model"
        $ ReqArg "FILE" \a s -> Right s { modelFiles = a : modelFiles s }

      , Option ['d'] ["data"]
        "Use this data series"
        $ ReqArg "FILE" \a s -> Right s { dataFiles = a : dataFiles s }

      , Option ['q'] ["diffeq"]
        "Use this DiffEq set"
        $ ReqArg "FILE" \a s -> Right s { deqFiles = a : deqFiles s}

      , Option ['o'] ["output"]
        "Use this output file"
        $ ReqArg "FILE" \a s -> case outFile s of
                                  "" -> Right s { outFile = a }
                                  _ -> Left "Multiple output files"

      , Option [] ["help"]
        "Show this help"
        $ NoArg \s -> Right s { onlyShowHelp = True }
      ]

  , progParamDocs = []
  , progParams = \_ _ -> Left "Unexpected parameter"

  }

parseODETimes :: String -> Either String (Double,Double,Double)
parseODETimes xs =
  case [ (start,step,end) | (start,':':ys) <- reads xs
                          , (step,':':zs)  <- reads ys
                          , (end,"")      <- reads zs
                          ] of
    [x] -> Right x
    _   -> Left "Malformed simulation time"

parseScale :: String -> Either String (Text,Double)
parseScale xs =
  case break (=='*') xs of
    (as,_:bs) | [(d,"")] <- reads bs -> Right (Text.pack as, d)
    _ -> Left "Invalid value for scale-fit"


getOptions :: IO Options
getOptions =
  do opts <- getOpts options
     when (onlyShowHelp opts)
       do showHelp
          throwIO (GetOptException [])

     when (gnuplot opts && null (outFile opts))
       $ throwIO (GetOptException ["gnuplot requires an output file"])

     pure opts


showHelp :: IO ()
showHelp = dumpUsage options
