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
import Control.Monad(when,unless)

import SimpleGetOpt

data Command =
    OnlyLex
  | OnlyParse
  | DumpCPP
  | SimulateODE Double Double Double
  | FitModel [Text] (Map Text Double)
  | ComputeError

data Options = Options
  { command :: Command
  , modelFiles :: [FilePath]
  , dataFiles :: [FilePath]
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

      , Option [] ["dbg-dump-cpp"]
        "Dump some c++ code"
        $ NoArg \s -> Right s { command = DumpCPP }

      , Option [] ["sim-ode"]
        "Solve using GSL's ODE solver"
        $ ReqArg "START:STEP:END"
          \a s -> do (start,step,end) <- parseODETimes a
                     Right s { command = SimulateODE start step end }

      , Option [] ["fit"]
        "Fit model parameters with optional residual scaling"
        $ ReqArg "PNAME or PNAME:SCALE"
          \a s -> do p <- parseParam a
                     let newCmd = case command s of
                                    FitModel ps mb -> addParam p ps mb
                                    _              -> addParam p [] Map.empty
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

addParam :: (Text,Maybe Double) -> [Text] -> Map Text Double -> Command
addParam (x,mb) xs scaled =
  FitModel (x:xs)
  case mb of
    Nothing -> scaled
    Just d  -> Map.insert x d scaled

parseParam :: String -> Either String (Text,Maybe Double)
parseParam xs =
  case break (==':') xs of
    ([], _) -> Left "Invalid parameter"
    (as,_:bs) ->
      case reads bs of
        [(d,"")] -> Right (Text.pack as, Just d)
        _ -> Left ("Invalid scaling in parameter " ++ show as)
    (as,[]) -> Right (Text.pack as, Nothing)


getOptions :: IO Options
getOptions =
  do opts <- getOpts options
     when (onlyShowHelp opts)
       do showHelp
          throwIO (GetOptException [])

     case command opts of
       SimulateODE {} ->
         unless (length (modelFiles opts) == 1)
            $ throwIO
            $ GetOptException ["Simulation expects a single model file"]
       _ -> pure ()

     when (gnuplot opts && null (outFile opts))
       $ throwIO (GetOptException ["gnuplot requires an output file"])

     pure opts


showHelp :: IO ()
showHelp = dumpUsage options
