{-# Language BlockArguments #-}
module Options
  ( Options(..)
  , Command(..)
  , ShowGromet(..)
  , getOptions
  , GetOptException(..)
  , showHelp
  ) where

import Data.Text(Text)
import Data.Set(Set)
import qualified Data.Set as Set
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
  | DumpDEQs
  | DumpPNC
  | DumpCore
  | DescribeInterface
  | SimulateODE Double Double Double -- ^ Start, step, end
  | SimulateCPP Double Double Double -- ^ Start, step, end
  | FitModel [Text] (Map Text Double)
  | ComputeError
  | ShowGromet ShowGromet
  | ConvertToCTMC
  deriving Show

data ShowGromet = JSON | PP
  deriving Show

data Options = Options
  { command :: Command
  , modelFiles :: [FilePath]
  , dataFiles :: [FilePath]
  , deqFiles :: [FilePath]
  , rnetFiles :: [FilePath]
  , pncFiles :: [FilePath]
  , outFile :: FilePath
  , gnuplot :: Bool
  , overwrite :: Map Text Double
  , measures :: Set Text
  , seed :: Maybe Int
  , onlyShowHelp :: Bool
  }
  deriving Show

options :: OptSpec Options
options = OptSpec
  { progDefaults =
      Options
        { command = DumpCPP
        , modelFiles = []
        , dataFiles = []
        , deqFiles = []
        , pncFiles = []
        , rnetFiles = []
        , onlyShowHelp = False
        , gnuplot = False
        , measures = Set.empty
        , overwrite = Map.empty
        , seed = Nothing
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

      , Option [] ["to-gromet"]
        "Convert to GroMEt"
        $ NoArg \s -> Right s { command = ShowGromet JSON }

      , Option [] ["dump-pnc-gromet"]
         "Parse a Petri Net Classic Gromet and print it"
        $ NoArg \s -> Right s { command = DumpPNC }

      , Option [] ["dump-core"]
         "Try to convert input to Core"
        $ NoArg \s -> Right s { command = DumpCore }

      , Option [] ["to-pp-gromet"]
        "Convert to human readable GroMEt"
        $ NoArg \s -> Right s { command = ShowGromet PP }

      , Option [] ["describe-interface"]
        "Desribe the interface of a model in JSON"
        $ NoArg \s -> Right s { command = DescribeInterface }

      , Option [] ["to-deq"]
        "Convert to differental equations"
        $ NoArg \s -> Right s { command = DumpDEQs }

      , Option [] ["sim-ode"]
        "Solve a model using GSL's ODE solver"
        $ ReqArg "START:STEP:END"
          \a s -> do (start,step,end) <- parseODETimes a
                     Right s { command = SimulateODE start step end }

      , Option [] ["obs"]
        "Observe this variable"
        $ ReqArg "IDENT"
        \a s -> Right s { measures = Set.insert (Text.pack a) (measures s) }

      , Option [] ["sim-cpp"]
        "Solve a model using a C++-based discrete event simulator"
        $ ReqArg "START:STEP:END"
          \a s -> do (start,step,end) <- parseODETimes a
                     Right s { command = SimulateCPP start step end }

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

      , Option [] ["let"]
        "Overwrite the value of a constant in the model."
        $ ReqArg "VNAME:DOUBLE"
        \a s -> do (x,d) <- parseOverwrite a
                   Right s { overwrite = Map.insert x d (overwrite s) }

      , Option [] ["seed"]
        "Use a particular seed during discrete event simulation"
        $ ReqArg "INT"
        \a s -> do i <- parseInt a
                   Right s { seed = Just i }

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

      , Option ['r'] ["rnet"]
        "Use this reaction network"
        $ ReqArg "FILE" \a s -> Right s { rnetFiles = a : rnetFiles s}

      , Option [] ["pnc"]
        "Use this Petri Net Classic"
        $ ReqArg "FILE" \a s -> Right s { pncFiles = a : pncFiles s}



      , Option ['o'] ["output"]
        "Use this output file"
        $ ReqArg "FILE" \a s -> case outFile s of
                                  "" -> Right s { outFile = a }
                                  _ -> Left "Multiple output files"

      , Option [] ["help"]
        "Show this help"
        $ NoArg \s -> Right s { onlyShowHelp = True }

      , Option [] ["core-to-ctmc"]
        "Converts core model to CTMC"
        $ NoArg \s -> Right s { command = ConvertToCTMC }
      ]

  , progParamDocs = []
  , progParams = \_ _ -> Left "Unexpected parameter"

  }

parseInt :: String -> Either String Int
parseInt xs =
  case reads xs of
    [(i, "")] -> Right i
    _         -> Left "Malformed integer"

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

parseOverwrite :: String -> Either String (Text,Double)
parseOverwrite xs =
  case break (==':') (reverse xs) of
    (as,_:bs)
      | [(d,"")] <- reads (reverse as) -> Right (Text.pack (reverse bs), d)
    _ -> Left "Invalid overwite, format is NAME:DOUBLE"


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
