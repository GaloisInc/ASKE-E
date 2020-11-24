{-# Language BlockArguments, OverloadedStrings #-}
module Main(main) where

import Data.List(transpose)
import qualified Data.Map as Map
import Control.Exception(catches, Handler(..))
import Control.Monad(when)
import System.Exit(exitSuccess,exitFailure)
import System.FilePath(replaceExtension)

import Language.ASKEE.Experiments
import Language.ASKEE.Core.DiffEq(asEquationSystem)
import qualified Language.ASKEE.Core.GSLODE as ODE

import Options

main :: IO ()
main =
  do opts <- getOptions
     case command opts of
       OnlyLex   -> mapM_ print =<< testLexModel (modelFile opts)
       OnlyParse -> print =<< testParseModel (modelFile opts)
       DumpCPP   -> genCppRunner (modelFile opts)
       SimulateODE x y z -> simODE opts x y z

  `catches`
  [ Handler  \(GetOptException errs) ->
      case errs of
        [] -> exitSuccess   -- showed help
        _  -> do mapM_ putStrLn errs
                 showHelp
                 exitFailure
  ]


simODE :: Options -> Double -> Double -> Double -> IO ()
simODE opts start step end =
  do let file = modelFile opts
         ofile = outFile opts
     m <- asEquationSystem <$> coreModel file
     let times = takeWhile (<= end) (iterate (+ step) start)
         res = ODE.simulate m times
         headings = unwords (map show ("time" : Map.keys res))
         columns = times : Map.elems res
         textData = unlines
                  $ headings
                  : map (unwords . map show) (transpose columns)
     if null ofile
       then putStrLn textData
       else do writeFile ofile textData
               when (gnuplot opts) $
                 writeFile (replaceExtension ofile "gnuplot") $
                   unlines $ gnuPlot (length columns) ofile

  where
  gnuPlot n f =
    [ "set key outside"
    , "plot for [col=2:" ++ show n ++ "] " ++
         show f ++ " using 1:col with lines title columnheader"
    ]


