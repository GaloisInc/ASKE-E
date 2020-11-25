{-# Language BlockArguments, OverloadedStrings #-}
module Main(main) where

import Control.Exception(catches, Handler(..))
import Control.Monad(when)
import System.Exit(exitSuccess,exitFailure)
import System.FilePath(replaceExtension)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Language.ASKEE.Experiments
import Language.ASKEE.Core.DiffEq(asEquationSystem)
import qualified Language.ASKEE.Core.GSLODE as ODE
import qualified Language.ASKEE.DataSeries as DS

import Options

main :: IO ()
main =
  do opts <- getOptions
     case command opts of
       OnlyLex   -> mapM_ print =<< testLexModel (modelFile opts)
       OnlyParse -> print =<< testParseModel (modelFile opts)
       DumpCPP   -> genCppRunner (modelFile opts)
       SimulateODE x y z ->
         do res <- simODE opts x y z
            let sep = toEnum $ fromEnum $ if gnuplot opts then ' ' else ','
                bs  = DS.encodeDataSeries sep res
                ofile = outFile opts
            if null ofile
              then LBS.putStrLn bs
              else LBS.writeFile (outFile opts) bs
            when (gnuplot opts && not (null ofile)) $
              writeFile (replaceExtension ofile "gnuplot")
                $ gnuPlotScript res ofile

  `catches`
  [ Handler  \(GetOptException errs) ->
      case errs of
        [] -> exitSuccess   -- showed help
        _  -> do mapM_ putStrLn errs
                 showHelp
                 exitFailure
  ]


simODE :: Options -> Double -> Double -> Double -> IO DS.DataSeries
simODE opts start step end =
  do let file = modelFile opts
     m <- asEquationSystem <$> coreModel file
     let times = takeWhile (<= end) (iterate (+ step) start)
     pure (ODE.simulate m times)


gnuPlotScript :: DS.DataSeries -> FilePath -> String
gnuPlotScript ds f =
  unlines
    [ "set key outside"
    , "plot for [col=2:" ++ show (DS.dsColumns ds) ++ "] " ++
         show f ++ " using 1:col with lines title columnheader"
    ]


