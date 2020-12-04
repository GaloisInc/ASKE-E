{-# Language BlockArguments, OverloadedStrings #-}
module Main(main) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Exception(catches, Handler(..),throwIO)
import Control.Monad(when,forM_)
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
       OnlyLex   -> forM_ (modelFiles opts) \f ->
                        mapM_ print =<< testLexModel f
       OnlyParse -> forM_ (modelFiles opts) \f ->
                      print =<< testParseModel f
       DumpCPP   -> forM_ (modelFiles opts) \f ->
                      genCppRunner f

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

       ComputeError ->
          forM_ (modelFiles opts) \m ->
            do putStrLn ("model: " ++ show m)
               eqs <- asEquationSystem <$> coreModel m []
               forM_ (dataFiles opts) \d ->
                 do putStrLn ("  data: " ++ show d)
                    ds <- DS.parseDataSeriesFromFile d
                    let errs = ODE.computeErrorPerVar
                             $ ODE.modelSquareError eqs ds Map.empty
                    forM_ (Map.toList errs) \(x,e) ->
                      putStrLn ("    " ++ Text.unpack x ++ ": " ++ show e)

       FitModel ps ->
         case (modelFiles opts, dataFiles opts) of
           ([mf],[df]) ->
              do eqs <- asEquationSystem <$> coreModel mf ps
                 ds  <- DS.parseDataSeriesFromFile df
                 let res = ODE.fitModel eqs ds 1e-4 1e-4 1000
                                (Map.fromList (zip ps (repeat 0)))
                 forM_ (Map.toList res) \(x,y) ->
                   putStrLn ("let " ++ Text.unpack x ++ " = " ++ show y)

           _ -> throwIO (GetOptException
                           ["Fitting needs 1 model and 1 data file. (for now)"])

  `catches`
  [ Handler  \(GetOptException errs) ->
      case errs of
        [] -> exitSuccess   -- showed help
        _  -> do mapM_ putStrLn errs
                 showHelp
                 exitFailure
  ]



simODE :: Options -> Double -> Double -> Double -> IO (DS.DataSeries Double)
simODE opts start step end =
  do let file = head (modelFiles opts)
     m <- asEquationSystem <$> coreModel file []
     let times = takeWhile (<= end) (iterate (+ step) start)
     pure (ODE.simulate m Map.empty times)


gnuPlotScript :: DS.DataSeries Double -> FilePath -> String
gnuPlotScript ds f =
  unlines
    [ "set key outside"
    , "plot for [col=2:" ++ show (DS.dsColumns ds) ++ "] " ++
         show f ++ " using 1:col with lines title columnheader"
    ]


