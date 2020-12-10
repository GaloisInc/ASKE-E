{-# Language BlockArguments, OverloadedStrings #-}
module Main(main) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Exception(catches, Handler(..),throwIO)
import Control.Monad(when,forM_,(>=>))
import System.Exit(exitSuccess,exitFailure)
import System.FilePath(replaceExtension)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Numeric(showFFloat)

import Language.ASKEE
import Language.ASKEE.Core.DiffEq(asEquationSystem)
import qualified Language.ASKEE.Core.GSLODE as ODE
import qualified Language.ASKEE.DataSeries as DS

import Options

main :: IO ()
main =
  do opts <- getOptions
     case command opts of
       OnlyLex -> 
          forM_ (modelFiles opts) $ 
            lexFile >=> either fail print
       OnlyParse -> 
          forM_ (modelFiles opts) $ 
            parseFile >=> either fail print
       OnlyCheck ->
          forM_ (modelFiles opts)
            checkPrint
       DumpCPP -> 
          forM_ (modelFiles opts) 
            genCppRunner

       SimulateODE x y z ->
         do res <- simODE opts x y z
            let bs    = DS.encodeDataSeries res
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
               model <- either fail pure <$> genCoreModel m []
               eqs <- asEquationSystem <$> model
               forM_ (dataFiles opts) \d ->
                 do putStrLn ("  data: " ++ show d)
                    ds <- DS.parseDataSeriesFromFile d
                    let errs = ODE.computeErrorPerVar
                             $ ODE.modelSquareError eqs ds Map.empty
                    forM_ (Map.toList errs) \(x,e) ->
                      putStrLn ("    " ++ Text.unpack x ++ ": " ++ show e)

       FitModel ps scale ->
         case (modelFiles opts, dataFiles opts) of
           ([mf],[df]) ->
              do model <- either fail pure <$> genCoreModel mf ps
                 eqs <- asEquationSystem <$> model
                 ds  <- DS.parseDataSeriesFromFile df
                 let (res,work) = ODE.fitModel eqs ds scale
                                          (Map.fromList (zip ps (repeat 0)))
                     see n xs =
                       do putStrLn n
                          forM_ (Map.toList xs) \(x,y) ->
                              putStrLn ("let " ++ Text.unpack x ++
                                              " = " ++ showFFloat (Just 4) y "")
                 forM_ (zip [ (1::Int) .. ] work) \(n,ys) ->
                     see ("-- Step " ++ show n ++ " --") ys

                 see "Result:" res

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
     model <- either fail pure <$> genCoreModel file []
     m <- asEquationSystem <$> model
     let times = takeWhile (<= end) (iterate (+ step) start)
     pure (ODE.simulate m Map.empty times)


gnuPlotScript :: DS.DataSeries Double -> FilePath -> String
gnuPlotScript ds f =
  unlines
    [ "set key outside"
    , "set datafile separator \",\""
    , "plot for [col=2:" ++ show (DS.dsColumns ds) ++ "] " ++
         show f ++ " using 1:col with lines title columnheader"
    ]


