{-# Language BlockArguments, OverloadedStrings #-}
module Main(main) where

import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Control.Exception(catches, Handler(..),throwIO)
import Control.Monad(when,forM_,(>=>))
import System.Exit(exitSuccess,exitFailure)
import System.FilePath(replaceExtension)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Numeric(showFFloat)

import Language.ASKEE
import Language.ASKEE.Core.DiffEq(asEquationSystem,DiffEqs)
import qualified Language.ASKEE.Core.GSLODE as ODE
import qualified Language.ASKEE.DataSeries as DS

import Options

main :: IO ()
main =
  do opts <- getOptions
     case command opts of
       OnlyLex -> 
          forM_ (modelFiles opts) $ 
            lexModel >=> either fail print
       OnlyParse -> 
          forM_ (modelFiles opts) $ 
            parseModel >=> either fail print
       OnlyCheck ->
          forM_ (modelFiles opts)
            checkModel
       DumpCPP -> 
          forM_ (modelFiles opts) 
            genCppRunner

       SimulateODE x y z ->
         do m <- exactlyOne "model" =<< loadDiffEqs opts []
            let res = simODE m x y z
            let bs    = DS.encodeDataSeries res
                ofile = outFile opts
            if null ofile
              then LBS.putStrLn bs
              else LBS.writeFile (outFile opts) bs
            when (gnuplot opts && not (null ofile)) $
              writeFile (replaceExtension ofile "gnuplot")
                $ gnuPlotScript res ofile

       ComputeError ->
         do eqss <- loadDiffEqs opts []
            forM_ eqss \eqs ->
              forM_ (dataFiles opts) \d ->
                do putStrLn ("  data: " ++ show d)
                   ds <- DS.parseDataSeriesFromFile d
                   let errs = ODE.computeErrorPerVar
                            $ ODE.modelSquareError eqs ds Map.empty
                   forM_ (Map.toList errs) \(x,e) ->
                     putStrLn ("    " ++ Text.unpack x ++ ": " ++ show e)

       FitModel ps scale ->
         case dataFiles opts of
           [df] ->
              do eqs <- exactlyOne "model" =<< loadDiffEqs opts ps
                 ds  <- DS.parseDataSeriesFromFile df
                 let (res,work) = ODE.fitModel eqs ds scale
                                          (Map.fromList (zip ps (repeat 0)))
                     see n xs =
                       do putStrLn n
                          forM_ (Map.toList xs) \(x,(y,e)) ->
                              putStrLn ("let " ++ Text.unpack x ++
                                        " = " ++
                                        showFFloat (Just 4) y
                                           (" # error = " ++ showFFloat (Just 4) e "")
                                       )
                 forM_ (zip [ (1::Int) .. ] work) \(n,ys) ->
                       do putStrLn ("-- Step " ++ show n ++ " --")
                          forM_ (Map.toList ys) \(x,y) ->
                              putStrLn ("let " ++ Text.unpack x ++
                                        " = " ++
                                        showFFloat (Just 4) y "")
 


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

exactlyOne :: String -> [a] -> IO a
exactlyOne thing xs =
  case xs of
    [a] -> pure a
    _   -> throwIO (GetOptException [ "Expected exactly 1 " ++ thing ])


loadDiffEqs :: Options -> [Text] -> IO [DiffEqs]
loadDiffEqs opts params =
  do ms1 <- mapM fromDiffEq (deqFiles opts)
     ms2 <- mapM fromModel  (modelFiles opts)
     pure (ms1 ++ ms2)

  where
  fromDiffEq file = either fail pure =<< loadEquations file params
  fromModel file  = either fail (pure . asEquationSystem) =<<
                                                      genCoreModel file params

simODE :: DiffEqs -> Double -> Double -> Double -> DS.DataSeries Double
simODE m start step end = ODE.simulate m Map.empty times
  where times = takeWhile (<= end) (iterate (+ step) start)

gnuPlotScript :: DS.DataSeries Double -> FilePath -> String
gnuPlotScript ds f =
  unlines
    [ "set key outside"
    , "set datafile separator \",\""
    , "plot for [col=2:" ++ show (DS.dsColumns ds) ++ "] " ++
         show f ++ " using 1:col with lines title columnheader"
    ]


