{-# Language BlockArguments, OverloadedStrings #-}
module Main(main) where

import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Control.Exception(catches, Handler(..),throwIO)
import Control.Monad(when,forM_)
import System.Exit(exitSuccess,exitFailure)
import System.FilePath(replaceExtension)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Numeric(showGFloat)

import Language.ASKEE
import qualified Language.ASKEE.Core as Core
import           Language.ASKEE.DEQ.Syntax ( DiffEqs(..) )
import           Language.ASKEE.DEQ.Print (ppDiffEqs)
import qualified Language.ASKEE.Core.DiffEq as DiffEq
import qualified Language.ASKEE.Core.GSLODE as ODE
import qualified Language.ASKEE.DataSeries as DS
import qualified Language.ASKEE.Print as PP
import Language.ASKEE.RNet.Reaction (reactionsAsModel)
import Language.ASKEE.Types

import Options

main :: IO ()
main =
  do opts <- getOptions
     case command opts of
       OnlyLex -> 
          forM_ (modelFiles opts) (lexModel . FromFile)
       OnlyParse -> 
          forM_ (modelFiles opts) (parseModel . FromFile)
       OnlyCheck ->
          forM_ (modelFiles opts) (loadModel . FromFile)
       DumpCPP -> 
          forM_ (modelFiles opts) 
            (genCppRunner . FromFile)

       DumpDEQs ->
         do ds <- exactlyOne "model" =<< loadDiffEqs opts []
            print (ppDiffEqs ds)

       SimulateODE x y z ->
         do m0 <- exactlyOne "model" =<< loadDiffEqs opts []
            let m     = DiffEq.applyParams (Core.NumLit <$> overwrite opts) m0
                res   = simODE m x y z
                bs    = DS.encodeDataSeries res
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
                 let showF f = showGFloat Nothing f ""
                 let (res,work) = ODE.fitModel eqs ds scale
                                          (Map.fromList (zip ps (repeat 0)))
                     see n xs =
                       do putStrLn n
                          forM_ (Map.toList xs) \(x,(y,e)) ->
                              putStrLn ("let " ++ Text.unpack x ++
                                        " = " ++ showF y ++
                                        " # error = " ++ showF e
                                       )
                 forM_ (zip [ (1::Int) .. ] work) \(n,ys) ->
                       do putStrLn ("-- Step " ++ show n ++ " --")
                          forM_ (Map.toList ys) \(x,y) ->
                              putStrLn ("let " ++ Text.unpack x ++
                                        " = " ++ showF y)

                 see "Result:" res
                 let totalErr = ODE.computeErrorPerVar
                              $ ODE.modelSquareError eqs ds (fst <$> res)
                 forM_ (Map.toList totalErr) \(x,e) ->
                    putStrLn $ "# error in " ++ Text.unpack x ++
                                                        " = " ++ showF e

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
loadDiffEqs opts ps0 =
  do ms1 <- mapM (`loadEquations` params) (map FromFile (deqFiles opts))
     ms2 <- mapM fromModel                (map FromFile (modelFiles opts))
     ms3 <- mapM fromRNet                 (map FromFile (rnetFiles opts))
     pure (ms1 ++ ms2 ++ ms3)
  where
  params = Map.keys (overwrite opts) ++ ps0

  fromModel file =
    do m <- loadCoreModel' file
       pure (DiffEq.asEquationSystem m)

  fromRNet file =
    do  rnet <- loadReactions file
        m <- case reactionsAsModel rnet of
          Right model -> pure model
          Left err -> fail err
        print (PP.printModel m)
        m' <- loadCoreModel' (Inline (Text.pack $ show $ PP.printModel m))
        pure (DiffEq.asEquationSystem m')

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


