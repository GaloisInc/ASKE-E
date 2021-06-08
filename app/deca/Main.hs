{-# Language BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
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

import qualified Language.ASKEE as A
-- import qualified Language.ASKEE.Core as Core
-- import           Language.ASKEE.DEQ.Syntax ( DiffEqs(..) )
-- import           Language.ASKEE.DEQ.Print (printDiffEqs)
-- import qualified Language.ASKEE.Core.DiffEq as DiffEq
-- import qualified Language.ASKEE.Core.GSLODE as ODE
-- import qualified Language.ASKEE.DataSeries as DS
-- import qualified Language.ASKEE.ESL.Print as PP
-- import Language.ASKEE.RNet.Reaction (reactionsAsModel)

import Options
import Language.ASKEE.Error
import qualified Language.ASKEE.DEQ as DEQ

main :: IO ()
main =
  do  opts <- getOptions
      case command opts of

      -- --  OnlyLex ->
      -- --     forM_ (modelFiles opts) (lexModel . FromFile)
      -- --  OnlyParse ->
      -- --     forM_ (modelFiles opts) (parseModel . FromFile)
      -- --  OnlyCheck ->
      -- --     forM_ (modelFiles opts) (loadModel . FromFile)
      -- --  DumpCPP ->
      -- --     forM_ (modelFiles opts)
      -- --       (genCppRunner . FromFile)

      --  DumpDEQs ->
      --    do ds <- exactlyOne "model" =<< loadDiffEqs opts []
      --       print (ppDiffEqs ds)
        SimulateODE start stop step ->
          do  (modelFile, modelType) <- exactlyOne "model-like thing" $ modelsProvided opts
              res <- A.simulateModel modelType (A.FromFile modelFile) start stop step (overwrite opts)
              let bs = A.dataSeriesAsCSV res
                  out = outFile opts
              if null out
                then LBS.putStrLn bs
                else LBS.writeFile out bs
              when (gnuplot opts && not (null out)) $
                writeFile (replaceExtension out "gnuplot") $
                  A.gnuPlotScript res out

        ComputeError ->
          do  eqss <- mapM (A.loadDiffEqs . A.FromFile) (deqFiles opts)
              forM_ eqss \eqs ->
                forM_ (dataFiles opts) \d ->
                  do  putStrLn ("  data: " ++ show d)
                      ds <- A.parseDataSeriesFromFile d
                      let errs = DEQ.computeErrorPerVar
                               $ DEQ.modelSquareError eqs ds Map.empty
                      forM_ (Map.toList errs) \(x,e) ->
                        putStrLn ("    " ++ Text.unpack x ++ ": " ++ show e)

        FitModel ps scale ->
          do  (modelFile, modelType) <- exactlyOne "model-like thing" $ modelsProvided opts
              equations <- A.loadDiffEqsFrom modelType (A.FromFile modelFile)
              dataFile <- exactlyOne "data file" (dataFiles opts)
              ds <- A.parseDataSeriesFromFile dataFile
              (res,work) <- A.fitModelToData modelType (A.FromFile dataFile) ps scale (A.FromFile modelFile)
              let showF f = showGFloat Nothing f ""
                  see n xs =
                    do  putStrLn n
                        forM_ (Map.toList xs) \(x,(y,e)) ->
                          putStrLn ("let " ++ Text.unpack x ++
                                    " = " ++ showF y ++
                                    " # error = " ++ showF e)
              forM_ (zip [ (1::Int) .. ] work) \(n,ys) ->
                do  putStrLn ("-- Step " ++ show n ++ " --")
                    forM_ (Map.toList ys) \(x,y) ->
                      putStrLn ("let " ++ Text.unpack x ++
                                " = " ++ showF y)
              see "Result:" res
              let totalErr = DEQ.computeErrorPerVar
                           $ DEQ.modelSquareError equations ds (fst <$> res)
              forM_ (Map.toList totalErr) \(x,e) ->
                putStrLn $ "# error in " ++ Text.unpack x ++ " = " ++ showF e

        _ -> throwIO (GetOptException [""])

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

modelsProvided :: Options -> [(FilePath, A.ModelType)]
modelsProvided opts =
  map (, A.DeqType) (deqFiles opts) ++ map (, A.EaselType) (modelFiles opts)