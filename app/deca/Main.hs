{-# Language BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main(main) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Exception(catches, Handler(..),throwIO)
import Control.Monad(when,forM_)
import System.Exit(exitSuccess,exitFailure)
import System.FilePath(replaceExtension)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Numeric(showGFloat)

import qualified Data.Aeson as JSON

import Language.ASKEE.Gromet.PetriNetClassic(pnFromGromet, ppPetriNet)
import qualified Language.ASKEE.Gromet.See as Gromet
import qualified Language.ASKEE as A
import qualified Language.ASKEE.Model as Model
import qualified Language.ASKEE.DEQ as DEQ
import qualified Language.ASKEE.Core as Core

import Options

main :: IO ()
main =
  do  opts <- getOptions
      case command opts of


        DumpPNC -> mapM_ dumpPNC (modelFiles opts)

        DumpDEQs -> mapM_ dumpDEQ  (modelsProvided opts)
        DumpCore -> mapM_ dumpCore (modelsProvided opts)

        DescribeInterface -> mapM_ testDescirbeInterface (modelsProvided opts)

        ShowGromet how -> mapM_ (showGromet how) (modelsProvided opts)

        SimulateODE start step stop ->
          do  (modelFile, modelType) <- exactlyOne "model-like thing" $ modelsProvided opts
              res <- A.simulateModelGSL modelType (A.FromFile modelFile) start stop step (overwrite opts) (measures opts)
              let bs = A.dataSeriesAsCSV res
                  out = outFile opts
              if null out
                then LBS.putStrLn bs
                else LBS.writeFile out bs
              when (gnuplot opts && not (null out)) $
                writeFile (replaceExtension out "gnuplot") $
                  A.gnuPlotScript res out

        SimulateCPP start step stop ->
          do  (modelFile, modelType) <- exactlyOne "model-like thing" $ modelsProvided opts
              res <- head <$> A.simulateModelDiscrete modelType (A.FromFile modelFile) start stop step (overwrite opts) (seed opts) 1
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
              ifaceErrs <- A.checkFitArgs modelType (A.FromFile modelFile) ps
              case ifaceErrs of
                [] ->
                  do (fit, work) <- A.fitModelToData modelType (A.FromFile dataFile) ps scale (A.FromFile modelFile)
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
                     see "Result:" fit
                     let totalErr = DEQ.computeErrorPerVar
                                  $ DEQ.modelSquareError equations ds (fst <$> fit)
                     forM_ (Map.toList totalErr) \(x,e) ->
                       putStrLn $ "# error in " ++ Text.unpack x ++ " = " ++ showF e
                errs -> forM_ errs (putStrLn . Text.unpack)

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
  map (, A.DeqType) (deqFiles opts) ++
  map (, A.EaselType) (modelFiles opts) ++
  map (, A.GrometPncType) (pncFiles opts) ++
  map (, A.RNetType) (rnetFiles opts) ++
  map (, A.GrometFnetType) (fnetFiles opts)

dumpPNC :: FilePath -> IO ()
dumpPNC file =
  do mb <- JSON.eitherDecodeFileStrict' file
     case pnFromGromet =<< mb of
       Right a -> print (ppPetriNet a)
       Left err -> print err

dumpCore :: (FilePath, A.ModelType) -> IO ()
dumpCore (file,ty) =
  do m <- A.loadModel ty (A.FromFile file)
     case Model.toCore m of
       Right c -> print (Core.ppModel c)
       Left err ->
         putStrLn $ unlines [ "Failed to convert to Core"
                            , "file: " ++ show file
                            , "type:" ++ show ty
                            , "error:" ++ err
                            ]


dumpDEQ :: (FilePath, A.ModelType) -> IO ()
dumpDEQ (file,ty) =
  do m <- A.loadModel ty (A.FromFile file)
     case Model.toDeqs m of
       Right c -> print (DEQ.printDiffEqs c)
       Left err ->
         putStrLn $ unlines [ "Failed to convert to DEqs"
                            , "file: " ++ show file
                            , "type:" ++ show ty
                            , "error:" ++ err
                            ]




testDescirbeInterface :: (FilePath, A.ModelType) -> IO ()
testDescirbeInterface (file,ty) =
  do m <- A.loadModel ty (A.FromFile file)
     LBS.putStrLn (JSON.encode (A.describeModelInterface m))

showGromet :: ShowGromet -> (FilePath, A.ModelType) -> IO ()
showGromet how (f,t) =
  do m <- A.loadGrometPrtFrom t (A.FromFile f)
     case how of
       JSON -> LBS.putStrLn (JSON.encode m)
       PP   -> print (Gromet.pp m)
