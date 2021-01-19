{-# Language BlockArguments, OverloadedStrings #-}
module Main(main) where

import Data.Text(Text)
import qualified Text.PrettyPrint as PP
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JS
import Control.Monad.IO.Class(liftIO)
import Control.Exception(try,SomeException)
import Snap.Core(Snap)
import qualified Snap.Core as Snap
import Snap.Http.Server (quickHttpServe)

import Language.ASKEE
import qualified Language.ASKEE.Core as Core
-- import Language.ASKEE.Core.DiffEq(DiffEqs)
import           Language.ASKEE.DEQ.Syntax ( DiffEqs(..), ppDiffEqs )
import qualified Language.ASKEE.Core.GSLODE as ODE
import qualified Language.ASKEE.Core.DiffEq as DiffEq
import Schema

import qualified Data.ByteString.Lazy.Char8 as BS8

main :: IO ()
main = quickHttpServe
  do let limit = 8 * 1024 * 1024    -- 8 megs
     body <- Snap.readRequestBody limit
     case JS.decode' body of
       Just a ->
         do r <- liftIO $ try $ handleRequest a
            case r of
              Right ok ->
                do Snap.modifyResponse (Snap.setResponseStatus 200 "OK")
                   Snap.writeLBS (JS.encode ok)
              Left err ->
                do Snap.modifyResponse
                              (Snap.setResponseStatus 400 "Bad request")
                   Snap.writeText $ Text.pack $ show (err :: SomeException)
       Nothing ->
         do Snap.modifyResponse (Snap.setResponseStatus 400 "Bad request")
            showHelp

--------------------------------------------------------------------------------

showHelp :: Snap ()
showHelp = Snap.writeLBS helpHTML

handleRequest :: Input -> IO Output
handleRequest r =
  print r >>
  case r of
    Simulate info ->
      do eqs <- loadDiffEqs (simModelType info)
                            (simModel info)
                            []
                            (simOverwrite info)
         let times = takeWhile (<= simEnd info)
                   $ iterate (+ simStep info)
                   $ simStart info
             res = ODE.simulate eqs Map.empty times
         pure (OutputData res)

    CheckModel cmd ->
      do  checkResult <- checkModel (checkModelModelType cmd) (checkModelModel cmd)
          case checkResult of
            Nothing  -> pure $ OutputResult (SuccessResult ())
            Just err -> pure $ OutputResult (FailureResult (Text.pack err))

    ConvertModel cmd ->
      do  eConverted <- convertModel (convertModelSourceType cmd)
                                     (convertModelModel cmd)
                                     (convertModelDestType cmd)

          pure $ OutputResult (asResult eConverted)

    Fit _ -> error "not implemented"



loadDiffEqs ::
  ModelType       {- ^ input file format -} ->
  DataSource      {- ^ where to get the data from -} ->
  [Text]          {- ^ parameters, if any -} ->
  Map Text Double {- ^ overwrite these parameters -} ->
  IO DiffEqs
loadDiffEqs mt src ps0 overwrite =
  fmap (DiffEq.applyParams (Core.NumLit <$> overwrite))
  case mt of
    Schema.DiffEqs    -> loadEquations src allParams
    AskeeModel -> DiffEq.asEquationSystem <$> loadCoreModel src allParams
  where
  allParams = Map.keys overwrite ++ ps0

checkModel :: ModelType -> DataSource -> IO (Maybe String)
checkModel mt src =
  case mt of
    Schema.DiffEqs    -> checkUsing parseEquations src
    Schema.AskeeModel -> checkUsing parseModel src
  where
    checkUsing parser ds =
      do  mbMdl <- try (parser ds)
          case mbMdl of
            Left (ParseError err) -> pure $ Just err
            Right _               -> pure Nothing


convertModel :: ModelType -> DataSource -> ModelType -> IO (Either Text Text)
convertModel inputType source outputType =
  case (inputType, outputType) of
    (_, Schema.DiffEqs) ->
      do  eq <- loadDiffEqs inputType source [] Map.empty
          pure $ Right (Text.pack . PP.render . ppDiffEqs $ eq)

    (_, _) ->
      pure $ Left "Conversion is not implemented"



