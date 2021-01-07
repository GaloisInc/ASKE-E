{-# Language BlockArguments, OverloadedStrings #-}
module Main(main) where

import Data.Text(Text)
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
import           Language.ASKEE.DEQ.Syntax ( DiffEqs(..) )
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



