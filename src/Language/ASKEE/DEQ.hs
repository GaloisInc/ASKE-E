module Language.ASKEE.DEQ
  ( loadDiffEqs
  , loadDiffEqsFrom
  , simulate
  , fitModel
  ) where

import           Data.Map  ( Map )
import qualified Data.Map  as Map
import           Data.Text ( Text )

import Language.ASKEE.DEQ.Syntax as DEQ
import qualified Language.ASKEE.Core.Syntax as Core
import Language.ASKEE.Core.Convert ( applyParams )
import Language.ASKEE.ModelType ( ModelType(..) )
import Language.ASKEE.Storage (loadModel, DataSource)
import Language.ASKEE.Error
import Language.ASKEE.Model
import Language.ASKEE.DEQ.Simulate (simulate, fitModel)

loadDiffEqs :: Map Text Double -> [Text] -> DataSource -> IO DEQ.DiffEqs
loadDiffEqs = loadDiffEqsFrom DeqType

loadDiffEqsFrom :: 
  ModelType ->
  Map Text Double -> 
  [Text] -> 
  DataSource -> 
  IO DEQ.DiffEqs
loadDiffEqsFrom format overwrite params source = 
  do  modelString <- loadModel format source
      model <- throwLeft ParseError (parseModel DeqType modelString)
      equations <- throwLeft ConversionError (toDeqs model)
      let eqns' = equations { DEQ.deqParams = params }
      let replaceParams = applyParams (Map.map Core.NumLit overwrite)
      pure $ replaceParams eqns'