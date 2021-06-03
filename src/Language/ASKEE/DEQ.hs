module Language.ASKEE.DEQ
  ( loadDiffEqs
  , loadDiffEqsFrom
  , simulate
  , fitModel

  , DiffEqs
  ) where

import           Data.Map  ( Map )
import qualified Data.Map  as Map
import           Data.Text ( Text )

import Language.ASKEE.DEQ.Syntax   ( DiffEqs(deqParams) )
import Language.ASKEE.Core.Syntax  hiding ( applyParams )
import Language.ASKEE.Core.Convert ( applyParams )
import Language.ASKEE.ModelType    ( ModelType(..) )
import Language.ASKEE.Storage      ( loadModel, DataSource)
import Language.ASKEE.Error        ( throwLeft
                                   , ASKEEError(ConversionError, ParseError) )
import Language.ASKEE.Model        ( toDeqs, parseModel )
import Language.ASKEE.DEQ.Simulate ( simulate, fitModel)

loadDiffEqs :: Map Text Double -> [Text] -> DataSource -> IO DiffEqs
loadDiffEqs = loadDiffEqsFrom DeqType

loadDiffEqsFrom :: 
  ModelType ->
  Map Text Double -> 
  [Text] -> 
  DataSource -> 
  IO DiffEqs
loadDiffEqsFrom format overwrite params source = 
  do  modelString <- loadModel format source
      model <- throwLeft ParseError (parseModel DeqType modelString)
      equations <- throwLeft ConversionError (toDeqs model)
      let eqns' = equations { deqParams = params }
      let replaceParams = applyParams (Map.map NumLit overwrite)
      pure $ replaceParams eqns'