{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.ESL 
  ( checkModel
  , printModel
  , loadESL
  , loadESLFrom
  , loadESLMeta
  , loadESLMetaFrom
  , describeModelInterface

  , Model
  , ModelMeta
  )
  where

import Data.Aeson

import           Language.ASKEE.Error        ( ASKEEError(..), throwLeft )
import           Language.ASKEE.ESL.Check    ( checkModel )
import           Language.ASKEE.ESL.Print    ( printModel )
import           Language.ASKEE.ESL.Syntax   ( Model, ModelMeta, stripMeta )
import qualified Language.ASKEE.ESL.Syntax   as ESL
import qualified Language.ASKEE.Metadata     as Meta
import           Language.ASKEE.ModelType    ( ModelType(..) )
import           Language.ASKEE.Model        ( parseModel, toEasel )
import           Language.ASKEE.Storage      ( loadModel, DataSource )
-- import           Language.ASKEE.Types        ( DataSource )

loadESLMeta :: DataSource -> IO ESL.ModelMeta 
loadESLMeta = loadESLMetaFrom EaselType

loadESLMetaFrom :: ModelType -> DataSource -> IO ESL.ModelMeta
loadESLMetaFrom format source =
  do  modelString <- loadModel format source
      model <- throwLeft ParseError (parseModel format modelString)
      esl <- throwLeft ConversionError (toEasel model)
      _ <- throwLeft ValidationError (checkModel $ stripMeta esl)
      pure esl


loadESL :: DataSource -> IO ESL.Model
loadESL = loadESLFrom EaselType

loadESLFrom :: ModelType -> DataSource -> IO ESL.Model
loadESLFrom format source = stripMeta <$> loadESLMetaFrom format source

describeModelInterface :: ModelType -> DataSource -> IO Value
describeModelInterface modelType modelSource =
  case modelType of
    EaselType ->
      do  mdl <- loadESLMeta modelSource
          let stateVars =
                [(n, Meta.metaMap md) | md <- ESL.modelMetaDecls mdl
                                      , (ESL.State n _) <- [Meta.metaValue md]
                                      ]
              params =
                [(n, d, Meta.metaMap md) | md <- ESL.modelMetaDecls mdl
                                          , (ESL.Parameter n d) <- [Meta.metaValue md]
                                          ]

              descParam (n, d, mp) =
                object [ "name" .= n
                          , "defaultValue" .= d
                          , "metadata" .= mp
                          ]
              descState (n, mp) =
                object [ "name" .= n
                          , "metadata" .= mp
                          ]
              desc =
                object [ "parameters" .= (descParam <$> params)
                          , "stateVars"  .= (descState <$> stateVars)
                          ]
          pure desc
    _ -> undefined
