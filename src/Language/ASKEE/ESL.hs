{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.ESL 
  ( checkModel
  , lexModel
  , parseModel
  , printModel
  , loadESL
  , loadESLFrom
  , loadMetaESL

  , Model
  , ModelMeta
  )
  where

import Data.Aeson

import Language.ASKEE.ESL.Check ( checkModel )
import Language.ASKEE.ESL.GenLexer ( lexModel )
import Language.ASKEE.ESL.GenParser ( parseModel )
import Language.ASKEE.ESL.Print ( printModel )
import Language.ASKEE.ESL.Syntax ( Model, ModelMeta )
import qualified Language.ASKEE.ESL.Syntax as ESL
import qualified Language.ASKEE.Metadata as Meta

import Language.ASKEE.Types ( DataSource, ModelType(..), Representation(..) )
import Language.ASKEE.Convert (converter)
import Language.ASKEE.Storage (loadModel)
import Language.ASKEE.Error

loadESL :: DataSource -> IO ESL.Model
loadESL = loadESLFrom (ESL Concrete)

loadESLFrom :: ModelType -> DataSource -> IO ESL.Model
loadESLFrom format source =
  do  modelString <- loadModel format source
      let conv =
            case format of
              ESL Concrete -> $(converter (ESL Concrete) (ESL Abstract))
              RNET Concrete -> $(converter (RNET Concrete) (ESL Abstract))
      model <- tryParse "loadESLFrom" Nothing $ conv modelString
      tryValidate "loadESLFrom" Nothing (checkModel model)

loadMetaESL :: DataSource -> IO ESL.ModelMeta
loadMetaESL source =
  do  modelString <- loadModel (ESLMETA Concrete) source
      let conv = $(converter (ESLMETA Concrete) (ESLMETA Abstract))
      tryParse "loadMetaESL" Nothing (conv modelString)

describeModelInterface :: ModelType -> DataSource -> IO Value
describeModelInterface modelType modelSource =
  case modelType of
    ESL _ ->
      do  mdl <- loadMetaESL modelSource
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
