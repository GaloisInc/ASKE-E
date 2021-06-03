{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.ESL.Interface where

import Data.Aeson ( object, (.=), Value )

-- import           Language.ASKEE.ESL.Check     ( checkModel )
-- import           Language.ASKEE.ESL.Convert   ( modelAsCore )
-- import           Language.ASKEE.ESL.GenParser ( parseModel, parseModelMeta )
-- import           Language.ASKEE.ESL.GenLexer  ( lexModel )
-- import           Language.ASKEE.ESL.Print     ( printModel )
-- import           Language.ASKEE.ESL.Syntax    ( Model, ModelMeta )
import qualified Language.ASKEE.ESL.Syntax    as ESL
import qualified Language.ASKEE.Metadata      as Meta

describeModelInterface :: ESL.ModelMeta -> Value
describeModelInterface model = desc
  where
    stateVars =
      [ (n, Meta.metaMap md) | md <- ESL.modelMetaDecls model
      , (ESL.State n _) <- [Meta.metaValue md]
      ]
    params =
      [ (n, d, Meta.metaMap md) | md <- ESL.modelMetaDecls model
      , (ESL.Parameter n d) <- [Meta.metaValue md]
      ]

    descParam (n, d, mp) =
      object
        [ "name" .= n
        , "defaultValue" .= d
        , "metadata" .= mp
        ]
    descState (n, mp) =
      object 
        [ "name" .= n
        , "metadata" .= mp
        ]
    desc =
      object 
        [ "parameters" .= (descParam <$> params)
        , "stateVars"  .= (descState <$> stateVars)
        ]