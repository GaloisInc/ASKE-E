{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.ESL.Interface where

import Data.Aeson ( object, (.=), Value )

import qualified Language.ASKEE.ESL.Syntax as ESL
import qualified Language.ASKEE.Metadata   as Meta

-- TODO return something more structured
describeModelInterface :: ESL.Model -> Value
describeModelInterface model = desc
  where
    stateVars =
      [ (n, Meta.metaMap md) | md <- ESL.modelDecls model
      , (ESL.State n _) <- [Meta.metaValue md]
      ]
    params =
      [ (n, d, Meta.metaMap md) | md <- ESL.modelDecls model
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