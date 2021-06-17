{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.AlgebraicJulia.Simulate where

import Data.Aeson ( object, (.=) )
import Data.Map   ( Map )
import Data.Text  ( Text )

import Language.ASKEE.AlgebraicJulia.Interact ( queryServer )

type Gromet = String

simulate :: Gromet -> Map Text Double -> IO String
simulate model parameters =
  do  let payload = 
            object
              [ "model" .= model
              , "parameters" .= parameters ]
      result <- queryServer payload
      -- parse result into a DataSeries...
      undefined
