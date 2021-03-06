{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Gromet.FunctionNetwork where

import qualified Data.HashMap.Lazy as HashMap
import Data.Text(Text)
import Data.Maybe(listToMaybe)
import qualified Data.Aeson as JSON
import qualified Language.ASKEE.Model.Interface as MI
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Language.ASKEE.Model.Basics as MB

type FunctionNetwork = JSON.Value

fnetInterface :: JSON.Value -> Either Text MI.ModelInterface
fnetInterface root =
  do  metas <- objLookup root "metadata" >>= arr
      let mbInterface = listToMaybe  [m | m <- metas
                                        , Just "ModelInterface" == try (objLookup m "metadata_type" >>= text)
                                        ]

      interface <-
        case mbInterface of
          Nothing -> Left "Cannot find interface in metadata"
          Just i -> Right i


      variableInfo <- objLookup root "variables"


      varV <- objLookup interface "variables" >>= arr
      vars <- text `traverse` varV

      stV <- objLookup interface "parameters" >>= arr
      states <- text `traverse` stV

      icV <- objLookup interface "initial_conditions" >>= arr
      ics <- text `traverse` icV

      let params = ics ++ vars

      paramPorts <- varToPort variableInfo `traverse` params
      statePorts <- varToPort variableInfo `traverse` states

      pure
        MI.ModelInterface { MI.modelInputs = paramPorts
                          , MI.modelOutputs = statePorts
                          }
  where
    varToPort vars uid =
      do  var <- findUid vars uid
          tyValue <- objLookup var "type" >>= text
          ty <-
            case MB.parseValueType tyValue of
              Nothing -> Left ("could not parse value type '" <> tyValue <> "'")
              Just v -> Right v

          pure MI.Port { MI.portName = uid
                       , MI.portValueType = ty
                       , MI.portDefault = Nothing
                       , MI.portMeta = Map.empty -- TODO
                       }

    text (JSON.String t) = Right t
    text _ = Left "expecting string"

    obj (JSON.Object o) = Right o
    obj _ = Left "expecting object"

    findUid a uid =
      do  elts <- arr a
          let mbVal = listToMaybe [ e | e <- elts
                                      , Just uid == try (objLookup e "uid" >>= text)]
          case mbVal of
            Nothing -> Left ("could not find variable with uid '" <> uid <> "'")
            Just v -> Right v


    objLookup o key =
      do  o' <- obj o
          case HashMap.lookup key o' of
            Nothing -> Left ("key '" <> key <> "' not found in object")
            Just s -> pure s

    arr (JSON.Array a) = Right (Vector.toList a)
    arr _ = Left "expecting array"

    try p =
      case p of
        Left _ -> Nothing
        Right a -> Just a


