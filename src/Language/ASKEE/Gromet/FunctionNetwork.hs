{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Gromet.FunctionNetwork where

import qualified Data.HashMap.Lazy as HashMap
import Data.Text(Text)
import Data.Maybe(listToMaybe, maybeToList)
import qualified Data.Aeson as JSON
import qualified Language.ASKEE.Model.Interface as MI
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Language.ASKEE.Model.Basics as MB

type FunctionNetwork = JSON.Value

data FNetModelLevelMeta = FNetModelLevelMeta
  { fmlmName :: Text
  , fmlmDescription :: Text
  }

data FNetInfo = FNetInfo
  { fiInterface :: MI.ModelInterface
  , fiModelLevelMeta :: Maybe FNetModelLevelMeta
  }

fnetInterface:: JSON.Value -> Either Text MI.ModelInterface
fnetInterface v = fiInterface <$> fnetInfo v

fnetInfo :: JSON.Value -> Either Text FNetInfo
fnetInfo root =
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

      let params = ics ++ states

      paramPorts <- varToPort variableInfo `traverse` params
      statePorts <- varToPort variableInfo `traverse` vars

      let descMlm = getDesc metas
      pure
        FNetInfo
          { fiInterface =
             MI.ModelInterface { MI.modelInputs = paramPorts
                               , MI.modelOutputs = statePorts
                               }
          , fiModelLevelMeta = descMlm
          }
  where
    getDesc metas = try (find nameDesc metas)

    nameDesc :: JSON.Value -> Either Text FNetModelLevelMeta
    nameDesc md =
      do  mdTy <- objLookup md "metadata_type"
          if mdTy == "ModelDescription"
            then Right ()
            else Left ("Not a description metadata" :: Text)
          name <- objLookup md "name" >>= text
          desc <- objLookup md "description" >>= text
          pure $ FNetModelLevelMeta name desc


    varToPort vars uid =
      do  var <- findUid vars uid
          tyValue <- objLookup var "type" >>= text
          let mbDesc = try $ do metas <- objLookup var "metadata" >>= arr
                                find metaDesc metas

          let metaMap = Map.fromList (maybeToList mbDesc)

          ty <-
            case MB.parseValueType tyValue of
              Nothing -> Left ("could not parse value type '" <> tyValue <> "'")
              Just v -> Right v

          portUid <- objLookup var "proxy_state" >>= text

          pure MI.Port { MI.portName = portUid
                       , MI.portValueType = ty
                       , MI.portDefault = Nothing
                       , MI.portMeta = metaMap
                       }

    text (JSON.String t) = Right t
    text _ = Left "expecting string"

    obj (JSON.Object o) = Right o
    obj _ = Left "expecting object"

    metaDesc o =
      do  desc <- objLookup o "variable_definition"
          descText <- text desc
          pure ("Description", [descText])

    find f as  =
      case as of
        [] -> Left ("could not find required value" :: Text)
        a:as' ->
          case try (f a) of
            Nothing -> find f as'
            Just b -> Right b

    findUid a uid =
      do  elts <- arr a
          let mbVal =  listToMaybe [ e | e <- elts
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


fnetUID :: FunctionNetwork -> Either String Text
fnetUID fnet =
  case fnet of
    JSON.Object o -> 
      case HashMap.lookup "uid" o of
        Just (JSON.String t) -> Right t
        Just _ -> Left "FNet gromet's UID wasn't a string"
        Nothing -> Left "FNet gromet didn't contain UID at key \"uid\" - this gromet is ill-formed"
    _ -> Left "internal error: FNet gromet represented as something other than a JSON object"