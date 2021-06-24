{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Storage
  ( initStorage
  , loadModelText
  , storeModel
  , listAllModels

  , DataSource(..)
  , ModelDef(..)
  ) where

import Control.Monad     ( when, unless )

import           Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Language.ASKEE.Error ( die, ASKEEError(StorageError) )
import Language.ASKEE.Model.Basics ( ModelType(..), allModelTypes )

import qualified System.Directory as Directory
import           System.FilePath  ( (</>), pathSeparator )


type ModelName = Text

validModelName :: Text -> Bool
validModelName name = not bad
  where
  bad = or [ ".." `Text.isInfixOf` name
           , Text.singleton pathSeparator `Text.isInfixOf` name
           ]


data DataSource =
    FromFile FilePath
  | FromStore ModelName
  | Inline Text
  deriving (Eq, Show)

data ModelDef =
    ModelDef { modelDefSource :: DataSource
             , modelDefType   :: ModelType
             }
  deriving (Eq, Show)

initStorage :: IO ()
initStorage = mapM_ make dirs
  where
    make = Directory.createDirectoryIfMissing True
    dirs = map modelTypeLocation allModelTypes

loadModelText :: ModelType -> DataSource -> IO Text
loadModelText format source =
  case source of
    Inline t -> pure t
    FromFile file -> Text.readFile file
    FromStore name
      | not (validModelName name) -> bad
      | otherwise ->
        do let path = modelLocation format name
           yes <- Directory.doesFileExist path
           unless yes bad
           Text.readFile path
      where
      bad = die (StorageError ("model "++ show name ++ " doesn't exist"))


storeModel :: Text -> ModelType -> Text -> IO ()
storeModel name format model =
  do unless (validModelName name) (die (StorageError "Invalid model name"))
     let path = modelLocation format name
     exists <- Directory.doesFileExist path
     when exists
              (die (StorageError ("model " <> show name <> " already exists")))
     Text.writeFile path model

listAllModels :: IO [ModelDef]
listAllModels = concat <$> sequence [ listModels mt | mt <- allModelTypes ]

listModels :: ModelType -> IO [ModelDef]
listModels mt =
  do files <- Directory.listDirectory (modelTypeLocation mt)
     pure [ ModelDef { modelDefSource = FromStore f
                     , modelDefType   = mt
                     } | f <- map Text.pack files, validModelName f ]


--------------------------------------------------------------------------------

modelLocation :: ModelType -> ModelName -> FilePath
modelLocation ty nm = modelTypeLocation ty </> Text.unpack nm

modelTypeLocation :: ModelType -> FilePath
modelTypeLocation ty = baseDirectory </> formatLocation ty

formatLocation :: ModelType -> FilePath
formatLocation mt =
  case mt of
    EaselType -> "easel"
    DeqType -> "deq"
    CoreType -> "core"
    GrometPrtType -> "gromet-prt"
    GrometFnetType -> "gromet-fnet"
    GrometPncType -> "gromet-pnc"

baseDirectory :: FilePath
baseDirectory = "modelRepo"


