{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Storage
  ( initStorage
  , loadModelText
  , storeModel
  , listAllModels

  , DataSource(..)
  , ModelDef(..)
  ) where

import Control.Monad     ( when )

import           Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Language.ASKEE.Error ( die, ASKEEError(StorageError) )
import Language.ASKEE.Model.Basics ( ModelType(..), allModelTypes )

import qualified System.Directory as Directory
import           System.FilePath  ( (</>), pathSeparator )


data DataSource =
    FromFile FilePath
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
    dirs =
      [ baseDirectory </> formatLocation mt
      | mt <- allModelTypes
      ]

loadModelText :: ModelType -> DataSource -> IO Text
loadModelText format source =
  case source of
    Inline t -> pure t
    FromFile {} ->
      do  models <- listModels format
          when (mdef `notElem` models)
            (die $ StorageError $ "model "++show source++" doesn't exist")
          loadText (modelDefSource mdef)
  where
    mdef = ModelDef source format

storeModel ::
  Text -> ModelType -> (Text -> IO ()) -> Text -> IO FilePath
storeModel name format check model =
  do  let path = baseDirectory </> formatLocation format </> Text.unpack name
          badName = ".." `Text.isInfixOf` name
                 || Text.singleton pathSeparator `Text.isInfixOf` name
      when badName (die (StorageError ("invalid name for model: " <> name')))
      exists <- Directory.doesFileExist path
      when exists (die (StorageError ("model " <> name' <> " already exists")))
      check model
      Text.writeFile path model
      pure path
  where
    name' = Text.unpack name

listAllModels :: IO [ModelDef]
listAllModels = concat <$> sequence [ listModels mt | mt <- allModelTypes ]

listModels :: ModelType -> IO [ModelDef]
listModels mt =
  do  files <- Directory.listDirectory (baseDirectory </> loc)
      pure $ map mdef files
  where
    mdef f = ModelDef (FromFile (baseDirectory </> loc </> f)) mt
    loc = formatLocation mt

formatLocation :: ModelType -> FilePath
formatLocation mt =
  case mt of
    EaselType -> "easel"
    DeqType -> "deq"
    CoreType -> "core"
    GrometPrtType -> "gromet-prt"
    GrometFnetType -> "gromet-fnet"
    GrometPrcType -> "gromet-prc"

baseDirectory :: FilePath
baseDirectory = "modelRepo"

loadText :: DataSource -> IO Text
loadText source =
  case source of
    FromFile file -> Text.readFile file
    Inline txt    -> pure txt
