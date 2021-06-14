{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Storage.Internal where

import Control.Monad     ( when )

import qualified Data.Text as Text
import           Data.Text ( Text, isInfixOf )

import Language.ASKEE.Error ( die, ASKEEError(StorageError) )
import Language.ASKEE.ModelType ( ModelType(..), allModelTypes )
import Language.ASKEE.Panic ( panic )

import qualified System.Directory as Directory
import           System.FilePath  ( (</>), pathSeparator )
import qualified Data.List as List

data DataSource =
    FromFile FilePath
  | Inline Text
  deriving (Eq, Show)

data ModelDef =
    ModelDef { modelDefSource :: DataSource
             , modelDefType   :: ModelType
             }
  deriving (Eq, Show)

initStorage :: FilePath -> IO ()
initStorage baseDir = mapM_ make dirs
  where
    make = Directory.createDirectoryIfMissing True
    dirs = 
      [ baseDir </> formatLocation mt
      | mt <- List.delete CoreType allModelTypes
      ]

loadModel :: FilePath -> ModelType -> DataSource -> IO String
loadModel baseDir format source =
  case source of
    Inline t -> pure $ Text.unpack t
    _ ->
      do  models <- listModels baseDir format
          when (mdef `notElem` models)
            (die $ StorageError $ "model "++show source++" doesn't exist")
          (loadString . modelDefSource) mdef
  where
    mdef = ModelDef source format 

storeModel :: FilePath -> Text -> ModelType -> (Text -> IO ()) -> Text -> IO FilePath
storeModel baseDir name format check model =
  do  let path = baseDir </> formatLocation format </> Text.unpack name
      when (".." `isInfixOf` name || Text.singleton pathSeparator `isInfixOf` name) 
        (die $ StorageError $ "invalid name for model: " <> name')
      exists <- Directory.doesFileExist path
      when exists 
        (die $ StorageError $ "model " <> name' <> " already exists")
      check model
      writeFile path (Text.unpack model)
      pure path
  where
    name' = Text.unpack name

listAllModels :: FilePath -> IO [ModelDef]
listAllModels baseDir =
  concat <$> sequence
    [ listModels baseDir mt 
    | mt <- List.delete CoreType allModelTypes 
    ]

listModels :: FilePath -> ModelType -> IO [ModelDef]
listModels baseDir format =
  do  files <- Directory.listDirectory (baseDir </> loc)
      pure $ map mdef files
  where
    mdef f = ModelDef (FromFile (baseDir </> loc </> f)) format
    loc = formatLocation format

formatLocation :: ModelType -> FilePath
formatLocation mt =
  case mt of
    EaselType -> "easel"
    DeqType -> "deq"
    CoreType -> panic "formatLocation" ["attempted to generate a location for core models, which have no concrete syntax"]
    GrometPrtType -> "gromet-prt"
    GrometFnetType -> "gromet-fnet"
    GrometPrcType -> "gromet-prc"

loadString :: DataSource -> IO String
loadString source =
  case source of
    FromFile file -> readFile file
    Inline txt    -> pure (Text.unpack txt)