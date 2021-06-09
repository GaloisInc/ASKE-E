{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Storage 
  ( initStorage
  , loadModel
  , storeModel
  , listAllModels 

  , DataSource(..)
  , ModelDef(..)
  ) where

import Control.Monad     ( when )

import qualified Data.Text as Text
import           Data.Text ( Text, isInfixOf )

import Language.ASKEE.Error ( die, ASKEEError(StorageError) )
import Language.ASKEE.ModelType ( ModelType(..) )
import Language.ASKEE.Panic ( panic )

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
      | mt <- [EaselType, DeqType, GrometPrtType] 
      ]

loadModel :: ModelType -> DataSource -> IO String
loadModel format source =
  case source of
    Inline t -> pure $ Text.unpack t
    _ ->
      do  models <- listModels format
          when (mdef `notElem` models)
            (die $ StorageError $ "model "++show source++" doesn't exist")
          (loadString . modelDefSource) mdef
  where
    mdef = ModelDef source format 

storeModel :: Text -> ModelType -> (Text -> IO ()) -> Text -> IO FilePath
storeModel name format check model =
  do  let path = baseDirectory </> formatLocation format </> Text.unpack name
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

listAllModels :: IO [ModelDef]
listAllModels = 
  concat <$> sequence
    [ listModels mt 
    | mt <- [EaselType, DeqType, GrometPrtType] 
    ]

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
    EaselType -> "easel-meta"
    DeqType -> "deq"
    CoreType -> panic "formatLocation" ["attempted to generate a location for core models, which have no concrete syntax"]
    GrometPrtType -> "gromet-prt"

baseDirectory :: FilePath
baseDirectory = "modelRepo"

loadString :: DataSource -> IO String
loadString source =
  case source of
    FromFile file -> readFile file
    Inline txt    -> pure (Text.unpack txt)