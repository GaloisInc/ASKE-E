{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Storage.Internal where

import Control.Monad     ( when, unless, void )

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

initStorage :: FilePath -> IO ()
initStorage baseDirectory = mapM_ make dirs
  where
    make = Directory.createDirectoryIfMissing True
    dirs = map (modelTypeLocation baseDirectory) allModelTypes

loadModelText :: FilePath -> ModelType -> DataSource -> IO Text
loadModelText baseDirectory format source =
  case source of
    Inline t -> pure t
    FromFile file -> Text.readFile file
    FromStore name
      | not (validModelName name) -> bad
      | otherwise ->
        do let path = modelLocation baseDirectory format name
           yes <- Directory.doesFileExist path
           unless yes bad
           Text.readFile path
      where
      bad = die (StorageError ("model "++ show name ++ " doesn't exist"))

storeModel' :: FilePath -> ModelName -> ModelType -> Text -> IO FilePath
storeModel' baseDirectory name format model =
  do unless (validModelName name) (die (StorageError "Invalid model name"))
     let path = modelLocation baseDirectory format name
     exists <- Directory.doesFileExist path
     when exists
              (die (StorageError ("model " <> show name <> " already exists")))
     Text.writeFile path model
     pure path 

storeModel :: FilePath -> ModelName -> ModelType -> Text -> IO ()
storeModel baseDirectory name format model = 
  void $ storeModel' baseDirectory name format model

listAllModels :: FilePath -> IO [ModelDef]
listAllModels baseDirectory = concat <$> sequence [ listModels baseDirectory mt | mt <- allModelTypes ]

listModels :: FilePath -> ModelType -> IO [ModelDef]
listModels baseDirectory mt =
  do files <- Directory.listDirectory (modelTypeLocation baseDirectory mt)
     pure [ ModelDef { modelDefSource = FromStore f
                     , modelDefType   = mt
                     } | f <- map Text.pack files, validModelName f ]


--------------------------------------------------------------------------------

modelLocation :: FilePath -> ModelType -> ModelName -> FilePath
modelLocation baseDirectory ty nm = modelTypeLocation baseDirectory ty </> Text.unpack nm

modelTypeLocation :: FilePath -> ModelType -> FilePath
modelTypeLocation baseDirectory ty = baseDirectory </> formatLocation ty

formatLocation :: ModelType -> FilePath
formatLocation mt =
  case mt of
    EaselType -> "easel"
    DeqType -> "deq"
    CoreType -> "core"
    GrometPrtType -> "gromet-prt"
    GrometFnetType -> "gromet-fnet"
    GrometPncType -> "gromet-pnc"
    RNetType  -> "rnet"