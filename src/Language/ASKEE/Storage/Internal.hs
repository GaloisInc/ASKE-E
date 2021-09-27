{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Storage.Internal where

import Control.Monad     ( when, unless, void, forM )

import           Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Encoding

import Language.ASKEE.Error ( die, ASKEEError(StorageError) )
import Language.ASKEE.Model.Basics ( ModelType(..), allModelTypes )
import qualified Language.ASKEE.ESL as ESL
import Language.ASKEE.Gromet(grometText,convertCoreToGromet)

import qualified System.Directory as Directory
import           System.FilePath  ( (</>), pathSeparator )
import qualified Data.Aeson as JSON
import qualified Language.ASKEE.DataSet as DSet
import qualified Data.ByteString.Lazy.Char8 as BS

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

data DataSetDescription = DataSetDescription
  { dataSetDescSource      :: DataSource
  , dataSetDescName        :: Text
  , dataSetDescDescription :: Maybe Text
  }

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
           case (yes,format) of

             -- Special case to auto-convert ESL to PRT Gromet
             (False,GrometPrtType) ->
                do txt <- loadModelText baseDirectory EaselType source
                   case eslAsGromet txt of
                     Right a -> pure a
                     Left err -> die (StorageError err)
             (False,_) -> bad
             _ -> Text.readFile path
      where
      bad = die (StorageError ("model "++ show name ++ " doesn't exist"))

eslAsGromet :: Text -> Either String Text
eslAsGromet txt =
  do m <- ESL.parseESL txt
     pure (grometText (convertCoreToGromet (ESL.modelAsCore m)))

-- | Is this model in the database
-- This check if the actual file is in the databse, and does consider
-- the "virtual" gromets
doesModelExist :: FilePath -> ModelType -> DataSource -> IO Bool
doesModelExist baseDirectory format source =
  case source of
    Inline _ -> pure True
    FromFile file -> Directory.doesFileExist file
    FromStore name
      | not (validModelName name) -> pure False
      | otherwise ->
        Directory.doesFileExist (modelLocation baseDirectory format name)


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
listAllModels baseDirectory =
  concat <$> sequence [ listModels baseDirectory mt | mt <- allModelTypes ]

listModels :: FilePath -> ModelType -> IO [ModelDef]
listModels baseDirectory mt =
  do files0 <- Directory.listDirectory (modelTypeLocation baseDirectory mt)
     files <- case mt of
                GrometPrtType ->
                  do extra0 <- Directory.listDirectory
                                    (modelTypeLocation baseDirectory EaselType)
                     let extra = [ f | f <- extra0, not (f `elem` files0) ]
                     pure (files0 ++ extra)
                _ -> pure files0
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
    SBMLType -> "sbml"

-------------------------------------------------------------------------------

initDataStorage :: FilePath -> IO ()
initDataStorage = Directory.createDirectoryIfMissing True

listDataSets :: FilePath -> IO [DataSetDescription]
listDataSets path =
  do  files <- Directory.listDirectory path
      concat <$> (display `traverse` files)
  where
    display file =
      do  v <- JSON.eitherDecodeFileStrict (path </> file)
          case v of
            Left _ -> pure []
            Right ds ->
              pure
              [ DataSetDescription { dataSetDescSource = FromStore (Text.pack file)
                                   , dataSetDescName = DSet.dataSetName ds
                                   , dataSetDescDescription = DSet.dataSetDescription ds
                                   }
              ]

loadDataSet :: FilePath -> DataSource -> IO DSet.DataSet
loadDataSet baseDir source =
  case source of
    Inline t ->
      case JSON.eitherDecode' (BS.fromStrict $ Encoding.encodeUtf8 t) of
        Left err -> die (StorageError $ "Could not parse input as storage JSON: " <> err)
        Right ds -> pure ds
    FromFile f -> fromFile f
    FromStore n ->
      do  files <- Directory.listDirectory baseDir
          if Text.unpack n `elem` files
            then fromFile (baseDir </> Text.unpack n)
            else die (StorageError $ "Data set not found: " <> Text.unpack n)
  where
    fromFile f =
      do  ds <- JSON.eitherDecodeFileStrict' f
          case ds of
            Left err -> die (StorageError $ "Could not parse storage file: " <> err)
            Right ds' -> pure ds'

--------------------------------------------------------------------------------

initComparisonStorage :: FilePath -> IO ()
initComparisonStorage = Directory.createDirectoryIfMissing True

loadComparison :: FilePath -> DataSource -> IO Text
loadComparison baseDirectory source =
  case source of
    Inline t -> pure t
    FromFile f -> Text.readFile f
    FromStore n ->
      do  files <- Directory.listDirectory baseDirectory
          if Text.unpack n `elem` files
            then Text.readFile (baseDirectory </> Text.unpack n)
            else die (StorageError $ "Comparison not found: " <> Text.unpack n)

listComparisons :: FilePath -> IO [Text]
listComparisons baseDirectory =
  do  files <- Directory.listDirectory baseDirectory
      forM files $ loadComparison baseDirectory . FromStore . Text.pack