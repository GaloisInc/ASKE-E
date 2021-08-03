{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Storage
  ( initStorage
  , loadModelText
  , storeModel
  , listAllModels
  , doesModelExist
  , initDataStorage
  , listDataSets
  , loadDataSet

  , Storage.DataSource(..)
  , Storage.ModelDef(..)
  , Storage.DataSetDescription(..)
  ) where

import           Data.Text ( Text )

import           Language.ASKEE.Model.Basics ( ModelType(..) )
import qualified Language.ASKEE.Storage.Internal as Storage
import qualified Language.ASKEE.DataSet as DataSet

initStorage :: IO ()
initStorage = Storage.initStorage modelBaseDirectory

loadModelText :: ModelType -> Storage.DataSource -> IO Text
loadModelText = Storage.loadModelText modelBaseDirectory

doesModelExist :: ModelType -> Storage.DataSource -> IO Bool
doesModelExist = Storage.doesModelExist modelBaseDirectory

storeModel :: Storage.ModelName -> ModelType -> Text -> IO ()
storeModel = Storage.storeModel modelBaseDirectory

listAllModels :: IO [Storage.ModelDef]
listAllModels = Storage.listAllModels modelBaseDirectory

modelBaseDirectory :: FilePath
modelBaseDirectory = "modelRepo"

listDataSets :: IO [Storage.DataSetDescription]
listDataSets = Storage.listDataSets dataBaseDirectory

-- TODO: the other kind of storage should be called "model storage"
--       (or they should be combined somehow)
initDataStorage :: IO ()
initDataStorage = Storage.initDataStorage dataBaseDirectory

loadDataSet :: Storage.DataSource -> IO DataSet.DataSet
loadDataSet = Storage.loadDataSet dataBaseDirectory

dataBaseDirectory :: FilePath
dataBaseDirectory = "dataRepo"

