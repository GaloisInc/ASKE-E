{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Storage
  ( initStorage
  , loadModelText
  , storeModel
  , listAllModels

  , Storage.DataSource(..)
  , Storage.ModelDef(..)
  ) where

import           Data.Text ( Text )

import           Language.ASKEE.Model.Basics ( ModelType(..) )
import qualified Language.ASKEE.Storage.Internal as Storage


initStorage :: IO ()
initStorage = Storage.initStorage baseDirectory

loadModelText :: ModelType -> Storage.DataSource -> IO Text
loadModelText = Storage.loadModelText baseDirectory

storeModel :: Storage.ModelName -> ModelType -> Text -> IO ()
storeModel = Storage.storeModel baseDirectory

listAllModels :: IO [Storage.ModelDef]
listAllModels = Storage.listAllModels baseDirectory

baseDirectory :: FilePath
baseDirectory = "modelRepo"