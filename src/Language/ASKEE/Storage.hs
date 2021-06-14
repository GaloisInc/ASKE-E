{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Storage 
  ( initStorage
  , loadModel
  , storeModel
  , listAllModels 

  , DataSource(..)
  , ModelDef(..)
  ) where

import           Data.Text ( Text )

import           Language.ASKEE.ModelType ( ModelType(..) )
import           Language.ASKEE.Storage.Internal ( DataSource(..), ModelDef(..) )
import qualified Language.ASKEE.Storage.Internal as Storage

initStorage :: IO ()
initStorage = Storage.initStorage baseDirectory

loadModel :: ModelType -> DataSource -> IO String
loadModel = Storage.loadModel baseDirectory

storeModel :: Text -> ModelType -> (Text -> IO ()) -> Text -> IO FilePath
storeModel = Storage.storeModel baseDirectory

listAllModels :: IO [ModelDef]
listAllModels = Storage.listAllModels baseDirectory

baseDirectory :: FilePath
baseDirectory = "modelRepo"