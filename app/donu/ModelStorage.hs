{-# LANGUAGE OverloadedStrings #-}
module ModelStorage ( loadModel, storeModel, listModels ) where

import Control.Monad     ( when, void )
import Control.Exception ( Exception, throwIO )

import qualified Data.Text as Text
import           Data.Text ( Text, isInfixOf )

import Language.ASKEE ( DataSource(..), parseEquations, parseModel, parseReactions, parseLatex )

import Schema ( ModelType(..), ModelDef(..) )

import qualified System.Directory as Directory
import           System.FilePath  ( (</>), pathSeparator )


newtype StorageError = StorageError String
  deriving Show

instance Exception StorageError

die :: String -> IO ()
die = throwIO . StorageError

loadModel :: String -> ModelType -> IO String
loadModel = undefined

storeModel :: Text -> ModelType -> Text -> IO ()
storeModel name format model =
  do  let path = baseDirectory </> formatLocation format </> Text.unpack name
      when (".." `isInfixOf` name || Text.singleton pathSeparator `isInfixOf` name) 
        (die "rude!")
      exists <- Directory.doesFileExist path
      when exists 
        (die "file exists")
      writeFile path (Text.unpack model)

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
    AskeeModel    -> "easel"
    ReactionNet   -> "rnet"
    DiffEqs       -> "deq"
    LatexEqnarray -> "latex"

baseDirectory :: FilePath
baseDirectory = "modelRepo"