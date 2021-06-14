{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage where

import Control.Exception ( try, SomeException )

import qualified Data.FileEmbed as Embed
import qualified Data.List as List
import           Data.Text      ( pack, Text )

import           Language.ASKEE.Error            ( die, ASKEEError(StorageError) )
import           Language.ASKEE.ModelType        ( allModelTypes, ModelType(..) )
import qualified Language.ASKEE.Storage.Internal as Storage

import System.Directory      ( doesDirectoryExist, doesFileExist )
import System.FilePath.Posix ( (</>) )
import System.IO.Temp        ( withSystemTempDirectory )

import qualified Test.Tasty       as Tasty
import           Test.Tasty.HUnit ( Assertion
                                  , assertBool
                                  , assertFailure
                                  , testCase )

sir :: Text
sir = $(Embed.embedStringFile "modelRepo/easel/sir.easel")

temp :: (FilePath -> IO a) -> IO a
temp = withSystemTempDirectory "foo"

checkPass :: Text -> IO ()
checkPass _ = pure ()

checkFail :: Text -> IO ()
checkFail _ = die (StorageError "this check always fails")

testInitStorage :: Assertion
testInitStorage = temp \dir ->
  do  Storage.initStorage dir
      existsCheck <- mapM (checkExists dir) mts
      assertBool ("A model directory didn't exist: "<>show existsCheck) (all snd existsCheck)
  where
    checkExists :: FilePath -> ModelType -> IO (ModelType, Bool)
    checkExists dir mt = 
      do  exists <- doesDirectoryExist (dir </> Storage.formatLocation mt)
          pure (mt, exists)
          
    mts = List.delete CoreType allModelTypes

testStoreModel :: Assertion
testStoreModel = temp \dir ->
  do  Storage.initStorage dir
      path <- Storage.storeModel dir "sir.easel" EaselType checkPass sir
      exists <- doesFileExist path
      assertBool ("Failed to find SIR model at path "<>path) exists

testStoreInvalidModel :: Assertion
testStoreInvalidModel = temp \dir ->
  do  Storage.initStorage dir
      path <- try $ Storage.storeModel dir "sir.easel" EaselType checkFail sir
      case path of
        Left (_ :: SomeException) -> pure ()
        Right _ -> assertFailure "Successfully stored a model whose check failed"

testStoreDotDotModel :: Assertion
testStoreDotDotModel = temp \dir ->
  do  Storage.initStorage dir
      path <- try $ Storage.storeModel dir "sir..easel" EaselType checkPass sir
      case path of
        Left (_ :: SomeException) -> pure ()
        Right _ -> assertFailure "Successfully stored an illegally-named model"

testStoreSlashModel :: Assertion
testStoreSlashModel = temp \dir ->
  do  Storage.initStorage dir
      path <- try $ Storage.storeModel dir "/sir.easel" EaselType checkPass sir
      case path of
        Left (_ :: SomeException) -> pure ()
        Right _ -> assertFailure "Successfully stored an illegally-named model"

testStorePreexistingModel :: Assertion
testStorePreexistingModel = temp \dir ->
  do  Storage.initStorage dir
      _     <-       Storage.storeModel dir "sir.easel" EaselType checkPass sir
      path2 <- try $ Storage.storeModel dir "sir.easel" EaselType checkPass sir
      case path2 of
        Left (_ :: SomeException) -> pure ()
        Right _ -> assertFailure "Successfully overwrote a model"

testLoadModel :: Assertion
testLoadModel = temp \dir ->
  do  Storage.initStorage dir
      path <- Storage.storeModel dir "sir.easel" EaselType checkPass sir
      path' <- try $ Storage.loadModel dir EaselType (Storage.FromFile path)
      sir' <- case path' of
        Left (err :: SomeException) -> 
          assertFailure ("Couldn't find SIR model just stored at "<>path<>": "<>show err)
        Right p -> pure p
      assertBool "Fetched model didn't match stored model" (sir == pack sir')
      
testLoadNonexistentModel :: Assertion
testLoadNonexistentModel = temp \dir ->
  do  Storage.initStorage dir
      path <- try $ Storage.loadModel dir EaselType (Storage.FromFile "foo")
      case path of
        Left (_ :: SomeException) -> pure ()
        Right _ -> assertFailure "Successfully loaded a nonexistent model"

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "ASKEE Storage Tests"
    [ testCase "Initialize storage" testInitStorage
    , testCase "Store model with passing check" testStoreModel
    , testCase "Store model with failing check" testStoreInvalidModel
    , testCase "Store model with \"/\" in name" testStoreDotDotModel
    , testCase "Store model with \"..\" in name" testStoreSlashModel
    , testCase "Store preexisting model" testStorePreexistingModel
    , testCase "Load model" testLoadModel
    , testCase "Load nonexistent model" testLoadNonexistentModel
    ]