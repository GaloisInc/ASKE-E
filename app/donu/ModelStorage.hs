{-# LANGUAGE OverloadedStrings #-}
module ModelStorage where --( initStorage, loadModel, storeModel, listAllModels ) where

-- import Control.Monad     ( when )
-- import Control.Exception ( Exception, throwIO )

-- import qualified Data.Text as Text
-- import           Data.Text ( Text, isInfixOf )

-- import Language.ASKEE ( DataSource(..) )
-- import Language.ASKEE.Types ( ModelType(..), Representation(..) )

-- import Schema ( ModelDef(..) )

-- import qualified System.Directory as Directory
-- import           System.FilePath  ( (</>), pathSeparator )


-- newtype StorageError = StorageError String
--   deriving Show

-- instance Exception StorageError

-- die :: String -> IO ()
-- die = throwIO . StorageError

-- initStorage :: IO ()
-- initStorage = mapM_ make dirs
--   where
--     make = Directory.createDirectoryIfMissing True
--     dirs = 
--       [ baseDirectory </> formatLocation mt
--       | mt <- [ ESL Concrete
--               , DEQ Concrete
--               , RNET Concrete
--               , LATEX Concrete
--               , GROMET Concrete
--               , TOPO Concrete
--               ]
--       ]

-- loadModel :: String -> ModelType -> IO String
-- loadModel = undefined

-- storeModel :: Text -> ModelType -> Text -> IO FilePath
-- storeModel name format model =
--   do  let path = baseDirectory </> formatLocation format </> Text.unpack name
--       when (".." `isInfixOf` name || Text.singleton pathSeparator `isInfixOf` name) 
--         (die $ "invalid name for model: " <> name')
--       exists <- Directory.doesFileExist path
--       when exists 
--         (die $ "model " <> name' <> " already exists")
--       writeFile path (Text.unpack model)
--       pure path
--   where
--     name' = Text.unpack name

-- listAllModels :: IO [ModelDef]
-- listAllModels = 
--   concat <$> sequence
--     [ listModels mt 
--     | mt <- [ ESL Concrete
--             , DEQ Concrete
--             , RNET Concrete
--             , LATEX Concrete
--             , GROMET Concrete
--             , TOPO Concrete
--             ]
--     ]

-- listModels :: ModelType -> IO [ModelDef]
-- listModels mt =
--   do  files <- Directory.listDirectory (baseDirectory </> loc)
--       pure $ map mdef files
--   where
--     mdef f = ModelDef (FromFile (baseDirectory </> loc </> f)) mt
--     loc = formatLocation mt

-- formatLocation :: ModelType -> FilePath
-- formatLocation mt =
--   case mt of
--     ESL    _ -> "easel"
--     RNET   _ -> "rnet"
--     DEQ    _ -> "deq"
--     LATEX  _ -> "latex"
--     GROMET _ -> "gromet"
--     TOPO   _ -> "topology"

-- baseDirectory :: FilePath
-- baseDirectory = "modelRepo"