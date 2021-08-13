-- |
{-# Language OverloadedStrings #-}
{-# Language OverloadedLists #-}
module Language.ASKEE.Exposure.Python (
    withPythonHandle
  , evaluate
  , PythonHandle
  , PythonResult(..)
  ) where

import           Control.Monad.IO.Class
import qualified Data.Aeson as JS
import           Data.ByteString.Lazy (hPutStr)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           Data.Text (Text)
import           GHC.Exts (toList)
import           System.IO (Handle, hFlush, hGetLine)
import           System.Process
import           System.FilePath ((</>))

import           Language.ASKEE.Exposure.Syntax
import           Paths_aske_e
import System.Directory (getDirectoryContents)
import Control.Monad (forM)

-- | The result of evaluating an external function call
data PythonResult
  = Success Value
  | Failure Text
  deriving (Eq, Show, Ord)

data PythonHandle = PythonHandle
  { pyStdout :: Handle
  , pyStdin  :: Handle
  }

-- | Starts a new interpreter. This can be used by clients to persist an
-- interpreter across the lifetime of the client, which may be longer than a
-- single interaction with the Exposure interpreter.
withPythonHandle ::
  MonadIO m =>
  [FilePath] -> (PythonHandle -> m a) -> m a
withPythonHandle pluginpaths act =
  do
     d <- liftIO $ getDataDir
     contents <- liftIO $ getDirectoryContents d
     liftIO $ forM contents $ \c ->
       putStrLn c
     contents <- liftIO $ getDirectoryContents (d </> "exposure")
     liftIO $ forM contents $ \c ->
       putStrLn c
     contents <- liftIO $ getDirectoryContents (d </> "exposure" </> "pyinterp")
     liftIO $ forM contents $ \c ->
       putStrLn c

     driver  <- liftIO $ getDataFileName ("exposure" </> "pyinterp" </> "pyinterp.py")
     ourExts <- liftIO $ getDataFileName ("exposure" </> "extensions")
     hdl <- liftIO $
              createProcess
                (proc "python3" (driver : pluginpaths ++ [ourExts]))
                  { std_out = CreatePipe
                  , std_in  = CreatePipe
                  }
     case hdl of
       (Just stdin, Just stdout, _, ph) ->
         do res <- act $ PythonHandle stdout stdin
            liftIO $ terminateProcess ph
            return res
       _ ->
         error "Error starting Python interpreter"

-- | This is the entrypoint that marshalls the Values to the python interpreter
evaluate :: PythonHandle -> Text -> [Value] -> IO PythonResult
evaluate hdl f args =
  do hPutStr (pyStdin hdl) cmd
     hPutStr (pyStdin hdl) "\n"
     hFlush  (pyStdin hdl)
     response <- hGetLine (pyStdout hdl)
     case JS.decode (fromString response) of
       Just r -> pure r
       Nothing -> error "Error parsing response from Python call"
  where
    cmd =
      JS.encode $
        JS.object [ "function" JS..= JS.toJSON f
                  , "args"     JS..= JS.toJSON (encodePython <$> args)
                  ]

encodePython :: Value -> JS.Value
encodePython v =
  case v of
    VInt i     -> JS.toJSON i
    VDouble d  -> JS.toJSON d
    VArray arr -> JS.toJSON $ fmap encodePython arr
    VPoint pt  -> JS.object [ k JS..= encodePython v' | (k,v') <- Map.toList pt ]
    VString s  -> JS.toJSON s

    _          ->
      error $ "Unimplemented encoding from Exposure to Python for: " ++ show v

newtype PythonValue = PythonValue { unPython :: Value }

instance JS.FromJSON PythonResult where
  parseJSON (JS.Object v) =
    do ty <- v JS..: "type"
       case ty :: String of
         "success" ->
           Success . unPython <$> v JS..: "value"
         "failure" ->
           Failure <$> v JS..: "message"
         _ ->
           error ("Unexpected JSON response type: '" ++ ty ++ "'")
  parseJSON v =
    error (show v)

instance JS.FromJSON PythonValue where
  parseJSON (JS.Number sci) =
    pure . PythonValue . VDouble . fromRational . toRational $ sci

  parseJSON (JS.Array arr) =
    do arr' <- traverse (fmap unPython . JS.parseJSON) arr
       pure . PythonValue . VArray . toList $ arr'

  parseJSON (JS.Object o) =
    do o' <- traverse (fmap unPython . JS.parseJSON) o
       pure . PythonValue . VPoint $ Map.fromList (HM.toList o')

  parseJSON (JS.String t) =
    pure . PythonValue . VString $ t

  parseJSON (JS.Bool b) =
    pure . PythonValue . VBool $ b

  parseJSON JS.Null =
    error "Python returned 'null'"
