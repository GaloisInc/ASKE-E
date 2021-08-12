{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import qualified Control.Exception as X
import Control.Applicative ((<**>))
import Control.Monad (replicateM_, when)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Strict (MonadState(..), StateT(..), evalStateT, gets, modify)
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import qualified Data.List.Extra as L
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Text.IO as T
import qualified Options.Applicative as Opt
import System.Console.Haskeline
import System.Directory
import System.FilePath

import Language.ASKEE.Exposure.Python (withPythonHandle, PythonHandle)
import Language.ASKEE.Exposure.GenLexer (lexExposure)
import Language.ASKEE.Exposure.GenParser (parseExposureStmt, parseExposureStmts)
import qualified Language.ASKEE.Exposure.Interpreter as Exposure
import qualified Language.ASKEE.Exposure.Print as Exposure
import qualified Language.ASKEE.Exposure.Syntax as Exposure

import Logo (displayLogo)
import Data.String (fromString)
import System.Directory.Internal.Prelude (isDoesNotExistError)
import Paths_aske_e

main :: IO ()
main = do
  opts <- Opt.execParser champInfo
  champ opts
  where
    champInfo :: Opt.ParserInfo Options
    champInfo = Opt.info (optionsParser <**> Opt.helper)
      ( Opt.fullDesc
     <> Opt.progDesc spiel
     <> Opt.header spiel )

    spiel = "A simple Exposure REPL"

champ :: Options -> IO ()
champ opts = do
  champConfigDir <- getXdgDirectory XdgConfig "champ"
  createDirectoryIfMissing True champConfigDir

  interpProg <- getDataFileName ("exposure" </> "pyinterp" </> "pyinterp.py")
  ourExts    <- getDataFileName ("exposure" </> "extensions")
  let extDirs = optPyExts opts
             ++ [ourExts]

  let launchREPL =
        runInputT defaultSettings
                    {historyFile = Just $ champConfigDir </> "history" }
        $ withInterrupt loop

  withPythonHandle interpProg extDirs $ \hdl ->
    case optBatchFile opts of
      Just batchFile ->
        evalChampT initialState { champPyHandle = Just hdl } $ do
          loadAndBatchProgramCmd batchFile
          executeBatchedStmts
          when (optInteractive opts) launchREPL

      Nothing -> do
        displayLogo (optColor opts) (optUnicode opts)
        evalChampT initialState { champPyHandle = Just hdl } launchREPL

data Options = Options
  { optColor   :: Bool
  , optUnicode :: Bool
  , optInteractive :: Bool
  , optBatchFile :: Maybe FilePath
  , optPyExts  :: [FilePath]
  } deriving Show

optionsParser :: Opt.Parser Options
optionsParser = Options
  <$> (fmap not . Opt.switch)
      (  Opt.long "no-color"
      <> Opt.help "Print the champ logo without color" )
  <*> (fmap not . Opt.switch)
      (  Opt.long "no-unicode"
      <> Opt.help "Print the champ logo without Unicode" )
  <*> Opt.switch
      (  Opt.long "interactive"
      <> Opt.short 'i'
      <> Opt.help "Run an interactive session after running an exposure file" )
  <*> (Opt.optional . Opt.strArgument)
        (  Opt.metavar "FILE"
        <> Opt.help "Run the exposure program provided and exit" )
  <*> Opt.many
      ( Opt.strOption
         (  Opt.long "py-dir"
         <> Opt.help "Directory containing Exposure extension modules"
         <> Opt.metavar "DIRECTORY" ))

data Input
  = FailedInput
  | CaughtCtrlC
  | SuccessfulInput String

data ChampState = ChampState
  { champEnv          :: Exposure.Env
  , champBatchedStmts :: Seq Exposure.Stmt
  , champMultiline    :: Maybe (Seq String)
  , champPyHandle     :: Maybe PythonHandle
    -- ^ 'Nothing' means multiline mode is not active.
    -- @'Just' lines@ means multiline mode is active, with @lines@ being the
    -- lines of code that have been entered thus far.
  }

initialState :: ChampState
initialState = ChampState
  { champEnv          = Exposure.initialEnv
  , champBatchedStmts = Seq.empty
  , champMultiline    = Nothing
  , champPyHandle     = Nothing
  }

putEnv :: Exposure.Env -> ChampM ()
putEnv env = modify $ \s -> s{champEnv = env}

batchStmt :: Exposure.Stmt -> ChampM ()
batchStmt stmt = modify $ \s -> s{champBatchedStmts = champBatchedStmts s Seq.|> stmt}

clearBatchedStmts :: ChampM ()
clearBatchedStmts = modify $ \s -> s{champBatchedStmts = Seq.empty}

appendLine :: String -> ChampM ()
appendLine line = modify $ \s ->
  s{ champMultiline =
       case champMultiline s of
         Nothing    -> fail "INVARIANT VIOLATED: Appending line without being in multiline mode!"
         Just linez -> Just $ linez Seq.|> line
   }

startMultiline :: ChampM ()
startMultiline = modify $ \s -> s{champMultiline = Just Seq.empty}

stopMultiline :: ChampM ()
stopMultiline = do
  linez <- gets $ \s ->
    case champMultiline s of
      Nothing    -> error "INVARIANT VIOLATED: Appending line without being in multiline mode!"
      Just linez -> L.intercalate " " $ toList linez
  modify $ \s ->
    s{ champBatchedStmts = Seq.empty
     , champMultiline    = Nothing
     }
  batchExposureStmtNoMultiline linez

newtype ChampM a = ChampM { unChampM :: StateT ChampState IO a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadIO, MonadThrow, MonadCatch, MonadMask, MonadState ChampState
#if !(MIN_VERSION_haskeline(0,8,0))
                   , MonadException
#endif
                   )

evalChampT :: ChampState -> ChampM a -> IO a
evalChampT st c = evalStateT (unChampM c) st

loop :: InputT ChampM ()
loop = do
  multiline <- lift $ gets champMultiline
  let prompt = if isJust multiline then "  ...> " else "champ> "
  mbInput <- handleInterrupt handleCtrlC (toInput <$> getInputLine prompt)
  case mbInput of
    FailedInput -> pure ()
    CaughtCtrlC -> loop
    SuccessfulInput input -> do
      res <- lift $ try $ runCommand $ L.trim input
      case res of
        Left (exc :: SomeException) -> do
          liftIO $ putStrLn $ "Caught exception: " ++ show exc
          lift clearBatchedStmts
          loop
        Right keepGoing -> when keepGoing loop
  where
    handleCtrlC :: InputT ChampM Input
    handleCtrlC = liftIO $ do
      liftIO $ putStrLn "Ctrl-C"
      pure CaughtCtrlC

    toInput :: Maybe String -> Input
    toInput Nothing  = FailedInput
    toInput (Just s) = SuccessfulInput s

runCommand :: String -> ChampM Bool
runCommand command =
  case words command of
    [""]  -> pure True
    [":"] -> pure True
    builtinCmd@(':':builtinCmdName) : rest -> do
      let possibleBuiltins = filter (\c -> any (builtinCmdName `L.isPrefixOf`)
                                               (commandNames c))
                                    builtins
      case possibleBuiltins of
        []        -> do liftIO $ putStrLn $ "Unknown command: " ++ builtinCmd
                        pure True
        [builtin] -> execCommand builtin rest
        (_:_)     -> do liftIO $ putStrLn $ unlines
                          [ builtinCmd ++ " is ambiguous, it could mean one of:"
                          , "\t" ++ L.intercalate ", "
                                    (map (':':) (concatMap commandNames possibleBuiltins))
                          ]
                        pure True
    _ -> do
      batchExposureStmt command
      pure True

data Command = Command
  { commandNames  :: [String]
  , commandHelp   :: String
  , commandAction :: CommandAction
  }

data CommandAction =
    NoArg (ChampM Bool)
  | StringArg (String -> ChampM Bool)

execCommand :: Command -> [String] -> ChampM Bool
execCommand cmd args =
  case (commandAction cmd, args) of
    (NoArg act, [])  -> act
    (StringArg act, [s]) -> act s
    _ -> do liftIO $ putStrLn cmdErr
            return True
  where
    cmdErr | NoArg _ <- commandAction cmd
           = "Command does not take any arguments"
           | otherwise
           = "Command takes exactly one argument"

builtins :: [Command]
builtins =
  [ Command ["?", "help"]
            "Display brief descriptions of each command."
            (NoArg $ keepGoing helpCmd)
  , Command ["q", "quit"]
            "Exit the REPL."
            (NoArg $ stop quitCmd)
  , Command ["l", "load"]
            "Load and batch a file."
            (StringArg (keepGoing . loadAndBatchProgramCmd))
  , Command ["e", "exec"]
            "Execute the batched statements."
            (NoArg $ keepGoing executeBatchedStmts)
  , Command ["{", "startmulti"]
            "Start multiline mode."
            (NoArg $ keepGoing startMultiline)
  , Command ["}", "stopmulti"]
            "Stop multiline mode."
            (NoArg $ keepGoing stopMultiline)
  ]
  where
    keepGoing :: ChampM () -> ChampM Bool
    keepGoing action = action *> pure True

    stop :: ChampM () -> ChampM Bool
    stop action = action *> pure False

helpCmd :: ChampM ()
helpCmd = traverse_ (liftIO . showHelp) builtins
  where
    showHelp :: Command -> IO ()
    showHelp command = do
      putStr "  "
      putStr $ L.intercalate ", "
             $ map (':':)
             $ commandNames command
      replicateM_ (20 - sum (map length (commandNames command))) $ putStr " "
      putStrLn $ commandHelp command

quitCmd :: ChampM ()
quitCmd = pure ()

loadAndBatchProgramCmd :: FilePath -> ChampM ()
loadAndBatchProgramCmd f = do
  do mstmts <- liftIO $ X.catch (loadStatements f) handleIOExc
     case mstmts of
       Left err    -> liftIO $ putStrLn err
       Right stmts -> traverse_ batchStmt stmts
  where
    handleIOExc ex
      | isDoesNotExistError ex =
        pure $ Left $ "'" ++ f ++ "' does not exist."
      | otherwise =
        pure $ Left $ "Error loading '" ++ f ++ "'."

loadStatements :: FilePath -> IO (Either String [Exposure.Stmt])
loadStatements f = do
  code <- readFile f
  pure (lexExposure code >>= parseExposureStmts)

batchExposureStmt :: String -> ChampM ()
batchExposureStmt code = do
  multiline <- gets champMultiline
  if isJust multiline
    then appendLine code
    else batchExposureStmtNoMultiline code

batchExposureStmtNoMultiline :: String -> ChampM ()
batchExposureStmtNoMultiline code =
  case lexExposure code >>= parseExposureStmt of
    Left err   -> liftIO $ putStrLn err
    Right stmt -> batchStmt stmt

executeBatchedStmts :: ChampM ()
executeBatchedStmts = do
  env      <- gets champEnv
  stmts    <- gets champBatchedStmts
  pyHandle <- gets champPyHandle
  let er = Exposure.mkEvalReadEnv champReadFile champWriteFile pyHandle
  (res, env') <- liftIO $ Exposure.evalLoop er env (toList stmts)
  case res of
    Left err ->
      liftIO $ T.putStrLn err
    Right (dvs, _) ->
      traverse_ (liftIO . print . Exposure.ppValue . Exposure.unDisplayValue) dvs
  putEnv env'
  clearBatchedStmts

champWriteFile :: Exposure.EvalWriteFileFn
champWriteFile f d =
  do res <- try (LBS.writeFile f d)
     case res of
       Left (x :: SomeException) ->
         pure $ Left (fromString $ displayException x)
       Right t -> pure $ Right t

champReadFile :: Exposure.EvalReadFileFn
champReadFile f =
  do res <- try (LBS.readFile f)
     case res of
       Left (x :: SomeException) ->
         pure $ Left (fromString $ displayException x)
       Right t -> pure $ Right t
