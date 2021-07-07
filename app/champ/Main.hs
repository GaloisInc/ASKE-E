{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative ((<**>))
import Control.Exception
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..), StateT(..), evalStateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Foldable
import qualified Data.List.Extra as L
import qualified Data.Text as T
import qualified Options.Applicative as Opt
import System.Console.Haskeline

import Language.ASKEE.Exposure.GenLexer (lexExposure)
import Language.ASKEE.Exposure.GenParser (parseExposureStmt)
import qualified Language.ASKEE.Exposure.Interpreter as Exposure
import qualified Language.ASKEE.Exposure.Pretty as Exposure
import qualified Language.ASKEE.Exposure.Syntax as Exposure

import Logo (displayLogo)

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

    spiel = "A simple REPL"

champ :: Options -> IO ()
champ opts = do
  displayLogo (optColor opts) (optUnicode opts)
  evalChampT Exposure.initialEnv
    $ runInputT defaultSettings
    $ withInterrupt loop

data Options = Options
  { optColor   :: Bool
  , optUnicode :: Bool
  } deriving Show

optionsParser :: Opt.Parser Options
optionsParser = Options
  <$> (fmap not . Opt.switch)
      (  Opt.long "no-color"
      <> Opt.help "Print the champ logo without color" )
  <*> (fmap not . Opt.switch)
      (  Opt.long "no-unicode"
      <> Opt.help "Print the champ logo without Unicode" )

data Input
  = FailedInput
  | CaughtCtrlC
  | SuccessfulInput String

newtype ChampM a = ChampM { unChampM :: StateT Exposure.Env IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadState Exposure.Env)

evalChampT :: Exposure.Env -> ChampM a -> IO a
evalChampT env c = evalStateT (unChampM c) env

loop :: InputT ChampM ()
loop = do
  mbInput <- handleInterrupt handleCtrlC (toInput <$> getInputLine "champ> ")
  case mbInput of
    FailedInput -> pure ()
    CaughtCtrlC -> loop
    SuccessfulInput input -> do
      keepGoing <- lift $ runCommand $ L.trim input
      when keepGoing loop
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
  case command of
    ""  -> pure True
    ":" -> pure True
    builtinCmd@(':':builtinCmdName) -> do
      let possibleBuiltins = filter (\c -> any (builtinCmdName `L.isPrefixOf`)
                                               (commandNames c))
                                    builtins
      case possibleBuiltins of
        []        -> do liftIO $ putStrLn $ "Unknown command: " ++ builtinCmd
                        pure True
        [builtin] -> liftIO $ commandAction builtin
        (_:_)     -> do liftIO $ putStrLn $ unlines
                          [ builtinCmd ++ " is ambiguous, it could mean one of:"
                          , "\t" ++ L.intercalate ", "
                                    (map (':':) (concatMap commandNames possibleBuiltins))
                          ]
                        pure True
    _ -> do
      exposureStmt command
      pure True

data Command = Command
  { commandNames  :: [String]
  , commandHelp   :: String
  , commandAction :: IO Bool
  }

builtins :: [Command]
builtins =
  [ Command ["?", "help"]
            "Display brief descriptions of each command."
            (keepGoing helpCmd)
  , Command ["q", "quit"]
            "Exit the REPL."
            (stop quitCmd)
  ]
  where
    keepGoing :: IO () -> IO Bool
    keepGoing action = action *> pure True

    stop :: IO () -> IO Bool
    stop action = action *> pure False

helpCmd :: IO ()
helpCmd = traverse_ showHelp builtins
  where
    showHelp :: Command -> IO ()
    showHelp command = do
      putStr "  "
      putStr $ L.intercalate ", "
             $ map (':':)
             $ commandNames command
      putStr "\t"
      putStrLn $ commandHelp command

quitCmd :: IO ()
quitCmd = pure ()

-- TODO: Make this smarter
exposureStmt :: String -> ChampM ()
exposureStmt code =
  case lexExposure code >>= parseExposureStmt of
    Left err   -> failure err
    Right stmt -> do
      env <- get
      res <- liftIO $ try $ Exposure.evalLoop env [stmt]
      case res of
        Left (exc :: SomeException)  -> failure (show exc)
        Right (Left err)             -> failure $ T.unpack err
        Right (Right (env', dvs, _)) -> do
          traverse_ (liftIO . print . Exposure.ppValue . Exposure.unDisplayValue) dvs
          put env'
  where
    failure = liftIO . putStrLn
