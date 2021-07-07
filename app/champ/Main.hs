{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Applicative ((<**>))
import Control.Exception
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
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
  runInputT defaultSettings $ withInterrupt $ loop Exposure.initialEnv

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

loop :: Exposure.Env -> InputT IO ()
loop env = do
  mbInput <- handleInterrupt handleCtrlC (toInput <$> getInputLine "champ> ")
  case mbInput of
    FailedInput -> pure ()
    CaughtCtrlC -> loop env
    SuccessfulInput input -> do
      (env', keepGoing) <- liftIO $ runCommand env $ L.trim input
      when keepGoing $ loop env'
  where
    handleCtrlC :: InputT IO Input
    handleCtrlC = liftIO $ do
      putStrLn "Ctrl-C"
      pure CaughtCtrlC

    toInput :: Maybe String -> Input
    toInput Nothing  = FailedInput
    toInput (Just s) = SuccessfulInput s

runCommand :: Exposure.Env -> String -> IO (Exposure.Env, Bool)
runCommand env command =
  case command of
    ""  -> carryOn
    ":" -> carryOn
    builtinCmd@(':':builtinCmdName) -> do
      let possibleBuiltins = filter (\c -> any (builtinCmdName `L.isPrefixOf`)
                                               (commandNames c))
                                    builtins
      case possibleBuiltins of
        []        -> do putStrLn $ "Unknown command: " ++ builtinCmd
                        carryOn
        [builtin] -> (env,) <$> commandAction builtin
        (_:_)     -> do putStrLn $ unlines
                          [ builtinCmd ++ " is ambiguous, it could mean one of:"
                          , "\t" ++ L.intercalate ", "
                                    (map (':':) (concatMap commandNames possibleBuiltins))
                          ]
                        carryOn
    _ -> do
      env' <- exposureStmt env command
      pure (env', True)
  where
    carryOn = pure (env, True)

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
exposureStmt :: Exposure.Env -> String -> IO Exposure.Env
exposureStmt env code =
  case lexExposure code >>= parseExposureStmt of
    Left err   -> failure err
    Right stmt -> do
      res <- try $ Exposure.evalLoop env [stmt]
      case res of
        Left (exc :: SomeException)  -> failure (show exc)
        Right (Left err)             -> failure $ T.unpack err
        Right (Right (env', dvs, _)) -> do
          traverse_ (print . Exposure.ppValue . Exposure.unDisplayValue) dvs
          pure env'
  where
    failure err = putStrLn err *> pure env
