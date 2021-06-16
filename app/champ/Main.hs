module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable
import qualified Data.List.Extra as L
import qualified Options.Applicative as Opt
import System.Console.Haskeline

import Language.ASKEE.Exposure.GenLexer (lexExposure)
import Language.ASKEE.Exposure.GenParser (parseExposureStmt)
import qualified Language.ASKEE.Exposure.Interpreter as Exposure

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
  runInputT defaultSettings $ withInterrupt $ loop

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

loop :: InputT IO ()
loop = do
  mbInput <- handleInterrupt handleCtrlC (toInput <$> getInputLine "champ> ")
  case mbInput of
    FailedInput -> pure ()
    CaughtCtrlC -> loop
    SuccessfulInput input -> do
      keepGoing <- liftIO $ runCommand $ L.trim input
      when keepGoing loop
  where
    handleCtrlC :: InputT IO Input
    handleCtrlC = liftIO $ do
      putStrLn "Ctrl-C"
      pure CaughtCtrlC

    toInput :: Maybe String -> Input
    toInput Nothing  = FailedInput
    toInput (Just s) = SuccessfulInput s

runCommand :: String -> IO Bool
runCommand command =
  case command of
    ":" -> pure True
    builtinCmd@(':':builtinCmdName) -> do
      let possibleBuiltins = filter (\c -> any (builtinCmdName `L.isPrefixOf`)
                                               (commandNames c))
                                    builtins
      case possibleBuiltins of
        []        -> do putStrLn $ "Unknown command: " ++ builtinCmd
                        pure True
        [builtin] -> commandAction builtin
        (_:_)     -> do putStrLn $ unlines
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
exposureStmt :: String -> IO ()
exposureStmt code =
  case lexExposure code >>= parseExposureStmt of
    Left err   -> putStrLn err
    Right stmt -> print $ Exposure.interpretStmt stmt
