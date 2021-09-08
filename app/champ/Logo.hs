module Logo where

import Data.Foldable (traverse_)
import Data.Version (showVersion)
import System.Console.ANSI

import Paths_aske_e (version)

type Version = String

type Logo = [String]

logo :: Bool -> (String -> [String]) -> Logo
logo useColor mk =
     [ sgr [SetColor Foreground Dull  White] ++ l | l <- ws ]
  ++ [ sgr [SetColor Foreground Vivid Blue ] ++ l | l <- vs ]
  ++ [ sgr [SetColor Foreground Dull  Blue ] ++ l | l <- ds ]
  ++ [ sgr [Reset] ]
  where
  sgr | useColor  = setSGRCode
      | otherwise = const []
  versionText = "version " ++ showVersion version
  ver = sgr [SetColor Foreground Dull White]
        ++ replicate (lineLen - 20 - length versionText) ' '
        ++ versionText ++ "\n"
  ls        = mk ver
  slen      = length ls `div` 3
  (ws,rest) = splitAt slen ls
  (vs,ds)   = splitAt slen rest
  lineLen   = length (head ls)

displayLogo :: Bool -> Bool -> IO ()
displayLogo useColor useUnicode =
  traverse_ putStrLn $ logo useColor
                     $ if useUnicode then logo2 else logo1

logo1 :: String -> [String]
logo1 ver =
    [ " _____ _____ _____ _____ _____ "
    , "|     |  |  |  _  |     |  _  |"
    , "|   --|     |     | | | |   __|"
    , "|_____|__|__|__|__|_|_|_|__|   "
    , ver
    ]

logo2 :: String -> [String]
logo2 ver =
    [ "┏━╸╻ ╻┏━┓┏┳┓┏━┓"
    , "┃  ┣━┫┣━┫┃┃┃┣━┛"
    , "┗━╸╹ ╹╹ ╹╹╹╹╹  "
    , ver
    ]
