{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Language.ASKEE.CPP.Compile where

import Data.Foldable(traverse_)
import qualified Language.ASKEE.CPP.Pretty as C
import qualified System.Exit as Exit
import qualified System.Process as Process
import qualified System.IO.Temp as Temp
import qualified System.FilePath as Path
import Language.ASKEE.Panic(panic)
import Control.Monad (forM)

--
-- Simple interface to GCC/Clang
--

data Compiler = GCC | Clang

compilerCmd :: Compiler -> String
compilerCmd c =
  case c of
    GCC -> "g++"
    Clang -> "clang++"

compileExe :: Compiler -> [FilePath] -> FilePath -> IO ()
compileExe compiler src dst =
  do  ec <- Process.rawSystem (compilerCmd compiler)
                              (src ++ ["--std=c++11", "-o", dst ])
      case ec of
        Exit.ExitSuccess   -> pure ()
        Exit.ExitFailure _ -> panic "compileExe" ["Compilation failed"]


compileAndRun :: Compiler -> [(FilePath, C.Doc)] -> [String] -> IO String
compileAndRun compiler srcs args = head <$> compileAndRunN compiler srcs (Left args) 1

compileAndRunN :: 
  Compiler -> 
  [(FilePath, C.Doc)] -> 
  Either [String] [[String]] {- ^ Either one arglist or a list thereof, one for each iteration -} -> 
  Int -> 
  IO [String]
compileAndRunN compiler srcs args howMany =
  Temp.withSystemTempDirectory "ASKEE-CPP"
    \dir ->
      do  writeSource dir `traverse_` srcs
          let files = path dir . fst <$> srcs
              exe = path dir "__out"
          compileExe compiler files exe
          let args' = case args of
                        Left constant -> repeat constant
                        Right variable -> variable

          forM (zip args' [1 .. howMany]) \(argList, _) ->
            do  (ec, stdout, stderr) <- Process.readProcessWithExitCode exe argList ""
                case ec of
                  Exit.ExitSuccess -> pure stdout
                  Exit.ExitFailure rv -> panic "compileAndRun" ["Error while running code: " <> show rv
                                                               , stderr]
  where
    path dir src = Path.joinPath [dir, src]
    writeSource dir (p, src) =
      writeFile (path dir p) (show src)