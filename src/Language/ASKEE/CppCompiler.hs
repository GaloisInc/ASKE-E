{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Language.ASKEE.CppCompiler where

import Data.Foldable(traverse_)
import qualified Language.ASKEE.C as C
import qualified System.Exit as Exit
import qualified System.Process as Process
import qualified System.IO.Temp as Temp
import qualified System.FilePath as Path
import Language.ASKEE.Panic(panic)

--
-- Simple interface to GCC/Clang
--

data Compiler = GCC | Clang

compilerCmd :: Compiler -> String
compilerCmd c =
  case c of
    GCC -> "g++"
    Clang -> "clang"

compileExe :: Compiler -> [FilePath] -> FilePath -> IO ()
compileExe compiler src dst =
  do  ec <- Process.rawSystem (compilerCmd compiler)
                              (src ++ ["-o", dst ])
      case ec of
        Exit.ExitSuccess   -> pure ()
        Exit.ExitFailure _ -> panic "compileExe" ["Compilation failed"]


compileAndRun :: Compiler -> [(FilePath, C.Doc)] -> IO String
compileAndRun compiler srcs =
  Temp.withSystemTempDirectory "ASKEE-CPP"
    \dir ->
      do  writeSource dir `traverse_` srcs
          let files = path dir . fst <$> srcs
              exe = path dir "__out"
          compileExe compiler files exe
          (ec, stdout, stderr) <- Process.readProcessWithExitCode exe [] []
          case ec of
            Exit.ExitSuccess -> pure stdout
            Exit.ExitFailure rv -> panic "compileAndRun" ["Error while running code: " <> show rv
                                                         , stderr]
  where
    path dir src = Path.joinPath [dir, src]
    writeSource dir (p, src) =
      writeFile (path dir p) (show src)
