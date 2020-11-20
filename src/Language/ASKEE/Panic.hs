{-# Language TemplateHaskell #-}
module Language.ASKEE.Panic(panic) where

import qualified Panic

data ASKEE = ASKEE

instance Panic.PanicComponent ASKEE where
  panicComponentName _ =  "ASKEE"
  panicComponentIssues _ =
    "https://gitlab-ext.galois.com/aske-e/aske-e/-/issues"
  panicComponentRevision = $(Panic.useGitRevision)

panic :: String -> [String] -> a
panic = Panic.panic ASKEE
