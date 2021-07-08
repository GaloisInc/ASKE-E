{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Exposure.Print where

import Language.ASKEE.Core.Print (ppModel)
import Language.ASKEE.Exposure.Syntax
import Prettyprinter

ppValue :: Value -> Doc ()
ppValue v = case v of
  VDouble d    -> pretty d
  VBool b      -> if b then "true" else "false"
  VString s    -> viaShow s
  VModel m     -> ppModel m
  VInt         -> notImplemented "VInt"
  VModelExpr{} -> notImplemented "VModelExpr"
  VDFold{}     -> notImplemented "VDFold"
  VSFold{}     -> notImplemented "VSFold"
  VSuspended   -> notImplemented "VSuspended"
  where
    notImplemented :: String -> a
    notImplemented what = error ("ppValue: not implemented: " <> what)
