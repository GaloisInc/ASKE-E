{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Exposure.Pretty where

import Language.ASKEE.Exposure.Syntax
import Prettyprinter

ppValue :: Value -> Doc a
ppValue v = case v of
  VDouble d    -> pretty d
  VBool b      -> if b then "true" else "false"
  VString s    -> viaShow s
  VInt         -> notImplemented "VInt"
  VModel{}     -> notImplemented "VModel"
  VModelExpr{} -> notImplemented "VModelExpr"
  VDFold{}     -> notImplemented "VDFold"
  VSFold{}     -> notImplemented "VSFold"
  VSuspended   -> notImplemented "VSuspended"
  where
    notImplemented :: String -> a
    notImplemented what = error ("ppValue: not implemented: " <> what)
