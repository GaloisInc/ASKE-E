{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Exposure.Print where

import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)
import Language.ASKEE.Core.Print (ppModel)
import Language.ASKEE.DataSeries (dataSeriesAsCSV)
import Language.ASKEE.Exposure.Syntax
import Prettyprinter

ppValue :: Value -> Doc ()
ppValue v = case v of
  VDouble d            -> pretty d
  VBool b              -> if b then "true" else "false"
  VString s            -> viaShow s
  VModel m             -> ppModel m
  VDataSeries ds       -> pretty $ TL.decodeUtf8 $ dataSeriesAsCSV ds
  VModelExpr (EVal v') -> ppValue v'
  VTimed _ _           -> notImplemented "VTimed"
  VArray _             -> notImplemented "VArray"
  VPoint _             -> notImplemented "VPoint"
  VModelExpr{}         -> notImplemented "Exotic VModelExpr"
  VInt _               -> notImplemented "VInt"
  VDFold{}             -> notImplemented "VDFold"
  VSFold{}             -> notImplemented "VSFold"
  VSuspended           -> notImplemented "VSuspended"
  where
    notImplemented :: String -> a
    notImplemented what = error ("ppValue: not implemented: " <> what)
