{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Exposure.Print where

import qualified Data.Map as Map
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8)
import Language.ASKEE.Core.Print (ppModel)
import Language.ASKEE.DataSeries (dataSeriesAsCSV)
import Language.ASKEE.Exposure.Syntax
import Prettyprinter

ppValue :: Value -> Doc ()
ppValue v = case v of
  VDouble d            -> pretty d
  VInt i               -> pretty i
  VBool b              -> if b then "true" else "false"
  VString s            -> viaShow s
  VModel m             -> ppModel m
  VArray vs            -> align $ list $ map ppValue vs
  VTimed v' t          -> ppValue v' <> "@time" <> pretty t
  VDataSeries ds       -> pretty $ TL.decodeUtf8 $ dataSeriesAsCSV ds
  VModelExpr e         -> ppModelExpr e
  VPoint m             -> group $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", " $
                          map (\(key, val) -> tupled [viaShow key, ppValue val]) $ Map.toList m
  VDFold{}             -> notImplemented "VDFold"
  VSFold{}             -> notImplemented "VSFold"
  VSuspended           -> notImplemented "VSuspended"
  where
    notImplemented :: String -> a
    notImplemented what = error ("ppValue: not implemented: " <> what)

    ppModelExpr :: Expr -> Doc ()
    ppModelExpr e = case e of
      EVal v'       -> ppValue v'
      _             -> notImplemented "Exotic VModelExpr"