{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Exposure.Print where

import qualified Data.Map as Map
import Language.ASKEE.Core.Print (ppModel, text)
import Language.ASKEE.Exposure.Syntax
import Language.ASKEE.Latex.Print (printLatex)
import Prettyprinter

ppValue :: Value -> Doc ()
ppValue v = case v of
  VDouble d            -> pretty d
  VInt i               -> pretty i
  VBool b              -> if b then "true" else "false"
  VString s            -> viaShow s
  VModel m             -> ppModel m
  VLatex l             -> printLatex l
  VSampledData vs      -> align $ encloseSep "{ " " }" ", " $ map ppValue vs
  VArray vs            -> align $ list $ map ppValue vs
  VTimed v' t          -> ppValue v' <> "@time" <> pretty t
  VModelExpr e         -> ppModelExpr e
  VPoint m             -> group $ encloseSep (flatAlt "{ " "{") (flatAlt " }" "}") ", " $
                          map (\(key, val) -> tupled [viaShow key, ppValue val]) $ Map.toList m
  VPlot{}              -> text "<plot>"
  VScatter{}           -> text "<scatter-plot>"
  VTable{}             -> text "<table>"
  VDFold{}             -> notImplemented "VDFold"
  VSFold{}             -> notImplemented "VSFold"
  VSuspended           -> notImplemented "VSuspended"
  VHistogram lo _hi sz bins ->
    vsep [ pretty x <+> "-" <+> pretty y <> ": " <> pretty count
         | (bin, count) <- Map.toList bins
         , let x = (sz * fromIntegral bin) + lo :: Double
         , let y = x + sz :: Double
         ]
  where
    notImplemented :: String -> a
    notImplemented what = error ("ppValue: not implemented: " <> what)

    ppModelExpr :: Expr -> Doc ()
    ppModelExpr e = case e of
      EVal v'       -> ppValue v'
      _             -> notImplemented "Exotic VModelExpr"
