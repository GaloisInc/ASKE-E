{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.DEQ.Print where

import Data.Map                  ( toList )
import Data.Text                 ( unpack )

import Language.ASKEE.Core.Print ( ppExpr, text, Doc )
import Language.ASKEE.DEQ.Syntax ( DiffEqs(..) )

import Prettyprinter ( hcat, hsep, vcat, parens, Pretty(pretty) )

printDiffEqs :: DiffEqs -> Doc
printDiffEqs DiffEqs{..} = vcat [params, lets, initial, rates]
  where
    params  = vcat $ map paramBinding (toList deqParams)
    lets    = vcat $ map (binding "let") (toList deqLets)
    initial = vcat $ map initBinding (toList deqInitial)
    rates   = vcat $ map (binding "d/dt") (toList deqRates)

    paramBinding (i,mbE) = hsep ["param", text (unpack i), foldMap (\e -> hsep ["=", ppExpr e]) mbE]
    binding decl (i,e) = hsep [decl, text (unpack i), "=", ppExpr e]
    initBinding (i,e) = hsep [hcat [text (unpack i),parens (pretty (0::Int))], "=", ppExpr e]
