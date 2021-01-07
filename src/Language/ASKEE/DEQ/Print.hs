{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.DEQ.Print where

import Data.Map                  ( toList )
import Data.Text                 ( unpack )

import Language.ASKEE.Core       ( ppExpr )
import Language.ASKEE.DEQ.Syntax ( DiffEqs(..) )

import Text.PrettyPrint          ( Doc, hsep, text, vcat )


ppDiffEqs :: DiffEqs -> Doc
ppDiffEqs DiffEqs{..} = vcat [lets, initial, rates]
  where
    lets    = vcat $ map (binding "let") (toList deqLets)
    initial = vcat $ map (binding "var") (toList deqInitial)
    rates   = vcat $ map (binding "d/dt") (toList deqRates)

    binding decl (i,e) = hsep [decl, text (unpack i), "=", ppExpr e]
