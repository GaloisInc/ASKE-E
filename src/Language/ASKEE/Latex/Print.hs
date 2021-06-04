{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.Latex.Print where

import qualified Data.Map as Map
import Data.Text ( unpack )

import Language.ASKEE.DEQ.Syntax ( DiffEqs(..) )
import Language.ASKEE.Latex.Syntax
import Language.ASKEE.Core.Expr
import Language.ASKEE.Panic ( panic )

import Text.PrettyPrint as PP
import Text.Printf ( printf )

printLatex :: Latex -> Doc
printLatex (Latex DiffEqs{..}) = vcat [lets, initials, rates]
  where
    lets = vcat $ map binding (Map.toList deqLets)
    initials = vcat $ map initBinding (Map.toList deqInitial)
    rates = vcat $ map ddtBinding (Map.toList deqRates)

    binding (ident, expr) = hsep [text (unpack ident), "=", printExpr expr]
    ddtBinding (ident, expr) = hcat ["\\frac{d ", text (unpack ident), "}{dt} = ", printExpr expr]
    initBinding (i,e) = hsep [hcat [text (unpack i),parens (int 0)], "=", printExpr e]

  
-- | Specialized version of `ppExpr` in Language.ASKEE.Core.Print
printExpr :: Expr -> Doc
printExpr expr =
  case expr of
    NumLit d -> text $ printf "%f" d
    Op1 Neg e' -> "-"PP.<>parens (pp e')
    e1 :+: e2 -> hsep [pp e1, "+", pp e2]
    e1 :-: e2 -> hsep [pp e1, "-", pp e2]
    e1 :*: e2 -> hsep [pp e1, "*", pp e2]
    e1 :/: e2 -> hcat ["\\frac{", pp e1, "}{", pp e2, "}"]
    Var v -> text $ unpack v
    _ -> 
      panic 
        "encountered unknown Core expression when pretty-printing latex" 
        [ show expr ]

  where
    pp :: Expr -> Doc
    pp e = 
      if prec e < prec expr
        then parens (printExpr e)
        else         printExpr e

    prec :: Expr -> Int
    prec e =
      case e of
        NumLit  _ -> 10
        BoolLit _ -> 10
        Op1 Neg _ -> 0
        Op1 Not _ -> 0
        _ :+:   _ -> 6
        _ :-:   _ -> 6
        _ :*:   _ -> 7
        _ :/:   _ -> 7
        _ :<:   _ -> 4
        _ :<=:  _ -> 4
        _ :==:  _ -> 4
        _ :&&:  _ -> 3
        _ :||:  _ -> 3
        Var     _ -> 10
        If     {} -> 1
        Fail s    -> error s -- XXX
        _ -> 
          panic 
            "encountered unknown Core expression when pretty-printing latex" 
            [ "while determining precedence", show e ]
