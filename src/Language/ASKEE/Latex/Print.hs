{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.Latex.Print where

import qualified Data.Map as Map
import Data.Text ( unpack )

import Language.ASKEE.DEQ.Syntax ( DiffEqs(..) )
import Language.ASKEE.Core
import Language.ASKEE.Panic ( panic )

import Text.PrettyPrint as PP
import Text.Printf ( printf )

printLatex :: DiffEqs -> Doc
printLatex DiffEqs {..} = inArray . vcat . terminate . concat $ [lets, initials, rates]
  where
    terminate lst = PP.punctuate "\\\\" lst
    inArray bdy = vcat ["\\begin{eqnarray}", bdy, "\\end{eqnarray}" ]
    lets = map binding (Map.toList deqLets)
    initials = map initBinding (Map.toList deqInitial)
    rates = map ddtBinding (Map.toList deqRates)

    binding (ident, expr) = hsep [ppIdent (unpack ident), " & = & ", printExpr expr]
    ddtBinding (ident, expr) = hcat ["\\frac{d ", ppIdent (unpack ident), "}{dt} & = & ", printExpr expr]
    initBinding (i,e) = hsep [hcat [ppIdent (unpack i),parens (int 0)], " & = & ", printExpr e]


greekSyms :: Map.Map String String
greekSyms =
  Map.fromList $
    withBs <$> ["alpha", "beta", "gamma", "nu", "mu", "theta"]
  where
    withBs a = (a, '\\':a)


-- TODO: we need a better solution for identifiers
ppIdent :: String -> Doc
ppIdent s =
  case Map.lookup s greekSyms of
    Just r -> text r
    Nothing ->
      if '_' `elem` s
        then text $ "\\verb|" ++ s ++ "|"
        else text s
  
-- | Specialized version of `ppExpr` in Language.ASKEE.Core
printExpr :: Expr -> Doc
printExpr expr =
  case expr of
    NumLit d -> text $ printf "%f" d
    Op1 Neg e' -> "-"PP.<>parens (pp e')
    Op1 Paren e' -> parens (printExpr e')
    e1 :+: e2 -> hsep [pp e1, "+", pp e2]
    e1 :-: e2 -> hsep [pp e1, "-", pp e2]
    e1 :*: e2 -> hsep [pp e1, " ", pp e2]
    e1 :/: e2 -> hcat ["\\frac{", printExpr e1, "}{", printExpr e2, "}"]
    Var v -> ppIdent (unpack v)
    _ -> 
      panic 
        "encountered unknown Core expression when pretty-printing latex" 
        [ show expr ]

  where
    pp :: Expr -> Doc
    pp e = 
      if prec e > prec expr
        then parens (printExpr e)
        else            printExpr e

    prec :: Expr -> Int
    prec e =
      case e of
        NumLit  _ -> 0
        BoolLit _ -> 0
        Op1 Neg _ -> 10
        Op1 Not _ -> 10
        Op1 Paren _ -> 0
        _ :+:   _ -> 6
        _ :-:   _ -> 6
        _ :*:   _ -> 7
        _ :/:   _ -> 7
        _ :<:   _ -> 4
        _ :<=:  _ -> 4
        _ :==:  _ -> 4
        _ :&&:  _ -> 3
        _ :||:  _ -> 3
        Var     _ -> 0
        If     {} -> 1
        Fail s    -> error s -- XXX
        _ -> 
          panic 
            "encountered unknown Core expression when pretty-printing latex" 
            [ "while determining precedence", show e ]
