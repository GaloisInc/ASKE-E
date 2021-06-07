{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.Latex.Print where

import qualified Data.Map as Map
import Data.Text ( unpack )
import Control.Monad.Identity ( Identity(Identity, runIdentity) )

import Language.ASKEE.DEQ.Syntax ( DiffEqs(..) )
import Language.ASKEE.Core
import Language.ASKEE.Panic ( panic )

import Text.PrettyPrint as PP
import Text.Printf ( printf )
import Language.ASKEE.Core.Simplify (simplifyExpr)

printLatex :: DiffEqs -> Doc
printLatex DiffEqs {..} = vcat [lets, initials, rates]
  where
    lets = vcat $ map binding (Map.toList deqLets)
    initials = vcat $ map initBinding (Map.toList deqInitial)
    rates = vcat $ map ddtBinding (Map.toList deqRates)

    binding (ident, expr) = hsep [text (unpack ident), "=", printExpr expr]
    initBinding (i,e) = hsep [hcat [text (unpack i),parens (int 0)], "=", printExpr e]
    ddtBinding (ident, expr) = hcat ["\\frac{d ", text (unpack ident), "}{dt} = ", printExpr expr]
    

  
-- | Specialized version of `ppExpr` in Language.ASKEE.Core
printExpr :: Expr -> Doc
printExpr expr =
  case simplify expr of
    NumLit c -> if c == fromInteger (round c)
      then text $ show $ round c
       else text $ printf "%f" c
    Op1 Neg e' -> "-"PP.<> pp e'
    e1 :+: e2 -> hsep [pp e1, "+", pp e2]
    e1 :-: e2 -> hsep [pp e1, "-", pp e2]
    e1 :*: e2 -> hsep [pp e1 PP.<> pp e2]
    e1 :/: e2 -> hcat ["\\frac{", printExpr e1, "}{", printExpr e2, "}"]
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
        Op1 Neg _ -> 7
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

simplify :: Expr -> Expr
simplify e =
  let e' = simplifyExpr $ mapAt exprChildren propLeft (propLeft e)
  in if e == e' then e' else simplify e'

propLeft :: Expr -> Expr
propLeft e0 =
  case e0 of
    Op2 Mul (NumLit n) (NumLit m) -> NumLit (n * m)
    e1 :*: (Op1 Neg e2) :*: e3 -> Op1 Neg e1 :*: e2 :*: e3
    Op2 Mul e1 e2@(NumLit _) -> Op2 Mul e2 e1
    Op2 Mul e1 (Op1 Neg e2) -> Op2 Mul (Op1 Neg e1) e2
    Op2 Mul e1 (Op2 Mul e2 e3) -> Op2 Mul (Op2 Mul e1 e2) e3
    _ -> e0