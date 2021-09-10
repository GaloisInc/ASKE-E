{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Language.ASKEE.ESL.Print where

import Data.Text (unpack, Text )

import Language.ASKEE.ESL.Syntax as Syntax
import Language.ASKEE.Expr as Expr

import Prelude hiding (GT, EQ, LT, (<>))

import Prettyprinter
import Language.ASKEE.Metadata

instance Pretty Expr where
  pretty = printExpr

-- Notes:
-- More line spacing could be nice?

pyPrintExpr :: Expr -> Doc a
pyPrintExpr expr = 
  case expr of
    (Add e1 e2) -> binop e1 "+"   e2
    (Sub e1 e2) -> binop e1 "-"   e2
    (Mul e1 e2) -> binop e1 "*"   e2
    (Div e1 e2) -> binop e1 "/"   e2
    (Neg e1) -> pretty '-' <> pp e1
    (LitD d) -> pretty d
    (Var i) -> pretty i
    (GT e1 e2)  -> aBinop e1 ">"   e2
    (GTE e1 e2) -> aBinop e1 ">="  e2
    (EQ e1 e2)  -> aBinop e1 "=="  e2
    (LTE e1 e2) -> aBinop e1 "<="  e2
    (LT e1 e2)  -> aBinop e1 "<"   e2
    (And e1 e2) -> lBinop e1 "and" e2
    (Or e1 e2)  -> lBinop e1 "or"  e2
    (Not e1) -> 
      hsep  [ text "not"
            , pp e1]
    If e1 e2 e3 -> 
      hsep  [ pp e2 
            , text "if"
            , pp e1
            , text "else"
            , pp e3
            ]
    Cond _ _ -> undefined
    LitB True -> text "True"
    LitB False -> text "False"
    (Fn f args) ->
      case (f, args) of
        ("exp", [e1]) -> "math.exp"<>parens (pyPrintExpr e1)
        ("exp", as) -> error $ 
          "pyPrintExpr: arity mismatch: function `exp` expected 1 argument, received "++
          show (length as)++
          ": "++show as
        ("log", [e1]) -> "math.log"<>parens (pyPrintExpr e1)
        ("log", as) -> error $ 
          "pyPrintExpr: arity mismatch: function `log` expected 1 argument, received "++
          show (length as)++
          ": "++show as
        _ -> error $ "pyPrintExpr: didn't recognize function "++unpack f

  
  where
    binop = expBinop pp
    aBinop = expBinop pp
    lBinop = expBinop pp

    pp :: Expr -> Doc a
    pp e = 
      if prec e <= prec expr
        then parens (pyPrintExpr e)
        else         pyPrintExpr e
        
    prec :: Expr -> Int
    prec e =
      case e of
        LitD _ -> 10
        LitB _ -> 10
        Neg _ -> 1
        Not _ -> 1
        Add _ _ -> 6
        Sub _ _ -> 6
        Mul _ _ -> 7
        Div _ _ -> 7
        LT _ _ -> 4
        LTE _ _ -> 4
        EQ _ _ -> 4
        GTE _ _ -> 4
        GT _ _ -> 4
        And _ _ -> 3
        Or _ _ -> 3
        Var _ -> 10
        If {} -> 0
        Cond {} -> 0
        Fn {} -> 10


printExpr :: Expr -> Doc a
printExpr expr = 
  case expr of
    (Add e1 e2) -> binop e1 "+"   e2
    (Sub e1 e2) -> binop e1 "-"   e2
    (Mul e1 e2) -> binop e1 "*"   e2
    (Div e1 e2) -> binop e1 "/"   e2
    (Neg e1) -> pretty '-' <> pp e1
    (LitD d) -> pretty d
    (Var i) -> pretty (unpack i)
    (GT e1 e2)  -> aBinop e1 ">"   e2
    (GTE e1 e2) -> aBinop e1 ">="  e2
    (EQ e1 e2)  -> aBinop e1 "=="  e2
    (LTE e1 e2) -> aBinop e1 "<="  e2
    (LT e1 e2)  -> aBinop e1 "<"   e2
    (And e1 e2) -> lBinop e1 "and" e2
    (Or e1 e2)  -> lBinop e1 "or"  e2
    (Not e1) -> 
      hsep  [ text "not"
            , pp e1]
    If e1 e2 e3 -> 
      hsep  [ text "if"
            , pp e1
            , text "then"
            , pp e2 
            , text "else"
            , pp e3
            ]
    Cond branches other ->
      let decl = text "cond:"
          branches' = vcat $ case other of
            Just e'  -> map (uncurry condBranch) branches ++ [condOther e']
            Nothing -> map (uncurry condBranch) branches
      in  vcat [indent 2 decl, indent 4 branches']
    LitB True -> text "true"
    LitB False -> text "false"
    Fn f args -> pretty f<>parens (hcat (punctuate comma (map pretty args)))
  
  where
    binop = expBinop pp
    aBinop = expBinop pp
    lBinop = expBinop pp

    pp :: Expr -> Doc a
    pp e = 
      if prec e <= prec expr
        then parens (pretty e)
        else         pretty e
        
    prec :: Expr -> Int
    prec e =
      case e of
        LitD _ -> 10
        LitB _ -> 10
        Neg _ -> 1
        Not _ -> 4
        Add _ _ -> 6
        Sub _ _ -> 6
        Mul _ _ -> 7
        Div _ _ -> 7
        LT _ _ -> 4
        LTE _ _ -> 4
        EQ _ _ -> 4
        GTE _ _ -> 4
        GT _ _ -> 4
        And _ _ -> 3
        Or _ _ -> 3
        Var _ -> 10
        If {} -> 0
        Cond {} -> 0
        Fn {} -> 10

    condBranch :: Expr -> Expr -> Doc a
    condBranch e1 e2 = 
      pretty e1 <+>
      text "if" <+>
      pretty e2

    condOther :: Expr -> Doc a
    condOther e =
      pretty e <+>
      text "otherwise"

expBinop :: (a -> Doc b) -> a -> String -> a -> Doc b
expBinop pr e1 op e2 = 
  hsep  [ pr e1
        , pretty op
        , pr e2
        ]

printEvent :: Event -> Doc a
printEvent Event{..} = vcat [decl, indent 2 body]
  where
    decl :: Doc a
    decl = text "event" <+>
           pretty (unpack eventName) <>
           pretty ':'

    body :: Doc a
    body = vcat [when, rate, effect]

    rate :: Doc a
    rate = vcat [text "rate:", indent 2 (pretty eventRate)]

    when :: Doc a
    when = case eventWhen of
      Nothing -> emptyDoc
      Just w -> vcat [text "when:", indent 2 (pretty w)]

    effect :: Doc a
    effect = vcat [text "effect:", indent 2 statements]
    
    statements :: Doc a
    statements = vcat $ map (uncurry printAssign) eventEffect

    printAssign :: Text -> Expr -> Doc a
    printAssign ident e = 
      hsep [ pretty (unpack ident)
           , pretty '='
           , pretty e]

-- | NB: does _not_ print metadata
printModel :: Model -> Doc a
printModel Model{..} = vcat [decl, indent 2 body]
  where
    decl :: Doc a
    decl = text "model" <+>
           pretty (unpack modelName) <>
           pretty ':'

    body :: Doc a
    body = vcat (state ++ events)

    state :: [Doc a]
    state = map (printDecl . metaValue) modelDecls

    events :: [Doc a]
    events = map printEvent modelEvents

    printDecl :: Decl -> Doc a
    printDecl (Let name val) = 
      hsep [ text "let"
           , pretty (unpack name)
           , pretty '='
           , pretty val
           ]
    printDecl (State name val) = 
      hsep [ text "state"
           , pretty (unpack name)
           , pretty '='
           , pretty val
           ]
    printDecl (Assert e) =
      hsep [ text "assert"
           , pretty e
           ]
    printDecl (Parameter name e) =
      case e of
        Just v -> hsep [ "parameter", pretty (unpack name), "=", pretty v ]
        Nothing -> hsep [ "parameter", pretty (unpack name) ]

text :: String -> Doc a
text = pretty