{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ASKEE.ESL.Print where

import Data.Text (unpack, Text )

import Language.ASKEE.ESL.Syntax as Syntax
import Language.ASKEE.Expr as Expr

import Prelude hiding (GT, EQ, LT, (<>))

import Prettyprinter ( (<>)
                     , (<+>)
                     , emptyDoc
                     , hsep
                     , vcat
                     , parens
                     , Pretty(pretty), indent )
import qualified Prettyprinter as PP
import Language.ASKEE.Metadata

type Doc = PP.Doc ()

-- Notes:
-- More line spacing could be nice?

pyPrintExpr :: Expr -> Doc
pyPrintExpr expr = 
  case expr of
    (Add e1 e2) -> binop e1 "+"   e2
    (Sub e1 e2) -> binop e1 "-"   e2
    (Mul e1 e2) -> binop e1 "*"   e2
    (Div e1 e2) -> binop e1 "/"   e2
    (Exp e1) -> text "math.exp" <> parens (pyPrintExpr e1)
    (Log e1) -> text "math.log" <> parens (pyPrintExpr e1)
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
  
  where
    binop = expBinop pp
    aBinop = expBinop pp
    lBinop = expBinop pp

    pp :: Expr -> Doc
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
        Exp _ -> 1
        Log _ -> 1
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


printExpr :: Expr -> Doc
printExpr expr = 
  case expr of
    (Add e1 e2) -> binop e1 "+"   e2
    (Sub e1 e2) -> binop e1 "-"   e2
    (Mul e1 e2) -> binop e1 "*"   e2
    (Div e1 e2) -> binop e1 "/"   e2
    (Exp e1) -> text "exp" <> parens (printExpr e1)
    (Log e1) -> text "log" <> parens (printExpr e1)
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
  
  where
    binop = expBinop pp
    aBinop = expBinop pp
    lBinop = expBinop pp

    pp :: Expr -> Doc
    pp e = 
      if prec e <= prec expr
        then parens (printExpr e)
        else         printExpr e
        
    prec :: Expr -> Int
    prec e =
      case e of
        LitD _ -> 10
        LitB _ -> 10
        Neg _ -> 1
        Not _ -> 1
        Exp _ -> 1
        Log _ -> 1
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

    condBranch :: Expr -> Expr -> Doc
    condBranch e1 e2 = 
      printExpr e1 <+>
      text "if" <+>
      printExpr e2

    condOther :: Expr -> Doc
    condOther e =
      printExpr e <+>
      text "otherwise"

expBinop :: (a -> Doc) -> a -> String -> a -> Doc
expBinop pr e1 op e2 = 
  hsep  [ pr e1
        , pretty op
        , pr e2
        ]

printEvent :: Event -> Doc
printEvent Event{..} = vcat [decl, indent 2 body]
  where
    decl :: Doc
    decl = text "event" <+>
           pretty (unpack eventName) <>
           pretty ':'

    body :: Doc
    body = vcat [when, rate, effect]

    rate :: Doc
    rate = vcat [text "rate:", indent 2 (printExpr eventRate)]

    when :: Doc
    when = case eventWhen of
      Nothing -> emptyDoc
      Just w -> vcat [text "when:", indent 2 (printExpr w)]

    effect :: Doc
    effect = vcat [text "effect:", indent 2 statements]
    
    statements :: Doc
    statements = vcat $ map (uncurry printAssign) eventEffect

    printAssign :: Text -> Expr -> Doc
    printAssign ident e = 
      hsep [ pretty (unpack ident)
           , pretty '='
           , printExpr e]

-- | NB: does _not_ print metadata
printModel :: Model -> Doc
printModel Model{..} = vcat [decl, indent 2 body]
  where
    decl :: Doc
    decl = text "model" <+>
           pretty (unpack modelName) <>
           pretty ':'

    body :: Doc
    body = vcat (state ++ events)

    state :: [Doc]
    state = map (printDecl . metaValue) modelDecls

    events :: [Doc]
    events = map printEvent modelEvents

    printDecl :: Decl -> Doc
    printDecl (Let name val) = 
      hsep [ text "let"
           , pretty (unpack name)
           , pretty '='
           , printExpr val
           ]
    printDecl (State name val) = 
      hsep [ text "state"
           , pretty (unpack name)
           , pretty '='
           , printExpr val
           ]
    printDecl (Assert e) =
      hsep [ text "assert"
           , printExpr e
           ]
    printDecl (Parameter name e) =
      case e of
        Just v -> hsep [ "parameter", pretty (unpack name), "=", printExpr v ]
        Nothing -> hsep [ "parameter", pretty (unpack name) ]

text :: String -> Doc
text = pretty