{-# LANGUAGE OverloadedStrings #-}

module Language.ASKEE.Core.CTMC (ppCTMC, ppTransition, ppExpr, text, Doc) where

import qualified Data.Text as Text
import qualified Data.Map as Map

import Language.ASKEE.Core.Syntax
import Language.ASKEE.Core.Expr
import Language.ASKEE.Panic       ( panic )

import qualified Prettyprinter as PP
import Prettyprinter ( (<+>)
                     , emptyDoc
                     , hsep
                     , vcat
                     , parens
                     , Pretty(pretty)
                     , indent
                     , line
                     , lbracket
                     , rbracket
                     , colon
                     , semi
                     , hang
                     )
import           Text.Printf      ( printf )

type Doc = PP.Doc ()

-- | NB: does _not_ print metadata 
ppCTMC :: Model ->  Doc
ppCTMC corem = vcat [decl, startModule, body, endModule, rewards]
  where
    m = inlineLets $ inlineParams corem

    decl :: Doc
    decl = vcat [ text "ctmc"
                , line
                , text "const int MAX;"
                , line
                ]

    startModule :: Doc
    startModule = text "module" <+> pretty (modelName m) <> line

    body :: Doc 
    body = indent 2 (vcat $ [ pretty (Text.toLower x) <> text ": [0..MAX] init" <+> ppExpr e <> semi
                | (x,e) <- Map.toList (modelInitState m)
                ] ++[line] ++ map ppTransition (modelEvents m))

    endModule :: Doc
    endModule = text "endmodule" <> line

    rewards :: Doc 
    rewards = vcat [text ("rewards"<>" \"time\""), indent 2 rewardsBody, text "endrewards"]

    rewardsBody :: Doc
    rewardsBody = vcat [rewardOne]

    rewardOne :: Doc
    rewardOne = hsep [ text "true : 1", semi ]

-- | Events translated to transitions in CTMC
ppTransition :: Event -> Doc
ppTransition ev = hsep [ label, guard <+> text "->", rate <> colon, effect <> semi ]
  where 
    label :: Doc
    label = lbracket <> pretty (eventName ev) <> rbracket

    guard :: Doc
    guard = ppExpr $ eventWhen ev 

    rate :: Doc
    rate = ppExpr $ eventRate ev

    effect :: Doc
    effect = hsep $ insertAnd [ pretty (Text.toLower x) <>"'" <+> "=" <+> ppExpr e | (x,e) <- Map.toList (eventEffect ev) ]

    insertAnd :: [Doc] -> [Doc]
    insertAnd [] = [emptyDoc]
    insertAnd (h:[]) = [ parens (h) ] 
    insertAnd (h:t) = [ parens (h) <> text " &"] ++ insertAnd t 
ppExpr :: Expr -> Doc
ppExpr expr =
  case expr of
    NumLit d -> if d == fromInteger (round d)
                then text $ show (round d)
                else text $ printf "%f" d
    BoolLit b -> if b then "true" else "false"
    Op1 Neg e' -> "-"PP.<>pp e'
    Op1 Not e' -> "not"PP.<+>pp e'
    e1 :+: e2 -> PP.hsep [pp e1, "+", pp e2]
    e1 :-: e2 -> PP.hsep [pp e1, "-", pp e2]
    e1 :*: e2 -> PP.hsep [pp e1, "*", pp e2]
    e1 :/: e2 -> PP.hsep [pp e1, "/", pp e2]
    e1 :<: e2 -> case e1 of 
                  NumLit d | d == 0 -> PP.hsep [pp e1, "<", pp e2, "&", pp e2, "< MAX"]
                  _ -> PP.hsep [pp e1, "<", pp e2] 
    e1 :<=: e2 -> PP.hsep [pp e1, "<=", pp e2]
    e1 :==: e2 -> PP.hsep [pp e1, "==", pp e2]
    e1 :&&: e2 -> PP.hsep [pp e1, "&", pp e2]
    e1 :||: e2 -> PP.hsep [pp e1, "|", pp e2]
    Var v -> text $ Text.unpack $ Text.toLower v
    If e1 e2 e3 -> PP.hsep ["if", pp e1, "then", pp e2, "else", pp e3]
    Fail s -> error s -- XXX
    _ -> 
      panic 
        "encountered unknown Core expression when pretty-printing" 
        [ show expr ]

  where
    pp :: Expr -> Doc
    pp e = 
      if prec e < prec expr
        then PP.parens (ppExpr e)
        else            ppExpr e

    prec :: Expr -> Int
    prec e =
      case e of
        NumLit  _ -> 10
        BoolLit _ -> 10
        Op1 Neg _ -> 10
        Op1 Not _ -> 10
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
        If     {} -> 0
        Fail s    -> error s -- XXX
        _ -> 
          panic 
            "encountered unknown Core expression when pretty-printing" 
            [ "while determining precedence", show e ]

text :: String -> Doc
text = PP.pretty
