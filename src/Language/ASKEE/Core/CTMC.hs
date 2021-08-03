{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.ASKEE.Core.CTMC ( ppCTMC
                                , ppTransition
                                , ppExpr
                                , text
                                , Doc
                                , ppCTMCSimpl
                                ) where

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
                     )
import           Text.Printf      ( printf )
import Language.ASKEE.Core.Properties 

type Doc = PP.Doc ()

-- | NB: does _not_ print metadata 
ppCTMC :: Model ->  Doc
ppCTMC m = 
  case populationLimit m of 
    Nothing -> error "It's not possible to convert this model to CTMC"
    Just totalPopulation -> vcat [decl, startModule, body <> line, endModule, rewards]
      where
        decl :: Doc
        decl = vcat [ text "ctmc"
                    , line
                    ]

        startModule :: Doc
        startModule = text "module" <+> pretty (modelName m) <> line

        body :: Doc 
        body = indent 2 (vcat $ [ pretty (Text.toLower x) <> text ": [0.."<> pretty (show (round totalPopulation::Integer)) <> text "] init" <+> ppExpr e <> semi
                    | (x,e) <- Map.toList (modelInitState m)
                    ] ++ [line] ++ map ppTransition (modelEvents m))

        endModule :: Doc
        endModule = text "endmodule" <> line

        rewards :: Doc 
        rewards = vcat [text ("rewards"<>" \"time\""), indent 2 rewardsBody, text "endrewards"]

        rewardsBody :: Doc
        rewardsBody = vcat [rewardOne]

        rewardOne :: Doc
        rewardOne = hsep [ text "true : 1", semi ]

-- | Top-level function that converts a core model in to corresponding CTMC
ppCTMCSimpl :: Model -> Maybe Doc
ppCTMCSimpl m =
  do  m' <- withExplicitPopulationLimits m >>= simplifyPopulations
      let m'' = inlineLets $ inlineParams m'
      p <- ppCTMCProp m''
      pure $ vcat [ ppCTMC m'', line, text "\"correctness\":" <+> text "filter(forall,"<+> p <> text ")" <> semi ]
      

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
    insertAnd [h] = [ parens h ] 
    insertAnd (h:t) = parens h <> text " &" : insertAnd t 

ppAsserts :: Model -> Maybe Doc
ppAsserts m =  Just $ vcat $ ppExpr <$> modelAsserts m
  
ppCTMCProp :: Model -> Maybe Doc
ppCTMCProp m =
  do  m' <- withExplicitPopulationLimits m >>= simplifyPopulations
      ppAsserts (inlineLets $ inlineParams m')

ppExpr :: Expr -> Doc
ppExpr expr =
  case simplifyExpr $ toFrom $ simplifyExpr expr of
    NumLit d -> let (val::Integer) = round d in
                if d == fromInteger val
                then text $ show val
                else text $ printf "%f" d
    BoolLit b -> if b then "true" else "false"
    Op1 Neg e' -> "-"PP.<>pp e'
    Op1 Not e' -> "not"PP.<+>pp e'
    e1 :+: e2 -> PP.hsep [pp e1, "+", pp e2]
    e1 :-: e2 -> PP.hsep [pp e1, "-", pp e2]
    e1 :*: e2 -> PP.hsep [pp e1, "*", pp e2]
    e1 :/: e2 -> PP.hsep [pp e1, "/", pp e2]
    e1 :<: e2 -> PP.hsep [pp e1, "<", pp e2]
    e1 :<=: e2 -> PP.hsep [pp e1, "<=", pp e2]
    e1 :==: e2 -> PP.hsep [pp e1, "==", pp e2]
    e1 :&&: e2 -> PP.hsep [pp e1, "&", pp e2]
    e1 :||: e2 -> PP.hsep [pp e1, "|", pp e2]
    Var v -> text $ Text.unpack $ Text.toLower $ head $ Text.splitOn "_" v 
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
