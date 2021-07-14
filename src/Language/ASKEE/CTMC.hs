{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.CTMC
    ( printCTMC
    , getESL
    ) 
    where

import Language.ASKEE.ESL.Syntax as Syntax
import Language.ASKEE.Expr as Expr

import Data.Text (unpack, Text, toLower)
import Language.ASKEE.ESL.Print (Doc)
import Prelude hiding (GT, EQ, LT)

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
import Language.ASKEE.Metadata
import Text.Printf ( printf )
--import Language.ASKEE.ESL.Print (printExpr)
import qualified Language.ASKEE.ESL as ESL
import Language.ASKEE ( DataSource( FromFile )
                      , loadESL
                      )



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
    (LitD d) -> if d == fromInteger (round d)
                then text $ show (round d)
                else text $ printf "%f" d
    (Var i) -> pretty (unpack $ toLower i)
    (GT e1 e2)  -> case e2 of 
                      LitD d -> if d == 0 then aBinop e1 ">" e2 <> text " & " <> printExpr e1 <> text "< MAX"
                                          else aBinop e1 ">" e2
                      _ -> aBinop e1 ">" e2
    (GTE e1 e2) -> aBinop e1 ">="  e2
    (EQ e1 e2)  -> aBinop e1 "="  e2
    (LTE e1 e2) -> aBinop e1 "<="  e2
    (LT e1 e2)  -> aBinop e1 "<"   e2
    (And e1 e2) -> lBinop e1 "&" e2
    (Or e1 e2)  -> lBinop e1 "|"  e2
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

printTransition :: Event -> Doc
printTransition Event{..} = hsep [decl, indent 2 body]
  where
    decl :: Doc
    decl = lbracket <+> pretty (unpack eventName) <+> rbracket

    body :: Doc
    body = hsep [when, text "->", rate, effect]

    rate :: Doc
    rate = hsep [indent 2 (printExpr eventRate), colon]

    when :: Doc
    when = case eventWhen of
      Nothing -> emptyDoc
      Just w -> hsep [indent 2 (printExpr w)]

    effect :: Doc
    effect = hsep [indent 2 statements, semi]
    
    statements :: Doc
    statements = hsep $ insertAnd $ map (uncurry printAssign) eventEffect
    
    insertAnd :: [Doc] -> [Doc]
    insertAnd [] = [emptyDoc]
    insertAnd (h:[]) = [ parens (h) ] 
    insertAnd (h:t) = [ parens (h) <> pretty " &"] ++ insertAnd t 


    printAssign :: Text -> Expr -> Doc
    printAssign ident e = 
      hsep [ pretty $ (unpack $ toLower ident) <> "'"
           , pretty '='
           , printExpr e
           ]

-- | NB: does _not_ print metadata
printCTMC :: Model -> Doc
printCTMC Model{..} = vcat [decl, consts, startModule, indent 4 body, endModule, rewards]
  where
    decl :: Doc
    decl = vcat [ text "ctmc"
                , line
                , text "const int MAX;"
                ]
    consts :: Doc
    consts = vcat $ map (printLets . metaValue) modelDecls
    
    printLets :: Decl -> Doc
    printLets (Let name val) = 
      case val of
        LitD c -> hsep [ if c == fromInteger (round c)
                           then text "const int"
                           else text "const double"
                          , pretty (unpack name)
                          , pretty '='
                          , printExpr val <+> semi
                          ]
        _   -> hsep [ text "const int"
                    , pretty (unpack name)
                    , pretty '='
                    , printExpr val
                    , semi
                    ]                   
    printLets _ = emptyDoc

    startModule :: Doc
    startModule =  text "module" <+>
                   text (unpack modelName) 

    endModule :: Doc
    endModule = text "endmodule" <> line

    body :: Doc
    body = vcat (state ++ events ++ [line])

    state :: [Doc]
    state = map (printStates . metaValue) modelDecls

    events :: [Doc]
    events = map printTransition modelEvents

    printStates :: Decl -> Doc
    printStates (State name val) = 
      hsep [ if toLower name == name then pretty (unpack name)
             else pretty (unpack $ toLower name)
           , colon
           , pretty "[0..MAX] init"
           , printExpr val
           , semi
           ]
    printStates _ = vcat []
    --printDecl (Assert e) = hsep []
     -- hsep [ text "assert"
     --      , printExpr e
     --      ]
    --printDecl (Parameter name e) = hsep []
     --case e of
     -- Just v -> hsep [ "parameter", pretty (unpack name), "=", printExpr v ]
     --    Nothing -> hsep [ "parameter", pretty (unpack name) ]

    rewards :: Doc 
    rewards = vcat [text ("rewards"<>" \"time\""), indent 4 rewardsBody, text "endrewards"]

    rewardsBody :: Doc
    rewardsBody = vcat [rewardOne]

    rewardOne :: Doc
    rewardOne = hsep [ pretty "true : 1", semi ]

    

text :: String -> Doc
text = pretty

-- | Useful for testing
getESL :: FilePath -> IO ESL.Model
getESL path = loadESL $ FromFile path
  
