{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ABM.Print where

import qualified Data.Map as Map
import Data.Map ( Map )
import qualified Data.Text as Text
import Data.Text ( Text )
import Language.ASKEE.ABM.Syntax
import Language.ASKEE.Expr ( Expr )
import Language.ASKEE.Print ( printExpr )

import Prelude hiding ( (<>) )

import Text.PrettyPrint

printABM :: Model -> Doc
printABM Model{..} = vcat $
  [ printStatuses (Map.elems modelAgent)
  , printAgent modelAgent
  , "model "<>printText modelName<>":"
  ] ++ map (nest 4)
  [ printLets modelLets
  , printEvents modelEvents
  ]
  

printStatuses :: [AgentAttribute] -> Doc
printStatuses = vcat . map printStatusDecl
  where
    printStatusDecl :: AgentAttribute -> Doc
    printStatusDecl AgentAttribute{..} = vcat $
      ("status "<>printText attributeName<>":") :
      map (nest 4 . printText) attributeStatuses

printAgent :: Map Text AgentAttribute -> Doc
printAgent a = vcat $
  "agent A:":
  [ nest 4 $ printText attrName<>" :: "<>printText attrType
  | (attrName, AgentAttribute attrType _) <- Map.toList a
  ]

printLets :: Map Text Expr -> Doc
printLets = vcat . map printLet . Map.toList
  where
    printLet (v, e) = "let "<>printText v<>" = "<>printExpr e

printEvents :: [Event] -> Doc
printEvents = vcat . map printEvent
  where
    printEvent Event{..} = vcat $
      [ "event "<>
        printText eventName<>
        parens (hcat $ punctuate comma $ map printText eventAgents)
        <>":"
      , nest 4 "when:"
      , nest 8 $ printAgentExpr eventWhen
      , nest 4 "rate:"
      , nest 8 $ printExpr eventRate
      , nest 4 "effect:"
      ] ++ map (nest 8 . printAgentAssign) eventEffect

    printAgentAssign :: AgentAssign -> Doc
    printAgentAssign (AgentAssign a1 a2) =
      case (a1, a2) of
        (Attribute{}, Attribute{}) -> printAttributeRef a1<>" = "<>printAttributeRef a2
        (Attribute{}, Status{}) -> printAttributeRef a1<>" = "<>printAttributeRef a2
        _ -> undefined

printAgentExpr :: AgentExpr -> Doc
printAgentExpr e =
  case e of
    Eq r1 r2 -> printAttributeRef r1<>" == "<>printAttributeRef r2
    And a1 a2 -> printAgentExpr a1<>" and "<>printAgentExpr a2
    Or a1 a2 -> printAgentExpr a1<>" or "<>printAgentExpr a2

printAttributeRef :: AttributeRef -> Doc
printAttributeRef a =
  case a of
    Status stat -> printText stat
    Attribute agent attr -> printText agent<>"."<>printText attr

printText :: Text -> Doc
printText = text . Text.unpack