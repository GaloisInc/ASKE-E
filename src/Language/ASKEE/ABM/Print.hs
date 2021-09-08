{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ABM.Print where

import qualified Data.Map as Map
import Data.Map ( Map )
import Data.Text ( Text )
import Language.ASKEE.ABM.Syntax
import Language.ASKEE.Expr ( Expr )
import Language.ASKEE.ESL.Print ( printExpr )

import Prelude hiding ( (<>) )

import Prettyprinter

printABM :: Model -> Doc ()
printABM Model{..} = vcat $
  [ printStatuses (Map.elems modelAgent)
  , printAgent modelAgent
  , "model "<>pretty modelName<>":"
  ] ++ map (nest 4)
  [ printLets modelLets
  , printEvents modelEvents
  ]
  

printStatuses :: [AgentAttribute] -> Doc ()
printStatuses = vcat . map printStatusDecl
  where
    printStatusDecl :: AgentAttribute -> Doc ()
    printStatusDecl AgentAttribute{..} = vcat $
      ("status "<>pretty attributeName<>":") :
      map (nest 4 . pretty) attributeStatuses

printAgent :: Map Text AgentAttribute -> Doc ()
printAgent a = vcat $
  "agent A:":
  [ nest 4 $ pretty attrName<>" :: "<>pretty attrType
  | (attrName, AgentAttribute attrType _) <- Map.toList a
  ]

printLets :: Map Text Expr -> Doc ()
printLets = vcat . map printLet . Map.toList
  where
    printLet (v, e) = "let "<>pretty v<>" = "<>printExpr e

printEvents :: [Event] -> Doc ()
printEvents = vcat . map printEvent
  where
    printEvent Event{..} = vcat $
      [ "event "<>
        pretty eventName<>
        parens (hcat $ punctuate comma $ map pretty eventAgents)
        <>":"
      , nest 4 "when:"
      , nest 8 $ printAgentExpr eventWhen
      , nest 4 "rate:"
      , nest 8 $ printExpr eventRate
      , nest 4 "effect:"
      ] ++ map (nest 8 . printAgentAssign) eventEffect

    printAgentAssign :: AgentAssign -> Doc ()
    printAgentAssign (AgentAssign a1 a2) =
      case (a1, a2) of
        (Attribute{}, Attribute{}) -> printAttributeRef a1<>" = "<>printAttributeRef a2
        (Attribute{}, Status{}) -> printAttributeRef a1<>" = "<>printAttributeRef a2
        _ -> undefined

printAgentExpr :: AgentExpr -> Doc ()
printAgentExpr e =
  case e of
    Eq r1 r2 -> printAttributeRef r1<>" == "<>printAttributeRef r2
    And a1 a2 -> printAgentExpr a1<>" and "<>printAgentExpr a2
    Or a1 a2 -> printAgentExpr a1<>" or "<>printAgentExpr a2

printAttributeRef :: AttributeRef -> Doc ()
printAttributeRef a =
  case a of
    Status stat -> pretty stat
    Attribute agent attr -> pretty agent<>"."<>pretty attr