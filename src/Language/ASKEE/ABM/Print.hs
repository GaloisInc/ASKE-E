{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.ABM.Print where

import qualified Data.Map as Map
import Data.Map ( Map )
import Data.Text ( Text )
import Language.ASKEE.ABM.Syntax
import Language.ASKEE.Expr ( Expr )
import Language.ASKEE.ESL.Print ()

import Prelude hiding ( (<>) )

import Prettyprinter

printABM :: Model -> Doc ()
printABM Model{..} = vcat $
  [ printStatuses (Map.elems modelAgent)
  , printAgent modelAgent
  , "model "<>pretty modelName<>":"
  ] ++ map (indent 2)
  [ printInit modelInit
  , printLets modelLets
  , printEvents modelEvents
  ]
  

printStatuses :: [AgentAttribute] -> Doc ()
printStatuses = vcat . map printStatusDecl
  where
    printStatusDecl :: AgentAttribute -> Doc ()
    printStatusDecl AgentAttribute{..} = vcat $
      ("status "<>pretty attributeName<>":") :
      map (indent 2 . pretty) attributeStatuses

printAgent :: Map Text AgentAttribute -> Doc ()
printAgent a = vcat $
  "agent A:":
  [ indent 2 $ pretty attrName<>" :: "<>pretty attrType
  | (attrName, AgentAttribute attrType _) <- Map.toList a
  ]

printInit :: Map Text Expr -> Doc ()
printInit i = vcat $
  "init:":
  [ indent 2 $ pretty var<>" <- "<>pretty val
  | (var, val) <- Map.toList i
  ]

printLets :: Map Text Expr -> Doc ()
printLets = vcat . map printLet . Map.toList
  where
    printLet (v, e) = "let "<>pretty v<>" = "<>pretty e

printEvents :: [Event] -> Doc ()
printEvents = vcat . map printEvent
  where
    printEvent Event{..} = vcat $
      [ "event "<>
        pretty eventName<>
        parens (hcat $ punctuate comma $ map pretty eventAgents)
        <>":"
      , indent 2 "when:"
      , indent 4 $ printAgentExpr eventWhen
      , indent 2 "rate:"
      , indent 4 $ pretty eventRate
      , indent 2 "effect:"
      ] ++ map (indent 4 . printAgentAssign) eventEffect

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