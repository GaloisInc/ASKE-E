{-# Language OverloadedStrings #-}
module Language.ASKEE.Gromet.See where

import Data.Text(Text)
import qualified Data.Text as Text
import Text.PrettyPrint hiding ((<>))

import Language.ASKEE.Gromet.Syntax

class PP t where
  pp :: t -> Doc

instance PP Gromet where
  pp g = ppBox g (grometRoot g)

instance PP Text where
  pp = text . Text.unpack

instance PP PortType where
  pp po =
    case po of
      ParameterPort -> "parameter"
      StatePort     -> "state"
      InputPort     -> "input"
      OutputPort    -> "output"

instance PP ValueType where
  pp vt =
    case vt of
      Real  -> "real"
      Bool  -> "bool"

instance PP Port where
  pp po =
    pp (portType po) <+> pp (portValueType po) <+> pp (portName po)
      <+> "// uid:" <+> pp (portUid po) <+> comma <+> "box:" <+> pp (portBox po)

instance PP PortUid where
  pp (PortUid x) = "P:" <> pp x

instance PP BoxUid where
  pp (BoxUid x) = "B:" <> pp x


instance PP WireUid where
  pp (WireUid x) = "W:" <> pp x

instance PP Wire where
  pp w = pp (wireSource w) <+> arrow <+> pp (wireTarget w)
          <+> "//" <+> pp (wireUid w)
    where
    arrow = case wireType w of
              Directed   -> "->"
              Undirected -> "--"

data GrometBox = GrometBox Gromet Box

instance PP GrometBox where
  pp (GrometBox g bo) = front <+> "box" <+> quotes (pp (boxName bo))
                          <+> "//" <+> pp (boxUid bo) $$ nest 2 def
    where
    ppPort pid = case [ p | p <- grometPorts g, portUid p == pid ] of
                   p : _ -> pp p
                   _     -> "undefined port" <+> pp pid

    ppWire wid = case [ w | w <- grometWires g, wireUid w == wid ] of
                   w : _ -> pp w
                   _     -> "undefined wire" <+> pp wid

    (front,back) = case boxSyntax bo of
                    BoxExpression e -> ("expression", pp e)
                    BoxRelation r   -> (pp r, empty)

    def = vcat [ vcat (map ppPort (boxPorts bo))
               , " "
               , vcat (map ppWire (boxWires bo))
               , " "
               , back
               , " "
               , vcat (map (ppBox g) (boxBoxes bo))
               ]

ppBox :: Gromet -> BoxUid -> Doc
ppBox g bid = case [ b | b <- grometBoxes g, boxUid b == bid ] of
                b : _ -> pp (GrometBox g b)
                _  -> "undefined box" <+> pp bid



instance PP Arg where
  pp arg =
    case arg of
      ArgPort pid    -> pp pid
      ArgLiteral x t -> parens (pp x <+> ":" <+> pp t)
      ArgCall f as   -> parens (pp f <+> hsep (map pp as))

instance PP BoxRelType where
  pp r =
    case r of
      PrTNet      -> "PrTNet"
      EnableRel   -> "enable"
      EventRel    -> "event"
      RateRel     -> "rate"
      EffectRel   -> "effect"



