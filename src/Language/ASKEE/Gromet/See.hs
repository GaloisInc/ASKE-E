{-# Language OverloadedStrings #-}
module Language.ASKEE.Gromet.See where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Text.PrettyPrint hiding ((<>))

import Language.ASKEE.Gromet.Syntax

class PP t where
  pp :: t -> Doc

instance PP Gromet where
  pp g = ppMeta (grometMeta g) $$ ppBox g (grometRoot g)

instance PP Text where
  pp = text . Text.unpack

instance PP PortType where
  pp po =
    case po of
      ParameterPort -> "parameter"
      InputPort     -> "input"
      OutputPort    -> "output"

instance PP ValueType where
  pp vt =
    case vt of
      Real    -> "real"
      Boolean -> "bool"
      Integer -> "integer"

instance PP Port where
  pp po =
    pp (portType po) <+> pp (portValueType po) <+> pp (portName po)
      <+> "// uid:" <+> pp (portUid po) <+> comma <+> "box:" <+> pp (portBox po)
      $$ nest 2 (ppMeta (portMeta po))

instance PP PortUid where
  pp (PortUid x) = pp x

instance PP BoxUid where
  pp (BoxUid x) = pp x

instance PP WireUid where
  pp (WireUid x) = pp x

instance PP JunctionUid where
  pp (JunctionUid x) = pp x

instance PP WirePort where
  pp p =
    case p of
      JPort x -> pp x
      PPort x -> pp x

instance PP Junction where
  pp j = "state" <+> pp (jName j) <+> ":" <+> pp (jValueType j)
          <+> "//" <+> pp (jUID j) $$ nest 2 (ppMeta (jMeta j))


instance PP Wire where
  pp w = pp (wireSource w) <+> arrow <+> pp (wireTarget w)
          <+> "//" <+> pp (wireUid w)
    where
    arrow = case wireType w of
              Directed   -> "->"
              Undirected -> "--"

data GrometBox = GrometBox Gromet Box

instance PP GrometBox where
  pp (GrometBox g bo) =
      vcat [ front <+> "box" <+> quotes (pp (boxName bo))
                          <+> "//" <+> pp (boxUid bo)
                            $$ nest 2 (ppMeta (boxMeta bo))
           , nest 2 def
           , "end"
           , " "
           ]
    where
    ppPort pid = case [ p | p <- grometPorts g, portUid p == pid ] of
                   p : _ -> pp p
                   _     -> "undefined port" <+> pp pid

    ppJunc jid = case [ j | j <- grometJunctions g, jUID j == jid ] of
                   j : _ -> pp j
                   _     -> "undefined junction" <+> pp jid

    ppWire wid = case [ w | w <- grometWires g, wireUid w == wid ] of
                   w : _ -> pp w
                   _     -> "undefined wire" <+> pp wid

    (front,back) = case boxSyntax bo of
                    BoxExpression e -> ("expression", pp e)
                    BoxRelation r   -> (pp r, empty)

    def = vcat [ pps (map ppPort (boxPorts bo))
               , pps (map ppJunc (boxJuncitons bo))
               , pps (map ppWire (boxWires bo))
               , back
               , pps (map (ppBox g) (boxBoxes bo))
               ]

    pps xs = case xs of
               [] -> empty
               _  -> vcat xs $$ " "

ppBox :: Gromet -> BoxUid -> Doc
ppBox g bid = case [ b | b <- grometBoxes g, boxUid b == bid ] of
                b : _ -> pp (GrometBox g b)
                _  -> "undefined box" <+> pp bid

ppMeta :: Map Text [Text] -> Doc
ppMeta mp = vcat [ (pp k <> colon) <+> pp v | (k,vs) <- Map.toList mp, v <- vs ]


instance PP Arg where
  pp arg =
    case arg of
      ArgPort pid    -> pp pid
      ArgLiteral l   -> pp l
      ArgCall f as   -> parens (pp f <+> hsep (map pp as))


instance PP Literal where
  pp l = case l of
           LitBool b    -> if b then "True" else "False"
           LitReal r    -> parens (text (show r) <+> ":" <+> pp Real)
           LitInteger i -> integer i

instance PP BoxRelType where
  pp r =
    case r of
      PrTNet      -> "PrTNet"
      EnableRel   -> "enable"
      EventRel    -> "event"
      RateRel     -> "rate"
      EffectRel   -> "effect"



