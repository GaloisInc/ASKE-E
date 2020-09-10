-- | A continuous Petri Net as described in
-- http://eprints.gla.ac.uk/3476/1/gilbert.3476.pdf
{-# Language RecordWildCards, OverloadedStrings, BlockArguments #-}
module PetriNet where

import Data.Map(Map)
import qualified Data.Map as Map
import Text.PrettyPrint as PP


type StateId       = Int

-- | A description of a Petri Net
data PetriNet r = PetriNet
  { netIntialState :: Map StateId r
    -- ^ Defines the states of the network, as well as their initial values.

  , netTransitions  :: [Transition r]
  }

data Transition r = Transition
  { transitionRate    :: Map StateId r -> r
    -- ^ The rate at which this transition fire.
    -- This is a function of the values of the inputs states.
    -- This function should return 0, if any of its inputs is 0,
    -- as this would mean that the transition is not enabled.

  , transitionInputs  :: Map StateId r
    -- ^ The key is the input state, and the value is the weight of this edge.

  , transitionOutputs :: Map StateId r
    -- ^ The key is the output state, and the value is the weight of this edge.
  }

--------------------------------------------------------------------------------
-- Simulation

-- | Current state of the system.
type State r        = Map StateId r
type StateChange r  = Map StateId r

-- | Compute the change in a state based on a transition
transitionChange :: Arith r => Transition r -> State r -> StateChange r
transitionChange Transition { .. } s = Map.unionWith add inChange outChange
  where
  inS       = Map.restrictKeys s (Map.keysSet transitionInputs)
  rate      = transitionRate inS
  inChange  = (neg rate `mul`) <$> transitionInputs
  outChange = (rate `mul`)     <$> transitionOutputs

netStep :: Arith r => PetriNet r -> State r -> State r
netStep PetriNet { .. } s =
  Map.unionsWith add (s : [ transitionChange t s | t <- netTransitions ])

netTrace :: PetriNet Double -> [State Double]
netTrace net = iterate (netStep net) (netIntialState net)

class Arith r where
  add :: r -> r -> r
  mul :: r -> r -> r
  neg :: r -> r

instance Arith Double where
  add = (+)
  mul = (*)
  neg = negate

--------------------------------------------------------------------------------
-- Symbolic

data Expr     = App Op [Expr] | Var StateId
data Op       = Neg | Add | Mul | Lit Double
data VarName  = StateVar StateId

op2 :: Op -> Expr -> Expr -> Expr
op2 op x y = App op [x,y]

op1 :: Op -> Expr -> Expr
op1 op x = App op [x]

lit :: Double -> Expr
lit d = App (Lit d) []

instance Arith Expr where
  add = op2 Add
  mul = op2 Mul
  neg = op1 Neg

netSymStep :: PetriNet Expr -> State Expr
netSymStep net = netStep net (Map.mapWithKey arb (netIntialState net))
  where
  arb k _ = Var k


--------------------------------------------------------------------------------

netCompile :: String -> PetriNet Expr -> Doc
netCompile className net = vcat
  [ "struct" <+> text className <+> "{"
  , nest 2 (vcat [ stmt ("double" <+> varName x) | x <- vars ])
  , nest 2 (vcat [ constructor, "", stepFunction ])
  , "};"
  ]
  where
  vars         = Map.keys (netIntialState net)

  varName x     = hcat [ "s", int x ]
  varNameNext x = hcat [ "next_s", int x ]


  stmt x       = hcat [ x, semi ]
  wrapWhen b e = if b then parens e else e

  prec op      = case op of
                   Lit _ -> 0
                   Neg   -> 2
                   Mul   -> 3
                   Add   -> 4 :: Int

  expr outP e =
    case e of
      Var x -> varName x
      App op xs ->
        wrapWhen (outP < prec op)
        case (op, map (expr (prec op)) xs) of
          (Add,   ~[e1,e2]) -> e1 <+> "+" <+> e2
          (Mul,   ~[e1,e2]) -> e1 <+> "*" <+> e2
          (Neg,   ~[e1])    -> "-" PP.<> e1
          (Lit d, ~[])      -> double d

  constructor = (text className PP.<> "()")
             $$ nest 2 (vcat (zipWith initStmt
                                (colon : repeat comma)
                                (Map.toList (netIntialState net)))
                      $$ "{}"
                       )

  initStmt pref (x,e) = pref <+> varName x PP.<> parens (expr 16 e)

  stepFunction = "void step() {" $$ nest 2 stepFunBody $$ "}"

  stepFunBody =
    let es = Map.toList (netSymStep net)
    in vcat [ stmt ("double" <+> varNameNext x <+> "=" <+> expr 16 e)
                                                            | (x,e) <- es ]
      $$ vcat [ stmt (varName x <+> "=" <+> varNameNext x) | x <- vars ]

--------------------------------------------------------------------------------

example :: PetriNet Expr
example = PetriNet
  { netIntialState = mp [ (1, lit 23), (2, lit 73),  (3, lit 0) ]
  , netTransitions =
      [ Transition
          { transitionRate = \xs -> mul (xs Map.! 1) (xs Map.! 2)
          , transitionInputs  = mp [ (1, lit 0.5), (2, lit 2) ]
          , transitionOutputs = mp [ (3, lit 4) ]
          }
      ]
  }

  where
  mp = Map.fromList

