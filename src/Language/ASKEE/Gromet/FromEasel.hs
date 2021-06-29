{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
module Language.ASKEE.Gromet.FromEasel where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Control.Monad.State as State
import Control.Monad(forM,forM_,foldM)
import Data.Maybe(maybeToList)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.ASKEE.Panic(panic)
import qualified Language.ASKEE.Core.Syntax as Core
import qualified Language.ASKEE.Core.Expr as Core

import Language.ASKEE.Gromet.Common(Uid)
import Language.ASKEE.Gromet.Syntax


--------------------------------------------------------------------------------
-- Conversion to Gromet

convertCoreToGromet :: Core.Model -> Gromet
convertCoreToGromet m =
  Gromet { grometName  = Core.modelName m
         , grometRoot  = rootUID
         , grometPorts = gcPorts finS
         , grometJunctions = gcJunctions finS
         , grometWires = gcWires finS
         , grometBoxes = gcBoxes finS
         }
  where
  (rootUID,finS) = State.runState (convertModel m) initState

  initState = GrometConv { gcBoxes      = []
                         , gcWires      = []
                         , gcJunctions  = []
                         , gcPorts      = []
                         , gcCurUid     = 0
                         , gcBoxStack   = []
                         }


convertModel :: Core.Model -> GrometGen BoxUid
convertModel model =
  do uid <- newBoxUid
     let mparams = Map.toList (Core.modelParams model)
     paramPorts <- forM [ x | (x,Nothing) <- mparams ] \paramName ->
        do puid <- newPortUid
           let p = Port { portUid       = puid
                        , portBox       = uid
                        , portType      = ParameterPort
                        , portValueType = Real
                        , portName      = paramName
                        }
           addPort p
           pure (paramName, Left p)

     let vars = Core.modelInitState model
         stateLets = Core.orderDecls $ Map.toList (Core.modelLets model)
         pureLets  = Core.orderDecls [ (x,e) | (x,Just e) <- mparams ]

     startNestedBox uid (Core.modelName model) PrTNet

     ports1 <- foldM doLet (Map.fromList paramPorts) pureLets

     let addJ ps s@(x,_) = do j <- convertState ports1 s
                              pure (Map.insert x (Right j) ps)
     ports2 <- foldM addJ ports1 (Map.toList vars)

     ports3 <- foldM doLet ports2 stateLets

     mapM_ (convertEvent ports3) (Core.modelEvents model)
     endNestedBox [ portUid x | (_,Left x) <- paramPorts ]

     pure uid

  where
  doLet ports (x,e) =
    do uid    <- newBoxUid
       lports <- fmap portUid <$>
                 connectInputPorts ports uid (Core.collectExprVars e)
       out    <- newPortUid
       let outP = Port { portUid = out
                       , portBox = uid
                       , portType = OutputPort
                       , portValueType = Real -- XXX: infer from expr
                       , portName = x
                       }
       addPort outP
       addBox Box { boxUid    = uid
                  , boxName   = x
                  , boxSyntax = BoxExpression (exprToArg lports e)
                  , boxPorts  = out : Map.elems lports
                  , boxBoxes  = []
                  , boxWires  = []
                  , boxJuncitons = []
                  }

       pure (Map.insert x (Left outP) ports)


convertState ::
  Map Core.Ident (Either Port Junction) ->
  (Core.Ident, Core.Expr) -> GrometGen Junction
convertState inPorts (v,e) =
  do jid <- newJunctionUid
     let ju = Junction { jUID = jid
                       , jName = v
                       , jValueType = Real  -- XXX: infer from Expr?
                       }
     addJunction ju
     convertExpr ("Initial value for " <> v) inPorts (Just (Right ju)) e
     pure ju


convertEvent ::
  Map Core.Ident (Either Port Junction) -> Core.Event -> GrometGen ()
convertEvent inPorts ev =
  do evBoxUid <- newBoxUid
     iPorts <- connectInputPorts inPorts evBoxUid allVars
     oPorts <- connectOutputPorts inPorts evBoxUid outVs

     startNestedBox evBoxUid (Core.eventName ev) EventRel
     makeWhen (Left <$> iPorts)
     makeRate (Left <$> iPorts)
     makeEffect (Left <$> iPorts) (Left <$> iPorts)

     endNestedBox (getPorts oPorts ++ getPorts iPorts)
  where
  getPorts   = map portUid . Map.elems

  allVars    = Set.unions [ enableVars, rateVars, effVars ]
  enableVars = Core.collectExprVars (Core.eventWhen ev)
  rateVars   = Core.collectExprVars (Core.eventRate ev)
  effVars    = Set.unions (Core.collectExprVars <$>
                                    Map.elems (Core.eventEffect ev))
  outVs      = Map.keysSet (Core.eventEffect ev)

  makeWhen evPorts =
    do uid   <- newBoxUid
       let name = "Enabling Condition"
       ports <- connectInputPorts evPorts uid enableVars
       startNestedBox uid name EnableRel
       convertExpr name (Left <$> ports) Nothing (Core.eventWhen ev)
       endNestedBox (map portUid (Map.elems ports))

  makeRate evPorts =
    do uid   <- newBoxUid
       let name = "Event Rate"
       ports <- connectInputPorts evPorts uid rateVars
       startNestedBox uid name RateRel
       convertExpr name (Left <$> ports) Nothing (Core.eventRate ev)
       endNestedBox (map portUid (Map.elems ports))

  makeEffect evPorts oPorts =
    do uid <- newBoxUid
       iports <- connectInputPorts evPorts uid effVars
       oports <- connectOutputPorts oPorts uid outVs
       let name = "Event Effect"
       startNestedBox uid name EffectRel
       forM_ (Map.toList (Core.eventEffect ev)) \(x,e) ->
          case Map.lookup x oports of
            Just p -> convertExpr name (Left <$> iports) (Just (Left p)) e
            Nothing -> panic "makeEffect" ["Missing outp port for ", show x]
       endNestedBox (getPorts oports ++ getPorts iports)



-- | Make ports on a nested box and connect them to the corresponding ports
-- in the current box.
connectInputPorts ::
  Map Core.Ident (Either Port Junction) {- ^ Ports in outer box -} ->
  BoxUid              {- ^ Uid of the new box -} ->
  Set Core.Ident      {- ^ Variables that need ports -} ->
  GrometGen (Map Core.Ident Port)
connectInputPorts inPorts uid vars =
  Map.fromList <$>
  forM (Set.toList vars) \x ->
     case Map.lookup x inPorts of
       Just p -> do p1 <- freshInputPort' uid p
                    pure (x,p1)
       Nothing -> panic "connectInputPorts" $ [ "Unknown variable"
                                            , show x
                                            , "in scope"
                                            ] ++ map show (Map.toList inPorts)



-- | Make ports on a nested box and connect them to the corresponding ports
-- in the current box.
connectOutputPorts ::
  Map Core.Ident (Either Port Junction) {- ^ Ports in outer box -} ->
  BoxUid              {- ^ Uid of the new box -} ->
  Set Core.Ident      {- ^ Variables that need ports -} ->
  GrometGen (Map Core.Ident Port)
connectOutputPorts inPorts uid vars =
  Map.fromList <$>
  forM (Set.toList vars) \x ->
     case Map.lookup x inPorts of
       Just p -> do p1 <- freshOutputPort' uid p
                    pure (x,p1)
       Nothing -> panic "connectOutputPorts" $
                            [ "Unknown variable"
                            , show x
                            , "in scope"
                            ] ++ map show (Map.toList inPorts)




convertExpr ::
  Text                {- ^ Name for the box -} ->
  Map Core.Ident (Either Port Junction) {- ^ Ports in outer box -} ->
  Maybe (Either Port Junction) {- ^ Output port in outer box, if any -} ->
  Core.Expr           {- ^ Definition -} ->
  GrometGen ()
convertExpr name inPorts mbOutPort expr =
  do uid    <- newBoxUid
     varMap <- fmap portUid <$>
               connectInputPorts inPorts uid (Core.collectExprVars expr)
     ourOut <- traverse (freshOutputPort uid) mbOutPort

     addBox Box { boxUid    = uid
                , boxName   = name
                , boxSyntax = BoxExpression (exprToArg varMap expr)
                , boxPorts  = maybeToList ourOut ++ Map.elems varMap
                , boxBoxes  = []
                , boxWires  = []
                , boxJuncitons = []
                }


exprToArg :: Map Core.Ident PortUid -> Core.Expr -> Arg
exprToArg vars expr =
  case expr of

    Core.Literal l ->
      case l of
        Core.Num x  -> ArgLiteral (LitReal x)
        Core.Bool x -> ArgLiteral (LitBool x)

    Core.Var x ->
      case Map.lookup x vars of
        Just p       -> ArgPort p
        Nothing      -> error ("[bug] Variable " ++ show x ++ " not in scope.")

    Core.Op1 f e ->
      let op x = doOp x [e]
      in case f of
           Core.Not     -> op "not"
           Core.Neg     -> op "-"
           Core.Exp     -> op "exp"
           Core.Log     -> op "log"

    -- XXX: we could turn binary to N-ary operators?
    Core.Op2 f e1 e2 ->
      let op x = doOp x [e1,e2]
      in case f of
           Core.Add     -> op "+"
           Core.Mul     -> op "*"
           Core.Sub     -> op "-"
           Core.Div     -> op "/"
           Core.Lt      -> op "lt"
           Core.Leq     -> op "leq"
           Core.Eq      -> op "=="
           Core.And     -> op "and"
           Core.Or      -> op "or"

    Core.If e1 e2 e3  -> doOp "if" [e1,e2,e3]
    Core.Fail _       -> doOp "error" []

  where
  doOp op es = ArgCall op (exprToArg vars <$> es)






--------------------------------------------------------------------------------
-- Monad for conversion to Gromet

data GrometConv =
  GrometConv { gcBoxes   :: [Box]   -- ^ Finished boxes
             , gcWires   :: [Wire]  -- ^ Finished wires
             , gcPorts   :: [Port]  -- ^ Finished ports
             , gcJunctions :: [Junction] -- ^ Finished junctions
             , gcCurUid  :: Int     -- ^ Uid generation

             , gcBoxStack :: [Box]
             }

type GrometGen = State.State GrometConv



-- | Utility to show a value as "Text"
showText :: Show a => a -> Text
showText = Text.pack . show

-- | Generate a fresh name
newUid :: Text -> GrometGen Uid
newUid pref =
  do s <- State.get
     let n = gcCurUid s
     State.put s { gcCurUid = n + 1 }
     pure (pref <> showText n)

newBoxUid :: GrometGen BoxUid
newBoxUid = BoxUid <$> newUid "B:"

newPortUid :: GrometGen PortUid
newPortUid = PortUid <$> newUid "P:"

newWireUid :: GrometGen WireUid
newWireUid = WireUid <$> newUid "W:"

newJunctionUid :: GrometGen JunctionUid
newJunctionUid = JunctionUid <$> newUid "J:"

-- | Add a port to the list of declaraitons.
addPort :: Port -> GrometGen ()
addPort p = State.modify \s -> s { gcPorts = p : gcPorts s }

-- | Add a wire to the list of declarations.
-- Also record it in the current "under construction" box
addWire :: Wire -> GrometGen ()
addWire w = State.modify \s ->
  s { gcWires = w : gcWires s
    , gcBoxStack = case gcBoxStack s of
                     [] -> []
                     b : bs -> b { boxWires = wireUid w : boxWires b } : bs
    }

addJunction :: Junction -> GrometGen ()
addJunction j = State.modify \s ->
  s { gcJunctions = j : gcJunctions s
    , gcBoxStack = case gcBoxStack s of
                     [] -> []
                     b : bs -> b { boxJuncitons = jUID j : boxJuncitons b } : bs
    }

-- | Add a box to the box on tope of the stack
-- Also record it in the current "under construction" box
addBox :: Box -> GrometGen ()
addBox b = State.modify \s ->
  s { gcBoxes = b : gcBoxes s
    , gcBoxStack = case gcBoxStack s of
                     [] -> []
                     bo : bs -> bo { boxBoxes = boxUid b : boxBoxes bo } : bs
    }

-- | Start a new "under construction" box
pushBox :: Box -> GrometGen ()
pushBox b = State.modify \s ->
  s { gcBoxStack = b : gcBoxStack s }

-- | Finish the construction of a box
popBox :: GrometGen Box
popBox =
  do s <- State.get
     case gcBoxStack s of
       b : bs -> do State.put s { gcBoxStack = bs }
                    pure b
       [] -> panic "popBox" ["No box to pop"]


startNestedBox :: BoxUid -> Text -> BoxRelType -> GrometGen ()
startNestedBox uid name ty =
  pushBox Box { boxUid = uid
              , boxName = name
              , boxSyntax = BoxRelation ty
              , boxPorts = []
              , boxBoxes = []
              , boxWires = []
              , boxJuncitons = []
              }

endNestedBox :: [PortUid] -> GrometGen ()
endNestedBox ps =
  do box <- popBox
     addBox box { boxPorts = ps }


-- | Generate a fresh input port linking to the given input
-- port, and also a wire connecting the two.
freshInputPort' :: BoxUid -> Either Port Junction -> GrometGen Port
freshInputPort' boxid p' =
  do puid <- newPortUid
     let newPort = case p' of
                     Left p  -> p { portUid  = puid
                                  , portBox = boxid
                                  , portType = InputPort
                                  }
                     Right j -> Port { portUid       = puid
                                     , portBox       = boxid
                                     , portType      = InputPort
                                     , portValueType = jValueType j
                                     , portName      = jName j
                                     }
     addPort newPort
     wuid <- newWireUid
     addWire Wire { wireUid       = wuid
                  , wireType      = Directed
                  , wireValueType = portValueType newPort
                  , wireSource    = case p' of
                                      Left p  -> PPort (portUid p)
                                      Right j -> JPort (jUID j)
                  , wireTarget    = PPort puid
                  }
     pure newPort

-- | Same as "freshInputPort'" but returns only the port UID
freshInputPort :: BoxUid -> Either Port Junction -> GrometGen PortUid
freshInputPort boxid p = portUid <$> freshInputPort' boxid p

-- |Same as "freshInputPort" but for output ports (i.e., in the other direction)
freshOutputPort' :: BoxUid -> Either Port Junction -> GrometGen Port
freshOutputPort' boxid p' =
  do puid <- newPortUid
     let newPort = case p' of
                     Left p  -> p { portUid  = puid
                                  , portBox  = boxid
                                  , portType = OutputPort
                                  }
                     Right j -> Port { portUid       = puid
                                     , portBox       = boxid
                                     , portType      = OutputPort
                                     , portValueType = jValueType j
                                     , portName      = jName j
                                     }
     addPort newPort

     wuid <- newWireUid
     addWire Wire { wireUid       = wuid
                  , wireType      = Directed
                  , wireValueType = portValueType newPort
                  , wireSource    = PPort puid
                  , wireTarget    = case p' of
                                      Left p  -> PPort (portUid p)
                                      Right j -> JPort (jUID j)
                  }
     pure newPort

freshOutputPort :: BoxUid -> Either Port Junction -> GrometGen PortUid
freshOutputPort boxid p = portUid <$> freshOutputPort' boxid p


