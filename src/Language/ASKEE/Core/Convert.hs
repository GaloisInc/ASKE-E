{-# Language BlockArguments #-}
{-# Language OverloadedStrings #-}
module Language.ASKEE.Core.Convert where

import           Data.Foldable ( traverse_ )
import qualified Data.List     as List
import           Data.Map      ( Map )
import qualified Data.Map      as Map
import           Data.Maybe    ( mapMaybe )
import qualified Data.Set      as Set
import           Data.Text     ( Text, pack )

import           Language.ASKEE.Core.Expr
import qualified Language.ASKEE.Core.Expr         as Core
import           Language.ASKEE.Core.Syntax
import           Language.ASKEE.DEQ.Syntax        ( DiffEqs(..) )
import           Language.ASKEE.Gromet.Conversion
import           Language.ASKEE.Gromet.Syntax
import qualified Language.ASKEE.Gromet.Syntax     as Gromet
import           Language.ASKEE.Panic             ( panic )

-------------------------------------------------------------------------------
-- DiffEqs

asDiffEqs :: Model -> DiffEqs
asDiffEqs mdl =
  DiffEqs { deqParams  = modelParams mdl
          , deqInitial = modelInitState mdl
          , deqRates   = Map.mapWithKey stateEq (modelInitState mdl)
          , deqLets    = modelLets mdl
          }
  where
  stateEq sv _ = simplifyExpr
               $ foldr (:+:) (NumLit 0)
               $ mapMaybe (eventTerm sv)
               $ modelEvents mdl

-- Eventually we may want to consider approaches to try to make guard
-- continues (e.g., sigmoid?)
eventTerm :: Core.Ident -> Event -> Maybe Expr
eventTerm sv event =
  do stExp <- Map.lookup sv (eventEffect event)
     pure (If (eventWhen event)
              (eventRate event :*: (stExp :-: Var sv))
              (NumLit 0))

-------------------------------------------------------------------------------
-- Gromet

describeOp1 :: Op1 -> Text
describeOp1 op = pack (show op)

describeOp2 :: Op2 -> Text
describeOp2 op = pack (show op)

assocVarPort :: Gromet.Ident -> PortUid -> GrometGen ()
assocVarPort name port =
  modifyGrometGen (\s -> s { gcPortMap = Map.insert name port (gcPortMap s) })

expToGrometPort :: Expr -> GrometGen PortUid
expToGrometPort e0 =
  do  arg <- expToGromet e0
      case arg of
        ArgPort p -> pure p
        ArgLiteral _ ty -> mkOpFromInputs "const" ty [arg]

expToGromet :: Expr -> GrometGen Arg
expToGromet e0 =
  case e0 of
    Literal (Num n) -> pure $ ArgLiteral (pack $ show n) typeFloat
    Literal (Bool b) -> pure $ ArgLiteral (pack $ show b) typeBool
    Var name ->
      do  pid <- varPort name
          pure $ ArgPort pid

    Op1 o e ->
      ArgPort <$> op (describeOp1 o) (typeOf1 o) [e]

    Op2 o e1 e2 ->
      ArgPort <$> op (describeOp2 o) (typeOf2 o) [e1, e2]

    If tst thn els ->
      do  tst' <- expToGromet tst
          thn' <- expToGromet thn
          els' <- expToGromet els
          let args' = [tst', thn', els']

          boxId <- mkBoxOp' "if"
          connectArg boxId `traverse_` args'
          tyId <- typeOfArg thn'
          ArgPort <$> mkOutputPort boxId tyId Nothing

    Fail msg -> pure $ ArgLiteral ("exception: " <> pack msg) typeUndef
  where
    op opname tyId args =
      (expToGromet `traverse` args) >>= mkOpFromInputs opname tyId

    typeOfArg a =
      case a of
        ArgLiteral _ t -> pure t
        ArgPort p -> portType <$> getPort p

    typeOf1 o =
      case o of
        Not -> typeBool
        Neg -> typeFloat
        Exp -> typeFloat
        Log -> typeFloat

    typeOf2 o =
      case o of
        Add -> typeFloat
        Sub -> typeFloat
        Div -> typeFloat
        Mul -> typeFloat
        Lt -> typeBool
        Leq -> typeBool
        Eq -> typeBool
        And -> typeBool
        Or -> typeBool

withEnvBox :: Text -> [Gromet.Ident] -> (Uid -> GrometGen a) -> GrometGen a
withEnvBox label vars action =
  do  boxId <- mkBoxWires label
      ips <- concat <$> (wireInputPort boxId `traverse` List.nub vars)
      portMapScope (bind `traverse_` ips >> action boxId)
  where
    bind = uncurry assocVarPort
    wireInputPort boxId v =
      do  pid <- varPort v
          pid' <- connectIn boxId pid (Just v)
          pure [(v, pid')]

modelToGromet :: Model -> GrometGen ()
modelToGromet model =
  withEnvBox (modelName model) [] \boxId ->
    do  bindInput boxId `traverse_` modelStateVars model
        bindLet `traverse_` Map.toList (modelLets model)
        evtOutputs <- eventToGromet `traverse` modelEvents model
        outputPorts <- Map.fromList <$> outputPort boxId `traverse` modelStateVars model
        _ <- wireOutMap outputPorts `traverse` evtOutputs
        pure ()
  where
    wireOutMap portMap outMap =
      wireOut portMap `traverse` Map.toList outMap
    wireOut portMap (v, srcPId) =
      case Map.lookup v portMap of
        Nothing -> panic "modelToGromet" ["unexpected output in event"]
        Just destPId -> mkWire srcPId destPId

    bindLet (v, e) =
      do  expUid <- expToGrometPort e
          assocVarPort v expUid

    bindInput box v =
      do  p <- mkInputPort box typeFloat (Just v)
          assocVarPort v p

    outputPort boxId s =
      do  p <- mkOutputPort boxId typeFloat (Just s)
          pure (s, p)


eventToGromet :: Event -> GrometGen (Map Gromet.Ident PortUid)
eventToGromet evt =
  withEnvBox name inputs \boxId ->
    do  _ <- singleExpBox "rate" (eventRate evt) "rate"
        _ <- singleExpBox "when" (eventWhen evt) "when"
        outs <- eventEffectToGromet (eventEffect evt)
        wireOutputPortMap boxId outs
  where
    name = "Event " <> eventName evt
    inputs = allvars [evt]

singleExpBox :: Text -> Expr -> Text -> GrometGen PortUid
singleExpBox name expr out =
  withEnvBox name (allvars [expr]) \boxId ->
    do  pid <- expToGrometPort expr
        asOutput boxId pid (Just out)

eventEffectToGromet :: Map Gromet.Ident Expr -> GrometGen (Map Gromet.Ident PortUid)
eventEffectToGromet varMap =
  withEnvBox "effect" vars \boxId ->
    do  outMap <- expToGrometPort `traverse` varMap
        wireOutputPortMap boxId outMap
  where
    vars = allvars ( snd <$> Map.toList varMap)

allvars :: TraverseExprs t => [t] -> [Gromet.Ident]
allvars ts =
  Set.toList $ Set.unions (collectVars <$> ts)

wireOutputPortMap :: Uid -> Map Gromet.Ident PortUid -> GrometGen (Map Gromet.Ident PortUid)
wireOutputPortMap boxId pmap =
    Map.unions <$> (out `traverse` Map.toList pmap)
  where
    out (n,uid) =
      do  p <- asOutput boxId uid (Just n)
          pure $ Map.singleton n p

asGromet :: Model -> Gromet
asGromet mdl =
  runGrometGen (modelToGromet mdl >> getsGrometGen gcGromet) initial
  where
    initial =
      GrometConv { gcBoxes = Map.empty
                 , gcPorts = Map.empty
                 , gcWires = Map.empty
                 , gcCurUid = 1
                 , gcPortMap = Map.empty
                 }
