{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.APRAM.Translate where


import           Data.List  ( intercalate )
import           Data.Map   ( Map )
import qualified Data.Map   as Map
import           Data.Maybe ( mapMaybe )
import           Data.Set   ( Set )
import qualified Data.Set   as Set
import           Data.Text  ( Text, pack, unpack )
import qualified Data.Text  as Text

import           Language.ASKEE.APRAM.Syntax as APRAMSyntax
import           Language.ASKEE.APRAM.Sample ()
import           Language.ASKEE.Syntax as ESLSyntax
import qualified Language.ASKEE.Expr as Expr
import Language.ASKEE.ExprTransform (inlineLets)

modelToAPRAM :: Model -> String -> APRAM
modelToAPRAM m columnName = APRAM (floor totalPop) params statuses cohorts mods
  where
    Model{..} = m

    params = Map.fromList [ (unpack v, e) | (v, e) <- letDecls modelDecls ]

    statuses = Map.singleton columnName stateNames

    cohorts = pop:[ Cohort state (Is columnName state) | state <- stateNames ]
    pop = Cohort "Population" (foldr1 Or (map (Is columnName) stateNames))
    stateNames = [ unpack v | (v, _) <- stateDecls modelDecls ]

    mods = initialize : eventsToMods columnName modelEvents
    (totalPop, initialize) = initMod pop columnName modelDecls

initMod :: Cohort -> String -> [Decl] -> (Double, Mod)
initMod pop columnName decls = (totalPop, Mod "Initialize" pop actions "setup")
  where
    stateInitValues = [ (v, either (err v) id $ Expr.eval letBindings e) | (v, e) <- stateDecls decls ]
    err v e = error $ "when evaluating "<>unpack v<>", encountered error: "<>e
    letBindings = Map.fromList $ letDecls decls

    actions = map mkAct stateInitValues
    mkAct (v, d) = (Actions [Assign columnName (unpack v)], Expr.LitD d `Expr.Div` Expr.LitD totalPop)
    totalPop = sum [ d | (_, d) <- stateInitValues]

eventsToMods :: String -> [Event] -> [Mod]
eventsToMods columnName events = map eventToMod events
  where
    eventToMod :: Event -> Mod
    eventToMod Event{..} = Mod (unpack eventName) cohort [action, pass] "loop" 
      where
        cohort = Cohort oldStatus (Is columnName oldStatus)

        oldStatus = 
          case mapMaybe subtractOne eventEffect of
            [status] -> status
            _ -> error "this event did not subtract from exactly one state variable"

        newStatus = 
          case mapMaybe addOne eventEffect of
            [status] -> status
            _ -> error "this event did not add to exactly one state variable"


        action = (Actions [Assign columnName newStatus], probability)
        pass = (Pass, Expr.Sub (Expr.LitD 1) probability)

        probability = Expr.Div eventRate (foldr (Expr.Add . ESLSyntax.eventRate) (Expr.LitD 0) events)

    subtractOne :: Statement -> Maybe Status
    subtractOne (v, e) =
      case e of
        Expr.Sub (Expr.Var v') (Expr.LitD 1) | v == v' -> Just (unpack v')
        _ -> Nothing

    addOne :: Statement -> Maybe Status
    addOne (v, e) =
      case e of
        Expr.Add (Expr.Var v') (Expr.LitD 1) | v == v' -> Just (unpack v')
        _ -> Nothing

apramToModel :: APRAM -> Model
apramToModel APRAM{..} = Model "foo" stateDecs (concatMap modToEvents apramMods)
  where
    allStates :: Set State
    allStates = 
      ( Set.fromList 
      . map Map.fromList 
      . filter (\combo -> length combo == Map.size apramStatuses) --(== Map.size apramStatuses) . length) 
      . taggedCombos 
      . Map.toList 
      ) apramStatuses

    stateDecs :: [Decl]
    stateDecs = 
      [ State (mkStateName s) (Expr.Var "???")
      | s <- Set.toAscList allStates
      ]
  
    -- Generate every possible combination, Cartesian-product style, of `b`s, 
    -- propagating their `a` tags
    -- e.g. [(1, [2,3]), (2, [4])] -> [[(1,2),(2,4)],[(1,3),(2,4)]]
    taggedCombos :: [(a, [b])] -> [[(a, b)]]
    taggedCombos xs = filter ((== length xs) . length) (go xs)
      where
        go [] = [[]]
        go ((_, []):ls) = go ls
        go ((tag, h:t):ls) = map ((tag, h):) (go ls) ++ go ((tag, t):ls)

    -- | A cohort composed of logical operators on statuses, which are themselves
    -- trivial cohorts, admits a definition of the States (in ESL lingo) that 
    -- cohort touches
    relevantStates :: Cohort -> Set State
    relevantStates (Cohort _ cexpr) = go allStates
      where
        go :: Set State -> Set State
        go =
          case cexpr of
            Is  column status -> Set.filter (\s -> s Map.! column == status) --Set.member    status)
            Not column status -> Set.filter (\s -> s Map.! column /= status) --Set.notMember status)
            And c1 c2  -> \s -> foldr (Set.intersection . relevantStates) s         [Cohort undefined c1, Cohort undefined c2] -- TODO ugly
            Or  c1 c2  -> \_ -> foldr (Set.union        . relevantStates) Set.empty [Cohort undefined c1, Cohort undefined c2] -- TODO ugly

    modToEvents :: Mod -> [Event]
    modToEvents Mod{..} = [ mkEvent (modName, actions, probs, state) 
                          | (actions, probs) <- mapMaybe filterPass modActions
                          , state <- Set.toAscList $ relevantStates modCohort
                          ]

    filterPass :: (ActionSequence, Expr.Expr) -> Maybe ([Action], Expr.Expr)
    filterPass (as, ps) =
      case as of
        Actions as' -> Just (as', ps)
        Pass -> Nothing

    mkEvent :: (String, [Action], Expr.Expr, State) -> Event
    mkEvent (modname, actions, prob, state) = 
      Event (name<>"___"<>pack modname) when rate effect Nothing
      where
        name = Text.intercalate "_AND_" $ map (\a -> pack (show a) <> "_" <> stateName) actions
        when = Just $ Expr.Var stateName `Expr.GT` Expr.LitD 0
        rate = prob `Expr.Mul` scale
        effect = concatMap actionToStmts actions
        
        actionToStmts :: Action -> [Statement]
        actionToStmts act =
          let newStateName = mkStateName $ newState act state
          in  [ (stateName,    Expr.Var stateName    `Expr.Sub` Expr.LitD 1)
              , (newStateName, Expr.Var newStateName `Expr.Add` Expr.LitD 1)
              ]

        stateName = mkStateName state
        scale = Expr.Var stateName `Expr.Div` Expr.LitD (fromIntegral apramAgents)

-- Invariant: every APRAM column accounted for
type State = Map Column Status

-- | Given an action and a state, make the changes the action describes
-- to the state, if they apply
newState :: Action -> State -> State
newState (Assign column status) = Map.adjust (const status) column

-- Deterministic naming of States
mkStateName :: State -> Text
mkStateName = pack . intercalate "_" . Map.elems