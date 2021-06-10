{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.APRAM.Translate where

import Control.Monad.Identity (Identity (runIdentity))

import           Data.Either ( isLeft )
import           Data.Map   ( Map )
import qualified Data.Map   as Map
import           Data.Maybe ( mapMaybe )
import           Data.Set   ( Set )
import qualified Data.Set   as Set
import           Data.Text  ( Text, pack, unpack )
import qualified Data.Text  as Text

import           Language.ASKEE.APRAM.Syntax as APRAMSyntax
import           Language.ASKEE.APRAM.Sample ()
import qualified Language.ASKEE.Expr as Expr
import           Language.ASKEE.ExprTransform
import           Language.ASKEE.ESL.Syntax as ESLSyntax
import Language.ASKEE.Metadata

modelToAPRAM :: Model -> Text -> APRAM
modelToAPRAM m columnName = APRAM (floor totalPop) params statuses cohorts mods
  where
    Model{..} = inlineLets m (Map.keys params)

    params = Map.fromList 
      [ (v, e) 
      | (v, e) <- letDecls (ESLSyntax.modelDecls m)
      , not (involvesState e) ]

    involvesState e = isLeft (transformExpr go e)
      where
        go e' =
          case e' of
            Expr.Var v -> case v `lookup` stateDecls modelDecls of
              Just _ -> Left () 
              Nothing -> Right e'
            _ -> Right e'

    statuses = Map.singleton columnName stateNames

    cohorts = pop:[ Cohort state (Is columnName state) | state <- stateNames ]
    pop = Cohort "Population" All
    stateNames = [ v | (v, _) <- stateDecls modelDecls ]

    mods = initialize : eventsToMods columnName modelEvents
    (totalPop, initialize) = initMod pop columnName modelDecls


initMod :: Cohort -> Text -> [MetaAnn Decl] -> (Double, Mod)
initMod pop columnName decls = (totalPop, Mod "Initialize" pop actions "setup")
  where
    stateInitValues = [ (v, either (err v) id $ Expr.eval letBindings e) | (v, e) <- stateDecls decls ]
    err v e = error $ "when evaluating "<>unpack v<>", encountered error: "<>e
    letBindings = Map.fromList $ letDecls decls

    actions = map mkAct stateInitValues
    mkAct (v, d) = (Actions [Assign columnName v], Probability $ Expr.LitD d `Expr.Div` Expr.LitD totalPop)
    totalPop = sum [ d | (_, d) <- stateInitValues]

eventsToMods :: Text -> [Event] -> [Mod]
eventsToMods columnName = map eventToMod
  where
    eventToMod :: Event -> Mod
    eventToMod Event{..} = Mod eventName cohort [action, pass] "loop" 
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

        action =
          case eventWhen of
            Just w -> (Actions [Assign columnName newStatus], Rate (Expr.If w eventRate (Expr.LitD 0)))
            Nothing -> (Actions [Assign columnName newStatus], Rate eventRate)
        pass = (Pass, Unknown)

    subtractOne :: Statement -> Maybe Status
    subtractOne (v, e) =
      case e of
        Expr.Sub (Expr.Var v') (Expr.LitD 1) | v == v' -> Just v'
        _ -> Nothing

    addOne :: Statement -> Maybe Status
    addOne (v, e) =
      case e of
        Expr.Add (Expr.Var v') (Expr.LitD 1) | v == v' -> Just v'
        _ -> Nothing

-------------------------------------------------------------------------------

apramToModel :: APRAM -> Double -> Model
apramToModel APRAM{..} delta = Model (pure "foo") (letDecs ++ stateDecs) (concatMap modToEvents apramMods)
  where
    allStates :: Set State
    allStates = 
      ( Set.fromList 
      . map Map.fromList 
      . filter (\combo -> length combo == Map.size apramStatuses) --(== Map.size apramStatuses) . length) 
      . taggedCombos 
      . Map.toList 
      ) apramStatuses

    stateDecs :: [MetaAnn Decl]
    stateDecs = 
      [ pure $ State (mkStateName s) (Expr.Var "???")
      | s <- Set.toAscList allStates
      ]

    letDecs :: [MetaAnn Decl]
    letDecs =
      [ pure $ Let t e
      | (t, e) <- Map.toList apramParams
      ] ++
      [ pure $ Let "delta" (Expr.LitD delta)
      , pure $ Let "size" (Expr.LitD $ fromIntegral apramAgents) ]
  
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
            Is  column status -> Set.filter (\s -> s Map.! column == status)
            Not column status -> Set.filter (\s -> s Map.! column /= status)
            And c1 c2  -> \s -> foldr (Set.intersection . relevantStates) s         [Cohort "" c1, Cohort "" c2]
            Or  c1 c2  -> \_ -> foldr (Set.union        . relevantStates) Set.empty [Cohort "" c1, Cohort "" c2]
            All -> const allStates

    modToEvents :: Mod -> [Event]
    modToEvents Mod{..}
      | modPhase == "setup" = []
      | otherwise = [ mkEvent (modName, thisAction, asRate thisProbSpec passProbSpec, state) 
                    | (thisAction, thisProbSpec) <- mapMaybe getActions modActions
                    , state <- Set.toAscList $ relevantStates modCohort
                    ]
      where
        passProbSpec :: ProbSpec
        passProbSpec =
          case mapMaybe (\case (Pass, p) -> Just p; _ -> Nothing) modActions of
            [p] -> p
            x -> error $ "in mod "<>unpack modName<>", there was not exactly one specified inaction path "<>show x

    getActions :: (ActionSequence, ProbSpec) -> Maybe ([Action], ProbSpec)
    getActions (as, ps) =
      case as of
        Actions as' -> Just (as', ps)
        Pass -> Nothing

    asRate :: ProbSpec -> ProbSpec -> Expr.Expr
    asRate thisProbSpec passProbSpec =
      case (thisProbSpec, passProbSpec) of
        (Rate r, _) -> r
        (Probability tp, Probability pp) -> Expr.Neg ((tp `Expr.Mul` Expr.Log pp) `Expr.Div` (Expr.Var "delta" `Expr.Mul` (Expr.LitD 1 `Expr.Sub` pp)))
        _ -> undefined

    mkEvent :: (Text, [Action], Expr.Expr, State) -> Event
    mkEvent (modname, actions, prob, state) = 
      Event (name<>"___"<>modname) when rate effect Nothing
      where
        name = Text.intercalate "_AND_" $ map (\a -> pack (show a) <> "_" <> stateName) actions
        when = Just $ Expr.Var stateName `Expr.GT` Expr.LitD 0
        rate = runIdentity $ transformExpr go $ prob `Expr.Mul` scale
        effect = concatMap actionToStmts actions

        go :: Expr.Expr -> Identity Expr.Expr
        go e =
          case e of
            Expr.Var v ->
              case filter (\(Cohort n _) -> n == v) apramCohorts of
                [cohort] -> pure $ foldr1 Expr.Add (map (Expr.Var . mkStateName) (Set.toList $ relevantStates cohort))
                _ -> pure e
            _ -> pure e

        
        actionToStmts :: Action -> [Statement]
        actionToStmts act =
          let newStateName = mkStateName $ newState act state
          in  [ (stateName,    Expr.Var stateName    `Expr.Sub` Expr.LitD 1)
              , (newStateName, Expr.Var newStateName `Expr.Add` Expr.LitD 1)
              ]

        stateName = mkStateName state
        scale = Expr.Var stateName `Expr.Div` Expr.Var "size"

-- Invariant: every APRAM column accounted for
type State = Map Column Status

-- | Given an action and a state, make the changes the action describes
-- to the state, if they apply
newState :: Action -> State -> State
newState (Assign column status) = Map.adjust (const status) column

-- Deterministic naming of States
mkStateName :: State -> Text
mkStateName = Text.intercalate "_" . Map.elems