{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.APRAM.Translate where


import           Data.List (intercalate, nub)
import           Data.Maybe (mapMaybe)
import qualified Data.Set  as Set
import           Data.Set  ( Set )
import           Data.Text (pack)

import           Language.ASKEE.APRAM.Syntax as APRAMSyntax
import           Language.ASKEE.APRAM.Sample
import           Language.ASKEE.Syntax as ESLSyntax
import qualified Language.ASKEE.Expr as Expr

apramToModel :: APRAM -> Model 
apramToModel APRAM{..} = Model "foo" (nub $ concatMap cohortToStates apramCohorts) (concatMap modToEvents apramMods)
  where
    cohortToStates :: Cohort -> [Decl]
    cohortToStates c = [ State (pack $ mkStateName s) (Expr.Var "???")
                       | s <- Set.elems $ relevantStates c]


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
    mkEvent (modname, actions, prob, state) = Event (pack name<>"___"<>pack modname) when rate effect Nothing
      where
        name = intercalate "_AND_" $ map (\a -> show a <> "_" <> stateName) actions
        when = Just $ stateVar `Expr.GT` Expr.LitD 0
        rate = prob `Expr.Mul` scale
        effect = concatMap actionsFromEffect actions
        
        actionsFromEffect :: Action -> [Statement]
        actionsFromEffect act =
          let newName = mkStateName $ newState act state
              newVar = Expr.Var $ pack newName
          in  [ (pack stateName, stateVar `Expr.Sub` Expr.LitD 1)
              , (pack newName, newVar `Expr.Add` Expr.LitD 1)
              ]

        scale = stateVar `Expr.Div` Expr.LitD (fromIntegral apramAgents)

        stateName = mkStateName state
        stateVar = Expr.Var $ pack stateName


-- In this formalism, an ESL State is a unique set of one status
-- from each APRAM column - e.g. fromList [infectedStatus, quarantinedStatus]
-- or fromList [susceptibleStatus, notQuarantinedStatus] but not fromList
-- [susceptibleStatus, exposedStatus] because the two, both being members of
-- the "health" column, are mutually exclusive
type State = Set Status

-- | Given an action and a state, make the changes the action describes
-- to the state, if they apply
newState :: Action -> State -> State
newState (Assign column (Status column' newTag)) = 
  if column == column'
    then Set.map change
    else id
  where
    change :: Status -> Status
    change s@(Status col _) = 
      if col == column 
        then Status col newTag
        else s

-- Deterministic naming of States
mkStateName :: State -> String
mkStateName s = intercalate "_" (map show $ Set.toAscList s)

-- Testing
allStates :: [State]
allStates = [ Set.fromList [q, h] 
            | q <- quarantineStatuses
            , h <- healthStatuses
            ]

-- | A cohort composed of logical operators on statuses, which are themselves
-- trivial cohorts, admits a definition of the States (in ESL lingo) that 
-- cohort touches
relevantStates :: Cohort -> Set State
relevantStates c = go (Set.fromList allStates)
  where
    go :: Set State -> Set State
    go =
      case c of
        Is  status -> Set.filter (Set.member    status)
        Not status -> Set.filter (Set.notMember status)
        And c1 c2  -> \s -> foldr (Set.intersection . relevantStates) s         [c1, c2]
        Or  c1 c2  -> \_ -> foldr (Set.union        . relevantStates) Set.empty [c1, c2]

