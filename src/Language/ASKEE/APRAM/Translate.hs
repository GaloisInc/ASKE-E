{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.APRAM.Translate where

import           Data.List (intercalate)
import           Data.Maybe (mapMaybe)
import qualified Data.Set  as Set
import           Data.Set  ( Set )
import           Data.Text (pack)

import           Language.ASKEE.APRAM.Syntax as APRAMSyntax
import           Language.ASKEE.APRAM.Sample
import           Language.ASKEE.Syntax as ESLSyntax
import qualified Language.ASKEE.Expr as Expr

apramToModel :: APRAM -> Model 
apramToModel APRAM{..} = Model "foo" [] (concatMap modToEvents apramMods)
  where
    modToEvents :: Mod -> [Event]
    modToEvents Mod{..} = [ mkEvent (modName, actions, probs, state) 
                          | (actions, probs) <- mapMaybe filterPass modActions
                          , state <- Set.toList $ relevantStates modCohort
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


type State = Set Status

newState :: Action -> State -> State
newState (Assign column (Status column' newTag)) = 
  if column == column'
    then Set.map change
    else id
  where
    change :: Status -> Status
    change s@(Status col oldTag) = 
      if col == column 
        then Status col newTag
        else s

mkStateName :: State -> String
mkStateName s = intercalate "_" (map show $ Set.toList s)

allStates :: [State]
allStates = [ Set.fromList [q, h] 
            | q <- quarantineStatuses
            , h <- healthStatuses
            ]

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

