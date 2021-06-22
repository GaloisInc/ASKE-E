{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ASKEE ( tests ) where

import qualified Data.FileEmbed as Embed
import qualified Data.Map       as Map
import           Data.Text      ( Text )
import           Language.ASKEE
import qualified Language.ASKEE.Core.Syntax        as Core
import qualified Language.ASKEE.Core.Expr          as CExp
import qualified Language.ASKEE.Core.Visualization as Viz

import qualified Test.Tasty       as Tasty
import           Test.Tasty.HUnit ( Assertion
                                  , assertBool
                                  , (@=?)
                                  , assertFailure
                                  , testCase )

sir :: Text
sir = $(Embed.embedStringFile "modelRepo/easel/sir.easel")

series1 :: DataSeries Double
series1 =
  DataSeries { times = [0,30,60,90,120]
             , values = Map.fromList
                [ ("I",[3,570.9758710681082,177.87795797377797,53.663601453388395,16.17524903479719])
                , ("S",[997,16.03663576555767,0.2688016239687885,7.747202089688689e-2,5.323898868597058e-2])
                , ("R",[0,412.9874931663346,821.8532404022534,946.258926525715,983.771511976517])
                ]
             }

testSimulateEsl :: DataSource -> DataSeries Double -> Assertion
testSimulateEsl mdlSrc expected =
  do  (start, step, stop) <- asRange (times expected)
      actual <- simulateModelGSL EaselType mdlSrc start stop step Map.empty
      assertDataClose actual expected

asRange :: [Double] -> IO (Double, Double, Double)
asRange expected =
  case expected of
    [] -> assertFailure "bad test: expected data series has no times!"
    [e] -> pure (e, 1, e)
    e1:e2:_ ->
      do  expected @=? take (length expected) [e1, e2 ..]
          pure (e1, e2 - e1, last expected)

assertDataClose :: DataSeries Double -> DataSeries Double -> Assertion
assertDataClose actual expected =
  do  assertBool "Data series times are not close"
                 (and (zipWith isClose (times actual) (times expected)))

      Map.keysSet (values expected) @=? Map.keysSet (values actual)

      -- TODO: maybe slightly better error messages
      assertBool  "Series data is not close"
                  (and (zipWith compareVars (asc actual) (asc expected)))

  where
    isClose n1 n2 = abs (n1 - n2) < 0.00000001
    compareVars (_, as) (_, bs) = and $ zipWith isClose as bs
    asc = Map.toAscList . values

-------------------------------------------------------------------------------
-- asSchematicGraph

-- coreModelGraphs :: [(Core.Model, Viz.Graph)]
s2s, s2e, e2s, e2e, n2n, s2sd :: (Core.Model, Viz.Graph)
(s2s, s2e, e2s, e2e, n2n, s2sd) =
  ( (stateToState, stateToStateGraph)
  , (stateToEvent, stateToEventGraph)
  , (eventToState, eventToStateGraph)
  , (eventToEvent, eventToEventGraph)
  , (noneToNone, noneToNoneGraph)
  , (stateToStateWithDups, stateToStateWithDupsGraph)
  )
  where
    -- E1 subtracts from X and adds to Y
    stateToStateGraph = Viz.Graph [(xNode, e1Node), (e1Node, yNode)]
    stateToState = Core.Model{..}
      where
        modelInitState = Map.fromList [xInit, yInit]
        modelEvents = [event "E1" [xSub1, yAdd1]]

    -- Two events named E1 subtract from X and add to Y
    stateToStateWithDupsGraph = stateToStateGraph
    stateToStateWithDups = Core.Model{..}
      where
        modelInitState = Map.fromList [xInit, yInit]
        modelEvents = replicate 2 (event "E1" [xSub1, yAdd1])

    -- E1 subtracts from X
    stateToEventGraph = Viz.Graph [(xNode, e1Node)]
    stateToEvent = Core.Model{..}
      where
        modelInitState = Map.fromList [xInit]
        modelEvents = [event "E1" [xSub1]]

    -- E1 adds to Y
    eventToStateGraph = Viz.Graph [(e1Node, yNode)]
    eventToState = Core.Model{..}
      where
        modelInitState = Map.fromList [yInit]
        modelEvents = [event "E1" [yAdd1]]

    -- E1 adds to X, E2 subtracts from X
    eventToEventGraph = Viz.Graph [(e1Node, xNode), (xNode, e2Node)]
    eventToEvent = Core.Model{..}
      where
        modelInitState = Map.fromList [xInit]
        modelEvents = [ event "E1" [xAdd1]
                      , event "E2" [xSub1]]

    -- No actions
    noneToNoneGraph = Viz.Graph []
    noneToNone = Core.Model{..}
      where
        modelInitState = mempty
        modelEvents = mempty
    
    -- Common parameters
    modelName = "foo"
    modelParams = mempty
    modelLets = mempty

    -- Schematic graphs don't care about event rates and enabling predicates
    event name eff = 
      Core.Event
        { Core.eventName = name
        , Core.eventRate = CExp.NumLit 0
        , Core.eventWhen = CExp.BoolLit False
        , Core.eventEffect = Map.fromList eff
        }

    xInit = ("X", CExp.NumLit 10)
    yInit = ("Y", CExp.NumLit 0)

    xSub1 = ("X", CExp.Var "X" CExp.:-: CExp.NumLit 1)
    xAdd1 = ("X", CExp.Var "X" CExp.:+: CExp.NumLit 1)
    yAdd1 = ("Y", CExp.Var "Y" CExp.:+: CExp.NumLit 1)

    xNode = Viz.Node "X" Viz.State
    e1Node = Viz.Node "E1" Viz.Event
    e2Node = Viz.Node "E2" Viz.Event
    yNode = Viz.Node "Y" Viz.State


testAsSchematicGraph :: (Core.Model, Viz.Graph) -> Assertion
testAsSchematicGraph (model, graph) =
  Just graph @=? asSchematicGraph model

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "ASKEE API Tests"
    [ testCase "Basic SIR simulation test" $ testSimulateEsl (Inline sir) series1
    , testCase "State-to-state flow schematic" $ testAsSchematicGraph s2s
    , testCase "State-to-event flow schematic" $ testAsSchematicGraph s2e
    , testCase "Event-to-state flow schematic" $ testAsSchematicGraph e2s
    , testCase "Event-to-event flow schematic" $ testAsSchematicGraph e2e
    , testCase "No-flow schematic" $ testAsSchematicGraph n2n
    , testCase "State-to-state flow schematic, no dups" $ testAsSchematicGraph s2sd
    ]