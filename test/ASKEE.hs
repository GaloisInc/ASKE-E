{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ASKEE ( tests ) where

import qualified Data.FileEmbed as Embed
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import           Data.Text      ( Text, pack )

import           Language.ASKEE
import qualified Language.ASKEE.Core.Syntax        as Core
import qualified Language.ASKEE.Core.Expr          as CExp
import qualified Language.ASKEE.Core.Visualization as Viz
import           Language.ASKEE.Model.Basics ( ValueType(..) )

import qualified Test.Tasty       as Tasty
import           Test.Tasty.HUnit ( Assertion
                                  , assertBool
                                  , (@=?)
                                  , assertFailure
                                  , testCase
                                  , assertEqual )

sir :: Text
sir = $(Embed.embedStringFile "modelRepo/easel/sir.easel")

sirEquations :: Text
sirEquations = $(Embed.embedStringFile "modelRepo/deq/sir.deq")

sirSansParameters :: Text
sirSansParameters = $(Embed.embedStringFile "modelRepo/easel/sir-no-parameters.easel")

seirPNC :: Text
seirPNC = $(Embed.embedStringFile "modelRepo/gromet-pnc/seir.json")

-- Generated via GSL simulation
series1 :: DataSeries Double
series1 =
  DataSeries { times = [0,30,60,90,120]
             , values = Map.fromList
                [ ("I",[3,570.9758710681082,177.87795797377797,53.663601453388395,16.17524903479719])
                , ("S",[997,16.03663576555767,0.2688016239687885,7.747202089688689e-2,5.323898868597058e-2])
                , ("R",[0,412.9874931663346,821.8532404022534,946.258926525715,983.771511976517])
                , ("total_population",[1000,1000,1000,1000])
                ]
             }

-- Generated via GSL simulation with beta=0.5
series2 :: DataSeries Double
series2 =
  DataSeries { times = [0,30,60,90,120]
             , values = Map.fromList
                [ ("I",[3,504.70103993445787,152.77443346245198,46.02318509849733,13.86300422788101])
                , ("S",[997,2.0953177959873206,2.5090771953370807e-2,6.605341099589403e-3,4.418715256915984e-3])
                , ("R",[0,493.2036422695545,847.2004757655942,953.9702095604024,986.1325770568615])
                , ("total_population",[1000,1000,1000,1000])
                ]
             }

-- Generated via discrete event simulation using seed 123 at times [0,30..120]
series3 :: DataSeries Double
series3 =
  DataSeries { times = [0.0,30.0428,60.2816,90.7133]
             , values = Map.fromList
                [ ("I",[3.0,604.0,188.0,70.0])
                , ("R",[0.0,361.0,812.0,930.0])
                , ("S",[997.0,35.0,0.0,0.0])
                , ("total_population",[1000,1000,1000,1000])
                ]
             }

testSimulateEsl :: DataSource -> DataSeries Double -> Assertion
testSimulateEsl mdlSrc expected =
  do  (start, step, stop) <- asRange (times expected)
      actual <- simulateModelGSL EaselType mdlSrc start stop step Map.empty mempty
      assertDataClose actual expected

testSimulateEslParameterized :: DataSource -> DataSeries Double -> Assertion
testSimulateEslParameterized mdlSrc expected =
  do  (start, step, stop) <- asRange (times expected)
      actual <- simulateModelGSL EaselType mdlSrc start stop step (Map.singleton "beta" 0.5) mempty
      assertDataClose actual expected

testSimulateEslDiscrete :: DataSource -> Double -> Double -> Double -> DataSeries Double -> Assertion
testSimulateEslDiscrete mdlSrc start stop step expected =
  do  actual <- simulateModelDiscrete EaselType mdlSrc start stop step (Just 123)
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
        modelMeta = mempty

    -- Two events named E1 subtract from X and add to Y
    stateToStateWithDupsGraph = stateToStateGraph
    stateToStateWithDups = Core.Model{..}
      where
        modelInitState = Map.fromList [xInit, yInit]
        modelEvents = replicate 2 (event "E1" [xSub1, yAdd1])
        modelMeta = mempty

    -- E1 subtracts from X
    stateToEventGraph = Viz.Graph [(xNode, e1Node)]
    stateToEvent = Core.Model{..}
      where
        modelInitState = Map.fromList [xInit]
        modelEvents = [event "E1" [xSub1]]
        modelMeta = mempty

    -- E1 adds to Y
    eventToStateGraph = Viz.Graph [(e1Node, yNode)]
    eventToState = Core.Model{..}
      where
        modelInitState = Map.fromList [yInit]
        modelEvents = [event "E1" [yAdd1]]
        modelMeta = mempty

    -- E1 adds to X, E2 subtracts from X
    eventToEventGraph = Viz.Graph [(e1Node, xNode), (xNode, e2Node)]
    eventToEvent = Core.Model{..}
      where
        modelInitState = Map.fromList [xInit]
        modelEvents = [ event "E1" [xAdd1]
                      , event "E2" [xSub1]]
        modelMeta = mempty

    -- No actions
    noneToNoneGraph = Viz.Graph []
    noneToNone = Core.Model{..}
      where
        modelInitState = mempty
        modelEvents = mempty
        modelMeta = mempty

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

testConvertESLToDEQ :: Assertion
testConvertESLToDEQ =
  do  result <- convertModelString EaselType (Inline sir) DeqType
      case result of
        Right r -> assertEqual "" sirEquations (pack r)
        Left err -> assertFailure err

testDescribeInterface :: Assertion
testDescribeInterface =
  do  seir <- loadModel GrometPncType (Inline seirPNC)
      let expected = 
            ModelInterface 
            { modelInputs = 
              [ Port {portName = "J:E_init", portValueType = Integer, portDefault = Nothing, portMeta = Map.fromList [("group",["Initial State"]),("name",["E"]),("type",["Integer"])]}
              , Port {portName = "J:I_init", portValueType = Integer, portDefault = Nothing, portMeta = Map.fromList [("group",["Initial State"]),("name",["I"]),("type",["Integer"])]}
              , Port {portName = "J:R_init", portValueType = Integer, portDefault = Nothing, portMeta = Map.fromList [("group",["Initial State"]),("name",["R"]),("type",["Integer"])]}
              , Port {portName = "J:S_init", portValueType = Integer, portDefault = Nothing, portMeta = Map.fromList [("group",["Initial State"]),("name",["S"]),("type",["Integer"])]}
              , Port {portName = "J:exp_rate", portValueType = Real, portDefault = Nothing, portMeta = Map.fromList [("group",["Rate"]),("name",["exp"]),("type",["Real"])]}
              , Port {portName = "J:inf_rate", portValueType = Real, portDefault = Nothing, portMeta = Map.fromList [("group",["Rate"]),("name",["inf"]),("type",["Real"])]}
              , Port {portName = "J:rec_rate", portValueType = Real, portDefault = Nothing, portMeta = Map.fromList [("group",["Rate"]),("name",["rec"]),("type",["Real"])]}
              ]
            , modelOutputs = 
              [ Port {portName = "J:E", portValueType = Integer, portDefault = Nothing, portMeta = Map.fromList [("name",["E"]),("type",["Integer"])]}
              , Port {portName = "J:I", portValueType = Integer, portDefault = Nothing, portMeta = Map.fromList [("name",["I"]),("type",["Integer"])]}
              , Port {portName = "J:R", portValueType = Integer, portDefault = Nothing, portMeta = Map.fromList [("name",["R"]),("type",["Integer"])]}
              , Port {portName = "J:S", portValueType = Integer, portDefault = Nothing, portMeta = Map.fromList [("name",["S"]),("type",["Integer"])]}]
            }
          expectedInputs = modelInputs expected
          expectedOutputs = modelOutputs expected
            
          actual = describeModelInterface seir
          actualInputs = modelInputs actual
          actualOutputs = modelOutputs actual

      assertEqual "Expected inputs match actuals" (Set.fromList expectedInputs) (Set.fromList actualInputs)
      assertEqual "Expected outputs match actuals" (Set.fromList expectedOutputs) (Set.fromList actualOutputs)


tests :: Tasty.TestTree
tests =
  Tasty.testGroup "ASKEE API Tests"
    [ testCase "Basic SIR ODE simulation test" $ testSimulateEsl (Inline sir) series1
    , testCase "Parameterized SIR ODE simulation test" $ testSimulateEslParameterized (Inline sir) series2
    , testCase "Basic SIR discrete event simulation test" $ testSimulateEslDiscrete (Inline sirSansParameters) 0 120 30 series3
    , testCase "State-to-state flow schematic" $ testAsSchematicGraph s2s
    , testCase "State-to-event flow schematic" $ testAsSchematicGraph s2e
    , testCase "Event-to-state flow schematic" $ testAsSchematicGraph e2s
    , testCase "Event-to-event flow schematic" $ testAsSchematicGraph e2e
    , testCase "No-flow schematic" $ testAsSchematicGraph n2n
    , testCase "State-to-state flow schematic, no dups" $ testAsSchematicGraph s2sd
    , testCase "Convert ESL to DEQ" testConvertESLToDEQ
    , testCase "Describe SEIR interface" testDescribeInterface
    ]
