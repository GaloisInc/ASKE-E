{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
module ASKEE where

import Language.ASKEE
import qualified Test.Tasty as Tasty
import Test.Tasty.HUnit(Assertion, assertBool, (@=?), assertFailure, testCase)
import qualified Data.FileEmbed as Embed
import qualified Data.Map as Map
import Data.Text(Text)

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
      actual <- simulateModel (ESL Concrete) mdlSrc start stop step Map.empty
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
    isClose n1 n2 = (n1 - n2) < 0.00000001
    compareVars (_, as) (_, bs) = and $ zipWith isClose as bs
    asc = Map.toAscList . values


tests :: Tasty.TestTree
tests =
  Tasty.testGroup "ASKEE API Tests"
    [ testCase "Basic SIR test" $ testSimulateEsl (Inline sir) series1
    ]