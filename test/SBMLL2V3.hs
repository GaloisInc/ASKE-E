module SBMLL2V3 where

import Data.Generics (Data, everywhere, mkT)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Language.ASKEE.SBML.Common.Parse ( parse )
import Language.ASKEE.SBML.L2V3.Parse
import Language.ASKEE.SBML.L2V3.Syntax
import Language.ASKEE.SBML.L2V3.ToXML

import Test.Tasty
import Test.Tasty.HUnit

import Text.XML.Light

brown2004Roundtrip :: Assertion
brown2004Roundtrip = roundtripTest "modelRepo/sbml/brown2004.sbml"

chen2009Roundtrip :: Assertion
chen2009Roundtrip = roundtripTest "modelRepo/sbml/chen2009.sbml"

roundtripTest :: FilePath -> Assertion
roundtripTest modelPath = do
  fileContents <- T.readFile modelPath
  case parseSBMLL2V3 (T.unpack fileContents) of
    Left err   -> assertFailure err
    Right sbml -> do
      let ppSBML = sbmlToXML sbml
      case parseSBMLL2V3 (T.unpack ppSBML) of
        Left err    -> assertFailure err
        Right sbml' -> scrubLines sbml @=? scrubLines sbml'
  where
    -- sbmlToXML does not preserve the line numbers in notes or annotations, so
    -- don't consider them important when checking for equality.
    scrubLines :: Data a => a -> a
    scrubLines = everywhere (mkT scrubElementLine . mkT scrubCDataLine)

    scrubCDataLine :: CData -> CData
    scrubCDataLine cd = cd{cdLine = (0 <$ cdLine cd)}

    scrubElementLine :: Element -> Element
    scrubElementLine el = el{elLine = (0 <$ elLine el)}

parseSBMLL2V3 :: String -> Either String SBML
parseSBMLL2V3 src = parse src parseSBML

-------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "SBML L2V3 tests"
  [ testGroup "SBML L2V3 real-world unit tests"
    [ {- testCase "brown2004 roundtrip" brown2004Roundtrip
    ,-} testCase "chen2009 roundtrip" chen2009Roundtrip
    ]
  ]
