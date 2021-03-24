{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.APRAM.Print where

import Control.Monad.Identity ( runIdentity, Identity )

import           Data.List ( intersperse )
import qualified Data.Map  as Map
import           Data.Map  ( Map )
import           Data.Text ( Text, unpack )
import qualified Data.Text as Text

import Language.ASKEE.APRAM.Syntax
import Language.ASKEE.Expr as Expr hiding ( And, Or, Not ) 
import Language.ASKEE.ExprTransform ( transformExpr )
import Language.ASKEE.Print ( pyPrintExpr )

import Text.PrettyPrint

import Prelude hiding ((<>))
import Data.Maybe (isJust, mapMaybe)

printAPRAM :: APRAM -> Doc 
printAPRAM APRAM{..} =
  vcat $ intersperse "" [ preamble apramAgents
                        , printStatuses apramStatuses
                        , printParams apramParams
                        , printCohorts apramCohorts
                        , printMods apramParams apramMods
                        , driver apramCohorts
                        ]

printCohorts :: [Cohort] -> Doc
printCohorts cs = 
  vcat $ 
    [ "pop.make_cohort("<>doubleQuotes (text name)<>", lambda: "<>printCohort cexpr<>")"
    | Cohort name cexpr <- cs
    ]
  where
    printCohort (And c1 c2) = "np.logical_and("<>printCohort c1<>","<>printCohort c2<>")"
    printCohort (Or c1 c2) = "np.logical_or("<>printCohort c1<>","<>printCohort c2<>")"
    printCohort (Is col stat) = "pop."<>text col<>".eq("<>text stat<>")"
    printCohort (Not col stat) = "pop."<>text col<>".ne("<>text stat<>")"
    printCohort All = "np.repeat(True, pop.size)"

printStatuses :: Map Column [Status] -> Doc
printStatuses = vcat . map printColumnWithStatuses . Map.toList
  where
    printColumnWithStatuses :: (Column, [Status]) -> Doc
    printColumnWithStatuses (col, sts) = 
      vcat $ 
        "pop.make_column("<>doubleQuotes (text col)<>", np.zeros(pop.size))" :
        [ text st<>" = "<>int i
        | (st, i) <- zip sts [1..] ]

printParams :: Map String Expr -> Doc 
printParams params = vcat (map printParam (Map.toList params))
  where
    printParam (v, e) = "pop.make_param("<>doubleQuotes (text v)<>", "<>pyPrintExpr e<>")"

preamble :: Int -> Doc
preamble apramAgents = vcat
  [ "import aPRAM_utils as utils"
  , "import matplotlib.pyplot as plt"
  , "import numpy as np"
  , "import pandas as pd "
  , ""
  , "from aPRAM_expressions import WN"
  , "from aPRAM_settings import pop, sim"
  , "from numpy.random import default_rng"
  , ""
  , "from probabilities import exponential_CDF"
  , ""
  , "rng = default_rng()"
  , ""
  , "pop.size = "<>int apramAgents
  , ""
  , "pop.reset()"
  , "sim.reset()"
  , ""
  , "delta = 1e-3"
  ]

driver :: [Cohort] -> Doc 
driver cohorts = vcat
  [ "def probe_fn(step):"
  , "    return "<>brackets (hcat probes)
  , ""
  , "probe_labels = "<>brackets (hcat labels)
  , ""
  , "sim.probe_labels   = probe_labels"
  , "sim.probe_fn       = probe_fn"
  , ""
  , "def run(time_steps):"
  , "    sim.num_iterations = int(time_steps / delta)"
  , "    sim.run_simulation()"
  , "    df = pd.DataFrame(sim.records,columns=probe_labels)"
  , "    steps = [delta * step for step in df['step']]"
  , "    df['time'] = steps"
  , "    return df"
  ]
  where
    names = [ n | Cohort n _ <- cohorts ]
    probes = "step,":[ "pop."<>text name<>".size," | name <- names ]
    labels = "\"step\",":[ doubleQuotes (text name)<>"," | name <- names ]

printMods :: Map String Expr -> [Mod] -> Doc
printMods params = vcat . map printMod
  where
  printMod Mod{..} =
    "sim.make_mod"<>lparen 
    $+$ nest 4 ("name="<>doubleQuotes (text modName)<>comma)
    $+$ nest 4 ("cohort=pop."<>text (cohortName modCohort)<>comma)
    $+$ nest 4 ("mods="<>lbrack)
    $+$ vcat (map (nest 8 . (<> comma) . brackets . printActionSequence) actions)
    $+$ nest 4 rbrack<>comma
    $+$ nest 4 ("prob_spec=lambda: "<>lbrack)
    $+$ vcat (map (nest 8 . (<> comma)) probabilities)
    $+$ nest 4 rbrack<>comma
    $+$ nest 4 ("sim_phase="<>doubleQuotes (text modPhase)<>comma)
    $+$ rparen
    
    where
      printActionSequence :: ActionSequence -> Doc
      printActionSequence (Actions as) = hcat $ map printAction as
      printActionSequence Pass = empty

      printAction :: Action -> Doc
      printAction (Assign column status) = "lambda **kwargs: pop."<>text column<>".assign"<>parens (text status<>", **kwargs")

      probabilities
        | all (isJust . fromProb) probSpecs = 
          printProbabilities (mapMaybe fromProb probSpecs)
        | all (isJust . asRateOrUnknown) probSpecs = 
          printRates (mapMaybe asRateOrUnknown probSpecs) 
        | otherwise = error $ "mixed probability specs in "++show probSpecs

      printProbabilities :: [Expr] -> [Doc]
      printProbabilities = map (pyPrintExpr . qualify)

      -- printRates needs ProbSpecs because it needs to assign a probability to 
      -- an `Unknown` rate, which would be created from a `Pass` action
      printRates :: [ProbSpec] -> [Doc]
      printRates rs = map printRate rs
        where
          rates = mapMaybe (\case Rate r -> Just $ qualify r; _ -> Nothing) rs
          rateSum = foldr1 Add rates
          baseActProb = "exponential_CDF"<>parens ("delta, "<>pyPrintExpr rateSum)
          baseActProbExpr = Text.pack $ show baseActProb
          inaction = rateSum `Expr.EQ` LitD 0

          printRate :: ProbSpec -> Doc
          printRate r = 
            pyPrintExpr $ 
              case r of
                Unknown -> If inaction (LitD 1) (LitD 1 `Sub` Blob baseActProbExpr)
                Rate rate -> If inaction (LitD 0) (Blob baseActProbExpr `Mul` (qualify rate `Div` rateSum))
                _ -> undefined

      fromProb p =
        case p of
          Probability e -> Just e
          _ -> Nothing

      asRateOrUnknown p =
        case p of
          Rate e -> Just (Rate e)
          Unknown -> Just Unknown
          _ -> Nothing

      (actions, probSpecs) = unzip modActions

      qualify :: Expr -> Expr
      qualify = runIdentity . transformExpr qualify'

      qualify' :: Expr -> Identity Expr
      qualify' (Var v) | unpack v `Map.member` params = -- it's a param
        pure $ Var $ "pop." `Text.append` v `Text.append` ".val"
      qualify' (Var v) = -- it's a cohort
        pure $ Var $ "pop." `Text.append` v `Text.append` ".size"
      qualify' e = pure e

text' :: Text -> Doc 
text' = text . Text.unpack