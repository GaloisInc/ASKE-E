{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.APRAM.Print where

import Control.Monad.Identity ( runIdentity, Identity )

import           Data.List ( intersperse )
import qualified Data.Map  as Map
import           Data.Map  ( Map )
import           Data.Text ( Text )
import qualified Data.Text as Text

import Language.ASKEE.APRAM.Syntax as APRAM
import Language.ASKEE.Expr as Expr hiding ( And, Or, Not ) 
import Language.ASKEE.ExprTransform ( transformExpr )
import Language.ASKEE.ESL.Print ( pyPrintExpr )

-- import Text.PrettyPrint
import Prettyprinter ( (<>)
                     , emptyDoc
                     , hcat
                     , indent
                     , vsep
                     , brackets
                     , comma
                     , dquotes
                     , lbracket
                     , lparen
                     , parens
                     , rbracket
                     , rparen
                     , Pretty(pretty) )
import qualified Prettyprinter as PP

import Prelude hiding ((<>))
import Data.Maybe (isJust, mapMaybe)

type Doc = PP.Doc ()

printAPRAM :: APRAM -> Doc 
printAPRAM APRAM{..} =
  vsep $ intersperse "" [ preamble apramAgents
                        , printStatuses apramStatuses
                        , printParams apramParams
                        , printCohorts apramCohorts
                        , printMods apramParams apramMods
                        , driver apramCohorts
                        ]

printCohorts :: [Cohort] -> Doc
printCohorts cs = 
  vsep $ 
    [ "pop.make_cohort("<>dquotes (text' name)<>", lambda: "<>printCohort cexpr<>")"
    | Cohort name cexpr <- cs
    ]
  where
    printCohort (And c1 c2) = "np.logical_and("<>printCohort c1<>","<>printCohort c2<>")"
    printCohort (Or c1 c2) = "np.logical_or("<>printCohort c1<>","<>printCohort c2<>")"
    printCohort (Is col stat) = "pop."<>text' col<>".eq("<>text' stat<>")"
    printCohort (Not col stat) = "pop."<>text' col<>".ne("<>text' stat<>")"
    printCohort All = "np.repeat(True, pop.size)"

printStatuses :: Map Column [Status] -> Doc
printStatuses = vsep . map printColumnWithStatuses . Map.toList
  where
    printColumnWithStatuses :: (Column, [Status]) -> Doc
    printColumnWithStatuses (col, sts) = 
      vsep $ 
        "pop.make_column("<>dquotes (text' col)<>", np.zeros(pop.size))" :
        [ text' st<>" = "<>pretty i
        | (st, i) <- zip sts [1::Int ..] ]

printParams :: Map Text Expr -> Doc 
printParams params = vsep (map printParam (Map.toList params))
  where
    printParam (v, e) = "pop.make_param("<>dquotes (text' v)<>", "<>pyPrintExpr e<>")"

preamble :: Int -> Doc
preamble apramAgents = vsep
  [ "import aPRAM_utils as utils"
  , "import math"
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
  , "pop.size = "<>pretty apramAgents
  , ""
  , "pop.reset()"
  , "sim.reset()"
  , ""
  , "delta = 1e-3"
  ]

driver :: [Cohort] -> Doc 
driver cohorts = vsep
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
    probes = "step,":[ "pop."<>text' name<>".size," | name <- names ]
    labels = "\"step\",":[ dquotes (text' name)<>"," | name <- names ]

printMods :: Map Text Expr -> [Mod] -> Doc
printMods params mods = (vsep . map printMod) mods
  where
  printMod Mod{..} = vsep
    [ "sim.make_mod"<>lparen 
    , indent 4 ("name="<>dquotes (text' modName)<>comma)
    , indent 4 ("cohort=pop."<>text' (cohortName modCohort)<>comma)
    , indent 4 ("mods="<>lbracket)
    , vsep (map (indent 8 . (<> comma) . brackets . printActionSequence) actions)
    , indent 4 rbracket<>comma
    , indent 4 ("prob_spec=lambda: "<>lbracket)
    , vsep (map (indent 8 . (<> comma)) probabilities)
    , indent 4 rbracket<>comma
    , indent 4 ("sim_phase="<>dquotes (text' modPhase)<>comma)
    , rparen ]
    
    where
      printActionSequence :: ActionSequence -> Doc
      printActionSequence (Actions as) = hcat $ map printAction as
      printActionSequence Pass = emptyDoc 

      printAction :: Action -> Doc
      printAction (Assign column status) = "lambda **kwargs: pop."<>text' column<>".assign"<>parens (text' status<>", **kwargs")

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
          potentialRates = map snd $ concatMap APRAM.modActions $ filter (\m -> APRAM.modCohort m == modCohort) mods
          rates = mapMaybe (\case Rate r -> Just $ qualify r; _ -> Nothing) potentialRates
          rateSum = foldr1 Add rates
          baseActProb = LitD 1 `Sub` Exp (Neg rateSum `Mul` Var deltaName)
          inaction = rateSum `Expr.EQ` LitD 0

          actProb :: Expr
          actProb =
            case filter (/= Unknown) rs of
              [Rate rate] -> baseActProb `Mul` (qualify rate `Div` rateSum)
              _ -> error "there was more than one known rate associated with this event"

          printRate :: ProbSpec -> Doc
          printRate r = 
            pyPrintExpr $
              case r of
                Unknown -> If inaction (LitD 1) (LitD 1 `Sub` actProb)
                Rate _  -> If inaction (LitD 0) actProb
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
      qualify' (Var v) | v `Map.member` params = -- it's a param
        pure $ Var $ "pop." `Text.append` v `Text.append` ".val"
      qualify' (Var v) | v == deltaName =
        pure (Var v)
      qualify' (Var v) = -- it's a cohort
        pure $ Var $ "pop." `Text.append` v `Text.append` ".size"
      qualify' e = pure e

text' :: Text -> Doc 
text' = pretty

-- The variable used in aPRAM-land to represent time delta between
-- event firings
deltaName :: Text 
deltaName = "delta"