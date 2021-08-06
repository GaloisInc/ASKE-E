{-# LANGUAGE OverloadedStrings #-}
module Language.ASKEE.Experiments where

import qualified System.Random as Random
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Text(Text)

import qualified Language.ASKEE as ASKEE
import qualified Language.ASKEE.ESL.Manipulate as MP
import qualified Language.ASKEE.Model as Model
import qualified Language.ASKEE.Error as Error
import qualified Language.ASKEE.DEQ.Simulate as Sim
import qualified Language.ASKEE.DataSeries as DS
import qualified Language.ASKEE.Utils as Utils


dsRestrictFields :: Set Text -> DS.DataSeries a -> DS.DataSeries a
dsRestrictFields fields ds =
  ds { DS.values = Map.restrictKeys (DS.values ds) fields }


mkInfectedData :: FilePath -> IO ()
mkInfectedData out =
  do  sir <- ASKEE.loadESLFrom ASKEE.EaselType (ASKEE.FromFile "modelRepo/easel/sir-meta.easel")
      seird <- ASKEE.loadESLFrom ASKEE.EaselType (ASKEE.FromFile "modelRepo/easel/seird_hosp.easel")
      let double_epi = MP.join (Map.singleton "S" "Susceptible") "ep1_" "ep2_" seird sir

      double_epi_deq <- Error.throwLeft Error.ConversionError (Model.toDeqs $ Model.Easel double_epi)
      let params = Map.fromList [("ep2_beta", 0.5)]
      let double_epi_series = Sim.simulate double_epi_deq params Set.empty [1,2..120]

      let Just infected = Map.lookup "ep2_I" (DS.values double_epi_series)
      let infectedNoise = fromInteger . floor  <$> Utils.withUniformNoise (Random.mkStdGen 1) infected 0.2
      let ds' = double_epi_series { DS.values = Map.singleton "Infected" infectedNoise }

      DS.saveDataSeries out ds'










