{-# LANGUAGE TemplateHaskell #-}
module Language.ASKEE.Translate where

import           Language.ASKEE.Convert              ( converter )
import           Language.ASKEE.Types                ( ModelType(..)
                                                     , Representation(..) )
import qualified Language.ASKEE.ESL.Syntax           as ESL
import qualified Language.ASKEE.DEQ.Syntax           as DEQ
import qualified Language.ASKEE.RNet.Syntax          as RNet
import qualified Language.ASKEE.Latex.Syntax         as Latex
import qualified Language.ASKEE.ModelStratify.Syntax as Topology

class ParseModel a where
  parseModel :: String -> Either String a

instance ParseModel ESL.Model where
  parseModel = $(converter (ESL Concrete) (ESL Abstract))

instance ParseModel DEQ.DiffEqs where
  parseModel = $(converter (DEQ Concrete) (DEQ Abstract))

instance ParseModel RNet.ReactionNet where
  parseModel = $(converter (RNET Concrete) (RNET Abstract))

instance ParseModel Latex.Latex where
  parseModel = $(converter (LATEX Concrete) (LATEX Abstract))

instance ParseModel Topology.Net where
  parseModel = $(converter (TOPO Concrete) (TOPO Abstract))

instance ParseModel ESL.ModelMeta where
  parseModel = $(converter (ESLMETA Concrete) (ESLMETA Abstract))

-------------------------------------------------------------------------------

class SerializeModel a where
  serializeModel :: a -> Either String String

instance SerializeModel ESL.Model where
  serializeModel = $(converter (ESL Abstract) (ESL Concrete))

instance SerializeModel DEQ.DiffEqs where
  serializeModel = $(converter (DEQ Abstract) (DEQ Concrete))

-- XXX: This probably should work
-- instance SerializeModel RNet.ReactionNet where
--   serializeModel = $(converter (RNET Abstract) RNET (Concrete))

instance SerializeModel Latex.Latex where
  serializeModel = $(converter (LATEX Abstract) (LATEX Concrete))

instance SerializeModel Topology.Net where
  serializeModel = $(converter (TOPO Abstract) (TOPO Concrete))

-- instance SerializeModel ESL.ModelMeta where
--   serializeModel = $(converter (ESLMETA Abstract) (ESLMETA Concrete))

-------------------------------------------------------------------------------

-- class AsESL a where
--   asESL :: a -> Either String ESL.Model

-- instance AsESL ESL.Model where
--   asESL = $(converter (ESL Abstract) (ESL Abstract))

-- -- XXX: This could be made to work partially
-- -- instance AsESL DEQ.DiffEqs where
-- --   asESL = $(converter (DEQ Abstract) (ESL Abstract))

-- instance AsESL RNet.ReactionNet where
--   asESL = $(converter (RNET Abstract) (ESL Abstract))

-- -- XXX blocked by lack of DEQ -> ESL translation
-- -- instance AsESL Latex.Latex where
-- --   asESL = $(converter (LATEX Abstract) (ESL Abstract))

-- instance AsESL Topology.Net where
--   asESL = $(converter (TOPO Abstract) (ESL Abstract))

-- -------------------------------------------------------------------------------

-- class AsDEQ a where
--   asDEQ :: a -> Either String DEQ.DiffEqs

-- instance AsDEQ ESL.Model where
--   asDEQ = $(converter (ESL Abstract) (DEQ Abstract))

-- instance AsDEQ DEQ.DiffEqs where
--   asDEQ = $(converter (DEQ Abstract) (DEQ Abstract))

-- instance AsDEQ RNet.ReactionNet where
--   asDEQ = $(converter (RNET Abstract) (DEQ Abstract))

-- instance AsDEQ Latex.Latex where
--   asDEQ = $(converter (LATEX Abstract) (DEQ Abstract))

-- instance AsDEQ Topology.Net where
--   asDEQ = $(converter (TOPO Abstract) (DEQ Abstract))

-- -------------------------------------------------------------------------------

-- class AsRNet a where
--   asRNet :: a -> Either String RNet.ReactionNet

-- -- XXX: don't remember whether there are theoretical limitations here 
-- -- instance AsRNet ESL.Model where
-- --   asRNet = $(converter (ESL Abstract) (RNET Abstract))

-- -- XXX: blocked by lack of DEQ -> ESL translation
-- -- instance AsRNet DEQ.DiffEqs where
-- --   asRNet = $(converter (DEQ Abstract) (RNET Abstract))

-- instance AsRNet RNet.ReactionNet where
--   asRNet = $(converter (RNET Abstract) (RNET Abstract))

-- -- XXX: blocked by lack of DEQ -> ESL translation
-- -- instance AsRNet Latex.Latex where
-- --   asRNet = $(converter (LATEX Abstract) (RNET Abstract))