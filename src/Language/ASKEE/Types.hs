module Language.ASKEE.Types where
  
data Representation = Abstract | Concrete
  deriving (Eq, Ord, Show)

data ModelType =
    ESL Representation
  | DEQ Representation
  | RNET Representation
  | TOPO Representation
  | LATEX Representation
  | GROMET Representation
  deriving (Eq, Ord, Show)