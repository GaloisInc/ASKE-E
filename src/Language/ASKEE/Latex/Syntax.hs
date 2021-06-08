module Language.ASKEE.Latex.Syntax where
  
import Language.ASKEE.DEQ.Syntax ( DiffEqs )

newtype Latex = Latex { unLatex :: DiffEqs }