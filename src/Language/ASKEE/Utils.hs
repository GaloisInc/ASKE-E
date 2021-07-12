module Language.ASKEE.Utils where

import Data.Text(Text)
import Prettyprinter
import Prettyprinter.Render.Text

render :: Doc ann -> Text
render = renderStrict . layoutPretty defaultLayoutOptions

