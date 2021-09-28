{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | These should probably live in the @xml@ package, but oh well.
module Language.ASKEE.SBML.Instances where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Text.XML.Light

deriving instance Eq CData
deriving instance Eq Content
deriving instance Eq Element

deriving instance Generic Attr
deriving instance Generic CData
deriving instance Generic CDataKind
deriving instance Generic Content
deriving instance Generic Element
deriving instance Generic QName

deriving instance NFData Attr
deriving instance NFData CData
deriving instance NFData CDataKind
deriving instance NFData Content
deriving instance NFData Element
deriving instance NFData QName

deriving instance Ord CData
deriving instance Ord CDataKind
deriving instance Ord Content
deriving instance Ord Element
