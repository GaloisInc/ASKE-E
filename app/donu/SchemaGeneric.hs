{-# Language BlockArguments, GADTs #-}
module SchemaGeneric
  ( generateDocs
  , DocFormat(..)
  , DocAlts
  ) where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(liftM,ap)

import Config.Schema.Types

-- | A list of rendered alternatives
type DocAlts d = [d]

data DocFormat d = DocFormat
  { docTopDoc   :: DocAlts d -> [d] -> d
    -- ^ Generate documentation given the alternatives for the protocal,
    -- and a list of the definitions.

  , docDef      :: Text -> DocAlts d -> d
    -- ^ Render a definition for the given label

  , docLabel    :: Text -> d
    -- | This is how we render labels (i.e., named pieces of documentation)

  , docSection  :: Bool -> Text -> Text -> DocAlts d -> d
    -- ^ Render a named field in a struct (aka Section)
    -- The arguments are:
    --   * Is this entry optional?
    --   * The name of the field
    --   * Doc string describing the purpose of the field
    --   * A type describing the possible values

  , docText     :: d
    -- ^ Render the Text/String type

  , docNumber   :: d
    -- ^ Render the numberic type

  , docAnyAtom  :: d
    -- ^ Render a type that accepts any atom

  , docAtom     :: Text -> d
    -- ^ Render a specific atom

  , docList     :: DocAlts d -> d
    -- ^ Render a list type

  , docSect     :: [d] -> d
    -- ^ Render a struct type

  , docAssoc    :: DocAlts d -> d
    -- ^ Render a "map" type

  , docCustom   :: Text -> DocAlts d -> d
    -- ^ Render a checked type.  The text argument is an adjective
    -- describing the validation (e.g., "positive")
  }

-- | Generate documentation for a value spec,
-- using the given generation format.
generateDocs :: DocFormat d -> ValueSpec a -> d
generateDocs r v = docTopDoc r doc (map doDef (Map.toList defs))
  where
  DocBuilder m = valueDocs v
  (doc,defs)   = m r Map.empty
  doDef (x,d)  = docDef r x d

valueDocs :: ValueSpec a -> DocBuilder d (DocAlts d)
valueDocs = sequenceA . runValueSpec_ (fmap pure primValueDocs)

sectionsDocs :: PrimSectionSpec a -> DocBuilder d d
sectionsDocs sect =
  case sect of
    ReqSection name doc spec -> field False name doc spec
    OptSection name doc spec -> field True  name doc spec
  where
  field opt name doc ty =
    do f <- format docSection
       f opt name doc <$> valueDocs ty

primValueDocs :: PrimValueSpec a -> DocBuilder d d
primValueDocs value =
  case value of

    TextSpec ->
      format docText

    NumberSpec ->
      format docNumber

    AnyAtomSpec ->
      format docAnyAtom

    AtomSpec a ->
      do f <- format docAtom
         pure (f a)

    ListSpec v ->
      do f <- format docList
         f <$> valueDocs v

    SectionsSpec l sect ->
      emitDoc l
         do fs <- sequenceA (runSections_ ((:[]) . sectionsDocs) sect)
            f  <- format docSect
            pure [f fs]

    AssocSpec v ->
      do f <- format docAssoc
         f <$> valueDocs v

    CustomSpec n v ->
      do f <- format docCustom
         f n <$> valueDocs v

    NamedSpec l v ->
      emitDoc l (valueDocs v)

--------------------------------------------------------------------------------
-- Doc Builders

type R d                = DocFormat d
type S d                = Map Text (DocAlts d)
newtype DocBuilder d a  = DocBuilder (R d -> S d -> (a,S d))

instance Functor (DocBuilder d) where
  fmap = liftM

instance Applicative (DocBuilder d) where
  pure a = DocBuilder \_ s -> (a,s)
  (<*>)  = ap

instance Monad (DocBuilder d) where
  DocBuilder m >>= f =
    DocBuilder \r s -> case m r s of
                         (a,s1) -> case f a of
                                     DocBuilder m1 -> m1 r s1

-- | Given a section name and section body, store the body
-- in the map of sections and return the section name.
emitDoc ::
  Text           {- ^ section name     -} ->
  DocBuilder d (DocAlts d) {- ^ section body     -} ->
  DocBuilder d d {- ^ section name doc -}
emitDoc l (DocBuilder sub) = DocBuilder \r s ->
  let s1 = if Map.member l s
              then s
              else let (v,s2) = sub r (Map.insert l v s)
                   in s2
  in (docLabel r l, s1)

-- | Access one of the document constructors
format :: (DocFormat d -> a) -> DocBuilder d a
format f = DocBuilder \r s -> (f r, s)



