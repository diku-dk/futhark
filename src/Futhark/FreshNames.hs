-- | This module provides facilities for generating unique names.
--
-- >>> let src = blankNameSource :: NameSource Name
-- >>> let (name, src') = newName src (nameFromString "foo")
-- >>> nameToString name
-- "foo_0"
-- >>> let (name2, src'') = newName src' (nameFromString "bar")
-- >>>> nameToString name2
-- "bar_1"
module Futhark.FreshNames
  ( VNameSource (..)
  , blankNameSource
  , newVName
  , newVNameFromName
  ) where

import Language.Futhark.Core

-- | A name source is conceptually an infinite sequence of names with
-- no repeating entries.  In practice, when asked for a name, the name
-- source will return the name along with a new name source, which
-- should then be used in place of the original.
data VNameSource = VNameSource {
    newName :: VName -> (VName, VNameSource)
  -- ^ Produce a fresh name, using the given name as a template.
}

counterGenerator :: Int -> VName -> (VName, VNameSource)
counterGenerator counter (ID (s, _)) =
  (ID (s, counter), newsrc)
  where newsrc = VNameSource next
        next = counterGenerator (counter+1)

-- | A blank name source.
blankNameSource :: VNameSource
blankNameSource = VNameSource $ counterGenerator 0

-- | Produce a fresh 'VName', using the given base name as a template.
newVName :: VNameSource -> String -> (VName, VNameSource)
newVName src = newVNameFromName src . nameFromString

-- | Produce a fresh 'VName', using the given base name as a template.
newVNameFromName :: VNameSource -> Name -> (VName, VNameSource)
newVNameFromName src s = newName src $ ID (s, 0)
