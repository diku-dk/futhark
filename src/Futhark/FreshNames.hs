{-# LANGUAGE DeriveLift #-}
-- | This module provides facilities for generating unique names.
module Futhark.FreshNames
  ( VNameSource
  , blankNameSource
  , newNameSource
  , newName
  , newVName
  , newVNameFromName
  ) where

import Language.Haskell.TH.Syntax (Lift)

import Language.Futhark.Core

-- | A name source is conceptually an infinite sequence of names with
-- no repeating entries.  In practice, when asked for a name, the name
-- source will return the name along with a new name source, which
-- should then be used in place of the original.
--
-- The 'Ord' instance is based on how many names have been extracted
-- from the name source.
newtype VNameSource = VNameSource Int
  deriving (Lift, Eq, Ord)

instance Semigroup VNameSource where
  VNameSource x <> VNameSource y = VNameSource (x `max` y)

instance Monoid VNameSource where
  mempty = blankNameSource

-- | Produce a fresh name, using the given name as a template.
newName :: VNameSource -> VName -> (VName, VNameSource)
newName (VNameSource i) k = (VName (baseName k) i, VNameSource (i+1))

-- | A blank name source.
blankNameSource :: VNameSource
blankNameSource = newNameSource 0

-- | A new name source that starts counting from the given number.
newNameSource :: Int -> VNameSource
newNameSource = VNameSource

-- | Produce a fresh 'VName', using the given base name as a template.
newVName :: VNameSource -> String -> (VName, VNameSource)
newVName src = newVNameFromName src . nameFromString

-- | Produce a fresh 'VName', using the given base name as a template.
newVNameFromName :: VNameSource -> Name -> (VName, VNameSource)
newVNameFromName src s = newName src $ VName s 0
