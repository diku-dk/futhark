-- | This module provides facilities for generating unique names.
--
-- >>> let src = blankNameSource :: NameSource Name
-- >>> let (name, src') = newName src (nameFromString "foo")
-- >>> nameToString name
-- "foo_0"
-- >>> let (name2, src'') = newName src' (nameFromString "bar")
-- >>>> nameToString name2
-- "bar_1"
module L0C.FreshNames
  ( NameSource(NameSource)
  , VNameSource
  , blankNameSource
  , newNameSource
  , newName
  , newID
  , newVName
  ) where

import qualified Data.HashSet as HS

import Language.L0.Misc

-- | A name source is conceptually an infinite sequence of names with
-- no repeating entries.  In practice, when asked for a name, the name
-- source will return the name along with a new name source, which
-- should then be used in place of the original.
data NameSource vn = NameSource {
    newName :: vn -> (vn, NameSource vn)
  -- ^ Produce a fresh name, using the given name as a template.
}

counterGenerator :: VarName vn => Int -> HS.HashSet vn -> vn -> (vn, NameSource vn)
counterGenerator counter skip s =
  let s' = s `setID` counter
  in if s' `HS.member` skip then next s
     else (s', newsrc)
    where newsrc = NameSource next
          next = counterGenerator (counter+1) skip

-- | A 'NameSource' that produces 'VName's.
type VNameSource = NameSource VName

-- | A blank name source.
blankNameSource :: VarName vn => NameSource vn
blankNameSource = NameSource $ counterGenerator 0 HS.empty

-- | Create a new 'NameSource' that will never produce any of the
-- names in the given set.
newNameSource :: VarName vn => HS.HashSet vn -> NameSource vn
newNameSource = NameSource . counterGenerator 0

-- | Produce a fresh 'ID', using the given base name as a template.
newID :: VarName vn =>
         NameSource (ID vn) -> vn -> (ID vn, NameSource (ID vn))
newID src s = newName src $ ID (s, 0)

-- | Produce a fresh 'VName', using the given base name as a template.
newVName :: VNameSource -> String -> (VName, VNameSource)
newVName src = newID src . nameFromString
