-- | This module provides facilities for generating unique names.
--
-- >>> let src = blankNameSource :: NameSource Name
-- >>> let (name, src') = newName (nameFromString "foo") src
-- >>> nameToString name
-- "foo_0"
-- >>> let (name2, src'') = newName (nameFromString "bar") src'
-- >>>> nameToString name2
-- "bar_1"
module L0C.FreshNames
  ( NameSource
  , VNameSource
  , blankNameSource
  , newNameSource
  , newNameSourceForProg
  , newName
  , newID
  , newVName
  ) where

import qualified Data.Set as S

import L0C.L0

-- | A name source is conceptually an infinite sequence of names with
-- no repeating entries.  In practice, when asked for a name, the name
-- source will return the name along with a new name source, which
-- should then be used in place of the original.
data NameSource vn = NameSource Int (S.Set vn)

-- | A 'NameSource' that produces 'VName's.
type VNameSource = NameSource VName

-- | A blank name source.
blankNameSource :: VarName vn => NameSource vn
blankNameSource = NameSource 0 S.empty

-- | Create a new 'NameSource' that will never produce any of the
-- names in the given set.
newNameSource :: VarName vn => S.Set vn -> NameSource vn
newNameSource = NameSource 0

-- | Create a new 'NameSource' that will never produce any of the
-- names used as variables in the given program.
newNameSourceForProg :: VarName vn => ProgBase ty vn -> NameSource vn
newNameSourceForProg = newNameSource . progNames

-- | Produce a fresh name, using the given name as a template.
newName :: VarName vn => vn -> NameSource vn -> (vn, NameSource vn)
newName s (NameSource counter skip) =
  let s' = s `setID` counter
  in if s' `S.member` skip then newName s newsrc
     else (s', newsrc)
  where newsrc = NameSource (counter+1) skip

-- | Produce a fresh 'ID', using the given base name as a template.
newID :: VarName vn =>
         vn -> NameSource (ID vn) -> (ID vn, NameSource (ID vn))
newID s = newName $ ID (s, 0)

-- | Produce a fresh 'VName', using the given base name as a template.
newVName :: String -> VNameSource -> (VName, VNameSource)
newVName = newID . nameFromString
