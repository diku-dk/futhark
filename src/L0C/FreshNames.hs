-- | This module provides facilities for generating unique names.
--
-- >>> let src = newNameSource mempty
-- >>> let (name, src') = newName "foo" src
-- >>> name
-- "foo_0"
-- >>> let (name2, src'') = newName "bar" src'
-- >>>> name2
-- "bar_1"
module L0C.FreshNames
  ( NameSource
  , blankNameSource
  , newNameSource
  , newNameSourceForProg
  , newName
  ) where

import Language.L0.Syntax
import Language.L0.Traversals
import qualified Data.Text as T
import Data.Monoid

import Data.Char (isDigit)
import qualified Data.Set as S

-- | A name source is conceptually an infinite sequence of names with
-- no repeating entries.  In practice, when asked for a name, the name
-- source will return the name along with a new name source, which
-- should then be used in place of the original.
data NameSource = NameSource Int (S.Set T.Text)

-- | Chop off terminating underscore followed by numbers.
baseName :: T.Text -> T.Text
baseName = T.dropWhileEnd (=='_') . T.dropWhileEnd isDigit

-- | A blank name source.
blankNameSource :: NameSource
blankNameSource = NameSource 0 S.empty

-- | Create a new 'NameSource' that will never produce any of the
-- names in the given set.
newNameSource :: S.Set T.Text -> NameSource
newNameSource = NameSource 0

-- | Create a new 'NameSource' that will never produce any of the
-- names used as variables in the given program.
newNameSourceForProg :: Prog ty -> NameSource
newNameSourceForProg = newNameSource . progNames

-- | Produce a fresh name, using the given name as a template.
newName :: T.Text -> NameSource -> (T.Text, NameSource)
newName s (NameSource counter skip) =
  let s' = baseName s <> T.pack ('_' : show counter)
  in if s' `S.member` skip then newName s newsrc
     else (s', newsrc)
  where newsrc = NameSource (counter+1) skip
