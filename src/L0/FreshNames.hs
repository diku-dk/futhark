-- | This module provides facilities for generating unique names.
--
-- >>> let src = newNameSource []
-- >>> let (name, src') = newName "foo" src
-- >>> name
-- "foo_0"
-- >>> let (name2, src'') = newName "bar" src'
-- >>>> name2
-- "bar_1"
module L0.FreshNames
  ( NameSource
  , newNameSource
  , newNameSourceForProg
  , newName
  ) where

import L0.AbSyn

import Data.Char (isDigit)
import qualified Data.Set as S

-- | A name source is conceptually an infinite sequence of names with
-- no repeating entries.  In practice, when asked for a name, the name
-- source will return the name along with a new name source, which
-- should then be used in place of the original.
data NameSource = NameSource Int (S.Set String)

-- | Chop off terminating underscore followed by numbers.
baseName :: String -> String
baseName = reverse . dropWhile (=='_') . dropWhile isDigit . reverse

-- | Create a new 'NameSource' that will never produce any of the
-- names in the given list.
newNameSource :: [String] -> NameSource
newNameSource = NameSource 0 . S.fromList

-- | Create a new 'NameSource' that will never produce any of the
-- names used as variables in the given program.
newNameSourceForProg :: TypeBox ty => Prog ty -> NameSource
newNameSourceForProg = newNameSource . progNames

-- | Produce a fresh name, using the given string as a template.
newName :: String -> NameSource -> (String, NameSource)
newName s (NameSource counter skip) =
  let s' = baseName s ++ "_" ++ show counter
  in if s' `S.member` skip then newName s newsrc
     else (s', newsrc)
  where newsrc = NameSource (counter+1) skip
