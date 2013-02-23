module L0.FreshNames
  ( NameSource
  , newNameSource
  , newNameSourceForProg
  , newName
  ) where

import L0.AbSyn

import qualified Data.Set as S

data NameSource = NameSource Int (S.Set String)

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
  let s' = s ++ "_" ++ show counter
  in if s' `S.member` skip then newName s newsrc
     else (s', newsrc)
  where newsrc = NameSource (counter+1) skip
