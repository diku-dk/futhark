-- | This module exports the full internal reprensetation of the Futhark
-- language.
module Futhark.InternalRep
  ( module Futhark.InternalRep.Syntax
  , module Futhark.InternalRep.Attributes
  , module Futhark.InternalRep.Traversals
  , module Futhark.InternalRep.Pretty

  , newNameSourceForProg

  , Several(..)
  , justOne
  )
where

import Text.PrettyPrint.Mainland

import Futhark.InternalRep.Syntax
import Futhark.InternalRep.Attributes
import Futhark.InternalRep.Traversals
import Futhark.InternalRep.Pretty
import Futhark.FreshNames

-- | Create a new 'NameSource' that will never produce any of the
-- names used as variables in the given program.
newNameSourceForProg :: Prog -> VNameSource
newNameSourceForProg = newNameSource . progNames

-- | A list.  Its 'Pretty' instance produces a comma-separated
-- sequence enclosed in braces if the list has anything but a single
-- element.
newtype Several a = Several [a]
  deriving (Eq, Ord, Show)

instance Pretty a => Pretty (Several a) where
  ppr (Several [t]) = ppr t
  ppr (Several ts)  = braces $ commasep $ map ppr ts

-- | Turn a single value into a singleton list.
justOne :: a -> Several a
justOne x = Several [x]
