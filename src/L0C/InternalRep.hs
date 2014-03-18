module L0C.InternalRep
  ( module L0C.InternalRep.Syntax
  , module L0C.InternalRep.Attributes
  , module L0C.InternalRep.Traversals
  , module L0C.InternalRep.Pretty

  , newNameSourceForProg

  , Several(..)
  , justOne
  )
where

import Text.PrettyPrint.Mainland

import L0C.InternalRep.Syntax
import L0C.InternalRep.Attributes
import L0C.InternalRep.Traversals
import L0C.InternalRep.Pretty
import L0C.FreshNames

-- | Create a new 'NameSource' that will never produce any of the
-- names used as variables in the given program.
newNameSourceForProg :: Prog -> VNameSource
newNameSourceForProg = newNameSource . progNames

newtype Several a = Several [a]
  deriving (Eq, Ord, Show)

instance Pretty a => Pretty (Several a) where
  ppr (Several [t]) = ppr t
  ppr (Several ts)  = braces $ commasep $ map ppr ts

justOne :: a -> Several a
justOne x = Several [x]
