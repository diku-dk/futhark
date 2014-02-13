module L0C.InternalRep
  ( module Language.L0.Misc
  , module L0C.InternalRep.Syntax
  , module L0C.InternalRep.Attributes
  , module L0C.InternalRep.Traversals
  , module L0C.InternalRep.Pretty

  , newNameSourceForProg

  , GenType
  , Several(..)
  , justOne
  )
where

import Text.PrettyPrint.Mainland

import Language.L0.Misc
import L0C.InternalRep.Syntax
import L0C.InternalRep.Attributes
import L0C.InternalRep.Traversals
import L0C.InternalRep.Pretty
import L0C.FreshNames

-- | Create a new 'NameSource' that will never produce any of the
-- names used as variables in the given program.
newNameSourceForProg :: Prog -> VNameSource
newNameSourceForProg = newNameSource . progNames

-- | A known type parametrised over its aliasing information (of kind
-- @* -> *@).
type GenType als = TypeBase (als VName)

newtype Several a = Several [a]

instance Pretty a => Pretty (Several a) where
  ppr (Several ts) = ppPat ts

justOne :: a -> Several a
justOne x = Several [x]
