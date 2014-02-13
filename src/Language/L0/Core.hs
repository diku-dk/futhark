-- | This module contains very basic definitions for L0 - so basic,
-- that they can be shared between the internal and external
-- representation.
module Language.L0.Core
  ( BinOp(..)
  , opStr
  , Uniqueness(..)
  )

where

import Data.Hashable
import Data.Monoid

-- | Binary operators.
data BinOp = Plus -- Binary Ops for Numbers
           | Minus
           | Pow
           | Times
           | Divide
           | Mod
           | ShiftR
           | ShiftL
           | Band
           | Xor
           | Bor
           | LogAnd
           | LogOr
           -- Relational Ops for all basic types at least
           | Equal
           | Less
           | Leq
             deriving (Eq, Ord, Enum, Bounded, Show)

-- | The Operator, without whitespace, that corresponds to this
-- @BinOp@.  For example, @opStr Plus@ gives @"+"@.
opStr :: BinOp -> String
opStr Plus = "+"
opStr Minus = "-"
opStr Pow = "pow"
opStr Times = "*"
opStr Divide = "/"
opStr Mod = "%"
opStr ShiftR = ">>"
opStr ShiftL = "<<"
opStr Band = "&"
opStr Xor = "^"
opStr Bor = "|"
opStr LogAnd = "&&"
opStr LogOr = "||"
opStr Equal = "="
opStr Less = "<"
opStr Leq = "<="

-- | The uniqueness attribute of a type.  This essentially indicates
-- whether or not in-place modifications are acceptable.
data Uniqueness = Unique    -- ^ At most one outer reference.
                | Nonunique -- ^ Any number of references.
                  deriving (Eq, Ord, Show)

instance Monoid Uniqueness where
  mempty = Unique
  _ `mappend` Nonunique = Nonunique
  Nonunique `mappend` _ = Nonunique
  u `mappend` _         = u

instance Hashable Uniqueness where
  hashWithSalt salt Unique    = salt
  hashWithSalt salt Nonunique = salt * 2
