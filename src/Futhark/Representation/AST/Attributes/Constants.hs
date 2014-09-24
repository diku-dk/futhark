module Futhark.Representation.AST.Attributes.Constants
       (
         IsValue (..)
       , constant
       , intconst
       )
       where

import Data.Loc

import Futhark.Representation.AST.Syntax

-- | If a Haskell type is an instance of 'IsValue', it means that a
-- value of that type can be converted to a Futhark 'Value'.  This is
-- intended to cut down on boilerplate when writing compiler code -
-- for example, you'll quickly grow tired of writing @Constant
-- (BasicVal $ LogVal True) loc@.
class IsValue a where
  value :: a -> Value

instance IsValue Int where
  value = BasicVal . IntVal

instance IsValue Double where
  value = BasicVal . RealVal

instance IsValue Bool where
  value = BasicVal . LogVal

instance IsValue Char where
  value = BasicVal . CharVal

-- | Create a 'Constant' 'SubExp' containing the given value.
constant :: IsValue v => v -> SrcLoc -> SubExp
constant = Constant . value

-- | For reasons of type ambiguity, a specialised 'constant' for integers is defined.
intconst :: Int -> SrcLoc -> SubExp
intconst = constant
