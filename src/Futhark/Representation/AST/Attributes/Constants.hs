-- | Possibly convenient facilities for constructing constants.
module Futhark.Representation.AST.Attributes.Constants
       (
         IsValue (..)
       , constant
       , intconst
       )
       where

import Futhark.Representation.AST.Syntax.Core

-- | If a Haskell type is an instance of 'IsValue', it means that a
-- value of that type can be converted to a Futhark 'BasicValue'.
-- This is intended to cut down on boilerplate when writing compiler
-- code - for example, you'll quickly grow tired of writing @Constant
-- (LogVal True) loc@.
class IsValue a where
  value :: a -> BasicValue

instance IsValue Int where
  value = IntVal . fromIntegral

instance IsValue Int32 where
  value = IntVal

instance IsValue Double where
  value = Float64Val

instance IsValue Float where
  value = Float32Val

instance IsValue Bool where
  value = LogVal

instance IsValue Char where
  value = CharVal

-- | Create a 'Constant' 'SubExp' containing the given value.
constant :: IsValue v => v -> SubExp
constant = Constant . value

-- | For reasons of type ambiguity, a specialised 'constant' for integers is defined.
intconst :: Int -> SubExp
intconst = constant
