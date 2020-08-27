-- | Possibly convenient facilities for constructing constants.
module Futhark.IR.Prop.Constants
       (
         IsValue (..)
       , constant
       , intConst
       , floatConst
       )
       where

import Futhark.IR.Syntax.Core

-- | If a Haskell type is an instance of 'IsValue', it means that a
-- value of that type can be converted to a Futhark 'PrimValue'.
-- This is intended to cut down on boilerplate when writing compiler
-- code - for example, you'll quickly grow tired of writing @Constant
-- (LogVal True) loc@.
class IsValue a where
  value :: a -> PrimValue a

instance IsValue Int8 where
  value = IntValue . Int8Value

instance IsValue Int16 where
  value = IntValue . Int16Value

instance IsValue Int32 where
  value = IntValue . Int32Value

instance IsValue Int64 where
  value = IntValue . Int64Value

instance IsValue Double where
  value = FloatValue . Float64Value

instance IsValue Float where
  value = FloatValue . Float32Value

instance IsValue Bool where
  value = BoolValue

-- | Create a 'Constant' 'SubExp' containing the given value.
constant :: IsValue v => v -> SubExp
constant = Constant . UT . value

-- | Utility definition for reasons of type ambiguity.
intConst :: IntType t -> t -> SubExp
intConst t v = Constant $ UT $ IntValue $ intValue t v

-- | Utility definition for reasons of type ambiguity.
floatConst :: FloatType t -> t -> SubExp
floatConst t v = Constant $ UT $ FloatValue $ floatValue t v
