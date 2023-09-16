-- | Possibly convenient facilities for constructing constants.
module Futhark.IR.Prop.Constants
  ( IsValue (..),
    constant,
    intConst,
    floatConst,
  )
where

import Futhark.IR.Syntax.Core (SubExp (..))
import Language.Futhark.Primitive

-- | If a Haskell type is an instance of 'IsValue', it means that a
-- value of that type can be converted to a Futhark 'PrimValue'.
-- This is intended to cut down on boilerplate when writing compiler
-- code - for example, you'll quickly grow tired of writing @Constant
-- (LogVal True) loc@.
class IsValue a where
  value :: a -> PrimValue

instance IsValue Int8 where
  value = IntValue . Int8Value

instance IsValue Int16 where
  value = IntValue . Int16Value

instance IsValue Int32 where
  value = IntValue . Int32Value

instance IsValue Int64 where
  value = IntValue . Int64Value

instance IsValue Word8 where
  value = IntValue . Int8Value . fromIntegral

instance IsValue Word16 where
  value = IntValue . Int16Value . fromIntegral

instance IsValue Word32 where
  value = IntValue . Int32Value . fromIntegral

instance IsValue Word64 where
  value = IntValue . Int64Value . fromIntegral

instance IsValue Double where
  value = FloatValue . Float64Value

instance IsValue Float where
  value = FloatValue . Float32Value

instance IsValue Bool where
  value = BoolValue

instance IsValue PrimValue where
  value = id

instance IsValue IntValue where
  value = IntValue

instance IsValue FloatValue where
  value = FloatValue

-- | Create a 'Constant' 'SubExp' containing the given value.
constant :: (IsValue v) => v -> SubExp
constant = Constant . value

-- | Utility definition for reasons of type ambiguity.
intConst :: IntType -> Integer -> SubExp
intConst t v = constant $ intValue t v

-- | Utility definition for reasons of type ambiguity.
floatConst :: FloatType -> Double -> SubExp
floatConst t v = constant $ floatValue t v
