-- | Possibly convenient facilities for constructing constants.
module Futhark.Representation.AST.Attributes.Constants
       (
         IsValue (..)
       , constant
       , intvalue
       , intvalue'
       , floatvalue
       , intconst
       , floatconst
       )
       where

import Futhark.Representation.AST.Syntax.Core

-- | If a Haskell type is an instance of 'IsValue', it means that a
-- value of that type can be converted to a Futhark 'PrimValue'.
-- This is intended to cut down on boilerplate when writing compiler
-- code - for example, you'll quickly grow tired of writing @Constant
-- (LogVal True) loc@.
class IsValue a where
  value :: a -> PrimValue

instance IsValue Int where
  value = IntValue . Int32Value . fromIntegral

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

instance IsValue Char where
  value = CharValue

-- | Create a 'Constant' 'SubExp' containing the given value.
constant :: IsValue v => v -> SubExp
constant = Constant . value

-- | For reasons of type ambiguity, a specialised 'value' for integers is defined.
intvalue :: IntType -> Integer -> PrimValue
intvalue = ((.).(.)) IntValue intvalue'

-- | Like 'intvalue', but doesn't tack on the 'IntValue' constructor.
intvalue' :: IntType -> Integer -> IntValue
intvalue' Int8 = Int8Value . fromIntegral
intvalue' Int16 = Int16Value . fromIntegral
intvalue' Int32 = Int32Value . fromIntegral
intvalue' Int64 = Int64Value . fromIntegral

-- | Construct a 'FloatValue' corresponding to a 'FloatType', from a 'Rational'.
floatvalue :: FloatType -> Rational -> FloatValue
floatvalue Float32 = Float32Value . fromRational
floatvalue Float64 = Float64Value . fromRational

-- | For reasons of type ambiguity, a specialised 'constant' for integers is defined.
intconst :: IntType -> Integer -> SubExp
intconst t = Constant . intvalue t

-- | Same as for 'intconst'.
floatconst :: FloatType -> Double -> SubExp
floatconst Float32 = Constant . FloatValue . Float32Value . fromRational . toRational
floatconst Float64 = Constant . FloatValue . Float64Value
