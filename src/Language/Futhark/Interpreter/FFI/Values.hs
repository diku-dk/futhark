module Language.Futhark.Interpreter.FFI.Values
  ( PrimitiveType (..),
    PrimitiveValue (..),
    Type (..),
    Value (..),
    Direction (..),
    Location (..),
    InType,
    InValue,
    primitiveType,
    primitiveTypeName,
    toTuple,
    toTupleType,
    toPrimValue,
    fromPrimValue,
    indexLocation,
    projectLocation,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Util.NDArray (NDArray)
import Language.Futhark.Core (Half, Int16, Int32, Int64, Int8, Word16, Word32, Word64, Word8)
import Language.Futhark.Interpreter.FFI.UIDs (ValueUID)
import Language.Futhark.Syntax qualified as I

data PrimitiveType
  = TInt8
  | TInt16
  | TInt32
  | TInt64
  | TUInt8
  | TUInt16
  | TUInt32
  | TUInt64
  | TFloat16
  | TFloat32
  | TFloat64
  | TBool
  deriving (Show, Eq, Ord)

data PrimitiveValue
  = Int8 Int8
  | Int16 Int16
  | Int32 Int32
  | Int64 Int64
  | UInt8 Word8
  | UInt16 Word16
  | UInt32 Word32
  | UInt64 Word64
  | Float16 Half
  | Float32 Float
  | Float64 Double
  | Bool Bool
  deriving (Show, Eq, Ord)

data Type a
  = TAtom a
  | TArray (Type a)
  | TRecord (M.Map T.Text (Type a))
  | TSum (M.Map T.Text [Type a])
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data Value a
  = Atom a
  | Array (NDArray (Value a))
  | Record (M.Map T.Text (Value a))
  | Sum T.Text [Value a]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data Direction
  = Index [Int]
  | Field T.Text
  | VariantValue T.Text Int
  deriving (Show, Eq, Ord)

type Directions = [Direction]

data Location = Location ValueUID Directions
  deriving (Show, Eq, Ord)

indexLocation :: [Int] -> Location -> Location
indexLocation i (Location vid ds) = Location vid $ Index i : ds

projectLocation :: T.Text -> Location -> Location
projectLocation f (Location vid ds) = Location vid $ Field f : ds

type InType = Type PrimitiveType

type InValue = Value PrimitiveValue

primitiveType :: PrimitiveValue -> PrimitiveType
primitiveType (Int8 _) = TInt8
primitiveType (Int16 _) = TInt16
primitiveType (Int32 _) = TInt32
primitiveType (Int64 _) = TInt64
primitiveType (UInt8 _) = TUInt8
primitiveType (UInt16 _) = TUInt16
primitiveType (UInt32 _) = TUInt32
primitiveType (UInt64 _) = TUInt64
primitiveType (Float16 _) = TFloat16
primitiveType (Float32 _) = TFloat32
primitiveType (Float64 _) = TFloat64
primitiveType (Bool _) = TBool

primitiveTypeName :: PrimitiveType -> String
primitiveTypeName TInt8 = "i8"
primitiveTypeName TInt16 = "i16"
primitiveTypeName TInt32 = "i32"
primitiveTypeName TInt64 = "i64"
primitiveTypeName TUInt8 = "u8"
primitiveTypeName TUInt16 = "u16"
primitiveTypeName TUInt32 = "u32"
primitiveTypeName TUInt64 = "u64"
primitiveTypeName TFloat16 = "f16"
primitiveTypeName TFloat32 = "f32"
primitiveTypeName TFloat64 = "f64"
primitiveTypeName TBool = "bool"

toTuple :: [Value a] -> Value a
toTuple vs = Record $ M.fromList $ zip (map T.show ([0 ..] :: [Int])) vs

toTupleType :: [Type a] -> Type a
toTupleType ts = TRecord $ M.fromList $ zip (map T.show ([0 ..] :: [Int])) ts

toPrimValue :: PrimitiveValue -> I.PrimValue
toPrimValue (Int8 v) = I.SignedValue $ I.Int8Value v
toPrimValue (Int16 v) = I.SignedValue $ I.Int16Value v
toPrimValue (Int32 v) = I.SignedValue $ I.Int32Value v
toPrimValue (Int64 v) = I.SignedValue $ I.Int64Value v
toPrimValue (UInt8 v) = I.UnsignedValue $ I.Int8Value $ fromIntegral v
toPrimValue (UInt16 v) = I.UnsignedValue $ I.Int16Value $ fromIntegral v
toPrimValue (UInt32 v) = I.UnsignedValue $ I.Int32Value $ fromIntegral v
toPrimValue (UInt64 v) = I.UnsignedValue $ I.Int64Value $ fromIntegral v
toPrimValue (Float16 v) = I.FloatValue $ I.Float16Value v
toPrimValue (Float32 v) = I.FloatValue $ I.Float32Value v
toPrimValue (Float64 v) = I.FloatValue $ I.Float64Value v
toPrimValue (Bool v) = I.BoolValue v

fromPrimValue :: I.PrimValue -> PrimitiveValue
fromPrimValue (I.SignedValue (I.Int8Value v)) = Int8 v
fromPrimValue (I.SignedValue (I.Int16Value v)) = Int16 v
fromPrimValue (I.SignedValue (I.Int32Value v)) = Int32 v
fromPrimValue (I.SignedValue (I.Int64Value v)) = Int64 v
fromPrimValue (I.UnsignedValue (I.Int8Value v)) = UInt8 $ fromIntegral v
fromPrimValue (I.UnsignedValue (I.Int16Value v)) = UInt16 $ fromIntegral v
fromPrimValue (I.UnsignedValue (I.Int32Value v)) = UInt32 $ fromIntegral v
fromPrimValue (I.UnsignedValue (I.Int64Value v)) = UInt64 $ fromIntegral v
fromPrimValue (I.FloatValue (I.Float16Value v)) = Float16 v
fromPrimValue (I.FloatValue (I.Float32Value v)) = Float32 v
fromPrimValue (I.FloatValue (I.Float64Value v)) = Float64 v
fromPrimValue (I.BoolValue v) = Bool v
