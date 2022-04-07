{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Simple C runtime representation.
--
-- Most types use the same memory and scalar variable representation.
-- For those that do not (as of this writing, only `Float16`), we use
-- 'primStorageType' for the array element representation, and
-- 'primTypeToCType' for their scalar representation.  Use 'toStorage'
-- and 'fromStorage' to convert back and forth.
module Futhark.CodeGen.Backends.SimpleRep
  ( tupleField,
    funName,
    defaultMemBlockType,
    intTypeToCType,
    primTypeToCType,
    primStorageType,
    primAPIType,
    arrayName,
    opaqueName,
    toStorage,
    fromStorage,
    cproduct,
    csum,
    scalarToPrim,

    -- * Primitive value operations
    cScalarDefs,

    -- * Storing/restoring values in byte sequences
    storageSize,
    storeValueHeader,
    loadValueHeader,
  )
where

import Data.Bits (shiftR, xor)
import Data.Char (isAlphaNum, isDigit, ord)
import qualified Data.Text as T
import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.RTS.C (scalarF16H, scalarH)
import Futhark.Util (zEncodeString)
import qualified Language.C.Quote.C as C
import qualified Language.C.Syntax as C
import Text.Printf

-- | The C type corresponding to a signed integer type.
intTypeToCType :: IntType -> C.Type
intTypeToCType Int8 = [C.cty|typename int8_t|]
intTypeToCType Int16 = [C.cty|typename int16_t|]
intTypeToCType Int32 = [C.cty|typename int32_t|]
intTypeToCType Int64 = [C.cty|typename int64_t|]

-- | The C type corresponding to an unsigned integer type.
uintTypeToCType :: IntType -> C.Type
uintTypeToCType Int8 = [C.cty|typename uint8_t|]
uintTypeToCType Int16 = [C.cty|typename uint16_t|]
uintTypeToCType Int32 = [C.cty|typename uint32_t|]
uintTypeToCType Int64 = [C.cty|typename uint64_t|]

-- | The C type corresponding to a primitive type.  Integers are
-- assumed to be unsigned.
primTypeToCType :: PrimType -> C.Type
primTypeToCType (IntType t) = intTypeToCType t
primTypeToCType (FloatType Float16) = [C.cty|typename f16|]
primTypeToCType (FloatType Float32) = [C.cty|float|]
primTypeToCType (FloatType Float64) = [C.cty|double|]
primTypeToCType Bool = [C.cty|typename bool|]
primTypeToCType Unit = [C.cty|typename bool|]

-- | The C storage type for arrays of this primitive type.
primStorageType :: PrimType -> C.Type
primStorageType (FloatType Float16) = [C.cty|typename uint16_t|]
primStorageType t = primTypeToCType t

-- | The C API corresponding to a primitive type.  Integers are
-- assumed to have the specified sign.
primAPIType :: Signedness -> PrimType -> C.Type
primAPIType TypeUnsigned (IntType t) = uintTypeToCType t
primAPIType TypeDirect (IntType t) = intTypeToCType t
primAPIType _ t = primStorageType t

-- | Convert from scalar to storage representation for the given type.
toStorage :: PrimType -> C.Exp -> C.Exp
toStorage (FloatType Float16) e = [C.cexp|futrts_to_bits16($exp:e)|]
toStorage _ e = e

-- | Convert from storage to scalar representation for the given type.
fromStorage :: PrimType -> C.Exp -> C.Exp
fromStorage (FloatType Float16) e = [C.cexp|futrts_from_bits16($exp:e)|]
fromStorage _ e = e

-- | @tupleField i@ is the name of field number @i@ in a tuple.
tupleField :: Int -> String
tupleField i = "v" ++ show i

-- | @funName f@ is the name of the C function corresponding to
-- the Futhark function @f@.
funName :: Name -> String
funName = ("futrts_" ++) . zEncodeString . nameToString

-- | The type of memory blocks in the default memory space.
defaultMemBlockType :: C.Type
defaultMemBlockType = [C.cty|unsigned char*|]

-- | The name of exposed array type structs.
arrayName :: PrimType -> Signedness -> Int -> String
arrayName pt signed rank =
  prettySigned (signed == TypeUnsigned) pt ++ "_" ++ show rank ++ "d"

-- | The name of exposed opaque types.
opaqueName :: String -> [ValueDesc] -> String
opaqueName s _
  | valid = "opaque_" ++ s
  where
    valid =
      head s /= '_'
        && not (isDigit $ head s)
        && all ok s
    ok c = isAlphaNum c || c == '_'
opaqueName s vds = "opaque_" ++ hash (zipWith xor [0 ..] $ map ord (s ++ concatMap p vds))
  where
    p (ScalarValue pt signed _) =
      show (pt, signed)
    p (ArrayValue _ space pt signed dims) =
      show (space, pt, signed, length dims)

    -- FIXME: a stupid hash algorithm; may have collisions.
    hash =
      printf "%x" . foldl xor 0
        . map
          ( iter . (* 0x45d9f3b)
              . iter
              . (* 0x45d9f3b)
              . iter
              . fromIntegral
          )
    iter x = ((x :: Word32) `shiftR` 16) `xor` x

-- | The 'PrimType' (and sign) correspond to a human-readable scalar
-- type name (e.g. @f64@).  Beware: partial!
scalarToPrim :: T.Text -> (Signedness, PrimType)
scalarToPrim "bool" = (TypeDirect, Bool)
scalarToPrim "i8" = (TypeDirect, IntType Int8)
scalarToPrim "i16" = (TypeDirect, IntType Int16)
scalarToPrim "i32" = (TypeDirect, IntType Int32)
scalarToPrim "i64" = (TypeDirect, IntType Int64)
scalarToPrim "u8" = (TypeUnsigned, IntType Int8)
scalarToPrim "u16" = (TypeUnsigned, IntType Int16)
scalarToPrim "u32" = (TypeUnsigned, IntType Int32)
scalarToPrim "u64" = (TypeUnsigned, IntType Int64)
scalarToPrim "f16" = (TypeDirect, FloatType Float16)
scalarToPrim "f32" = (TypeDirect, FloatType Float32)
scalarToPrim "f64" = (TypeDirect, FloatType Float64)
scalarToPrim tname = error $ "scalarToPrim: " <> T.unpack tname

-- | Return an expression multiplying together the given expressions.
-- If an empty list is given, the expression @1@ is returned.
cproduct :: [C.Exp] -> C.Exp
cproduct [] = [C.cexp|1|]
cproduct (e : es) = foldl mult e es
  where
    mult x y = [C.cexp|$exp:x * $exp:y|]

-- | Return an expression summing the given expressions.
-- If an empty list is given, the expression @0@ is returned.
csum :: [C.Exp] -> C.Exp
csum [] = [C.cexp|0|]
csum (e : es) = foldl mult e es
  where
    mult x y = [C.cexp|$exp:x + $exp:y|]

instance C.ToIdent Name where
  toIdent = C.toIdent . zEncodeString . nameToString

-- Orphan!
instance C.ToIdent T.Text where
  toIdent = C.toIdent . T.unpack

instance C.ToIdent VName where
  toIdent = C.toIdent . zEncodeString . pretty

instance C.ToExp VName where
  toExp v _ = [C.cexp|$id:v|]

instance C.ToExp IntValue where
  toExp (Int8Value k) _ = [C.cexp|(typename int8_t)$int:k|]
  toExp (Int16Value k) _ = [C.cexp|(typename int16_t)$int:k|]
  toExp (Int32Value k) _ = [C.cexp|$int:k|]
  toExp (Int64Value k) _ = [C.cexp|(typename int64_t)$int:k|]

instance C.ToExp FloatValue where
  toExp (Float16Value x) _
    | isInfinite x =
        if x > 0 then [C.cexp|INFINITY|] else [C.cexp|-INFINITY|]
    | isNaN x =
        [C.cexp|NAN|]
    | otherwise =
        [C.cexp|$float:(fromRational (toRational x))|]
  toExp (Float32Value x) _
    | isInfinite x =
        if x > 0 then [C.cexp|INFINITY|] else [C.cexp|-INFINITY|]
    | isNaN x =
        [C.cexp|NAN|]
    | otherwise =
        [C.cexp|$float:x|]
  toExp (Float64Value x) _
    | isInfinite x =
        if x > 0 then [C.cexp|INFINITY|] else [C.cexp|-INFINITY|]
    | isNaN x =
        [C.cexp|NAN|]
    | otherwise =
        [C.cexp|$double:x|]

instance C.ToExp PrimValue where
  toExp (IntValue v) = C.toExp v
  toExp (FloatValue v) = C.toExp v
  toExp (BoolValue True) = C.toExp (1 :: Int8)
  toExp (BoolValue False) = C.toExp (0 :: Int8)
  toExp UnitValue = C.toExp (0 :: Int8)

instance C.ToExp SubExp where
  toExp (Var v) = C.toExp v
  toExp (Constant c) = C.toExp c

-- | Implementations of scalar operations.
cScalarDefs :: T.Text
cScalarDefs = scalarH <> scalarF16H

-- | @storageSize pt rank shape@ produces an expression giving size
-- taken when storing this value in the binary value format.  It is
-- assumed that the @shape@ is an array with @rank@ dimensions.
storageSize :: PrimType -> Int -> C.Exp -> C.Exp
storageSize pt rank shape =
  [C.cexp|$int:header_size +
          $int:rank * sizeof(typename int64_t) +
          $exp:(cproduct dims) * $int:pt_size|]
  where
    header_size, pt_size :: Int
    header_size = 1 + 1 + 1 + 4 -- 'b' <version> <num_dims> <type>
    pt_size = primByteSize pt
    dims = [[C.cexp|$exp:shape[$int:i]|] | i <- [0 .. rank - 1]]

typeStr :: Signedness -> PrimType -> String
typeStr sign pt =
  case (sign, pt) of
    (_, Bool) -> "bool"
    (_, Unit) -> "bool"
    (_, FloatType Float16) -> " f16"
    (_, FloatType Float32) -> " f32"
    (_, FloatType Float64) -> " f64"
    (TypeDirect, IntType Int8) -> "  i8"
    (TypeDirect, IntType Int16) -> " i16"
    (TypeDirect, IntType Int32) -> " i32"
    (TypeDirect, IntType Int64) -> " i64"
    (TypeUnsigned, IntType Int8) -> "  u8"
    (TypeUnsigned, IntType Int16) -> " u16"
    (TypeUnsigned, IntType Int32) -> " u32"
    (TypeUnsigned, IntType Int64) -> " u64"

-- | Produce code for storing the header (everything besides the
-- actual payload) for a value of this type.
storeValueHeader :: Signedness -> PrimType -> Int -> C.Exp -> C.Exp -> [C.Stm]
storeValueHeader sign pt rank shape dest =
  [C.cstms|
          *$exp:dest++ = 'b';
          *$exp:dest++ = 2;
          *$exp:dest++ = $int:rank;
          memcpy($exp:dest, $string:(typeStr sign pt), 4);
          $exp:dest += 4;
          $stms:copy_shape
          |]
  where
    copy_shape
      | rank == 0 = []
      | otherwise =
          [C.cstms|
                memcpy($exp:dest, $exp:shape, $int:rank*sizeof(typename int64_t));
                $exp:dest += $int:rank*sizeof(typename int64_t);|]

-- | Produce code for loading the header (everything besides the
-- actual payload) for a value of this type.
loadValueHeader :: Signedness -> PrimType -> Int -> C.Exp -> C.Exp -> [C.Stm]
loadValueHeader sign pt rank shape src =
  [C.cstms|
     err |= (*$exp:src++ != 'b');
     err |= (*$exp:src++ != 2);
     err |= (*$exp:src++ != $exp:rank);
     err |= (memcmp($exp:src, $string:(typeStr sign pt), 4) != 0);
     $exp:src += 4;
     if (err == 0) {
       $stms:load_shape
       $exp:src += $int:rank*sizeof(typename int64_t);
     }|]
  where
    load_shape
      | rank == 0 = []
      | otherwise = [C.cstms|memcpy($exp:shape, src, $int:rank*sizeof(typename int64_t));|]
