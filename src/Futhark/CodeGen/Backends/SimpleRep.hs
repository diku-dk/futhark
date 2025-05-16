{-# LANGUAGE QuasiQuotes #-}
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
    isValidCName,
    escapeName,
    toStorage,
    fromStorage,
    cproduct,
    csum,
    allEqual,
    allTrue,
    scalarToPrim,

    -- * Primitive value operations
    cScalarDefs,

    -- * Storing/restoring values in byte sequences
    storageSize,
    storeValueHeader,
    loadValueHeader,
  )
where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Text qualified as T
import Data.Void (Void)
import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.RTS.C (scalarF16H, scalarH)
import Futhark.Util (hashText, showText, zEncodeText)
import Language.C.Quote.C qualified as C
import Language.C.Syntax qualified as C
import Text.Megaparsec
import Text.Megaparsec.Char (space)

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
primAPIType Unsigned (IntType t) = uintTypeToCType t
primAPIType Signed (IntType t) = intTypeToCType t
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
funName :: Name -> T.Text
funName = ("futrts_" <>) . zEncodeText . nameToText

-- | The type of memory blocks in the default memory space.
defaultMemBlockType :: C.Type
defaultMemBlockType = [C.cty|unsigned char*|]

-- | The name of exposed array type structs.
arrayName :: PrimType -> Signedness -> Int -> T.Text
arrayName pt signed rank =
  prettySigned (signed == Unsigned) pt <> "_" <> prettyText rank <> "d"

-- | Is this name a valid C identifier?  If not, it should be escaped
-- before being emitted into C.
isValidCName :: T.Text -> Bool
isValidCName = maybe True check . T.uncons
  where
    check (c, cs) = isAlpha c && T.all constituent cs
    constituent c = isAlphaNum c || c == '_'

-- | If the provided text is a valid C identifier, then return it
-- verbatim.  Otherwise, escape it such that it becomes valid.
escapeName :: T.Text -> T.Text
escapeName v
  | isValidCName v = v
  | otherwise = zEncodeText v

-- | Valid C identifier name?
valid :: T.Text -> Bool
valid s =
  T.head s /= '_'
    && not (isDigit $ T.head s)
    && T.all ok s
  where
    ok c = isAlphaNum c || c == '_'

-- | Find a nice C type name name for the Futhark type. This solely
-- serves to make the generated header file easy to read, and we can
-- always fall back on an ugly hash.
findPrettyName :: T.Text -> Either String T.Text
findPrettyName =
  either (Left . errorBundlePretty) Right . parse (p <* eof) "type name"
  where
    p :: Parsec Void T.Text T.Text
    p = choice [pArr, pTup, pQual]
    pArr = do
      dims <- some "[]"
      (("arr" <> showText (length dims) <> "d_") <>) <$> p
    pTup = between "(" ")" $ do
      ts <- p `sepBy` pComma
      pure $ "tup" <> showText (length ts) <> "_" <> T.intercalate "_" ts
    pAtom = T.pack <$> some (satisfy (`notElem` ("[]{}(),." :: String)))
    pComma = void $ "," <* space
    -- Rewrite 'x.y' to 'x_y'.
    pQual = do
      x <- pAtom
      choice
        [ "." >> ((x <> "_") <>) <$> pAtom,
          pure x
        ]

-- | The name of exposed opaque types.
opaqueName :: Name -> T.Text
opaqueName "()" = "opaque_unit" -- Hopefully this ad-hoc convenience won't bite us.
opaqueName s
  | Right v <- findPrettyName s',
    valid v =
      "opaque_" <> v
  | valid s' = "opaque_" <> s'
  where
    s' = nameToText s
opaqueName s = "opaque_" <> hashText (nameToText s)

-- | The 'PrimType' (and sign) corresponding to a human-readable scalar
-- type name (e.g. @f64@).  Beware: partial!
scalarToPrim :: T.Text -> (Signedness, PrimType)
scalarToPrim "bool" = (Signed, Bool)
scalarToPrim "i8" = (Signed, IntType Int8)
scalarToPrim "i16" = (Signed, IntType Int16)
scalarToPrim "i32" = (Signed, IntType Int32)
scalarToPrim "i64" = (Signed, IntType Int64)
scalarToPrim "u8" = (Unsigned, IntType Int8)
scalarToPrim "u16" = (Unsigned, IntType Int16)
scalarToPrim "u32" = (Unsigned, IntType Int32)
scalarToPrim "u64" = (Unsigned, IntType Int64)
scalarToPrim "f16" = (Signed, FloatType Float16)
scalarToPrim "f32" = (Signed, FloatType Float32)
scalarToPrim "f64" = (Signed, FloatType Float64)
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

-- | An expression that is true if these are also all true.
allTrue :: [C.Exp] -> C.Exp
allTrue [] = [C.cexp|true|]
allTrue [x] = x
allTrue (x : xs) = [C.cexp|$exp:x && $exp:(allTrue xs)|]

-- | An expression that is true if these expressions are all equal by
-- @==@.
allEqual :: [C.Exp] -> C.Exp
allEqual [x, y] = [C.cexp|$exp:x == $exp:y|]
allEqual (x : y : xs) = [C.cexp|$exp:x == $exp:y && $exp:(allEqual(y:xs))|]
allEqual _ = [C.cexp|true|]

instance C.ToIdent Name where
  toIdent = C.toIdent . zEncodeText . nameToText

-- Orphan!
instance C.ToIdent T.Text where
  toIdent = C.toIdent . T.unpack

instance C.ToIdent VName where
  toIdent = C.toIdent . zEncodeText . prettyText

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
        if x > 0 then [C.cexp|(typename f16)INFINITY|] else [C.cexp|(typename f16)-INFINITY|]
    | isNaN x =
        [C.cexp|(typename f16)NAN|]
    | otherwise =
        [C.cexp|(typename f16)$float:(fromRational (toRational x))|]
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
          $exp:(cproduct dims) * sizeof($ty:(primStorageType pt))|]
  where
    header_size :: Int
    header_size = 1 + 1 + 1 + 4 -- 'b' <version> <num_dims> <type>
    dims = [[C.cexp|$exp:shape[$int:i]|] | i <- [0 .. rank - 1]]

typeStr :: Signedness -> PrimType -> String
typeStr sign pt =
  case (sign, pt) of
    (_, Bool) -> "bool"
    (_, Unit) -> "bool"
    (_, FloatType Float16) -> " f16"
    (_, FloatType Float32) -> " f32"
    (_, FloatType Float64) -> " f64"
    (Signed, IntType Int8) -> "  i8"
    (Signed, IntType Int16) -> " i16"
    (Signed, IntType Int32) -> " i32"
    (Signed, IntType Int64) -> " i64"
    (Unsigned, IntType Int8) -> "  u8"
    (Unsigned, IntType Int16) -> " u16"
    (Unsigned, IntType Int32) -> " u32"
    (Unsigned, IntType Int64) -> " u64"

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
