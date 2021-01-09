{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Trustworthy #-}

-- | This module defines an efficient value representation as well as
-- parsing and comparison functions.  This is because the standard
-- Futhark parser is not able to cope with large values (like arrays
-- that are tens of megabytes in size).  The representation defined
-- here does not support tuples, so don't use those as input/output
-- for your test programs.
module Futhark.Test.Values
  ( Value (..),
    Vector,

    -- * Reading Values
    readValues,

    -- * Types of values
    ValueType (..),
    valueType,
    valueShape,

    -- * Manipulating values
    valueElems,

    -- * Comparing Values
    compareValues,
    compareValues1,
    Mismatch,
  )
where

import Control.Monad
import Control.Monad.ST
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (chr, isSpace, ord)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Vector.Generic (freeze)
import qualified Data.Vector.Storable as SVec
import Data.Vector.Storable.ByteString (byteStringToVector, vectorToByteString)
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector.Unboxed.Mutable as UMVec
import Futhark.IR.Primitive (PrimValue)
import Futhark.IR.Prop.Constants (IsValue (..))
import Futhark.Util (maybeHead)
import Futhark.Util.Loc (Pos (..))
import Futhark.Util.Pretty
import qualified Futhark.Util.Pretty as PP
import Language.Futhark.Parser.Lexer
import Language.Futhark.Pretty ()
import qualified Language.Futhark.Syntax as F

type STVector s = UMVec.STVector s

-- | The value vector type.
type Vector = SVec.Vector

-- | An efficiently represented Futhark value.  Use 'pretty' to get a
-- human-readable representation, and v'put' to obtain binary a
-- representation.
data Value
  = Int8Value (Vector Int) (Vector Int8)
  | Int16Value (Vector Int) (Vector Int16)
  | Int32Value (Vector Int) (Vector Int32)
  | Int64Value (Vector Int) (Vector Int64)
  | Word8Value (Vector Int) (Vector Word8)
  | Word16Value (Vector Int) (Vector Word16)
  | Word32Value (Vector Int) (Vector Word32)
  | Word64Value (Vector Int) (Vector Word64)
  | Float32Value (Vector Int) (Vector Float)
  | Float64Value (Vector Int) (Vector Double)
  | BoolValue (Vector Int) (Vector Bool)
  deriving (Show)

binaryFormatVersion :: Word8
binaryFormatVersion = 2

instance Binary Value where
  put (Int8Value shape vs) = putBinaryValue "  i8" shape vs
  put (Int16Value shape vs) = putBinaryValue " i16" shape vs
  put (Int32Value shape vs) = putBinaryValue " i32" shape vs
  put (Int64Value shape vs) = putBinaryValue " i64" shape vs
  put (Word8Value shape vs) = putBinaryValue "  u8" shape vs
  put (Word16Value shape vs) = putBinaryValue " u16" shape vs
  put (Word32Value shape vs) = putBinaryValue " u32" shape vs
  put (Word64Value shape vs) = putBinaryValue " u64" shape vs
  put (Float32Value shape vs) = putBinaryValue " f32" shape vs
  put (Float64Value shape vs) = putBinaryValue " f64" shape vs
  -- Bool must be treated specially because the Storable instance
  -- uses four bytes.
  put (BoolValue shape vs) = putBinaryValue "bool" shape $ SVec.map boolToInt8 vs
    where
      boolToInt8 True = 1 :: Int8
      boolToInt8 False = 0

  get = do
    first <- getInt8
    version <- getWord8
    rank <- getInt8

    unless (chr (fromIntegral first) == 'b') $
      fail "Input does not begin with ASCII 'b'."
    unless (version == binaryFormatVersion) $
      fail $ "Expecting binary format version 1; found version: " ++ show version
    unless (rank >= 0) $
      fail $ "Rank must be non-negative, but is: " ++ show rank

    type_f <- getLazyByteString 4

    shape <- replicateM (fromIntegral rank) $ fromIntegral <$> getInt64le
    let num_elems = product shape
        shape' = SVec.fromList shape

    case LBS.unpack type_f of
      "  i8" -> get' (Int8Value shape') num_elems 1
      " i16" -> get' (Int16Value shape') num_elems 2
      " i32" -> get' (Int32Value shape') num_elems 4
      " i64" -> get' (Int64Value shape') num_elems 8
      "  u8" -> get' (Word8Value shape') num_elems 1
      " u16" -> get' (Word16Value shape') num_elems 2
      " u32" -> get' (Word32Value shape') num_elems 4
      " u64" -> get' (Word64Value shape') num_elems 8
      " f32" -> get' (Float32Value shape') num_elems 4
      " f64" -> get' (Float64Value shape') num_elems 8
      -- Bool must be treated specially because the Storable instance
      -- uses four bytes.
      "bool" -> BoolValue shape' . SVec.map int8ToBool . byteStringToVector . BS.copy <$> getByteString num_elems
      s -> fail $ "Cannot parse binary values of type " ++ show s
    where
      -- The copy is to ensure that the bytestring is properly
      -- aligned.
      get' mk num_elems elem_size =
        mk . byteStringToVector . BS.copy <$> getByteString (num_elems * elem_size)

      int8ToBool :: Int8 -> Bool
      int8ToBool = (/= 0)

putBinaryValue ::
  SVec.Storable a =>
  String ->
  Vector Int ->
  Vector a ->
  Put
putBinaryValue tstr shape vs = do
  putInt8 $ fromIntegral $ ord 'b'
  putWord8 binaryFormatVersion
  putWord8 $ fromIntegral $ SVec.length shape
  mapM_ (putInt8 . fromIntegral . ord) tstr
  putByteString $ vectorToByteString shape
  putByteString $ vectorToByteString vs

instance PP.Pretty Value where
  ppr v
    | product (valueShape v) == 0 =
      text "empty"
        <> parens (dims <> ppr (valueElemType v))
    where
      dims = mconcat $ map (brackets . ppr) $ valueShape v
  ppr (Int8Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (Int16Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (Int32Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (Int64Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (Word8Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (Word16Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (Word32Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (Word64Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (Float32Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (Float64Value shape vs) = pprArray (SVec.toList shape) vs
  ppr (BoolValue shape vs) = pprArray (SVec.toList shape) vs

pprArray :: (SVec.Storable a, F.IsPrimValue a) => [Int] -> SVec.Vector a -> Doc
pprArray [] vs =
  ppr $ F.primValue $ SVec.head vs
pprArray (d : ds) vs =
  brackets $ cat $ punctuate separator $ map (pprArray ds . slice) [0 .. d -1]
  where
    slice_size = product ds
    slice i = SVec.slice (i * slice_size) slice_size vs
    separator
      | null ds = comma <> space
      | otherwise = comma <> line

-- | A representation of the simple values we represent in this module.
data ValueType = ValueType [Int] F.PrimType
  deriving (Show)

instance PP.Pretty ValueType where
  ppr (ValueType ds t) = mconcat (map pprDim ds) <> ppr t
    where
      pprDim d = brackets $ ppr d

-- | Get the type of a value.
valueType :: Value -> ValueType
valueType v = ValueType (valueShape v) $ valueElemType v

valueElemType :: Value -> F.PrimType
valueElemType Int8Value {} = F.Signed F.Int8
valueElemType Int16Value {} = F.Signed F.Int16
valueElemType Int32Value {} = F.Signed F.Int32
valueElemType Int64Value {} = F.Signed F.Int64
valueElemType Word8Value {} = F.Unsigned F.Int8
valueElemType Word16Value {} = F.Unsigned F.Int16
valueElemType Word32Value {} = F.Unsigned F.Int32
valueElemType Word64Value {} = F.Unsigned F.Int64
valueElemType Float32Value {} = F.FloatType F.Float32
valueElemType Float64Value {} = F.FloatType F.Float64
valueElemType BoolValue {} = F.Bool

-- | The shape of a value.  Empty list in case of a scalar.
valueShape :: Value -> [Int]
valueShape (Int8Value shape _) = SVec.toList shape
valueShape (Int16Value shape _) = SVec.toList shape
valueShape (Int32Value shape _) = SVec.toList shape
valueShape (Int64Value shape _) = SVec.toList shape
valueShape (Word8Value shape _) = SVec.toList shape
valueShape (Word16Value shape _) = SVec.toList shape
valueShape (Word32Value shape _) = SVec.toList shape
valueShape (Word64Value shape _) = SVec.toList shape
valueShape (Float32Value shape _) = SVec.toList shape
valueShape (Float64Value shape _) = SVec.toList shape
valueShape (BoolValue shape _) = SVec.toList shape

-- | Produce a list of the immediate elements of the value.  That is,
-- a 2D array will produce a list of 1D values.  While lists are of
-- course inefficient, the actual values are just slices of the
-- original value, which makes them fairly efficient.
valueElems :: Value -> [Value]
valueElems v
  | n : ns <- valueShape v =
    let k = product ns
        slices mk vs =
          [ mk (SVec.fromList ns) $
              SVec.slice (k * i) k vs
            | i <- [0 .. n -1]
          ]
     in case v of
          Int8Value _ vs -> slices Int8Value vs
          Int16Value _ vs -> slices Int16Value vs
          Int32Value _ vs -> slices Int32Value vs
          Int64Value _ vs -> slices Int64Value vs
          Word8Value _ vs -> slices Word8Value vs
          Word16Value _ vs -> slices Word16Value vs
          Word32Value _ vs -> slices Word32Value vs
          Word64Value _ vs -> slices Word64Value vs
          Float32Value _ vs -> slices Float32Value vs
          Float64Value _ vs -> slices Float64Value vs
          BoolValue _ vs -> slices BoolValue vs
  | otherwise =
    []

-- The parser

dropRestOfLine, dropSpaces :: LBS.ByteString -> LBS.ByteString
dropRestOfLine = LBS.drop 1 . LBS.dropWhile (/= '\n')
dropSpaces t = case LBS.dropWhile isSpace t of
  t'
    | "--" `LBS.isPrefixOf` t' -> dropSpaces $ dropRestOfLine t'
    | otherwise -> t'

type ReadValue v = LBS.ByteString -> Maybe (v, LBS.ByteString)

symbol :: Char -> LBS.ByteString -> Maybe LBS.ByteString
symbol c t
  | Just (c', t') <- LBS.uncons t, c' == c = Just $ dropSpaces t'
  | otherwise = Nothing

lexeme :: LBS.ByteString -> LBS.ByteString -> Maybe LBS.ByteString
lexeme l t
  | l `LBS.isPrefixOf` t = Just $ dropSpaces $ LBS.drop (LBS.length l) t
  | otherwise = Nothing

-- (Used elements, shape, elements, remaining input)
type State s v = (Int, Vector Int, STVector s v, LBS.ByteString)

readArrayElemsST ::
  UMVec.Unbox v =>
  Int ->
  Int ->
  ReadValue v ->
  State s v ->
  ST s (Maybe (Int, State s v))
readArrayElemsST j r rv s = do
  ms <- readRankedArrayOfST r rv s
  case ms of
    Just (i, shape, arr, t)
      | Just t' <- symbol ',' t -> do
        next <- readArrayElemsST (j + 1) r rv (i, shape, arr, t')
        -- Not OK to have zero values after a comma.
        case next of
          Just (0, _) -> return Nothing
          _ -> return next
      | otherwise -> return $ Just (j, (i, shape, arr, t))
    _ ->
      return $ Just (0, s)

updateShape :: Int -> Int -> Vector Int -> Maybe (Vector Int)
updateShape d n shape
  | old_n < 0 = Just $ shape SVec.// [(r - d, n)]
  | old_n == n = Just shape
  | otherwise = Nothing
  where
    r = SVec.length shape
    old_n = shape SVec.! (r - d)

growIfFilled :: UVec.Unbox v => Int -> STVector s v -> ST s (STVector s v)
growIfFilled i arr =
  if i >= capacity
    then UMVec.grow arr capacity
    else return arr
  where
    capacity = UMVec.length arr

readRankedArrayOfST ::
  UMVec.Unbox v =>
  Int ->
  ReadValue v ->
  State s v ->
  ST s (Maybe (State s v))
readRankedArrayOfST 0 rv (i, shape, arr, t)
  | Just (v, t') <- rv t = do
    arr' <- growIfFilled i arr
    UMVec.write arr' i v
    return $ Just (i + 1, shape, arr', t')
readRankedArrayOfST r rv (i, shape, arr, t)
  | Just t' <- symbol '[' t = do
    ms <- readArrayElemsST 1 (r -1) rv (i, shape, arr, t')
    return $ do
      (j, s) <- ms
      closeArray r j s
readRankedArrayOfST _ _ _ =
  return Nothing

closeArray :: Int -> Int -> State s v -> Maybe (State s v)
closeArray r j (i, shape, arr, t) = do
  t' <- symbol ']' t
  shape' <- updateShape r j shape
  return (i, shape', arr, t')

readRankedArrayOf ::
  (UMVec.Unbox v, SVec.Storable v) =>
  Int ->
  ReadValue v ->
  LBS.ByteString ->
  Maybe (Vector Int, Vector v, LBS.ByteString)
readRankedArrayOf r rv t = runST $ do
  arr <- UMVec.new 1024
  ms <- readRankedArrayOfST r rv (0, SVec.replicate r (-1), arr, t)
  case ms of
    Just (i, shape, arr', t') -> do
      arr'' <- freeze (UMVec.slice 0 i arr')
      return $ Just (shape, UVec.convert arr'', t')
    Nothing ->
      return Nothing

-- | A character that can be part of a value.  This doesn't work for
-- string and character literals.
constituent :: Char -> Bool
constituent ',' = False
constituent ']' = False
constituent ')' = False
constituent c = not $ isSpace c

readIntegral :: Integral int => (Token -> Maybe int) -> ReadValue int
readIntegral f t = do
  v <- case fst <$> scanTokens (Pos "" 1 1 0) a of
    Right [L _ NEGATE, L _ (INTLIT x)] -> Just $ negate $ fromIntegral x
    Right [L _ (INTLIT x)] -> Just $ fromIntegral x
    Right [L _ tok] -> f tok
    Right [L _ NEGATE, L _ tok] -> negate <$> f tok
    _ -> Nothing
  return (v, dropSpaces b)
  where
    (a, b) = LBS.span constituent t

readInt8 :: ReadValue Int8
readInt8 = readIntegral f
  where
    f (I8LIT x) = Just x
    f _ = Nothing

readInt16 :: ReadValue Int16
readInt16 = readIntegral f
  where
    f (I16LIT x) = Just x
    f _ = Nothing

readInt32 :: ReadValue Int32
readInt32 = readIntegral f
  where
    f (I32LIT x) = Just x
    f _ = Nothing

readInt64 :: ReadValue Int64
readInt64 = readIntegral f
  where
    f (I64LIT x) = Just x
    f _ = Nothing

readWord8 :: ReadValue Word8
readWord8 = readIntegral f
  where
    f (U8LIT x) = Just x
    f _ = Nothing

readWord16 :: ReadValue Word16
readWord16 = readIntegral f
  where
    f (U16LIT x) = Just x
    f _ = Nothing

readWord32 :: ReadValue Word32
readWord32 = readIntegral f
  where
    f (U32LIT x) = Just x
    f _ = Nothing

readWord64 :: ReadValue Word64
readWord64 = readIntegral f
  where
    f (U64LIT x) = Just x
    f _ = Nothing

readFloat :: RealFloat float => ([Token] -> Maybe float) -> ReadValue float
readFloat f t = do
  v <- case map unLoc . fst <$> scanTokens (Pos "" 1 1 0) a of
    Right [NEGATE, FLOATLIT x] -> Just $ negate $ fromDouble x
    Right [FLOATLIT x] -> Just $ fromDouble x
    Right (NEGATE : toks) -> negate <$> f toks
    Right toks -> f toks
    _ -> Nothing
  return (v, dropSpaces b)
  where
    (a, b) = LBS.span constituent t
    fromDouble = uncurry encodeFloat . decodeFloat
    unLoc (L _ x) = x

readFloat32 :: ReadValue Float
readFloat32 = readFloat lexFloat32
  where
    lexFloat32 [F32LIT x] = Just x
    lexFloat32 [ID "f32", PROJ_FIELD "inf"] = Just $ 1 / 0
    lexFloat32 [ID "f32", PROJ_FIELD "nan"] = Just $ 0 / 0
    lexFloat32 _ = Nothing

readFloat64 :: ReadValue Double
readFloat64 = readFloat lexFloat64
  where
    lexFloat64 [F64LIT x] = Just x
    lexFloat64 [ID "f64", PROJ_FIELD "inf"] = Just $ 1 / 0
    lexFloat64 [ID "f64", PROJ_FIELD "nan"] = Just $ 0 / 0
    lexFloat64 _ = Nothing

readBool :: ReadValue Bool
readBool t = do
  v <- case fst <$> scanTokens (Pos "" 1 1 0) a of
    Right [L _ TRUE] -> Just True
    Right [L _ FALSE] -> Just False
    _ -> Nothing
  return (v, dropSpaces b)
  where
    (a, b) = LBS.span constituent t

readPrimType :: ReadValue String
readPrimType t = do
  pt <- case fst <$> scanTokens (Pos "" 1 1 0) a of
    Right [L _ (ID s)] -> Just $ F.nameToString s
    _ -> Nothing
  return (pt, dropSpaces b)
  where
    (a, b) = LBS.span constituent t

readEmptyArrayOfShape :: [Int] -> LBS.ByteString -> Maybe (Value, LBS.ByteString)
readEmptyArrayOfShape shape t
  | Just t' <- symbol '[' t,
    Just (d, t'') <- readIntegral (const Nothing) t',
    Just t''' <- symbol ']' t'' =
    readEmptyArrayOfShape (shape ++ [d]) t'''
  | otherwise = do
    (pt, t') <- readPrimType t
    guard $ elem 0 shape
    v <- case pt of
      "i8" -> Just $ Int8Value (SVec.fromList shape) SVec.empty
      "i16" -> Just $ Int16Value (SVec.fromList shape) SVec.empty
      "i32" -> Just $ Int32Value (SVec.fromList shape) SVec.empty
      "i64" -> Just $ Int64Value (SVec.fromList shape) SVec.empty
      "u8" -> Just $ Word8Value (SVec.fromList shape) SVec.empty
      "u16" -> Just $ Word16Value (SVec.fromList shape) SVec.empty
      "u32" -> Just $ Word32Value (SVec.fromList shape) SVec.empty
      "u64" -> Just $ Word64Value (SVec.fromList shape) SVec.empty
      "f32" -> Just $ Float32Value (SVec.fromList shape) SVec.empty
      "f64" -> Just $ Float64Value (SVec.fromList shape) SVec.empty
      "bool" -> Just $ BoolValue (SVec.fromList shape) SVec.empty
      _ -> Nothing
    return (v, t')

readEmptyArray :: LBS.ByteString -> Maybe (Value, LBS.ByteString)
readEmptyArray t = do
  t' <- symbol '(' =<< lexeme "empty" t
  (v, t'') <- readEmptyArrayOfShape [] t'
  t''' <- symbol ')' t''
  return (v, t''')

readValue :: LBS.ByteString -> Maybe (Value, LBS.ByteString)
readValue full_t
  | Right (t', _, v) <- decodeOrFail full_t =
    Just (v, dropSpaces t')
  | otherwise = readEmptyArray full_t `mplus` insideBrackets 0 full_t
  where
    insideBrackets r t = maybe (tryValueAndReadValue r t) (insideBrackets (r + 1)) $ symbol '[' t
    tryWith f mk r t
      | Just _ <- f t = do
        (shape, arr, rest_t) <- readRankedArrayOf r f full_t
        return (mk shape arr, rest_t)
      | otherwise = Nothing
    tryValueAndReadValue r t =
      -- 32-bit signed integers come first such that we parse
      -- unsuffixed integer constants as of that type.
      tryWith readInt32 Int32Value r t
        `mplus` tryWith readInt8 Int8Value r t
        `mplus` tryWith readInt16 Int16Value r t
        `mplus` tryWith readInt64 Int64Value r t
        `mplus` tryWith readWord8 Word8Value r t
        `mplus` tryWith readWord16 Word16Value r t
        `mplus` tryWith readWord32 Word32Value r t
        `mplus` tryWith readWord64 Word64Value r t
        `mplus` tryWith readFloat64 Float64Value r t
        `mplus` tryWith readFloat32 Float32Value r t
        `mplus` tryWith readBool BoolValue r t

-- | Parse Futhark values from the given bytestring.
readValues :: LBS.ByteString -> Maybe [Value]
readValues = readValues' . dropSpaces
  where
    readValues' t
      | LBS.null t = Just []
      | otherwise = do
        (a, t') <- readValue t
        (a :) <$> readValues' t'

-- Comparisons

-- | Two values differ in some way.  The 'Show' instance produces a
-- human-readable explanation.
data Mismatch
  = -- | The position the value number and a flat index
    -- into the array.
    PrimValueMismatch (Int, Int) PrimValue PrimValue
  | ArrayShapeMismatch Int [Int] [Int]
  | TypeMismatch Int String String
  | ValueCountMismatch Int Int

instance Show Mismatch where
  show (PrimValueMismatch (i, j) got expected) =
    explainMismatch (i, j) "" got expected
  show (ArrayShapeMismatch i got expected) =
    explainMismatch i "array of shape " got expected
  show (TypeMismatch i got expected) =
    explainMismatch i "value of type " got expected
  show (ValueCountMismatch got expected) =
    "Expected " ++ show expected ++ " values, got " ++ show got

-- | A human-readable description of how two values are not the same.
explainMismatch :: (Show i, PP.Pretty a) => i -> String -> a -> a -> String
explainMismatch i what got expected =
  "Value " ++ show i ++ " expected " ++ what ++ PP.pretty expected ++ ", got " ++ PP.pretty got

-- | Compare two sets of Futhark values for equality.  Shapes and
-- types must also match.
compareValues :: [Value] -> [Value] -> [Mismatch]
compareValues got expected
  | n /= m = [ValueCountMismatch n m]
  | otherwise = concat $ zipWith3 compareValue [0 ..] got expected
  where
    n = length got
    m = length expected

-- | As 'compareValues', but only reports one mismatch.
compareValues1 :: [Value] -> [Value] -> Maybe Mismatch
compareValues1 got expected = maybeHead $ compareValues got expected

compareValue :: Int -> Value -> Value -> [Mismatch]
compareValue i got_v expected_v
  | valueShape got_v == valueShape expected_v =
    case (got_v, expected_v) of
      (Int8Value _ got_vs, Int8Value _ expected_vs) ->
        compareNum 1 got_vs expected_vs
      (Int16Value _ got_vs, Int16Value _ expected_vs) ->
        compareNum 1 got_vs expected_vs
      (Int32Value _ got_vs, Int32Value _ expected_vs) ->
        compareNum 1 got_vs expected_vs
      (Int64Value _ got_vs, Int64Value _ expected_vs) ->
        compareNum 1 got_vs expected_vs
      (Word8Value _ got_vs, Word8Value _ expected_vs) ->
        compareNum 1 got_vs expected_vs
      (Word16Value _ got_vs, Word16Value _ expected_vs) ->
        compareNum 1 got_vs expected_vs
      (Word32Value _ got_vs, Word32Value _ expected_vs) ->
        compareNum 1 got_vs expected_vs
      (Word64Value _ got_vs, Word64Value _ expected_vs) ->
        compareNum 1 got_vs expected_vs
      (Float32Value _ got_vs, Float32Value _ expected_vs) ->
        compareFloat (tolerance expected_vs) got_vs expected_vs
      (Float64Value _ got_vs, Float64Value _ expected_vs) ->
        compareFloat (tolerance expected_vs) got_vs expected_vs
      (BoolValue _ got_vs, BoolValue _ expected_vs) ->
        compareGen compareBool got_vs expected_vs
      _ ->
        [TypeMismatch i (pretty $ valueElemType got_v) (pretty $ valueElemType expected_v)]
  | otherwise =
    [ArrayShapeMismatch i (valueShape got_v) (valueShape expected_v)]
  where
    {-# INLINE compareGen #-}
    {-# INLINE compareNum #-}
    {-# INLINE compareFloat #-}
    {-# INLINE compareFloatElement #-}
    {-# INLINE compareElement #-}
    compareNum tol = compareGen $ compareElement tol
    compareFloat tol = compareGen $ compareFloatElement tol

    compareGen cmp got expected =
      let l = SVec.length got
          check acc j
            | j < l =
              case cmp j (got SVec.! j) (expected SVec.! j) of
                Just mismatch ->
                  check (mismatch : acc) (j + 1)
                Nothing ->
                  check acc (j + 1)
            | otherwise =
              acc
       in reverse $ check [] 0

    compareElement tol j got expected
      | comparePrimValue tol got expected = Nothing
      | otherwise = Just $ PrimValueMismatch (i, j) (value got) (value expected)

    compareFloatElement tol j got expected
      | isNaN got,
        isNaN expected =
        Nothing
      | isInfinite got,
        isInfinite expected,
        signum got == signum expected =
        Nothing
      | otherwise =
        compareElement tol j got expected

    compareBool j got expected
      | got == expected = Nothing
      | otherwise = Just $ PrimValueMismatch (i, j) (value got) (value expected)

comparePrimValue ::
  (Ord num, Num num) =>
  num ->
  num ->
  num ->
  Bool
comparePrimValue tol x y =
  diff < tol
  where
    diff = abs $ x - y

minTolerance :: Fractional a => a
minTolerance = 0.002 -- 0.2%

tolerance :: (RealFloat a, SVec.Storable a) => Vector a -> a
tolerance = SVec.foldl tolerance' minTolerance . SVec.filter (not . nanOrInf)
  where
    tolerance' t v = max t $ minTolerance * v
    nanOrInf x = isInfinite x || isNaN x
