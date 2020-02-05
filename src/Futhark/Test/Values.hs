{-# LANGUAGE OverloadedStrings #-}
-- | This module defines an efficient value representation as well as
-- parsing and comparison functions.  This is because the standard
-- Futhark parser is not able to cope with large values (like arrays
-- that are tens of megabytes in size).  The representation defined
-- here does not support tuples, so don't use those as input/output
-- for your test programs.
module Futhark.Test.Values
       ( Value(..)
       , Vector

       -- * Reading Values
       , readValues

       -- * Types of values
       , ValueType(..)
       , valueType

       -- * Comparing Values
       , compareValues
       , compareValues1
       , Mismatch
       )
       where

import Control.Monad
import Control.Monad.ST
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.IEEE754
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Char (isSpace, ord, chr)
import Data.Vector.Binary
import qualified Data.Vector.Unboxed.Mutable as UMVec
import qualified Data.Vector.Unboxed as UVec
import Data.Vector.Generic (freeze)
import Data.Loc (Pos(..))

import qualified Language.Futhark.Syntax as F
import Language.Futhark.Pretty()
import Futhark.Representation.Primitive (PrimValue)
import Language.Futhark.Parser.Lexer
import qualified Futhark.Util.Pretty as PP
import Futhark.Representation.AST.Attributes.Constants (IsValue(..))
import Futhark.Representation.AST.Pretty ()
import Futhark.Util.Pretty
import Futhark.Util (maybeHead)

type STVector s = UMVec.STVector s

-- | An Unboxed vector.
type Vector = UVec.Vector

-- | An efficiently represented Futhark value.  Use 'pretty' to get a
-- human-readable representation, and the instances of 'Get' and 'Put'
-- to obtain binary representations
data Value = Int8Value (Vector Int) (Vector Int8)
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
           deriving Show

binaryFormatVersion :: Word8
binaryFormatVersion = 2

instance Binary Value where
  put (Int8Value shape vs) = putBinaryValue "  i8" shape vs putInt8
  put (Int16Value shape vs) = putBinaryValue " i16" shape vs putInt16le
  put (Int32Value shape vs) = putBinaryValue " i32" shape vs putInt32le
  put (Int64Value shape vs) = putBinaryValue " i64" shape vs putInt64le
  put (Word8Value shape vs) = putBinaryValue "  u8" shape vs putWord8
  put (Word16Value shape vs) = putBinaryValue " u16" shape vs putWord16le
  put (Word32Value shape vs) = putBinaryValue " u32" shape vs putWord32le
  put (Word64Value shape vs) = putBinaryValue " u64" shape vs putWord64le
  put (Float32Value shape vs) = putBinaryValue " f32" shape vs putFloat32le
  put (Float64Value shape vs) = putBinaryValue " f64" shape vs putFloat64le
  put (BoolValue shape vs) = putBinaryValue "bool" shape vs $ putInt8 . boolToInt
    where boolToInt True = 1
          boolToInt False = 0

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
        shape' = UVec.fromList shape

    case BS.unpack type_f of
      "  i8" -> get' (Int8Value shape') getInt8 num_elems
      " i16" -> get' (Int16Value shape') getInt16le num_elems
      " i32" -> get' (Int32Value shape') getInt32le num_elems
      " i64" -> get' (Int64Value shape') getInt64le num_elems
      "  u8" -> get' (Word8Value shape') getWord8 num_elems
      " u16" -> get' (Word16Value shape') getWord16le num_elems
      " u32" -> get' (Word32Value shape') getWord32le num_elems
      " u64" -> get' (Word64Value shape') getWord64le num_elems
      " f32" -> get' (Float32Value shape') getFloat32le num_elems
      " f64" -> get' (Float64Value shape') getFloat64le num_elems
      "bool" -> get' (BoolValue shape') getBool num_elems
      s      -> fail $ "Cannot parse binary values of type " ++ show s
    where getBool = (/=0) <$> getWord8

          get' mk get_elem num_elems =
            mk <$> genericGetVectorWith (pure num_elems) get_elem

putBinaryValue :: UVec.Unbox a =>
                  String -> Vector Int -> Vector a -> (a -> Put) -> Put
putBinaryValue tstr shape vs putv = do
  putInt8 $ fromIntegral $ ord 'b'
  putWord8 binaryFormatVersion
  putWord8 $ fromIntegral $ UVec.length shape
  mapM_ (putInt8 . fromIntegral . ord) tstr
  mapM_ (putInt64le . fromIntegral) $ UVec.toList shape
  mapM_ putv $ UVec.toList vs

instance PP.Pretty Value where
  ppr v | product (valueShape v) == 0 =
            text "empty" <>
            parens (dims <> ppr (valueElemType v))
    where dims = mconcat $ map (brackets . ppr) $ valueShape v
  ppr (Int8Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (Int16Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (Int32Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (Int64Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (Word8Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (Word16Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (Word32Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (Word64Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (Float32Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (Float64Value shape vs) = pprArray (UVec.toList shape) vs
  ppr (BoolValue shape vs) = pprArray (UVec.toList shape) vs

pprArray :: (UVec.Unbox a, F.IsPrimValue a) => [Int] -> UVec.Vector a -> Doc
pprArray [] vs =
  ppr $ F.primValue $ UVec.head vs
pprArray (d:ds) vs =
  brackets $ commasep $ map (pprArray ds . slice) [0..d-1]
  where slice_size = product ds
        slice i = UVec.slice (i*slice_size) slice_size vs

-- | A representation of the simple values we represent in this module.
data ValueType = ValueType [Int] F.PrimType
               deriving (Show)

instance PP.Pretty ValueType where
  ppr (ValueType ds t) = mconcat (map pprDim ds) <> ppr t
    where pprDim d = brackets $ ppr d

-- | A textual description of the type of a value.  Follows Futhark
-- type notation, and contains the exact dimension sizes if an array.
valueType :: Value -> ValueType
valueType v = ValueType (valueShape v) $ valueElemType v

valueElemType :: Value -> F.PrimType
valueElemType Int8Value{} = F.Signed F.Int8
valueElemType Int16Value{} = F.Signed F.Int16
valueElemType Int32Value{} = F.Signed F.Int32
valueElemType Int64Value{} = F.Signed F.Int64
valueElemType Word8Value{} = F.Unsigned F.Int8
valueElemType Word16Value{} = F.Unsigned F.Int16
valueElemType Word32Value{} = F.Unsigned F.Int32
valueElemType Word64Value{} = F.Unsigned F.Int64
valueElemType Float32Value{} = F.FloatType F.Float32
valueElemType Float64Value{} = F.FloatType F.Float64
valueElemType BoolValue{} = F.Bool

valueShape :: Value -> [Int]
valueShape (Int8Value shape _) = UVec.toList shape
valueShape (Int16Value shape _) = UVec.toList shape
valueShape (Int32Value shape _) = UVec.toList shape
valueShape (Int64Value shape _) = UVec.toList shape
valueShape (Word8Value shape _) = UVec.toList shape
valueShape (Word16Value shape _) = UVec.toList shape
valueShape (Word32Value shape _) = UVec.toList shape
valueShape (Word64Value shape _) = UVec.toList shape
valueShape (Float32Value shape _) = UVec.toList shape
valueShape (Float64Value shape _) = UVec.toList shape
valueShape (BoolValue shape _) = UVec.toList shape

-- The parser

dropRestOfLine, dropSpaces :: BS.ByteString -> BS.ByteString
dropRestOfLine = BS.drop 1 . BS.dropWhile (/='\n')
dropSpaces t = case BS.dropWhile isSpace t of
  t' | "--" `BS.isPrefixOf` t' -> dropSpaces $ dropRestOfLine t'
     | otherwise -> t'

type ReadValue v = BS.ByteString -> Maybe (v, BS.ByteString)

symbol :: Char -> BS.ByteString -> Maybe BS.ByteString
symbol c t
  | Just (c', t') <- BS.uncons t, c' == c = Just $ dropSpaces t'
  | otherwise = Nothing

lexeme :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
lexeme l t
  | l `BS.isPrefixOf` t = Just $ dropSpaces $ BS.drop (BS.length l) t
  | otherwise = Nothing

-- (Used elements, shape, elements, remaining input)
type State s v = (Int, Vector Int, STVector s v, BS.ByteString)

readArrayElemsST :: UMVec.Unbox v =>
                    Int -> Int -> ReadValue v -> State s v
                 -> ST s (Maybe (Int, State s v))
readArrayElemsST j r rv s = do
  ms <- readRankedArrayOfST r rv s
  case ms of
    Just (i, shape, arr, t)
      | Just t' <- symbol ',' t ->
          readArrayElemsST (j+1) r rv (i, shape, arr, t')
      | otherwise -> return $ Just (j, (i, shape, arr, t))
    _ ->
      return $ Just (0, s)

updateShape :: Int -> Int -> Vector Int -> Maybe (Vector Int)
updateShape d n shape
  | old_n < 0  = Just $ shape UVec.// [(r-d, n)]
  | old_n == n = Just shape
  | otherwise  = Nothing
  where r = UVec.length shape
        old_n = shape UVec.! (r-d)

growIfFilled :: UVec.Unbox v => Int -> STVector s v -> ST s (STVector s v)
growIfFilled i arr =
  if i >= capacity
  then UMVec.grow arr capacity
  else return arr
  where capacity = UMVec.length arr

readRankedArrayOfST :: UMVec.Unbox v =>
                 Int -> ReadValue v -> State s v
              -> ST s (Maybe (State s v))
readRankedArrayOfST 0 rv (i, shape, arr, t)
  | Just (v, t') <- rv t = do
      arr' <- growIfFilled i arr
      UMVec.write arr' i v
      return $ Just (i+1, shape, arr', t')
readRankedArrayOfST r rv (i, shape, arr, t)
  | Just t' <- symbol '[' t = do
      ms <- readArrayElemsST 1 (r-1) rv (i, shape, arr, t')
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

readRankedArrayOf :: UMVec.Unbox v =>
                     Int -> ReadValue v -> BS.ByteString -> Maybe (Vector Int, Vector v, BS.ByteString)
readRankedArrayOf r rv t = runST $ do
  arr <- UMVec.new 1024
  ms <- readRankedArrayOfST r rv (0, UVec.replicate r (-1), arr, t)
  case ms of
    Just (i, shape, arr', t') -> do
      arr'' <- freeze (UMVec.slice 0 i arr')
      return $ Just (shape, arr'', t')
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
  where (a,b) = BS.span constituent t

readInt8 :: ReadValue Int8
readInt8 = readIntegral f
  where f (I8LIT x) = Just x
        f _          = Nothing

readInt16 :: ReadValue Int16
readInt16 = readIntegral f
  where f (I16LIT x) = Just x
        f _          = Nothing

readInt32 :: ReadValue Int32
readInt32 = readIntegral f
  where f (I32LIT x) = Just x
        f _          = Nothing

readInt64 :: ReadValue Int64
readInt64 = readIntegral f
  where f (I64LIT x) = Just x
        f _          = Nothing

readWord8 :: ReadValue Word8
readWord8 = readIntegral f
  where f (U8LIT x) = Just x
        f _          = Nothing

readWord16 :: ReadValue Word16
readWord16 = readIntegral f
  where f (U16LIT x) = Just x
        f _          = Nothing

readWord32 :: ReadValue Word32
readWord32 = readIntegral f
  where f (U32LIT x) = Just x
        f _          = Nothing

readWord64 :: ReadValue Word64
readWord64 = readIntegral f
  where f (U64LIT x) = Just x
        f _          = Nothing

readFloat :: RealFloat float => ([Token] -> Maybe float) -> ReadValue float
readFloat f t = do
  v <- case map unLoc . fst <$> scanTokens (Pos "" 1 1 0) a of
         Right [NEGATE, FLOATLIT x] -> Just $ negate $ fromDouble x
         Right [FLOATLIT x] -> Just $ fromDouble x
         Right (NEGATE : toks) -> negate <$> f toks
         Right toks -> f toks
         _ -> Nothing
  return (v, dropSpaces b)
  where (a,b) = BS.span constituent t
        fromDouble = uncurry encodeFloat . decodeFloat
        unLoc (L _ x) = x

readFloat32 :: ReadValue Float
readFloat32 = readFloat lexFloat32
  where lexFloat32 [F32LIT x] = Just x
        lexFloat32 [ID "f32", PROJ_FIELD "inf"] = Just $ 1/0
        lexFloat32 [ID "f32", PROJ_FIELD "nan"] = Just $ 0/0
        lexFloat32 _ = Nothing

readFloat64 :: ReadValue Double
readFloat64 = readFloat lexFloat64
  where lexFloat64 [F64LIT x] = Just x
        lexFloat64 [ID "f64", PROJ_FIELD "inf"] = Just $ 1/0
        lexFloat64 [ID "f64", PROJ_FIELD "nan"] = Just $ 0/0
        lexFloat64 _          = Nothing

readBool :: ReadValue Bool
readBool t = do v <- case fst <$> scanTokens (Pos "" 1 1 0) a of
                       Right [L _ TRUE]  -> Just True
                       Right [L _ FALSE] -> Just False
                       _                 -> Nothing
                return (v, dropSpaces b)
  where (a,b) = BS.span constituent t

readPrimType :: ReadValue String
readPrimType t = do
  pt <- case fst <$> scanTokens (Pos "" 1 1 0) a of
          Right [L _ (ID s)] -> Just $ F.nameToString s
          _                  -> Nothing
  return (pt, dropSpaces b)
  where (a,b) = BS.span constituent t

readEmptyArrayOfShape :: [Int] -> BS.ByteString -> Maybe (Value, BS.ByteString)
readEmptyArrayOfShape shape t
  | Just t' <- symbol '[' t,
    Just (d, t'') <- readIntegral (const Nothing) t',
    Just t''' <- symbol ']' t'' = readEmptyArrayOfShape (shape++[d]) t'''

  | otherwise = do
      (pt, t') <- readPrimType t
      guard $ elem 0 shape
      v <- case pt of
             "i8" -> Just $ Int8Value (UVec.fromList shape) UVec.empty
             "i16" -> Just $ Int16Value (UVec.fromList shape) UVec.empty
             "i32" -> Just $ Int32Value (UVec.fromList shape) UVec.empty
             "i64" -> Just $ Int64Value (UVec.fromList shape) UVec.empty
             "u8" -> Just $ Word8Value (UVec.fromList shape) UVec.empty
             "u16" -> Just $ Word16Value (UVec.fromList shape) UVec.empty
             "u32" -> Just $ Word32Value (UVec.fromList shape) UVec.empty
             "u64" -> Just $ Word64Value (UVec.fromList shape) UVec.empty
             "f32" -> Just $ Float32Value (UVec.fromList shape) UVec.empty
             "f64" -> Just $ Float64Value (UVec.fromList shape) UVec.empty
             "bool" -> Just $ BoolValue (UVec.fromList shape) UVec.empty
             _  -> Nothing
      return (v, t')

readEmptyArray :: BS.ByteString -> Maybe (Value, BS.ByteString)
readEmptyArray t = do
  t' <- symbol '(' =<< lexeme "empty" t
  (v, t'') <- readEmptyArrayOfShape [] t'
  t''' <- symbol ')' t''
  return (v, t''')

readValue :: BS.ByteString -> Maybe (Value, BS.ByteString)
readValue full_t
  | Right (t', _, v) <- decodeOrFail full_t =
      Just (v, dropSpaces t')
  | otherwise = readEmptyArray full_t `mplus` insideBrackets 0 full_t
  where insideBrackets r t = maybe (tryValueAndReadValue r t) (insideBrackets (r+1)) $ symbol '[' t
        tryWith f mk r t
          | Just _ <- f t = do
              (shape, arr, rest_t) <- readRankedArrayOf r f full_t
              return (mk shape arr, rest_t)
          | otherwise = Nothing
        tryValueAndReadValue r t =
          -- 32-bit signed integers come first such that we parse
          -- unsuffixed integer constants as of that type.
          tryWith readInt32 Int32Value r t `mplus`
          tryWith readInt8 Int8Value r t `mplus`
          tryWith readInt16 Int16Value r t `mplus`
          tryWith readInt64 Int64Value r t `mplus`

          tryWith readWord8 Word8Value r t `mplus`
          tryWith readWord16 Word16Value r t `mplus`
          tryWith readWord32 Word32Value r t `mplus`
          tryWith readWord64 Word64Value r t `mplus`

          tryWith readFloat64 Float64Value r t `mplus`
          tryWith readFloat32 Float32Value r t `mplus`

          tryWith readBool BoolValue r t

-- | Parse Futhark values from the given bytestring.
readValues :: BS.ByteString -> Maybe [Value]
readValues = readValues' . dropSpaces
  where readValues' t
          | BS.null t = Just []
          | otherwise = do (a, t') <- readValue t
                           (a:) <$> readValues' t'

-- Comparisons

-- | Two values differ in some way.  The 'Show' instance produces a
-- human-readable explanation.
data Mismatch = PrimValueMismatch (Int,Int) PrimValue PrimValue
              -- ^ The position the value number and a flat index
              -- into the array.
              | ArrayShapeMismatch Int [Int] [Int]
              | TypeMismatch Int String String
              | ValueCountMismatch Int Int

instance Show Mismatch where
  show (PrimValueMismatch (i,j) got expected) =
    explainMismatch (i,j) "" got expected
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
  | otherwise = concat $ zipWith3 compareValue [0..] got expected
  where n = length got
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
  where compareNum tol = compareGen $ compareElement tol
        compareFloat tol = compareGen $ compareFloatElement tol

        compareGen cmp got expected =
          concat $
          zipWith cmp (UVec.toList $ UVec.indexed got) (UVec.toList expected)

        compareElement tol (j, got) expected
          | comparePrimValue tol got expected = []
          | otherwise = [PrimValueMismatch (i,j) (value got) (value expected)]

        compareFloatElement tol (j, got) expected
          | isNaN got, isNaN expected = []
          | isInfinite got, isInfinite expected,
            signum got == signum expected = []
          | otherwise = compareElement tol (j, got) expected

        compareBool (j, got) expected
          | got == expected = []
          | otherwise = [PrimValueMismatch (i,j) (value got) (value expected)]

comparePrimValue :: (Ord num, Num num) =>
                    num -> num -> num -> Bool
comparePrimValue tol x y =
  diff < tol
  where diff = abs $ x - y

minTolerance :: Fractional a => a
minTolerance = 0.002 -- 0.2%

tolerance :: (Ord a, Fractional a, UVec.Unbox a) => Vector a -> a
tolerance = UVec.foldl tolerance' minTolerance
  where tolerance' t v = max t $ minTolerance * v
