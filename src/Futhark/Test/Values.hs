{-# LANGUAGE OverloadedStrings #-}
-- | This module defines an efficient value representation as well as
-- parsing and comparison functions.  This is because the standard
-- Futhark parser is not able to cope with large values (like arrays
-- that are tens of megabytes in size).  The representation defined
-- here does not support tuples, so don't use those as input/output
-- for your test programs.
module Futhark.Test.Values
       ( Value

       -- * Reading Values
       , readValues

       -- * Comparing Values
       , compareValues
       , Mismatch
       , explainMismatch
       )
       where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import qualified Data.Array as A
import Data.Maybe
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Char (isSpace)
import qualified Data.Vector.Unboxed.Mutable as UMVec
import qualified Data.Vector.Unboxed as UVec
import Data.Vector.Generic (freeze)
import qualified Data.Text as T

import Prelude

import qualified Futhark.Representation.AST.Syntax.Core as F
import Futhark.Representation.Primitive
       (PrimType(..), IntType(..), FloatType(..), PrimValue)
import Language.Futhark.Parser.Lexer
import qualified Futhark.Util.Pretty as PP
import Futhark.Representation.AST.Attributes.Constants (IsValue(..))
import Futhark.Representation.AST.Pretty ()

type STVector s = UMVec.STVector s
type Vector = UVec.Vector

-- | An efficiently represented Futhark value.
data Value = Int8Value (Vector Int) (Vector Int8)
           | Int16Value (Vector Int) (Vector Int16)
           | Int32Value (Vector Int) (Vector Int32)
           | Int64Value (Vector Int) (Vector Int64)

           | Float32Value (Vector Int) (Vector Float)
           | Float64Value (Vector Int) (Vector Double)

           | BoolValue (Vector Int) (Vector Bool)
           deriving Show

instance PP.Pretty Value where
  ppr (Int8Value shape vs) = pprAsCoreValue (IntType Int8) shape vs
  ppr (Int16Value shape vs) = pprAsCoreValue (IntType Int16) shape vs
  ppr (Int32Value shape vs) = pprAsCoreValue (IntType Int32) shape vs
  ppr (Int64Value shape vs) = pprAsCoreValue (IntType Int64) shape vs
  ppr (Float32Value shape vs) = pprAsCoreValue (FloatType Float32) shape vs
  ppr (Float64Value shape vs) = pprAsCoreValue (FloatType Float64) shape vs
  ppr (BoolValue shape vs) = pprAsCoreValue Bool shape vs

pprAsCoreValue :: (UVec.Unbox v, IsValue v) =>
                  PrimType -> Vector Int -> Vector v -> PP.Doc
pprAsCoreValue bt shape vs
  | [v] <- vs', UVec.null shape = PP.ppr v
  | otherwise = PP.ppr $ F.ArrayVal (A.listArray (0, n-1) vs') bt shape'
  where n = UVec.product shape
        vs' = map value $ UVec.toList vs
        shape' = UVec.toList shape

valueType :: Value -> PrimType
valueType (Int8Value _ _) = IntType Int8
valueType (Int16Value _ _) = IntType Int16
valueType (Int32Value _ _) = IntType Int32
valueType (Int64Value _ _) = IntType Int64
valueType (Float32Value _ _) = FloatType Float32
valueType (Float64Value _ _) = FloatType Float64
valueType (BoolValue _ _) = Bool

valueShape :: Value -> [Int]
valueShape (Int8Value shape _) = UVec.toList shape
valueShape (Int16Value shape _) = UVec.toList shape
valueShape (Int32Value shape _) = UVec.toList shape
valueShape (Int64Value shape _) = UVec.toList shape
valueShape (Float32Value shape _) = UVec.toList shape
valueShape (Float64Value shape _) = UVec.toList shape
valueShape (BoolValue shape _) = UVec.toList shape

-- The parser

dropRestOfLine, dropSpaces :: T.Text -> T.Text
dropRestOfLine = T.drop 1 . T.dropWhile (/='\n')
dropSpaces t = case T.dropWhile isSpace t of
  t' | "--" `T.isPrefixOf` t' -> dropSpaces $ dropRestOfLine t'
     | otherwise -> t'

type ReadValue v = T.Text -> Maybe (v, T.Text)

symbol :: Char -> T.Text -> Maybe T.Text
symbol c t
  | Just (c', t') <- T.uncons t, c' == c = Just $ dropSpaces t'
  | otherwise = Nothing

lexeme :: T.Text -> T.Text -> Maybe T.Text
lexeme l t
  | l `T.isPrefixOf` t = Just $ dropSpaces $ T.drop (T.length l) t
  | otherwise = Nothing

-- (Used elements, shape, elements, remaining input)
type State s v = (Int, Vector Int, STVector s v, T.Text)

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
                     Int -> ReadValue v -> T.Text -> Maybe (Vector Int, Vector v, T.Text)
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
  v <- case scanTokens "" a of
         Right [L _ MINUS, L _ (INTLIT x)] -> Just $ negate $ fromIntegral x
         Right [L _ (INTLIT x)] -> Just $ fromIntegral x
         Right [L _ tok] -> f tok
         Right [L _ MINUS, L _ tok] -> negate <$> f tok
         _ -> Nothing
  return (v, dropSpaces b)
  where (a,b) = T.span constituent t

readInt8 :: ReadValue Int8
readInt8 = readIntegral f
  where f (I8LIT x) = Just x
        f (U8LIT x) = Just x
        f _          = Nothing

readInt16 :: ReadValue Int16
readInt16 = readIntegral f
  where f (I16LIT x) = Just x
        f (U16LIT x) = Just x
        f _          = Nothing

readInt32 :: ReadValue Int32
readInt32 = readIntegral f
  where f (I32LIT x) = Just x
        f (U32LIT x) = Just x
        f _          = Nothing

readInt64 :: ReadValue Int64
readInt64 = readIntegral f
  where f (I64LIT x) = Just x
        f (U64LIT x) = Just x
        f _          = Nothing

readFloat :: RealFloat float => (Token -> Maybe float) -> ReadValue float
readFloat f t = do
  v <- case scanTokens "" a of
         Right [L _ MINUS, L _ (REALLIT x)] -> Just $ negate $ fromDouble x
         Right [L _ (REALLIT x)] -> Just $ fromDouble x
         Right [L _ tok] -> f tok
         Right [L _ MINUS, L _ tok] -> negate <$> f tok
         _ -> Nothing
  return (v, dropSpaces b)
  where (a,b) = T.span constituent t
        fromDouble = uncurry encodeFloat . decodeFloat

readFloat32 :: ReadValue Float
readFloat32 = readFloat lexFloat32
  where lexFloat32 (F32LIT x) = Just x
        lexFloat32 _          = Nothing

readFloat64 :: ReadValue Double
readFloat64 = readFloat lexFloat64
  where lexFloat64 (F64LIT x) = Just x
        lexFloat64 _          = Nothing

readBool :: ReadValue Bool
readBool t = do v <- case scanTokens "" a of
                       Right [L _ TRUE]  -> Just True
                       Right [L _ FALSE] -> Just False
                       _                 -> Nothing
                return (v, dropSpaces b)
  where (a,b) = T.span constituent t

readPrimType :: ReadValue PrimType
readPrimType t = do
  pt <- case scanTokens "" a of
          Right [L _ (ID s)]
            | F.nameToString s == "i8"   -> Just $ IntType Int8
            | F.nameToString s == "i16"  -> Just $ IntType Int16
            | F.nameToString s == "i32"  -> Just $ IntType Int32
            | F.nameToString s == "i64"  -> Just $ IntType Int64
            | F.nameToString s == "f32"  -> Just $ FloatType Float32
            | F.nameToString s == "f64"  -> Just $ FloatType Float64
            | F.nameToString s == "bool" -> Just Bool
          _                            -> Nothing
  return (pt, dropSpaces b)
  where (a,b) = T.span constituent t

readEmptyArrayOfRank :: Int -> T.Text -> Maybe (Value, T.Text)
readEmptyArrayOfRank r t
  | Just t' <- symbol '[' t,
    Just t'' <- symbol ']' t' = readEmptyArrayOfRank (r+1) t''
  | otherwise = do
      (pt, t') <- readPrimType t
      let v = case pt of
                IntType Int8 -> Int8Value (UVec.replicate r 0) UVec.empty
                IntType Int16 -> Int16Value (UVec.replicate r 0) UVec.empty
                IntType Int32 -> Int32Value (UVec.replicate r 0) UVec.empty
                IntType Int64 -> Int64Value (UVec.replicate r 0) UVec.empty
                FloatType Float32 -> Float32Value (UVec.replicate r 0) UVec.empty
                FloatType Float64 -> Float64Value (UVec.replicate r 0) UVec.empty
                Bool -> BoolValue (UVec.replicate r 0) UVec.empty
                Cert -> BoolValue (UVec.replicate r 0) UVec.empty
      return (v, t')

readEmptyArray :: T.Text -> Maybe (Value, T.Text)
readEmptyArray t = do
  t' <- symbol '(' =<< lexeme "empty" t
  (v, t'') <- readEmptyArrayOfRank 1 t'
  t''' <- symbol ')' t''
  return (v, t''')

readValue :: T.Text -> Maybe (Value, T.Text)
readValue full_t = readEmptyArray full_t `mplus` insideBrackets 0 full_t
  where insideBrackets r t = maybe (tryValueAndReadValue r t) (insideBrackets (r+1)) $ symbol '[' t
        tryWith f mk r t
          | Just _ <- f t = do
              (shape, arr, rest_t) <- readRankedArrayOf r f full_t
              return (mk shape arr, rest_t)
          | otherwise = Nothing
        tryValueAndReadValue r t =
          tryWith readInt32 Int32Value r t `mplus`
          tryWith readInt8 Int8Value r t `mplus`
          tryWith readInt16 Int16Value r t `mplus`
          tryWith readInt64 Int64Value r t `mplus`

          tryWith readFloat64 Float64Value r t `mplus`
          tryWith readFloat32 Float32Value r t `mplus`

          tryWith readBool BoolValue r t

-- | Parse Futhark values from the given string.
readValues :: T.Text -> Maybe [Value]
readValues = readValues' . dropSpaces
  where readValues' t
          | T.null t = Just []
          | otherwise = do (a, t') <- readValue t
                           (a:) <$> readValues' t'

-- Comparisons

-- | Two values differ in some way.
data Mismatch = PrimValueMismatch (Int,Int) PrimValue PrimValue
              -- ^ The position the value number and a flat index
              -- into the array.
              | ArrayShapeMismatch Int [Int] [Int]
              | TypeMismatch Int PrimType PrimType
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
compareValues :: [Value] -> [Value] -> Maybe Mismatch
compareValues got expected
  | n /= m = Just $ ValueCountMismatch n m
  | otherwise = case catMaybes $ zipWith3 compareValue [0..] got expected of
    e : _ -> Just e
    []    -> Nothing
  where n = length got
        m = length expected

compareValue :: Int -> Value -> Value -> Maybe Mismatch
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
      (Float32Value _ got_vs, Float32Value _ expected_vs) ->
        compareNum (tolerance expected_vs) got_vs expected_vs
      (Float64Value _ got_vs, Float64Value _ expected_vs) ->
        compareNum (tolerance expected_vs) got_vs expected_vs
      (BoolValue _ got_vs, BoolValue _ expected_vs) ->
        compareGen compareBool got_vs expected_vs
      _ ->
        Just $ TypeMismatch i (valueType got_v) (valueType expected_v)
  | otherwise =
      Just $ ArrayShapeMismatch i (valueShape got_v) (valueShape expected_v)
  where compareNum tol = compareGen $ compareElement tol

        compareGen cmp got expected =
          foldl mplus Nothing $
          zipWith cmp (UVec.toList $ UVec.indexed got) (UVec.toList expected)

        compareElement tol (j, got) expected
          | comparePrimValue tol got expected = Nothing
          | otherwise = Just $ PrimValueMismatch (i,j) (value got) (value expected)

        compareBool (j, got) expected
          | got == expected = Nothing
          | otherwise = Just $ PrimValueMismatch (i,j) (value got) (value expected)

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
