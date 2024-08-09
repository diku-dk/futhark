{-# LANGUAGE LambdaCase #-}

-- | The value representation used in the interpreter.
--
-- Kept simple and free of unnecessary operational details (in
-- particular, no references to the interpreter monad).
module Language.Futhark.Interpreter.Values
  ( -- * Shapes
    Shape (..),
    ValueShape,
    typeShape,
    structTypeShape,

    -- * Values
    Value (..),
    valueShape,
    prettyValue,
    valueText,
    fromTuple,
    arrayLength,
    isEmptyArray,
    prettyEmptyArray,
    toArray,
    toArray',
    toArrayR,
    toTuple,
    repArray,

    -- * Conversion
    fromDataValue,
  )
where

import Data.Array
import Data.List (genericLength, genericReplicate)
import Data.Map qualified as M
import Data.Maybe
import Data.Monoid hiding (Sum)
import Data.Text qualified as T
import Data.Vector.Storable qualified as SVec
import Futhark.Data qualified as V
import Futhark.Util (chunk)
import Futhark.Util.Pretty
import Language.Futhark hiding (Shape, matchDims)
import Language.Futhark.Primitive qualified as P
import Prelude hiding (break, mod)

prettyRecord :: (a -> Doc ann) -> M.Map Name a -> Doc ann
prettyRecord p m
  | Just vs <- areTupleFields m =
      parens $ align $ vsep $ punctuate comma $ map p vs
  | otherwise =
      braces $ align $ vsep $ punctuate comma $ map field $ M.toList m
  where
    field (k, v) = pretty k <+> equals <+> p v

-- | A shape is a tree to accomodate the case of records.  It is
-- parameterised over the representation of dimensions.
data Shape d
  = ShapeDim d (Shape d)
  | ShapeLeaf
  | ShapeRecord (M.Map Name (Shape d))
  | ShapeSum (M.Map Name [Shape d])
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | The shape of an array.
type ValueShape = Shape Int64

instance (Pretty d) => Pretty (Shape d) where
  pretty ShapeLeaf = mempty
  pretty (ShapeDim d s) = brackets (pretty d) <> pretty s
  pretty (ShapeRecord m) = prettyRecord pretty m
  pretty (ShapeSum cs) =
    mconcat (punctuate " | " cs')
    where
      ppConstr (name, fs) = sep $ ("#" <> pretty name) : map pretty fs
      cs' = map ppConstr $ M.toList cs

emptyShape :: ValueShape -> Bool
emptyShape (ShapeDim d s) = d == 0 || emptyShape s
emptyShape _ = False

typeShape :: TypeBase d u -> Shape d
typeShape (Array _ shape et) =
  foldr ShapeDim (typeShape (Scalar et)) $ shapeDims shape
typeShape (Scalar (Record fs)) =
  ShapeRecord $ M.map typeShape fs
typeShape (Scalar (Sum cs)) =
  ShapeSum $ M.map (map typeShape) cs
typeShape t
  | Just t' <- isAccType t =
      typeShape t'
  | otherwise =
      ShapeLeaf

structTypeShape :: StructType -> Shape (Maybe Int64)
structTypeShape = fmap dim . typeShape
  where
    dim (IntLit x _ _) = Just $ fromIntegral x
    dim _ = Nothing

-- | A fully evaluated Futhark value.
data Value m
  = ValuePrim !PrimValue
  | ValueArray ValueShape !(Array Int (Value m))
  | -- Stores the full shape.
    ValueRecord (M.Map Name (Value m))
  | ValueFun (Value m -> m (Value m))
  | -- Stores the full shape.
    ValueSum ValueShape Name [Value m]
  | -- The shape, the update function, and the array.
    ValueAcc ValueShape (Value m -> Value m -> m (Value m)) !(Array Int (Value m))

instance Show (Value m) where
  show (ValuePrim v) = "ValuePrim " <> show v <> ""
  show (ValueArray shape vs) = unwords ["ValueArray", "(" <> show shape <> ")", "(" <> show vs <> ")"]
  show (ValueRecord fs) = "ValueRecord " <> "(" <> show fs <> ")"
  show (ValueSum shape c vs) = unwords ["ValueSum", "(" <> show shape <> ")", show c, "(" <> show vs <> ")"]
  show ValueFun {} = "ValueFun _"
  show ValueAcc {} = "ValueAcc _"

instance Eq (Value m) where
  ValuePrim (SignedValue x) == ValuePrim (SignedValue y) =
    P.doCmpEq (P.IntValue x) (P.IntValue y)
  ValuePrim (UnsignedValue x) == ValuePrim (UnsignedValue y) =
    P.doCmpEq (P.IntValue x) (P.IntValue y)
  ValuePrim (FloatValue x) == ValuePrim (FloatValue y) =
    P.doCmpEq (P.FloatValue x) (P.FloatValue y)
  ValuePrim (BoolValue x) == ValuePrim (BoolValue y) =
    P.doCmpEq (P.BoolValue x) (P.BoolValue y)
  ValueArray _ x == ValueArray _ y = x == y
  ValueRecord x == ValueRecord y = x == y
  ValueSum _ n1 vs1 == ValueSum _ n2 vs2 = n1 == n2 && vs1 == vs2
  ValueAcc _ _ x == ValueAcc _ _ y = x == y
  _ == _ = False

prettyValueWith :: (PrimValue -> Doc a) -> Value m -> Doc a
prettyValueWith pprPrim = pprPrec 0
  where
    pprPrec _ (ValuePrim v) = pprPrim v
    pprPrec _ (ValueArray _ a) =
      let elements = elems a -- [Value]
          separator = case elements of
            ValueArray _ _ : _ -> vsep
            _ -> hsep
       in brackets $ align $ separator $ punctuate comma $ map pprElem elements
    pprPrec _ (ValueRecord m) = prettyRecord (pprPrec 0) m
    pprPrec _ ValueFun {} = "#<fun>"
    pprPrec _ ValueAcc {} = "#<acc>"
    pprPrec p (ValueSum _ n vs) =
      parensIf (p > (0 :: Int)) $ "#" <> sep (pretty n : map (pprPrec 1) vs)
    pprElem v@ValueArray {} = pprPrec 0 v
    pprElem v = group $ pprPrec 0 v

-- | Prettyprint value.
prettyValue :: Value m -> Doc a
prettyValue = prettyValueWith pprPrim
  where
    pprPrim (UnsignedValue (Int8Value v)) = pretty (fromIntegral v :: Word8)
    pprPrim (UnsignedValue (Int16Value v)) = pretty (fromIntegral v :: Word16)
    pprPrim (UnsignedValue (Int32Value v)) = pretty (fromIntegral v :: Word32)
    pprPrim (UnsignedValue (Int64Value v)) = pretty (fromIntegral v :: Word64)
    pprPrim (SignedValue (Int8Value v)) = pretty v
    pprPrim (SignedValue (Int16Value v)) = pretty v
    pprPrim (SignedValue (Int32Value v)) = pretty v
    pprPrim (SignedValue (Int64Value v)) = pretty v
    pprPrim (BoolValue True) = "true"
    pprPrim (BoolValue False) = "false"
    pprPrim (FloatValue (Float16Value v)) = pprFloat "f16." v
    pprPrim (FloatValue (Float32Value v)) = pprFloat "f32." v
    pprPrim (FloatValue (Float64Value v)) = pprFloat "f64." v
    pprFloat t v
      | isInfinite v, v >= 0 = t <> "inf"
      | isInfinite v, v < 0 = "-" <> t <> "inf"
      | isNaN v = t <> "nan"
      | otherwise = pretty $ show v

-- | The value in the textual format.
valueText :: Value m -> T.Text
valueText = docText . prettyValueWith pretty

valueShape :: Value m -> ValueShape
valueShape (ValueArray shape _) = shape
valueShape (ValueAcc shape _ _) = shape
valueShape (ValueRecord fs) = ShapeRecord $ M.map valueShape fs
valueShape (ValueSum shape _ _) = shape
valueShape _ = ShapeLeaf

-- | Does the value correspond to an empty array?
isEmptyArray :: Value m -> Bool
isEmptyArray = emptyShape . valueShape

-- | String representation of an empty array with the provided element
-- type.  This is pretty ad-hoc - don't expect good results unless the
-- element type is a primitive.
prettyEmptyArray :: TypeBase () () -> Value m -> T.Text
prettyEmptyArray t v =
  "empty(" <> dims (valueShape v) <> prettyText t' <> ")"
  where
    t' = stripArray (arrayRank t) t
    dims (ShapeDim n rowshape) =
      "[" <> prettyText n <> "]" <> dims rowshape
    dims _ = ""

toArray :: ValueShape -> [Value m] -> Value m
toArray shape vs = ValueArray shape (listArray (0, length vs - 1) vs)

toArray' :: ValueShape -> [Value m] -> Value m
toArray' rowshape vs = ValueArray shape (listArray (0, length vs - 1) vs)
  where
    shape = ShapeDim (genericLength vs) rowshape

-- | Produce multidimensional array from a flat list of values.
toArrayR :: [Int64] -> ValueShape -> [Value m] -> Value m
toArrayR [] _ = \case
  [v] -> v
  _ -> error "toArrayR: empty shape"
toArrayR [_] elemshape = toArray' elemshape
toArrayR (n : ns) elemshape =
  toArray (foldr ShapeDim elemshape (n : ns))
    . map (toArrayR ns elemshape)
    . chunk (fromIntegral (product ns))

arrayLength :: (Integral int) => Array Int (Value m) -> int
arrayLength = fromIntegral . (+ 1) . snd . bounds

toTuple :: [Value m] -> Value m
toTuple = ValueRecord . M.fromList . zip tupleFieldNames

fromTuple :: Value m -> Maybe [Value m]
fromTuple (ValueRecord m) = areTupleFields m
fromTuple _ = Nothing

fromDataShape :: V.Vector Int -> ValueShape
fromDataShape = foldr (ShapeDim . fromIntegral) ShapeLeaf . SVec.toList

fromDataValueWith ::
  (SVec.Storable a) =>
  (a -> PrimValue) ->
  SVec.Vector Int ->
  SVec.Vector a ->
  Value m
fromDataValueWith f shape vector
  | SVec.null shape = ValuePrim $ f $ SVec.head vector
  | SVec.null vector =
      toArray (fromDataShape shape) $
        replicate (SVec.head shape) (fromDataValueWith f shape' vector)
  | otherwise =
      toArray (fromDataShape shape)
        . map (fromDataValueWith f shape' . SVec.fromList)
        $ chunk (SVec.product shape') (SVec.toList vector)
  where
    shape' = SVec.tail shape

repArray :: [Int64] -> Value m -> Value m
repArray [] v = v
repArray (n : ns) v =
  toArray' (valueShape v') (genericReplicate n v')
  where
    v' = repArray ns v

-- | Convert a Futhark value in the externally observable data format
-- to an interpreter value.
fromDataValue :: V.Value -> Value m
fromDataValue (V.I8Value shape vector) =
  fromDataValueWith (SignedValue . Int8Value) shape vector
fromDataValue (V.I16Value shape vector) =
  fromDataValueWith (SignedValue . Int16Value) shape vector
fromDataValue (V.I32Value shape vector) =
  fromDataValueWith (SignedValue . Int32Value) shape vector
fromDataValue (V.I64Value shape vector) =
  fromDataValueWith (SignedValue . Int64Value) shape vector
fromDataValue (V.U8Value shape vector) =
  fromDataValueWith (UnsignedValue . Int8Value . fromIntegral) shape vector
fromDataValue (V.U16Value shape vector) =
  fromDataValueWith (UnsignedValue . Int16Value . fromIntegral) shape vector
fromDataValue (V.U32Value shape vector) =
  fromDataValueWith (UnsignedValue . Int32Value . fromIntegral) shape vector
fromDataValue (V.U64Value shape vector) =
  fromDataValueWith (UnsignedValue . Int64Value . fromIntegral) shape vector
fromDataValue (V.F16Value shape vector) =
  fromDataValueWith (FloatValue . Float16Value) shape vector
fromDataValue (V.F32Value shape vector) =
  fromDataValueWith (FloatValue . Float32Value) shape vector
fromDataValue (V.F64Value shape vector) =
  fromDataValueWith (FloatValue . Float64Value) shape vector
fromDataValue (V.BoolValue shape vector) =
  fromDataValueWith BoolValue shape vector
