-- | Functions for inspecting and constructing various types.
module Futhark.IR.Prop.Types
  ( rankShaped,
    arrayRank,
    arrayShape,
    setArrayShape,
    isEmptyArray,
    existential,
    uniqueness,
    unique,
    staticShapes,
    staticShapes1,
    primType,
    isAcc,
    arrayOf,
    arrayOfRow,
    arrayOfShape,
    setOuterSize,
    setDimSize,
    setOuterDim,
    setOuterDims,
    setDim,
    setArrayDims,
    peelArray,
    stripArray,
    arrayDims,
    arrayExtDims,
    shapeSize,
    arraySize,
    arraysSize,
    elemType,
    rowType,
    transposeType,
    rearrangeType,
    mapOnExtType,
    mapOnType,
    diet,
    subtypeOf,
    subtypesOf,
    toDecl,
    fromDecl,
    isExt,
    isFree,
    extractShapeContext,
    shapeContext,
    hasStaticShape,
    generaliseExtTypes,
    existentialiseExtTypes,
    shapeExtMapping,

    -- * Abbreviations
    int8,
    int16,
    int32,
    int64,
    float32,
    float64,

    -- * The Typed typeclass
    Typed (..),
    DeclTyped (..),
    ExtTyped (..),
    DeclExtTyped (..),
    SetType (..),
    FixExt (..),
  )
where

import Control.Monad
import Control.Monad.State
import Data.List (elemIndex, foldl')
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.IR.Prop.Constants
import Futhark.IR.Prop.Rearrange
import Futhark.IR.Syntax.Core

-- | Remove shape information from a type.
rankShaped :: (ArrayShape shape) => TypeBase shape u -> TypeBase Rank u
rankShaped (Array et sz u) = Array et (Rank $ shapeRank sz) u
rankShaped (Prim pt) = Prim pt
rankShaped (Acc acc ispace ts u) = Acc acc ispace ts u
rankShaped (Mem space) = Mem space

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayRank :: (ArrayShape shape) => TypeBase shape u -> Int
arrayRank = shapeRank . arrayShape

-- | Return the shape of a type - for non-arrays, this is the
-- 'mempty'.
arrayShape :: (ArrayShape shape) => TypeBase shape u -> shape
arrayShape (Array _ ds _) = ds
arrayShape _ = mempty

-- | Modify the shape of an array - for non-arrays, this does nothing.
modifyArrayShape ::
  (ArrayShape newshape) =>
  (oldshape -> newshape) ->
  TypeBase oldshape u ->
  TypeBase newshape u
modifyArrayShape f (Array t ds u)
  | shapeRank ds' == 0 = Prim t
  | otherwise = Array t ds' u
  where
    ds' = f ds
modifyArrayShape _ (Prim t) = Prim t
modifyArrayShape _ (Acc acc ispace ts u) = Acc acc ispace ts u
modifyArrayShape _ (Mem space) = Mem space

-- | Set the shape of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayShape ::
  (ArrayShape newshape) =>
  TypeBase oldshape u ->
  newshape ->
  TypeBase newshape u
setArrayShape t ds = modifyArrayShape (const ds) t

-- | If the array is statically an empty array (meaning any dimension
-- is a static zero), return the element type and the shape.
isEmptyArray :: Type -> Maybe (PrimType, Shape)
isEmptyArray (Array pt (Shape ds) _)
  | intConst Int64 0 `elem` ds = Just (pt, Shape ds)
isEmptyArray _ = Nothing

-- | True if the given type has a dimension that is existentially sized.
existential :: ExtType -> Bool
existential = any ext . shapeDims . arrayShape
  where
    ext (Ext _) = True
    ext (Free _) = False

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape Uniqueness -> Uniqueness
uniqueness (Array _ _ u) = u
uniqueness (Acc _ _ _ u) = u
uniqueness _ = Nonunique

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase shape Uniqueness -> Bool
unique = (== Unique) . uniqueness

-- | Convert types with non-existential shapes to types with
-- existential shapes.  Only the representation is changed, so all
-- the shapes will be 'Free'.
staticShapes :: [TypeBase Shape u] -> [TypeBase ExtShape u]
staticShapes = map staticShapes1

-- | As 'staticShapes', but on a single type.
staticShapes1 :: TypeBase Shape u -> TypeBase ExtShape u
staticShapes1 (Prim t) =
  Prim t
staticShapes1 (Acc acc ispace ts u) =
  Acc acc ispace ts u
staticShapes1 (Array bt (Shape shape) u) =
  Array bt (Shape $ map Free shape) u
staticShapes1 (Mem space) =
  Mem space

-- | @arrayOf t s u@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.  If the shape @s@ has rank 0, then the @t@ will
-- be returned, although if it is an array, with the uniqueness
-- changed to @u@.
arrayOf ::
  (ArrayShape shape) =>
  TypeBase shape u_unused ->
  shape ->
  u ->
  TypeBase shape u
arrayOf (Array et size1 _) size2 u =
  Array et (size2 <> size1) u
arrayOf (Prim t) shape u
  | 0 <- shapeRank shape = Prim t
  | otherwise = Array t shape u
arrayOf (Acc acc ispace ts _) _shape u =
  Acc acc ispace ts u
arrayOf Mem {} _ _ =
  error "arrayOf Mem"

-- | Construct an array whose rows are the given type, and the outer
-- size is the given dimension.  This is just a convenient wrapper
-- around 'arrayOf'.
arrayOfRow ::
  (ArrayShape (ShapeBase d)) =>
  TypeBase (ShapeBase d) NoUniqueness ->
  d ->
  TypeBase (ShapeBase d) NoUniqueness
arrayOfRow t size = arrayOf t (Shape [size]) NoUniqueness

-- | Construct an array whose rows are the given type, and the outer
-- size is the given t'Shape'.  This is just a convenient wrapper
-- around 'arrayOf'.
arrayOfShape :: Type -> Shape -> Type
arrayOfShape t shape = arrayOf t shape NoUniqueness

-- | Set the dimensions of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayDims :: TypeBase oldshape u -> [SubExp] -> TypeBase Shape u
setArrayDims t dims = t `setArrayShape` Shape dims

-- | Replace the size of the outermost dimension of an array.  If the
-- given type is not an array, it is returned unchanged.
setOuterSize ::
  (ArrayShape (ShapeBase d)) =>
  TypeBase (ShapeBase d) u ->
  d ->
  TypeBase (ShapeBase d) u
setOuterSize = setDimSize 0

-- | Replace the size of the given dimension of an array.  If the
-- given type is not an array, it is returned unchanged.
setDimSize ::
  (ArrayShape (ShapeBase d)) =>
  Int ->
  TypeBase (ShapeBase d) u ->
  d ->
  TypeBase (ShapeBase d) u
setDimSize i t e = t `setArrayShape` setDim i (arrayShape t) e

-- | Replace the outermost dimension of an array shape.
setOuterDim :: ShapeBase d -> d -> ShapeBase d
setOuterDim = setDim 0

-- | Replace some outermost dimensions of an array shape.
setOuterDims :: ShapeBase d -> Int -> ShapeBase d -> ShapeBase d
setOuterDims old k new = new <> stripDims k old

-- | Replace the specified dimension of an array shape.
setDim :: Int -> ShapeBase d -> d -> ShapeBase d
setDim i (Shape ds) e = Shape $ take i ds ++ e : drop (i + 1) ds

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: Int -> TypeBase Shape u -> Maybe (TypeBase Shape u)
peelArray 0 t = Just t
peelArray n (Array et shape u)
  | shapeRank shape == n = Just $ Prim et
  | shapeRank shape > n = Just $ Array et (stripDims n shape) u
peelArray _ _ = Nothing

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: Int -> TypeBase Shape u -> TypeBase Shape u
stripArray n (Array et shape u)
  | n < shapeRank shape = Array et (stripDims n shape) u
  | otherwise = Prim et
stripArray _ t = t

-- | Return the size of the given dimension.  If the dimension does
-- not exist, the zero constant is returned.
shapeSize :: Int -> Shape -> SubExp
shapeSize i shape = case drop i $ shapeDims shape of
  e : _ -> e
  [] -> constant (0 :: Int64)

-- | Return the dimensions of a type - for non-arrays, this is the
-- empty list.
arrayDims :: TypeBase Shape u -> [SubExp]
arrayDims = shapeDims . arrayShape

-- | Return the existential dimensions of a type - for non-arrays,
-- this is the empty list.
arrayExtDims :: TypeBase ExtShape u -> [ExtSize]
arrayExtDims = shapeDims . arrayShape

-- | Return the size of the given dimension.  If the dimension does
-- not exist, the zero constant is returned.
arraySize :: Int -> TypeBase Shape u -> SubExp
arraySize i = shapeSize i . arrayShape

-- | Return the size of the given dimension in the first element of
-- the given type list.  If the dimension does not exist, or no types
-- are given, the zero constant is returned.
arraysSize :: Int -> [TypeBase Shape u] -> SubExp
arraysSize _ [] = constant (0 :: Int64)
arraysSize i (t : _) = arraySize i t

-- | Return the immediate row-type of an array.  For @[[int]]@, this
-- would be @[int]@.
rowType :: TypeBase Shape u -> TypeBase Shape u
rowType = stripArray 1

-- | A type is a primitive type if it is not an array or memory block.
primType :: TypeBase shape u -> Bool
primType Prim {} = True
primType _ = False

-- | Is this an accumulator?
isAcc :: TypeBase shape u -> Bool
isAcc Acc {} = True
isAcc _ = False

-- | Returns the bottommost type of an array.  For @[][]i32@, this
-- would be @i32@.  If the given type is not an array, it is returned.
elemType :: TypeBase shape u -> PrimType
elemType (Array t _ _) = t
elemType (Prim t) = t
elemType Acc {} = error "elemType Acc"
elemType Mem {} = error "elemType Mem"

-- | Swap the two outer dimensions of the type.
transposeType :: Type -> Type
transposeType = rearrangeType [1, 0]

-- | Rearrange the dimensions of the type.  If the length of the
-- permutation does not match the rank of the type, the permutation
-- will be extended with identity.
rearrangeType :: [Int] -> Type -> Type
rearrangeType perm t =
  t `setArrayShape` Shape (rearrangeShape perm' $ arrayDims t)
  where
    perm' = perm ++ [length perm .. arrayRank t - 1]

-- | Transform any t'SubExp's in the type.
mapOnExtType ::
  (Monad m) =>
  (SubExp -> m SubExp) ->
  TypeBase ExtShape u ->
  m (TypeBase ExtShape u)
mapOnExtType _ (Prim bt) =
  pure $ Prim bt
mapOnExtType f (Acc acc ispace ts u) =
  Acc <$> f' acc <*> traverse f ispace <*> mapM (mapOnType f) ts <*> pure u
  where
    f' v = do
      x <- f $ Var v
      case x of
        Var v' -> pure v'
        Constant {} -> pure v
mapOnExtType _ (Mem space) =
  pure $ Mem space
mapOnExtType f (Array t shape u) =
  Array t
    <$> (Shape <$> mapM (traverse f) (shapeDims shape))
    <*> pure u

-- | Transform any t'SubExp's in the type.
mapOnType ::
  (Monad m) =>
  (SubExp -> m SubExp) ->
  TypeBase Shape u ->
  m (TypeBase Shape u)
mapOnType _ (Prim bt) = pure $ Prim bt
mapOnType f (Acc acc ispace ts u) =
  Acc <$> f' acc <*> traverse f ispace <*> mapM (mapOnType f) ts <*> pure u
  where
    f' v = do
      x <- f $ Var v
      case x of
        Var v' -> pure v'
        Constant {} -> pure v
mapOnType _ (Mem space) = pure $ Mem space
mapOnType f (Array t shape u) =
  Array t
    <$> (Shape <$> mapM f (shapeDims shape))
    <*> pure u

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ might consume its argument.
diet :: TypeBase shape Uniqueness -> Diet
diet Prim {} = ObservePrim
diet (Acc _ _ _ Unique) = Consume
diet (Acc _ _ _ Nonunique) = Observe
diet (Array _ _ Unique) = Consume
diet (Array _ _ Nonunique) = Observe
diet Mem {} = Observe

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf ::
  (Ord u, ArrayShape shape) =>
  TypeBase shape u ->
  TypeBase shape u ->
  Bool
subtypeOf (Array t1 shape1 u1) (Array t2 shape2 u2) =
  u2
    <= u1
    && t1
      == t2
    && shape1
      `subShapeOf` shape2
subtypeOf t1 t2 = t1 == t2

-- | @xs \`subtypesOf\` ys@ is true if @xs@ is the same size as @ys@,
-- and each element in @xs@ is a subtype of the corresponding element
-- in @ys@..
subtypesOf ::
  (Ord u, ArrayShape shape) =>
  [TypeBase shape u] ->
  [TypeBase shape u] ->
  Bool
subtypesOf xs ys =
  length xs == length ys
    && and (zipWith subtypeOf xs ys)

-- | Add the given uniqueness information to the types.
toDecl ::
  TypeBase shape NoUniqueness ->
  Uniqueness ->
  TypeBase shape Uniqueness
toDecl (Prim t) _ = Prim t
toDecl (Acc acc ispace ts _) u = Acc acc ispace ts u
toDecl (Array et shape _) u = Array et shape u
toDecl (Mem space) _ = Mem space

-- | Remove uniqueness information from the type.
fromDecl ::
  TypeBase shape Uniqueness ->
  TypeBase shape NoUniqueness
fromDecl (Prim t) = Prim t
fromDecl (Acc acc ispace ts _) = Acc acc ispace ts NoUniqueness
fromDecl (Array et shape _) = Array et shape NoUniqueness
fromDecl (Mem space) = Mem space

-- | If an existential, then return its existential index.
isExt :: Ext a -> Maybe Int
isExt (Ext i) = Just i
isExt _ = Nothing

-- | If a known size, then return that size.
isFree :: Ext a -> Maybe a
isFree (Free d) = Just d
isFree _ = Nothing

-- | Given the existential return type of a function, and the shapes
-- of the values returned by the function, return the existential
-- shape context.  That is, those sizes that are existential in the
-- return type.
extractShapeContext :: [TypeBase ExtShape u] -> [[a]] -> [a]
extractShapeContext ts shapes =
  evalState (concat <$> zipWithM extract ts shapes) S.empty
  where
    extract t shape =
      catMaybes <$> zipWithM extract' (shapeDims $ arrayShape t) shape
    extract' (Ext x) v = do
      seen <- gets $ S.member x
      if seen
        then pure Nothing
        else do
          modify $ S.insert x
          pure $ Just v
    extract' (Free _) _ = pure Nothing

-- | The 'Ext' integers used for existential sizes in the given types.
shapeContext :: [TypeBase ExtShape u] -> S.Set Int
shapeContext = S.fromList . concatMap (mapMaybe isExt . shapeDims . arrayShape)

-- | If all dimensions of the given 'ExtShape' are statically known,
-- change to the corresponding t'Shape'.
hasStaticShape :: TypeBase ExtShape u -> Maybe (TypeBase Shape u)
hasStaticShape (Prim bt) = Just $ Prim bt
hasStaticShape (Acc acc ispace ts u) = Just $ Acc acc ispace ts u
hasStaticShape (Mem space) = Just $ Mem space
hasStaticShape (Array bt (Shape shape) u) =
  Array bt <$> (Shape <$> mapM isFree shape) <*> pure u

-- | Given two lists of 'ExtType's of the same length, return a list
-- of 'ExtType's that is a subtype of the two operands.
generaliseExtTypes ::
  [TypeBase ExtShape u] ->
  [TypeBase ExtShape u] ->
  [TypeBase ExtShape u]
generaliseExtTypes rt1 rt2 =
  evalState (zipWithM unifyExtShapes rt1 rt2) (0, M.empty)
  where
    unifyExtShapes t1 t2 =
      setArrayShape t1 . Shape
        <$> zipWithM
          unifyExtDims
          (shapeDims $ arrayShape t1)
          (shapeDims $ arrayShape t2)
    unifyExtDims (Free se1) (Free se2)
      | se1 == se2 = pure $ Free se1 -- Arbitrary
      | otherwise = do
          (n, m) <- get
          put (n + 1, m)
          pure $ Ext n
    unifyExtDims (Ext x) (Ext y)
      | x == y = Ext <$> (maybe (new x) pure =<< gets (M.lookup x . snd))
    unifyExtDims (Ext x) _ = Ext <$> new x
    unifyExtDims _ (Ext x) = Ext <$> new x
    new x = do
      (n, m) <- get
      put (n + 1, M.insert x n m)
      pure n

-- | Given a list of 'ExtType's and a list of "forbidden" names,
-- modify the dimensions of the 'ExtType's such that they are 'Ext'
-- where they were previously 'Free' with a variable in the set of
-- forbidden names.
existentialiseExtTypes :: [VName] -> [ExtType] -> [ExtType]
existentialiseExtTypes inaccessible = map makeBoundShapesFree
  where
    makeBoundShapesFree =
      modifyArrayShape $ fmap checkDim
    checkDim (Free (Var v))
      | Just i <- v `elemIndex` inaccessible =
          Ext i
    checkDim d = d

-- | Produce a mapping for the dimensions context.
shapeExtMapping :: [TypeBase ExtShape u] -> [TypeBase Shape u1] -> M.Map Int SubExp
shapeExtMapping = dimMapping arrayExtDims arrayDims match mappend
  where
    match Free {} _ = mempty
    match (Ext i) dim = M.singleton i dim

dimMapping ::
  (Monoid res) =>
  (t1 -> [dim1]) ->
  (t2 -> [dim2]) ->
  (dim1 -> dim2 -> res) ->
  (res -> res -> res) ->
  [t1] ->
  [t2] ->
  res
dimMapping getDims1 getDims2 f comb ts1 ts2 =
  foldl' comb mempty $ concat $ zipWith (zipWith f) (map getDims1 ts1) (map getDims2 ts2)

-- | @IntType Int8@
int8 :: PrimType
int8 = IntType Int8

-- | @IntType Int16@
int16 :: PrimType
int16 = IntType Int16

-- | @IntType Int32@
int32 :: PrimType
int32 = IntType Int32

-- | @IntType Int64@
int64 :: PrimType
int64 = IntType Int64

-- | @FloatType Float32@
float32 :: PrimType
float32 = FloatType Float32

-- | @FloatType Float64@
float64 :: PrimType
float64 = FloatType Float64

-- | Typeclass for things that contain 'Type's.
class Typed t where
  typeOf :: t -> Type

instance Typed Type where
  typeOf = id

instance Typed DeclType where
  typeOf = fromDecl

instance Typed Ident where
  typeOf = identType

instance (Typed dec) => Typed (Param dec) where
  typeOf = typeOf . paramDec

instance (Typed dec) => Typed (PatElem dec) where
  typeOf = typeOf . patElemDec

instance (Typed b) => Typed (a, b) where
  typeOf = typeOf . snd

-- | Typeclass for things that contain 'DeclType's.
class DeclTyped t where
  declTypeOf :: t -> DeclType

instance DeclTyped DeclType where
  declTypeOf = id

instance (DeclTyped dec) => DeclTyped (Param dec) where
  declTypeOf = declTypeOf . paramDec

-- | Typeclass for things that contain 'ExtType's.
class (FixExt t) => ExtTyped t where
  extTypeOf :: t -> ExtType

instance ExtTyped ExtType where
  extTypeOf = id

instance ExtTyped DeclExtType where
  extTypeOf = fromDecl . declExtTypeOf

-- | Typeclass for things that contain 'DeclExtType's.
class (FixExt t) => DeclExtTyped t where
  declExtTypeOf :: t -> DeclExtType

instance DeclExtTyped DeclExtType where
  declExtTypeOf = id

-- | Typeclass for things whose type can be changed.
class (Typed a) => SetType a where
  setType :: a -> Type -> a

instance SetType Type where
  setType _ t = t

instance (SetType b) => SetType (a, b) where
  setType (a, b) t = (a, setType b t)

instance (SetType dec) => SetType (PatElem dec) where
  setType (PatElem name dec) t =
    PatElem name $ setType dec t

-- | Something with an existential context that can be (partially)
-- fixed.
class FixExt t where
  -- | Fix the given existentional variable to the indicated free
  -- value.
  fixExt :: Int -> SubExp -> t -> t

  -- | Map a function onto any existential.
  mapExt :: (Int -> Int) -> t -> t

instance (FixExt shape, ArrayShape shape) => FixExt (TypeBase shape u) where
  fixExt i se = modifyArrayShape $ fixExt i se
  mapExt f = modifyArrayShape $ mapExt f

instance (FixExt d) => FixExt (ShapeBase d) where
  fixExt i se = fmap $ fixExt i se
  mapExt f = fmap $ mapExt f

instance (FixExt a) => FixExt [a] where
  fixExt i se = fmap $ fixExt i se
  mapExt f = fmap $ mapExt f

instance FixExt ExtSize where
  fixExt i se (Ext j)
    | j > i = Ext $ j - 1
    | j == i = Free se
    | otherwise = Ext j
  fixExt _ _ (Free x) = Free x

  mapExt f (Ext i) = Ext $ f i
  mapExt _ (Free x) = Free x

instance FixExt () where
  fixExt _ _ () = ()
  mapExt _ () = ()
