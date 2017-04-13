{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Language.Syntax" simple, and put whatever embellishments
-- we need here.
module Language.Futhark.Attributes
  (
  -- * Various
    Intrinsic(..)
  , intrinsics
  , maxIntrinsicTag
  , namesToPrimTypes
  , qualName
  , qualify
  , typeName
  , valueType
  , leadingOperator
  , progImports

  -- * Queries on expressions
  , typeOf

  -- * Queries on patterns
  , patNameSet
  , patIdentSet
  , patternType
  , patternStructType
  , patternNoShapeAnnotations

  -- * Queries on types
  , uniqueness
  , unique
  , recordArrayElemUniqueness
  , aliases
  , diet
  , subtypeOf
  , similarTo
  , arrayRank
  , arrayDims
  , arrayDims'
  , nestedDims
  , nestedDims'
  , arrayShape
  , returnType
  , lambdaReturnType
  , concreteType

  -- * Operations on types
  , peelArray
  , arrayOf
  , arrayType
  , toStructural
  , toStruct
  , fromStruct
  , setAliases
  , addAliases
  , setUniqueness
  , modifyShapeAnnotations
  , setArrayShape
  , removeShapeAnnotations
  , vacuousShapeAnnotations
  , typeToRecordArrayElem
  , recordArrayElemToType
  , tupleRecord
  , isTupleRecord
  , areTupleFields
  , sortFields

  -- | Values of these types are produces by the parser.  They use
  -- unadorned names and have no type information, apart from that
  -- which is syntactically required.
  , NoInfo(..)
  , UncheckedType
  , UncheckedTypeExp
  , UncheckedArrayType
  , UncheckedIdent
  , UncheckedTypeDecl
  , UncheckedDimIndex
  , UncheckedExp
  , UncheckedModExp
  , UncheckedSigExp
  , UncheckedLambda
  , UncheckedPattern
  , UncheckedFunBind
  , UncheckedDec
  , UncheckedProg
  )
  where

import           Control.Arrow           (second)
import           Control.Monad.Writer
import           Data.Foldable
import           Data.Hashable
import qualified Data.Map.Strict       as M
import qualified Data.Set            as S
import           Data.List
import           Data.Loc
import           Data.Maybe
import           Data.Ord

import           Prelude

import           Futhark.Util.Pretty

import           Language.Futhark.Syntax
import qualified Futhark.Representation.Primitive as Primitive

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayRank :: ArrayShape shape =>
             TypeBase shape as -> Int
arrayRank = shapeRank . arrayShape

-- | Return the shape of a type - for non-arrays, this is 'mempty'.
arrayShape :: ArrayShape shape =>
              TypeBase shape as -> shape
arrayShape (Array (PrimArray _ ds _ _)) = ds
arrayShape (Array (PolyArray _ ds _ _)) = ds
arrayShape (Array (RecordArray _ ds _)) = ds
arrayShape _                            = mempty

-- | Return the shape of a type - for non-arrays, this is 'mempty'.
arrayShape' :: TypeExp vn -> ShapeDecl vn
arrayShape' (TEArray t d _) = ShapeDecl [d] <> arrayShape' t
arrayShape' (TEUnique t _)  = arrayShape' t
arrayShape' _               = mempty

-- | Return the dimensions of a type with (possibly) known dimensions.
arrayDims :: Ord vn => TypeBase (ShapeDecl vn) as -> [DimDecl vn]
arrayDims = shapeDims . arrayShape

-- | Return the dimensions of a type with (possibly) known dimensions.
arrayDims' :: TypeExp vn -> [DimDecl vn]
arrayDims' = shapeDims . arrayShape'

-- | Return any shape declaration in the type, with duplicates removed.
nestedDims :: Ord vn => TypeBase (ShapeDecl vn) as -> [DimDecl vn]
nestedDims t =
  case t of Array a   -> nub $ arrayNestedDims a
            Record fs -> nub $ fold $ fmap nestedDims fs
            Prim{}    -> mempty
            TypeVar{} -> mempty
  where arrayNestedDims (PrimArray _ ds _ _) =
          shapeDims ds
        arrayNestedDims (PolyArray _ ds _ _) =
          shapeDims ds
        arrayNestedDims (RecordArray ts ds _) =
          shapeDims ds <> fold (fmap recordArrayElemNestedDims ts)
        recordArrayElemNestedDims (ArrayArrayElem a) =
          arrayNestedDims a
        recordArrayElemNestedDims (RecordArrayElem fs) =
          fold $ fmap recordArrayElemNestedDims fs
        recordArrayElemNestedDims PrimArrayElem{} =
          mempty
        recordArrayElemNestedDims PolyArrayElem{} =
          mempty

-- | Return any shape declaration in the type, with duplicates removed.
nestedDims' :: Ord vn => TypeExp vn -> [DimDecl vn]
nestedDims' (TEArray t d _) = nub $ d : nestedDims' t
nestedDims' (TETuple ts _)  = nub $ mconcat $ map nestedDims' ts
nestedDims' (TEUnique t _)  = nestedDims' t
nestedDims' _               = mempty

-- | Set the dimensions of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayShape :: TypeBase shape as -> shape -> TypeBase shape as
setArrayShape (Array (PrimArray et _ u as)) ds = Array $ PrimArray et ds u as
setArrayShape (Array (PolyArray v _ u as))  ds = Array $ PolyArray v ds u as
setArrayShape (Array (RecordArray et _ u))  ds = Array $ RecordArray et ds u
setArrayShape (Record ts)                   _  = Record ts
setArrayShape (Prim t)                      _  = Prim t
setArrayShape (TypeVar x)                   _  = TypeVar x

-- | Change the shape of a type to be just the 'Rank'.
removeShapeAnnotations :: ArrayShape shape =>
                          TypeBase shape as -> TypeBase Rank as
removeShapeAnnotations = modifyShapeAnnotations $ Rank . shapeRank

-- | Change the shape of a type to be a 'ShapeDecl' where all
-- dimensions are 'Nothing'.
vacuousShapeAnnotations :: ArrayShape shape =>
                           TypeBase shape as -> TypeBase (ShapeDecl vn) as
vacuousShapeAnnotations = modifyShapeAnnotations $ \shape ->
  ShapeDecl (replicate (shapeRank shape) AnyDim)

-- | Change the shape of a type.
modifyShapeAnnotations :: (oldshape -> newshape)
                       -> TypeBase oldshape as
                       -> TypeBase newshape as
modifyShapeAnnotations f (Array at) =
  Array $ modifyShapeAnnotationsFromArray f at
modifyShapeAnnotations f (Record ts) =
  Record $ fmap (modifyShapeAnnotations f) ts
modifyShapeAnnotations _ (Prim t) =
  Prim t
modifyShapeAnnotations _ (TypeVar x) =
  TypeVar x

modifyShapeAnnotationsFromArray :: (oldshape -> newshape)
                                -> ArrayTypeBase oldshape as
                                -> ArrayTypeBase newshape as
modifyShapeAnnotationsFromArray f (PrimArray et shape u as) =
  PrimArray et (f shape) u as
modifyShapeAnnotationsFromArray f (PolyArray et shape u as) =
  PolyArray et (f shape) u as
modifyShapeAnnotationsFromArray f (RecordArray ts shape u) =
  RecordArray
  (fmap (modifyShapeAnnotationsFromRecordArrayElem f) ts)
  (f shape) u

-- Try saying this one three times fast.
modifyShapeAnnotationsFromRecordArrayElem :: (oldshape -> newshape)
                                          -> RecordArrayElemTypeBase oldshape as
                                          -> RecordArrayElemTypeBase newshape as
modifyShapeAnnotationsFromRecordArrayElem
  _ (PrimArrayElem bt as u) = PrimArrayElem bt as u
modifyShapeAnnotationsFromRecordArrayElem
  _ (PolyArrayElem bt as u) = PolyArrayElem bt as u
modifyShapeAnnotationsFromRecordArrayElem
  f (ArrayArrayElem at) = ArrayArrayElem $ modifyShapeAnnotationsFromArray f at
modifyShapeAnnotationsFromRecordArrayElem
  f (RecordArrayElem ts) = RecordArrayElem $ fmap (modifyShapeAnnotationsFromRecordArrayElem f) ts

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _              = True

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf :: ArrayShape shape =>
             TypeBase shape as1 -> TypeBase shape as2 -> Bool
subtypeOf
  (Array (PrimArray t1 dims1 u1 _))
  (Array (PrimArray t2 dims2 u2 _)) =
  u1 `subuniqueOf` u2 &&
  t1 == t2 &&
  dims1 == dims2
subtypeOf
  (Array (PolyArray t1 dims1 u1 _))
  (Array (PolyArray t2 dims2 u2 _)) =
  u1 `subuniqueOf` u2 &&
  t1 == t2 &&
  dims1 == dims2
subtypeOf
  (Array (RecordArray et1 dims1 u1))
  (Array (RecordArray et2 dims2 u2)) =
  and [sort (M.keys et1) == sort (M.keys et2),
       u1 `subuniqueOf` u2,
       length et1 == length et2,
       and (M.intersectionWith subtypeOf
            (fmap recordArrayElemToType et1)
            (fmap recordArrayElemToType et2)),
       dims1 == dims2]
subtypeOf (Record ts1) (Record ts2) =
  sort (M.keys ts1) == sort (M.keys ts2) &&
  length ts1 == length ts2 && and (M.intersectionWith subtypeOf ts1 ts2)
subtypeOf (Prim bt1) (Prim bt2) = bt1 == bt2
subtypeOf (TypeVar v1) (TypeVar v2) = v1 == v2
subtypeOf _ _ = False

-- | @x \`similarTo\` y@ is true if @x@ and @y@ are the same type,
-- ignoring uniqueness.
similarTo :: ArrayShape shape =>
             TypeBase shape as1
          -> TypeBase shape as2
          -> Bool
similarTo t1 t2 = t1 `subtypeOf` t2 || t2 `subtypeOf` t1

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape as -> Uniqueness
uniqueness (Array (PrimArray _ _ u _)) = u
uniqueness (Array (PolyArray _ _ u _)) = u
uniqueness (Array (RecordArray _ _ u)) = u
uniqueness _                           = Nonunique

recordArrayElemUniqueness :: RecordArrayElemTypeBase shape as -> Uniqueness
recordArrayElemUniqueness (PrimArrayElem _ _ u) = u
recordArrayElemUniqueness (PolyArrayElem _ _ u) = u
recordArrayElemUniqueness (ArrayArrayElem (PrimArray _ _ u _)) = u
recordArrayElemUniqueness (ArrayArrayElem (PolyArray _ _ u _)) = u
recordArrayElemUniqueness (ArrayArrayElem (RecordArray _ _ u)) = u
recordArrayElemUniqueness (RecordArrayElem ts) = fold $ fmap recordArrayElemUniqueness ts

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase shape as -> Bool
unique = (==Unique) . uniqueness

-- | Return the set of all variables mentioned in the aliasing of a
-- type.
aliases :: Monoid as => TypeBase shape as -> as
aliases (Array (PrimArray _ _ _ als)) = als
aliases (Array (PolyArray _ _ _ als)) = als
aliases (Array (RecordArray ts _ _))  = fold $ fmap recordArrayElemAliases ts
aliases (Record et)                   = fold $ fmap aliases et
aliases (Prim _)                      = mempty
aliases (TypeVar _)                   = mempty

recordArrayElemAliases :: Monoid as =>
                         RecordArrayElemTypeBase shape as -> as
recordArrayElemAliases (PrimArrayElem _ als _) = als
recordArrayElemAliases (PolyArrayElem _ als _) = als
recordArrayElemAliases (ArrayArrayElem (PrimArray _ _ _ als)) =
  als
recordArrayElemAliases (ArrayArrayElem (PolyArray _ _ _ als)) =
  als
recordArrayElemAliases (ArrayArrayElem (RecordArray ts _ _)) =
  fold $ M.map recordArrayElemAliases ts
recordArrayElemAliases (RecordArrayElem ts) =
  fold $ M.map recordArrayElemAliases ts

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ might consume its argument.
diet :: TypeBase shape as -> Diet
diet (Record ets)                        = RecordDiet $ fmap diet ets
diet (Prim _)                            = Observe
diet (TypeVar _)                         = Observe
diet (Array (PrimArray _ _ Unique _))    = Consume
diet (Array (PrimArray _ _ Nonunique _)) = Observe
diet (Array (PolyArray _ _ Unique _))    = Consume
diet (Array (PolyArray _ _ Nonunique _)) = Observe
diet (Array (RecordArray _ _ Unique))    = Consume
diet (Array (RecordArray _ _ Nonunique)) = Observe

-- | @t `maskAliases` d@ removes aliases (sets them to 'mempty') from
-- the parts of @t@ that are denoted as 'Consumed' by the 'Diet' @d@.
maskAliases :: Monoid as =>
               TypeBase shape as
            -> Diet
            -> TypeBase shape as
maskAliases t Consume = t `setAliases` mempty
maskAliases t Observe = t
maskAliases (Record ets) (RecordDiet ds) =
  Record $ M.intersectionWith maskAliases ets ds
maskAliases _ _ = error "Invalid arguments passed to maskAliases."

-- | Convert any type to one that has rank information, no alias
-- information, and no embedded names.
toStructural :: ArrayShape shape =>
                TypeBase shape as
             -> TypeBase Rank ()
toStructural = removeNames . removeShapeAnnotations

-- | Remove aliasing information from a type.
toStruct :: TypeBase shape as
         -> TypeBase shape ()
toStruct t = t `setAliases` ()

-- | Replace no aliasing with an empty alias set.
fromStruct :: TypeBase shape as
           -> TypeBase shape (Names vn)
fromStruct t = t `setAliases` S.empty

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: ArrayShape shape =>
             Int -> TypeBase shape as -> Maybe (TypeBase shape as)
peelArray 0 t = Just t
peelArray n (Array (PrimArray et shape _ _))
  | shapeRank shape == n =
    Just $ Prim et
peelArray n (Array (PolyArray et shape _ _))
  | shapeRank shape == n =
    Just $ TypeVar et
peelArray n (Array (RecordArray ts shape _))
  | shapeRank shape == n =
    Just $ Record $ fmap asType ts
  where asType (PrimArrayElem bt _ _) = Prim bt
        asType (PolyArrayElem bt _ _) = TypeVar bt
        asType (ArrayArrayElem at)    = Array at
        asType (RecordArrayElem ts')  = Record $ fmap asType ts'
peelArray n (Array (PrimArray et shape u als)) = do
  shape' <- stripDims n shape
  return $ Array $ PrimArray et shape' u als
peelArray n (Array (PolyArray et shape u als)) = do
  shape' <- stripDims n shape
  return $ Array $ PolyArray et shape' u als
peelArray n (Array (RecordArray et shape u)) = do
  shape' <- stripDims n shape
  return $ Array $ RecordArray et shape' u
peelArray _ _ = Nothing

-- | Remove names from a type - this involves removing all size
-- annotations from arrays, as well as all aliasing.
removeNames :: ArrayShape shape =>
               TypeBase shape as
            -> TypeBase Rank ()
removeNames = flip setAliases () . removeShapeAnnotations

-- | @arrayOf t s u@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.
arrayOf :: (ArrayShape shape, Monoid as) =>
           TypeBase shape as
        -> shape
        -> Uniqueness
        -> TypeBase shape as
arrayOf (Array (PrimArray et shape1 _ als)) shape2 u =
  Array $ PrimArray et (shape2 <> shape1) u als
arrayOf (Array (PolyArray et shape1 _ als)) shape2 u =
  Array $ PolyArray et (shape2 <> shape1) u als
arrayOf (Array (RecordArray et shape1 _)) shape2 u =
  Array $ RecordArray et (shape2 <> shape1) u
arrayOf (Prim et) shape u =
  Array $ PrimArray et shape u mempty
arrayOf (TypeVar x) shape u =
  Array $ PolyArray x shape u mempty
arrayOf (Record ts) shape u =
  Array $ RecordArray (fmap (`typeToRecordArrayElem` u) ts) shape u

typeToRecordArrayElem :: Monoid as =>
                        TypeBase shape as
                     -> Uniqueness
                     -> RecordArrayElemTypeBase shape as
typeToRecordArrayElem (Prim bt)    u = PrimArrayElem bt mempty u
typeToRecordArrayElem (TypeVar bt) u = PolyArrayElem bt mempty u
typeToRecordArrayElem (Record ts') u = RecordArrayElem $ fmap (`typeToRecordArrayElem` u) ts'
typeToRecordArrayElem (Array at)   _ = ArrayArrayElem at

recordArrayElemToType :: RecordArrayElemTypeBase shape as
                     -> TypeBase shape as
recordArrayElemToType (PrimArrayElem bt _ _) = Prim bt
recordArrayElemToType (PolyArrayElem bt _ _) = TypeVar bt
recordArrayElemToType (RecordArrayElem ts)   = Record $ fmap recordArrayElemToType ts
recordArrayElemToType (ArrayArrayElem at)    = Array at

-- | @arrayType n t@ is the type of @n@-dimensional arrays having @t@ as
-- the base type.  If @t@ is itself an m-dimensional array, the result
-- is an @n+m@-dimensional array with the same base type as @t@.  If
-- you need to specify size information for the array, use 'arrayOf'
-- instead.
arrayType :: (ArrayShape shape, Monoid as) =>
             Int
          -> TypeBase shape as
          -> Uniqueness
          -> TypeBase Rank as
arrayType 0 t _ = removeShapeAnnotations t
arrayType n t u = arrayOf (removeShapeAnnotations t) (Rank n) u

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: ArrayShape shape =>
              Int -> TypeBase shape as -> TypeBase shape as
stripArray n (Array (PrimArray et shape u als))
  | Just shape' <- stripDims n shape =
    Array $ PrimArray et shape' u als
  | otherwise = Prim et
stripArray n (Array (PolyArray et shape u als))
  | Just shape' <- stripDims n shape =
    Array $ PolyArray et shape' u als
  | otherwise = TypeVar et
stripArray n (Array (RecordArray fs shape u))
  | Just shape' <- stripDims n shape =
    Array $ RecordArray fs shape' u
  | otherwise = Record $ fmap recordArrayElemToType fs
stripArray _ t = t

-- | Create a record type corresponding to a tuple with the given
-- element types.
tupleRecord :: [TypeBase shape as] -> TypeBase shape as
tupleRecord = Record . M.fromList . zip tupleFieldNames

isTupleRecord :: TypeBase shape as -> Maybe [TypeBase shape as]
isTupleRecord (Record fs) = areTupleFields fs
isTupleRecord _ = Nothing

areTupleFields :: M.Map Name a -> Maybe [a]
areTupleFields fs =
  let fs' = sortFields fs
  in if and $ zipWith (==) (map fst fs') tupleFieldNames
     then Just $ map snd fs'
     else Nothing

-- | Increasing field names for a tuple (starts at 1).
tupleFieldNames :: [Name]
tupleFieldNames = map (nameFromString . show) [(1::Int)..]

-- | Sort fields by their name; taking care to sort numeric fields by
-- their numeric value.  This ensures that tuples and tuple-like
-- records match.
sortFields :: M.Map Name a -> [(Name,a)]
sortFields l = map snd $ sortBy (comparing fst) $ zip (map (fieldish . fst) l') l'
  where l' = M.toList l
        fieldish s = case reads $ nameToString s of
          [(x, "")] -> Left (x::Int)
          _         -> Right s


-- | Set the uniqueness attribute of a type.  If the type is a tuple,
-- the uniqueness of its components will be modified.
setUniqueness :: TypeBase shape as -> Uniqueness -> TypeBase shape as
setUniqueness (Array at) u =
  Array $ setArrayUniqueness at u
setUniqueness (Record ets) u =
  Record $ fmap (`setUniqueness` u) ets
setUniqueness t _ = t

setArrayUniqueness :: ArrayTypeBase shape as -> Uniqueness
                   -> ArrayTypeBase shape as
setArrayUniqueness (PrimArray et dims _ als) u =
  PrimArray et dims u als
setArrayUniqueness (PolyArray et dims _ als) u =
  PolyArray et dims u als
setArrayUniqueness (RecordArray et dims _) u =
  RecordArray (fmap (`setRecordArrayElemUniqueness` u) et) dims u

setRecordArrayElemUniqueness :: RecordArrayElemTypeBase shape as -> Uniqueness
                            -> RecordArrayElemTypeBase shape as
setRecordArrayElemUniqueness (PrimArrayElem bt als _) u =
  PrimArrayElem bt als u
setRecordArrayElemUniqueness (PolyArrayElem bt als _) u =
  PolyArrayElem bt als u
setRecordArrayElemUniqueness (ArrayArrayElem at) u =
  ArrayArrayElem $ setArrayUniqueness at u
setRecordArrayElemUniqueness (RecordArrayElem ts) u =
  RecordArrayElem $ fmap (`setRecordArrayElemUniqueness` u) ts

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliasing.
setAliases :: TypeBase shape asf -> ast -> TypeBase shape ast
setAliases t = addAliases t . const

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliasing replaced by @f@ applied to that aliasing.
addAliases :: TypeBase shape asf -> (asf -> ast)
           -> TypeBase shape ast
addAliases (Array at) f =
  Array $ addArrayAliases at f
addAliases (Record ts) f =
  Record $ fmap (`addAliases` f) ts
addAliases (Prim et) _ =
  Prim et
addAliases (TypeVar et) _ =
  TypeVar et

addArrayAliases :: ArrayTypeBase shape asf
                -> (asf -> ast)
                -> ArrayTypeBase shape ast
addArrayAliases (PrimArray et dims u als) f =
  PrimArray et dims u $ f als
addArrayAliases (PolyArray et dims u als) f =
  PolyArray et dims u $ f als
addArrayAliases (RecordArray et dims u) f =
  RecordArray (fmap (`addRecordArrayElemAliases` f) et) dims u

addRecordArrayElemAliases :: RecordArrayElemTypeBase shape asf
                         -> (asf -> ast)
                         -> RecordArrayElemTypeBase shape ast
addRecordArrayElemAliases (PrimArrayElem bt als u) f =
  PrimArrayElem bt (f als) u
addRecordArrayElemAliases (PolyArrayElem bt als u) f =
  PolyArrayElem bt (f als) u
addRecordArrayElemAliases (ArrayArrayElem at) f =
  ArrayArrayElem $ addArrayAliases at f
addRecordArrayElemAliases (RecordArrayElem ts) f =
  RecordArrayElem $ fmap (`addRecordArrayElemAliases` f) ts

intValueType :: IntValue -> IntType
intValueType Int8Value{}  = Int8
intValueType Int16Value{} = Int16
intValueType Int32Value{} = Int32
intValueType Int64Value{} = Int64

floatValueType :: FloatValue -> FloatType
floatValueType Float32Value{} = Float32
floatValueType Float64Value{} = Float64

-- | The type of a basic value.
primValueType :: PrimValue -> PrimType
primValueType (SignedValue v)   = Signed $ intValueType v
primValueType (UnsignedValue v) = Unsigned $ intValueType v
primValueType (FloatValue v)    = FloatType $ floatValueType v
primValueType BoolValue{}       = Bool

valueType :: Value -> TypeBase Rank ()
valueType (PrimValue bv) = Prim $ primValueType bv
valueType (ArrayValue _ (Prim et)) =
  Array $ PrimArray et (Rank 1) Nonunique ()
valueType (ArrayValue _ (TypeVar et)) =
  Array $ PolyArray et (Rank 1) Nonunique ()
valueType (ArrayValue _ (Record fs)) =
  Array $ RecordArray (fmap (`typeToRecordArrayElem` Nonunique) fs) (Rank 1) Nonunique
valueType (ArrayValue _ (Array (PrimArray et shape _ _))) =
  Array $ PrimArray et (Rank $ 1 + shapeRank shape) Nonunique ()
valueType (ArrayValue _ (Array (PolyArray et shape _ _))) =
  Array $ PolyArray et (Rank $ 1 + shapeRank shape) Nonunique ()
valueType (ArrayValue _ (Array (RecordArray et shape _))) =
  Array $ RecordArray et (Rank $ 1 + shapeRank shape) Nonunique

-- | The type of an Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: (Ord vn, Hashable vn) => ExpBase Info vn -> CompTypeBase vn
typeOf (Literal val _) = Prim $ primValueType val
typeOf (Parens e _) = typeOf e
typeOf (TupLit es _) = tupleRecord $ map typeOf es
typeOf (RecordLit fs _) =
  -- Reverse, because M.unions is biased to the left.
  Record $ M.unions $ reverse $ map record fs
  where record (RecordField name e _) = M.singleton name $ typeOf e
        record (RecordRecord e) = case typeOf e of
          Record rfs -> rfs
          _          -> error "typeOf: RecordLit: the impossible happened."
typeOf (ArrayLit es (Info t) _) =
  arrayType 1 t $ mconcat $ map (uniqueness . typeOf) es
typeOf (Empty (TypeDecl _ (Info t)) _) =
  arrayType 1 (fromStruct t) Unique
typeOf (BinOp _ _ _ (Info t) _) = t
typeOf (Project _ _ (Info t) _) = t
typeOf (If _ _ _ (Info t) _) = t
typeOf (Var _ (Info (Record ets)) _) = Record ets
typeOf (Var qn (Info t) _) = t `addAliases` S.insert (qualLeaf qn)
typeOf (Ascript e _ _) = typeOf e
typeOf (Apply _ _ (Info t) _) = t
typeOf (Negate e _) = typeOf e
typeOf (LetPat _ _ body _) = typeOf body
typeOf (LetFun _ _ body _) = typeOf body
typeOf (LetWith _ _ _ _ body _) = typeOf body
typeOf (Index ident idx _) =
  stripArray (length $ filter isFix idx) (typeOf ident)
  where isFix DimFix{} = True
        isFix _        = False
typeOf (Update e _ _ _) = typeOf e
typeOf (Iota e _) = arrayType 1 (typeOf e) Unique
typeOf (Shape _ _) = Array $ PrimArray (Signed Int32) (Rank 1) Unique mempty
typeOf (Replicate _ e _) = arrayType 1 (typeOf e) Unique `setAliases` mempty
typeOf (Reshape shape  e _) =
  typeOf e `setArrayShape` Rank n
  where n = case typeOf shape of Record ts -> length ts
                                 _         -> 1
typeOf (Rearrange _ e _) = typeOf e
typeOf (Transpose e _) = typeOf e
typeOf (Rotate _ _ e _) = typeOf e
typeOf (Map f _ _) = arrayType 1 et Unique `setAliases` S.empty
  where et = lambdaReturnType f
typeOf (Reduce _ fun _ _ _) =
  lambdaReturnType fun `setAliases` mempty
typeOf (Zip i e es _) =
  Array $ RecordArray (M.fromList $ zip tupleFieldNames $
                       zipWith typeToRecordArrayElem es_ts es_us) (Rank (1+i)) u
  where ts' = map typeOf $ e:es
        es_ts = map (stripArray (1+i)) ts'
        es_us = map uniqueness ts'
        u     = mconcat es_us
typeOf (Unzip _ ts _) =
  tupleRecord $ map unInfo ts
typeOf (Unsafe e _) =
  typeOf e
typeOf (Scan fun _ _ _) =
  arrayType 1 et Unique
  where et = lambdaReturnType fun `setAliases` mempty
typeOf (Filter _ arr _) =
  typeOf arr
typeOf (Partition funs arr _) =
  tupleRecord $ replicate (length funs + 1) $ typeOf arr
typeOf (Stream form lam _ _) =
  case form of
    MapLike{}    -> lambdaReturnType lam
                    `setAliases` S.empty
                    `setUniqueness` Unique
    RedLike{}    -> lambdaReturnType lam
                    `setAliases` S.empty
                    `setUniqueness` Unique
    Sequential{} -> lambdaReturnType lam
                    `setAliases` S.empty
                    `setUniqueness` Unique
typeOf (Concat _ x _ _) =
  typeOf x `setUniqueness` Unique `setAliases` S.empty
typeOf (Split _ splitexps e _) =
  tupleRecord $ replicate (1 + n) (typeOf e)
  where n = case typeOf splitexps of Record ts -> length ts
                                     _         -> 1
typeOf (Copy e _) = typeOf e `setUniqueness` Unique `setAliases` S.empty
typeOf (DoLoop _ _ _ _ body _) = typeOf body
typeOf (Scatter a _i _v _) = typeOf a `setAliases` S.empty

-- | The result of applying the arguments of the given types to a
-- function with the given return type, consuming its parameters with
-- the given diets.
returnType :: (Ord vn, Hashable vn) =>
              TypeBase shape ()
           -> [Diet]
           -> [CompTypeBase vn]
           -> TypeBase shape (Names vn)
returnType (Array at) ds args =
  Array $ arrayReturnType at ds args
returnType (Record fs) ds args =
  Record $ fmap (\et -> returnType et ds args) fs
returnType (Prim t) _ _ = Prim t
returnType (TypeVar t) _ _ = TypeVar t

arrayReturnType :: (Ord vn, Hashable vn) =>
                   ArrayTypeBase shape ()
                -> [Diet]
                -> [CompTypeBase vn]
                -> ArrayTypeBase shape (Names vn)
arrayReturnType (PrimArray bt sz Nonunique ()) ds args =
  PrimArray bt sz Nonunique als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
arrayReturnType (PolyArray bt sz Nonunique ()) ds args =
  PolyArray bt sz Nonunique als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
arrayReturnType (RecordArray et sz Nonunique) ds args =
  RecordArray (fmap (\t -> recordArrayElemReturnType t ds args) et) sz Nonunique
arrayReturnType (PrimArray et sz Unique ()) _ _ =
  PrimArray et sz Unique mempty
arrayReturnType (PolyArray et sz Unique ()) _ _ =
  PolyArray et sz Unique mempty
arrayReturnType (RecordArray et sz Unique) _ _ =
  RecordArray (fmap (`addRecordArrayElemAliases` const mempty) et) sz Unique

recordArrayElemReturnType :: (Ord vn, Hashable vn) =>
                            RecordArrayElemTypeBase shape ()
                         -> [Diet]
                         -> [CompTypeBase vn]
                         -> RecordArrayElemTypeBase shape (Names vn)
recordArrayElemReturnType (PrimArrayElem bt () u) ds args =
  PrimArrayElem bt als u
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
recordArrayElemReturnType (PolyArrayElem bt () u) ds args =
  PolyArrayElem bt als u
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
recordArrayElemReturnType (ArrayArrayElem at) ds args =
  ArrayArrayElem $ arrayReturnType at ds args
recordArrayElemReturnType (RecordArrayElem ts) ds args =
  RecordArrayElem $ fmap (\t -> recordArrayElemReturnType t ds args) ts

-- | The specified return type of a lambda.
lambdaReturnType :: Ord vn =>
                    LambdaBase Info vn -> TypeBase Rank ()
lambdaReturnType (AnonymFun _ _ _ (Info t) _)       = removeShapeAnnotations t
lambdaReturnType (CurryFun _ _ (Info (_, t)) _)     = toStruct t
lambdaReturnType (BinOpFun _ _ _ (Info t) _)        = toStruct t
lambdaReturnType (CurryBinOpLeft _ _ _ (Info t) _)  = toStruct t
lambdaReturnType (CurryBinOpRight _ _ _ (Info t) _) = toStruct t

-- | Is the type concrete, i.e, without any type variables?
concreteType :: TypeBase f vn -> Bool
concreteType Prim{} = True
concreteType TypeVar{} = False
concreteType (Record ts) = all concreteType ts
concreteType (Array at) = concreteArrayType at
  where concreteArrayType PrimArray{}          = True
        concreteArrayType PolyArray{}          = False
        concreteArrayType (RecordArray ts _ _) = all concreteRecordArrayElem ts

        concreteRecordArrayElem PrimArrayElem{} = True
        concreteRecordArrayElem (ArrayArrayElem at') = concreteArrayType at'
        concreteRecordArrayElem PolyArrayElem{} = False
        concreteRecordArrayElem (RecordArrayElem fs) = all concreteRecordArrayElem fs

-- | The set of names bound in a pattern, including dimension declarations.
patNameSet :: (Ord vn, Hashable vn) => PatternBase NoInfo vn -> S.Set vn
patNameSet =  S.fromList . map identName . patIdentsGen sizeIdent
  where sizeIdent name = Ident name NoInfo

-- | The set of identifiers bound in a pattern, including dimension declarations.
patIdentSet :: (Ord vn, Hashable vn) => PatternBase Info vn -> S.Set (IdentBase Info vn)
patIdentSet = S.fromList . patIdentsGen sizeIdent
  where sizeIdent name = Ident name (Info $ Prim $ Signed Int32)

patIdentsGen :: (Ord vn, Hashable vn) =>
                (vn -> SrcLoc -> IdentBase f vn) -> PatternBase f vn
             -> [IdentBase f vn]
patIdentsGen _ (Id ident)              = [ident]
patIdentsGen f (PatternParens p _)     = patIdentsGen f p
patIdentsGen f (TuplePattern pats _)   = mconcat $ map (patIdentsGen f) pats
patIdentsGen f (RecordPattern fs _)    = mconcat $ map (patIdentsGen f . snd) fs
patIdentsGen _ Wildcard{}              = []
patIdentsGen f (PatternAscription p t) =
  patIdentsGen f p <> mapMaybe (dimIdent (srclocOf p)) (nestedDims' (declaredType t))
  where dimIdent _ AnyDim            = Nothing
        dimIdent _ (ConstDim _)      = Nothing
        dimIdent _ (NamedDim _)      = Nothing
        dimIdent loc (BoundDim name) = Just $ f name loc

-- | The type of values bound by the pattern.
patternType :: PatternBase Info VName -> CompTypeBase VName
patternType (Wildcard (Info t) _)   = t
patternType (PatternParens p _)     = patternType p
patternType (Id ident)              = unInfo $ identType ident
patternType (TuplePattern pats _)   = tupleRecord $ map patternType pats
patternType (RecordPattern fs _)    = Record $ patternType <$> M.fromList fs
patternType (PatternAscription p _) = patternType p

-- | The type matched by the pattern, including shape declarations if present.
patternStructType :: PatternBase Info VName -> StructTypeBase VName
patternStructType (PatternAscription _ td) = unInfo $ expandedType td
patternStructType (PatternParens p _) = patternStructType p
patternStructType (Id v) = vacuousShapeAnnotations $ toStruct $ unInfo $ identType v
patternStructType (TuplePattern ps _) = tupleRecord $ map patternStructType ps
patternStructType (RecordPattern fs _) = Record $ patternStructType <$> M.fromList fs
patternStructType (Wildcard (Info t) _) = vacuousShapeAnnotations $ toStruct t

-- | Remove all shape annotations from a pattern, leaving them unnamed
-- instead.  Note that this will change the names bound by the
-- pattern.
patternNoShapeAnnotations :: PatternBase Info VName -> PatternBase Info VName
patternNoShapeAnnotations (PatternAscription p (TypeDecl te (Info t))) =
  PatternAscription (patternNoShapeAnnotations p) $
  TypeDecl te $ Info $ vacuousShapeAnnotations t
patternNoShapeAnnotations (PatternParens p loc) =
  PatternParens (patternNoShapeAnnotations p) loc
patternNoShapeAnnotations (Id v) = Id v
patternNoShapeAnnotations (TuplePattern ps loc) =
  TuplePattern (map patternNoShapeAnnotations ps) loc
patternNoShapeAnnotations (RecordPattern ps loc) =
  RecordPattern (map (fmap patternNoShapeAnnotations) ps) loc
patternNoShapeAnnotations (Wildcard t loc) =
  Wildcard t loc

-- | Names of primitive types to types.  This is only valid if no
-- shadowing is going on, but useful for tools.
namesToPrimTypes :: M.Map Name PrimType
namesToPrimTypes = M.fromList
                   [ (nameFromString $ pretty t, t) |
                     t <- Bool :
                          map Signed [minBound..maxBound] ++
                          map Unsigned [minBound..maxBound] ++
                          map FloatType [minBound..maxBound] ]

-- | The nature of something predefined.  These can either be monomorphic
-- or polymorphic.  A polymorphic builtin is a mapping from valid
-- parameter types to the result type.
data Intrinsic = IntrinsicMonoFun [PrimType] PrimType
               | IntrinsicPolyFun [([PrimType], PrimType)]
               | IntrinsicType PrimType
               | IntrinsicEquality -- Special cased.
               | IntrinsicOpaque

-- | A map of all built-ins.
intrinsics :: M.Map VName Intrinsic
intrinsics = M.fromList $ zipWith namify [0..] $
             map (second $ uncurry IntrinsicMonoFun)
             [("sqrt32", ([FloatType Float32], FloatType Float32))
             ,("log32", ([FloatType Float32], FloatType Float32))
             ,("exp32", ([FloatType Float32], FloatType Float32))
             ,("cos32", ([FloatType Float32], FloatType Float32))
             ,("sin32", ([FloatType Float32], FloatType Float32))
             ,("acos32", ([FloatType Float32], FloatType Float32))
             ,("asin32", ([FloatType Float32], FloatType Float32))
             ,("atan32", ([FloatType Float32], FloatType Float32))
             ,("atan2_32", ([FloatType Float32, FloatType Float32], FloatType Float32))
             ,("isinf32", ([FloatType Float32], Bool))
             ,("isnan32", ([FloatType Float32], Bool))

             ,("sqrt64", ([FloatType Float64], FloatType Float64))
             ,("log64", ([FloatType Float64], FloatType Float64))
             ,("exp64", ([FloatType Float64], FloatType Float64))
             ,("cos64", ([FloatType Float64], FloatType Float64))
             ,("sin64", ([FloatType Float64], FloatType Float64))
             ,("acos64", ([FloatType Float64], FloatType Float64))
             ,("asin64", ([FloatType Float64], FloatType Float64))
             ,("atan64", ([FloatType Float64], FloatType Float64))
             ,("atan2_64", ([FloatType Float64, FloatType Float64], FloatType Float64))
             ,("isinf64", ([FloatType Float64], Bool))
             ,("isnan64", ([FloatType Float64], Bool))

             ] ++

             [ ("~", IntrinsicPolyFun $
                     [([Signed t], Signed t) | t <- [minBound..maxBound] ] ++
                     [([Unsigned t], Unsigned t) | t <- [minBound..maxBound] ])
             , ("!", IntrinsicMonoFun [Bool] Bool)] ++

             [("opaque", IntrinsicOpaque)] ++

             map (convertFun anyPrimType) anyPrimType ++

             map unOpFun Primitive.allUnOps ++

             map binOpFun Primitive.allBinOps ++

             map cmpOpFun Primitive.allCmpOps ++

             map intrinsicType (map Signed [minBound..maxBound] ++
                                map Unsigned [minBound..maxBound] ++
                                map FloatType [minBound..maxBound] ++
                                [Bool]) ++

             -- The reason for the loop formulation is to ensure that we
             -- get a missing case warning if we forget a case.
             map mkIntrinsicBinOp [minBound..maxBound]

  where namify i (k,v) = (VName (nameFromString k) i, v)

        convertFun :: [PrimType] -> PrimType -> (String,Intrinsic)
        convertFun from to = (pretty to, IntrinsicPolyFun $ zip (map pure from) (repeat to))

        unOpFun bop = (pretty bop, IntrinsicMonoFun [t] t)
          where t = unPrim $ Primitive.unOpType bop

        binOpFun bop = (pretty bop, IntrinsicMonoFun [t, t] t)
          where t = unPrim $ Primitive.binOpType bop

        cmpOpFun bop = (pretty bop, IntrinsicMonoFun [t, t] Bool)
          where t = unPrim $ Primitive.cmpOpType bop

        unPrim (Primitive.IntType t) = Signed t
        unPrim (Primitive.FloatType t) = FloatType t
        unPrim Primitive.Bool = Bool
        unPrim Primitive.Cert = Bool

        intrinsicType t = (pretty t, IntrinsicType t)

        anyIntType = map Signed [minBound..maxBound] ++
                     map Unsigned [minBound..maxBound]
        anyNumberType = anyIntType ++
                        map FloatType [minBound..maxBound]
        anyPrimType = Bool : anyNumberType

        mkIntrinsicBinOp :: BinOp -> (String, Intrinsic)
        mkIntrinsicBinOp op = (pretty op, intrinsicBinOp op)

        binOp :: [PrimType] -> Intrinsic
        binOp ts = IntrinsicPolyFun [ ([t,t], t) | t <- ts ]

        intrinsicBinOp Plus     = binOp anyNumberType
        intrinsicBinOp Minus    = binOp anyNumberType
        intrinsicBinOp Pow      = binOp anyNumberType
        intrinsicBinOp Times    = binOp anyNumberType
        intrinsicBinOp Divide   = binOp anyNumberType
        intrinsicBinOp Mod      = binOp anyNumberType
        intrinsicBinOp Quot     = binOp anyIntType
        intrinsicBinOp Rem      = binOp anyIntType
        intrinsicBinOp ShiftR   = binOp anyIntType
        intrinsicBinOp ZShiftR  = binOp anyIntType
        intrinsicBinOp ShiftL   = binOp anyIntType
        intrinsicBinOp Band     = binOp anyIntType
        intrinsicBinOp Xor      = binOp anyIntType
        intrinsicBinOp Bor      = binOp anyIntType
        intrinsicBinOp LogAnd   = IntrinsicMonoFun [Bool,Bool] Bool
        intrinsicBinOp LogOr    = IntrinsicMonoFun [Bool,Bool] Bool
        intrinsicBinOp Equal    = IntrinsicEquality
        intrinsicBinOp NotEqual = IntrinsicEquality
        intrinsicBinOp Less     = ordering
        intrinsicBinOp Leq      = ordering
        intrinsicBinOp Greater  = ordering
        intrinsicBinOp Geq      = ordering

        ordering = IntrinsicPolyFun [ ([t,t], Bool) | t <- anyPrimType ]

-- | The largest tag used by an intrinsic - this can be used to
-- determine whether a 'VName' refers to an intrinsic or a user-defined name.
maxIntrinsicTag :: Int
maxIntrinsicTag = maximum $ map baseTag $ M.keys intrinsics

-- | Create a name with no qualifiers from a name.
qualName :: v -> QualName v
qualName = QualName []

-- | Add another qualifier (at the head) to a qualified name.
qualify :: Name -> QualName v -> QualName v
qualify k (QualName ks v) = QualName (k:ks) v

-- | Create a type name name with no qualifiers from a 'VName'.
typeName :: VName -> TypeName
typeName = typeNameFromQualName . qualName

progImports :: ProgBase f vn -> [String]
progImports (Prog decs) = concatMap decImports decs
  where decImports (OpenDec x xs _) =
          concatMap modExpImports $ x:xs
        decImports (ModDec md) =
          modExpImports $ modExp md
        decImports SigDec{} = []
        decImports TypeDec{} = []
        decImports ValDec{} = []
        decImports FunDec{} = []

        modExpImports ModVar{}              = []
        modExpImports (ModParens p _)       = modExpImports p
        modExpImports (ModImport f _)       = [f]
        modExpImports (ModDecs ds _)        = concatMap decImports ds
        modExpImports (ModApply _ me _ _ _) = modExpImports me
        modExpImports (ModAscript me _ _ _) = modExpImports me
        modExpImports ModLambda{}           = []

-- | Given an operator name, return the operator that determines its
-- syntactical properties.
leadingOperator :: Name -> BinOp
leadingOperator s = maybe LogAnd snd $ find ((`isPrefixOf` s') . fst) $
                    sortBy (flip $ comparing $ length . fst) $
                    zip (map pretty operators) operators
  where s' = nameToString s
        operators :: [BinOp]
        operators = [minBound..maxBound::BinOp]

-- | A type with no aliasing information but shape annotations.
type UncheckedType = TypeBase (ShapeDecl Name) ()

type UncheckedTypeExp = TypeExp Name

-- | An array type with no aliasing information.
type UncheckedArrayType = ArrayTypeBase (ShapeDecl Name) ()

-- | A type declaration with no expanded type.
type UncheckedTypeDecl = TypeDeclBase NoInfo Name

-- | An identifier with no type annotations.
type UncheckedIdent = IdentBase NoInfo Name

-- | An index with no type annotations.
type UncheckedDimIndex = DimIndexBase NoInfo Name

-- | An expression with no type annotations.
type UncheckedExp = ExpBase NoInfo Name

-- | A module expression with no type annotations.
type UncheckedModExp = ModExpBase NoInfo Name

-- | A module type expression with no type annotations.
type UncheckedSigExp = SigExpBase NoInfo Name

-- | A lambda with no type annotations.
type UncheckedLambda = LambdaBase NoInfo Name

-- | A pattern with no type annotations.
type UncheckedPattern = PatternBase NoInfo Name

-- | A function declaration with no type annotations.
type UncheckedFunBind = FunBindBase NoInfo Name

-- | A declaration with no type annotations.
type UncheckedDec = DecBase NoInfo Name

-- | A Futhark program with no type annotations.
type UncheckedProg = ProgBase NoInfo Name
