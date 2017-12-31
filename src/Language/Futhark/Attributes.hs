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

  -- * Queries on patterns and params
  , patIdentSet
  , patternType
  , patternStructType
  , patternParam
  , patternNoShapeAnnotations
  , paramType
  , paramName

  -- * Queries on types
  , uniqueness
  , unique
  , recordArrayElemUniqueness
  , aliases
  , diet
  , subuniqueOf
  , subtypeOf
  , similarTo
  , arrayRank
  , arrayDims
  , nestedDims
  , arrayShape
  , returnType
  , concreteType

  -- * Operations on types
  , rank
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
  , UncheckedTypeParam
  , UncheckedPattern
  , UncheckedValBind
  , UncheckedDec
  , UncheckedProg
  )
  where

import           Control.Monad.Writer
import           Data.Foldable
import qualified Data.Map.Strict       as M
import qualified Data.Set            as S
import           Data.List
import           Data.Loc
import           Data.Maybe
import           Data.Ord
import           Data.Bifunctor
import           Data.Bifoldable

import           Prelude

import           Futhark.Util.Pretty

import           Language.Futhark.Syntax
import qualified Futhark.Representation.Primitive as Primitive

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayRank :: ArrayDim dim =>
             TypeBase dim as -> Int
arrayRank = shapeRank . arrayShape

-- | Return the shape of a type - for non-arrays, this is 'mempty'.
arrayShape :: TypeBase dim as -> ShapeDecl dim
arrayShape (Array (PrimArray _ ds _ _))   = ds
arrayShape (Array (PolyArray _ _ ds _ _)) = ds
arrayShape (Array (RecordArray _ ds _))   = ds
arrayShape _                              = mempty

-- | Return the dimensions of a type with (possibly) known dimensions.
arrayDims :: TypeBase dim as -> [dim]
arrayDims = shapeDims . arrayShape

-- | Return any shape declarations in the type, with duplicates
-- removed.
nestedDims :: TypeBase (DimDecl VName) as -> [DimDecl VName]
nestedDims t =
  case t of Array a         -> nub $ arrayNestedDims a
            Record fs       -> nub $ fold $ fmap nestedDims fs
            Prim{}          -> mempty
            TypeVar _ targs -> concatMap typeArgDims targs
  where arrayNestedDims (PrimArray _ ds _ _) =
          shapeDims ds
        arrayNestedDims (PolyArray _ targs ds _ _) =
          shapeDims ds <> concatMap typeArgDims targs
        arrayNestedDims (RecordArray ts ds _) =
          shapeDims ds <> fold (fmap recordArrayElemNestedDims ts)
        recordArrayElemNestedDims (ArrayArrayElem a) =
          arrayNestedDims a
        recordArrayElemNestedDims (RecordArrayElem fs) =
          fold $ fmap recordArrayElemNestedDims fs
        recordArrayElemNestedDims PrimArrayElem{} =
          mempty
        recordArrayElemNestedDims (PolyArrayElem _ targs _ _) =
          concatMap typeArgDims targs

        typeArgDims (TypeArgDim d _) = [d]
        typeArgDims (TypeArgType at _) = nestedDims at

-- | Set the dimensions of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayShape :: TypeBase dim as -> ShapeDecl dim -> TypeBase dim as
setArrayShape (Array (PrimArray et _ u as)) ds =
  Array $ PrimArray et ds u as
setArrayShape (Array (PolyArray v targs _ u as)) ds =
  Array $ PolyArray v targs ds u as
setArrayShape (Array (RecordArray et _ u))  ds =
  Array $ RecordArray et ds u
setArrayShape (Record ts) _ =
  Record ts
setArrayShape (Prim t) _ =
  Prim t
setArrayShape (TypeVar x targs) _ =
  TypeVar x targs

-- | Change the shape of a type to be just the 'Rank'.
removeShapeAnnotations :: TypeBase dim as -> TypeBase () as
removeShapeAnnotations = modifyShapeAnnotations $ const ()

-- | Change all size annotations to be 'AnyDim'.
vacuousShapeAnnotations :: TypeBase dim as -> TypeBase (DimDecl vn) as
vacuousShapeAnnotations = modifyShapeAnnotations $ const AnyDim

-- | Change the size annotations of a type.
modifyShapeAnnotations :: (oldshape -> newshape)
                       -> TypeBase oldshape as
                       -> TypeBase newshape as
modifyShapeAnnotations f = bimap f id

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _              = True

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf :: ArrayDim dim =>
             TypeBase dim as1 -> TypeBase dim as2 -> Bool
subtypeOf
  (Array (PrimArray t1 dims1 u1 _))
  (Array (PrimArray t2 dims2 u2 _)) =
  u1 `subuniqueOf` u2 &&
  t1 == t2 &&
  dims1 == dims2
subtypeOf
  (Array (PolyArray t1 targs1 dims1 u1 _))
  (Array (PolyArray t2 targs2 dims2 u2 _)) =
  u1 `subuniqueOf` u2 &&
  length targs1 == length targs2 &&
  and (zipWith subargOf targs1 targs2) &&
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
subtypeOf (TypeVar v1 targs1) (TypeVar v2 targs2) =
  v1 == v2 && length targs1 == length targs2 &&
  and (zipWith subargOf targs1 targs2)
subtypeOf _ _ = False

subargOf :: ArrayDim dim => TypeArg dim as1 -> TypeArg dim as2 -> Bool
subargOf TypeArgDim{} TypeArgDim{} = True
subargOf (TypeArgType t1 _) (TypeArgType t2 _) = t1 `subtypeOf` t2
subargOf _ _ = False

-- | @x \`similarTo\` y@ is true if @x@ and @y@ are the same type,
-- ignoring uniqueness.
similarTo :: ArrayDim dim =>
             TypeBase dim as1
          -> TypeBase dim as2
          -> Bool
similarTo t1 t2 = t1 `subtypeOf` t2 || t2 `subtypeOf` t1

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape as -> Uniqueness
uniqueness (Array (PrimArray _ _ u _))   = u
uniqueness (Array (PolyArray _ _ _ u _)) = u
uniqueness (Array (RecordArray _ _ u))   = u
uniqueness _                             = Nonunique

recordArrayElemUniqueness :: RecordArrayElemTypeBase shape as -> Uniqueness
recordArrayElemUniqueness (PrimArrayElem _ _) = Nonunique
recordArrayElemUniqueness (PolyArrayElem _ _ _ u) = u
recordArrayElemUniqueness (ArrayArrayElem (PrimArray _ _ u _)) = u
recordArrayElemUniqueness (ArrayArrayElem (PolyArray _ _ _ u _)) = u
recordArrayElemUniqueness (ArrayArrayElem (RecordArray _ _ u)) = u
recordArrayElemUniqueness (RecordArrayElem ts) = fold $ fmap recordArrayElemUniqueness ts

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase shape as -> Bool
unique = (==Unique) . uniqueness

-- | Return the set of all variables mentioned in the aliasing of a
-- type.
aliases :: Monoid as => TypeBase shape as -> as
aliases = bifoldMap (const mempty) id

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ might consume its argument.
diet :: TypeBase shape as -> Diet
diet (Record ets)                          = RecordDiet $ fmap diet ets
diet (Prim _)                              = Observe
diet TypeVar{}                             = Observe
diet (Array (PrimArray _ _ Unique _))      = Consume
diet (Array (PrimArray _ _ Nonunique _))   = Observe
diet (Array (PolyArray _ _ _ Unique _))    = Consume
diet (Array (PolyArray _ _ _ Nonunique _)) = Observe
diet (Array (RecordArray _ _ Unique))      = Consume
diet (Array (RecordArray _ _ Nonunique))   = Observe

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
toStructural :: ArrayDim dim =>
                TypeBase dim as
             -> TypeBase () ()
toStructural = removeNames . removeShapeAnnotations

-- | Remove aliasing information from a type.
toStruct :: TypeBase dim as
         -> TypeBase dim ()
toStruct t = t `setAliases` ()

-- | Replace no aliasing with an empty alias set.
fromStruct :: TypeBase dim as
           -> TypeBase dim Names
fromStruct t = t `setAliases` S.empty

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: ArrayDim dim =>
             Int -> TypeBase dim as -> Maybe (TypeBase dim as)
peelArray 0 t = Just t
peelArray n (Array (PrimArray et shape _ _))
  | shapeRank shape == n =
    Just $ Prim et
peelArray n (Array (PolyArray et targs shape _ _))
  | shapeRank shape == n =
    Just $ TypeVar et targs
peelArray n (Array (RecordArray ts shape _))
  | shapeRank shape == n =
    Just $ Record $ fmap asType ts
  where asType (PrimArrayElem bt _) = Prim bt
        asType (PolyArrayElem bt targs _ _) = TypeVar bt targs
        asType (ArrayArrayElem at)    = Array at
        asType (RecordArrayElem ts')  = Record $ fmap asType ts'
peelArray n (Array (PrimArray et shape u als)) = do
  shape' <- stripDims n shape
  return $ Array $ PrimArray et shape' u als
peelArray n (Array (PolyArray et targs shape u als)) = do
  shape' <- stripDims n shape
  return $ Array $ PolyArray et targs shape' u als
peelArray n (Array (RecordArray et shape u)) = do
  shape' <- stripDims n shape
  return $ Array $ RecordArray et shape' u
peelArray _ _ = Nothing

-- | Remove names from a type - this involves removing all size
-- annotations from arrays, as well as all aliasing.
removeNames :: ArrayDim dim =>
               TypeBase dim as
            -> TypeBase () ()
removeNames = flip setAliases () . removeShapeAnnotations

-- | @arrayOf t s u@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.
arrayOf :: (ArrayDim dim, Monoid as) =>
           TypeBase dim as
        -> ShapeDecl dim
        -> Uniqueness
        -> TypeBase dim as
arrayOf (Array (PrimArray et shape1 _ als)) shape2 u =
  Array $ PrimArray et (shape2 <> shape1) u als
arrayOf (Array (PolyArray et targs shape1 _ als)) shape2 u =
  Array $ PolyArray et targs (shape2 <> shape1) u als
arrayOf (Array (RecordArray et shape1 _)) shape2 u =
  Array $ RecordArray et (shape2 <> shape1) u
arrayOf (Prim et) shape u =
  Array $ PrimArray et shape u mempty
arrayOf (TypeVar x targs) shape u =
  Array $ PolyArray x targs shape u mempty
arrayOf (Record ts) shape u =
  Array $ RecordArray (fmap (`typeToRecordArrayElem` u) ts) shape u

typeToRecordArrayElem :: Monoid as =>
                        TypeBase dim as
                     -> Uniqueness
                     -> RecordArrayElemTypeBase dim as
typeToRecordArrayElem (Prim bt) _           = PrimArrayElem bt mempty
typeToRecordArrayElem (TypeVar bt targs) u  = PolyArrayElem bt targs mempty u
typeToRecordArrayElem (Record ts') u        = RecordArrayElem $ fmap (`typeToRecordArrayElem` u) ts'
typeToRecordArrayElem (Array at)   _        = ArrayArrayElem at

recordArrayElemToType :: RecordArrayElemTypeBase dim as
                     -> TypeBase dim as
recordArrayElemToType (PrimArrayElem bt _)         = Prim bt
recordArrayElemToType (PolyArrayElem bt targs _ _) = TypeVar bt targs
recordArrayElemToType (RecordArrayElem ts)         = Record $ fmap recordArrayElemToType ts
recordArrayElemToType (ArrayArrayElem at)          = Array at

-- | @arrayType n t@ is the type of @n@-dimensional arrays having @t@ as
-- the base type.  If @t@ is itself an m-dimensional array, the result
-- is an @n+m@-dimensional array with the same base type as @t@.  If
-- you need to specify size information for the array, use 'arrayOf'
-- instead.
arrayType :: Monoid as =>
             Int
          -> TypeBase dim as
          -> Uniqueness
          -> TypeBase () as
arrayType 0 t _ = removeShapeAnnotations t
arrayType n t u = arrayOf (removeShapeAnnotations t) (rank n) u

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: ArrayDim dim =>
              Int -> TypeBase dim as -> TypeBase dim as
stripArray n (Array (PrimArray et shape u als))
  | Just shape' <- stripDims n shape =
    Array $ PrimArray et shape' u als
  | otherwise = Prim et
stripArray n (Array (PolyArray et targs shape u als))
  | Just shape' <- stripDims n shape =
    Array $ PolyArray et targs shape' u als
  | otherwise = TypeVar et targs
stripArray n (Array (RecordArray fs shape u))
  | Just shape' <- stripDims n shape =
    Array $ RecordArray fs shape' u
  | otherwise = Record $ fmap recordArrayElemToType fs
stripArray _ t = t

-- | Create a record type corresponding to a tuple with the given
-- element types.
tupleRecord :: [TypeBase dim as] -> TypeBase dim as
tupleRecord = Record . M.fromList . zip tupleFieldNames

isTupleRecord :: TypeBase dim as -> Maybe [TypeBase dim as]
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
setUniqueness :: TypeBase dim as -> Uniqueness -> TypeBase dim as
setUniqueness (Array at) u =
  Array $ setArrayUniqueness at u
setUniqueness (Record ets) u =
  Record $ fmap (`setUniqueness` u) ets
setUniqueness t _ = t

setArrayUniqueness :: ArrayTypeBase dim as -> Uniqueness
                   -> ArrayTypeBase dim as
setArrayUniqueness (PrimArray et dims _ als) u =
  PrimArray et dims u als
setArrayUniqueness (PolyArray et targs dims _ als) u =
  PolyArray et targs dims u als
setArrayUniqueness (RecordArray et dims _) u =
  RecordArray (fmap (`setRecordArrayElemUniqueness` u) et) dims u

setRecordArrayElemUniqueness :: RecordArrayElemTypeBase dim as -> Uniqueness
                            -> RecordArrayElemTypeBase dim as
setRecordArrayElemUniqueness (PrimArrayElem bt als) _ =
  PrimArrayElem bt als
setRecordArrayElemUniqueness (PolyArrayElem bt targs als _) u =
  PolyArrayElem bt targs als u
setRecordArrayElemUniqueness (ArrayArrayElem at) u =
  ArrayArrayElem $ setArrayUniqueness at u
setRecordArrayElemUniqueness (RecordArrayElem ts) u =
  RecordArrayElem $ fmap (`setRecordArrayElemUniqueness` u) ts

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliasing.
setAliases :: TypeBase dim asf -> ast -> TypeBase dim ast
setAliases t = addAliases t . const

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliasing replaced by @f@ applied to that aliasing.
addAliases :: TypeBase dim asf -> (asf -> ast)
           -> TypeBase dim ast
addAliases t f = bimap id f t

addRecordArrayElemAliases :: RecordArrayElemTypeBase dim asf
                          -> (asf -> ast)
                          -> RecordArrayElemTypeBase dim ast
addRecordArrayElemAliases t f = bimap id f t

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

valueType :: Value -> TypeBase () ()
valueType (PrimValue bv) = Prim $ primValueType bv
valueType (ArrayValue _ (Prim et)) =
  Array $ PrimArray et (rank 1) Nonunique ()
valueType (ArrayValue _ (TypeVar et targs)) =
  Array $ PolyArray et targs (rank 1) Nonunique ()
valueType (ArrayValue _ (Record fs)) =
  Array $ RecordArray (fmap (`typeToRecordArrayElem` Nonunique) fs) (rank 1) Nonunique
valueType (ArrayValue _ (Array (PrimArray et shape _ _))) =
  Array $ PrimArray et (rank $ 1 + shapeRank shape) Nonunique ()
valueType (ArrayValue _ (Array (PolyArray et targs shape _ _))) =
  Array $ PolyArray et targs (rank $ 1 + shapeRank shape) Nonunique ()
valueType (ArrayValue _ (Array (RecordArray et shape _))) =
  Array $ RecordArray et (rank $ 1 + shapeRank shape) Nonunique

-- | Construct a 'ShapeDecl' with the given number of zero-information
-- dimensions.
rank :: Int -> ShapeDecl ()
rank n = ShapeDecl $ replicate n ()

-- | The type of an Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
--
-- HACK: For terms that are really in a function position (such as the
-- functional argument to a @Map@), the type will be the *return
-- type*, even if the function is not fully applied.  This will change
-- once Futhark gets proper support for higher-order functions.
typeOf :: ExpBase Info VName -> CompType
typeOf (Literal val _) = Prim $ primValueType val
typeOf (Parens e _) = typeOf e
typeOf (QualParens _ e _) = typeOf e
typeOf (TupLit es _) = tupleRecord $ map typeOf es
typeOf (RecordLit fs _) =
  -- Reverse, because M.unions is biased to the left.
  Record $ M.unions $ reverse $ map record fs
  where record (RecordFieldExplicit name e _) = M.singleton name $ typeOf e
        record (RecordFieldImplicit name (Info t) _) = M.singleton (baseName name) t
typeOf (ArrayLit _ (Info t) _) =
  arrayType 1 t Unique `setAliases` mempty
typeOf (Range e _ _ _) =
  arrayType 1 (typeOf e) Unique `setAliases` mempty
typeOf (Empty (TypeDecl _ (Info t)) _) =
  arrayType 1 (fromStruct t) Unique
typeOf (BinOp _ _ _ (Info t) _) = t
typeOf (Project _ _ (Info t) _) = t
typeOf (If _ _ _ (Info t) _) = t
typeOf (Var _ (Info (_, Record ets)) _) = Record ets
typeOf (Var qn (Info (_, t)) _) = t `addAliases` S.insert (qualLeaf qn)
typeOf (Ascript e _ _) = typeOf e
typeOf (Apply _ _ _ (Info (_, t)) _) = t
typeOf (Negate e _) = typeOf e
typeOf (LetPat _ _ _ body _) = typeOf body
typeOf (LetFun _ _ body _) = typeOf body
typeOf (LetWith _ _ _ _ body _) = typeOf body
typeOf (Index ident idx _) =
  stripArray (length $ filter isFix idx) (typeOf ident)
  where isFix DimFix{} = True
        isFix _        = False
typeOf (Update e _ _ _) = typeOf e
typeOf (Reshape shape  e _) =
  typeOf e `setArrayShape` rank n
  where n = case typeOf shape of Record ts -> length ts
                                 _         -> 1
typeOf (Rearrange _ e _) = typeOf e
typeOf (Rotate _ _ e _) = typeOf e
typeOf (Zip i e es _) =
  Array $ RecordArray (M.fromList $ zip tupleFieldNames $
                       zipWith typeToRecordArrayElem es_ts es_us) (rank (1+i)) u
  where ts' = map typeOf $ e:es
        es_ts = map (stripArray (1+i)) ts'
        es_us = map uniqueness ts'
        u     = mconcat es_us
typeOf (Unzip _ ts _) =
  tupleRecord $ map unInfo ts
typeOf (Unsafe e _) =
  typeOf e
typeOf (Map f _ _) =
  arrayType 1 (typeOf f) Unique
typeOf (Reduce _ fun _ _ _) =
  typeOf fun
typeOf (Scan fun _ _ _) =
  arrayType 1 et Unique
  where et = typeOf fun
typeOf (Filter _ arr _) =
  typeOf arr
typeOf (Partition funs arr _) =
  tupleRecord $ replicate (length funs + 1) $ typeOf arr
typeOf (Stream form lam _ _) =
  case form of
    MapLike{}    -> typeOf lam `setUniqueness` Unique
    RedLike{}    -> typeOf lam `setUniqueness` Unique
typeOf (Concat _ x _ _) =
  typeOf x `setUniqueness` Unique `setAliases` S.empty
typeOf (DoLoop _ pat _ _ _ _) = patternType pat
typeOf (Lambda _ _ _ _ (Info t) _) =
  removeShapeAnnotations t `setAliases` mempty
typeOf (OpSection _ _ _ (Info t) _)      = toStruct t `setAliases` mempty
typeOf (OpSectionLeft _ _ _ (Info t) _)  = toStruct t `setAliases` mempty
typeOf (OpSectionRight _ _ _ (Info t) _) = toStruct t `setAliases` mempty

-- | The result of applying the arguments of the given types to a
-- function with the given return type, consuming its parameters with
-- the given diets.
returnType :: TypeBase dim ()
           -> [Diet]
           -> [CompType]
           -> TypeBase dim Names
returnType (Array at) ds args =
  Array $ arrayReturnType at ds args
returnType (Record fs) ds args =
  Record $ fmap (\et -> returnType et ds args) fs
returnType (Prim t) _ _ = Prim t
returnType (TypeVar t targs) ds args =
  TypeVar t $ map (\arg -> typeArgReturnType arg ds args) targs

typeArgReturnType :: TypeArg shape () -> [Diet] -> [CompType]
                  -> TypeArg shape Names
typeArgReturnType (TypeArgDim v loc) _ _ =
  TypeArgDim v loc
typeArgReturnType (TypeArgType t loc) ds args =
  TypeArgType (returnType t ds args) loc

arrayReturnType :: ArrayTypeBase dim ()
                -> [Diet]
                -> [CompType]
                -> ArrayTypeBase dim Names
arrayReturnType (PrimArray bt sz Nonunique ()) ds args =
  PrimArray bt sz Nonunique als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
arrayReturnType (PolyArray bt targs sz Nonunique ()) ds args =
  PolyArray bt (map (\arg -> typeArgReturnType arg ds args) targs) sz Nonunique als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
arrayReturnType (RecordArray et sz Nonunique) ds args =
  RecordArray (fmap (\t -> recordArrayElemReturnType t ds args) et) sz Nonunique
arrayReturnType (PrimArray et sz Unique ()) _ _ =
  PrimArray et sz Unique mempty
arrayReturnType (PolyArray et targs sz Unique ()) ds args =
  PolyArray et (map (\arg -> typeArgReturnType arg ds args) targs) sz Unique mempty
arrayReturnType (RecordArray et sz Unique) _ _ =
  RecordArray (fmap (`addRecordArrayElemAliases` const mempty) et) sz Unique

recordArrayElemReturnType :: RecordArrayElemTypeBase dim ()
                         -> [Diet]
                         -> [CompType]
                         -> RecordArrayElemTypeBase dim Names
recordArrayElemReturnType (PrimArrayElem bt ()) ds args =
  PrimArrayElem bt als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
recordArrayElemReturnType (PolyArrayElem bt targs () u) ds args =
  PolyArrayElem bt (map (\arg -> typeArgReturnType arg ds args) targs) als u
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
recordArrayElemReturnType (ArrayArrayElem at) ds args =
  ArrayArrayElem $ arrayReturnType at ds args
recordArrayElemReturnType (RecordArrayElem ts) ds args =
  RecordArrayElem $ fmap (\t -> recordArrayElemReturnType t ds args) ts

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

-- | The set of identifiers bound in a pattern.
patIdentSet :: (Functor f, Ord vn) => PatternBase f vn -> S.Set (IdentBase f vn)
patIdentSet (Id v t loc)            = S.singleton $ Ident v (removeShapeAnnotations <$> t) loc
patIdentSet (PatternParens p _)     = patIdentSet p
patIdentSet (TuplePattern pats _)   = mconcat $ map patIdentSet pats
patIdentSet (RecordPattern fs _)    = mconcat $ map (patIdentSet . snd) fs
patIdentSet Wildcard{}              = mempty
patIdentSet (PatternAscription p _) = patIdentSet p

-- | The type of values bound by the pattern.
patternType :: PatternBase Info VName -> CompType
patternType (Wildcard (Info t) _)   = removeShapeAnnotations t
patternType (PatternParens p _)     = patternType p
patternType (Id _ (Info t) _)       = removeShapeAnnotations t
patternType (TuplePattern pats _)   = tupleRecord $ map patternType pats
patternType (RecordPattern fs _)    = Record $ patternType <$> M.fromList fs
patternType (PatternAscription p _) = patternType p

-- | The type matched by the pattern, including shape declarations if present.
patternStructType :: PatternBase Info VName -> StructType
patternStructType (PatternAscription _ td) = unInfo $ expandedType td
patternStructType (PatternParens p _) = patternStructType p
patternStructType (Id _ (Info t) _) = t `setAliases` ()
patternStructType (TuplePattern ps _) = tupleRecord $ map patternStructType ps
patternStructType (RecordPattern fs _) = Record $ patternStructType <$> M.fromList fs
patternStructType (Wildcard (Info t) _) = vacuousShapeAnnotations $ toStruct t

-- | When viewed as a function parameter, this this pattern correspond
-- to a named parameter of some type?
patternParam :: PatternBase Info VName -> (Maybe VName, StructType)
patternParam (PatternParens p _) =
  patternParam p
patternParam (PatternAscription (Id v _ _) td) =
  (Just v, unInfo $ expandedType td)
patternParam p =
  (Nothing, patternStructType p)

-- | Remove all shape annotations from a pattern, leaving them unnamed
-- instead.
patternNoShapeAnnotations :: PatternBase Info VName -> PatternBase Info VName
patternNoShapeAnnotations (PatternAscription p (TypeDecl te (Info t))) =
  PatternAscription (patternNoShapeAnnotations p) $
  TypeDecl te $ Info $ vacuousShapeAnnotations t
patternNoShapeAnnotations (PatternParens p loc) =
  PatternParens (patternNoShapeAnnotations p) loc
patternNoShapeAnnotations (Id v (Info t) loc) =
  Id v (Info $ vacuousShapeAnnotations t) loc
patternNoShapeAnnotations (TuplePattern ps loc) =
  TuplePattern (map patternNoShapeAnnotations ps) loc
patternNoShapeAnnotations (RecordPattern ps loc) =
  RecordPattern (map (fmap patternNoShapeAnnotations) ps) loc
patternNoShapeAnnotations (Wildcard (Info t) loc) =
  Wildcard (Info (vacuousShapeAnnotations t)) loc

-- | The type of a parameter.
paramType :: ParamBase f vn -> TypeDeclBase f vn
paramType (UnnamedParam t) = t
paramType (NamedParam _ t _) = t

-- | The name of a parameter.  Not all parameters have names.
paramName :: ParamBase f vn -> Maybe vn
paramName UnnamedParam{} = Nothing
paramName (NamedParam v _ _) = Just v

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
-- or overloaded.  An overloaded builtin is a mapping from valid
-- parameter types to the result type.
data Intrinsic = IntrinsicMonoFun [PrimType] PrimType
               | IntrinsicOverloadedFun [([PrimType], PrimType)]
               | IntrinsicPolyFun [TypeParamBase VName] [TypeBase () ()] (TypeBase () ())
               | IntrinsicType PrimType
               | IntrinsicEquality -- Special cased.
               | IntrinsicOpaque

-- | A map of all built-ins.
intrinsics :: M.Map VName Intrinsic
intrinsics = M.fromList $ zipWith namify [10..] $

             map primFun (M.toList Primitive.primFuns) ++

             [ ("~", IntrinsicOverloadedFun $
                     [([Signed t], Signed t) | t <- [minBound..maxBound] ] ++
                     [([Unsigned t], Unsigned t) | t <- [minBound..maxBound] ])
             , ("!", IntrinsicMonoFun [Bool] Bool)] ++

             [("opaque", IntrinsicOpaque)] ++

             map unOpFun Primitive.allUnOps ++

             map binOpFun Primitive.allBinOps ++

             map cmpOpFun Primitive.allCmpOps ++

             map convOpFun Primitive.allConvOps ++

             map signFun Primitive.allIntTypes ++

             map unsignFun Primitive.allIntTypes ++

             map intrinsicType (map Signed [minBound..maxBound] ++
                                map Unsigned [minBound..maxBound] ++
                                map FloatType [minBound..maxBound] ++
                                [Bool]) ++

             -- The reason for the loop formulation is to ensure that we
             -- get a missing case warning if we forget a case.
             map mkIntrinsicBinOp [minBound..maxBound] ++

             [("scatter", IntrinsicPolyFun [tp_a]
                          [Array $ PolyArray tv_a' [] (rank 1) Unique (),
                           Array $ PrimArray (Signed Int32) (rank 1) Nonunique (),
                           Array $ PolyArray tv_a' [] (rank 1) Nonunique ()] $
                          Array $ PolyArray tv_a' [] (rank 1) Unique ())]

  where tv_a = VName (nameFromString "a") 0
        tv_a' = typeName tv_a
        tp_a = TypeParamType tv_a noLoc

        namify i (k,v) = (VName (nameFromString k) i, v)

        primFun (name, (ts,t, _)) =
          (name, IntrinsicMonoFun (map unPrim ts) $ unPrim t)

        unOpFun bop = (pretty bop, IntrinsicMonoFun [t] t)
          where t = unPrim $ Primitive.unOpType bop

        binOpFun bop = (pretty bop, IntrinsicMonoFun [t, t] t)
          where t = unPrim $ Primitive.binOpType bop

        cmpOpFun bop = (pretty bop, IntrinsicMonoFun [t, t] Bool)
          where t = unPrim $ Primitive.cmpOpType bop

        convOpFun cop = (pretty cop, IntrinsicMonoFun [unPrim ft] $ unPrim tt)
          where (ft, tt) = Primitive.convOpType cop

        signFun t = ("sign_" ++ pretty t, IntrinsicMonoFun [Unsigned t] $ Signed t)

        unsignFun t = ("unsign_" ++ pretty t, IntrinsicMonoFun [Signed t] $ Unsigned t)

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
        binOp ts = IntrinsicOverloadedFun [ ([t,t], t) | t <- ts ]

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

        ordering = IntrinsicOverloadedFun [ ([t,t], Bool) | t <- anyPrimType ]

-- | The largest tag used by an intrinsic - this can be used to
-- determine whether a 'VName' refers to an intrinsic or a user-defined name.
maxIntrinsicTag :: Int
maxIntrinsicTag = maximum $ map baseTag $ M.keys intrinsics

-- | Create a name with no qualifiers from a name.
qualName :: v -> QualName v
qualName = QualName []

-- | Add another qualifier (at the head) to a qualified name.
qualify :: v -> QualName v -> QualName v
qualify k (QualName ks v) = QualName (k:ks) v

-- | Create a type name name with no qualifiers from a 'VName'.
typeName :: VName -> TypeName
typeName = typeNameFromQualName . qualName

progImports :: ProgBase f vn -> [(String,SrcLoc)]
progImports = concatMap decImports . progDecs
  where decImports (OpenDec x xs _ _) =
          concatMap modExpImports $ x:xs
        decImports (ModDec md) =
          modExpImports $ modExp md
        decImports SigDec{} = []
        decImports TypeDec{} = []
        decImports ValDec{} = []
        decImports (LocalDec d _) = decImports d

        modExpImports ModVar{}              = []
        modExpImports (ModParens p _)       = modExpImports p
        modExpImports (ModImport f _ loc)   = [(f,loc)]
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

-- | A type parameter with no type annotations.
type UncheckedTypeParam = TypeParamBase Name

-- | A pattern with no type annotations.
type UncheckedPattern = PatternBase NoInfo Name

-- | A function declaration with no type annotations.
type UncheckedValBind = ValBindBase NoInfo Name

-- | A declaration with no type annotations.
type UncheckedDec = DecBase NoInfo Name

-- | A Futhark program with no type annotations.
type UncheckedProg = ProgBase NoInfo Name
