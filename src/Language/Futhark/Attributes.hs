{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Language.Syntax" simple, and put whatever embellishments
-- we need here.
module Language.Futhark.Attributes
  (
    builtInFunctions

  -- * Queries on expressions
  , typeOf
  , commutative

  -- * Queries on patterns
  , patNameSet
  , patIdentSet
  , patternType
  , patternStructType

  -- * Queries on types
  , primType
  , uniqueness
  , unique
  , tupleArrayElemUniqueness
  , aliases
  , diet
  , subtypeOf
  , similarTo
  , arrayRank
  , arrayDims
  , arrayDims'
  , nestedDims
  , nestedDims'
  , setArrayShape
  , removeShapeAnnotations
  , vacuousShapeAnnotations
  , returnType
  , lambdaReturnType

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
  , tupleArrayElemToType

  -- ** Removing and adding names
  --
  -- $names
  , removeNames
  , addNames
  , nameToQualName

  -- * Queries on values
  , valueType

  -- * Getters for decs
  , isType
  , isFun
  , isConst
  , isFunOrType
  , isMod
  , isSig

  -- | Values of these types are produces by the parser.  They use
  -- unadorned names and have no type information, apart from that
  -- which is syntactically required.
  , NoInfo(..)
  , UncheckedType
  , UncheckedUserType
  , UncheckedArrayType
  , UncheckedIdent
  , UncheckedTypeDecl
  , UncheckedUserTypeDecl
  , UncheckedDimIndex
  , UncheckedExp
  , UncheckedLambda
  , UncheckedPattern
  , UncheckedFunDef
  , UncheckedProg
  , UncheckedProgWithHeaders
  )
  where

import           Control.Monad.Writer
import           Data.Hashable
import qualified Data.HashMap.Lazy       as HM
import qualified Data.HashSet            as HS
import           Data.List
import           Data.Loc
import           Data.Maybe

import           Prelude

import           Language.Futhark.Syntax

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayRank :: ArrayShape (shape vn) =>
             TypeBase shape as vn -> Int
arrayRank = shapeRank . arrayShape

-- | Return the shape of a type - for non-arrays, this is 'mempty'.
arrayShape :: ArrayShape (shape vn) =>
              TypeBase shape as vn -> shape vn
arrayShape (Array (PrimArray _ ds _ _)) = ds
arrayShape (Array (TupleArray _ ds _))  = ds
arrayShape _                            = mempty

-- | Return the shape of a type - for non-arrays, this is 'mempty'.
arrayShape' :: UserType vn -> ShapeDecl vn
arrayShape' (UserArray t d _) = ShapeDecl [d] <> arrayShape' t
arrayShape' (UserUnique t _)  = arrayShape' t
arrayShape' _                 = mempty

-- | Return the dimensions of a type with (possibly) known dimensions.
arrayDims :: Ord vn => TypeBase ShapeDecl as vn -> [DimDecl vn]
arrayDims = shapeDims . arrayShape

-- | Return the dimensions of a type with (possibly) known dimensions.
arrayDims' :: UserType vn -> [DimDecl vn]
arrayDims' = shapeDims . arrayShape'

-- | Return any shape declaration in the type, with duplicates removed.
nestedDims :: Ord vn => TypeBase ShapeDecl as vn -> [DimDecl vn]
nestedDims t =
  case t of Array a  -> nub $ arrayNestedDims a
            Tuple ts -> nub $ mconcat $ map nestedDims ts
            Prim{}   -> mempty
  where arrayNestedDims (PrimArray _ ds _ _) =
          shapeDims ds
        arrayNestedDims (TupleArray ts ds _) =
          shapeDims ds <> mconcat (map tupleArrayElemNestedDims ts)
        tupleArrayElemNestedDims (ArrayArrayElem a) =
          arrayNestedDims a
        tupleArrayElemNestedDims (TupleArrayElem ts) =
          mconcat $ map tupleArrayElemNestedDims ts
        tupleArrayElemNestedDims PrimArrayElem{} =
          mempty

-- | Return any shape declaration in the type, with duplicates removed.
nestedDims' :: Ord vn => UserType vn -> [DimDecl vn]
nestedDims' (UserArray t d _) = nub $ d : nestedDims' t
nestedDims' (UserTuple ts _)  = nub $ mconcat $ map nestedDims' ts
nestedDims' (UserUnique t _)  = nestedDims' t
nestedDims' _                 = mempty

-- | Set the dimensions of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayShape :: shape vn -> TypeBase shape as vn -> TypeBase shape as vn
setArrayShape ds (Array (PrimArray et _ u as)) = Array $ PrimArray et ds u as
setArrayShape ds (Array (TupleArray et _ u))   = Array $ TupleArray et ds u
setArrayShape _  (Tuple ts)                    = Tuple ts
setArrayShape _  (Prim t)                      = Prim t

-- | Change the shape of a type to be just the 'Rank'.
removeShapeAnnotations :: ArrayShape (shape vn) =>
                          TypeBase shape as vn -> TypeBase Rank as vn
removeShapeAnnotations = modifyShapeAnnotations $ Rank . shapeRank

-- | Change the shape of a type to be a 'ShapeDecl' where all
-- dimensions are 'Nothing'.
vacuousShapeAnnotations :: ArrayShape (shape vn) =>
                           TypeBase shape as vn -> TypeBase ShapeDecl as vn
vacuousShapeAnnotations = modifyShapeAnnotations $ \shape ->
  ShapeDecl (replicate (shapeRank shape) AnyDim)

-- | Change the shape of a type.
modifyShapeAnnotations :: (oldshape vn -> newshape vn)
                       -> TypeBase oldshape as vn
                       -> TypeBase newshape as vn
modifyShapeAnnotations f (Array at) =
  Array $ modifyShapeAnnotationsFromArray f at
modifyShapeAnnotations f (Tuple ts) =
  Tuple $ map (modifyShapeAnnotations f) ts
modifyShapeAnnotations _ (Prim t) =
  Prim t

modifyShapeAnnotationsFromArray :: (oldshape vn -> newshape vn)
                                -> ArrayTypeBase oldshape as vn
                                -> ArrayTypeBase newshape as vn
modifyShapeAnnotationsFromArray f (PrimArray et shape u as) =
  PrimArray et (f shape) u as
modifyShapeAnnotationsFromArray f (TupleArray ts shape u) =
  TupleArray
  (map (modifyShapeAnnotationsFromTupleArrayElem f) ts)
  (f shape) u

  -- Try saying this one three times fast.
modifyShapeAnnotationsFromTupleArrayElem :: (oldshape vn -> newshape vn)
                                         -> TupleArrayElemTypeBase oldshape as vn
                                         -> TupleArrayElemTypeBase newshape as vn
modifyShapeAnnotationsFromTupleArrayElem
  _ (PrimArrayElem bt as u) = PrimArrayElem bt as u
modifyShapeAnnotationsFromTupleArrayElem
  f (ArrayArrayElem at) = ArrayArrayElem $ modifyShapeAnnotationsFromArray f at
modifyShapeAnnotationsFromTupleArrayElem
  f (TupleArrayElem ts) = TupleArrayElem $ map (modifyShapeAnnotationsFromTupleArrayElem f) ts

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _              = True

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf :: ArrayShape (shape vn) =>
             TypeBase shape as1 vn -> TypeBase shape as2 vn -> Bool
subtypeOf
  (Array (PrimArray t1 dims1 u1 _))
  (Array (PrimArray t2 dims2 u2 _)) =
  u1 `subuniqueOf` u2
       && t1 == t2
       && dims1 == dims2
subtypeOf
  (Array (TupleArray et1 dims1 u1))
  (Array (TupleArray et2 dims2 u2)) =
  u1 `subuniqueOf` u2
       && length et1 == length et2
       && and (zipWith subtypeOf
               (map tupleArrayElemToType et1)
               (map tupleArrayElemToType et2))
       && dims1 == dims2
subtypeOf (Tuple ts1) (Tuple ts2) =
  length ts1 == length ts2 && and (zipWith subtypeOf ts1 ts2)
subtypeOf (Prim bt1) (Prim bt2) = bt1 == bt2
subtypeOf _ _ = False

-- | @x \`similarTo\` y@ is true if @x@ and @y@ are the same type,
-- ignoring uniqueness.
similarTo :: ArrayShape (shape vn) =>
             TypeBase shape as1 vn
          -> TypeBase shape as2 vn
          -> Bool
similarTo t1 t2 = t1 `subtypeOf` t2 || t2 `subtypeOf` t1

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape as vn -> Uniqueness
uniqueness (Array (PrimArray _ _ u _)) = u
uniqueness (Array (TupleArray _ _ u))  = u
uniqueness _                           = Nonunique

tupleArrayElemUniqueness :: TupleArrayElemTypeBase shape as vn -> Uniqueness
tupleArrayElemUniqueness (PrimArrayElem _ _ u) = u
tupleArrayElemUniqueness (ArrayArrayElem (PrimArray _ _ u _)) = u
tupleArrayElemUniqueness (ArrayArrayElem (TupleArray _ _ u)) = u
tupleArrayElemUniqueness (TupleArrayElem ts) = mconcat $ map tupleArrayElemUniqueness ts

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase shape as vn -> Bool
unique = (==Unique) . uniqueness

-- | Return the set of all variables mentioned in the aliasing of a
-- type.
aliases :: Monoid (as vn) => TypeBase shape as vn -> as vn
aliases (Array (PrimArray _ _ _ als)) = als
aliases (Array (TupleArray ts _ _))   = mconcat $ map tupleArrayElemAliases ts
aliases (Tuple et)                    = mconcat $ map aliases et
aliases (Prim _)                      = mempty

tupleArrayElemAliases :: Monoid (as vn) =>
                         TupleArrayElemTypeBase shape as vn -> as vn
tupleArrayElemAliases (PrimArrayElem _ als _) = als
tupleArrayElemAliases (ArrayArrayElem (PrimArray _ _ _ als)) =
  als
tupleArrayElemAliases (ArrayArrayElem (TupleArray ts _ _)) =
  mconcat $ map tupleArrayElemAliases ts
tupleArrayElemAliases (TupleArrayElem ts) =
  mconcat $ map tupleArrayElemAliases ts

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ might consume its argument.
diet :: TypeBase shape as vn -> Diet
diet (Tuple ets)                         = TupleDiet $ map diet ets
diet (Prim _)                            = Observe
diet (Array (PrimArray _ _ Unique _))    = Consume
diet (Array (PrimArray _ _ Nonunique _)) = Observe
diet (Array (TupleArray _ _ Unique))     = Consume
diet (Array (TupleArray _ _ Nonunique))  = Observe

-- | @t `maskAliases` d@ removes aliases (sets them to 'mempty') from
-- the parts of @t@ that are denoted as 'Consumed' by the 'Diet' @d@.
maskAliases :: Monoid (as vn) =>
               TypeBase shape as vn
            -> Diet
            -> TypeBase shape as vn
maskAliases t Consume = t `setAliases` mempty
maskAliases t Observe = t
maskAliases (Tuple ets) (TupleDiet ds) =
  Tuple $ zipWith maskAliases ets ds
maskAliases _ _ = error "Invalid arguments passed to maskAliases."

-- | Convert any type to one that has rank information, no alias
-- information, and no embedded names.
toStructural :: (ArrayShape (shape vn)) =>
                TypeBase shape as vn
             -> TypeBase Rank NoInfo ()
toStructural = removeNames . removeShapeAnnotations

-- | Remove aliasing information from a type.
toStruct :: TypeBase shape as vn
         -> TypeBase shape NoInfo vn
toStruct t = t `setAliases` NoInfo

-- | Replace no aliasing with an empty alias set.
fromStruct :: TypeBase shape as vn
           -> TypeBase shape Names vn
fromStruct t = t `setAliases` HS.empty

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: ArrayShape (shape vn) =>
             Int -> TypeBase shape as vn -> Maybe (TypeBase shape as vn)
peelArray 0 t = Just t
peelArray n (Array (PrimArray et shape _ _))
  | shapeRank shape == n =
    Just $ Prim et
peelArray n (Array (TupleArray ts shape _))
  | shapeRank shape == n =
    Just $ Tuple $ map asType ts
  where asType (PrimArrayElem bt _ _) = Prim bt
        asType (ArrayArrayElem at)    = Array at
        asType (TupleArrayElem ts')   = Tuple $ map asType ts'
peelArray n (Array (PrimArray et shape u als)) = do
  shape' <- stripDims n shape
  return $ Array $ PrimArray et shape' u als
peelArray n (Array (TupleArray et shape u)) = do
  shape' <- stripDims n shape
  return $ Array $ TupleArray et shape' u
peelArray _ _ = Nothing

-- | A type is a primitive type if it is not an array and any component
-- types are prim types.
primType :: TypeBase shape as vn -> Bool
primType (Tuple ts) = all primType ts
primType (Prim _)   = True
primType (Array _)  = False

-- $names
--
-- The element type annotation of 'ArrayVal' values is a 'TypeBase'
-- with '()' for variable names.  This means that when we construct
-- 'ArrayVal's based on a type with a more common name representation,
-- we need to remove the names and replace them with '()'s.  Since the
-- only place names appear in types is in the array size annotations,
-- we have to replace the shape with a 'Rank'.

-- | Remove names from a type - this involves removing all size
-- annotations from arrays, as well as all aliasing.
removeNames :: ArrayShape (shape vn) =>
               TypeBase shape as vn
            -> TypeBase Rank NoInfo ()
removeNames (Prim et)  = Prim et
removeNames (Tuple ts) = Tuple $ map removeNames ts
removeNames (Array at) = Array $ removeArrayNames at

removeArrayNames :: ArrayShape (shape vn) =>
                    ArrayTypeBase shape as vn
                 -> ArrayTypeBase Rank NoInfo ()
removeArrayNames (PrimArray et shape u _) =
  PrimArray et (Rank $ shapeRank shape) u NoInfo
removeArrayNames (TupleArray et shape u) =
  TupleArray (map removeTupleArrayElemNames et) (Rank $ shapeRank shape) u

removeTupleArrayElemNames :: ArrayShape (shape vn) =>
                             TupleArrayElemTypeBase shape as vn
                          -> TupleArrayElemTypeBase Rank NoInfo ()
removeTupleArrayElemNames (PrimArrayElem bt _ u) =
  PrimArrayElem bt NoInfo u
removeTupleArrayElemNames (ArrayArrayElem et) =
  ArrayArrayElem $ removeArrayNames et
removeTupleArrayElemNames (TupleArrayElem ts) =
  TupleArrayElem $ map removeTupleArrayElemNames ts

-- | Add names to a type.
addNames :: TypeBase Rank NoInfo ()
         -> TypeBase Rank NoInfo vn
addNames (Prim et)  = Prim et
addNames (Tuple ts) = Tuple $ map addNames ts
addNames (Array at) = Array $ addArrayNames at

addArrayNames :: ArrayTypeBase Rank NoInfo ()
              -> ArrayTypeBase Rank NoInfo vn
addArrayNames (PrimArray et (Rank n) u _) =
  PrimArray et (Rank n) u NoInfo
addArrayNames (TupleArray et (Rank n) u) =
  TupleArray (map addTupleArrayElemNames et) (Rank n) u

addTupleArrayElemNames :: TupleArrayElemTypeBase Rank NoInfo ()
                       -> TupleArrayElemTypeBase Rank NoInfo vn
addTupleArrayElemNames (PrimArrayElem bt _ u) =
  PrimArrayElem bt NoInfo u
addTupleArrayElemNames (ArrayArrayElem et) =
  ArrayArrayElem $ addArrayNames et
addTupleArrayElemNames (TupleArrayElem ts) =
  TupleArrayElem $ map addTupleArrayElemNames ts

-- | @arrayOf t s u@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.
arrayOf :: (ArrayShape (shape vn), Monoid (as vn)) =>
           TypeBase shape as vn
        -> shape vn
        -> Uniqueness
        -> TypeBase shape as vn
arrayOf (Array (PrimArray et shape1 _ als)) shape2 u =
  Array $ PrimArray et (shape2 <> shape1) u als
arrayOf (Array (TupleArray et shape1 _)) shape2 u =
  Array $ TupleArray et (shape2 <> shape1) u
arrayOf (Prim et) shape u =
  Array $ PrimArray et shape u mempty
arrayOf (Tuple ts) shape u =
  Array $ TupleArray (map (`typeToTupleArrayElem` u) ts) shape u

typeToTupleArrayElem :: Monoid (as vn) =>
                        TypeBase shape as vn
                     -> Uniqueness
                     -> TupleArrayElemTypeBase shape as vn
typeToTupleArrayElem (Prim bt)   u = PrimArrayElem bt mempty u
typeToTupleArrayElem (Tuple ts') u = TupleArrayElem $ map (`typeToTupleArrayElem` u) ts'
typeToTupleArrayElem (Array at)  _ = ArrayArrayElem at

tupleArrayElemToType :: TupleArrayElemTypeBase shape as vn
                     -> TypeBase shape as vn
tupleArrayElemToType (PrimArrayElem bt _ _) = Prim bt
tupleArrayElemToType (TupleArrayElem ts)    = Tuple $ map tupleArrayElemToType ts
tupleArrayElemToType (ArrayArrayElem at)    = Array at

-- | @array n t@ is the type of @n@-dimensional arrays having @t@ as
-- the base type.  If @t@ is itself an m-dimensional array, the result
-- is an @n+m@-dimensional array with the same base type as @t@.  If
-- you need to specify size information for the array, use 'arrayOf'
-- instead.
arrayType :: (ArrayShape (shape vn), Monoid (as vn)) =>
             Int
          -> TypeBase shape as vn
          -> Uniqueness
          -> TypeBase Rank as vn
arrayType 0 t _ = removeShapeAnnotations t
arrayType n t u = arrayOf (removeShapeAnnotations t) (Rank n) u

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: (ArrayShape (shape vn), Monoid (as vn)) =>
              Int -> TypeBase shape as vn -> TypeBase shape as vn
stripArray n (Array (PrimArray et shape u als))
  | Just shape' <- stripDims n shape =
    Array $ PrimArray et shape' u als
  | otherwise = Prim et
stripArray n (Array (TupleArray et shape u))
  | Just shape' <- stripDims n shape =
    Array $ TupleArray et shape' u
  | otherwise = Tuple $ map tupleArrayElemToType et
stripArray _ t = t

-- | Set the uniqueness attribute of a type.  If the type is a tuple,
-- the uniqueness of its components will be modified.
setUniqueness :: TypeBase shape as vn -> Uniqueness -> TypeBase shape as vn
setUniqueness (Array at) u =
  Array $ setArrayUniqueness at u
setUniqueness (Tuple ets) u =
  Tuple $ map (`setUniqueness` u) ets
setUniqueness t _ = t

setArrayUniqueness :: ArrayTypeBase shape as vn -> Uniqueness
                   -> ArrayTypeBase shape as vn
setArrayUniqueness (PrimArray et dims _ als) u =
  PrimArray et dims u als
setArrayUniqueness (TupleArray et dims _) u =
  TupleArray (map (`setTupleArrayElemUniqueness` u) et) dims u

setTupleArrayElemUniqueness :: TupleArrayElemTypeBase shape as vn -> Uniqueness
                            -> TupleArrayElemTypeBase shape as vn
setTupleArrayElemUniqueness (PrimArrayElem bt als _) u =
  PrimArrayElem bt als u
setTupleArrayElemUniqueness (ArrayArrayElem at) u =
  ArrayArrayElem $ setArrayUniqueness at u
setTupleArrayElemUniqueness (TupleArrayElem ts) u =
  TupleArrayElem $ map (`setTupleArrayElemUniqueness` u) ts

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliasing.
setAliases :: TypeBase shape asf vn -> ast vn -> TypeBase shape ast vn
setAliases t = addAliases t . const

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliasing replaced by @f@ applied to that aliasing.
addAliases :: TypeBase shape asf vn -> (asf vn -> ast vn)
           -> TypeBase shape ast vn
addAliases (Array at) f =
  Array $ addArrayAliases at f
addAliases (Tuple ts) f =
  Tuple $ map (`addAliases` f) ts
addAliases (Prim et) _ =
  Prim et

addArrayAliases :: ArrayTypeBase shape asf vn
                -> (asf vn -> ast vn)
                -> ArrayTypeBase shape ast vn
addArrayAliases (PrimArray et dims u als) f =
  PrimArray et dims u $ f als
addArrayAliases (TupleArray et dims u) f =
  TupleArray (map (`addTupleArrayElemAliases` f) et) dims u

addTupleArrayElemAliases :: TupleArrayElemTypeBase shape asf vn
                         -> (asf vn -> ast vn)
                         -> TupleArrayElemTypeBase shape ast vn
addTupleArrayElemAliases (PrimArrayElem bt als u) f =
  PrimArrayElem bt (f als) u
addTupleArrayElemAliases (ArrayArrayElem at) f =
  ArrayArrayElem $ addArrayAliases at f
addTupleArrayElemAliases (TupleArrayElem ts) f =
  TupleArrayElem $ map (`addTupleArrayElemAliases` f) ts

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

-- | The type of an Futhark value.
valueType :: Value -> TypeBase Rank NoInfo vn
valueType (PrimValue bv) = Prim $ primValueType bv
valueType (TupValue vs) = Tuple (map valueType vs)
valueType (ArrayValue _ (Prim et)) =
  Array $ PrimArray et (Rank 1) Nonunique NoInfo
valueType (ArrayValue _ (Tuple ts)) =
  addNames $ Array $ TupleArray (map (`typeToTupleArrayElem` Nonunique) ts) (Rank 1) Nonunique
valueType (ArrayValue _ (Array (PrimArray et shape _ _))) =
  Array $ PrimArray et (Rank $ 1 + shapeRank shape) Nonunique NoInfo
valueType (ArrayValue _ (Array (TupleArray et shape _))) =
  addNames $ Array $ TupleArray et (Rank $ 1 + shapeRank shape) Nonunique

-- | The type of an Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: (Ord vn, Hashable vn) => ExpBase Info vn -> CompTypeBase vn
typeOf (Literal val _) = Prim $ primValueType val
typeOf (TupLit es _) = Tuple $ map typeOf es
typeOf (ArrayLit es (Info t) _) =
  arrayType 1 t $ mconcat $ map (uniqueness . typeOf) es
typeOf (Empty (TypeDecl _ (Info t)) _) =
  arrayType 1 (fromStruct t) Unique
typeOf (BinOp _ _ _ (Info t) _) = t
typeOf (UnOp Not _ _) = Prim Bool
typeOf (UnOp Negate e _) = typeOf e
typeOf (UnOp Complement e _) = typeOf e
typeOf (UnOp Abs e _) = typeOf e
typeOf (UnOp Signum e _) = typeOf e
typeOf (UnOp (ToFloat t) _ _) = Prim $ FloatType t
typeOf (UnOp (ToSigned t) _ _) = Prim $ Signed t
typeOf (UnOp (ToUnsigned t) _ _) = Prim $ Unsigned t
typeOf (If _ _ _ (Info t) _) = t
typeOf (Var _ (Info (Tuple ets)) _) = Tuple ets
typeOf (Var (QualName (_, name)) (Info t) _) = t `addAliases` HS.insert name
typeOf (Apply _ _ (Info t) _) = t
typeOf (LetPat _ _ body _) = typeOf body
typeOf (LetWith _ _ _ _ body _) = typeOf body
typeOf (Index ident idx _) =
  stripArray (length $ filter isFix idx) (typeOf ident)
  where isFix DimFix{} = True
        isFix _        = False
typeOf (TupleIndex _ _ (Info t) _) = t
typeOf (Iota e _) = arrayType 1 (typeOf e) Unique
typeOf (Shape _ _) = Array $ PrimArray (Signed Int32) (Rank 1) Unique mempty
typeOf (Replicate _ e _) = arrayType 1 (typeOf e) Unique `setAliases` mempty
typeOf (Reshape shape  e _) =
  Rank n `setArrayShape` typeOf e
  where n = case typeOf shape of Tuple ts -> length ts
                                 _        -> 1
typeOf (Rearrange _ e _) = typeOf e
typeOf (Transpose e _) = typeOf e
typeOf (Rotate _ _ e _) = typeOf e
typeOf (Map f _ _) = arrayType 1 et Unique `setAliases` HS.empty
  where et = lambdaReturnType f
typeOf (Reduce _ fun _ _ _) =
  lambdaReturnType fun `setAliases` mempty
typeOf (Zip i e es _) =
  Array $ TupleArray (zipWith typeToTupleArrayElem es_ts es_us) (Rank (1+i)) u
  where ts' = map typeOf $ e:es
        es_ts = map (stripArray (1+i)) ts'
        es_us = map uniqueness ts'
        u     = mconcat es_us
typeOf (Unzip _ ts _) =
  Tuple $ map unInfo ts
typeOf (Unsafe e _) =
  typeOf e
typeOf (Scan fun _ _ _) =
  arrayType 1 et Unique
  where et = lambdaReturnType fun `setAliases` mempty
typeOf (Filter _ arr _) =
  typeOf arr
typeOf (Partition funs arr _) =
  Tuple $ replicate (length funs + 1) $ typeOf arr
typeOf (Stream form lam _ _) =
  case form of
    MapLike{}    -> lambdaReturnType lam
                    `setAliases` HS.empty
                    `setUniqueness` Unique
    RedLike{}    -> lambdaReturnType lam
                    `setAliases` HS.empty
                    `setUniqueness` Unique
    Sequential{} -> lambdaReturnType lam
                    `setAliases` HS.empty
                    `setUniqueness` Unique
typeOf (Concat _ x _ _) =
  typeOf x `setUniqueness` Unique `setAliases` HS.empty
typeOf (Split _ splitexps e _) =
  Tuple $ replicate (1 + n) (typeOf e)
  where n = case typeOf splitexps of Tuple ts -> length ts
                                     _        -> 1
typeOf (Copy e _) = typeOf e `setUniqueness` Unique `setAliases` HS.empty
typeOf (DoLoop _ _ _ _ body _) = typeOf body
typeOf (Write _i _v a _) = typeOf a `setAliases` HS.empty

-- | The result of applying the arguments of the given types to a
-- function with the given return type, consuming its parameters with
-- the given diets.
returnType :: (Ord vn, Hashable vn) =>
              TypeBase shape NoInfo vn
           -> [Diet]
           -> [CompTypeBase vn]
           -> TypeBase shape Names vn
returnType (Array at) ds args =
  Array $ arrayReturnType at ds args
returnType (Tuple ets) ds args =
  Tuple $ map (\et -> returnType et ds args) ets
returnType (Prim t) _ _ = Prim t

arrayReturnType :: (Eq vn, Hashable vn) =>
                   ArrayTypeBase shape NoInfo vn
                -> [Diet]
                -> [CompTypeBase vn]
                -> ArrayTypeBase shape Names vn
arrayReturnType (PrimArray bt sz Nonunique NoInfo) ds args =
  PrimArray bt sz Nonunique als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
arrayReturnType (TupleArray et sz Nonunique) ds args =
  TupleArray (map (\t -> tupleArrayElemReturnType t ds args) et) sz Nonunique
arrayReturnType (PrimArray et sz Unique NoInfo) _ _ =
  PrimArray et sz Unique mempty
arrayReturnType (TupleArray et sz Unique) _ _ =
  TupleArray (map (`addTupleArrayElemAliases` const mempty) et) sz Unique

tupleArrayElemReturnType :: (Eq vn, Hashable vn) =>
                            TupleArrayElemTypeBase shape NoInfo vn
                         -> [Diet]
                         -> [CompTypeBase vn]
                         -> TupleArrayElemTypeBase shape Names vn
tupleArrayElemReturnType (PrimArrayElem bt NoInfo u) ds args =
  PrimArrayElem bt als u
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
tupleArrayElemReturnType (ArrayArrayElem at) ds args =
  ArrayArrayElem $ arrayReturnType at ds args
tupleArrayElemReturnType (TupleArrayElem ts) ds args =
  TupleArrayElem $ map (\t -> tupleArrayElemReturnType t ds args) ts

-- | The specified return type of a lambda.
lambdaReturnType :: Ord vn =>
                    LambdaBase Info vn -> TypeBase Rank NoInfo vn
lambdaReturnType (AnonymFun _ _ _ (Info t) _)       = removeShapeAnnotations t
lambdaReturnType (CurryFun _ _ (Info t) _)          = toStruct t
lambdaReturnType (UnOpFun _ _ (Info t) _)           = toStruct t
lambdaReturnType (BinOpFun _ _ _ (Info t) _)        = toStruct t
lambdaReturnType (CurryBinOpLeft _ _ _ (Info t) _)  = toStruct t
lambdaReturnType (CurryBinOpRight _ _ _ (Info t) _) = toStruct t

-- | Is the given binary operator commutative?
commutative :: BinOp -> Bool
commutative = flip elem [Plus, Pow, Times, Band, Xor, Bor, LogAnd, LogOr, Equal]

-- | The set of names bound in a pattern, including dimension declarations.
patNameSet :: (Ord vn, Eq vn, Hashable vn) => PatternBase NoInfo vn -> HS.HashSet vn
patNameSet =  HS.fromList . map identName . patIdentsGen sizeIdent
  where sizeIdent name = Ident name NoInfo

-- | The set of identifiers bound in a pattern, including dimension declarations.
patIdentSet :: (Ord vn, Eq vn, Hashable vn) => PatternBase Info vn -> HS.HashSet (IdentBase Info vn)
patIdentSet = HS.fromList . patIdentsGen sizeIdent
  where sizeIdent name = Ident name (Info $ Prim $ Signed Int32)

patIdentsGen :: (Ord vn, Eq vn, Hashable vn) =>
                (vn -> SrcLoc -> IdentBase f vn) -> PatternBase f vn
             -> [IdentBase f vn]
patIdentsGen _ (Id ident)              = [ident]
patIdentsGen f (TuplePattern pats _)   = mconcat $ map (patIdentsGen f) pats
patIdentsGen _ Wildcard{}              = []
patIdentsGen f (PatternAscription p t) =
  patIdentsGen f p <> mapMaybe (dimIdent (srclocOf p)) (nestedDims' (declaredType t))
  where dimIdent _ AnyDim            = Nothing
        dimIdent _ (ConstDim _)      = Nothing
        dimIdent loc (NamedDim name) = Just $ f name loc

-- | The type of values bound by the pattern.
patternType :: PatternBase Info VName -> CompTypeBase VName
patternType (Wildcard (Info t) _)   = t
patternType (Id ident)              = unInfo $ identType ident
patternType (TuplePattern pats _)   = Tuple $ map patternType pats
patternType (PatternAscription p _) = patternType p

-- | The type matched by the pattern, including shape declarations if present.
patternStructType :: PatternBase Info VName -> StructTypeBase VName
patternStructType (PatternAscription _ td) = unInfo $ expandedType td
patternStructType (Id v) = vacuousShapeAnnotations $ toStruct $ unInfo $ identType v
patternStructType (TuplePattern ps _) = Tuple $ map patternStructType ps
patternStructType (Wildcard (Info t) _) = vacuousShapeAnnotations $ toStruct t

-- | A map of all built-in functions and their types.
builtInFunctions :: HM.HashMap VName (PrimType,[PrimType])
builtInFunctions = HM.fromList $ zipWith namify [0..]
                   [("sqrt32", (FloatType Float32, [FloatType Float32]))
                   ,("log32", (FloatType Float32, [FloatType Float32]))
                   ,("exp32", (FloatType Float32, [FloatType Float32]))
                   ,("cos32", (FloatType Float32, [FloatType Float32]))
                   ,("sin32", (FloatType Float32, [FloatType Float32]))
                   ,("acos32", (FloatType Float32, [FloatType Float32]))
                   ,("asin32", (FloatType Float32, [FloatType Float32]))
                   ,("atan32", (FloatType Float32, [FloatType Float32]))
                   ,("atan2_32", (FloatType Float32, [FloatType Float32, FloatType Float32]))
                   ,("isinf32", (Bool, [FloatType Float32]))
                   ,("isnan32", (Bool, [FloatType Float32]))

                   ,("sqrt64", (FloatType Float64, [FloatType Float64]))
                   ,("log64", (FloatType Float64, [FloatType Float64]))
                   ,("exp64", (FloatType Float64, [FloatType Float64]))
                   ,("cos64", (FloatType Float64, [FloatType Float64]))
                   ,("sin64", (FloatType Float64, [FloatType Float64]))
                   ,("acos64", (FloatType Float64, [FloatType Float64]))
                   ,("asin64", (FloatType Float64, [FloatType Float64]))
                   ,("atan64", (FloatType Float64, [FloatType Float64]))
                   ,("atan2_64", (FloatType Float64, [FloatType Float64, FloatType Float64]))
                   ,("isinf64", (Bool, [FloatType Float64]))
                   ,("isnan64", (Bool, [FloatType Float64]))
                   ]
  where namify i (k,v) = (ID (nameFromString k, i), v)

isFun :: DecBase f vn -> Maybe (FunDefBase f vn)
isFun (FunOrTypeDec (FunDec a)) = Just a
isFun _                         = Nothing

isConst :: DecBase f vn -> Maybe (ConstDefBase f vn)
isConst (FunOrTypeDec (ConstDec a)) = Just a
isConst _                           = Nothing

isType :: DecBase f vn -> Maybe (TypeDefBase f vn)
isType (FunOrTypeDec (TypeDec t)) = Just t
isType _                          = Nothing

isFunOrType :: DecBase f vn -> Maybe (FunOrTypeDecBase f vn)
isFunOrType (FunOrTypeDec ft) = Just ft
isFunOrType _                 = Nothing

isSig :: DecBase f vn -> Maybe (SigDefBase f vn)
isSig (SigDec sig) = Just sig
isSig _            = Nothing

isMod :: DecBase f vn -> Maybe (ModDefBase f vn)
isMod (ModDec modd) = Just modd
isMod _             = Nothing

-- | Create a name with no qualifiers from a name.
nameToQualName :: Name -> QualName Name
nameToQualName n = QualName ([], n)

-- | A type with no aliasing information but shape annotations.
type UncheckedType = TypeBase ShapeDecl NoInfo Name

type UncheckedUserType = UserType Name

-- | An array type with no aliasing information.
type UncheckedArrayType = ArrayTypeBase ShapeDecl NoInfo Name

-- | A type declaration with no expanded type.
type UncheckedUserTypeDecl = TypeDeclBase NoInfo Name

-- | A type declaration with no expanded type.
type UncheckedTypeDecl = TypeDeclBase NoInfo Name

-- | An identifier with no type annotations.
type UncheckedIdent = IdentBase NoInfo Name

-- | An index with no type annotations.
type UncheckedDimIndex = DimIndexBase NoInfo Name

-- | An expression with no type annotations.
type UncheckedExp = ExpBase NoInfo Name

-- | A lambda with no type annotations.
type UncheckedLambda = LambdaBase NoInfo Name

-- | A pattern with no type annotations.
type UncheckedPattern = PatternBase NoInfo Name

-- | A function declaration with no type annotations.
type UncheckedFunDef = FunDefBase NoInfo Name

-- | An Futhark program with no type annotations.
type UncheckedProg = ProgBase NoInfo Name

-- | An Futhark program with no type annotations, but with headers.
type UncheckedProgWithHeaders = ProgBaseWithHeaders NoInfo Name
