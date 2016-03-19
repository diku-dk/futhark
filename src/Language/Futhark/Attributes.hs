{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Language.Syntax" simple, and put whatever embellishments
-- we need here.
module Language.Futhark.Attributes
  (
    funDecByName
  , isBuiltInFunction
  , builtInFunctions

  -- * Parameter handling
  , toParam
  , fromParam

  -- * Queries on expressions
  , expToValue
  , typeOf
  , commutative

  -- * Queries on patterns
  , patNames
  , patNameSet
  , patIdents
  , patIdentSet

  -- * Queries on types
  , primType
  , uniqueness
  , unique
  , uniqueOrPrim
  , aliases
  , diet
  , dietingAs
  , subtypeOf
  , similarTo
  , arrayRank
  , arrayDims
  , nestedDims
  , setArrayShape
  , removeShapeAnnotations
  , vacuousShapeAnnotations
  , returnType
  , lambdaType
  , lambdaReturnType

  -- * Operations on types
  , stripArray
  , peelArray
  , arrayOf
  , arrayType
  , rowType
  , toStructural
  , toDecl
  , fromDecl
  , setAliases
  , addAliases
  , setUniqueness
  , unifyUniqueness

  , tupleArrayElemToType

  -- ** Removing and adding names
  --
  -- $names
  , addNames
  , removeNames

  -- * Queries on values
  , arrayString
  , primValueType
  , valueType

  -- * Operations on values
  , arrayValue
  , emptyArray

  -- * Type aliases

  -- | Values of these types are produces by the parser.  They use
  -- unadorned names and have no type information, apart from that
  -- which is syntactically required.
  , NoInfo(..)
  , UncheckedType
  , UncheckedArrayType
  , UncheckedIdent
  , UncheckedExp
  , UncheckedLambda
  , UncheckedPattern
  , UncheckedFunDec
  , UncheckedProg
  , UncheckedProgWithHeaders
  )
  where

import Control.Monad.Writer
import Data.Array
import Data.Hashable
import Data.List
import qualified Data.HashSet as HS
import qualified Data.HashMap.Lazy as HM

import Prelude

import Language.Futhark.Syntax

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
arrayShape (Array (TupleArray _ ds _))   = ds
arrayShape _                             = mempty

-- | Return the dimensions of a type with (possibly) known dimensions.
arrayDims :: Ord vn => TypeBase ShapeDecl as vn -> [DimDecl vn]
arrayDims = shapeDims . arrayShape

-- | Return any shape declaration in the type, with duplicates removed.
nestedDims :: Ord vn => TypeBase ShapeDecl as vn -> [DimDecl vn]
nestedDims t =
  case t of Array a -> nub $ arrayNestedDims a
            Tuple ts -> nub $ mconcat $ map nestedDims ts
            Prim{} -> mempty
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

-- | Set the dimensions of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayShape :: ArrayShape (shape vn) =>
                 shape vn -> TypeBase shape as vn -> TypeBase shape as vn
setArrayShape ds (Array (PrimArray et _ u as)) = Array $ PrimArray et ds u as
setArrayShape ds (Array (TupleArray et _ u))    = Array $ TupleArray et ds u
setArrayShape _  (Tuple ts)                     = Tuple ts
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
  _ (PrimArrayElem bt as) = PrimArrayElem bt as
modifyShapeAnnotationsFromTupleArrayElem
  f (ArrayArrayElem at) = ArrayArrayElem $ modifyShapeAnnotationsFromArray f at
modifyShapeAnnotationsFromTupleArrayElem
  f (TupleArrayElem ts) = TupleArrayElem $ map (modifyShapeAnnotationsFromTupleArrayElem f) ts

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _ = True

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal to
-- @y@), meaning @x@ is valid whenever @y@ is.
subtypeOf :: (ArrayShape (shape vn), Monoid (as1 vn), Monoid (as2 vn)) =>
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
similarTo :: (ArrayShape (shape vn), Monoid (as vn)) =>
             TypeBase shape as vn
          -> TypeBase shape as vn
          -> Bool
similarTo t1 t2 = t1 `subtypeOf` t2 || t2 `subtypeOf` t1

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape as vn -> Uniqueness
uniqueness (Array (PrimArray _ _ u _)) = u
uniqueness (Array (TupleArray _ _ u))   = u
uniqueness _                            = Nonunique

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase shape as vn -> Bool
unique = (==Unique) . uniqueness

-- | Return the set of all variables mentioned in the aliasing of a
-- type.
aliases :: Monoid (as vn) => TypeBase shape as vn -> as vn
aliases (Array (PrimArray _ _ _ als)) = als
aliases (Array (TupleArray ts _ _)) = mconcat $ map tupleArrayElemAliases ts
aliases (Tuple et) = mconcat $ map aliases et
aliases (Prim _) = mempty

tupleArrayElemAliases :: Monoid (as vn) =>
                         TupleArrayElemTypeBase shape as vn -> as vn
tupleArrayElemAliases (PrimArrayElem _ als) = als
tupleArrayElemAliases (ArrayArrayElem (PrimArray _ _ _ als)) =
  als
tupleArrayElemAliases (ArrayArrayElem (TupleArray ts _ _)) =
  mconcat $ map tupleArrayElemAliases ts
tupleArrayElemAliases (TupleArrayElem ts) =
  mconcat $ map tupleArrayElemAliases ts

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ might consume its argument.
diet :: TypeBase shape as vn -> Diet
diet (Tuple ets) = TupleDiet $ map diet ets
diet (Prim _) = Observe
diet (Array (PrimArray _ _ Unique _)) = Consume
diet (Array (PrimArray _ _ Nonunique _)) = Observe
diet (Array (TupleArray _ _ Unique)) = Consume
diet (Array (TupleArray _ _ Nonunique)) = Observe

-- | @t `dietingAs` d@ modifies the uniqueness attributes of @t@ to
-- reflect how it is consumed according to @d@ - if it is consumed, it
-- becomes 'Unique'.  Tuples are handled intelligently.
dietingAs :: TypeBase shape as vn -> Diet -> TypeBase shape as vn
Tuple ets `dietingAs` TupleDiet ds =
  Tuple $ zipWith dietingAs ets ds
t `dietingAs` Consume =
  t `setUniqueness` Unique
t `dietingAs` _ =
  t `setUniqueness` Nonunique

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
toDecl :: TypeBase shape as vn
       -> TypeBase shape NoInfo vn
toDecl t = t `setAliases` NoInfo

-- | Replace no aliasing with an empty alias set.
fromDecl :: TypeBase shape as vn
         -> TypeBase shape Names vn
fromDecl t = t `setAliases` HS.empty

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
  where asType (PrimArrayElem bt _) = Prim bt
        asType (ArrayArrayElem at)   = Array at
        asType (TupleArrayElem ts')  = Tuple $ map asType ts'
peelArray n (Array (PrimArray et shape u als)) = do
  shape' <- stripDims n shape
  return $ Array $ PrimArray et shape' u als
peelArray n (Array (TupleArray et shape u)) = do
  shape' <- stripDims n shape
  return $ Array $ TupleArray et shape' u
peelArray _ _ = Nothing

-- | Return the immediate row-type of an array.  For @[[int]]@, this
-- would be @[int]@.
rowType :: (ArrayShape (shape vn), Monoid (as vn)) =>
           TypeBase shape as vn -> TypeBase shape as vn
rowType = stripArray 1

-- | A type is a primitive type if it is not an array and any component
-- types are prim types.
primType :: TypeBase shape as vn -> Bool
primType (Tuple ts) = all primType ts
primType (Prim _) = True
primType (Array _) = False

-- | Is the given type either unique (as per 'unique') or prim (as
-- per 'primType')?
uniqueOrPrim :: TypeBase shape as vn -> Bool
uniqueOrPrim x = primType x || unique x

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
removeNames (Prim et) = Prim et
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
removeTupleArrayElemNames (PrimArrayElem bt _) =
  PrimArrayElem bt NoInfo
removeTupleArrayElemNames (ArrayArrayElem et) =
  ArrayArrayElem $ removeArrayNames et
removeTupleArrayElemNames (TupleArrayElem ts) =
  TupleArrayElem $ map removeTupleArrayElemNames ts

-- | Add names to a type.
addNames :: TypeBase Rank NoInfo ()
         -> TypeBase Rank NoInfo vn
addNames (Prim et) = Prim et
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
addTupleArrayElemNames (PrimArrayElem bt _) =
  PrimArrayElem bt NoInfo
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
  Array $ TupleArray (map typeToTupleArrayElem ts) shape u

typeToTupleArrayElem :: Monoid (as vn) =>
                        TypeBase shape as vn
                     -> TupleArrayElemTypeBase shape as vn
typeToTupleArrayElem (Prim bt)  = PrimArrayElem bt mempty
typeToTupleArrayElem (Tuple ts') = TupleArrayElem $ map typeToTupleArrayElem ts'
typeToTupleArrayElem (Array at)  = ArrayArrayElem at

tupleArrayElemToType :: Monoid (as vn) =>
                        TupleArrayElemTypeBase shape as vn
                     -> TypeBase shape as vn
tupleArrayElemToType (PrimArrayElem bt _) = Prim bt
tupleArrayElemToType (TupleArrayElem ts)   = Tuple $ map tupleArrayElemToType ts
tupleArrayElemToType (ArrayArrayElem at)   = Array at

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
setTupleArrayElemUniqueness (PrimArrayElem bt als) _ =
  PrimArrayElem bt als
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
addTupleArrayElemAliases (PrimArrayElem bt als) f =
  PrimArrayElem bt $ f als
addTupleArrayElemAliases (ArrayArrayElem at) f =
  ArrayArrayElem $ addArrayAliases at f
addTupleArrayElemAliases (TupleArrayElem ts) f =
  TupleArrayElem $ map (`addTupleArrayElemAliases` f) ts

-- | Unify the uniqueness attributes and aliasing information of two
-- types.  The two types must otherwise be identical.  The resulting
-- alias set will be the 'mappend' of the two input types aliasing sets,
-- and the uniqueness will be 'Unique' only if both of the input types
-- are unique.
unifyUniqueness :: Monoid (as vn) =>
                   TypeBase shape as vn
                -> TypeBase shape as vn
                -> TypeBase shape as vn
unifyUniqueness (Array (PrimArray et dims u1 als1)) (Array (PrimArray _ _ u2 als2)) =
  Array $ PrimArray et dims (u1 <> u2) (als1 <> als2)
unifyUniqueness (Array (TupleArray et dims u1)) (Array (TupleArray _ _ u2)) =
  Array $ TupleArray et dims $ u1 <> u2
unifyUniqueness (Tuple ets1) (Tuple ets2) =
  Tuple $ zipWith unifyUniqueness ets1 ets2
unifyUniqueness t1 _ = t1

intValueType :: IntValue -> IntType
intValueType Int8Value{} = Int8
intValueType Int16Value{} = Int16
intValueType Int32Value{} = Int32
intValueType Int64Value{} = Int64

floatValueType :: FloatValue -> FloatType
floatValueType Float32Value{} = Float32
floatValueType Float64Value{} = Float64

-- | The type of a basic value.
primValueType :: PrimValue -> PrimType
primValueType (SignedValue v) = Signed $ intValueType v
primValueType (UnsignedValue v) = Unsigned $ intValueType v
primValueType (FloatValue v) = FloatType $ floatValueType v
primValueType BoolValue{} = Bool
primValueType CharValue{} = Char

-- | The type of an Futhark value.
valueType :: Value -> TypeBase Rank NoInfo vn
valueType (PrimValue bv) = Prim $ primValueType bv
valueType (TupValue vs) = Tuple (map valueType vs)
valueType (ArrayValue _ (Prim et)) =
  Array $ PrimArray et (Rank 1) Nonunique NoInfo
valueType (ArrayValue _ (Tuple ts)) =
  addNames $ Array $ TupleArray (map typeToTupleArrayElem ts) (Rank 1) Nonunique
valueType (ArrayValue _ (Array (PrimArray et shape _ _))) =
  Array $ PrimArray et (Rank $ 1 + shapeRank shape) Nonunique NoInfo
valueType (ArrayValue _ (Array (TupleArray et shape _))) =
  addNames $ Array $ TupleArray et (Rank $ 1 + shapeRank shape) Nonunique

-- | Construct an array value containing the given elements.
arrayValue :: ArrayShape (shape vn) =>
            [Value] -> TypeBase shape as vn -> Value
arrayValue vs = ArrayValue (listArray (0, length vs-1) vs) . removeNames . toDecl

-- | An empty array with the given row type.
emptyArray :: ArrayShape (shape vn) =>
              TypeBase shape as vn -> Value
emptyArray = arrayValue []

-- | If the given value is a nonempty array containing only
-- characters, return the corresponding 'String', otherwise return
-- 'Nothing'.
arrayString :: Value -> Maybe String
arrayString (ArrayValue arr (Prim Char))
  | c:cs <- elems arr = mapM asChar $ c:cs
  where asChar (PrimValue (CharValue c)) = Just c
        asChar _                       = Nothing
arrayString _ = Nothing

-- | The type of an Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: (Ord vn, Hashable vn) => ExpBase CompTypeBase vn -> CompTypeBase vn
typeOf (Literal val _) = fromDecl $ valueType val
typeOf (TupLit es _) = Tuple $ map typeOf es
typeOf (ArrayLit es t _) =
  arrayType 1 t $ mconcat $ map (uniqueness . typeOf) es
typeOf (BinOp _ _ _ t _) = t
typeOf (UnOp Not _ _) = Prim Bool
typeOf (UnOp Negate e _) = typeOf e
typeOf (UnOp Complement e _) = typeOf e
typeOf (UnOp Abs e _) = typeOf e
typeOf (UnOp Signum e _) = typeOf e
typeOf (UnOp (ToFloat t) _ _) = Prim $ FloatType t
typeOf (UnOp (ToSigned t) _ _) = Prim $ Signed t
typeOf (UnOp (ToUnsigned t) _ _) = Prim $ Unsigned t
typeOf (If _ _ _ t _) = t
typeOf (Var ident) =
  case identType ident of
    Tuple ets -> Tuple ets
    t         -> t `addAliases` HS.insert (identName ident)
typeOf (Apply _ _ t _) = t
typeOf (LetPat _ _ body _) = typeOf body
typeOf (LetWith _ _ _ _ body _) = typeOf body
typeOf (Index ident idx _) =
  stripArray (length idx) (typeOf ident)
typeOf (Iota _ _) = Array $ PrimArray (Signed Int32) (Rank 1) Unique mempty
typeOf Size{} = Prim $ Signed Int32
typeOf (Replicate _ e _) = arrayType 1 (typeOf e) Unique
typeOf (Reshape shape  e _) =
  Rank (length shape) `setArrayShape` typeOf e
typeOf (Rearrange _ e _) = typeOf e
typeOf (Transpose e _) = typeOf e
typeOf (Map f arr _) = arrayType 1 et Unique
                       `setAliases` HS.empty
                       `setUniqueness` Unique
  where et = lambdaType f [rowType $ typeOf arr]
typeOf (Reduce _ fun start arr _) =
  removeShapeAnnotations $
  lambdaType fun [typeOf start, rowType (typeOf arr)]
typeOf (Zip es _) = arrayType 1 (Tuple $ map (rowType . snd) es) Nonunique
typeOf (Unzip _ ts _) =
  Tuple ts
typeOf (Unsafe e _) =
  typeOf e
typeOf (Scan fun start arr _) =
  arrayType 1 et Unique
    where et = lambdaType fun [typeOf start, rowType $ typeOf arr]
typeOf (Filter _ arr _) =
  typeOf arr
typeOf (Partition funs arr _) =
  Tuple $ replicate (length funs + 1) $ typeOf arr
typeOf (Stream form lam arr _) =
  case form of
    MapLike{}       -> lambdaType lam [Prim $ Signed Int32, typeOf arr]
                       `setAliases` HS.empty
                       `setUniqueness` Unique
    RedLike _ _ _ acc -> lambdaType lam [Prim $ Signed Int32, typeOf acc, typeOf arr]
                         `setAliases` HS.empty
                         `setUniqueness` Unique
    Sequential  acc -> lambdaType lam [Prim $ Signed Int32, typeOf acc, typeOf arr]
                       `setAliases` HS.empty
                       `setUniqueness` Unique
typeOf (Concat x _ _) =
  typeOf x `setUniqueness` Unique `setAliases` HS.empty
typeOf (Split splitexps e _) =
  Tuple $ replicate (1 + length splitexps) (typeOf e)
typeOf (Copy e _) = typeOf e `setUniqueness` Unique `setAliases` HS.empty
typeOf (DoLoop _ _ _ _ body _) = typeOf body

-- | If possible, convert an expression to a value.  This is not a
-- true constant propagator, but a quick way to convert array/tuple
-- literal expressions into literal values instead.
expToValue :: ArrayShape (shape vn) =>
              ExpBase (TypeBase shape as) vn -> Maybe Value
expToValue (Literal val _) = Just val
expToValue (TupLit es _) = do es' <- mapM expToValue es
                              Just $ TupValue es'
expToValue (ArrayLit es t _) = do es' <- mapM expToValue es
                                  Just $ arrayValue es' t
expToValue _ = Nothing

-- | The result of applying the arguments of the given types to the
-- given lambda function.
lambdaType :: (Ord vn, Hashable vn) =>
              LambdaBase CompTypeBase vn -> [CompTypeBase vn]
           -> TypeBase Rank Names vn
lambdaType lam = returnType (lambdaReturnType lam) (lambdaParamDiets lam)

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
tupleArrayElemReturnType (PrimArrayElem bt NoInfo) ds args =
  PrimArrayElem bt als
  where als = mconcat $ map aliases $ zipWith maskAliases args ds
tupleArrayElemReturnType (ArrayArrayElem at) ds args =
  ArrayArrayElem $ arrayReturnType at ds args
tupleArrayElemReturnType (TupleArrayElem ts) ds args =
  TupleArrayElem $ map (\t -> tupleArrayElemReturnType t ds args) ts

-- | The specified return type of a lambda.
lambdaReturnType :: Ord vn =>
                    LambdaBase CompTypeBase vn -> TypeBase Rank NoInfo vn
lambdaReturnType (AnonymFun _ _ t _) = removeShapeAnnotations t
lambdaReturnType (CurryFun _ _ t _) = toDecl t
lambdaReturnType (UnOpFun _ _ t _) = toDecl t
lambdaReturnType (BinOpFun _ _ _ t _) = toDecl t
lambdaReturnType (CurryBinOpLeft _ _ _ t _) = toDecl t
lambdaReturnType (CurryBinOpRight _ _ _ t _) = toDecl t

-- | The parameter 'Diet's of a lambda.
lambdaParamDiets :: LambdaBase ty vn -> [Diet]
lambdaParamDiets (AnonymFun params _ _ _) = map (diet . identType) params
lambdaParamDiets (CurryFun _ args _ _) = map (const Observe) args
lambdaParamDiets UnOpFun{} = [Observe]
lambdaParamDiets BinOpFun{} = [Observe, Observe]
lambdaParamDiets CurryBinOpLeft{} = [Observe]
lambdaParamDiets CurryBinOpRight{} = [Observe]

-- | Find the function of the given name in the Futhark program.
funDecByName :: Name -> ProgBase ty vn -> Maybe (FunDecBase ty vn)
funDecByName fname = find (\(fname',_,_,_,_) -> fname == fname') . progFunctions

-- | Is the given binary operator commutative?
commutative :: BinOp -> Bool
commutative = flip elem [Plus, Pow, Times, Band, Xor, Bor, LogAnd, LogOr, Equal]

-- | Remove alias information from the type of an ident.
toParam :: ArrayShape (shape vn) =>
           IdentBase (TypeBase shape as) vn
        -> IdentBase (TypeBase ShapeDecl NoInfo) vn
toParam (Ident name t loc) = Ident name (vacuousShapeAnnotations $ toDecl t) loc

-- | Add (vacuous) alias information and remove shape annotations from
-- the type of an identifier.
fromParam :: ArrayShape (shape vn) =>
             IdentBase (TypeBase shape NoInfo) vn
          -> IdentBase (TypeBase Rank Names) vn
fromParam (Ident name t loc) =
  Ident name (removeShapeAnnotations $ fromDecl t) loc

-- | The list of names bound in the given pattern.
patNames :: (Eq vn, Hashable vn) => PatternBase ty vn -> [vn]
patNames = map identName . patIdents

-- | As 'patNames', but returns a the set of names (which means that
-- information about ordering is destroyed - make sure this is what
-- you want).
patNameSet :: (Eq vn, Hashable vn) => PatternBase ty vn -> HS.HashSet vn
patNameSet = HS.map identName . patIdentSet

-- | The list of idents bound in the given pattern.  The order of
-- idents is given by the pre-order traversal of the pattern.
patIdents :: (Eq vn, Hashable vn) => PatternBase ty vn -> [IdentBase ty vn]
patIdents (Id ident)     = [ident]
patIdents (TuplePattern pats _) = mconcat $ map patIdents pats
patIdents (Wildcard _ _) = []

-- | As 'patIdents', but returns a the set of names (which means that
-- information about ordering is destroyed - make sure this is what
-- you want).
patIdentSet :: (Eq vn, Hashable vn) => PatternBase ty vn -> HS.HashSet (IdentBase ty vn)
patIdentSet = HS.fromList . patIdents

-- | @isBuiltInFunction k@ is 'True' if @k@ is an element of 'builtInFunctions'.
isBuiltInFunction :: Name -> Bool
isBuiltInFunction fnm = fnm `HM.member` builtInFunctions

-- | A map of all built-in functions and their types.
builtInFunctions :: HM.HashMap Name (PrimType,[PrimType])
builtInFunctions = HM.fromList $ map namify
                   [("sqrt32", (FloatType Float32, [FloatType Float32]))
                   ,("log32", (FloatType Float32, [FloatType Float32]))
                   ,("exp32", (FloatType Float32, [FloatType Float32]))
                   ,("cos32", (FloatType Float32, [FloatType Float32]))
                   ,("sin32", (FloatType Float32, [FloatType Float32]))
                   ,("atan2_32", (FloatType Float32, [FloatType Float32, FloatType Float32]))
                   ,("isinf32", (Bool, [FloatType Float32]))
                   ,("isnan32", (Bool, [FloatType Float32]))

                   ,("sqrt64", (FloatType Float64, [FloatType Float64]))
                   ,("log64", (FloatType Float64, [FloatType Float64]))
                   ,("exp64", (FloatType Float64, [FloatType Float64]))
                   ,("cos64", (FloatType Float64, [FloatType Float64]))
                   ,("sin64", (FloatType Float64, [FloatType Float64]))
                   ,("atan2_64", (FloatType Float64, [FloatType Float64, FloatType Float64]))
                   ,("isinf64", (Bool, [FloatType Float64]))
                   ,("isnan64", (Bool, [FloatType Float64]))
                   ]
  where namify (k,v) = (nameFromString k, v)

-- | A type with no aliasing information but shape annotations.
type UncheckedType = TypeBase ShapeDecl NoInfo Name

-- | An array type with no aliasing information.
type UncheckedArrayType = ArrayTypeBase ShapeDecl NoInfo Name

-- | An identifier with no type annotations.
type UncheckedIdent = IdentBase NoInfo Name

-- | An expression with no type annotations.
type UncheckedExp = ExpBase NoInfo Name

-- | A lambda with no type annotations.
type UncheckedLambda = LambdaBase NoInfo Name

-- | A pattern with no type annotations.
type UncheckedPattern = PatternBase NoInfo Name

-- | A function declaration with no type annotations.
type UncheckedFunDec = FunDecBase NoInfo Name

-- | An Futhark program with no type annotations.
type UncheckedProg = ProgBase NoInfo Name

-- | An Futhark program with no type annotations, but with headers.
type UncheckedProgWithHeaders = ProgBaseWithHeaders NoInfo Name
