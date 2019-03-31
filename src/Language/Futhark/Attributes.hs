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
  , decImports
  , progModuleTypes
  , identifierReference
  , identifierReferences

  -- * Queries on expressions
  , typeOf

  -- * Queries on patterns and params
  , patIdentSet
  , patternType
  , patternStructType
  , patternPatternType
  , patternParam
  , patternNoShapeAnnotations
  , patternOrderZero
  , patternDimNames

  -- * Queries on types
  , uniqueness
  , unique
  , aliases
  , diet
  , arrayRank
  , nestedDims
  , returnType
  , concreteType
  , orderZero
  , unfoldFunType
  , foldFunType
  , typeVars
  , typeDimNames

  -- * Operations on types
  , rank
  , peelArray
  , stripArray
  , arrayOf
  , arrayOfWithAliases
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
  , tupleFieldNames
  , sortFields
  , isTypeParam

  -- | Values of these types are produces by the parser.  They use
  -- unadorned names and have no type information, apart from that
  -- which is syntactically required.
  , NoInfo(..)
  , UncheckedType
  , UncheckedTypeExp
  , UncheckedArrayElemType
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
  , UncheckedCase
  )
  where

import           Control.Monad.Writer
import           Data.Char
import           Data.Foldable
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
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
arrayRank :: TypeBase dim as -> Int
arrayRank = shapeRank . arrayShape

-- | Return the shape of a type - for non-arrays, this is 'mempty'.
arrayShape :: TypeBase dim as -> ShapeDecl dim
arrayShape (Array _ _ _ ds) = ds
arrayShape _ = mempty

-- | Return any shape declarations in the type, with duplicates
-- removed.
nestedDims :: TypeBase (DimDecl VName) as -> [DimDecl VName]
nestedDims t =
  case t of Array _ _ a ds      -> nub $ arrayNestedDims a <> shapeDims ds
            Record fs           -> nub $ fold $ fmap nestedDims fs
            Prim{}              -> mempty
            TypeVar _ _ _ targs -> concatMap typeArgDims targs
            Arrow _ v t1 t2     -> filter (notV v) $ nestedDims t1 <> nestedDims t2
            Enum{}              -> []
  where arrayNestedDims ArrayPrimElem{} =
          mempty
        arrayNestedDims (ArrayPolyElem _ targs) =
          concatMap typeArgDims targs
        arrayNestedDims (ArrayRecordElem ts) =
          fold (fmap recordArrayElemNestedDims ts)
        arrayNestedDims ArrayEnumElem{} = mempty

        recordArrayElemNestedDims (RecordArrayArrayElem a ds) =
          arrayNestedDims a <> shapeDims ds
        recordArrayElemNestedDims (RecordArrayElem et) =
          arrayNestedDims et

        typeArgDims (TypeArgDim d _) = [d]
        typeArgDims (TypeArgType at _) = nestedDims at

        notV Nothing  = const True
        notV (Just v) = (/=NamedDim (qualName v))

-- | Set the dimensions of an array.  If the given type is not an
-- array, return the type unchanged.
setArrayShape :: TypeBase dim as -> ShapeDecl dim -> TypeBase dim as
setArrayShape (Array a u t _) ds = Array a u t ds
setArrayShape t _ = t

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

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape as -> Uniqueness
uniqueness (Array _ u _ _) = u
uniqueness (TypeVar _ u _ _) = u
uniqueness _ = Nonunique

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
diet (Record ets)            = RecordDiet $ fmap diet ets
diet (Prim _)                = Observe
diet TypeVar{}               = Observe
diet (Arrow _ _ t1 t2)       = FuncDiet (diet t1) (diet t2)
diet (Array _ Unique _ _)    = Consume
diet (Array _ Nonunique _ _) = Observe
diet (Enum _)                = Observe

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
maskAliases t FuncDiet{} = t
maskAliases _ _ = error "Invalid arguments passed to maskAliases."

-- | Convert any type to one that has rank information, no alias
-- information, and no embedded names.
toStructural :: TypeBase dim as
             -> TypeBase () ()
toStructural = removeNames . removeShapeAnnotations

-- | Remove aliasing information from a type.
toStruct :: TypeBase dim as
         -> TypeBase dim ()
toStruct t = t `setAliases` ()

-- | Replace no aliasing with an empty alias set.
fromStruct :: TypeBase dim as
           -> TypeBase dim Aliasing
fromStruct t = t `setAliases` S.empty

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: Int -> TypeBase dim as -> Maybe (TypeBase dim as)
peelArray 0 t = Just t
peelArray n (Array _ _ (ArrayPrimElem et) shape)
  | shapeRank shape == n =
    Just $ Prim et
peelArray n (Array als u (ArrayPolyElem et targs) shape)
  | shapeRank shape == n =
    Just $ TypeVar als u et targs
peelArray n (Array als u (ArrayRecordElem ts) shape)
  | shapeRank shape == n =
    Just $ Record $ fmap asType ts
  where asType (RecordArrayElem (ArrayPrimElem bt)) = Prim bt
        asType (RecordArrayElem (ArrayPolyElem bt targs)) = TypeVar als u bt targs
        asType (RecordArrayElem (ArrayRecordElem ts')) = Record $ fmap asType ts'
        asType (RecordArrayElem (ArrayEnumElem cs)) = Enum cs
        asType (RecordArrayArrayElem et e_shape) = Array als u et e_shape
peelArray n (Array _ _ (ArrayEnumElem cs) shape)
  | shapeRank shape == n =
    Just $ Enum cs
peelArray n (Array als u et shape) = do
  shape' <- stripDims n shape
  return $ Array als u et shape'
peelArray _ _ = Nothing

-- | Remove names from a type - this involves removing all size
-- annotations from arrays, as well as all aliasing.
removeNames :: TypeBase dim as
            -> TypeBase () ()
removeNames = flip setAliases () . removeShapeAnnotations

-- | @arrayOf t s u@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.  The function returns 'Nothing' in case an
-- attempt is made to create an array of functions.
arrayOf :: Monoid as =>
           TypeBase dim as
        -> ShapeDecl dim
        -> Uniqueness
        -> Maybe (TypeBase dim as)
arrayOf t = arrayOfWithAliases t mempty

arrayOfWithAliases :: Monoid as =>
                      TypeBase dim as
                   -> as
                   -> ShapeDecl dim
                   -> Uniqueness
                   -> Maybe (TypeBase dim as)
arrayOfWithAliases (Array as1 _ et shape1) as2 shape2 u =
  Just $ Array (as1<>as2) u et (shape2 <> shape1)
arrayOfWithAliases (Prim et) as shape u =
  Just $ Array as u (ArrayPrimElem et) shape
arrayOfWithAliases (TypeVar _ _ x targs) as shape u =
  Just $ Array as u (ArrayPolyElem x targs) shape
arrayOfWithAliases (Record ts) as shape u = do
  ts' <- traverse typeToRecordArrayElem ts
  return $ Array as u (ArrayRecordElem ts') shape
arrayOfWithAliases Arrow{} _ _ _ = Nothing
arrayOfWithAliases (Enum cs) as shape u  =
  Just $ Array as u (ArrayEnumElem cs) shape

typeToRecordArrayElem :: Monoid as =>
                         TypeBase dim as -> Maybe (RecordArrayElemTypeBase dim)
typeToRecordArrayElem (Prim bt) =
  Just $ RecordArrayElem $ ArrayPrimElem bt
typeToRecordArrayElem (TypeVar _ _ bt targs) =
  Just $ RecordArrayElem $ ArrayPolyElem bt targs
typeToRecordArrayElem (Record ts') =
  RecordArrayElem . ArrayRecordElem <$>
  traverse typeToRecordArrayElem ts'
typeToRecordArrayElem (Array _ _ et shape) =
  Just $ RecordArrayArrayElem et shape
typeToRecordArrayElem Arrow{} = Nothing
typeToRecordArrayElem (Enum cs) =
  Just $ RecordArrayElem $ ArrayEnumElem cs

recordArrayElemToType :: Monoid as =>
                         RecordArrayElemTypeBase dim
                      -> TypeBase dim as
recordArrayElemToType (RecordArrayElem et)              = arrayElemToType et
recordArrayElemToType (RecordArrayArrayElem et shape) = Array mempty Nonunique et shape

arrayElemToType :: Monoid as => ArrayElemTypeBase dim -> TypeBase dim as
arrayElemToType (ArrayPolyElem bt targs) =
  TypeVar mempty Nonunique bt targs
arrayElemToType (ArrayRecordElem ts) =
  Record $ fmap recordArrayElemToType ts
arrayElemToType (ArrayPrimElem bt) = Prim bt
arrayElemToType (ArrayEnumElem cs) = Enum cs

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: Monoid as => Int -> TypeBase dim as -> TypeBase dim as
stripArray n (Array als u et shape)
  | Just shape' <- stripDims n shape =
    Array als u et shape'
  | otherwise = arrayElemToType et `setUniqueness` u `addAliases` (<>als)
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
sortFields l = map snd $ sortOn fst $ zip (map (fieldish . fst) l') l'
  where l' = M.toList l
        fieldish s = case reads $ nameToString s of
          [(x, "")] -> Left (x::Int)
          _         -> Right s

isTypeParam :: TypeParamBase vn -> Bool
isTypeParam TypeParamType{}       = True
isTypeParam TypeParamDim{}        = False


-- | Set the uniqueness attribute of a type.  If the type is a tuple,
-- the uniqueness of its components will be modified.
setUniqueness :: TypeBase dim as -> Uniqueness -> TypeBase dim as
setUniqueness (Array als _ et shape) u =
  Array als u et shape
setUniqueness (TypeVar als _ t targs) u =
  TypeVar als u t targs
setUniqueness (Record ets) u =
  Record $ fmap (`setUniqueness` u) ets
setUniqueness t _ = t

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliasing.
setAliases :: TypeBase dim asf -> ast -> TypeBase dim ast
setAliases t = addAliases t . const

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliasing replaced by @f@ applied to that aliasing.
addAliases :: TypeBase dim asf -> (asf -> ast)
           -> TypeBase dim ast
addAliases t f = bimap id f t

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
valueType (ArrayValue _ t) = t

-- | Construct a 'ShapeDecl' with the given number of zero-information
-- dimensions.
rank :: Int -> ShapeDecl ()
rank n = ShapeDecl $ replicate n ()

unscopeAliases :: S.Set VName -> CompType -> CompType
unscopeAliases bound_here t = t `addAliases` S.map unbind
  where unbind (AliasBound v) | v `S.member` bound_here = AliasFree v
        unbind a = a

-- | The type of an Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: ExpBase Info VName -> CompType
typeOf (Literal val _) = Prim $ primValueType val
typeOf (IntLit _ (Info t) _) = fromStruct t
typeOf (FloatLit _ (Info t) _) = fromStruct t
typeOf (Parens e _) = typeOf e
typeOf (QualParens _ e _) = typeOf e
typeOf (TupLit es _) = tupleRecord $ map typeOf es
typeOf (RecordLit fs _) =
  -- Reverse, because M.unions is biased to the left.
  Record $ M.unions $ reverse $ map record fs
  where record (RecordFieldExplicit name e _) = M.singleton name $ typeOf e
        record (RecordFieldImplicit name (Info t) _) =
          M.singleton (baseName name) $ removeShapeAnnotations t
          `addAliases` S.insert (AliasBound name)
typeOf (ArrayLit _ (Info t) _) = removeShapeAnnotations t
typeOf (Range _ _ _ (Info t) _) = removeShapeAnnotations t
typeOf (BinOp _ _ _ _ (Info t) _) = removeShapeAnnotations t
typeOf (Project _ _ (Info t) _) = removeShapeAnnotations t
typeOf (If _ _ _ (Info t) _) = removeShapeAnnotations t
typeOf (Var _ (Info t) _) = removeShapeAnnotations t
typeOf (Ascript e _ _) = typeOf e
typeOf (Apply _ _ _ (Info t) _) = removeShapeAnnotations t
typeOf (Negate e _) = typeOf e
typeOf (LetPat _ pat _ body _) =
  unscopeAliases (S.map identName $ patIdentSet pat) $ typeOf body
typeOf (LetFun _ _ body _) = typeOf body
typeOf (LetWith dest _ _ _ body _) =
  unscopeAliases (S.singleton $ identName dest) $ typeOf body
typeOf (Index _ _ (Info t) _) = removeShapeAnnotations t
typeOf (Update e _ _ _) = typeOf e `setAliases` mempty
typeOf (RecordUpdate _ _ _ (Info t) _) = removeShapeAnnotations t
typeOf (Unsafe e _) = typeOf e
typeOf (Assert _ e _ _) = typeOf e
typeOf (Map _ _ (Info t) _) = removeShapeAnnotations t `setUniqueness` Unique
typeOf (Reduce _ _ _ arr _) =
  stripArray 1 (typeOf arr) `setAliases` mempty
typeOf (GenReduce hist _ _ _ _ _) =
  typeOf hist `setAliases` mempty `setUniqueness` Unique
typeOf (Scan _ _ arr _) = typeOf arr `setAliases` mempty `setUniqueness` Unique
typeOf (Filter _ arr _) = typeOf arr `setAliases` mempty `setUniqueness` Unique
typeOf (Partition _ _ arr _) =
  tupleRecord [typeOf arr `setAliases` mempty `setUniqueness` Unique,
               Array mempty Unique (ArrayPrimElem (Signed Int32)) (rank 1)]
typeOf (Stream _ lam _ _) =
  rettype (typeOf lam) `setUniqueness` Unique
  where rettype (Arrow _ _ _ t) = rettype t
        rettype t = t
typeOf (DoLoop _ pat _ _ _ _) = patternType pat
typeOf (Lambda tparams params _ _ (Info (als, t)) _) =
  unscopeAliases bound_here $
  removeShapeAnnotations (foldr (uncurry (Arrow ()) . patternParam) t params)
  `setAliases` als
  where bound_here = S.fromList (map typeParamName tparams) <>
                     S.map identName (mconcat $ map patIdentSet params)
typeOf (OpSection _ (Info t) _) =
  removeShapeAnnotations t
typeOf (OpSectionLeft _ _ _ (_, Info pt2) (Info ret) _)  =
  removeShapeAnnotations $ foldFunType [fromStruct pt2] ret
typeOf (OpSectionRight _ _ _ (Info pt1, _) (Info ret) _) =
  removeShapeAnnotations $ foldFunType [fromStruct pt1] ret
typeOf (ProjectSection _ (Info t) _) = removeShapeAnnotations t
typeOf (IndexSection _ (Info t) _) = removeShapeAnnotations t
typeOf (VConstr0 _ (Info t) _)  = removeShapeAnnotations t
typeOf (Match _ _ (Info t) _) = removeShapeAnnotations t

foldFunType :: Monoid as => [TypeBase dim as] -> TypeBase dim as -> TypeBase dim as
foldFunType ps ret = foldr (Arrow mempty Nothing) ret ps

-- | Extract the parameter types and return type from a type.
-- If the type is not an arrow type, the list of parameter types is empty.
unfoldFunType :: TypeBase dim as -> ([TypeBase dim as], TypeBase dim as)
unfoldFunType (Arrow _ _ t1 t2) = let (ps, r) = unfoldFunType t2
                                  in (t1 : ps, r)
unfoldFunType t = ([], t)

-- | The type names mentioned in a type.
typeVars :: Monoid as => TypeBase dim as -> S.Set VName
typeVars t =
  case t of
    Prim{} -> mempty
    TypeVar _ _ tn targs ->
      mconcat $ typeVarFree tn : map typeArgFree targs
    Arrow _ _ t1 t2 -> typeVars t1 <> typeVars t2
    Record fields -> foldMap typeVars fields
    Array _ _ ArrayPrimElem{} _ -> mempty
    Array _ _ (ArrayPolyElem tn targs) _ ->
      mconcat $ typeVarFree tn : map typeArgFree targs
    Array _ _ (ArrayRecordElem fields) _ ->
      foldMap (typeVars . f) fields
      -- This local function is to avoid an ambiguous type.
      where f :: RecordArrayElemTypeBase dim -> TypeBase dim ()
            f = recordArrayElemToType
    Array _ _ ArrayEnumElem{} _ -> mempty
    Enum{} -> mempty
  where typeVarFree = S.singleton . typeLeaf
        typeArgFree (TypeArgType ta _) = typeVars ta
        typeArgFree TypeArgDim{} = mempty

-- | The result of applying the arguments of the given types to a
-- function with the given return type, consuming its parameters with
-- the given diets.
returnType :: TypeBase dim Aliasing
           -> Diet
           -> CompType
           -> TypeBase dim Aliasing
returnType (Array _ Unique et shape) _ _ =
  Array mempty Unique et shape
returnType (Array als Nonunique et shape) d arg =
  Array (als<>arg_als) Unique et shape -- Intentional!
  where arg_als = aliases $ maskAliases arg d
returnType (Record fs) d arg =
  Record $ fmap (\et -> returnType et d arg) fs
returnType (Prim t) _ _ = Prim t
returnType (TypeVar _ Unique t targs) _ _ =
  TypeVar mempty Unique t targs
returnType (TypeVar als Nonunique t targs) d arg =
  TypeVar (als<>arg_als) Unique t targs -- Intentional!
  where arg_als = aliases $ maskAliases arg d
returnType (Arrow _ v t1 t2) d arg =
  Arrow als v (bimap id (const mempty) t1) (t2 `setAliases` als)
  where als = aliases $ maskAliases arg d
returnType (Enum cs) _ _ = Enum cs

-- | Is the type concrete, i.e, without any type variables or function arrows?
concreteType :: TypeBase f vn -> Bool
concreteType Prim{} = True
concreteType TypeVar{} = False
concreteType Arrow{} = False
concreteType (Record ts) = all concreteType ts
concreteType Enum{} = True
concreteType (Array _ _ at _) = concreteArrayType at
  where concreteArrayType ArrayPrimElem{}      = True
        concreteArrayType ArrayPolyElem{}      = False
        concreteArrayType (ArrayRecordElem ts) = all concreteRecordArrayElem ts
        concreteArrayType ArrayEnumElem{}      = True

        concreteRecordArrayElem (RecordArrayElem et) = concreteArrayType et
        concreteRecordArrayElem (RecordArrayArrayElem et _) = concreteArrayType et

-- | @orderZero t@ is 'True' if the argument type has order 0, i.e., it is not
-- a function type, does not contain a function type as a subcomponent, and may
-- not be instantiated with a function type.
orderZero :: TypeBase dim as -> Bool
orderZero (Prim _)        = True
orderZero Array{}         = True
orderZero (Record fs)     = all orderZero $ M.elems fs
orderZero TypeVar{}       = True
orderZero Arrow{}         = False
orderZero Enum{}          = True

-- | Extract all the shape names that occur in a given pattern.
patternDimNames :: PatternBase Info VName -> S.Set VName
patternDimNames (TuplePattern ps _)    = foldMap patternDimNames ps
patternDimNames (RecordPattern fs _)   = foldMap (patternDimNames . snd) fs
patternDimNames (PatternParens p _)    = patternDimNames p
patternDimNames (Id _ (Info tp) _)     = typeDimNames tp
patternDimNames (Wildcard (Info tp) _) = typeDimNames tp
patternDimNames (PatternAscription p (TypeDecl _ (Info t)) _) =
  patternDimNames p <> typeDimNames t
patternDimNames (PatternLit _ (Info tp) _) = typeDimNames tp

-- | Extract all the shape names that occur in a given type.
typeDimNames :: TypeBase (DimDecl VName) als -> S.Set VName
typeDimNames = foldMap dimName . nestedDims
  where dimName :: DimDecl VName -> S.Set VName
        dimName (NamedDim qn) = S.singleton $ qualLeaf qn
        dimName _             = mempty

-- | @patternOrderZero pat@ is 'True' if all of the types in the given pattern
-- have order 0.
patternOrderZero :: PatternBase Info vn -> Bool
patternOrderZero pat = case pat of
  TuplePattern ps _       -> all patternOrderZero ps
  RecordPattern fs _      -> all (patternOrderZero . snd) fs
  PatternParens p _       -> patternOrderZero p
  Id _ (Info t) _         -> orderZero t
  Wildcard (Info t) _     -> orderZero t
  PatternAscription p _ _ -> patternOrderZero p
  PatternLit _ (Info t) _ -> orderZero t

-- | The set of identifiers bound in a pattern.
patIdentSet :: (Functor f, Ord vn) => PatternBase f vn -> S.Set (IdentBase f vn)
patIdentSet (Id v t loc)              = S.singleton $ Ident v t loc
patIdentSet (PatternParens p _)       = patIdentSet p
patIdentSet (TuplePattern pats _)     = mconcat $ map patIdentSet pats
patIdentSet (RecordPattern fs _)      = mconcat $ map (patIdentSet . snd) fs
patIdentSet Wildcard{}                = mempty
patIdentSet (PatternAscription p _ _) = patIdentSet p
patIdentSet PatternLit{}              = mempty

-- | The type of values bound by the pattern.
patternType :: PatternBase Info VName -> CompType
patternType (Wildcard (Info t) _)     = removeShapeAnnotations t
patternType (PatternParens p _)       = patternType p
patternType (Id _ (Info t) _)         = removeShapeAnnotations t
patternType (TuplePattern pats _)     = tupleRecord $ map patternType pats
patternType (RecordPattern fs _)      = Record $ patternType <$> M.fromList fs
patternType (PatternAscription p _ _) = patternType p
patternType (PatternLit _ (Info t) _) = removeShapeAnnotations t

-- | The type of a pattern, including shape annotations.
patternPatternType :: PatternBase Info VName -> PatternType
patternPatternType (Wildcard (Info t) _)      = t
patternPatternType (PatternParens p _)        = patternPatternType p
patternPatternType (Id _ (Info t) _)          = t
patternPatternType (TuplePattern pats _)      = tupleRecord $ map patternPatternType pats
patternPatternType (RecordPattern fs _)       = Record $ patternPatternType <$> M.fromList fs
patternPatternType (PatternAscription p _ _)  = patternPatternType p
patternPatternType (PatternLit _ (Info t) _)  = t

-- | The type matched by the pattern, including shape declarations if present.
patternStructType :: PatternBase Info VName -> StructType
patternStructType = toStruct . patternPatternType

-- | When viewed as a function parameter, does this pattern correspond
-- to a named parameter of some type?
patternParam :: PatternBase Info VName -> (Maybe VName, StructType)
patternParam (PatternParens p _) =
  patternParam p
patternParam (PatternAscription (Id v _ _) td _) =
  (Just v, unInfo $ expandedType td)
patternParam p =
  (Nothing, patternStructType p)

-- | Remove all shape annotations from a pattern, leaving them unnamed
-- instead.
patternNoShapeAnnotations :: PatternBase Info VName -> PatternBase Info VName
patternNoShapeAnnotations (PatternAscription p (TypeDecl te (Info t)) loc) =
  PatternAscription (patternNoShapeAnnotations p)
  (TypeDecl te $ Info $ vacuousShapeAnnotations t) loc
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
patternNoShapeAnnotations (PatternLit e (Info t) loc) =
  PatternLit e (Info (vacuousShapeAnnotations t)) loc

-- | Names of primitive types to types.  This is only valid if no
-- shadowing is going on, but useful for tools.
namesToPrimTypes :: M.Map Name PrimType
namesToPrimTypes = M.fromList
                   [ (nameFromString $ pretty t, t) |
                     t <- Bool :
                          map Signed [minBound..maxBound] ++
                          map Unsigned [minBound..maxBound] ++
                          map FloatType [minBound..maxBound] ]

-- | The nature of something predefined.  These can either be
-- monomorphic or overloaded.  An overloaded builtin is a list valid
-- types it can be instantiated with, to the parameter and result
-- type, with 'Nothing' representing the overloaded parameter type.
data Intrinsic = IntrinsicMonoFun [PrimType] PrimType
               | IntrinsicOverloadedFun [PrimType] [Maybe PrimType] (Maybe PrimType)
               | IntrinsicPolyFun [TypeParamBase VName] [TypeBase () ()] (TypeBase () ())
               | IntrinsicType PrimType
               | IntrinsicEquality -- Special cased.
               | IntrinsicOpaque

-- | A map of all built-ins.
intrinsics :: M.Map VName Intrinsic
intrinsics = M.fromList $ zipWith namify [10..] $

             map primFun (M.toList Primitive.primFuns) ++

             [ ("~", IntrinsicOverloadedFun
                     (map Signed [minBound..maxBound] ++
                      map Unsigned [minBound..maxBound])
                     [Nothing] Nothing)
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
             mapMaybe mkIntrinsicBinOp [minBound..maxBound] ++

             [("flatten", IntrinsicPolyFun [tp_a]
                          [Array () Nonunique (ArrayPolyElem tv_a' []) (rank 2)] $
                          Array () Nonunique (ArrayPolyElem tv_a' []) (rank 1)),
              ("unflatten", IntrinsicPolyFun [tp_a]
                            [Prim $ Signed Int32,
                             Prim $ Signed Int32,
                             Array () Nonunique (ArrayPolyElem tv_a' []) (rank 1)] $
                            Array () Nonunique (ArrayPolyElem tv_a' []) (rank 2)),

              ("concat", IntrinsicPolyFun [tp_a]
                         [arr_a, arr_a] uarr_a),
              ("rotate", IntrinsicPolyFun [tp_a]
                         [Prim $ Signed Int32, arr_a] arr_a),
              ("transpose", IntrinsicPolyFun [tp_a] [arr_a] arr_a),

              ("cmp_threshold", IntrinsicPolyFun []
                                [Prim $ Signed Int32,
                                 Array () Nonunique (ArrayPrimElem (Signed Int32)) (rank 1)] $
                                Prim Bool),

               ("scatter", IntrinsicPolyFun [tp_a]
                          [Array () Unique (ArrayPolyElem tv_a' []) (rank 1),
                           Array () Nonunique (ArrayPrimElem (Signed Int32)) (rank 1),
                           Array () Nonunique (ArrayPolyElem tv_a' []) (rank 1)] $
                          Array () Unique (ArrayPolyElem tv_a' []) (rank 1)),

              ("zip", IntrinsicPolyFun [tp_a, tp_b] [arr_a, arr_b] arr_a_b),
              ("unzip", IntrinsicPolyFun [tp_a, tp_b] [arr_a_b] t_arr_a_arr_b),

              ("gen_reduce", IntrinsicPolyFun [tp_a]
                             [uarr_a,
                              t_a `arr` (t_a `arr` t_a),
                              t_a,
                              Array () Nonunique (ArrayPrimElem (Signed Int32)) (rank 1),
                              arr_a]
                             uarr_a),

              ("map", IntrinsicPolyFun [tp_a, tp_b] [t_a `arr` t_b, arr_a] uarr_b),

              ("reduce", IntrinsicPolyFun [tp_a]
                         [t_a `arr` (t_a `arr` t_a), t_a, arr_a] t_a),

              ("reduce_comm", IntrinsicPolyFun [tp_a]
                              [t_a `arr` (t_a `arr` t_a), t_a, arr_a] t_a),

              ("scan", IntrinsicPolyFun [tp_a]
                       [t_a `arr` (t_a `arr` t_a), t_a, arr_a] uarr_a),

              ("partition",
               IntrinsicPolyFun [tp_a]
               [Prim (Signed Int32), t_a `arr` Prim (Signed Int32), arr_a] $
               tupleRecord [uarr_a, Array () Unique (ArrayPrimElem (Signed Int32)) (rank 1)]),

              ("stream_map",
               IntrinsicPolyFun [tp_a, tp_b] [arr_a `arr` arr_b, arr_a] uarr_b),

              ("stream_map_per",
               IntrinsicPolyFun [tp_a, tp_b] [arr_a `arr` arr_b, arr_a] uarr_b),

              ("stream_red",
               IntrinsicPolyFun [tp_a, tp_b] [t_b `arr` (t_b `arr` t_b), arr_a `arr` t_b, arr_a] t_b),

              ("stream_red_per",
               IntrinsicPolyFun [tp_a, tp_b] [t_b `arr` (t_b `arr` t_b), arr_a `arr` t_b, arr_a] t_b),


              ("trace", IntrinsicPolyFun [tp_a] [t_a] t_a),
              ("break", IntrinsicPolyFun [tp_a] [t_a] t_a)]

  where tv_a = VName (nameFromString "a") 0
        tv_a' = typeName tv_a
        t_a = TypeVar () Nonunique tv_a' []
        arr_a = Array () Nonunique (ArrayPolyElem tv_a' []) (rank 1)
        uarr_a = Array () Unique (ArrayPolyElem tv_a' []) (rank 1)
        tp_a = TypeParamType Unlifted tv_a noLoc

        tv_b = VName (nameFromString "b") 1
        tv_b' = typeName tv_b
        t_b = TypeVar () Nonunique tv_b' []
        arr_b = Array () Nonunique (ArrayPolyElem tv_b' []) (rank 1)
        uarr_b = Array () Unique (ArrayPolyElem tv_b' []) (rank 1)
        tp_b = TypeParamType Unlifted tv_b noLoc

        arr_a_b = Array () Nonunique
                  (ArrayRecordElem (M.fromList $ zip tupleFieldNames
                                     [RecordArrayElem $ ArrayPolyElem tv_a' [],
                                      RecordArrayElem $ ArrayPolyElem tv_b' []]))
                  (rank 1)
        t_arr_a_arr_b = Record $ M.fromList $ zip tupleFieldNames [arr_a, arr_b]

        arr = Arrow mempty Nothing

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

        mkIntrinsicBinOp :: BinOp -> Maybe (String, Intrinsic)
        mkIntrinsicBinOp op = do op' <- intrinsicBinOp op
                                 return (pretty op, op')

        binOp ts = Just $ IntrinsicOverloadedFun ts [Nothing, Nothing] Nothing
        ordering = Just $ IntrinsicOverloadedFun anyPrimType [Nothing, Nothing] (Just Bool)

        intrinsicBinOp Plus     = binOp anyNumberType
        intrinsicBinOp Minus    = binOp anyNumberType
        intrinsicBinOp Pow      = binOp anyNumberType
        intrinsicBinOp Times    = binOp anyNumberType
        intrinsicBinOp Divide   = binOp anyNumberType
        intrinsicBinOp Mod      = binOp anyNumberType
        intrinsicBinOp Quot     = binOp anyIntType
        intrinsicBinOp Rem      = binOp anyIntType
        intrinsicBinOp ShiftR   = binOp anyIntType
        intrinsicBinOp ShiftL   = binOp anyIntType
        intrinsicBinOp Band     = binOp anyIntType
        intrinsicBinOp Xor      = binOp anyIntType
        intrinsicBinOp Bor      = binOp anyIntType
        intrinsicBinOp LogAnd   = Just $ IntrinsicMonoFun [Bool,Bool] Bool
        intrinsicBinOp LogOr    = Just $ IntrinsicMonoFun [Bool,Bool] Bool
        intrinsicBinOp Equal    = Just IntrinsicEquality
        intrinsicBinOp NotEqual = Just IntrinsicEquality
        intrinsicBinOp Less     = ordering
        intrinsicBinOp Leq      = ordering
        intrinsicBinOp Greater  = ordering
        intrinsicBinOp Geq      = ordering
        intrinsicBinOp _        = Nothing

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

-- | The modules imported by a Futhark program.
progImports :: ProgBase f vn -> [(String,SrcLoc)]
progImports = concatMap decImports . progDecs

-- | The modules imported by a single declaration.
decImports :: DecBase f vn -> [(String,SrcLoc)]
decImports (OpenDec x _) = modExpImports x
decImports (ModDec md) = modExpImports $ modExp md
decImports SigDec{} = []
decImports TypeDec{} = []
decImports ValDec{} = []
decImports (LocalDec d _) = decImports d
decImports (ImportDec x _ loc) = [(x, loc)]

modExpImports :: ModExpBase f vn -> [(String,SrcLoc)]
modExpImports ModVar{}              = []
modExpImports (ModParens p _)       = modExpImports p
modExpImports (ModImport f _ loc)   = [(f,loc)]
modExpImports (ModDecs ds _)        = concatMap decImports ds
modExpImports (ModApply _ me _ _ _) = modExpImports me
modExpImports (ModAscript me _ _ _) = modExpImports me
modExpImports ModLambda{}           = []

-- | The set of module types used in any exported (non-local)
-- declaration.
progModuleTypes :: Ord vn => ProgBase f vn -> S.Set vn
progModuleTypes = mconcat . map onDec . progDecs
  where onDec (OpenDec x _) = onModExp x
        onDec (ModDec md) =
          maybe mempty (onSigExp . fst) (modSignature md) <> onModExp (modExp md)
        onDec SigDec{} = mempty
        onDec TypeDec{} = mempty
        onDec ValDec{} = mempty
        onDec LocalDec{} = mempty
        onDec ImportDec{} = mempty

        onModExp ModVar{} = mempty
        onModExp (ModParens p _) = onModExp p
        onModExp ModImport {} = mempty
        onModExp (ModDecs ds _) = mconcat $ map onDec ds
        onModExp (ModApply me1 me2 _ _ _) = onModExp me1 <> onModExp me2
        onModExp (ModAscript me se _ _) = onModExp me <> onSigExp se
        onModExp (ModLambda p r me _) =
          onModParam p <> maybe mempty (onSigExp . fst) r <> onModExp me

        onModParam = onSigExp . modParamType

        onSigExp (SigVar v _) = S.singleton $ qualLeaf v
        onSigExp (SigParens e _) = onSigExp e
        onSigExp SigSpecs{} = mempty
        onSigExp (SigWith e _ _) = onSigExp e
        onSigExp (SigArrow _ e1 e2 _) = onSigExp e1 <> onSigExp e2

-- | Extract a leading @((name, namespace, file), remainder)@ from a
-- documentation comment string.  These are formatted as
-- \`name\`\@namespace[\@file].  Let us hope that this pattern does not occur
-- anywhere else.
identifierReference :: String -> Maybe ((String, String, Maybe FilePath), String)
identifierReference ('`' : s)
  | (identifier, '`' : '@' : s') <- break (=='`') s,
    (namespace, s'') <- span isAlpha s',
    not $ null namespace =
      case s'' of
        '@' : '"' : s'''
          | (file, '"' : s'''') <- span (/= '"') s''' ->
            Just ((identifier, namespace, Just file), s'''')
        _ -> Just ((identifier, namespace, Nothing), s'')

identifierReference _ = Nothing

-- | Find all the identifier references in a string.
identifierReferences :: String -> [(String, String, Maybe FilePath)]
identifierReferences [] = []
identifierReferences s
  | Just (ref, s') <- identifierReference s =
      ref : identifierReferences s'
identifierReferences (_:s') =
  identifierReferences s'

-- | Given an operator name, return the operator that determines its
-- syntactical properties.
leadingOperator :: Name -> BinOp
leadingOperator s = maybe Backtick snd $ find ((`isPrefixOf` s') . fst) $
                    sortBy (flip $ comparing $ length . fst) $
                    zip (map pretty operators) operators
  where s' = nameToString s
        operators :: [BinOp]
        operators = [minBound..maxBound::BinOp]

-- | A type with no aliasing information but shape annotations.
type UncheckedType = TypeBase (ShapeDecl Name) ()

type UncheckedTypeExp = TypeExp Name

-- | An array element type with no aliasing information.
type UncheckedArrayElemType = ArrayElemTypeBase (ShapeDecl Name)

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

-- | A case (of a match expression) with no type annotations.
type UncheckedCase = CaseBase NoInfo Name
