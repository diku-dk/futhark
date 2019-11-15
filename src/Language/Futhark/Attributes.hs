{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , primValueType
  , leadingOperator
  , progImports
  , decImports
  , progModuleTypes
  , identifierReference
  , identifierReferences
  , prettyStacktrace

  -- * Queries on expressions
  , typeOf

  -- * Queries on patterns and params
  , patternIdents
  , patternType
  , patternStructType
  , patternParam
  , patternOrderZero
  , patternDimNames

  -- * Queries on types
  , uniqueness
  , unique
  , aliases
  , diet
  , arrayRank
  , arrayShape
  , nestedDims
  , orderZero
  , unfoldFunType
  , foldFunType
  , typeVars
  , typeDimNames
  , primByteSize

  -- * Operations on types
  , rank
  , peelArray
  , stripArray
  , arrayOf
  , toStructural
  , toStruct
  , fromStruct
  , setAliases
  , addAliases
  , setUniqueness
  , removeShapeAnnotations
  , vacuousShapeAnnotations
  , anyDimShapeAnnotations
  , traverseDims
  , DimPos(..)
  , tupleRecord
  , isTupleRecord
  , areTupleFields
  , tupleFields
  , tupleFieldNames
  , sortFields
  , sortConstrs
  , isTypeParam
  , isSizeParam
  , combineTypeShapes
  , unscopeType
  , onRecordField

  -- | Values of these types are produces by the parser.  They use
  -- unadorned names and have no type information, apart from that
  -- which is syntactically required.
  , NoInfo(..)
  , UncheckedType
  , UncheckedTypeExp
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

import           Control.Monad.Writer  hiding (Sum)
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
import           Data.Bitraversable (bitraverse)

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
  case t of Array _ _ a ds ->
              nub $ nestedDims (Scalar a) <> shapeDims ds
            Scalar (Record fs) ->
              nub $ foldMap nestedDims fs
            Scalar Prim{} ->
              mempty
            Scalar (Sum cs) ->
              nub $ foldMap (concatMap nestedDims) cs
            Scalar (Arrow _ v t1 t2) ->
              filter (notV v) $ nestedDims t1 <> nestedDims t2
            Scalar (TypeVar _ _ _ targs) ->
              concatMap typeArgDims targs

  where typeArgDims (TypeArgDim d _) = [d]
        typeArgDims (TypeArgType at _) = nestedDims at

        notV Unnamed  = const True
        notV (Named v) = (/=NamedDim (qualName v))

-- | Change the shape of a type to be just the 'Rank'.
removeShapeAnnotations :: TypeBase (DimDecl vn) as -> TypeBase () as
removeShapeAnnotations = modifyShapeAnnotations $ const ()

-- | Add size annotations that are all 'AnyDim'.
vacuousShapeAnnotations :: TypeBase () as -> TypeBase (DimDecl vn) as
vacuousShapeAnnotations = modifyShapeAnnotations $ const AnyDim

-- | Change all size annotations to be 'AnyDim'.
anyDimShapeAnnotations :: TypeBase (DimDecl vn) as -> TypeBase (DimDecl vn) as
anyDimShapeAnnotations = modifyShapeAnnotations $ const AnyDim

-- | Change the size annotations of a type.
modifyShapeAnnotations :: (oldshape -> newshape)
                       -> TypeBase oldshape as
                       -> TypeBase newshape as
modifyShapeAnnotations = first

-- | Where does this dimension occur?
data DimPos
  = PosImmediate
    -- ^ Immediately in the argument to 'traverseDims'.
  | PosParam
    -- ^ In a function parameter type.
  | PosReturn
    -- ^ In a function return type.
  deriving (Eq, Ord, Show)

-- | Perform a traversal (possibly including replacement) on sizes
-- that are parameters in a function type, but also including the type
-- immediately passed to the function.
traverseDims :: forall f fdim tdim als.
                Applicative f =>
                (DimPos -> fdim -> f tdim)
             -> TypeBase fdim als
             -> f (TypeBase tdim als)
traverseDims f = go PosImmediate
  where go :: forall als'. DimPos -> TypeBase fdim als' -> f (TypeBase tdim als')
        go b t@Array{} = bitraverse (f b) pure t
        go b (Scalar (Record fields)) = Scalar . Record <$> traverse (go b) fields
        go b (Scalar (TypeVar as u tn targs)) =
          Scalar <$> (TypeVar as u tn <$> traverse (onTypeArg b) targs)
        go b (Scalar (Sum cs)) = Scalar . Sum <$> traverse (traverse (go b)) cs
        go _ (Scalar (Prim t)) = pure $ Scalar $ Prim t
        go _ (Scalar (Arrow als p t1 t2)) =
          Scalar <$> (Arrow als p <$> go PosParam t1 <*> go PosReturn t2)

        onTypeArg b (TypeArgDim d loc) =
          TypeArgDim <$> f b d <*> pure loc
        onTypeArg b (TypeArgType t loc) =
          TypeArgType <$> go b t <*> pure loc

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape as -> Uniqueness
uniqueness (Array _ u _ _) = u
uniqueness (Scalar (TypeVar _ u _ _)) = u
uniqueness (Scalar (Sum ts)) = mconcat $ map (mconcat . map uniqueness) $ M.elems ts
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
diet (Scalar (Record ets))      = RecordDiet $ fmap diet ets
diet (Scalar (Prim _))          = Observe
diet (Scalar TypeVar{})         = Observe
diet (Scalar (Arrow _ _ t1 t2)) = FuncDiet (diet t1) (diet t2)
diet (Array _ Unique _ _)       = Consume
diet (Array _ Nonunique _ _)    = Observe
diet (Scalar Sum{})             = Observe

-- | Convert any type to one that has rank information, no alias
-- information, and no embedded names.
toStructural :: TypeBase dim as
             -> TypeBase () ()
toStructural = flip setAliases () . modifyShapeAnnotations (const ())

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
peelArray n (Array als u t shape)
  | shapeRank shape == n =
      Just $ Scalar t `addAliases` const als
  | otherwise =
      Array als u t <$> stripDims n shape
peelArray _ _ = Nothing

-- | @arrayOf t s u@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.
arrayOf :: Monoid as =>
           TypeBase dim as
        -> ShapeDecl dim
        -> Uniqueness
        -> TypeBase dim as
arrayOf t = arrayOfWithAliases (t `setUniqueness` Nonunique) mempty

arrayOfWithAliases :: Monoid as =>
                      TypeBase dim as
                   -> as
                   -> ShapeDecl dim
                   -> Uniqueness
                   -> TypeBase dim as
arrayOfWithAliases (Array as1 _ et shape1) as2 shape2 u =
  Array (as1<>as2) u et (shape2 <> shape1)
arrayOfWithAliases (Scalar t) as shape u =
  Array as u (second (const ()) t) shape

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: Int -> TypeBase dim as -> TypeBase dim as
stripArray n (Array als u et shape)
  | Just shape' <- stripDims n shape =
      Array als u et shape'
  | otherwise =
      Scalar et `setUniqueness` u `setAliases` als
stripArray _ t = t

-- | Create a record type corresponding to a tuple with the given
-- element types.
tupleRecord :: [TypeBase dim as] -> TypeBase dim as
tupleRecord = Scalar . Record . M.fromList . zip tupleFieldNames

isTupleRecord :: TypeBase dim as -> Maybe [TypeBase dim as]
isTupleRecord (Scalar (Record fs)) = areTupleFields fs
isTupleRecord _ = Nothing

-- | Does this record map correspond to a tuple?
areTupleFields :: M.Map Name a -> Maybe [a]
areTupleFields fs =
  let fs' = sortFields fs
  in if and $ zipWith (==) (map fst fs') tupleFieldNames
     then Just $ map snd fs'
     else Nothing

-- | Construct a record map corresponding to a tuple.
tupleFields :: [a] -> M.Map Name a
tupleFields as = M.fromList $ zip tupleFieldNames as

-- | Increasing field names for a tuple (starts at 0).
tupleFieldNames :: [Name]
tupleFieldNames = map (nameFromString . show) [(0::Int)..]

-- | Sort fields by their name; taking care to sort numeric fields by
-- their numeric value.  This ensures that tuples and tuple-like
-- records match.
sortFields :: M.Map Name a -> [(Name,a)]
sortFields l = map snd $ sortOn fst $ zip (map (fieldish . fst) l') l'
  where l' = M.toList l
        fieldish s = case reads $ nameToString s of
          [(x, "")] -> Left (x::Int)
          _         -> Right s
sortConstrs :: M.Map Name a -> [(Name, a)]
sortConstrs cs = sortOn fst $ M.toList cs

isTypeParam :: TypeParamBase vn -> Bool
isTypeParam TypeParamType{}       = True
isTypeParam TypeParamDim{}        = False

isSizeParam :: TypeParamBase vn -> Bool
isSizeParam = not . isTypeParam

-- | Combine the shape information of types as much as possible. The first
-- argument is the orignal type and the second is the type of the transformed
-- expression. This is necessary since the original type may contain additional
-- information (e.g., shape restrictions) from the user given annotation.
combineTypeShapes :: (Monoid as, ArrayDim dim) =>
                     TypeBase dim as -> TypeBase dim as -> TypeBase dim as
combineTypeShapes (Scalar (Record ts1)) (Scalar (Record ts2))
  | M.keys ts1 == M.keys ts2 =
      Scalar $ Record $ M.map (uncurry combineTypeShapes) (M.intersectionWith (,) ts1 ts2)
combineTypeShapes (Array als1 u1 et1 shape1) (Array als2 _u2 et2 shape2)
  | Just new_shape <- unifyShapes shape1 shape2 =
      arrayOfWithAliases (combineTypeShapes (Scalar et1) (Scalar et2)
                          `setAliases` mempty)
      (als1<>als2) new_shape u1
combineTypeShapes _ new_tp = new_tp

-- | Set the uniqueness attribute of a type.  If the type is a record
-- or sum type, the uniqueness of its components will be modified.
setUniqueness :: TypeBase dim as -> Uniqueness -> TypeBase dim as
setUniqueness (Array als _ et shape) u =
  Array als u et shape
setUniqueness (Scalar (TypeVar als _ t targs)) u =
  Scalar $ TypeVar als u t targs
setUniqueness (Scalar (Record ets)) u =
  Scalar $ Record $ fmap (`setUniqueness` u) ets
setUniqueness (Scalar (Sum ets)) u =
  Scalar $ Sum $ fmap (map (`setUniqueness` u)) ets
setUniqueness t _ = t

-- | @t \`setAliases\` als@ returns @t@, but with @als@ substituted for
-- any already present aliasing.
setAliases :: TypeBase dim asf -> ast -> TypeBase dim ast
setAliases t = addAliases t . const

-- | @t \`addAliases\` f@ returns @t@, but with any already present
-- aliasing replaced by @f@ applied to that aliasing.
addAliases :: TypeBase dim asf -> (asf -> ast)
           -> TypeBase dim ast
addAliases = flip second

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

-- | The type of the value.
valueType :: Value -> ValueType
valueType (PrimValue bv) = Scalar $ Prim $ primValueType bv
valueType (ArrayValue _ t) = t

-- | The size of values of this type, in bytes.
primByteSize :: Num a => PrimType -> a
primByteSize (Signed it) = Primitive.intByteSize it
primByteSize (Unsigned it) = Primitive.intByteSize it
primByteSize (FloatType ft) = Primitive.floatByteSize ft
primByteSize Bool = 1

-- | Construct a 'ShapeDecl' with the given number of 'AnyDim'
-- dimensions.
rank :: Int -> ShapeDecl (DimDecl VName)
rank n = ShapeDecl $ replicate n AnyDim

-- | The type is leaving a scope, so clean up any aliases that
-- reference the bound variables, and turn any dimensions that name
-- them into AnyDim instead.
unscopeType :: S.Set VName -> PatternType -> PatternType
unscopeType bound_here t = modifyShapeAnnotations onDim $ t `addAliases` S.map unbind
  where unbind (AliasBound v) | v `S.member` bound_here = AliasFree v
        unbind a = a
        onDim (NamedDim qn) | qualLeaf qn `S.member` bound_here = AnyDim
        onDim d = d

-- | Perform some operation on a given record field.  Returns
-- 'Nothing' if that field does not exist.
onRecordField :: (TypeBase dim als -> TypeBase dim als)
              -> [Name]
              -> TypeBase dim als -> Maybe (TypeBase dim als)
onRecordField f [] t = Just $ f t
onRecordField f (k:ks) (Scalar (Record m)) = do
  t <- onRecordField f ks =<< M.lookup k m
  Just $ Scalar $ Record $ M.insert k t m
onRecordField _ _ _ = Nothing

-- | The type of an Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: ExpBase Info VName -> PatternType
typeOf (Literal val _) = Scalar $ Prim $ primValueType val
typeOf (IntLit _ (Info t) _) = t
typeOf (FloatLit _ (Info t) _) = t
typeOf (Parens e _) = typeOf e
typeOf (QualParens _ e _) = typeOf e
typeOf (TupLit es _) = tupleRecord $ map typeOf es
typeOf (RecordLit fs _) =
  -- Reverse, because M.unions is biased to the left.
  Scalar $ Record $ M.unions $ reverse $ map record fs
  where record (RecordFieldExplicit name e _) = M.singleton name $ typeOf e
        record (RecordFieldImplicit name (Info t) _) =
          M.singleton (baseName name) $ t
          `addAliases` S.insert (AliasBound name)
typeOf (ArrayLit _ (Info t) _) = t
typeOf (StringLit vs _) =
  Array mempty Unique (Prim (Unsigned Int8))
  (ShapeDecl [ConstDim $ genericLength vs])
typeOf (Range _ _ _ (Info t, _) _) = t
typeOf (BinOp _ _ _ _ (Info t) _ _) = t
typeOf (Project _ _ (Info t) _) = t
typeOf (If _ _ _ (Info t, _) _) = t
typeOf (Var _ (Info t) _) = t
typeOf (Ascript e _ _) = typeOf e
typeOf (Coerce _ _ (Info t, _) _) = t
typeOf (Apply _ _ _ (Info t, _) _) = t
typeOf (Negate e _) = typeOf e
typeOf (LetPat _ _ _ (Info t, _) _) = t
typeOf (LetFun name _ body _) = unscopeType (S.singleton name) $ typeOf body
typeOf (LetWith _ _ _ _ _ (Info t) _) = t
typeOf (Index _ _ (Info t, _) _) = t
typeOf (Update e _ _ _) = typeOf e `setAliases` mempty
typeOf (RecordUpdate _ _ _ (Info t) _) = t
typeOf (Unsafe e _) = typeOf e
typeOf (Assert _ e _ _) = typeOf e
typeOf (DoLoop _ _ _ _ _ (Info (t, _)) _) = t
typeOf (Lambda params _ _ (Info (als, t)) _) =
  unscopeType bound_here $ foldr (arrow . patternParam) t params `setAliases` als
  where bound_here = S.map identName (mconcat $ map patternIdents params) `S.difference`
                     S.fromList (mapMaybe (named . patternParam) params)
        arrow (px, tx) y = Scalar $ Arrow () px tx y
        named (Named x, _) = Just x
        named (Unnamed, _) = Nothing
typeOf (OpSection _ (Info t) _) =
  t
typeOf (OpSectionLeft _ _ _ (_, Info pt2) (Info ret, _) _)  =
  foldFunType [fromStruct pt2] ret
typeOf (OpSectionRight _ _ _ (Info pt1, _) (Info ret) _) =
  foldFunType [fromStruct pt1] ret
typeOf (ProjectSection _ (Info t) _) = t
typeOf (IndexSection _ (Info t) _) = t
typeOf (Constr _ _ (Info t) _)  = t
typeOf (Match _ cs (Info t, _) _) =
  unscopeType (foldMap unscopeSet cs) t
  where unscopeSet (CasePat p _ _) = S.map identName $ patternIdents p

foldFunType :: Monoid as => [TypeBase dim as] -> TypeBase dim as -> TypeBase dim as
foldFunType ps ret = foldr arrow ret ps
  where arrow t1 t2 = Scalar $ Arrow mempty Unnamed t1 t2

-- | Extract the parameter types and return type from a type.
-- If the type is not an arrow type, the list of parameter types is empty.
unfoldFunType :: TypeBase dim as -> ([TypeBase dim as], TypeBase dim as)
unfoldFunType (Scalar (Arrow _ _ t1 t2)) =
  let (ps, r) = unfoldFunType t2
  in (t1 : ps, r)
unfoldFunType t = ([], t)

-- | The type names mentioned in a type.
typeVars :: Monoid as => TypeBase dim as -> S.Set VName
typeVars t =
  case t of
    Scalar Prim{} -> mempty
    Scalar (TypeVar _ _ tn targs) ->
      mconcat $ typeVarFree tn : map typeArgFree targs
    Scalar (Arrow _ _ t1 t2) -> typeVars t1 <> typeVars t2
    Scalar (Record fields) -> foldMap typeVars fields
    Scalar (Sum cs) -> mconcat $ (foldMap . fmap) typeVars cs
    Array _ _ rt _ -> typeVars $ Scalar rt
  where typeVarFree = S.singleton . typeLeaf
        typeArgFree (TypeArgType ta _) = typeVars ta
        typeArgFree TypeArgDim{} = mempty

-- | @orderZero t@ is 'True' if the argument type has order 0, i.e., it is not
-- a function type, does not contain a function type as a subcomponent, and may
-- not be instantiated with a function type.
orderZero :: TypeBase dim as -> Bool
orderZero Array{}     = True
orderZero (Scalar (Prim _)) = True
orderZero (Scalar (Record fs)) = all orderZero $ M.elems fs
orderZero (Scalar TypeVar{}) = True
orderZero (Scalar Arrow{}) = False
orderZero (Scalar (Sum cs)) = all (all orderZero) cs

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
patternDimNames (PatternConstr _ _ ps _) = foldMap patternDimNames ps

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
  PatternConstr _ _ ps _  -> all patternOrderZero ps

-- | The set of identifiers bound in a pattern.
patternIdents :: (Functor f, Ord vn) => PatternBase f vn -> S.Set (IdentBase f vn)
patternIdents (Id v t loc)              = S.singleton $ Ident v t loc
patternIdents (PatternParens p _)       = patternIdents p
patternIdents (TuplePattern pats _)     = mconcat $ map patternIdents pats
patternIdents (RecordPattern fs _)      = mconcat $ map (patternIdents . snd) fs
patternIdents Wildcard{}                = mempty
patternIdents (PatternAscription p _ _) = patternIdents p
patternIdents PatternLit{}              = mempty
patternIdents (PatternConstr _ _ ps _ ) = mconcat $ map patternIdents ps

-- | The type of values bound by the pattern.
patternType :: PatternBase Info VName -> PatternType
patternType (Wildcard (Info t) _)          = t
patternType (PatternParens p _)            = patternType p
patternType (Id _ (Info t) _)              = t
patternType (TuplePattern pats _)          = tupleRecord $ map patternType pats
patternType (RecordPattern fs _)           = Scalar $ Record $ patternType <$> M.fromList fs
patternType (PatternAscription p _ _)      = patternType p
patternType (PatternLit _ (Info t) _)      = t
patternType (PatternConstr _ (Info t) _ _) = t

-- | The type matched by the pattern, including shape declarations if present.
patternStructType :: PatternBase Info VName -> StructType
patternStructType = toStruct . patternType

-- | When viewed as a function parameter, does this pattern correspond
-- to a named parameter of some type?
patternParam :: PatternBase Info VName -> (PName, StructType)
patternParam (PatternParens p _) =
  patternParam p
patternParam (PatternAscription (Id v _ _) td _) =
  (Named v, unInfo $ expandedType td)
patternParam (Id v (Info t) _) =
  (Named v, toStruct t)
patternParam p =
  (Unnamed, patternStructType p)

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
               | IntrinsicPolyFun [TypeParamBase VName] [StructType] StructType
               | IntrinsicType PrimType
               | IntrinsicEquality -- Special cased.

-- | A map of all built-ins.
intrinsics :: M.Map VName Intrinsic
intrinsics = M.fromList $ zipWith namify [10..] $

             map primFun (M.toList Primitive.primFuns) ++

             [("opaque", IntrinsicPolyFun [tp_a] [Scalar t_a] $ Scalar t_a)] ++

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

             -- This overrides the ! from Primitive.
             [ ("!", IntrinsicOverloadedFun
                     (map Signed [minBound..maxBound] ++
                      map Unsigned [minBound..maxBound] ++
                     [Bool])
                     [Nothing] Nothing) ] ++

             -- The reason for the loop formulation is to ensure that we
             -- get a missing case warning if we forget a case.
             mapMaybe mkIntrinsicBinOp [minBound..maxBound] ++

             [("flatten", IntrinsicPolyFun [tp_a]
                          [Array () Nonunique t_a (rank 2)] $
                          Array () Nonunique t_a (rank 1)),
              ("unflatten", IntrinsicPolyFun [tp_a]
                            [Scalar $ Prim $ Signed Int32,
                             Scalar $ Prim $ Signed Int32,
                             Array () Nonunique t_a (rank 1)] $
                            Array () Nonunique t_a (rank 2)),

              ("concat", IntrinsicPolyFun [tp_a]
                         [arr_a, arr_a] uarr_a),
              ("rotate", IntrinsicPolyFun [tp_a]
                         [Scalar $ Prim $ Signed Int32, arr_a] arr_a),
              ("transpose", IntrinsicPolyFun [tp_a] [arr_2d_a] arr_2d_a),

              ("cmp_threshold", IntrinsicPolyFun []
                                [Scalar $ Prim $ Signed Int32,
                                 Array () Nonunique (Prim $ Signed Int32) (rank 1)] $
                                Scalar $ Prim Bool),

               ("scatter", IntrinsicPolyFun [tp_a]
                          [Array () Unique t_a (rank 1),
                           Array () Nonunique (Prim $ Signed Int32) (rank 1),
                           Array () Nonunique t_a (rank 1)] $
                          Array () Unique t_a (rank 1)),

              ("zip", IntrinsicPolyFun [tp_a, tp_b] [arr_a, arr_b] arr_a_b),
              ("unzip", IntrinsicPolyFun [tp_a, tp_b] [arr_a_b] t_arr_a_arr_b),

              ("hist", IntrinsicPolyFun [tp_a]
                       [Scalar $ Prim $ Signed Int32,
                        uarr_a,
                        Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a),
                        Scalar t_a,
                        Array () Nonunique (Prim $ Signed Int32) (rank 1),
                        arr_a]
                       uarr_a),

              ("map", IntrinsicPolyFun [tp_a, tp_b] [Scalar t_a `arr` Scalar t_b, arr_a] uarr_b),

              ("reduce", IntrinsicPolyFun [tp_a]
                         [Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a), Scalar t_a, arr_a] $
                         Scalar t_a),

              ("reduce_comm", IntrinsicPolyFun [tp_a]
                              [Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a), Scalar t_a, arr_a] $
                              Scalar t_a),

              ("scan", IntrinsicPolyFun [tp_a]
                       [Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a), Scalar t_a, arr_a] uarr_a),

              ("partition",
               IntrinsicPolyFun [tp_a]
               [Scalar (Prim $ Signed Int32),
                Scalar t_a `arr` Scalar (Prim $ Signed Int32), arr_a] $
               tupleRecord [uarr_a, Array () Unique (Prim $ Signed Int32) (rank 1)]),

              ("map_stream",
               IntrinsicPolyFun [tp_a, tp_b]
                [Scalar (Prim $ Signed Int32) `karr` (arr_ka `arr` arr_kb), arr_a]
                uarr_b),

              ("map_stream_per",
               IntrinsicPolyFun [tp_a, tp_b]
                [Scalar (Prim $ Signed Int32) `karr` (arr_ka `arr` arr_kb), arr_a]
                uarr_b),

              ("reduce_stream",
               IntrinsicPolyFun [tp_a, tp_b]
                [Scalar t_b `arr` (Scalar t_b `arr` Scalar t_b),
                 Scalar (Prim $ Signed Int32) `karr` (arr_ka `arr` Scalar t_b),
                 arr_a] $
                Scalar t_b),

              ("reduce_stream_per",
               IntrinsicPolyFun [tp_a, tp_b]
                [Scalar t_b `arr` (Scalar t_b `arr` Scalar t_b),
                 Scalar (Prim $ Signed Int32) `karr` (arr_ka `arr` Scalar t_b),
                 arr_a] $
                Scalar t_b),

              ("trace", IntrinsicPolyFun [tp_a] [Scalar t_a] $ Scalar t_a),
              ("break", IntrinsicPolyFun [tp_a] [Scalar t_a] $ Scalar t_a)]

  where tv_a = VName (nameFromString "a") 0
        t_a = TypeVar () Nonunique (typeName tv_a) []
        arr_a = Array () Nonunique t_a (rank 1)
        arr_2d_a = Array () Nonunique t_a (rank 2)
        uarr_a = Array () Unique t_a (rank 1)
        tp_a = TypeParamType Unlifted tv_a noLoc

        tv_b = VName (nameFromString "b") 1
        t_b = TypeVar () Nonunique (typeName tv_b) []
        arr_b = Array () Nonunique t_b (rank 1)
        uarr_b = Array () Unique t_b (rank 1)
        tp_b = TypeParamType Unlifted tv_b noLoc

        arr_a_b = Array () Nonunique
                  (Record (M.fromList $ zip tupleFieldNames [Scalar t_a, Scalar t_b]))
                  (rank 1)
        t_arr_a_arr_b = Scalar $ Record $ M.fromList $ zip tupleFieldNames [arr_a, arr_b]

        arr x y = Scalar $ Arrow mempty Unnamed x y

        kv = VName (nameFromString "k") 2
        arr_ka = Array () Nonunique t_a (ShapeDecl [NamedDim $ qualName kv])
        arr_kb = Array () Nonunique t_b (ShapeDecl [NamedDim $ qualName kv])
        karr x y = Scalar $ Arrow mempty (Named kv) x y

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
        intrinsicBinOp LogAnd   = binOp [Bool]
        intrinsicBinOp LogOr    = binOp [Bool]
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

        onSigExp (SigVar v _ _) = S.singleton $ qualLeaf v
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
