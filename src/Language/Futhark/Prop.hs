-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Language.Syntax" simple, and put whatever embellishments
-- we need here.
module Language.Futhark.Prop
  ( -- * Various
    Intrinsic (..),
    intrinsics,
    isBuiltin,
    isBuiltinLoc,
    maxIntrinsicTag,
    namesToPrimTypes,
    qualName,
    qualify,
    primValueType,
    leadingOperator,
    progImports,
    decImports,
    progModuleTypes,
    identifierReference,
    prettyStacktrace,
    progHoles,
    defaultEntryPoint,
    paramName,

    -- * Queries on expressions
    typeOf,
    valBindTypeScheme,
    valBindBound,
    funType,

    -- * Queries on patterns and params
    patIdents,
    patNames,
    patternMap,
    patternType,
    patternStructType,
    patternParam,
    patternOrderZero,

    -- * Queries on types
    uniqueness,
    unique,
    aliases,
    diet,
    arrayRank,
    arrayShape,
    orderZero,
    unfoldFunType,
    foldFunType,
    foldFunTypeFromParams,
    typeVars,

    -- * Operations on types
    peelArray,
    stripArray,
    arrayOf,
    toStructural,
    toStruct,
    fromStruct,
    setAliases,
    addAliases,
    setUniqueness,
    noSizes,
    traverseDims,
    DimPos (..),
    tupleRecord,
    isTupleRecord,
    areTupleFields,
    tupleFields,
    tupleFieldNames,
    sortFields,
    sortConstrs,
    isTypeParam,
    isSizeParam,
    combineTypeShapes,
    matchDims,
    -- | Values of these types are produces by the parser.  They use
    -- unadorned names and have no type information, apart from that
    -- which is syntactically required.
    NoInfo (..),
    UncheckedType,
    UncheckedTypeExp,
    UncheckedIdent,
    UncheckedDimIndex,
    UncheckedSlice,
    UncheckedExp,
    UncheckedModExp,
    UncheckedSigExp,
    UncheckedTypeParam,
    UncheckedPat,
    UncheckedValBind,
    UncheckedDec,
    UncheckedSpec,
    UncheckedProg,
    UncheckedCase,
  )
where

import Control.Monad
import Control.Monad.State
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable (bitraverse)
import Data.Char
import Data.Foldable
import Data.List (genericLength, isPrefixOf, sortOn)
import Data.Loc (Loc (..), posFile)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Data.Set qualified as S
import Futhark.Util (maxinum)
import Futhark.Util.Pretty
import Language.Futhark.Primitive qualified as Primitive
import Language.Futhark.Syntax
import Language.Futhark.Traversals
import Language.Futhark.Tuple
import System.FilePath (takeDirectory)

-- | The name of the default program entry point (@main@).
defaultEntryPoint :: Name
defaultEntryPoint = nameFromString "main"

-- | Return the dimensionality of a type.  For non-arrays, this is
-- zero.  For a one-dimensional array it is one, for a two-dimensional
-- it is two, and so forth.
arrayRank :: TypeBase dim as -> Int
arrayRank = shapeRank . arrayShape

-- | Return the shape of a type - for non-arrays, this is 'mempty'.
arrayShape :: TypeBase dim as -> Shape dim
arrayShape (Array _ _ ds _) = ds
arrayShape _ = mempty

-- | Change the shape of a type to be just the rank.
noSizes :: TypeBase Size as -> TypeBase () as
noSizes = first $ const ()

-- | Where does this dimension occur?
data DimPos
  = -- | Immediately in the argument to 'traverseDims'.
    PosImmediate
  | -- | In a function parameter type.
    PosParam
  | -- | In a function return type.
    PosReturn
  deriving (Eq, Ord, Show)

-- | Perform a traversal (possibly including replacement) on sizes
-- that are parameters in a function type, but also including the type
-- immediately passed to the function.  Also passes along a set of the
-- parameter names inside the type that have come in scope at the
-- occurrence of the dimension.
traverseDims ::
  forall f fdim tdim als.
  Applicative f =>
  (S.Set VName -> DimPos -> fdim -> f tdim) ->
  TypeBase fdim als ->
  f (TypeBase tdim als)
traverseDims f = go mempty PosImmediate
  where
    go ::
      forall als'.
      S.Set VName ->
      DimPos ->
      TypeBase fdim als' ->
      f (TypeBase tdim als')
    go bound b t@Array {} =
      bitraverse (f bound b) pure t
    go bound b (Scalar (Record fields)) =
      Scalar . Record <$> traverse (go bound b) fields
    go bound b (Scalar (TypeVar as u tn targs)) =
      Scalar <$> (TypeVar as u tn <$> traverse (onTypeArg bound b) targs)
    go bound b (Scalar (Sum cs)) =
      Scalar . Sum <$> traverse (traverse (go bound b)) cs
    go _ _ (Scalar (Prim t)) =
      pure $ Scalar $ Prim t
    go bound _ (Scalar (Arrow als p u t1 (RetType dims t2))) =
      Scalar <$> (Arrow als p u <$> go bound' PosParam t1 <*> (RetType dims <$> go bound' PosReturn t2))
      where
        bound' =
          S.fromList dims
            <> case p of
              Named p' -> S.insert p' bound
              Unnamed -> bound

    onTypeArg bound b (TypeArgDim d loc) =
      TypeArgDim <$> f bound b d <*> pure loc
    onTypeArg bound b (TypeArgType t loc) =
      TypeArgType <$> go bound b t <*> pure loc

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape as -> Uniqueness
uniqueness (Array _ u _ _) = u
uniqueness (Scalar (TypeVar _ u _ _)) = u
uniqueness (Scalar (Sum ts))
  | any (any unique) ts = Unique
uniqueness (Scalar (Record fs))
  | any unique fs = Unique
uniqueness _ = Nonunique

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase shape as -> Bool
unique = (== Unique) . uniqueness

-- | Return the set of all variables mentioned in the aliasing of a
-- type.
aliases :: Monoid as => TypeBase shape as -> as
aliases = bifoldMap (const mempty) id

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ consumes its argument.
diet :: TypeBase shape as -> Diet
diet (Scalar (Record ets)) = foldl max Observe $ fmap diet ets
diet (Scalar (Prim _)) = Observe
diet (Scalar (Arrow {})) = Observe
diet (Array _ Unique _ _) = Consume
diet (Array _ Nonunique _ _) = Observe
diet (Scalar (TypeVar _ Unique _ _)) = Consume
diet (Scalar (TypeVar _ Nonunique _ _)) = Observe
diet (Scalar (Sum cs)) = foldl max Observe $ foldMap (map diet) cs

-- | Convert any type to one that has rank information, no alias
-- information, and no embedded names.
toStructural ::
  TypeBase dim as ->
  TypeBase () ()
toStructural = flip setAliases () . first (const ())

-- | Remove aliasing information from a type.
toStruct ::
  TypeBase dim as ->
  TypeBase dim ()
toStruct t = t `setAliases` ()

-- | Replace no aliasing with an empty alias set.
fromStruct ::
  TypeBase dim as ->
  TypeBase dim Aliasing
fromStruct t = t `setAliases` S.empty

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: Int -> TypeBase dim as -> Maybe (TypeBase dim as)
peelArray n (Array als u shape t)
  | shapeRank shape == n =
      Just $ Scalar t `addAliases` const als
  | otherwise =
      Array als u <$> stripDims n shape <*> pure t
peelArray _ _ = Nothing

-- | @arrayOf u s t@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
-- The uniqueness of the new array will be @u@, no matter the
-- uniqueness of @t@.
arrayOf ::
  Monoid as =>
  Uniqueness ->
  Shape dim ->
  TypeBase dim as ->
  TypeBase dim as
arrayOf = arrayOfWithAliases mempty

arrayOfWithAliases ::
  Monoid as =>
  as ->
  Uniqueness ->
  Shape dim ->
  TypeBase dim as ->
  TypeBase dim as
arrayOfWithAliases as2 u shape2 (Array as1 _ shape1 et) =
  Array (as1 <> as2) u (shape2 <> shape1) et
arrayOfWithAliases as u shape (Scalar t) =
  Array as u shape (second (const ()) t)

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: Int -> TypeBase dim as -> TypeBase dim as
stripArray n (Array als u shape et)
  | Just shape' <- stripDims n shape =
      Array als u shape' et
  | otherwise =
      Scalar et `setUniqueness` u `setAliases` als
stripArray _ t = t

-- | Create a record type corresponding to a tuple with the given
-- element types.
tupleRecord :: [TypeBase dim as] -> ScalarTypeBase dim as
tupleRecord = Record . M.fromList . zip tupleFieldNames

-- | Does this type corespond to a tuple?  If so, return the elements
-- of that tuple.
isTupleRecord :: TypeBase dim as -> Maybe [TypeBase dim as]
isTupleRecord (Scalar (Record fs)) = areTupleFields fs
isTupleRecord _ = Nothing

-- | Sort the constructors of a sum type in some well-defined (but not
-- otherwise significant) manner.
sortConstrs :: M.Map Name a -> [(Name, a)]
sortConstrs cs = sortOn fst $ M.toList cs

-- | Is this a 'TypeParamType'?
isTypeParam :: TypeParamBase vn -> Bool
isTypeParam TypeParamType {} = True
isTypeParam TypeParamDim {} = False

-- | Is this a 'TypeParamDim'?
isSizeParam :: TypeParamBase vn -> Bool
isSizeParam = not . isTypeParam

-- | Combine the shape information of types as much as possible. The first
-- argument is the orignal type and the second is the type of the transformed
-- expression. This is necessary since the original type may contain additional
-- information (e.g., shape restrictions) from the user given annotation.
combineTypeShapes ::
  (Monoid as) =>
  TypeBase Size as ->
  TypeBase Size as ->
  TypeBase Size as
combineTypeShapes (Scalar (Record ts1)) (Scalar (Record ts2))
  | M.keys ts1 == M.keys ts2 =
      Scalar $
        Record $
          M.map
            (uncurry combineTypeShapes)
            (M.intersectionWith (,) ts1 ts2)
combineTypeShapes (Scalar (Sum cs1)) (Scalar (Sum cs2))
  | M.keys cs1 == M.keys cs2 =
      Scalar $
        Sum $
          M.map
            (uncurry $ zipWith combineTypeShapes)
            (M.intersectionWith (,) cs1 cs2)
combineTypeShapes (Scalar (Arrow als1 p1 d1 a1 (RetType dims1 b1))) (Scalar (Arrow als2 _p2 _d2 a2 (RetType _ b2))) =
  Scalar $
    Arrow
      (als1 <> als2)
      p1
      d1
      (combineTypeShapes a1 a2)
      (RetType dims1 (combineTypeShapes b1 b2))
combineTypeShapes (Scalar (TypeVar als1 u1 v targs1)) (Scalar (TypeVar als2 _ _ targs2)) =
  Scalar $ TypeVar (als1 <> als2) u1 v $ zipWith f targs1 targs2
  where
    f (TypeArgType t1 loc) (TypeArgType t2 _) =
      TypeArgType (combineTypeShapes t1 t2) loc
    f targ _ = targ
combineTypeShapes (Array als1 u1 shape1 et1) (Array als2 _u2 _shape2 et2) =
  arrayOfWithAliases
    (als1 <> als2)
    u1
    shape1
    (combineTypeShapes (Scalar et1) (Scalar et2) `setAliases` mempty)
combineTypeShapes _ new_tp = new_tp

-- | The name, if any.
paramName :: PName -> Maybe VName
paramName (Named v) = Just v
paramName Unnamed = Nothing

-- | Match the dimensions of otherwise assumed-equal types.  The
-- combining function is also passed the names bound within the type
-- (from named parameters or return types).
matchDims ::
  forall as m d1 d2.
  (Monoid as, Monad m) =>
  ([VName] -> d1 -> d2 -> m d1) ->
  TypeBase d1 as ->
  TypeBase d2 as ->
  m (TypeBase d1 as)
matchDims onDims = matchDims' mempty
  where
    matchDims' ::
      forall as'. Monoid as' => [VName] -> TypeBase d1 as' -> TypeBase d2 as' -> m (TypeBase d1 as')
    matchDims' bound t1 t2 =
      case (t1, t2) of
        (Array als1 u1 shape1 et1, Array als2 u2 shape2 et2) ->
          flip setAliases (als1 <> als2)
            <$> ( arrayOf (min u1 u2)
                    <$> onShapes bound shape1 shape2
                    <*> matchDims' bound (Scalar et1) (Scalar et2)
                )
        (Scalar (Record f1), Scalar (Record f2)) ->
          Scalar . Record
            <$> traverse (uncurry (matchDims' bound)) (M.intersectionWith (,) f1 f2)
        (Scalar (Sum cs1), Scalar (Sum cs2)) ->
          Scalar . Sum
            <$> traverse
              (traverse (uncurry (matchDims' bound)))
              (M.intersectionWith zip cs1 cs2)
        ( Scalar (Arrow als1 p1 d1 a1 (RetType dims1 b1)),
          Scalar (Arrow als2 p2 _d2 a2 (RetType dims2 b2))
          ) ->
            let bound' = mapMaybe paramName [p1, p2] <> dims1 <> dims2 <> bound
             in Scalar
                  <$> ( Arrow (als1 <> als2) p1 d1
                          <$> matchDims' bound' a1 a2
                          <*> (RetType dims1 <$> matchDims' bound' b1 b2)
                      )
        ( Scalar (TypeVar als1 u v targs1),
          Scalar (TypeVar als2 _ _ targs2)
          ) ->
            Scalar . TypeVar (als1 <> als2) u v
              <$> zipWithM (matchTypeArg bound) targs1 targs2
        _ -> pure t1

    matchTypeArg _ ta@TypeArgType {} _ = pure ta
    matchTypeArg bound (TypeArgDim x loc) (TypeArgDim y _) =
      TypeArgDim <$> onDims bound x y <*> pure loc
    matchTypeArg _ a _ = pure a

    onShapes bound shape1 shape2 =
      Shape <$> zipWithM (onDims bound) (shapeDims shape1) (shapeDims shape2)

-- | Set the uniqueness attribute of a type.  If the type is a record
-- or sum type, the uniqueness of its components will be modified.
setUniqueness :: TypeBase dim as -> Uniqueness -> TypeBase dim as
setUniqueness (Array als _ shape et) u =
  Array als u shape et
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
addAliases ::
  TypeBase dim asf ->
  (asf -> ast) ->
  TypeBase dim ast
addAliases = flip second

intValueType :: IntValue -> IntType
intValueType Int8Value {} = Int8
intValueType Int16Value {} = Int16
intValueType Int32Value {} = Int32
intValueType Int64Value {} = Int64

floatValueType :: FloatValue -> FloatType
floatValueType Float16Value {} = Float16
floatValueType Float32Value {} = Float32
floatValueType Float64Value {} = Float64

-- | The type of a basic value.
primValueType :: PrimValue -> PrimType
primValueType (SignedValue v) = Signed $ intValueType v
primValueType (UnsignedValue v) = Unsigned $ intValueType v
primValueType (FloatValue v) = FloatType $ floatValueType v
primValueType BoolValue {} = Bool

-- | The type of an Futhark term.  The aliasing will refer to itself, if
-- the term is a non-tuple-typed variable.
typeOf :: ExpBase Info VName -> PatType
typeOf (Literal val _) = Scalar $ Prim $ primValueType val
typeOf (IntLit _ (Info t) _) = t
typeOf (FloatLit _ (Info t) _) = t
typeOf (Parens e _) = typeOf e
typeOf (QualParens _ e _) = typeOf e
typeOf (TupLit es _) = Scalar $ tupleRecord $ map typeOf es
typeOf (RecordLit fs _) =
  -- Reverse, because M.unions is biased to the left.
  Scalar $ Record $ M.unions $ reverse $ map record fs
  where
    record (RecordFieldExplicit name e _) = M.singleton name $ typeOf e
    record (RecordFieldImplicit name (Info t) _) =
      M.singleton (baseName name) $
        t
          `addAliases` S.insert (AliasBound name)
typeOf (ArrayLit _ (Info t) _) = t
typeOf (StringLit vs _) =
  Array
    mempty
    Nonunique
    (Shape [ConstSize $ genericLength vs])
    (Prim (Unsigned Int8))
typeOf (Project _ _ (Info t) _) = t
typeOf (Var _ (Info t) _) = t
typeOf (Hole (Info t) _) = t
typeOf (Ascript e _ _) = typeOf e
typeOf (Negate e _) = typeOf e
typeOf (Not e _) = typeOf e
typeOf (Update e _ _ _) = typeOf e `setAliases` mempty
typeOf (RecordUpdate _ _ _ (Info t) _) = t
typeOf (Assert _ e _ _) = typeOf e
typeOf (Lambda params _ _ (Info (als, t)) _) =
  funType params t `setAliases` als
typeOf (OpSection _ (Info t) _) =
  t
typeOf (OpSectionLeft _ _ _ (_, Info (pn, pt2)) (Info ret, _) _) =
  Scalar $ Arrow mempty pn Observe pt2 ret
typeOf (OpSectionRight _ _ _ (Info (pn, pt1), _) (Info ret) _) =
  Scalar $ Arrow mempty pn Observe pt1 ret
typeOf (ProjectSection _ (Info t) _) = t
typeOf (IndexSection _ (Info t) _) = t
typeOf (Constr _ _ (Info t) _) = t
typeOf (Attr _ e _) = typeOf e
typeOf (AppExp _ (Info res)) = appResType res

-- | The type of a function with the given parameters and return type.
funType :: [PatBase Info VName] -> StructRetType -> StructType
funType params ret =
  let RetType _ t = foldr (arrow . patternParam) ret params
   in t
  where
    arrow (xp, d, xt) yt =
      RetType [] $ Scalar $ Arrow () xp d xt' yt
      where
        xt' = xt `setUniqueness` Nonunique

-- | @foldFunType ts ret@ creates a function type ('Arrow') that takes
-- @ts@ as parameters and returns @ret@.
foldFunType ::
  Monoid as =>
  [(Diet, TypeBase dim pas)] ->
  RetTypeBase dim as ->
  TypeBase dim as
foldFunType ps ret =
  let RetType _ t = foldr arrow ret ps
   in t
  where
    arrow (d, t1) t2 =
      RetType [] $ Scalar $ Arrow mempty Unnamed d t1' t2
      where
        t1' = toStruct t1 `setUniqueness` Nonunique

foldFunTypeFromParams ::
  Monoid as =>
  [PatBase Info VName] ->
  RetTypeBase Size as ->
  TypeBase Size as
foldFunTypeFromParams params =
  foldFunType (zip (map diet params_ts) params_ts)
  where
    params_ts = map patternStructType params

-- | Extract the parameter types and return type from a type.
-- If the type is not an arrow type, the list of parameter types is empty.
unfoldFunType :: TypeBase dim as -> ([(Diet, TypeBase dim ())], TypeBase dim ())
unfoldFunType (Scalar (Arrow _ _ d t1 (RetType _ t2))) =
  let (ps, r) = unfoldFunType t2
   in ((d, t1) : ps, r)
unfoldFunType t = ([], toStruct t)

-- | The type scheme of a value binding, comprising the type
-- parameters and the actual type.
valBindTypeScheme :: ValBindBase Info VName -> ([TypeParamBase VName], StructType)
valBindTypeScheme vb =
  ( valBindTypeParams vb,
    funType (valBindParams vb) (unInfo (valBindRetType vb))
  )

-- | The names that are brought into scope by this value binding (not
-- including its own parameter names, but including any existential
-- sizes).
valBindBound :: ValBindBase Info VName -> [VName]
valBindBound vb =
  valBindName vb
    : case valBindParams vb of
      [] -> retDims (unInfo (valBindRetType vb))
      _ -> []

-- | The type names mentioned in a type.
typeVars :: Monoid as => TypeBase dim as -> S.Set VName
typeVars t =
  case t of
    Scalar Prim {} -> mempty
    Scalar (TypeVar _ _ tn targs) ->
      mconcat $ S.singleton (qualLeaf tn) : map typeArgFree targs
    Scalar (Arrow _ _ _ t1 (RetType _ t2)) -> typeVars t1 <> typeVars t2
    Scalar (Record fields) -> foldMap typeVars fields
    Scalar (Sum cs) -> mconcat $ (foldMap . fmap) typeVars cs
    Array _ _ _ rt -> typeVars $ Scalar rt
  where
    typeArgFree (TypeArgType ta _) = typeVars ta
    typeArgFree TypeArgDim {} = mempty

-- | @orderZero t@ is 'True' if the argument type has order 0, i.e., it is not
-- a function type, does not contain a function type as a subcomponent, and may
-- not be instantiated with a function type.
orderZero :: TypeBase dim as -> Bool
orderZero Array {} = True
orderZero (Scalar (Prim _)) = True
orderZero (Scalar (Record fs)) = all orderZero $ M.elems fs
orderZero (Scalar TypeVar {}) = True
orderZero (Scalar Arrow {}) = False
orderZero (Scalar (Sum cs)) = all (all orderZero) cs

-- | @patternOrderZero pat@ is 'True' if all of the types in the given pattern
-- have order 0.
patternOrderZero :: PatBase Info vn -> Bool
patternOrderZero pat = case pat of
  TuplePat ps _ -> all patternOrderZero ps
  RecordPat fs _ -> all (patternOrderZero . snd) fs
  PatParens p _ -> patternOrderZero p
  Id _ (Info t) _ -> orderZero t
  Wildcard (Info t) _ -> orderZero t
  PatAscription p _ _ -> patternOrderZero p
  PatLit _ (Info t) _ -> orderZero t
  PatConstr _ _ ps _ -> all patternOrderZero ps
  PatAttr _ p _ -> patternOrderZero p

-- | The set of identifiers bound in a pattern.
patIdents :: (Functor f, Ord vn) => PatBase f vn -> S.Set (IdentBase f vn)
patIdents (Id v t loc) = S.singleton $ Ident v t loc
patIdents (PatParens p _) = patIdents p
patIdents (TuplePat pats _) = mconcat $ map patIdents pats
patIdents (RecordPat fs _) = mconcat $ map (patIdents . snd) fs
patIdents Wildcard {} = mempty
patIdents (PatAscription p _ _) = patIdents p
patIdents PatLit {} = mempty
patIdents (PatConstr _ _ ps _) = mconcat $ map patIdents ps
patIdents (PatAttr _ p _) = patIdents p

-- | The set of names bound in a pattern.
patNames :: (Functor f, Ord vn) => PatBase f vn -> S.Set vn
patNames (Id v _ _) = S.singleton v
patNames (PatParens p _) = patNames p
patNames (TuplePat pats _) = mconcat $ map patNames pats
patNames (RecordPat fs _) = mconcat $ map (patNames . snd) fs
patNames Wildcard {} = mempty
patNames (PatAscription p _ _) = patNames p
patNames PatLit {} = mempty
patNames (PatConstr _ _ ps _) = mconcat $ map patNames ps
patNames (PatAttr _ p _) = patNames p

-- | A mapping from names bound in a map to their identifier.
patternMap :: (Functor f) => PatBase f VName -> M.Map VName (IdentBase f VName)
patternMap pat =
  M.fromList $ zip (map identName idents) idents
  where
    idents = S.toList $ patIdents pat

-- | The type of values bound by the pattern.
patternType :: PatBase Info VName -> PatType
patternType (Wildcard (Info t) _) = t
patternType (PatParens p _) = patternType p
patternType (Id _ (Info t) _) = t
patternType (TuplePat pats _) = Scalar $ tupleRecord $ map patternType pats
patternType (RecordPat fs _) = Scalar $ Record $ patternType <$> M.fromList fs
patternType (PatAscription p _ _) = patternType p
patternType (PatLit _ (Info t) _) = t
patternType (PatConstr _ (Info t) _ _) = t
patternType (PatAttr _ p _) = patternType p

-- | The type matched by the pattern, including shape declarations if present.
patternStructType :: PatBase Info VName -> StructType
patternStructType = toStruct . patternType

-- | When viewed as a function parameter, does this pattern correspond
-- to a named parameter of some type?
patternParam :: PatBase Info VName -> (PName, Diet, StructType)
patternParam (PatParens p _) =
  patternParam p
patternParam (PatAttr _ p _) =
  patternParam p
patternParam (PatAscription (Id v (Info t) _) _ _) =
  (Named v, diet t, toStruct t)
patternParam (Id v (Info t) _) =
  (Named v, diet t, toStruct t)
patternParam p =
  (Unnamed, diet p_t, p_t)
  where
    p_t = patternStructType p

-- | Names of primitive types to types.  This is only valid if no
-- shadowing is going on, but useful for tools.
namesToPrimTypes :: M.Map Name PrimType
namesToPrimTypes =
  M.fromList
    [ (nameFromString $ prettyString t, t)
      | t <-
          Bool
            : map Signed [minBound .. maxBound]
            ++ map Unsigned [minBound .. maxBound]
            ++ map FloatType [minBound .. maxBound]
    ]

-- | The nature of something predefined.  For functions, these can
-- either be monomorphic or overloaded.  An overloaded builtin is a
-- list valid types it can be instantiated with, to the parameter and
-- result type, with 'Nothing' representing the overloaded parameter
-- type.
data Intrinsic
  = IntrinsicMonoFun [PrimType] PrimType
  | IntrinsicOverloadedFun [PrimType] [Maybe PrimType] (Maybe PrimType)
  | IntrinsicPolyFun [TypeParamBase VName] [(Diet, StructType)] (RetTypeBase Size ())
  | IntrinsicType Liftedness [TypeParamBase VName] StructType
  | IntrinsicEquality -- Special cased.

intrinsicAcc :: (VName, Intrinsic)
intrinsicAcc =
  ( acc_v,
    IntrinsicType SizeLifted [TypeParamType Unlifted t_v mempty] $
      Scalar $
        TypeVar () Nonunique (qualName acc_v) [arg]
  )
  where
    acc_v = VName "acc" 10
    t_v = VName "t" 11
    arg = TypeArgType (Scalar (TypeVar () Nonunique (qualName t_v) [])) mempty

-- | A map of all built-ins.
intrinsics :: M.Map VName Intrinsic
intrinsics =
  (M.fromList [intrinsicAcc] <>) $
    M.fromList $
      zipWith namify [20 ..] $
        map primFun (M.toList Primitive.primFuns)
          ++ map unOpFun Primitive.allUnOps
          ++ map binOpFun Primitive.allBinOps
          ++ map cmpOpFun Primitive.allCmpOps
          ++ map convOpFun Primitive.allConvOps
          ++ map signFun Primitive.allIntTypes
          ++ map unsignFun Primitive.allIntTypes
          ++ map
            intrinsicPrim
            ( map Signed [minBound .. maxBound]
                ++ map Unsigned [minBound .. maxBound]
                ++ map FloatType [minBound .. maxBound]
                ++ [Bool]
            )
          ++
          -- This overrides the ! from Primitive.
          [ ( "!",
              IntrinsicOverloadedFun
                ( map Signed [minBound .. maxBound]
                    ++ map Unsigned [minBound .. maxBound]
                    ++ [Bool]
                )
                [Nothing]
                Nothing
            )
          ]
          ++
          -- The reason for the loop formulation is to ensure that we
          -- get a missing case warning if we forget a case.
          mapMaybe mkIntrinsicBinOp [minBound .. maxBound]
          ++ [ ( "flatten",
                 IntrinsicPolyFun
                   [tp_a, sp_n, sp_m]
                   [(Observe, Array () Nonunique (shape [n, m]) t_a)]
                   $ RetType [k]
                   $ Array () Nonunique (shape [k]) t_a
               ),
               ( "unflatten",
                 IntrinsicPolyFun
                   [tp_a, sp_n]
                   [ (Observe, Scalar $ Prim $ Signed Int64),
                     (Observe, Scalar $ Prim $ Signed Int64),
                     (Observe, Array () Nonunique (shape [n]) t_a)
                   ]
                   $ RetType [k, m]
                   $ Array () Nonunique (shape [k, m]) t_a
               ),
               ( "concat",
                 IntrinsicPolyFun
                   [tp_a, sp_n, sp_m]
                   [ (Observe, array_a $ shape [n]),
                     (Observe, array_a $ shape [m])
                   ]
                   $ RetType [k]
                   $ uarray_a
                   $ shape [k]
               ),
               ( "rotate",
                 IntrinsicPolyFun
                   [tp_a, sp_n]
                   [ (Observe, Scalar $ Prim $ Signed Int64),
                     (Observe, array_a $ shape [n])
                   ]
                   $ RetType []
                   $ array_a
                   $ shape [n]
               ),
               ( "transpose",
                 IntrinsicPolyFun
                   [tp_a, sp_n, sp_m]
                   [(Observe, array_a $ shape [n, m])]
                   $ RetType []
                   $ array_a
                   $ shape [m, n]
               ),
               ( "scatter",
                 IntrinsicPolyFun
                   [tp_a, sp_n, sp_l]
                   [ (Consume, Array () Unique (shape [n]) t_a),
                     (Observe, Array () Nonunique (shape [l]) (Prim $ Signed Int64)),
                     (Observe, Array () Nonunique (shape [l]) t_a)
                   ]
                   $ RetType []
                   $ Array () Unique (shape [n]) t_a
               ),
               ( "scatter_2d",
                 IntrinsicPolyFun
                   [tp_a, sp_n, sp_m, sp_l]
                   [ (Consume, uarray_a $ shape [n, m]),
                     (Observe, Array () Nonunique (shape [l]) (tupInt64 2)),
                     (Observe, Array () Nonunique (shape [l]) t_a)
                   ]
                   $ RetType []
                   $ uarray_a
                   $ shape [n, m]
               ),
               ( "scatter_3d",
                 IntrinsicPolyFun
                   [tp_a, sp_n, sp_m, sp_k, sp_l]
                   [ (Consume, uarray_a $ shape [n, m, k]),
                     (Observe, Array () Nonunique (shape [l]) (tupInt64 3)),
                     (Observe, Array () Nonunique (shape [l]) t_a)
                   ]
                   $ RetType []
                   $ uarray_a
                   $ shape [n, m, k]
               ),
               ( "zip",
                 IntrinsicPolyFun
                   [tp_a, tp_b, sp_n]
                   [ (Observe, array_a (shape [n])),
                     (Observe, array_b (shape [n]))
                   ]
                   $ RetType []
                   $ tuple_uarray (Scalar t_a) (Scalar t_b)
                   $ shape [n]
               ),
               ( "unzip",
                 IntrinsicPolyFun
                   [tp_a, tp_b, sp_n]
                   [(Observe, tuple_arr (Scalar t_a) (Scalar t_b) $ shape [n])]
                   $ RetType [] . Scalar . Record . M.fromList
                   $ zip tupleFieldNames [array_a $ shape [n], array_b $ shape [n]]
               ),
               ( "hist_1d",
                 IntrinsicPolyFun
                   [tp_a, sp_n, sp_m]
                   [ (Consume, Scalar $ Prim $ Signed Int64),
                     (Observe, uarray_a $ shape [m]),
                     (Observe, Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a)),
                     (Observe, Scalar t_a),
                     (Observe, Array () Nonunique (shape [n]) (tupInt64 1)),
                     (Observe, array_a (shape [n]))
                   ]
                   $ RetType []
                   $ uarray_a
                   $ shape [m]
               ),
               ( "hist_2d",
                 IntrinsicPolyFun
                   [tp_a, sp_n, sp_m, sp_k]
                   [ (Observe, Scalar $ Prim $ Signed Int64),
                     (Consume, uarray_a $ shape [m, k]),
                     (Observe, Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a)),
                     (Observe, Scalar t_a),
                     (Observe, Array () Nonunique (shape [n]) (tupInt64 2)),
                     (Observe, array_a (shape [n]))
                   ]
                   $ RetType []
                   $ uarray_a
                   $ shape [m, k]
               ),
               ( "hist_3d",
                 IntrinsicPolyFun
                   [tp_a, sp_n, sp_m, sp_k, sp_l]
                   [ (Observe, Scalar $ Prim $ Signed Int64),
                     (Consume, uarray_a $ shape [m, k, l]),
                     (Observe, Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a)),
                     (Observe, Scalar t_a),
                     (Observe, Array () Nonunique (shape [n]) (tupInt64 3)),
                     (Observe, array_a (shape [n]))
                   ]
                   $ RetType []
                   $ uarray_a
                   $ shape [m, k, l]
               ),
               ( "map",
                 IntrinsicPolyFun
                   [tp_a, tp_b, sp_n]
                   [ (Observe, Scalar t_a `arr` Scalar t_b),
                     (Observe, array_a $ shape [n])
                   ]
                   $ RetType []
                   $ uarray_b
                   $ shape [n]
               ),
               ( "reduce",
                 IntrinsicPolyFun
                   [tp_a, sp_n]
                   [ (Observe, Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a)),
                     (Observe, Scalar t_a),
                     (Observe, array_a $ shape [n])
                   ]
                   $ RetType []
                   $ Scalar t_a
               ),
               ( "reduce_comm",
                 IntrinsicPolyFun
                   [tp_a, sp_n]
                   [ (Observe, Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a)),
                     (Observe, Scalar t_a),
                     (Observe, array_a $ shape [n])
                   ]
                   $ RetType []
                   $ Scalar t_a
               ),
               ( "scan",
                 IntrinsicPolyFun
                   [tp_a, sp_n]
                   [ (Observe, Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a)),
                     (Observe, Scalar t_a),
                     (Observe, array_a $ shape [n])
                   ]
                   $ RetType []
                   $ uarray_a
                   $ shape [n]
               ),
               ( "partition",
                 IntrinsicPolyFun
                   [tp_a, sp_n]
                   [ (Observe, Scalar (Prim $ Signed Int32)),
                     (Observe, Scalar t_a `arr` Scalar (Prim $ Signed Int64)),
                     (Observe, array_a $ shape [n])
                   ]
                   ( RetType [m] . Scalar $
                       tupleRecord
                         [ uarray_a $ shape [k],
                           Array () Unique (shape [n]) (Prim $ Signed Int64)
                         ]
                   )
               ),
               ( "acc_write",
                 IntrinsicPolyFun
                   [sp_k, tp_a]
                   [ (Consume, Scalar $ accType array_ka),
                     (Observe, Scalar (Prim $ Signed Int64)),
                     (Observe, Scalar t_a)
                   ]
                   $ RetType []
                   $ Scalar
                   $ accType array_ka
               ),
               ( "scatter_stream",
                 IntrinsicPolyFun
                   [tp_a, tp_b, sp_k, sp_n]
                   [ (Consume, uarray_ka),
                     ( Observe,
                       Scalar (accType array_ka)
                         `carr` ( Scalar t_b
                                    `arr` Scalar (accType $ array_a $ shape [k])
                                )
                     ),
                     (Observe, array_b $ shape [n])
                   ]
                   $ RetType [] uarray_ka
               ),
               ( "hist_stream",
                 IntrinsicPolyFun
                   [tp_a, tp_b, sp_k, sp_n]
                   [ (Consume, uarray_a $ shape [k]),
                     (Observe, Scalar t_a `arr` (Scalar t_a `arr` Scalar t_a)),
                     (Observe, Scalar t_a),
                     ( Observe,
                       Scalar (accType array_ka)
                         `carr` ( Scalar t_b
                                    `arr` Scalar (accType $ array_a $ shape [k])
                                )
                     ),
                     (Observe, array_b $ shape [n])
                   ]
                   $ RetType []
                   $ uarray_a
                   $ shape [k]
               ),
               ( "jvp2",
                 IntrinsicPolyFun
                   [tp_a, tp_b]
                   [ (Observe, Scalar t_a `arr` Scalar t_b),
                     (Observe, Scalar t_a),
                     (Observe, Scalar t_a)
                   ]
                   $ RetType []
                   $ Scalar
                   $ tupleRecord [Scalar t_b, Scalar t_b]
               ),
               ( "vjp2",
                 IntrinsicPolyFun
                   [tp_a, tp_b]
                   [ (Observe, Scalar t_a `arr` Scalar t_b),
                     (Observe, Scalar t_a),
                     (Observe, Scalar t_b)
                   ]
                   $ RetType []
                   $ Scalar
                   $ tupleRecord [Scalar t_b, Scalar t_a]
               )
             ]
          ++
          -- Experimental LMAD ones.
          [ ( "flat_index_2d",
              IntrinsicPolyFun
                [tp_a, sp_n]
                [ (Observe, array_a $ shape [n]),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64))
                ]
                $ RetType [m, k]
                $ array_a
                $ shape [m, k]
            ),
            ( "flat_update_2d",
              IntrinsicPolyFun
                [tp_a, sp_n, sp_k, sp_l]
                [ (Consume, uarray_a $ shape [n]),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, array_a $ shape [k, l])
                ]
                $ RetType []
                $ uarray_a
                $ shape [n]
            ),
            ( "flat_index_3d",
              IntrinsicPolyFun
                [tp_a, sp_n]
                [ (Observe, array_a $ shape [n]),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64))
                ]
                $ RetType [m, k, l]
                $ array_a
                $ shape [m, k, l]
            ),
            ( "flat_update_3d",
              IntrinsicPolyFun
                [tp_a, sp_n, sp_k, sp_l, sp_p]
                [ (Consume, uarray_a $ shape [n]),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, array_a $ shape [k, l, p])
                ]
                $ RetType []
                $ uarray_a
                $ shape [n]
            ),
            ( "flat_index_4d",
              IntrinsicPolyFun
                [tp_a, sp_n]
                [ (Observe, array_a $ shape [n]),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64))
                ]
                $ RetType [m, k, l, p]
                $ array_a
                $ shape [m, k, l, p]
            ),
            ( "flat_update_4d",
              IntrinsicPolyFun
                [tp_a, sp_n, sp_k, sp_l, sp_p, sp_q]
                [ (Consume, uarray_a $ shape [n]),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, Scalar (Prim $ Signed Int64)),
                  (Observe, array_a $ shape [k, l, p, q])
                ]
                $ RetType []
                $ uarray_a
                $ shape [n]
            )
          ]
  where
    [a, b, n, m, k, l, p, q] = zipWith VName (map nameFromString ["a", "b", "n", "m", "k", "l", "p", "q"]) [0 ..]

    t_a = TypeVar () Nonunique (qualName a) []
    array_a s = Array () Nonunique s t_a
    uarray_a s = Array () Unique s t_a
    tp_a = TypeParamType Unlifted a mempty

    t_b = TypeVar () Nonunique (qualName b) []
    array_b s = Array () Nonunique s t_b
    uarray_b s = Array () Unique s t_b
    tp_b = TypeParamType Unlifted b mempty

    [sp_n, sp_m, sp_k, sp_l, sp_p, sp_q] = map (`TypeParamDim` mempty) [n, m, k, l, p, q]

    shape = Shape . map (NamedSize . qualName)

    tuple_arr x y s =
      Array
        ()
        Nonunique
        s
        (Record (M.fromList $ zip tupleFieldNames [x, y]))
    tuple_uarray x y s = tuple_arr x y s `setUniqueness` Unique

    arr x y = Scalar $ Arrow mempty Unnamed Observe x (RetType [] y)
    carr x y = Scalar $ Arrow mempty Unnamed Consume x (RetType [] y)

    array_ka = Array () Nonunique (Shape [NamedSize $ qualName k]) t_a
    uarray_ka = Array () Unique (Shape [NamedSize $ qualName k]) t_a

    accType t =
      TypeVar () Unique (qualName (fst intrinsicAcc)) [TypeArgType t mempty]

    namify i (x, y) = (VName (nameFromString x) i, y)

    primFun (name, (ts, t, _)) =
      (name, IntrinsicMonoFun (map unPrim ts) $ unPrim t)

    unOpFun bop = (prettyString bop, IntrinsicMonoFun [t] t)
      where
        t = unPrim $ Primitive.unOpType bop

    binOpFun bop = (prettyString bop, IntrinsicMonoFun [t, t] t)
      where
        t = unPrim $ Primitive.binOpType bop

    cmpOpFun bop = (prettyString bop, IntrinsicMonoFun [t, t] Bool)
      where
        t = unPrim $ Primitive.cmpOpType bop

    convOpFun cop = (prettyString cop, IntrinsicMonoFun [unPrim ft] $ unPrim tt)
      where
        (ft, tt) = Primitive.convOpType cop

    signFun t = ("sign_" ++ prettyString t, IntrinsicMonoFun [Unsigned t] $ Signed t)

    unsignFun t = ("unsign_" ++ prettyString t, IntrinsicMonoFun [Signed t] $ Unsigned t)

    unPrim (Primitive.IntType t) = Signed t
    unPrim (Primitive.FloatType t) = FloatType t
    unPrim Primitive.Bool = Bool
    unPrim Primitive.Unit = Bool

    intrinsicPrim t = (prettyString t, IntrinsicType Unlifted [] $ Scalar $ Prim t)

    anyIntType =
      map Signed [minBound .. maxBound]
        ++ map Unsigned [minBound .. maxBound]
    anyNumberType =
      anyIntType
        ++ map FloatType [minBound .. maxBound]
    anyPrimType = Bool : anyNumberType

    mkIntrinsicBinOp :: BinOp -> Maybe (String, Intrinsic)
    mkIntrinsicBinOp op = do
      op' <- intrinsicBinOp op
      pure (prettyString op, op')

    binOp ts = Just $ IntrinsicOverloadedFun ts [Nothing, Nothing] Nothing
    ordering = Just $ IntrinsicOverloadedFun anyPrimType [Nothing, Nothing] (Just Bool)

    intrinsicBinOp Plus = binOp anyNumberType
    intrinsicBinOp Minus = binOp anyNumberType
    intrinsicBinOp Pow = binOp anyNumberType
    intrinsicBinOp Times = binOp anyNumberType
    intrinsicBinOp Divide = binOp anyNumberType
    intrinsicBinOp Mod = binOp anyNumberType
    intrinsicBinOp Quot = binOp anyIntType
    intrinsicBinOp Rem = binOp anyIntType
    intrinsicBinOp ShiftR = binOp anyIntType
    intrinsicBinOp ShiftL = binOp anyIntType
    intrinsicBinOp Band = binOp anyIntType
    intrinsicBinOp Xor = binOp anyIntType
    intrinsicBinOp Bor = binOp anyIntType
    intrinsicBinOp LogAnd = binOp [Bool]
    intrinsicBinOp LogOr = binOp [Bool]
    intrinsicBinOp Equal = Just IntrinsicEquality
    intrinsicBinOp NotEqual = Just IntrinsicEquality
    intrinsicBinOp Less = ordering
    intrinsicBinOp Leq = ordering
    intrinsicBinOp Greater = ordering
    intrinsicBinOp Geq = ordering
    intrinsicBinOp _ = Nothing

    tupInt64 1 =
      Prim $ Signed Int64
    tupInt64 x =
      tupleRecord $ replicate x $ Scalar $ Prim $ Signed Int64

-- | Is this include part of the built-in prelude?
isBuiltin :: FilePath -> Bool
isBuiltin = (== "/prelude") . takeDirectory

-- | Is the position of this thing builtin as per 'isBuiltin'?  Things
-- without location are considered not built-in.
isBuiltinLoc :: Located a => a -> Bool
isBuiltinLoc x =
  case locOf x of
    NoLoc -> False
    Loc pos _ -> isBuiltin $ posFile pos

-- | The largest tag used by an intrinsic - this can be used to
-- determine whether a 'VName' refers to an intrinsic or a user-defined name.
maxIntrinsicTag :: Int
maxIntrinsicTag = maxinum $ map baseTag $ M.keys intrinsics

-- | Create a name with no qualifiers from a name.
qualName :: v -> QualName v
qualName = QualName []

-- | Add another qualifier (at the head) to a qualified name.
qualify :: v -> QualName v -> QualName v
qualify k (QualName ks v) = QualName (k : ks) v

-- | The modules imported by a Futhark program.
progImports :: ProgBase f vn -> [(String, Loc)]
progImports = concatMap decImports . progDecs

-- | The modules imported by a single declaration.
decImports :: DecBase f vn -> [(String, Loc)]
decImports (OpenDec x _) = modExpImports x
decImports (ModDec md) = modExpImports $ modExp md
decImports SigDec {} = []
decImports TypeDec {} = []
decImports ValDec {} = []
decImports (LocalDec d _) = decImports d
decImports (ImportDec x _ loc) = [(x, locOf loc)]

modExpImports :: ModExpBase f vn -> [(String, Loc)]
modExpImports ModVar {} = []
modExpImports (ModParens p _) = modExpImports p
modExpImports (ModImport f _ loc) = [(f, locOf loc)]
modExpImports (ModDecs ds _) = concatMap decImports ds
modExpImports (ModApply _ me _ _ _) = modExpImports me
modExpImports (ModAscript me _ _ _) = modExpImports me
modExpImports ModLambda {} = []

-- | The set of module types used in any exported (non-local)
-- declaration.
progModuleTypes :: ProgBase Info VName -> S.Set VName
progModuleTypes prog = foldMap reach mtypes_used
  where
    -- Fixed point iteration.
    reach v = S.singleton v <> maybe mempty (foldMap reach) (M.lookup v reachable_from_mtype)

    reachable_from_mtype = foldMap onDec $ progDecs prog
      where
        onDec OpenDec {} = mempty
        onDec ModDec {} = mempty
        onDec (SigDec sb) =
          M.singleton (sigName sb) (onSigExp (sigExp sb))
        onDec TypeDec {} = mempty
        onDec ValDec {} = mempty
        onDec (LocalDec d _) = onDec d
        onDec ImportDec {} = mempty

        onSigExp (SigVar v _ _) = S.singleton $ qualLeaf v
        onSigExp (SigParens e _) = onSigExp e
        onSigExp (SigSpecs ss _) = foldMap onSpec ss
        onSigExp (SigWith e _ _) = onSigExp e
        onSigExp (SigArrow _ e1 e2 _) = onSigExp e1 <> onSigExp e2

        onSpec ValSpec {} = mempty
        onSpec TypeSpec {} = mempty
        onSpec TypeAbbrSpec {} = mempty
        onSpec (ModSpec vn e _ _) = S.singleton vn <> onSigExp e
        onSpec (IncludeSpec e _) = onSigExp e

    mtypes_used = foldMap onDec $ progDecs prog
      where
        onDec (OpenDec x _) = onModExp x
        onDec (ModDec md) =
          maybe mempty (onSigExp . fst) (modSignature md) <> onModExp (modExp md)
        onDec SigDec {} = mempty
        onDec TypeDec {} = mempty
        onDec ValDec {} = mempty
        onDec LocalDec {} = mempty
        onDec ImportDec {} = mempty

        onModExp ModVar {} = mempty
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
        onSigExp SigSpecs {} = mempty
        onSigExp (SigWith e _ _) = onSigExp e
        onSigExp (SigArrow _ e1 e2 _) = onSigExp e1 <> onSigExp e2

-- | Extract a leading @((name, namespace, file), remainder)@ from a
-- documentation comment string.  These are formatted as
-- \`name\`\@namespace[\@file].  Let us hope that this pattern does not occur
-- anywhere else.
identifierReference :: String -> Maybe ((String, String, Maybe FilePath), String)
identifierReference ('`' : s)
  | (identifier, '`' : '@' : s') <- break (== '`') s,
    (namespace, s'') <- span isAlpha s',
    not $ null namespace =
      case s'' of
        '@' : '"' : s'''
          | (file, '"' : s'''') <- span (/= '"') s''' ->
              Just ((identifier, namespace, Just file), s'''')
        _ -> Just ((identifier, namespace, Nothing), s'')
identifierReference _ = Nothing

-- | Given an operator name, return the operator that determines its
-- syntactical properties.
leadingOperator :: Name -> BinOp
leadingOperator s =
  maybe Backtick snd $
    find ((`isPrefixOf` s') . fst) $
      sortOn (Down . length . fst) $
        zip (map prettyString operators) operators
  where
    s' = nameToString s
    operators :: [BinOp]
    operators = [minBound .. maxBound :: BinOp]

-- | Find instances of typed holes in the program.
progHoles :: ProgBase Info VName -> [(Loc, StructType)]
progHoles = foldMap holesInDec . progDecs
  where
    holesInDec (ValDec vb) = holesInExp $ valBindBody vb
    holesInDec (ModDec me) = holesInModExp $ modExp me
    holesInDec (OpenDec me _) = holesInModExp me
    holesInDec (LocalDec d _) = holesInDec d
    holesInDec TypeDec {} = mempty
    holesInDec SigDec {} = mempty
    holesInDec ImportDec {} = mempty

    holesInModExp (ModDecs ds _) = foldMap holesInDec ds
    holesInModExp (ModParens me _) = holesInModExp me
    holesInModExp (ModApply x y _ _ _) = holesInModExp x <> holesInModExp y
    holesInModExp (ModAscript me _ _ _) = holesInModExp me
    holesInModExp (ModLambda _ _ me _) = holesInModExp me
    holesInModExp ModVar {} = mempty
    holesInModExp ModImport {} = mempty

    holesInExp = flip execState mempty . onExp

    onExp e@(Hole (Info t) loc) = do
      modify ((locOf loc, toStruct t) :)
      pure e
    onExp e = astMap (identityMapper {mapOnExp = onExp}) e

-- | A type with no aliasing information but shape annotations.
type UncheckedType = TypeBase (Shape Name) ()

-- | An expression with no type annotations.
type UncheckedTypeExp = TypeExp NoInfo Name

-- | An identifier with no type annotations.
type UncheckedIdent = IdentBase NoInfo Name

-- | An index with no type annotations.
type UncheckedDimIndex = DimIndexBase NoInfo Name

-- | A slice with no type annotations.
type UncheckedSlice = SliceBase NoInfo Name

-- | An expression with no type annotations.
type UncheckedExp = ExpBase NoInfo Name

-- | A module expression with no type annotations.
type UncheckedModExp = ModExpBase NoInfo Name

-- | A module type expression with no type annotations.
type UncheckedSigExp = SigExpBase NoInfo Name

-- | A type parameter with no type annotations.
type UncheckedTypeParam = TypeParamBase Name

-- | A pattern with no type annotations.
type UncheckedPat = PatBase NoInfo Name

-- | A function declaration with no type annotations.
type UncheckedValBind = ValBindBase NoInfo Name

-- | A declaration with no type annotations.
type UncheckedDec = DecBase NoInfo Name

-- | A spec with no type annotations.
type UncheckedSpec = SpecBase NoInfo Name

-- | A Futhark program with no type annotations.
type UncheckedProg = ProgBase NoInfo Name

-- | A case (of a match expression) with no type annotations.
type UncheckedCase = CaseBase NoInfo Name
