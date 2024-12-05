-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Language.Syntax" simple, and put whatever embellishments
-- we need here.
module Language.Futhark.Prop
  ( -- * Various
    Intrinsic (..),
    intrinsics,
    intrinsicVar,
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
    anySize,

    -- * Queries on expressions
    typeOf,
    valBindTypeScheme,
    valBindBound,
    funType,
    stripExp,
    subExps,
    similarExps,
    sameExp,

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
    diet,
    arrayRank,
    arrayShape,
    orderZero,
    unfoldFunType,
    foldFunType,
    typeVars,
    isAccType,

    -- * Operations on types
    peelArray,
    stripArray,
    arrayOf,
    arrayOfWithAliases,
    toStructural,
    toStruct,
    toRes,
    toParam,
    resToParam,
    paramToRes,
    toResRet,
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
    matchDims,

    -- * Un-typechecked ASTs
    UncheckedType,
    UncheckedTypeExp,
    UncheckedIdent,
    UncheckedDimIndex,
    UncheckedSlice,
    UncheckedExp,
    UncheckedModExp,
    UncheckedModTypeExp,
    UncheckedTypeParam,
    UncheckedPat,
    UncheckedValBind,
    UncheckedTypeBind,
    UncheckedModTypeBind,
    UncheckedModBind,
    UncheckedDec,
    UncheckedSpec,
    UncheckedProg,
    UncheckedCase,

    -- * Type-checked ASTs
    Ident,
    DimIndex,
    Slice,
    AppExp,
    Exp,
    Pat,
    ModExp,
    ModParam,
    ModTypeExp,
    ModBind,
    ModTypeBind,
    ValBind,
    Dec,
    Spec,
    Prog,
    TypeBind,
    StructTypeArg,
    ScalarType,
    TypeParam,
    Case,
  )
where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable (bitraverse)
import Data.Char
import Data.Foldable
import Data.List (genericLength, isPrefixOf, sortOn)
import Data.List.NonEmpty qualified as NE
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
arrayRank :: TypeBase d u -> Int
arrayRank = shapeRank . arrayShape

-- | Return the shape of a type - for non-arrays, this is 'mempty'.
arrayShape :: TypeBase dim as -> Shape dim
arrayShape (Array _ ds _) = ds
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
  (Applicative f) =>
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
    go bound b (Scalar (TypeVar as tn targs)) =
      Scalar <$> (TypeVar as tn <$> traverse (onTypeArg tn bound b) targs)
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

    onTypeArg _ bound b (TypeArgDim d) =
      TypeArgDim <$> f bound b d
    onTypeArg tn bound b (TypeArgType t) =
      TypeArgType <$> go bound b' t
      where
        b' =
          if qualLeaf tn == fst intrinsicAcc
            then b
            else PosParam

-- | Return the uniqueness of a type.
uniqueness :: TypeBase shape Uniqueness -> Uniqueness
uniqueness (Array u _ _) = u
uniqueness (Scalar (TypeVar u _ _)) = u
uniqueness (Scalar (Sum ts))
  | any (any unique) ts = Unique
uniqueness (Scalar (Record fs))
  | any unique fs = Unique
uniqueness _ = Nonunique

-- | @unique t@ is 'True' if the type of the argument is unique.
unique :: TypeBase shape Uniqueness -> Bool
unique = (== Unique) . uniqueness

-- | @diet t@ returns a description of how a function parameter of
-- type @t@ consumes its argument.
diet :: TypeBase shape Diet -> Diet
diet (Scalar (Record ets)) = foldl max Observe $ fmap diet ets
diet (Scalar (Prim _)) = Observe
diet (Scalar (Arrow {})) = Observe
diet (Array d _ _) = d
diet (Scalar (TypeVar d _ _)) = d
diet (Scalar (Sum cs)) = foldl max Observe $ foldMap (map diet) cs

-- | Convert any type to one that has rank information, no alias
-- information, and no embedded names.
toStructural ::
  TypeBase dim as ->
  TypeBase () ()
toStructural = bimap (const ()) (const ())

-- | Remove uniquenss information from a type.
toStruct :: TypeBase dim u -> TypeBase dim NoUniqueness
toStruct = second (const NoUniqueness)

-- | Uses 'Observe'.
toParam :: Diet -> TypeBase Size u -> ParamType
toParam d = fmap (const d)

-- | Convert to 'ResType'
toRes :: Uniqueness -> TypeBase Size u -> ResType
toRes u = fmap (const u)

-- | Convert to 'ResRetType'
toResRet :: Uniqueness -> RetTypeBase Size u -> ResRetType
toResRet u = second (const u)

-- | Preserves relation between 'Diet' and 'Uniqueness'.
resToParam :: ResType -> ParamType
resToParam = second f
  where
    f Unique = Consume
    f Nonunique = Observe

-- | Preserves relation between 'Diet' and 'Uniqueness'.
paramToRes :: ParamType -> ResType
paramToRes = second f
  where
    f Consume = Unique
    f Observe = Nonunique

-- | @peelArray n t@ returns the type resulting from peeling the first
-- @n@ array dimensions from @t@.  Returns @Nothing@ if @t@ has less
-- than @n@ dimensions.
peelArray :: Int -> TypeBase dim u -> Maybe (TypeBase dim u)
peelArray n (Array u shape t)
  | shapeRank shape == n =
      Just $ second (const u) (Scalar t)
  | otherwise =
      Array u <$> stripDims n shape <*> pure t
peelArray _ _ = Nothing

-- | @arrayOf u s t@ constructs an array type.  The convenience
-- compared to using the 'Array' constructor directly is that @t@ can
-- itself be an array.  If @t@ is an @n@-dimensional array, and @s@ is
-- a list of length @n@, the resulting type is of an @n+m@ dimensions.
arrayOf ::
  Shape dim ->
  TypeBase dim NoUniqueness ->
  TypeBase dim NoUniqueness
arrayOf = arrayOfWithAliases mempty

-- | Like 'arrayOf', but you can pass in uniqueness info of the
-- resulting array.
arrayOfWithAliases ::
  u ->
  Shape dim ->
  TypeBase dim u' ->
  TypeBase dim u
arrayOfWithAliases u shape2 (Array _ shape1 et) =
  Array u (shape2 <> shape1) et
arrayOfWithAliases u shape (Scalar t) =
  Array u shape (second (const mempty) t)

-- | @stripArray n t@ removes the @n@ outermost layers of the array.
-- Essentially, it is the type of indexing an array of type @t@ with
-- @n@ indexes.
stripArray :: Int -> TypeBase dim as -> TypeBase dim as
stripArray n (Array u shape et)
  | Just shape' <- stripDims n shape =
      Array u shape' et
  | otherwise =
      second (const u) (Scalar et)
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

-- | The name, if any.
paramName :: PName -> Maybe VName
paramName (Named v) = Just v
paramName Unnamed = Nothing

-- | A special expression representing no known size.  When present in
-- a type, each instance represents a distinct size.  The type checker
-- should _never_ produce these - they are a (hopefully temporary)
-- thing introduced by defunctorisation and monomorphisation.  They
-- represent a flaw in our implementation.  When they occur in a
-- return type, they can be replaced with freshly created existential
-- sizes.  When they occur in parameter types, they can be replaced
-- with size parameters.
anySize :: Size
anySize =
  -- The definition here is weird to avoid seeing this as a free
  -- variable.
  StringLit [65, 78, 89] mempty

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
      forall u'. (Monoid u') => [VName] -> TypeBase d1 u' -> TypeBase d2 u' -> m (TypeBase d1 u')
    matchDims' bound t1 t2 =
      case (t1, t2) of
        (Array u1 shape1 et1, Array u2 shape2 et2) ->
          arrayOfWithAliases u1
            <$> onShapes bound shape1 shape2
            <*> matchDims' bound (second (const u2) (Scalar et1)) (second (const u2) (Scalar et2))
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
        ( Scalar (TypeVar als1 v targs1),
          Scalar (TypeVar als2 _ targs2)
          ) ->
            Scalar . TypeVar (als1 <> als2) v
              <$> zipWithM (matchTypeArg bound) targs1 targs2
        _ -> pure t1

    matchTypeArg bound (TypeArgType t1) (TypeArgType t2) =
      TypeArgType <$> matchDims' bound t1 t2
    matchTypeArg bound (TypeArgDim x) (TypeArgDim y) =
      TypeArgDim <$> onDims bound x y
    matchTypeArg _ a _ = pure a

    onShapes bound shape1 shape2 =
      Shape <$> zipWithM (onDims bound) (shapeDims shape1) (shapeDims shape2)

-- | Set the uniqueness attribute of a type.  If the type is a record
-- or sum type, the uniqueness of its components will be modified.
setUniqueness :: TypeBase dim u1 -> u2 -> TypeBase dim u2
setUniqueness t u = second (const u) t

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
typeOf :: ExpBase Info VName -> StructType
typeOf (Literal val _) = Scalar $ Prim $ primValueType val
typeOf (IntLit _ (Info t) _) = t
typeOf (FloatLit _ (Info t) _) = t
typeOf (Parens e _) = typeOf e
typeOf (QualParens _ e _) = typeOf e
typeOf (TupLit es _) = Scalar $ tupleRecord $ map typeOf es
typeOf (RecordLit fs _) =
  Scalar $ Record $ M.fromList $ map record fs
  where
    record (RecordFieldExplicit (L _ name) e _) = (name, typeOf e)
    record (RecordFieldImplicit (L _ name) (Info t) _) = (baseName name, t)
typeOf (ArrayLit _ (Info t) _) = t
typeOf (ArrayVal vs t loc) =
  Array mempty (Shape [sizeFromInteger (genericLength vs) loc]) (Prim t)
typeOf (StringLit vs loc) =
  Array
    mempty
    (Shape [sizeFromInteger (genericLength vs) loc])
    (Prim (Unsigned Int8))
typeOf (Project _ _ (Info t) _) = t
typeOf (Var _ (Info t) _) = t
typeOf (Hole (Info t) _) = t
typeOf (Ascript e _ _) = typeOf e
typeOf (Coerce _ _ (Info t) _) = t
typeOf (Negate e _) = typeOf e
typeOf (Not e _) = typeOf e
typeOf (Update e _ _ _) = typeOf e
typeOf (RecordUpdate _ _ _ (Info t) _) = t
typeOf (Assert _ e _ _) = typeOf e
typeOf (Lambda params _ _ (Info t) _) = funType params t
typeOf (OpSection _ (Info t) _) = t
typeOf (OpSectionLeft _ _ _ (_, Info (pn, pt2)) (Info ret, _) _) =
  Scalar $ Arrow mempty pn (diet pt2) (toStruct pt2) ret
typeOf (OpSectionRight _ _ _ (Info (pn, pt1), _) (Info ret) _) =
  Scalar $ Arrow mempty pn (diet pt1) (toStruct pt1) ret
typeOf (ProjectSection _ (Info t) _) = t
typeOf (IndexSection _ (Info t) _) = t
typeOf (Constr _ _ (Info t) _) = t
typeOf (Attr _ e _) = typeOf e
typeOf (AppExp _ (Info res)) = appResType res

-- | The type of a function with the given parameters and return type.
funType :: [Pat ParamType] -> ResRetType -> StructType
funType params ret =
  let RetType _ t = foldr (arrow . patternParam) ret params
   in toStruct t
  where
    arrow (xp, d, xt) yt =
      RetType [] $ Scalar $ Arrow Nonunique xp d xt yt

-- | @foldFunType ts ret@ creates a function type ('Arrow') that takes
-- @ts@ as parameters and returns @ret@.
foldFunType :: [ParamType] -> ResRetType -> StructType
foldFunType ps ret =
  let RetType _ t = foldr arrow ret ps
   in toStruct t
  where
    arrow t1 t2 =
      RetType [] $ Scalar $ Arrow Nonunique Unnamed (diet t1) (toStruct t1) t2

-- | Extract the parameter types and return type from a type.
-- If the type is not an arrow type, the list of parameter types is empty.
unfoldFunType :: TypeBase dim as -> ([TypeBase dim Diet], TypeBase dim NoUniqueness)
unfoldFunType (Scalar (Arrow _ _ d t1 (RetType _ t2))) =
  let (ps, r) = unfoldFunType t2
   in (second (const d) t1 : ps, r)
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
typeVars :: TypeBase dim as -> S.Set VName
typeVars t =
  case t of
    Scalar Prim {} -> mempty
    Scalar (TypeVar _ tn targs) ->
      mconcat $ S.singleton (qualLeaf tn) : map typeArgFree targs
    Scalar (Arrow _ _ _ t1 (RetType _ t2)) -> typeVars t1 <> typeVars t2
    Scalar (Record fields) -> foldMap typeVars fields
    Scalar (Sum cs) -> mconcat $ (foldMap . fmap) typeVars cs
    Array _ _ rt -> typeVars $ Scalar rt
  where
    typeArgFree (TypeArgType ta) = typeVars ta
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
patternOrderZero :: Pat (TypeBase d u) -> Bool
patternOrderZero = orderZero . patternType

-- | The set of identifiers bound in a pattern.
patIdents :: PatBase f vn t -> [IdentBase f vn t]
patIdents (Id v t loc) = [Ident v t loc]
patIdents (PatParens p _) = patIdents p
patIdents (TuplePat pats _) = foldMap patIdents pats
patIdents (RecordPat fs _) = foldMap (patIdents . snd) fs
patIdents Wildcard {} = mempty
patIdents (PatAscription p _ _) = patIdents p
patIdents PatLit {} = mempty
patIdents (PatConstr _ _ ps _) = foldMap patIdents ps
patIdents (PatAttr _ p _) = patIdents p

-- | The set of names bound in a pattern.
patNames :: Pat t -> [VName]
patNames = map fst . patternMap

-- | Each name bound in a pattern alongside its type.
patternMap :: Pat t -> [(VName, t)]
patternMap = map f . patIdents
  where
    f (Ident v (Info t) _) = (v, t)

-- | The type of values bound by the pattern.
patternType :: Pat (TypeBase d u) -> TypeBase d u
patternType (Wildcard (Info t) _) = t
patternType (PatParens p _) = patternType p
patternType (Id _ (Info t) _) = t
patternType (TuplePat pats _) = Scalar $ tupleRecord $ map patternType pats
patternType (RecordPat fs _) =
  Scalar $ Record $ patternType <$> M.fromList (map (first unLoc) fs)
patternType (PatAscription p _ _) = patternType p
patternType (PatLit _ (Info t) _) = t
patternType (PatConstr _ (Info t) _ _) = t
patternType (PatAttr _ p _) = patternType p

-- | The type matched by the pattern, including shape declarations if present.
patternStructType :: Pat (TypeBase Size u) -> StructType
patternStructType = toStruct . patternType

-- | When viewed as a function parameter, does this pattern correspond
-- to a named parameter of some type?
patternParam :: Pat ParamType -> (PName, Diet, StructType)
patternParam (PatParens p _) =
  patternParam p
patternParam (PatAttr _ p _) =
  patternParam p
patternParam (PatAscription (Id v (Info t) _) _ _) =
  (Named v, diet t, toStruct t)
patternParam (Id v (Info t) _) =
  (Named v, diet t, toStruct t)
patternParam p =
  (Unnamed, diet p_t, toStruct p_t)
  where
    p_t = patternType p

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
  | IntrinsicPolyFun [TypeParamBase VName] [ParamType] (RetTypeBase Size Uniqueness)
  | IntrinsicType Liftedness [TypeParamBase VName] StructType
  | IntrinsicEquality -- Special cased.

intrinsicAcc :: (VName, Intrinsic)
intrinsicAcc =
  ( acc_v,
    IntrinsicType SizeLifted [TypeParamType Unlifted t_v mempty] $
      Scalar $
        TypeVar mempty (qualName acc_v) [arg]
  )
  where
    acc_v = VName "acc" 10
    t_v = VName "t" 11
    arg = TypeArgType $ Scalar (TypeVar mempty (qualName t_v) [])

-- | If this type corresponds to the builtin "acc" type, return the
-- type of the underlying array.
isAccType :: TypeBase d u -> Maybe (TypeBase d NoUniqueness)
isAccType (Scalar (TypeVar _ (QualName [] v) [TypeArgType t]))
  | v == fst intrinsicAcc =
      Just t
isAccType _ = Nothing

-- | Find the 'VName' corresponding to a builtin.  Crashes if that
-- name cannot be found.
intrinsicVar :: Name -> VName
intrinsicVar v =
  fromMaybe bad $ find ((v ==) . baseName) $ M.keys intrinsics
  where
    bad = error $ "findBuiltin: " <> nameToString v

mkBinOp :: Name -> StructType -> Exp -> Exp -> Exp
mkBinOp op t x y =
  AppExp
    ( BinOp
        (qualName (intrinsicVar op), mempty)
        (Info t)
        (x, Info Nothing)
        (y, Info Nothing)
        mempty
    )
    (Info $ AppRes t [])

mkAdd, mkMul :: Exp -> Exp -> Exp
mkAdd = mkBinOp "+" $ Scalar $ Prim $ Signed Int64
mkMul = mkBinOp "*" $ Scalar $ Prim $ Signed Int64

-- | A map of all built-ins.
intrinsics :: M.Map VName Intrinsic
intrinsics =
  (M.fromList [intrinsicAcc] <>) $
    M.fromList $
      primOp
        ++ zipWith
          namify
          [intrinsicStart ..]
          ( [ ( "manifest",
                IntrinsicPolyFun
                  [tp_a]
                  [Scalar $ t_a mempty]
                  $ RetType []
                  $ Scalar
                  $ t_a mempty
              ),
              ( "flatten",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_m]
                  [Array Observe (shape [n, m]) $ t_a mempty]
                  $ RetType []
                  $ Array
                    Nonunique
                    (Shape [size n `mkMul` size m])
                    (t_a mempty)
              ),
              ( "unflatten",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_m]
                  [ Scalar $ Prim $ Signed Int64,
                    Scalar $ Prim $ Signed Int64,
                    Array Observe (Shape [size n `mkMul` size m]) $ t_a mempty
                  ]
                  $ RetType []
                  $ Array Nonunique (shape [n, m]) (t_a mempty)
              ),
              ( "concat",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_m]
                  [ array_a Observe $ shape [n],
                    array_a Observe $ shape [m]
                  ]
                  $ RetType []
                  $ array_a Unique
                  $ Shape [size n `mkAdd` size m]
              ),
              ( "transpose",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_m]
                  [array_a Observe $ shape [n, m]]
                  $ RetType []
                  $ array_a Nonunique
                  $ shape [m, n]
              ),
              ( "scatter",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_l]
                  [ Array Consume (shape [n]) $ t_a mempty,
                    Array Observe (shape [l]) (Prim $ Signed Int64),
                    Array Observe (shape [l]) $ t_a mempty
                  ]
                  $ RetType []
                  $ Array Unique (shape [n]) (t_a mempty)
              ),
              ( "scatter_2d",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_m, sp_l]
                  [ array_a Consume $ shape [n, m],
                    Array Observe (shape [l]) (tupInt64 2),
                    Array Observe (shape [l]) $ t_a mempty
                  ]
                  $ RetType []
                  $ array_a Unique
                  $ shape [n, m]
              ),
              ( "scatter_3d",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_m, sp_k, sp_l]
                  [ array_a Consume $ shape [n, m, k],
                    Array Observe (shape [l]) (tupInt64 3),
                    Array Observe (shape [l]) $ t_a mempty
                  ]
                  $ RetType []
                  $ array_a Unique
                  $ shape [n, m, k]
              ),
              ( "zip",
                IntrinsicPolyFun
                  [tp_a, tp_b, sp_n]
                  [ array_a Observe (shape [n]),
                    array_b Observe (shape [n])
                  ]
                  $ RetType []
                  $ tuple_array Unique (Scalar $ t_a mempty) (Scalar $ t_b mempty)
                  $ shape [n]
              ),
              ( "unzip",
                IntrinsicPolyFun
                  [tp_a, tp_b, sp_n]
                  [tuple_array Observe (Scalar $ t_a mempty) (Scalar $ t_b mempty) $ shape [n]]
                  $ RetType [] . Scalar . Record . M.fromList
                  $ zip tupleFieldNames [array_a Unique $ shape [n], array_b Unique $ shape [n]]
              ),
              ( "hist_1d",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_m]
                  [ Scalar $ Prim $ Signed Int64,
                    array_a Consume $ shape [m],
                    Scalar (t_a mempty) `arr` (Scalar (t_a mempty) `arr` Scalar (t_a Nonunique)),
                    Scalar $ t_a Observe,
                    Array Observe (shape [n]) (tupInt64 1),
                    array_a Observe (shape [n])
                  ]
                  $ RetType []
                  $ array_a Unique
                  $ shape [m]
              ),
              ( "hist_2d",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_m, sp_k]
                  [ Scalar $ Prim $ Signed Int64,
                    array_a Consume $ shape [m, k],
                    Scalar (t_a mempty) `arr` (Scalar (t_a mempty) `arr` Scalar (t_a Nonunique)),
                    Scalar $ t_a Observe,
                    Array Observe (shape [n]) (tupInt64 2),
                    array_a Observe (shape [n])
                  ]
                  $ RetType []
                  $ array_a Unique
                  $ shape [m, k]
              ),
              ( "hist_3d",
                IntrinsicPolyFun
                  [tp_a, sp_n, sp_m, sp_k, sp_l]
                  [ Scalar $ Prim $ Signed Int64,
                    array_a Consume $ shape [m, k, l],
                    Scalar (t_a mempty) `arr` (Scalar (t_a mempty) `arr` Scalar (t_a Nonunique)),
                    Scalar $ t_a Observe,
                    Array Observe (shape [n]) (tupInt64 3),
                    array_a Observe (shape [n])
                  ]
                  $ RetType []
                  $ array_a Unique
                  $ shape [m, k, l]
              ),
              ( "map",
                IntrinsicPolyFun
                  [tp_a, tp_b, sp_n]
                  [ Scalar (t_a mempty) `arr` Scalar (t_b Nonunique),
                    array_a Observe $ shape [n]
                  ]
                  $ RetType []
                  $ array_b Unique
                  $ shape [n]
              ),
              ( "reduce",
                IntrinsicPolyFun
                  [tp_a, sp_n]
                  [ Scalar (t_a mempty) `arr` (Scalar (t_a mempty) `arr` Scalar (t_a Nonunique)),
                    Scalar $ t_a Observe,
                    array_a Observe $ shape [n]
                  ]
                  $ RetType []
                  $ Scalar (t_a Unique)
              ),
              ( "reduce_comm",
                IntrinsicPolyFun
                  [tp_a, sp_n]
                  [ Scalar (t_a mempty) `arr` (Scalar (t_a mempty) `arr` Scalar (t_a Nonunique)),
                    Scalar $ t_a Observe,
                    array_a Observe $ shape [n]
                  ]
                  $ RetType [] (Scalar (t_a Unique))
              ),
              ( "scan",
                IntrinsicPolyFun
                  [tp_a, sp_n]
                  [ Scalar (t_a mempty) `arr` (Scalar (t_a mempty) `arr` Scalar (t_a Nonunique)),
                    Scalar $ t_a Observe,
                    array_a Observe $ shape [n]
                  ]
                  $ RetType [] (array_a Unique $ shape [n])
              ),
              ( "partition",
                IntrinsicPolyFun
                  [tp_a, sp_n]
                  [ Scalar (Prim $ Signed Int32),
                    Scalar (t_a mempty) `arr` Scalar (Prim $ Signed Int64),
                    array_a Observe $ shape [n]
                  ]
                  ( RetType [k] . Scalar $
                      tupleRecord
                        [ array_a Unique $ shape [n],
                          Array Unique (shape [k]) (Prim $ Signed Int64)
                        ]
                  )
              ),
              ( "acc_write",
                IntrinsicPolyFun
                  [sp_k, tp_a]
                  [ Scalar $ accType Consume $ array_ka mempty,
                    Scalar (Prim $ Signed Int64),
                    Scalar $ t_a Observe
                  ]
                  $ RetType []
                  $ Scalar
                  $ accType Unique (array_ka mempty)
              ),
              ( "scatter_stream",
                IntrinsicPolyFun
                  [tp_a, tp_b, sp_k, sp_n]
                  [ array_ka Consume,
                    Scalar (accType mempty (array_ka mempty))
                      `carr` ( Scalar (t_b mempty)
                                 `arr` Scalar (accType Nonunique $ array_a mempty $ shape [k])
                             ),
                    array_b Observe $ shape [n]
                  ]
                  $ RetType []
                  $ array_ka Unique
              ),
              ( "hist_stream",
                IntrinsicPolyFun
                  [tp_a, tp_b, sp_k, sp_n]
                  [ array_a Consume $ shape [k],
                    Scalar (t_a mempty) `arr` (Scalar (t_a mempty) `arr` Scalar (t_a Nonunique)),
                    Scalar $ t_a Observe,
                    Scalar (accType mempty $ array_ka mempty)
                      `carr` ( Scalar (t_b mempty)
                                 `arr` Scalar (accType Nonunique $ array_a mempty $ shape [k])
                             ),
                    array_b Observe $ shape [n]
                  ]
                  $ RetType []
                  $ array_a Unique
                  $ shape [k]
              ),
              ( "jvp2",
                IntrinsicPolyFun
                  [tp_a, tp_b]
                  [ Scalar (t_a mempty) `arr` Scalar (t_b Nonunique),
                    Scalar (t_a Observe),
                    Scalar (t_a Observe)
                  ]
                  $ RetType []
                  $ Scalar
                  $ tupleRecord [Scalar $ t_b Nonunique, Scalar $ t_b Nonunique]
              ),
              ( "vjp2",
                IntrinsicPolyFun
                  [tp_a, tp_b]
                  [ Scalar (t_a mempty) `arr` Scalar (t_b Nonunique),
                    Scalar (t_a Observe),
                    Scalar (t_b Observe)
                  ]
                  $ RetType []
                  $ Scalar
                  $ tupleRecord [Scalar $ t_b Nonunique, Scalar $ t_a Nonunique]
              ),
              ( "jvp2_vec",
                IntrinsicPolyFun
                  [tp_a, tp_b, sp_n]
                  [ Scalar (t_a mempty) `arr` Scalar (t_b Nonunique),
                    Scalar (t_a Observe),
                    array_a Observe $ shape [n]
                  ]
                  $ RetType []
                  $ Scalar
                  $ tupleRecord
                    [ Scalar $ t_b Nonunique,
                      array_b Unique $ shape [n]
                    ]
              ),
              ( "vjp2_vec",
                IntrinsicPolyFun
                  [tp_a, tp_b, sp_n]
                  [ Scalar (t_a mempty) `arr` Scalar (t_b Nonunique),
                    Scalar (t_a Observe),
                    array_b Observe $ shape [n]
                  ]
                  $ RetType []
                  $ Scalar
                  $ tupleRecord
                    [ Scalar $ t_b Nonunique,
                      array_a Unique $ shape [n]
                    ]
              )
            ]
              ++
              -- Experimental LMAD ones.
              [ ( "flat_index_2d",
                  IntrinsicPolyFun
                    [tp_a, sp_n]
                    [ array_a Observe $ shape [n],
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64)
                    ]
                    $ RetType [m, k]
                    $ array_a Nonunique
                    $ shape [m, k]
                ),
                ( "flat_update_2d",
                  IntrinsicPolyFun
                    [tp_a, sp_n, sp_k, sp_l]
                    [ array_a Consume $ shape [n],
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      array_a Observe $ shape [k, l]
                    ]
                    $ RetType []
                    $ array_a Unique
                    $ shape [n]
                ),
                ( "flat_index_3d",
                  IntrinsicPolyFun
                    [tp_a, sp_n]
                    [ array_a Observe $ shape [n],
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64)
                    ]
                    $ RetType [m, k, l]
                    $ array_a Nonunique
                    $ shape [m, k, l]
                ),
                ( "flat_update_3d",
                  IntrinsicPolyFun
                    [tp_a, sp_n, sp_k, sp_l, sp_p]
                    [ array_a Consume $ shape [n],
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      array_a Observe $ shape [k, l, p]
                    ]
                    $ RetType []
                    $ array_a Unique
                    $ shape [n]
                ),
                ( "flat_index_4d",
                  IntrinsicPolyFun
                    [tp_a, sp_n]
                    [ array_a Observe $ shape [n],
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64)
                    ]
                    $ RetType [m, k, l, p]
                    $ array_a Nonunique
                    $ shape [m, k, l, p]
                ),
                ( "flat_update_4d",
                  IntrinsicPolyFun
                    [tp_a, sp_n, sp_k, sp_l, sp_p, sp_q]
                    [ array_a Consume $ shape [n],
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      Scalar (Prim $ Signed Int64),
                      array_a Observe $ shape [k, l, p, q]
                    ]
                    $ RetType []
                    $ array_a Unique
                    $ shape [n]
                )
              ]
          )
  where
    primOp =
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
            ),
            ( "neg",
              IntrinsicOverloadedFun
                ( map Signed [minBound .. maxBound]
                    ++ map Unsigned [minBound .. maxBound]
                    ++ map FloatType [minBound .. maxBound]
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

    intrinsicStart = 1 + baseTag (fst $ last primOp)

    [a, b, n, m, k, l, p, q] = zipWith VName (map nameFromString ["a", "b", "n", "m", "k", "l", "p", "q"]) [0 ..]

    t_a u = TypeVar u (qualName a) []
    array_a u s = Array u s $ t_a mempty
    tp_a = TypeParamType Unlifted a mempty

    t_b u = TypeVar u (qualName b) []
    array_b u s = Array u s $ t_b mempty
    tp_b = TypeParamType Unlifted b mempty

    [sp_n, sp_m, sp_k, sp_l, sp_p, sp_q] = map (`TypeParamDim` mempty) [n, m, k, l, p, q]

    size = flip sizeFromName mempty . qualName
    shape = Shape . map size

    tuple_array u x y s =
      Array u s (Record (M.fromList $ zip tupleFieldNames [x, y]))

    arr x y = Scalar $ Arrow mempty Unnamed Observe x (RetType [] y)
    carr x y = Scalar $ Arrow mempty Unnamed Consume x (RetType [] y)

    array_ka u = Array u (Shape [sizeFromName (qualName k) mempty]) $ t_a mempty

    accType u t =
      TypeVar u (qualName (fst intrinsicAcc)) [TypeArgType t]

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
isBuiltinLoc :: (Located a) => a -> Bool
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
decImports ModTypeDec {} = []
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
        onDec (ModTypeDec sb) =
          M.singleton (modTypeName sb) (onModTypeExp (modTypeExp sb))
        onDec TypeDec {} = mempty
        onDec ValDec {} = mempty
        onDec (LocalDec d _) = onDec d
        onDec ImportDec {} = mempty

        onModTypeExp (ModTypeVar v _ _) = S.singleton $ qualLeaf v
        onModTypeExp (ModTypeParens e _) = onModTypeExp e
        onModTypeExp (ModTypeSpecs ss _) = foldMap onSpec ss
        onModTypeExp (ModTypeWith e _ _) = onModTypeExp e
        onModTypeExp (ModTypeArrow _ e1 e2 _) = onModTypeExp e1 <> onModTypeExp e2

        onSpec ValSpec {} = mempty
        onSpec TypeSpec {} = mempty
        onSpec TypeAbbrSpec {} = mempty
        onSpec (ModSpec vn e _ _) = S.singleton vn <> onModTypeExp e
        onSpec (IncludeSpec e _) = onModTypeExp e

    mtypes_used = foldMap onDec $ progDecs prog
      where
        onDec (OpenDec x _) = onModExp x
        onDec (ModDec md) =
          maybe mempty (onModTypeExp . fst) (modType md) <> onModExp (modExp md)
        onDec ModTypeDec {} = mempty
        onDec TypeDec {} = mempty
        onDec ValDec {} = mempty
        onDec LocalDec {} = mempty
        onDec ImportDec {} = mempty

        onModExp ModVar {} = mempty
        onModExp (ModParens p _) = onModExp p
        onModExp ModImport {} = mempty
        onModExp (ModDecs ds _) = mconcat $ map onDec ds
        onModExp (ModApply me1 me2 _ _ _) = onModExp me1 <> onModExp me2
        onModExp (ModAscript me se _ _) = onModExp me <> onModTypeExp se
        onModExp (ModLambda p r me _) =
          onModParam p <> maybe mempty (onModTypeExp . fst) r <> onModExp me

        onModParam = onModTypeExp . modParamType

        onModTypeExp (ModTypeVar v _ _) = S.singleton $ qualLeaf v
        onModTypeExp (ModTypeParens e _) = onModTypeExp e
        onModTypeExp ModTypeSpecs {} = mempty
        onModTypeExp (ModTypeWith e _ _) = onModTypeExp e
        onModTypeExp (ModTypeArrow _ e1 e2 _) = onModTypeExp e1 <> onModTypeExp e2

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
    holesInDec ModTypeDec {} = mempty
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

-- | Strip semantically irrelevant stuff from the top level of
-- expression.  This is used to provide a slightly fuzzy notion of
-- expression equality.
--
-- Ideally we'd implement unification on a simpler representation that
-- simply didn't allow us.
stripExp :: Exp -> Maybe Exp
stripExp (Parens e _) = stripExp e `mplus` Just e
stripExp (Assert _ e _ _) = stripExp e `mplus` Just e
stripExp (Attr _ e _) = stripExp e `mplus` Just e
stripExp (Ascript e _ _) = stripExp e `mplus` Just e
stripExp _ = Nothing

-- | All non-trivial subexpressions (as by stripExp) of some
-- expression, not including the expression itself.
subExps :: Exp -> [Exp]
subExps e
  | Just e' <- stripExp e = subExps e'
  | otherwise = astMap mapper e `execState` mempty
  where
    mapOnExp e'
      | Just e'' <- stripExp e' = mapOnExp e''
      | otherwise = do
          modify (e' :)
          astMap mapper e'
    mapper = identityMapper {mapOnExp}

similarSlices :: Slice -> Slice -> Maybe [(Exp, Exp)]
similarSlices slice1 slice2
  | length slice1 == length slice2 = do
      concat <$> zipWithM match slice1 slice2
  | otherwise = Nothing
  where
    match (DimFix e1) (DimFix e2) = Just [(e1, e2)]
    match (DimSlice a1 b1 c1) (DimSlice a2 b2 c2) =
      concat <$> sequence [pair (a1, a2), pair (b1, b2), pair (c1, c2)]
    match _ _ = Nothing
    pair (Nothing, Nothing) = Just []
    pair (Just x, Just y) = Just [(x, y)]
    pair _ = Nothing

-- | If these two expressions are structurally similar at top level as
-- sizes, produce their subexpressions (which are not necessarily
-- similar, but you can check for that!). This is the machinery
-- underlying expresssion unification. We assume that the expressions
-- have the same type.
similarExps :: Exp -> Exp -> Maybe [(Exp, Exp)]
similarExps e1 e2 | bareExp e1 == bareExp e2 = Just []
similarExps e1 e2 | Just e1' <- stripExp e1 = similarExps e1' e2
similarExps e1 e2 | Just e2' <- stripExp e2 = similarExps e1 e2'
similarExps (IntLit x _ _) (Literal v _) =
  case v of
    SignedValue (Int8Value y) | x == toInteger y -> Just []
    SignedValue (Int16Value y) | x == toInteger y -> Just []
    SignedValue (Int32Value y) | x == toInteger y -> Just []
    SignedValue (Int64Value y) | x == toInteger y -> Just []
    _ -> Nothing
similarExps
  (AppExp (BinOp (op1, _) _ (x1, _) (y1, _) _) _)
  (AppExp (BinOp (op2, _) _ (x2, _) (y2, _) _) _)
    | op1 == op2 = Just [(x1, x2), (y1, y2)]
similarExps (AppExp (Apply f1 args1 _) _) (AppExp (Apply f2 args2 _) _)
  | f1 == f2 = Just $ zip (map snd $ NE.toList args1) (map snd $ NE.toList args2)
similarExps (AppExp (Index arr1 slice1 _) _) (AppExp (Index arr2 slice2 _) _)
  | arr1 == arr2,
    length slice1 == length slice2 =
      similarSlices slice1 slice2
similarExps (TupLit es1 _) (TupLit es2 _)
  | length es1 == length es2 =
      Just $ zip es1 es2
similarExps (RecordLit fs1 _) (RecordLit fs2 _)
  | length fs1 == length fs2 =
      zipWithM onFields fs1 fs2
  where
    onFields (RecordFieldExplicit (L _ n1) fe1 _) (RecordFieldExplicit (L _ n2) fe2 _)
      | n1 == n2 = Just (fe1, fe2)
    onFields (RecordFieldImplicit (L _ vn1) ty1 _) (RecordFieldImplicit (L _ vn2) ty2 _) =
      Just (Var (qualName vn1) ty1 mempty, Var (qualName vn2) ty2 mempty)
    onFields _ _ = Nothing
similarExps (ArrayLit es1 _ _) (ArrayLit es2 _ _)
  | length es1 == length es2 =
      Just $ zip es1 es2
similarExps (Project field1 e1 _ _) (Project field2 e2 _ _)
  | field1 == field2 =
      Just [(e1, e2)]
similarExps (Negate e1 _) (Negate e2 _) =
  Just [(e1, e2)]
similarExps (Not e1 _) (Not e2 _) =
  Just [(e1, e2)]
similarExps (Constr n1 es1 _ _) (Constr n2 es2 _ _)
  | length es1 == length es2,
    n1 == n2 =
      Just $ zip es1 es2
similarExps (Update e1 slice1 e'1 _) (Update e2 slice2 e'2 _) =
  ([(e1, e2), (e'1, e'2)] ++) <$> similarSlices slice1 slice2
similarExps (RecordUpdate e1 names1 e'1 _ _) (RecordUpdate e2 names2 e'2 _ _)
  | names1 == names2 =
      Just [(e1, e2), (e'1, e'2)]
similarExps (OpSection op1 _ _) (OpSection op2 _ _)
  | op1 == op2 = Just []
similarExps (OpSectionLeft op1 _ x1 _ _ _) (OpSectionLeft op2 _ x2 _ _ _)
  | op1 == op2 = Just [(x1, x2)]
similarExps (OpSectionRight op1 _ x1 _ _ _) (OpSectionRight op2 _ x2 _ _ _)
  | op1 == op2 = Just [(x1, x2)]
similarExps (ProjectSection names1 _ _) (ProjectSection names2 _ _)
  | names1 == names2 = Just []
similarExps (IndexSection slice1 _ _) (IndexSection slice2 _ _) =
  similarSlices slice1 slice2
similarExps _ _ = Nothing

-- | Are these the same expression as per recursively invoking
-- 'similarExps'?
sameExp :: Exp -> Exp -> Bool
sameExp e1 e2
  | Just es <- similarExps e1 e2 =
      all (uncurry sameExp) es
  | otherwise = False

-- | An identifier with type- and aliasing information.
type Ident = IdentBase Info VName

-- | An index with type information.
type DimIndex = DimIndexBase Info VName

-- | A slice with type information.
type Slice = SliceBase Info VName

-- | An expression with type information.
type Exp = ExpBase Info VName

-- | An application expression with type information.
type AppExp = AppExpBase Info VName

-- | A pattern with type information.
type Pat = PatBase Info VName

-- | An constant declaration with type information.
type ValBind = ValBindBase Info VName

-- | A type binding with type information.
type TypeBind = TypeBindBase Info VName

-- | A type-checked module binding.
type ModBind = ModBindBase Info VName

-- | A type-checked module type binding.
type ModTypeBind = ModTypeBindBase Info VName

-- | A type-checked module expression.
type ModExp = ModExpBase Info VName

-- | A type-checked module parameter.
type ModParam = ModParamBase Info VName

-- | A type-checked module type expression.
type ModTypeExp = ModTypeExpBase Info VName

-- | A type-checked declaration.
type Dec = DecBase Info VName

-- | A type-checked specification.
type Spec = SpecBase Info VName

-- | An Futhark program with type information.
type Prog = ProgBase Info VName

-- | A known type arg with shape annotations.
type StructTypeArg = TypeArg Size

-- | A type-checked type parameter.
type TypeParam = TypeParamBase VName

-- | A known scalar type with no shape annotations.
type ScalarType = ScalarTypeBase ()

-- | A type-checked case (of a match expression).
type Case = CaseBase Info VName

-- | A type with no aliasing information but shape annotations.
type UncheckedType = TypeBase (Shape Name) ()

-- | An unchecked type expression.
type UncheckedTypeExp = TypeExp UncheckedExp Name

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
type UncheckedModTypeExp = ModTypeExpBase NoInfo Name

-- | A type parameter with no type annotations.
type UncheckedTypeParam = TypeParamBase Name

-- | A pattern with no type annotations.
type UncheckedPat = PatBase NoInfo Name

-- | A function declaration with no type annotations.
type UncheckedValBind = ValBindBase NoInfo Name

-- | A type binding with no type annotations.
type UncheckedTypeBind = TypeBindBase NoInfo Name

-- | A module type binding with no type annotations.
type UncheckedModTypeBind = ModTypeBindBase NoInfo Name

-- | A module binding with no type annotations.
type UncheckedModBind = ModBindBase NoInfo Name

-- | A declaration with no type annotations.
type UncheckedDec = DecBase NoInfo Name

-- | A spec with no type annotations.
type UncheckedSpec = SpecBase NoInfo Name

-- | A Futhark program with no type annotations.
type UncheckedProg = ProgBase NoInfo Name

-- | A case (of a match expression) with no type annotations.
type UncheckedCase = CaseBase NoInfo Name
