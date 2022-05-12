{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Type checker building blocks that do not involve unification.
module Language.Futhark.TypeChecker.Types
  ( checkTypeExp,
    renameRetType,
    subtypeOf,
    subuniqueOf,
    addAliasesFromType,
    checkForDuplicateNames,
    checkTypeParams,
    typeParamToArg,
    Subst (..),
    substFromAbbr,
    TypeSubs,
    Substitutable (..),
    substTypesAny,
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.List (find, foldl', sort, unzip4, (\\))
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty hiding ((<|>))
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Monad

addAliasesFromType :: StructType -> PatType -> PatType
addAliasesFromType (Array _ u1 et1 shape1) (Array als _ _ _) =
  Array als u1 et1 shape1
addAliasesFromType
  (Scalar (TypeVar _ u1 tv1 targs1))
  (Scalar (TypeVar als2 _ _ _)) =
    Scalar $ TypeVar als2 u1 tv1 targs1
addAliasesFromType (Scalar (Record ts1)) (Scalar (Record ts2))
  | length ts1 == length ts2,
    sort (M.keys ts1) == sort (M.keys ts2) =
      Scalar $ Record $ M.intersectionWith addAliasesFromType ts1 ts2
addAliasesFromType
  (Scalar (Arrow _ mn1 pt1 (RetType dims1 rt1)))
  (Scalar (Arrow as2 _ _ (RetType _ rt2))) =
    Scalar (Arrow as2 mn1 pt1 (RetType dims1 rt1'))
    where
      rt1' = addAliasesFromType rt1 rt2
addAliasesFromType (Scalar (Sum cs1)) (Scalar (Sum cs2))
  | length cs1 == length cs2,
    sort (M.keys cs1) == sort (M.keys cs2) =
      Scalar $ Sum $ M.intersectionWith (zipWith addAliasesFromType) cs1 cs2
addAliasesFromType (Scalar (Prim t)) _ = Scalar $ Prim t
addAliasesFromType t1 t2 =
  error $ "addAliasesFromType invalid args: " ++ show (t1, t2)

-- | @unifyTypes uf t1 t2@ attempts to unify @t1@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.
-- Uniqueness is unified with @uf@.  Assumes sizes already match, and
-- always picks the size of the leftmost type.
unifyTypesU ::
  (Monoid als, ArrayDim dim) =>
  (Uniqueness -> Uniqueness -> Maybe Uniqueness) ->
  TypeBase dim als ->
  TypeBase dim als ->
  Maybe (TypeBase dim als)
unifyTypesU uf (Array als1 u1 shape1 et1) (Array als2 u2 _shape2 et2) =
  Array (als1 <> als2) <$> uf u1 u2
    <*> pure shape1
    <*> unifyScalarTypes uf et1 et2
unifyTypesU uf (Scalar t1) (Scalar t2) = Scalar <$> unifyScalarTypes uf t1 t2
unifyTypesU _ _ _ = Nothing

unifyScalarTypes ::
  (Monoid als, ArrayDim dim) =>
  (Uniqueness -> Uniqueness -> Maybe Uniqueness) ->
  ScalarTypeBase dim als ->
  ScalarTypeBase dim als ->
  Maybe (ScalarTypeBase dim als)
unifyScalarTypes _ (Prim t1) (Prim t2)
  | t1 == t2 = Just $ Prim t1
  | otherwise = Nothing
unifyScalarTypes uf (TypeVar als1 u1 tv1 targs1) (TypeVar als2 u2 tv2 targs2)
  | tv1 == tv2 = do
      u3 <- uf u1 u2
      targs3 <- zipWithM unifyTypeArgs targs1 targs2
      Just $ TypeVar (als1 <> als2) u3 tv1 targs3
  | otherwise = Nothing
  where
    unifyTypeArgs (TypeArgDim d1 loc) (TypeArgDim _d2 _) =
      pure $ TypeArgDim d1 loc
    unifyTypeArgs (TypeArgType t1 loc) (TypeArgType t2 _) =
      TypeArgType <$> unifyTypesU uf t1 t2 <*> pure loc
    unifyTypeArgs _ _ =
      Nothing
unifyScalarTypes uf (Record ts1) (Record ts2)
  | length ts1 == length ts2,
    sort (M.keys ts1) == sort (M.keys ts2) =
      Record
        <$> traverse
          (uncurry (unifyTypesU uf))
          (M.intersectionWith (,) ts1 ts2)
unifyScalarTypes
  uf
  (Arrow as1 mn1 t1 (RetType dims1 t1'))
  (Arrow as2 _ t2 (RetType _ t2')) =
    Arrow (as1 <> as2) mn1 <$> unifyTypesU (flip uf) t1 t2
      <*> (RetType dims1 <$> unifyTypesU uf t1' t2')
unifyScalarTypes uf (Sum cs1) (Sum cs2)
  | length cs1 == length cs2,
    sort (M.keys cs1) == sort (M.keys cs2) =
      Sum
        <$> traverse
          (uncurry (zipWithM (unifyTypesU uf)))
          (M.intersectionWith (,) cs1 cs2)
unifyScalarTypes _ _ _ = Nothing

-- | @x \`subtypeOf\` y@ is true if @x@ is a subtype of @y@ (or equal
-- to @y@), meaning @x@ is valid whenever @y@ is.  Ignores sizes.
-- Mostly used for checking uniqueness.
subtypeOf :: TypeBase () () -> TypeBase () () -> Bool
subtypeOf t1 t2 = isJust $ unifyTypesU unifyUniqueness (toStruct t1) (toStruct t2)
  where
    unifyUniqueness u2 u1 = if u2 `subuniqueOf` u1 then Just u1 else Nothing

-- | @x `subuniqueOf` y@ is true if @x@ is not less unique than @y@.
subuniqueOf :: Uniqueness -> Uniqueness -> Bool
subuniqueOf Nonunique Unique = False
subuniqueOf _ _ = True

-- | Ensure that the dimensions of the RetType are unique by
-- generating new names for them.  This is to avoid name capture.
renameRetType :: MonadTypeChecker m => StructRetType -> m StructRetType
renameRetType (RetType dims st)
  | dims /= mempty = do
      dims' <- mapM newName dims
      let m = M.fromList $ zip dims $ map (SizeSubst . NamedDim . qualName) dims'
          st' = applySubst (`M.lookup` m) st
      pure $ RetType dims' st'
  | otherwise =
      pure $ RetType dims st

evalTypeExp ::
  MonadTypeChecker m =>
  TypeExp Name ->
  m (TypeExp VName, [VName], StructRetType, Liftedness)
evalTypeExp (TEVar name loc) = do
  (name', ps, t, l) <- lookupType loc name
  t' <- renameRetType t
  case ps of
    [] -> pure (TEVar name' loc, [], t', l)
    _ ->
      typeError loc mempty $
        "Type constructor" <+> pquote (spread (ppr name : map ppr ps))
          <+> "used without any arguments."
--
evalTypeExp (TETuple ts loc) = do
  (ts', svars, ts_s, ls) <- unzip4 <$> mapM evalTypeExp ts
  pure
    ( TETuple ts' loc,
      mconcat svars,
      RetType (foldMap retDims ts_s) $ Scalar $ tupleRecord $ map retType ts_s,
      foldl' max Unlifted ls
    )
--
evalTypeExp t@(TERecord fs loc) = do
  -- Check for duplicate field names.
  let field_names = map fst fs
  unless (sort field_names == sort (nubOrd field_names)) $
    typeError loc mempty $ "Duplicate record fields in" <+> ppr t <> "."

  checked <- traverse evalTypeExp $ M.fromList fs
  let fs' = fmap (\(x, _, _, _) -> x) checked
      fs_svars = foldMap (\(_, y, _, _) -> y) checked
      ts_s = fmap (\(_, _, z, _) -> z) checked
      ls = fmap (\(_, _, _, v) -> v) checked
  pure
    ( TERecord (M.toList fs') loc,
      fs_svars,
      RetType (foldMap retDims ts_s) $ Scalar $ Record $ M.map retType ts_s,
      foldl' max Unlifted ls
    )
--
evalTypeExp (TEArray d t loc) = do
  (d_svars, d', d'') <- checkDimExp d
  (t', svars, RetType dims st, l) <- evalTypeExp t
  case (l, arrayOf Nonunique (ShapeDecl [d'']) st) of
    (Unlifted, st') ->
      pure
        ( TEArray d' t' loc,
          svars,
          RetType (d_svars ++ dims) st',
          Unlifted
        )
    (SizeLifted, _) ->
      typeError loc mempty $
        "Cannot create array with elements of size-lifted type" <+> pquote (ppr t)
          <+/> "(might cause irregular array)."
    (Lifted, _) ->
      typeError loc mempty $
        "Cannot create array with elements of lifted type" <+> pquote (ppr t)
          <+/> "(might contain function)."
  where
    checkDimExp DimExpAny = do
      dv <- newTypeName "d"
      pure ([dv], DimExpAny, NamedDim $ qualName dv)
    checkDimExp (DimExpConst k dloc) =
      pure ([], DimExpConst k dloc, ConstDim k)
    checkDimExp (DimExpNamed v dloc) = do
      v' <- checkNamedDim loc v
      pure ([], DimExpNamed v' dloc, NamedDim v')
--
evalTypeExp (TEUnique t loc) = do
  (t', svars, RetType dims st, l) <- evalTypeExp t
  unless (mayContainArray st) $
    warn loc $ "Declaring" <+> pquote (ppr st) <+> "as unique has no effect."
  pure (TEUnique t' loc, svars, RetType dims $ st `setUniqueness` Unique, l)
  where
    mayContainArray (Scalar Prim {}) = False
    mayContainArray Array {} = True
    mayContainArray (Scalar (Record fs)) = any mayContainArray fs
    mayContainArray (Scalar TypeVar {}) = True
    mayContainArray (Scalar Arrow {}) = False
    mayContainArray (Scalar (Sum cs)) = (any . any) mayContainArray cs
--
evalTypeExp (TEArrow (Just v) t1 t2 loc) = do
  (t1', svars1, RetType dims1 st1, _) <- evalTypeExp t1
  bindSpaced [(Term, v)] $ do
    v' <- checkName Term v loc
    bindVal v' (BoundV [] st1) $ do
      (t2', svars2, RetType dims2 st2, _) <- evalTypeExp t2
      pure
        ( TEArrow (Just v') t1' t2' loc,
          svars1 ++ dims1 ++ svars2,
          RetType [] $ Scalar $ Arrow mempty (Named v') st1 (RetType dims2 st2),
          Lifted
        )
--
evalTypeExp (TEArrow Nothing t1 t2 loc) = do
  (t1', svars1, RetType dims1 st1, _) <- evalTypeExp t1
  (t2', svars2, RetType dims2 st2, _) <- evalTypeExp t2
  pure
    ( TEArrow Nothing t1' t2' loc,
      svars1 ++ dims1 ++ svars2,
      RetType [] $ Scalar $ Arrow mempty Unnamed st1 $ RetType dims2 st2,
      Lifted
    )
--
evalTypeExp (TEDim dims t loc) = do
  bindSpaced (map (Term,) dims) $ do
    dims' <- mapM (flip (checkName Term) loc) dims
    bindDims dims' $ do
      (t', svars, RetType t_dims st, l) <- evalTypeExp t
      let (witnessed, _) = determineSizeWitnesses st
      case find (`S.notMember` witnessed) dims' of
        Just d ->
          typeError loc mempty . withIndexLink "unused-existential" $
            "Existential size " <> pquote (pprName d)
              <> " not used as array size."
        Nothing ->
          pure
            ( TEDim dims' t' loc,
              svars,
              RetType (dims' ++ t_dims) st,
              max l SizeLifted
            )
  where
    bindDims [] m = m
    bindDims (d : ds) m =
      bindVal d (BoundV [] $ Scalar $ Prim $ Signed Int64) $ bindDims ds m
--
evalTypeExp t@(TESum cs loc) = do
  let constructors = map fst cs
  unless (sort constructors == sort (nubOrd constructors)) $
    typeError loc mempty $ "Duplicate constructors in" <+> ppr t

  unless (length constructors < 256) $
    typeError loc mempty "Sum types must have less than 256 constructors."

  checked <- (traverse . traverse) evalTypeExp $ M.fromList cs
  let cs' = (fmap . fmap) (\(x, _, _, _) -> x) checked
      cs_svars = (foldMap . foldMap) (\(_, y, _, _) -> y) checked
      ts_s = (fmap . fmap) (\(_, _, z, _) -> z) checked
      ls = (concatMap . fmap) (\(_, _, _, v) -> v) checked
  pure
    ( TESum (M.toList cs') loc,
      cs_svars,
      RetType (foldMap (foldMap retDims) ts_s) $
        Scalar $ Sum $ M.map (map retType) ts_s,
      foldl' max Unlifted ls
    )
evalTypeExp ote@TEApply {} = do
  (tname, tname_loc, targs) <- rootAndArgs ote
  (tname', ps, tname_t, l) <- lookupType tloc tname
  RetType t_dims t <- renameRetType tname_t
  if length ps /= length targs
    then
      typeError tloc mempty $
        "Type constructor" <+> pquote (ppr tname) <+> "requires" <+> ppr (length ps)
          <+> "arguments, but provided"
          <+> ppr (length targs) <> "."
    else do
      (targs', dims, substs) <- unzip3 <$> zipWithM checkArgApply ps targs
      pure
        ( foldl (\x y -> TEApply x y tloc) (TEVar tname' tname_loc) targs',
          [],
          RetType (t_dims ++ mconcat dims) $ applySubst (`M.lookup` mconcat substs) t,
          l
        )
  where
    tloc = srclocOf ote

    rootAndArgs :: MonadTypeChecker m => TypeExp Name -> m (QualName Name, SrcLoc, [TypeArgExp Name])
    rootAndArgs (TEVar qn loc) = pure (qn, loc, [])
    rootAndArgs (TEApply op arg _) = do
      (op', loc, args) <- rootAndArgs op
      pure (op', loc, args ++ [arg])
    rootAndArgs te' =
      typeError (srclocOf te') mempty $
        "Type" <+> pquote (ppr te') <+> "is not a type constructor."

    checkArgApply (TypeParamDim pv _) (TypeArgExpDim (DimExpNamed v dloc) loc) = do
      v' <- checkNamedDim loc v
      pure
        ( TypeArgExpDim (DimExpNamed v' dloc) loc,
          [],
          M.singleton pv $ SizeSubst $ NamedDim v'
        )
    checkArgApply (TypeParamDim pv _) (TypeArgExpDim (DimExpConst x dloc) loc) =
      pure
        ( TypeArgExpDim (DimExpConst x dloc) loc,
          [],
          M.singleton pv $ SizeSubst $ ConstDim x
        )
    checkArgApply (TypeParamDim pv _) (TypeArgExpDim DimExpAny loc) = do
      d <- newTypeName "d"
      pure
        ( TypeArgExpDim DimExpAny loc,
          [d],
          M.singleton pv $ SizeSubst $ NamedDim $ qualName d
        )
    checkArgApply (TypeParamType _ pv _) (TypeArgExpType te) = do
      (te', svars, RetType dims st, _) <- evalTypeExp te
      pure
        ( TypeArgExpType te',
          svars ++ dims,
          M.singleton pv $ Subst [] $ RetType [] st
        )
    checkArgApply p a =
      typeError tloc mempty $
        "Type argument" <+> ppr a
          <+> "not valid for a type parameter"
          <+> ppr p <> "."

-- | Check a type expression, producing:
--
-- * The checked expression.
-- * Size variables for any anonymous sizes in the expression.
-- * The elaborated type.
-- * The liftedness of the type.
checkTypeExp ::
  MonadTypeChecker m =>
  TypeExp Name ->
  m (TypeExp VName, [VName], StructRetType, Liftedness)
checkTypeExp te = do
  checkForDuplicateNamesInType te
  evalTypeExp te

-- | Check for duplication of names inside a binding group.
checkForDuplicateNames ::
  MonadTypeChecker m => [UncheckedTypeParam] -> [UncheckedPat] -> m ()
checkForDuplicateNames tps pats = (`evalStateT` mempty) $ do
  mapM_ checkTypeParam tps
  mapM_ checkPat pats
  where
    checkTypeParam (TypeParamType _ v loc) = seen Type v loc
    checkTypeParam (TypeParamDim v loc) = seen Term v loc

    checkPat (Id v _ loc) = seen Term v loc
    checkPat (PatParens p _) = checkPat p
    checkPat (PatAttr _ p _) = checkPat p
    checkPat Wildcard {} = pure ()
    checkPat (TuplePat ps _) = mapM_ checkPat ps
    checkPat (RecordPat fs _) = mapM_ (checkPat . snd) fs
    checkPat (PatAscription p _ _) = checkPat p
    checkPat PatLit {} = pure ()
    checkPat (PatConstr _ _ ps _) = mapM_ checkPat ps

    seen ns v loc = do
      already <- gets $ M.lookup (ns, v)
      case already of
        Just prev_loc ->
          lift $
            typeError loc mempty $
              "Name" <+> pquote (ppr v) <+> "also bound at"
                <+> text (locStr prev_loc) <> "."
        Nothing ->
          modify $ M.insert (ns, v) loc

-- | Check whether the type contains arrow types that define the same
-- parameter.  These might also exist further down, but that's not
-- really a problem - we mostly do this checking to help the user,
-- since it is likely an error, but it's easy to assign a semantics to
-- it (normal name shadowing).
checkForDuplicateNamesInType ::
  MonadTypeChecker m =>
  TypeExp Name ->
  m ()
checkForDuplicateNamesInType = check mempty
  where
    bad v loc prev_loc =
      typeError loc mempty $
        text "Name" <+> pquote (ppr v)
          <+> "also bound at"
          <+> text (locStr prev_loc) <> "."

    check seen (TEArrow (Just v) t1 t2 loc)
      | Just prev_loc <- M.lookup v seen =
          bad v loc prev_loc
      | otherwise =
          check seen' t1 >> check seen' t2
      where
        seen' = M.insert v loc seen
    check seen (TEArrow Nothing t1 t2 _) =
      check seen t1 >> check seen t2
    check seen (TETuple ts _) = mapM_ (check seen) ts
    check seen (TERecord fs _) = mapM_ (check seen . snd) fs
    check seen (TEUnique t _) = check seen t
    check seen (TESum cs _) = mapM_ (mapM (check seen) . snd) cs
    check seen (TEApply t1 (TypeArgExpType t2) _) =
      check seen t1 >> check seen t2
    check seen (TEApply t1 TypeArgExpDim {} _) =
      check seen t1
    check seen (TEDim (v : vs) t loc)
      | Just prev_loc <- M.lookup v seen =
          bad v loc prev_loc
      | otherwise =
          check (M.insert v loc seen) (TEDim vs t loc)
    check seen (TEDim [] t _) =
      check seen t
    check _ TEArray {} = pure ()
    check _ TEVar {} = pure ()

-- | @checkTypeParams ps m@ checks the type parameters @ps@, then
-- invokes the continuation @m@ with the checked parameters, while
-- extending the monadic name map with @ps@.
checkTypeParams ::
  MonadTypeChecker m =>
  [TypeParamBase Name] ->
  ([TypeParamBase VName] -> m a) ->
  m a
checkTypeParams ps m =
  bindSpaced (map typeParamSpace ps) $
    m =<< evalStateT (mapM checkTypeParam ps) mempty
  where
    typeParamSpace (TypeParamDim pv _) = (Term, pv)
    typeParamSpace (TypeParamType _ pv _) = (Type, pv)

    checkParamName ns v loc = do
      seen <- gets $ M.lookup (ns, v)
      case seen of
        Just prev ->
          lift $
            typeError loc mempty $
              text "Type parameter" <+> pquote (ppr v)
                <+> "previously defined at"
                <+> text (locStr prev) <> "."
        Nothing -> do
          modify $ M.insert (ns, v) loc
          lift $ checkName ns v loc

    checkTypeParam (TypeParamDim pv loc) =
      TypeParamDim <$> checkParamName Term pv loc <*> pure loc
    checkTypeParam (TypeParamType l pv loc) =
      TypeParamType l <$> checkParamName Type pv loc <*> pure loc

-- | Construct a type argument corresponding to a type parameter.
typeParamToArg :: TypeParam -> StructTypeArg
typeParamToArg (TypeParamDim v ploc) =
  TypeArgDim (NamedDim $ qualName v) ploc
typeParamToArg (TypeParamType _ v ploc) =
  TypeArgType (Scalar $ TypeVar () Nonunique (typeName v) []) ploc

-- | A type substitution may be a substitution or a yet-unknown
-- substitution (but which is certainly an overloaded primitive
-- type!).  The latter is used to remove aliases from types that are
-- yet-unknown but that we know cannot carry aliases (see issue #682).
data Subst t = Subst [TypeParam] t | PrimSubst | SizeSubst (DimDecl VName)
  deriving (Show)

instance Pretty t => Pretty (Subst t) where
  ppr (Subst [] t) = ppr t
  ppr (Subst tps t) = mconcat (map ppr tps) <> colon <+> ppr t
  ppr PrimSubst = "#<primsubst>"
  ppr (SizeSubst d) = ppr d

-- | Create a type substitution corresponding to a type binding.
substFromAbbr :: TypeBinding -> Subst StructRetType
substFromAbbr (TypeAbbr _ ps rt) = Subst ps rt

-- | Substitutions to apply in a type.
type TypeSubs = VName -> Maybe (Subst StructRetType)

instance Functor Subst where
  fmap f (Subst ps t) = Subst ps $ f t
  fmap _ PrimSubst = PrimSubst
  fmap _ (SizeSubst v) = SizeSubst v

-- | Class of types which allow for substitution of types with no
-- annotations for type variable names.
class Substitutable a where
  applySubst :: TypeSubs -> a -> a

instance Substitutable (RetTypeBase (DimDecl VName) ()) where
  applySubst f (RetType dims t) =
    let RetType more_dims t' = substTypesRet f t
     in RetType (dims ++ more_dims) t'

instance Substitutable (RetTypeBase (DimDecl VName) Aliasing) where
  applySubst f (RetType dims t) =
    let RetType more_dims t' = substTypesRet f' t
     in RetType (dims ++ more_dims) t'
    where
      f' = fmap (fmap (second (const mempty))) . f

instance Substitutable (TypeBase (DimDecl VName) ()) where
  applySubst = substTypesAny

instance Substitutable (TypeBase (DimDecl VName) Aliasing) where
  applySubst = substTypesAny . (fmap (fmap (second (const mempty))) .)

instance Substitutable (DimDecl VName) where
  applySubst f (NamedDim (QualName _ v))
    | Just (SizeSubst d) <- f v = d
  applySubst _ d = d

instance Substitutable d => Substitutable (ShapeDecl d) where
  applySubst f = fmap $ applySubst f

instance Substitutable Pat where
  applySubst f = runIdentity . astMap mapper
    where
      mapper =
        ASTMapper
          { mapOnExp = pure,
            mapOnName = pure,
            mapOnQualName = pure,
            mapOnStructType = pure . applySubst f,
            mapOnPatType = pure . applySubst f,
            mapOnStructRetType = pure . applySubst f,
            mapOnPatRetType = pure . applySubst f
          }

applyType ::
  Monoid als =>
  [TypeParam] ->
  TypeBase (DimDecl VName) als ->
  [StructTypeArg] ->
  TypeBase (DimDecl VName) als
applyType ps t args = substTypesAny (`M.lookup` substs) t
  where
    substs = M.fromList $ zipWith mkSubst ps args
    -- We are assuming everything has already been type-checked for correctness.
    mkSubst (TypeParamDim pv _) (TypeArgDim d _) =
      (pv, SizeSubst d)
    mkSubst (TypeParamType _ pv _) (TypeArgType at _) =
      (pv, Subst [] $ RetType [] $ second mempty at)
    mkSubst p a =
      error $ "applyType mkSubst: cannot substitute " ++ pretty a ++ " for " ++ pretty p

substTypesRet ::
  Monoid as =>
  (VName -> Maybe (Subst (RetTypeBase (DimDecl VName) as))) ->
  TypeBase (DimDecl VName) as ->
  RetTypeBase (DimDecl VName) as
substTypesRet lookupSubst ot =
  uncurry (flip RetType) $ runState (onType ot) []
  where
    -- In case we are substituting the same RetType in multiple
    -- places, we must ensure each instance is given distinct
    -- dimensions.  E.g. substituting 'a ↦ ?[n].[n]bool' into '(a,a)'
    -- should give '?[n][m].([n]bool,[m]bool)'.
    --
    -- XXX: the size names we invent here not globally unique.  This
    -- is _probably_ not a problem, since substituting types with
    -- outermost non-null existential sizes is done only when type
    -- checking modules.
    freshDims (RetType [] t) = pure $ RetType [] t
    freshDims (RetType ext t) = do
      seen_ext <- get
      if not $ any (`elem` seen_ext) ext
        then pure $ RetType ext t
        else do
          let start = maximum $ map baseTag seen_ext
              ext' = zipWith VName (map baseName ext) [start + 1 ..]
              extsubsts = M.fromList $ zip ext $ map (SizeSubst . NamedDim . qualName) ext'
              RetType [] t' = substTypesRet (`M.lookup` extsubsts) t
          pure $ RetType ext' t'

    onType ::
      forall as.
      Monoid as =>
      TypeBase (DimDecl VName) as ->
      State [VName] (TypeBase (DimDecl VName) as)

    onType (Array als u shape et) = do
      t <- arrayOf u (applySubst lookupSubst' shape) <$> onType (Scalar et)
      pure $ t `setAliases` als
    onType (Scalar (Prim t)) = pure $ Scalar $ Prim t
    onType (Scalar (TypeVar als u v targs)) = do
      targs' <- mapM subsTypeArg targs
      case lookupSubst $ qualLeaf (qualNameFromTypeName v) of
        Just (Subst ps rt) -> do
          RetType ext t <- freshDims rt
          modify (ext ++)
          pure $
            applyType ps (t `setAliases` mempty) targs'
              `setUniqueness` u `addAliases` (<> als)
        Just PrimSubst ->
          pure $ Scalar $ TypeVar mempty u v targs'
        _ ->
          pure $ Scalar $ TypeVar als u v targs'
    onType (Scalar (Record ts)) =
      Scalar . Record <$> traverse onType ts
    onType (Scalar (Arrow als v t1 t2)) =
      Scalar <$> (Arrow als v <$> onType t1 <*> onRetType t2)
    onType (Scalar (Sum ts)) =
      Scalar . Sum <$> traverse (traverse onType) ts

    onRetType (RetType dims t) = do
      ext <- get
      let (t', ext') = runState (onType t) ext
          new_ext = ext' \\ ext
      case t of
        Scalar Arrow {} -> do
          put ext'
          pure $ RetType dims t'
        _ ->
          pure $ RetType (new_ext <> dims) t'

    subsTypeArg (TypeArgType t loc) = do
      let RetType dims t' = substTypesRet lookupSubst' t
      modify (dims ++)
      pure $ TypeArgType t' loc
    subsTypeArg (TypeArgDim v loc) =
      pure $ TypeArgDim (applySubst lookupSubst' v) loc

    lookupSubst' = fmap (fmap $ second (const ())) . lookupSubst

-- | Perform substitutions, from type names to types, on a type. Works
-- regardless of what shape and uniqueness information is attached to the type.
substTypesAny ::
  Monoid as =>
  (VName -> Maybe (Subst (RetTypeBase (DimDecl VName) as))) ->
  TypeBase (DimDecl VName) as ->
  TypeBase (DimDecl VName) as
substTypesAny lookupSubst ot =
  case substTypesRet lookupSubst ot of
    RetType [] ot' -> ot'
    RetType dims ot' ->
      -- XXX HACK FIXME: turn any sizes that propagate to the top into
      -- AnyDim.  This should _never_ happen during type-checking, but
      -- may happen as we substitute types during monomorphisation and
      -- defunctorisation later on. See Note [AnyDim]
      let toAny (NamedDim v)
            | qualLeaf v `elem` dims = AnyDim Nothing
          toAny d = d
       in first toAny ot'

-- Note [AnyDim]
--
-- Consider a program:
--
-- module m : { type~ t } = { type~ t = ?[n].[n]bool }
-- let f (x: m.t) (y: m.t) = 0
--
-- After defunctorisation (and inlining the definitions of types), we
-- want this:
--
-- let f [n][m] (x: [n]bool) (y: [m]bool) = 0
--
-- But this means that defunctorisation would need to redo some amount
-- of size inference.  Not so complicated in the example above, but
-- what if loops and branches are involved?
--
-- So instead, what defunctorisation actually does is produce this:
--
-- let f (x: []bool) (y: []bool) = 0
--
-- I.e. we put in empty dimensions (AnyDim), which are much later
-- turned into distinct sizes in Futhark.Internalise.Exps.  This will
-- result in unnecessary dynamic size checks, which will hopefully be
-- optimised away.
--
-- Important: The type checker will _never_ produce programs with
-- AnyDim, but unfortunately some of the compilation steps
-- (defunctorisation, monomorphisation, defunctionalisation) will do
-- so.  Similarly, the core language is also perfectly well behaved.
--
-- Example with monomorphisation:
--
-- let f '~a (b: bool) (x: () -> a) (y: () -> a) : a = if b then x () else y ()
-- let g = f true (\() -> [1]) (\() -> [1,2])
--
-- This should produce:
--
-- let f (b: bool) (x: () -> ?[n].[n]i32) (y: () -> ?[m].[m]i32) : ?[k].[k]i32 =
--   if b then x () else y ()
--
-- Not so easy!  Again, what we actually produce is
--
-- let f (b: bool) (x: () -> []i32) (y: () -> []i32) : []i32 =
--   if b then x () else y ()
