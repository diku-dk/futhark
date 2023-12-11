-- | Type checker building blocks that do not involve unification.
module Language.Futhark.TypeChecker.Types
  ( checkTypeExp,
    renameRetType,
    checkForDuplicateNames,
    checkTypeParams,
    typeParamToArg,
    Subst (..),
    substFromAbbr,
    TypeSubs,
    Substitutable (..),
    substTypesAny,

    -- * Witnesses
    mustBeExplicitInType,
    mustBeExplicitInBinding,
    determineSizeWitnesses,
  )
where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.List (find, foldl', sort, unzip4, (\\))
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Monad

mustBeExplicitAux :: StructType -> M.Map VName Bool
mustBeExplicitAux t =
  execState (traverseDims onDim t) mempty
  where
    onDim bound _ (Var d _ _)
      | qualLeaf d `S.member` bound =
          modify $ \s -> M.insertWith (&&) (qualLeaf d) False s
    onDim _ PosImmediate (Var d _ _) =
      modify $ \s -> M.insertWith (&&) (qualLeaf d) False s
    onDim _ _ e =
      modify $ flip (S.foldr (\v -> M.insertWith (&&) v True)) $ fvVars $ freeInExp e

-- | Determine which of the sizes in a type are used as sizes outside
-- of functions in the type, and which are not.  The former are said
-- to be "witnessed" by this type, while the latter are not.  In
-- practice, the latter means that the actual sizes must come from
-- somewhere else.
determineSizeWitnesses :: StructType -> (S.Set VName, S.Set VName)
determineSizeWitnesses t =
  bimap (S.fromList . M.keys) (S.fromList . M.keys) $
    M.partition not $
      mustBeExplicitAux t

-- | Figure out which of the sizes in a binding type must be passed
-- explicitly, because their first use is as something else than just
-- an array dimension.
mustBeExplicitInBinding :: StructType -> S.Set VName
mustBeExplicitInBinding bind_t =
  let (ts, ret) = unfoldFunType bind_t
      alsoRet = M.unionWith (&&) $ M.fromList $ map (,True) (S.toList (fvVars (freeInType ret)))
   in S.fromList $ M.keys $ M.filter id $ alsoRet $ foldl' onType mempty $ map toStruct ts
  where
    onType uses t = uses <> mustBeExplicitAux t -- Left-biased union.

-- | Figure out which of the sizes in a parameter type must be passed
-- explicitly, because their first use is as something else than just
-- an array dimension.
mustBeExplicitInType :: StructType -> S.Set VName
mustBeExplicitInType = snd . determineSizeWitnesses

-- | Ensure that the dimensions of the RetType are unique by
-- generating new names for them.  This is to avoid name capture.
renameRetType :: (MonadTypeChecker m) => ResRetType -> m ResRetType
renameRetType (RetType dims st)
  | dims /= mempty = do
      dims' <- mapM newName dims
      let mkSubst = ExpSubst . flip sizeFromName mempty . qualName
          m = M.fromList . zip dims $ map mkSubst dims'
          st' = applySubst (`M.lookup` m) st
      pure $ RetType dims' st'
  | otherwise =
      pure $ RetType dims st

evalTypeExp ::
  (MonadTypeChecker m) =>
  TypeExp NoInfo Name ->
  m (TypeExp Info VName, [VName], ResRetType, Liftedness)
evalTypeExp (TEVar name loc) = do
  (name', ps, t, l) <- lookupType loc name
  t' <- renameRetType $ toResRet Nonunique t
  case ps of
    [] -> pure (TEVar name' loc, [], t', l)
    _ ->
      typeError loc mempty $
        "Type constructor"
          <+> dquotes (hsep (pretty name : map pretty ps))
          <+> "used without any arguments."
--
evalTypeExp (TEParens te loc) = do
  (te', svars, ts, ls) <- evalTypeExp te
  pure (TEParens te' loc, svars, ts, ls)
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
    typeError loc mempty $
      "Duplicate record fields in" <+> pretty t <> "."

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
  (d_svars, d', d'') <- checkSizeExp d
  (t', svars, RetType dims st, l) <- evalTypeExp t
  case (l, arrayOfWithAliases Nonunique (Shape [d'']) st) of
    (Unlifted, st') ->
      pure
        ( TEArray d' t' loc,
          svars,
          RetType (d_svars ++ dims) st',
          Unlifted
        )
    (SizeLifted, _) ->
      typeError loc mempty $
        "Cannot create array with elements of size-lifted type"
          <+> dquotes (pretty t)
          <+> "(might cause irregular array)."
    (Lifted, _) ->
      typeError loc mempty $
        "Cannot create array with elements of lifted type"
          <+> dquotes (pretty t)
          <+> "(might contain function)."
  where
    checkSizeExp (SizeExpAny dloc) = do
      dv <- newTypeName "d"
      pure ([dv], SizeExpAny dloc, sizeFromName (qualName dv) dloc)
    checkSizeExp (SizeExp e dloc) = do
      e' <- checkExpForSize e
      pure ([], SizeExp e' dloc, e')
--
evalTypeExp (TEUnique t loc) = do
  (t', svars, RetType dims st, l) <- evalTypeExp t
  unless (mayContainArray st) $
    warn loc $
      "Declaring" <+> dquotes (pretty st) <+> "as unique has no effect."
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
    bindVal v' (BoundV [] $ toStruct st1) $ do
      (t2', svars2, RetType dims2 st2, _) <- evalTypeExp t2
      pure
        ( TEArrow (Just v') t1' t2' loc,
          svars1 ++ dims1 ++ svars2,
          RetType [] $ Scalar $ Arrow Nonunique (Named v') (diet $ resToParam st1) (toStruct st1) (RetType dims2 st2),
          Lifted
        )
--
evalTypeExp (TEArrow Nothing t1 t2 loc) = do
  (t1', svars1, RetType dims1 st1, _) <- evalTypeExp t1
  (t2', svars2, RetType dims2 st2, _) <- evalTypeExp t2
  pure
    ( TEArrow Nothing t1' t2' loc,
      svars1 ++ dims1 ++ svars2,
      RetType [] . Scalar $
        Arrow Nonunique Unnamed (diet $ resToParam st1) (toStruct st1) $
          RetType dims2 st2,
      Lifted
    )
--
evalTypeExp (TEDim dims t loc) = do
  bindSpaced (map (Term,) dims) $ do
    dims' <- mapM (flip (checkName Term) loc) dims
    bindDims dims' $ do
      (t', svars, RetType t_dims st, l) <- evalTypeExp t
      let (witnessed, _) = determineSizeWitnesses $ toStruct st
      case find (`S.notMember` witnessed) dims' of
        Just d ->
          typeError loc mempty . withIndexLink "unused-existential" $
            "Existential size "
              <> dquotes (prettyName d)
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
    typeError loc mempty $
      "Duplicate constructors in" <+> pretty t

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
        Scalar $
          Sum $
            M.map (map retType) ts_s,
      foldl' max Unlifted ls
    )
evalTypeExp ote@TEApply {} = do
  (tname, tname_loc, targs) <- rootAndArgs ote
  (tname', ps, tname_t, l) <- lookupType tloc tname
  RetType t_dims t <- renameRetType $ toResRet Nonunique tname_t
  if length ps /= length targs
    then
      typeError tloc mempty $
        "Type constructor"
          <+> dquotes (pretty tname)
          <+> "requires"
          <+> pretty (length ps)
          <+> "arguments, but provided"
          <+> pretty (length targs)
          <> "."
    else do
      (targs', dims, substs) <- unzip3 <$> zipWithM checkArgApply ps targs
      pure
        ( foldl (\x y -> TEApply x y tloc) (TEVar tname' tname_loc) targs',
          [],
          RetType (t_dims ++ mconcat dims) $
            applySubst (`M.lookup` mconcat substs) t,
          l
        )
  where
    tloc = srclocOf ote

    rootAndArgs ::
      (MonadTypeChecker m) =>
      TypeExp NoInfo Name ->
      m (QualName Name, SrcLoc, [TypeArgExp NoInfo Name])
    rootAndArgs (TEVar qn loc) = pure (qn, loc, [])
    rootAndArgs (TEApply op arg _) = do
      (op', loc, args) <- rootAndArgs op
      pure (op', loc, args ++ [arg])
    rootAndArgs te' =
      typeError (srclocOf te') mempty $
        "Type" <+> dquotes (pretty te') <+> "is not a type constructor."

    checkSizeExp (SizeExp e dloc) = do
      e' <- checkExpForSize e
      pure
        ( TypeArgExpSize (SizeExp e' dloc),
          [],
          ExpSubst e'
        )
    checkSizeExp (SizeExpAny loc) = do
      d <- newTypeName "d"
      pure
        ( TypeArgExpSize (SizeExpAny loc),
          [d],
          ExpSubst $ sizeFromName (qualName d) loc
        )

    checkArgApply (TypeParamDim pv _) (TypeArgExpSize d) = do
      (d', svars, subst) <- checkSizeExp d
      pure (d', svars, M.singleton pv subst)
    checkArgApply (TypeParamType _ pv _) (TypeArgExpType te) = do
      (te', svars, RetType dims st, _) <- evalTypeExp te
      pure
        ( TypeArgExpType te',
          svars ++ dims,
          M.singleton pv $ Subst [] $ RetType [] $ toStruct st
        )
    checkArgApply p a =
      typeError tloc mempty $
        "Type argument"
          <+> pretty a
          <+> "not valid for a type parameter"
          <+> pretty p
          <> "."

-- | Check a type expression, producing:
--
-- * The checked expression.
-- * Size variables for any anonymous sizes in the expression.
-- * The elaborated type.
-- * The liftedness of the type.
checkTypeExp ::
  (MonadTypeChecker m) =>
  TypeExp NoInfo Name ->
  m (TypeExp Info VName, [VName], ResRetType, Liftedness)
checkTypeExp te = do
  checkForDuplicateNamesInType te
  evalTypeExp te

-- | Check for duplication of names inside a binding group.
checkForDuplicateNames ::
  (MonadTypeChecker m) => [UncheckedTypeParam] -> [UncheckedPat t] -> m ()
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
              "Name"
                <+> dquotes (pretty v)
                <+> "also bound at"
                <+> pretty (locStr prev_loc)
                <> "."
        Nothing ->
          modify $ M.insert (ns, v) loc

-- | Check whether the type contains arrow types that define the same
-- parameter.  These might also exist further down, but that's not
-- really a problem - we mostly do this checking to help the user,
-- since it is likely an error, but it's easy to assign a semantics to
-- it (normal name shadowing).
checkForDuplicateNamesInType ::
  (MonadTypeChecker m) =>
  TypeExp NoInfo Name ->
  m ()
checkForDuplicateNamesInType = check mempty
  where
    bad v loc prev_loc =
      typeError loc mempty $
        "Name"
          <+> dquotes (pretty v)
          <+> "also bound at"
          <+> pretty (locStr prev_loc)
          <> "."

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
    check seen (TEApply t1 TypeArgExpSize {} _) =
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
    check seen (TEParens te _) = check seen te

-- | @checkTypeParams ps m@ checks the type parameters @ps@, then
-- invokes the continuation @m@ with the checked parameters, while
-- extending the monadic name map with @ps@.
checkTypeParams ::
  (MonadTypeChecker m) =>
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
              "Type parameter"
                <+> dquotes (pretty v)
                <+> "previously defined at"
                <+> pretty (locStr prev)
                <> "."
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
  TypeArgDim $ sizeFromName (qualName v) ploc
typeParamToArg (TypeParamType _ v _) =
  TypeArgType $ Scalar $ TypeVar mempty (qualName v) []

-- | A type substitution may be a substitution or a yet-unknown
-- substitution (but which is certainly an overloaded primitive
-- type!).
data Subst t = Subst [TypeParam] t | ExpSubst Exp
  deriving (Show)

instance (Pretty t) => Pretty (Subst t) where
  pretty (Subst [] t) = pretty t
  pretty (Subst tps t) = mconcat (map pretty tps) <> colon <+> pretty t
  pretty (ExpSubst e) = pretty e

instance Functor Subst where
  fmap f (Subst ps t) = Subst ps $ f t
  fmap _ (ExpSubst e) = ExpSubst e

-- | Create a type substitution corresponding to a type binding.
substFromAbbr :: TypeBinding -> Subst StructRetType
substFromAbbr (TypeAbbr _ ps rt) = Subst ps rt

-- | Substitutions to apply in a type.
type TypeSubs = VName -> Maybe (Subst StructRetType)

-- | Class of types which allow for substitution of types with no
-- annotations for type variable names.
class Substitutable a where
  applySubst :: TypeSubs -> a -> a

instance Substitutable (RetTypeBase Size Uniqueness) where
  applySubst f (RetType dims t) =
    let RetType more_dims t' = substTypesRet f' t
     in RetType (dims ++ more_dims) t'
    where
      f' = fmap (fmap (second (const mempty))) . f

instance Substitutable (RetTypeBase Size NoUniqueness) where
  applySubst f (RetType dims t) =
    let RetType more_dims t' = substTypesRet f t
     in RetType (dims ++ more_dims) t'

instance Substitutable StructType where
  applySubst = substTypesAny

instance Substitutable ParamType where
  applySubst f = substTypesAny $ fmap (fmap $ second $ const Observe) . f

instance Substitutable (TypeBase Size Uniqueness) where
  applySubst f = substTypesAny $ fmap (fmap $ second $ const Nonunique) . f

instance Substitutable Exp where
  applySubst f = runIdentity . mapOnExp
    where
      mapOnExp (Var (QualName _ v) _ _)
        | Just (ExpSubst e') <- f v = pure e'
      mapOnExp e' = astMap mapper e'

      mapper =
        ASTMapper
          { mapOnExp,
            mapOnName = pure,
            mapOnStructType = pure . applySubst f,
            mapOnParamType = pure . applySubst f,
            mapOnResRetType = pure . applySubst f
          }

instance (Substitutable d) => Substitutable (Shape d) where
  applySubst f = fmap $ applySubst f

instance Substitutable (Pat StructType) where
  applySubst f = runIdentity . astMap mapper
    where
      mapper =
        ASTMapper
          { mapOnExp = pure . applySubst f,
            mapOnName = pure,
            mapOnStructType = pure . applySubst f,
            mapOnParamType = pure . applySubst f,
            mapOnResRetType = pure . applySubst f
          }

instance Substitutable (Pat ParamType) where
  applySubst f = runIdentity . astMap mapper
    where
      mapper =
        ASTMapper
          { mapOnExp = pure . applySubst f,
            mapOnName = pure,
            mapOnStructType = pure . applySubst f,
            mapOnParamType = pure . applySubst f,
            mapOnResRetType = pure . applySubst f
          }

applyType ::
  (Monoid u) =>
  [TypeParam] ->
  TypeBase Size u ->
  [StructTypeArg] ->
  TypeBase Size u
applyType ps t args = substTypesAny (`M.lookup` substs) t
  where
    substs = M.fromList $ zipWith mkSubst ps args
    -- We are assuming everything has already been type-checked for correctness.
    mkSubst (TypeParamDim pv _) (TypeArgDim e) =
      (pv, ExpSubst e)
    mkSubst (TypeParamType _ pv _) (TypeArgType at) =
      (pv, Subst [] $ RetType [] $ second mempty at)
    mkSubst p a =
      error $ "applyType mkSubst: cannot substitute " ++ prettyString a ++ " for " ++ prettyString p

substTypesRet ::
  (Monoid u) =>
  (VName -> Maybe (Subst (RetTypeBase Size u))) ->
  TypeBase Size u ->
  RetTypeBase Size u
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
              mkSubst = ExpSubst . flip sizeFromName mempty . qualName
              extsubsts = M.fromList $ zip ext $ map mkSubst ext'
              RetType [] t' = substTypesRet (`M.lookup` extsubsts) t
          pure $ RetType ext' t'

    onType ::
      forall as.
      (Monoid as) =>
      TypeBase Size as ->
      State [VName] (TypeBase Size as)

    onType (Array u shape et) =
      arrayOfWithAliases u (applySubst lookupSubst' shape)
        <$> onType (second (const mempty) $ Scalar et)
    onType (Scalar (Prim t)) = pure $ Scalar $ Prim t
    onType (Scalar (TypeVar u v targs)) = do
      targs' <- mapM subsTypeArg targs
      case lookupSubst $ qualLeaf v of
        Just (Subst ps rt) -> do
          RetType ext t <- freshDims rt
          modify (ext ++)
          pure $ second (<> u) $ applyType ps (second (const u) t) targs'
        _ ->
          pure $ Scalar $ TypeVar u v targs'
    onType (Scalar (Record ts)) =
      Scalar . Record <$> traverse onType ts
    onType (Scalar (Arrow u v d t1 t2)) =
      Scalar <$> (Arrow u v d <$> onType t1 <*> onRetType t2)
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

    subsTypeArg (TypeArgType t) = do
      let RetType dims t' = substTypesRet lookupSubst' t
      modify (dims ++)
      pure $ TypeArgType t'
    subsTypeArg (TypeArgDim v) =
      pure $ TypeArgDim $ applySubst lookupSubst' v

    lookupSubst' = fmap (fmap $ second (const NoUniqueness)) . lookupSubst

-- | Perform substitutions, from type names to types, on a type. Works
-- regardless of what shape and uniqueness information is attached to the type.
substTypesAny ::
  (Monoid u) =>
  (VName -> Maybe (Subst (RetTypeBase Size u))) ->
  TypeBase Size u ->
  TypeBase Size u
substTypesAny lookupSubst ot =
  case substTypesRet lookupSubst ot of
    RetType [] ot' -> ot'
    RetType dims ot' ->
      -- XXX HACK FIXME: turn any sizes that propagate to the top into
      -- AnySize.  This should _never_ happen during type-checking, but
      -- may happen as we substitute types during monomorphisation and
      -- defunctorisation later on. See Note [AnySize]
      let toAny (Var v _ _) | qualLeaf v `elem` dims = anySize
          toAny d = d
       in first toAny ot'

-- Note [AnySize]
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
-- I.e. we put in empty dimensions (AnySize), which are much later
-- turned into distinct sizes in Futhark.Internalise.Exps.  This will
-- result in unnecessary dynamic size checks, which will hopefully be
-- optimised away.
--
-- Important: The type checker will _never_ produce programs with
-- AnySize, but unfortunately some of the compilation steps
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
