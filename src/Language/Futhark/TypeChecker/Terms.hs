{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Facilities for type-checking Futhark terms.  Checking a term
-- requires a little more context to track uniqueness and such.
--
-- Type inference is implemented through a variation of
-- Hindley-Milner.  The main complication is supporting the rich
-- number of built-in language constructs, as well as uniqueness
-- types.  This is mostly done in an ad hoc way, and many programs
-- will require the programmer to fall back on type annotations.
module Language.Futhark.TypeChecker.Terms
  ( checkOneExp,
    checkFunDef,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Either
import Data.List (find, foldl', partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Futhark.IR.Primitive (intByteSize)
import Futhark.Util.Pretty hiding (bool, group, space)
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Match
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Terms.DoLoop
import Language.Futhark.TypeChecker.Terms.Monad
import Language.Futhark.TypeChecker.Terms.Pat
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify hiding (Usage)
import Prelude hiding (mod)

overloadedTypeVars :: Constraints -> Names
overloadedTypeVars = mconcat . map f . M.elems
  where
    f (_, HasFields fs _) = mconcat $ map typeVars $ M.elems fs
    f _ = mempty

--- Basic checking

-- | Determine if the two types are identical, ignoring uniqueness.
-- Mismatched dimensions are turned into fresh rigid type variables.
-- Causes a 'TypeError' if they fail to match, and otherwise returns
-- one of them.
unifyBranchTypes :: SrcLoc -> PatType -> PatType -> TermTypeM (PatType, [VName])
unifyBranchTypes loc t1 t2 =
  onFailure (CheckingBranches (toStruct t1) (toStruct t2)) $
    unifyMostCommon (mkUsage loc "unification of branch results") t1 t2

unifyBranches :: SrcLoc -> Exp -> Exp -> TermTypeM (PatType, [VName])
unifyBranches loc e1 e2 = do
  e1_t <- expTypeFully e1
  e2_t <- expTypeFully e2
  unifyBranchTypes loc e1_t e2_t

sliceShape ::
  Maybe (SrcLoc, Rigidity) ->
  Slice ->
  TypeBase (DimDecl VName) as ->
  TermTypeM (TypeBase (DimDecl VName) as, [VName])
sliceShape r slice t@(Array als u et (ShapeDecl orig_dims)) =
  runStateT (setDims <$> adjustDims slice orig_dims) []
  where
    setDims [] = stripArray (length orig_dims) t
    setDims dims' = Array als u et $ ShapeDecl dims'

    -- If the result is supposed to be a nonrigid size variable, then
    -- don't bother trying to create non-existential sizes.  This is
    -- necessary to make programs type-check without too much
    -- ceremony; see e.g. tests/inplace5.fut.
    isRigid Rigid {} = True
    isRigid _ = False
    refine_sizes = maybe False (isRigid . snd) r

    sliceSize orig_d i j stride =
      case r of
        Just (loc, Rigid _) -> do
          (d, ext) <-
            lift . extSize loc $
              SourceSlice orig_d' (bareExp <$> i) (bareExp <$> j) (bareExp <$> stride)
          modify (maybeToList ext ++)
          pure d
        Just (loc, Nonrigid) ->
          lift $ NamedDim . qualName <$> newDimVar loc Nonrigid "slice_dim"
        Nothing -> do
          v <- lift $ newID "slice_anydim"
          modify (v :)
          pure $ NamedDim $ qualName v
      where
        -- The original size does not matter if the slice is fully specified.
        orig_d'
          | isJust i, isJust j = Nothing
          | otherwise = Just orig_d

    adjustDims (DimFix {} : idxes') (_ : dims) =
      adjustDims idxes' dims
    -- Pat match some known slices to be non-existential.
    adjustDims (DimSlice i j stride : idxes') (_ : dims)
      | refine_sizes,
        maybe True ((== Just 0) . isInt64) i,
        Just j' <- maybeDimFromExp =<< j,
        maybe True ((== Just 1) . isInt64) stride =
        (j' :) <$> adjustDims idxes' dims
    adjustDims (DimSlice Nothing Nothing stride : idxes') (d : dims)
      | refine_sizes,
        maybe True (maybe False ((== 1) . abs) . isInt64) stride =
        (d :) <$> adjustDims idxes' dims
    adjustDims (DimSlice i j stride : idxes') (d : dims) =
      (:) <$> sliceSize d i j stride <*> adjustDims idxes' dims
    adjustDims _ dims =
      pure dims
sliceShape _ _ t = pure (t, [])

--- Main checkers

-- The closure of a lambda or local function are those variables that
-- it references, and which local to the current top-level function.
lexicalClosure :: [Pat] -> Occurrences -> TermTypeM Aliasing
lexicalClosure params closure = do
  vtable <- asks $ scopeVtable . termScope
  let isLocal v = case v `M.lookup` vtable of
        Just (BoundV Local _ _) -> True
        _ -> False
  pure . S.map AliasBound . S.filter isLocal $
    allOccurring closure S.\\ mconcat (map patNames params)

noAliasesIfOverloaded :: PatType -> TermTypeM PatType
noAliasesIfOverloaded t@(Scalar (TypeVar _ u tn [])) = do
  subst <- fmap snd . M.lookup (typeLeaf tn) <$> getConstraints
  case subst of
    Just Overloaded {} -> pure $ Scalar $ TypeVar mempty u tn []
    _ -> pure t
noAliasesIfOverloaded t =
  pure t

checkAscript ::
  SrcLoc ->
  UncheckedTypeDecl ->
  UncheckedExp ->
  TermTypeM (TypeDecl, Exp)
checkAscript loc (TypeDecl te NoInfo) e = do
  (te', decl_t, _) <- checkTypeExpNonrigid te
  e' <- checkExp e
  e_t <- toStruct <$> expTypeFully e'

  onFailure (CheckingAscription decl_t e_t) $
    unify (mkUsage loc "type ascription") decl_t e_t

  -- We also have to make sure that uniqueness matches.  This is done
  -- explicitly, because uniqueness is ignored by unification.
  e_t' <- normTypeFully e_t
  decl_t' <- normTypeFully decl_t
  unless (noSizes e_t' `subtypeOf` noSizes decl_t') $
    typeError loc mempty $
      "Type" <+> pquote (ppr e_t') <+> "is not a subtype of"
        <+> pquote (ppr decl_t') <> "."

  pure (TypeDecl te' $ Info decl_t', e')

checkCoerce ::
  SrcLoc ->
  UncheckedTypeDecl ->
  UncheckedExp ->
  TermTypeM (TypeDecl, Exp, [VName])
checkCoerce loc (TypeDecl te NoInfo) e = do
  (te', decl_t, ext) <- checkTypeExpRigid te RigidCoerce
  e' <- checkExp e
  e_t <- toStruct <$> expTypeFully e'

  (e_t_nonrigid, _) <-
    allDimsFreshInType loc Nonrigid "coerce_d" e_t

  onFailure (CheckingAscription decl_t e_t) $
    unify (mkUsage loc "type ascription") decl_t e_t_nonrigid

  -- We also have to make sure that uniqueness matches.  This is done
  -- explicitly, because uniqueness is ignored by unification.
  e_t' <- normTypeFully e_t
  decl_t' <- normTypeFully decl_t
  unless (noSizes e_t' `subtypeOf` noSizes decl_t') $
    typeError loc mempty $
      "Type" <+> pquote (ppr e_t') <+> "is not a subtype of"
        <+> pquote (ppr decl_t') <> "."

  pure (TypeDecl te' $ Info decl_t', e', ext)

unscopeType ::
  SrcLoc ->
  M.Map VName Ident ->
  PatType ->
  TermTypeM (PatType, [VName])
unscopeType tloc unscoped t = do
  (t', m) <- runStateT (traverseDims onDim t) mempty
  pure (t' `addAliases` S.map unAlias, M.elems m)
  where
    onDim bound _ (NamedDim d)
      | Just loc <- srclocOf <$> M.lookup (qualLeaf d) unscoped,
        not $ qualLeaf d `S.member` bound =
        inst loc $ qualLeaf d
    onDim _ _ d = pure d

    inst loc d = do
      prev <- gets $ M.lookup d
      case prev of
        Just d' -> pure $ NamedDim $ qualName d'
        Nothing -> do
          d' <- lift $ newDimVar tloc (Rigid $ RigidOutOfScope loc d) "d"
          modify $ M.insert d d'
          pure $ NamedDim $ qualName d'

    unAlias (AliasBound v) | v `M.member` unscoped = AliasFree v
    unAlias a = a

-- When a function result is not immediately bound to a name, we need
-- to invent a name for it so we can track it during aliasing
-- (uniqueness-error54.fut, uniqueness-error55.fut).
addResultAliases :: NameReason -> PatType -> TermTypeM PatType
addResultAliases r (Scalar (Record fs)) =
  Scalar . Record <$> traverse (addResultAliases r) fs
addResultAliases r (Scalar (Sum fs)) =
  Scalar . Sum <$> traverse (traverse (addResultAliases r)) fs
addResultAliases r (Scalar (TypeVar as u tn targs)) = do
  v <- newID "internal_app_result"
  modify $ \s -> s {stateNames = M.insert v r $ stateNames s}
  pure $ Scalar $ TypeVar (S.insert (AliasFree v) as) u tn targs
addResultAliases _ (Scalar t@Prim {}) = pure (Scalar t)
addResultAliases _ (Scalar t@Arrow {}) = pure (Scalar t)
addResultAliases r (Array als u t shape) = do
  v <- newID "internal_app_result"
  modify $ \s -> s {stateNames = M.insert v r $ stateNames s}
  pure $ Array (S.insert (AliasFree v) als) u t shape

-- 'checkApplyExp' is like 'checkExp', but tries to find the "root
-- function", for better error messages.
checkApplyExp :: UncheckedExp -> TermTypeM (Exp, ApplyOp)
checkApplyExp (AppExp (Apply e1 e2 _ loc) _) = do
  arg <- checkArg e2
  (e1', (fname, i)) <- checkApplyExp e1
  t <- expType e1'
  (t1, rt, argext, exts) <- checkApply loc (fname, i) t arg
  rt' <- addResultAliases (NameAppRes fname loc) rt
  return
    ( AppExp
        (Apply e1' (argExp arg) (Info (diet t1, argext)) loc)
        (Info $ AppRes rt' exts),
      (fname, i + 1)
    )
checkApplyExp e = do
  e' <- checkExp e
  return
    ( e',
      ( case e' of
          Var qn _ _ -> Just qn
          _ -> Nothing,
        0
      )
    )

checkExp :: UncheckedExp -> TermTypeM Exp
checkExp (Literal val loc) =
  pure $ Literal val loc
checkExp (StringLit vs loc) =
  pure $ StringLit vs loc
checkExp (IntLit val NoInfo loc) = do
  t <- newTypeVar loc "t"
  mustBeOneOf anyNumberType (mkUsage loc "integer literal") t
  pure $ IntLit val (Info $ fromStruct t) loc
checkExp (FloatLit val NoInfo loc) = do
  t <- newTypeVar loc "t"
  mustBeOneOf anyFloatType (mkUsage loc "float literal") t
  pure $ FloatLit val (Info $ fromStruct t) loc
checkExp (TupLit es loc) =
  TupLit <$> mapM checkExp es <*> pure loc
checkExp (RecordLit fs loc) = do
  fs' <- evalStateT (mapM checkField fs) mempty

  pure $ RecordLit fs' loc
  where
    checkField (RecordFieldExplicit f e rloc) = do
      errIfAlreadySet f rloc
      modify $ M.insert f rloc
      RecordFieldExplicit f <$> lift (checkExp e) <*> pure rloc
    checkField (RecordFieldImplicit name NoInfo rloc) = do
      errIfAlreadySet name rloc
      (QualName _ name', t) <- lift $ lookupVar rloc $ qualName name
      modify $ M.insert name rloc
      pure $ RecordFieldImplicit name' (Info t) rloc

    errIfAlreadySet f rloc = do
      maybe_sloc <- gets $ M.lookup f
      case maybe_sloc of
        Just sloc ->
          lift . typeError rloc mempty $
            "Field" <+> pquote (ppr f)
              <+> "previously defined at"
              <+> text (locStrRel rloc sloc) <> "."
        Nothing -> pure ()
checkExp (ArrayLit all_es _ loc) =
  -- Construct the result type and unify all elements with it.  We
  -- only create a type variable for empty arrays; otherwise we use
  -- the type of the first element.  This significantly cuts down on
  -- the number of type variables generated for pathologically large
  -- multidimensional array literals.
  case all_es of
    [] -> do
      et <- newTypeVar loc "t"
      t <- arrayOfM loc et (ShapeDecl [ConstDim 0]) Unique
      pure $ ArrayLit [] (Info t) loc
    e : es -> do
      e' <- checkExp e
      et <- expType e'
      es' <- mapM (unifies "type of first array element" (toStruct et) <=< checkExp) es
      et' <- normTypeFully et
      t <- arrayOfM loc et' (ShapeDecl [ConstDim $ length all_es]) Unique
      pure $ ArrayLit (e' : es') (Info t) loc
checkExp (AppExp (Range start maybe_step end loc) _) = do
  start' <- require "use in range expression" anySignedType =<< checkExp start
  start_t <- toStruct <$> expTypeFully start'
  maybe_step' <- case maybe_step of
    Nothing -> pure Nothing
    Just step -> do
      let warning = warn loc "First and second element of range are identical, this will produce an empty array."
      case (start, step) of
        (Literal x _, Literal y _) -> when (x == y) warning
        (Var x_name _ _, Var y_name _ _) -> when (x_name == y_name) warning
        _ -> pure ()
      Just <$> (unifies "use in range expression" start_t =<< checkExp step)

  let unifyRange e = unifies "use in range expression" start_t =<< checkExp e
  end' <- traverse unifyRange end

  end_t <- case end' of
    DownToExclusive e -> expType e
    ToInclusive e -> expType e
    UpToExclusive e -> expType e

  -- Special case some ranges to give them a known size.
  let dimFromBound = dimFromExp (SourceBound . bareExp)
  (dim, retext) <-
    case (isInt64 start', isInt64 <$> maybe_step', end') of
      (Just 0, Just (Just 1), UpToExclusive end'')
        | Scalar (Prim (Signed Int64)) <- end_t ->
          dimFromBound end''
      (Just 0, Nothing, UpToExclusive end'')
        | Scalar (Prim (Signed Int64)) <- end_t ->
          dimFromBound end''
      (Just 1, Just (Just 2), ToInclusive end'')
        | Scalar (Prim (Signed Int64)) <- end_t ->
          dimFromBound end''
      _ -> do
        d <- newDimVar loc (Rigid RigidRange) "range_dim"
        pure (NamedDim $ qualName d, Just d)

  t <- arrayOfM loc start_t (ShapeDecl [dim]) Unique
  let res = AppRes (t `setAliases` mempty) (maybeToList retext)

  pure $ AppExp (Range start' maybe_step' end' loc) (Info res)
checkExp (Ascript e decl loc) = do
  (decl', e') <- checkAscript loc decl e
  pure $ Ascript e' decl' loc
checkExp (AppExp (Coerce e decl loc) _) = do
  (decl', e', ext) <- checkCoerce loc decl e
  t <- expTypeFully e'
  t' <- matchDims (const . const pure) t $ fromStruct $ unInfo $ expandedType decl'
  pure $ AppExp (Coerce e' decl' loc) (Info $ AppRes t' ext)
checkExp (AppExp (BinOp (op, oploc) NoInfo (e1, _) (e2, _) loc) NoInfo) = do
  (op', ftype) <- lookupVar oploc op
  e1_arg <- checkArg e1
  e2_arg <- checkArg e2

  -- Note that the application to the first operand cannot fix any
  -- existential sizes, because it must by necessity be a function.
  (p1_t, rt, p1_ext, _) <- checkApply loc (Just op', 0) ftype e1_arg
  (p2_t, rt', p2_ext, retext) <- checkApply loc (Just op', 1) rt e2_arg

  pure $
    AppExp
      ( BinOp
          (op', oploc)
          (Info ftype)
          (argExp e1_arg, Info (toStruct p1_t, p1_ext))
          (argExp e2_arg, Info (toStruct p2_t, p2_ext))
          loc
      )
      (Info (AppRes rt' retext))
checkExp (Project k e NoInfo loc) = do
  e' <- checkExp e
  t <- expType e'
  kt <- mustHaveField (mkUsage loc $ "projection of field " ++ quote (pretty k)) k t
  pure $ Project k e' (Info kt) loc
checkExp (AppExp (If e1 e2 e3 loc) _) =
  sequentially checkCond $ \e1' _ -> do
    ((e2', e3'), dflow) <- tapOccurrences $ checkExp e2 `alternative` checkExp e3

    (brancht, retext) <- unifyBranches loc e2' e3'
    let t' = addAliases brancht $ S.filter $ (`S.notMember` allConsumed dflow) . aliasVar

    zeroOrderType
      (mkUsage loc "returning value of this type from 'if' expression")
      "type returned from branch"
      t'

    pure $ AppExp (If e1' e2' e3' loc) (Info $ AppRes t' retext)
  where
    checkCond = do
      e1' <- checkExp e1
      let bool = Scalar $ Prim Bool
      e1_t <- toStruct <$> expType e1'
      onFailure (CheckingRequired [bool] e1_t) $
        unify (mkUsage (srclocOf e1') "use as 'if' condition") bool e1_t
      pure e1'
checkExp (Parens e loc) =
  Parens <$> checkExp e <*> pure loc
checkExp (QualParens (modname, modnameloc) e loc) = do
  (modname', mod) <- lookupMod loc modname
  case mod of
    ModEnv env -> local (`withEnv` qualifyEnv modname' env) $ do
      e' <- checkExp e
      pure $ QualParens (modname', modnameloc) e' loc
    ModFun {} ->
      typeError loc mempty . withIndexLink "module-is-parametric" $
        "Module" <+> ppr modname <+> " is a parametric module."
  where
    qualifyEnv modname' env =
      env {envNameMap = M.map (qualify' modname') $ envNameMap env}
    qualify' modname' (QualName qs name) =
      QualName (qualQuals modname' ++ [qualLeaf modname'] ++ qs) name
checkExp (Var qn NoInfo loc) = do
  -- The qualifiers of a variable is divided into two parts: first a
  -- possibly-empty sequence of module qualifiers, followed by a
  -- possible-empty sequence of record field accesses.  We use scope
  -- information to perform the split, by taking qualifiers off the
  -- end until we find a module.

  (qn', t, fields) <- findRootVar (qualQuals qn) (qualLeaf qn)

  foldM checkField (Var qn' (Info t) loc) fields
  where
    findRootVar qs name =
      (whenFound <$> lookupVar loc (QualName qs name)) `catchError` notFound qs name

    whenFound (qn', t) = (qn', t, [])

    notFound qs name err
      | null qs = throwError err
      | otherwise = do
        (qn', t, fields) <-
          findRootVar (init qs) (last qs)
            `catchError` const (throwError err)
        pure (qn', t, fields ++ [name])

    checkField e k = do
      t <- expType e
      let usage = mkUsage loc $ "projection of field " ++ quote (pretty k)
      kt <- mustHaveField usage k t
      pure $ Project k e (Info kt) loc
checkExp (Negate arg loc) = do
  arg' <- require "numeric negation" anyNumberType =<< checkExp arg
  pure $ Negate arg' loc
checkExp (Not arg loc) = do
  arg' <- require "logical negation" (Bool : anyIntType) =<< checkExp arg
  pure $ Not arg' loc
checkExp e@(AppExp Apply {} _) = fst <$> checkApplyExp e
checkExp (AppExp (LetPat sizes pat e body loc) _) =
  sequentially (checkExp e) $ \e' e_occs -> do
    -- Not technically an ascription, but we want the pattern to have
    -- exactly the type of 'e'.
    t <- expType e'
    case anyConsumption e_occs of
      Just c ->
        let msg = "type computed with consumption at " ++ locStr (location c)
         in zeroOrderType (mkUsage loc "consumption in right-hand side of 'let'-binding") msg t
      _ -> pure ()

    incLevel . bindingSizes sizes $ \sizes' ->
      bindingPat sizes' pat (Ascribed t) $ \pat' -> do
        body' <- checkExp body
        (body_t, retext) <-
          unscopeType loc (sizesMap sizes' <> patternMap pat') =<< expTypeFully body'

        pure $ AppExp (LetPat sizes' pat' e' body' loc) (Info $ AppRes body_t retext)
  where
    sizesMap = foldMap onSize
    onSize size =
      M.singleton (sizeName size) $
        Ident (sizeName size) (Info (Scalar $ Prim $ Signed Int64)) (srclocOf size)
checkExp (AppExp (LetFun name (tparams, params, maybe_retdecl, NoInfo, e) body loc) _) =
  sequentially (checkBinding (name, maybe_retdecl, tparams, params, e, loc)) $
    \(tparams', params', maybe_retdecl', rettype, e') closure -> do
      closure' <- lexicalClosure params' closure

      bindSpaced [(Term, name)] $ do
        name' <- checkName Term name loc

        let arrow (xp, xt) yt = RetType [] $ Scalar $ Arrow () xp xt yt
            RetType _ ftype = foldr (arrow . patternParam) rettype params'
            entry = BoundV Local tparams' $ ftype `setAliases` closure'
            bindF scope =
              scope
                { scopeVtable =
                    M.insert name' entry $ scopeVtable scope,
                  scopeNameMap =
                    M.insert (Term, name) (qualName name') $
                      scopeNameMap scope
                }
        body' <- localScope bindF $ checkExp body

        -- We fake an ident here, but it's OK as it can't be a size
        -- anyway.
        let fake_ident = Ident name' (Info $ fromStruct ftype) mempty
        (body_t, ext) <-
          unscopeType loc (M.singleton name' fake_ident)
            =<< expTypeFully body'

        pure $
          AppExp
            ( LetFun
                name'
                (tparams', params', maybe_retdecl', Info rettype, e')
                body'
                loc
            )
            (Info $ AppRes body_t ext)
checkExp (AppExp (LetWith dest src slice ve body loc) _) =
  sequentially (checkIdent src) $ \src' _ -> do
    slice' <- checkSlice slice
    (t, _) <- newArrayType (srclocOf src) "src" $ sliceDims slice'
    unify (mkUsage loc "type of target array") t $ toStruct $ unInfo $ identType src'

    -- Need the fully normalised type here to get the proper aliasing information.
    src_t <- normTypeFully $ unInfo $ identType src'

    (elemt, _) <- sliceShape (Just (loc, Nonrigid)) slice' =<< normTypeFully t

    unless (unique src_t) $ notConsumable loc $ pquote $ pprName $ identName src

    sequentially (unifies "type of target array" (toStruct elemt) =<< checkExp ve) $ \ve' _ -> do
      ve_t <- expTypeFully ve'
      when (AliasBound (identName src') `S.member` aliases ve_t) $
        badLetWithValue src ve loc

      bindingIdent dest (src_t `setAliases` S.empty) $ \dest' -> do
        body' <- consuming src' $ checkExp body
        (body_t, ext) <-
          unscopeType loc (M.singleton (identName dest') dest')
            =<< expTypeFully body'
        pure $ AppExp (LetWith dest' src' slice' ve' body' loc) (Info $ AppRes body_t ext)
checkExp (Update src slice ve loc) = do
  slice' <- checkSlice slice
  (t, _) <- newArrayType (srclocOf src) "src" $ sliceDims slice'
  (elemt, _) <- sliceShape (Just (loc, Nonrigid)) slice' =<< normTypeFully t

  sequentially (checkExp ve >>= unifies "type of target array" elemt) $ \ve' _ ->
    sequentially (checkExp src >>= unifies "type of target array" t) $ \src' _ -> do
      src_t <- expTypeFully src'

      unless (unique src_t) $ notConsumable loc $ pquote $ ppr src

      let src_als = aliases src_t
      ve_t <- expTypeFully ve'
      unless (S.null $ src_als `S.intersection` aliases ve_t) $ badLetWithValue src ve loc

      consume loc src_als
      pure $ Update src' slice' ve' loc

-- Record updates are a bit hacky, because we do not have row typing
-- (yet?).  For now, we only permit record updates where we know the
-- full type up to the field we are updating.
checkExp (RecordUpdate src fields ve NoInfo loc) = do
  src' <- checkExp src
  ve' <- checkExp ve
  a <- expTypeFully src'
  foldM_ (flip $ mustHaveField usage) a fields
  ve_t <- expType ve'
  updated_t <- updateField fields ve_t =<< expTypeFully src'
  pure $ RecordUpdate src' fields ve' (Info updated_t) loc
  where
    usage = mkUsage loc "record update"
    updateField [] ve_t src_t = do
      (src_t', _) <- allDimsFreshInType loc Nonrigid "any" src_t
      onFailure (CheckingRecordUpdate fields (toStruct src_t') (toStruct ve_t)) $
        unify usage (toStruct src_t') (toStruct ve_t)
      -- Important that we return ve_t so that we get the right aliases.
      pure ve_t
    updateField (f : fs) ve_t (Scalar (Record m))
      | Just f_t <- M.lookup f m = do
        f_t' <- updateField fs ve_t f_t
        pure $ Scalar $ Record $ M.insert f f_t' m
    updateField _ _ _ =
      typeError loc mempty . withIndexLink "record-type-not-known" $
        "Full type of"
          </> indent 2 (ppr src)
          </> textwrap " is not known at this point.  Add a type annotation to the original record to disambiguate."

--
checkExp (AppExp (Index e slice loc) _) = do
  slice' <- checkSlice slice
  (t, _) <- newArrayType loc "e" $ sliceDims slice'
  e' <- unifies "being indexed at" t =<< checkExp e
  -- XXX, the RigidSlice here will be overridden in sliceShape with a proper value.
  (t', retext) <-
    sliceShape (Just (loc, Rigid (RigidSlice Nothing ""))) slice'
      =<< expTypeFully e'

  -- Remove aliases if the result is an overloaded type, because that
  -- will certainly not be aliased.
  t'' <- noAliasesIfOverloaded t'

  pure $ AppExp (Index e' slice' loc) (Info $ AppRes t'' retext)
checkExp (Assert e1 e2 NoInfo loc) = do
  e1' <- require "being asserted" [Bool] =<< checkExp e1
  e2' <- checkExp e2
  pure $ Assert e1' e2' (Info (pretty e1)) loc
checkExp (Lambda params body rettype_te NoInfo loc) =
  removeSeminullOccurrences . noUnique . incLevel . bindingParams [] params $ \_ params' -> do
    rettype_checked <- traverse checkTypeExpNonrigid rettype_te
    let declared_rettype =
          case rettype_checked of
            Just (_, st, _) -> Just st
            Nothing -> Nothing
    (body', closure) <-
      tapOccurrences $ checkFunBody params' body declared_rettype loc
    body_t <- expTypeFully body'

    params'' <- mapM updateTypes params'

    (rettype', rettype_st) <-
      case rettype_checked of
        Just (te, st, ext) -> do
          let st_structural = toStructural st
          checkReturnAlias loc st_structural params'' body_t
          pure (Just te, RetType ext st)
        Nothing -> do
          ret <-
            inferReturnSizes params'' . toStruct $
              inferReturnUniqueness params'' body_t
          pure (Nothing, ret)

    checkGlobalAliases params' body_t loc
    verifyFunctionParams Nothing params'

    closure' <- lexicalClosure params'' closure

    pure $ Lambda params'' body' rettype' (Info (closure', rettype_st)) loc
  where
    -- Inferring the sizes of the return type of a lambda is a lot
    -- like let-generalisation.  We wish to remove any rigid sizes
    -- that were created when checking the body, except for those that
    -- are visible in types that existed before we entered the body,
    -- are parameters, or are used in parameters.
    inferReturnSizes params' ret = do
      cur_lvl <- curLevel
      let named (Named x, _) = Just x
          named (Unnamed, _) = Nothing
          param_names = mapMaybe (named . patternParam) params'
          pos_sizes =
            typeDimNamesPos . foldFunType (map patternStructType params') $
              RetType [] ret
          hide k (lvl, _) =
            lvl >= cur_lvl && k `notElem` param_names && k `S.notMember` pos_sizes

      hidden_sizes <-
        S.fromList . M.keys . M.filterWithKey hide <$> getConstraints

      let onDim (NamedDim name)
            | qualLeaf name `S.member` hidden_sizes = S.singleton $ qualLeaf name
          onDim _ = mempty

      pure $ RetType (S.toList $ foldMap onDim $ nestedDims ret) ret
checkExp (OpSection op _ loc) = do
  (op', ftype) <- lookupVar loc op
  pure $ OpSection op' (Info ftype) loc
checkExp (OpSectionLeft op _ e _ _ loc) = do
  (op', ftype) <- lookupVar loc op
  e_arg <- checkArg e
  (t1, rt, argext, retext) <- checkApply loc (Just op', 0) ftype e_arg
  case (ftype, rt) of
    (Scalar (Arrow _ m1 _ _), Scalar (Arrow _ m2 t2 rettype)) ->
      pure $
        OpSectionLeft
          op'
          (Info ftype)
          (argExp e_arg)
          (Info (m1, toStruct t1, argext), Info (m2, toStruct t2))
          (Info rettype, Info retext)
          loc
    _ ->
      typeError loc mempty $
        "Operator section with invalid operator of type" <+> ppr ftype
checkExp (OpSectionRight op _ e _ NoInfo loc) = do
  (op', ftype) <- lookupVar loc op
  e_arg <- checkArg e
  case ftype of
    Scalar (Arrow as1 m1 t1 (RetType [] (Scalar (Arrow as2 m2 t2 (RetType dims2 ret))))) -> do
      (t2', ret', argext, _) <-
        checkApply
          loc
          (Just op', 1)
          (Scalar $ Arrow as2 m2 t2 $ RetType [] $ Scalar $ Arrow as1 m1 t1 $ RetType [] ret)
          e_arg
      pure $
        OpSectionRight
          op'
          (Info ftype)
          (argExp e_arg)
          (Info (m1, toStruct t1), Info (m2, toStruct t2', argext))
          (Info $ RetType dims2 $ addAliases ret (<> aliases ret'))
          loc
    _ ->
      typeError loc mempty $
        "Operator section with invalid operator of type" <+> ppr ftype
checkExp (ProjectSection fields NoInfo loc) = do
  a <- newTypeVar loc "a"
  let usage = mkUsage loc "projection at"
  b <- foldM (flip $ mustHaveField usage) a fields
  pure $ ProjectSection fields (Info $ Scalar $ Arrow mempty Unnamed a $ RetType [] b) loc
checkExp (IndexSection slice NoInfo loc) = do
  slice' <- checkSlice slice
  (t, _) <- newArrayType loc "e" $ sliceDims slice'
  (t', retext) <- sliceShape Nothing slice' t
  pure $ IndexSection slice' (Info $ fromStruct $ Scalar $ Arrow mempty Unnamed t $ RetType retext t') loc
checkExp (AppExp (DoLoop _ mergepat mergeexp form loopbody loc) _) = do
  ((sparams, mergepat', mergeexp', form', loopbody'), appres) <-
    checkDoLoop checkExp (mergepat, mergeexp, form, loopbody) loc
  pure $
    AppExp
      (DoLoop sparams mergepat' mergeexp' form' loopbody' loc)
      (Info appres)
checkExp (Constr name es NoInfo loc) = do
  t <- newTypeVar loc "t"
  es' <- mapM checkExp es
  ets <- mapM expTypeFully es'
  mustHaveConstr (mkUsage loc "use of constructor") name t (toStruct <$> ets)
  -- A sum value aliases *anything* that went into its construction.
  let als = foldMap aliases ets
  pure $ Constr name es' (Info $ fromStruct t `addAliases` (<> als)) loc
checkExp (AppExp (Match e cs loc) _) =
  sequentially (checkExp e) $ \e' _ -> do
    mt <- expTypeFully e'
    (cs', t, retext) <- checkCases mt cs
    zeroOrderType
      (mkUsage loc "being returned 'match'")
      "type returned from pattern match"
      t
    pure $ AppExp (Match e' cs' loc) (Info $ AppRes t retext)
checkExp (Attr info e loc) =
  Attr <$> checkAttr info <*> checkExp e <*> pure loc

checkCases ::
  PatType ->
  NE.NonEmpty (CaseBase NoInfo Name) ->
  TermTypeM (NE.NonEmpty (CaseBase Info VName), PatType, [VName])
checkCases mt rest_cs =
  case NE.uncons rest_cs of
    (c, Nothing) -> do
      (c', t, retext) <- checkCase mt c
      pure (c' NE.:| [], t, retext)
    (c, Just cs) -> do
      (((c', c_t, _), (cs', cs_t, _)), dflow) <-
        tapOccurrences $ checkCase mt c `alternative` checkCases mt cs
      (brancht, retext) <- unifyBranchTypes (srclocOf c) c_t cs_t
      let t =
            addAliases
              brancht
              (`S.difference` S.map AliasBound (allConsumed dflow))
      pure (NE.cons c' cs', t, retext)

checkCase ::
  PatType ->
  CaseBase NoInfo Name ->
  TermTypeM (CaseBase Info VName, PatType, [VName])
checkCase mt (CasePat p e loc) =
  bindingPat [] p (Ascribed mt) $ \p' -> do
    e' <- checkExp e
    (t, retext) <- unscopeType loc (patternMap p') =<< expTypeFully e'
    pure (CasePat p' e' loc, t, retext)

-- | An unmatched pattern. Used in in the generation of
-- unmatched pattern warnings by the type checker.
data Unmatched p
  = UnmatchedNum p [PatLit]
  | UnmatchedBool p
  | UnmatchedConstr p
  | Unmatched p
  deriving (Functor, Show)

instance Pretty (Unmatched (PatBase Info VName)) where
  ppr um = case um of
    (UnmatchedNum p nums) -> ppr' p <+> "where p is not one of" <+> ppr nums
    (UnmatchedBool p) -> ppr' p
    (UnmatchedConstr p) -> ppr' p
    (Unmatched p) -> ppr' p
    where
      ppr' (PatAscription p t _) = ppr p <> ":" <+> ppr t
      ppr' (PatParens p _) = parens $ ppr' p
      ppr' (PatAttr _ p _) = parens $ ppr' p
      ppr' (Id v _ _) = pprName v
      ppr' (TuplePat pats _) = parens $ commasep $ map ppr' pats
      ppr' (RecordPat fs _) = braces $ commasep $ map ppField fs
        where
          ppField (name, t) = text (nameToString name) <> equals <> ppr' t
      ppr' Wildcard {} = "_"
      ppr' (PatLit e _ _) = ppr e
      ppr' (PatConstr n _ ps _) = "#" <> ppr n <+> sep (map ppr' ps)

checkUnmatched :: Exp -> TermTypeM ()
checkUnmatched e = void $ checkUnmatched' e >> astMap tv e
  where
    checkUnmatched' (AppExp (Match _ cs loc) _) =
      let ps = fmap (\(CasePat p _ _) -> p) cs
       in case unmatched $ NE.toList ps of
            [] -> pure ()
            ps' ->
              typeError loc mempty . withIndexLink "unmatched-cases" $
                "Unmatched cases in match expression:"
                  </> indent 2 (stack (map ppr ps'))
    checkUnmatched' _ = pure ()
    tv = identityMapper {mapOnExp = \e' -> checkUnmatched' e' >> pure e'}

checkIdent :: IdentBase NoInfo Name -> TermTypeM Ident
checkIdent (Ident name _ loc) = do
  (QualName _ name', vt) <- lookupVar loc (qualName name)
  pure $ Ident name' (Info vt) loc

checkSlice :: UncheckedSlice -> TermTypeM Slice
checkSlice = mapM checkDimIndex
  where
    checkDimIndex (DimFix i) =
      DimFix <$> (require "use as index" anySignedType =<< checkExp i)
    checkDimIndex (DimSlice i j s) =
      DimSlice <$> check i <*> check j <*> check s

    check =
      maybe (return Nothing) $
        fmap Just . unifies "use as index" (Scalar $ Prim $ Signed Int64) <=< checkExp

-- The number of dimensions affected by this slice (so the minimum
-- rank of the array we are slicing).
sliceDims :: Slice -> Int
sliceDims = length

type Arg = (Exp, PatType, Occurrences, SrcLoc)

argExp :: Arg -> Exp
argExp (e, _, _, _) = e

argType :: Arg -> PatType
argType (_, t, _, _) = t

checkArg :: UncheckedExp -> TermTypeM Arg
checkArg arg = do
  (arg', dflow) <- collectOccurrences $ checkExp arg
  arg_t <- expType arg'
  pure (arg', arg_t, dflow, srclocOf arg')

instantiateDimsInReturnType ::
  SrcLoc ->
  Maybe (QualName VName) ->
  RetTypeBase (DimDecl VName) als ->
  TermTypeM (TypeBase (DimDecl VName) als, [VName])
instantiateDimsInReturnType tloc fname =
  instantiateEmptyArrayDims tloc $ Rigid $ RigidRet fname

-- Some information about the function/operator we are trying to
-- apply, and how many arguments it has previously accepted.  Used for
-- generating nicer type errors.
type ApplyOp = (Maybe (QualName VName), Int)

-- | Extract all those names that are bound inside the type.
boundInsideType :: TypeBase (DimDecl VName) as -> S.Set VName
boundInsideType (Array _ _ t _) = boundInsideType (Scalar t)
boundInsideType (Scalar Prim {}) = mempty
boundInsideType (Scalar (TypeVar _ _ _ targs)) = foldMap f targs
  where
    f (TypeArgType t _) = boundInsideType t
    f TypeArgDim {} = mempty
boundInsideType (Scalar (Record fs)) = foldMap boundInsideType fs
boundInsideType (Scalar (Sum cs)) = foldMap (foldMap boundInsideType) cs
boundInsideType (Scalar (Arrow _ pn t1 (RetType dims t2))) =
  pn' <> boundInsideType t1 <> S.fromList dims <> boundInsideType t2
  where
    pn' = case pn of
      Unnamed -> mempty
      Named v -> S.singleton v

-- Returns the sizes of the immediate type produced,
-- the sizes of parameter types, and the sizes of return types.
dimUses :: StructType -> (Names, Names)
dimUses = flip execState mempty . traverseDims f
  where
    f bound _ (NamedDim v) | qualLeaf v `S.member` bound = pure ()
    f _ PosImmediate (NamedDim v) = modify ((S.singleton (qualLeaf v), mempty) <>)
    f _ PosParam (NamedDim v) = modify ((mempty, S.singleton (qualLeaf v)) <>)
    f _ _ _ = pure ()

checkApply ::
  SrcLoc ->
  ApplyOp ->
  PatType ->
  Arg ->
  TermTypeM (PatType, PatType, Maybe VName, [VName])
checkApply
  loc
  (fname, _)
  (Scalar (Arrow as pname tp1 tp2))
  (argexp, argtype, dflow, argloc) =
    onFailure (CheckingApply fname argexp (toStruct tp1) (toStruct argtype)) $ do
      expect (mkUsage argloc "use as function argument") (toStruct tp1) (toStruct argtype)

      -- Perform substitutions of instantiated variables in the types.
      tp1' <- normTypeFully tp1
      (tp2', ext) <- instantiateDimsInReturnType loc fname =<< normTypeFully tp2
      argtype' <- normTypeFully argtype

      -- Check whether this would produce an impossible return type.
      let (tp2_produced_dims, tp2_paramdims) = dimUses $ toStruct tp2'
          problematic = S.fromList ext <> boundInsideType argtype'
      when (any (`S.member` problematic) (tp2_paramdims `S.difference` tp2_produced_dims)) $ do
        typeError loc mempty . withIndexLink "existential-param-ret" $
          "Existential size would appear in function parameter of return type:"
            </> indent 2 (ppr (RetType ext tp2'))
            </> textwrap "This is usually because a higher-order function is used with functional arguments that return existential sizes or locally named sizes, which are then used as parameters of other function arguments."

      occur [observation as loc]

      checkOccurrences dflow

      case anyConsumption dflow of
        Just c ->
          let msg = "type of expression with consumption at " ++ locStr (location c)
           in zeroOrderType (mkUsage argloc "potential consumption in expression") msg tp1
        _ -> pure ()

      occurs <- (dflow `seqOccurrences`) <$> consumeArg argloc argtype' (diet tp1')

      checkIfConsumable loc $ S.map AliasBound $ allConsumed occurs
      occur occurs

      -- Unification ignores uniqueness in higher-order arguments, so
      -- we check for that here.
      unless (toStructural argtype' `subtypeOf` toStructural tp1') $
        typeError loc mempty "Uniqueness does not match."

      (argext, parsubst) <-
        case pname of
          Named pname'
            | (Scalar (Prim (Signed Int64))) <- tp1' -> do
              (d, argext) <- dimFromArg fname argexp
              pure
                ( argext,
                  (`M.lookup` M.singleton pname' (SizeSubst d))
                )
          _ -> pure (Nothing, const Nothing)
      let tp2'' = applySubst parsubst $ returnType tp2' (diet tp1') argtype'

      pure (tp1', tp2'', argext, ext)
checkApply loc fname tfun@(Scalar TypeVar {}) arg = do
  tv <- newTypeVar loc "b"
  -- Change the uniqueness of the argument type because we never want
  -- to infer that a function is consuming.
  let argt_nonunique = toStruct (argType arg) `setUniqueness` Nonunique
  unify (mkUsage loc "use as function") (toStruct tfun) $
    Scalar $ Arrow mempty Unnamed argt_nonunique $ RetType [] tv
  tfun' <- normPatType tfun
  checkApply loc fname tfun' arg
checkApply loc (fname, prev_applied) ftype (argexp, _, _, _) = do
  let fname' = maybe "expression" (pquote . ppr) fname

  typeError loc mempty $
    if prev_applied == 0
      then
        "Cannot apply" <+> fname' <+> "as function, as it has type:"
          </> indent 2 (ppr ftype)
      else
        "Cannot apply" <+> fname' <+> "to argument #" <> ppr (prev_applied + 1)
          <+> pquote (shorten $ pretty $ flatten $ ppr argexp) <> ","
          <+/> "as"
          <+> fname'
          <+> "only takes"
          <+> ppr prev_applied
          <+> arguments <> "."
  where
    arguments
      | prev_applied == 1 = "argument"
      | otherwise = "arguments"

-- | @returnType ret_type arg_diet arg_type@ gives result of applying
-- an argument the given types to a function with the given return
-- type, consuming the argument with the given diet.
returnType ::
  PatType ->
  Diet ->
  PatType ->
  PatType
returnType (Array _ Unique et shape) _ _ =
  Array mempty Unique et shape
returnType (Array als Nonunique et shape) d arg =
  Array (als <> arg_als) Unique et shape -- Intentional!
  where
    arg_als = aliases $ maskAliases arg d
returnType (Scalar (Record fs)) d arg =
  Scalar $ Record $ fmap (\et -> returnType et d arg) fs
returnType (Scalar (Prim t)) _ _ =
  Scalar $ Prim t
returnType (Scalar (TypeVar _ Unique t targs)) _ _ =
  Scalar $ TypeVar mempty Unique t targs
returnType (Scalar (TypeVar als Nonunique t targs)) d arg =
  Scalar $ TypeVar (als <> arg_als) Unique t targs -- Intentional!
  where
    arg_als = aliases $ maskAliases arg d
returnType (Scalar (Arrow old_als v t1 (RetType dims t2))) d arg =
  Scalar $ Arrow als v (t1 `setAliases` mempty) $ RetType dims $ t2 `setAliases` als
  where
    -- Make sure to propagate the aliases of an existing closure.
    als = old_als <> aliases (maskAliases arg d)
returnType (Scalar (Sum cs)) d arg =
  Scalar $ Sum $ (fmap . fmap) (\et -> returnType et d arg) cs

-- | @t `maskAliases` d@ removes aliases (sets them to 'mempty') from
-- the parts of @t@ that are denoted as consumed by the 'Diet' @d@.
maskAliases ::
  Monoid as =>
  TypeBase shape as ->
  Diet ->
  TypeBase shape as
maskAliases t Consume = t `setAliases` mempty
maskAliases t Observe = t
maskAliases (Scalar (Record ets)) (RecordDiet ds) =
  Scalar $ Record $ M.intersectionWith maskAliases ets ds
maskAliases (Scalar (Sum ets)) (SumDiet ds) =
  Scalar $ Sum $ M.intersectionWith (zipWith maskAliases) ets ds
maskAliases t FuncDiet {} = t
maskAliases _ _ = error "Invalid arguments passed to maskAliases."

consumeArg :: SrcLoc -> PatType -> Diet -> TermTypeM [Occurrence]
consumeArg loc (Scalar (Record ets)) (RecordDiet ds) =
  concat . M.elems <$> traverse (uncurry $ consumeArg loc) (M.intersectionWith (,) ets ds)
consumeArg loc (Scalar (Sum ets)) (SumDiet ds) =
  concat <$> traverse (uncurry $ consumeArg loc) (concat $ M.elems $ M.intersectionWith zip ets ds)
consumeArg loc (Array _ Nonunique _ _) Consume =
  typeError loc mempty . withIndexLink "consuming-parameter" $
    "Consuming parameter passed non-unique argument."
consumeArg loc (Scalar (TypeVar _ Nonunique _ _)) Consume =
  typeError loc mempty . withIndexLink "consuming-parameter" $
    "Consuming parameter passed non-unique argument."
consumeArg loc (Scalar (Arrow _ _ t1 _)) (FuncDiet d _)
  | not $ contravariantArg t1 d =
    typeError loc mempty . withIndexLink "consuming-argument" $
      "Non-consuming higher-order parameter passed consuming argument."
  where
    contravariantArg (Array _ Unique _ _) Observe =
      False
    contravariantArg (Scalar (TypeVar _ Unique _ _)) Observe =
      False
    contravariantArg (Scalar (Record ets)) (RecordDiet ds) =
      and (M.intersectionWith contravariantArg ets ds)
    contravariantArg (Scalar (Arrow _ _ tp (RetType _ tr))) (FuncDiet dp dr) =
      contravariantArg tp dp && contravariantArg tr dr
    contravariantArg _ _ =
      True
consumeArg loc at Consume = pure [consumption (aliases at) loc]
consumeArg loc at _ = pure [observation (aliases at) loc]

-- | Type-check a single expression in isolation.  This expression may
-- turn out to be polymorphic, in which case the list of type
-- parameters will be non-empty.
checkOneExp :: UncheckedExp -> TypeM ([TypeParam], Exp)
checkOneExp e = fmap fst . runTermTypeM $ do
  e' <- checkExp e
  let t = toStruct $ typeOf e'
  (tparams, _, _) <-
    letGeneralise (nameFromString "<exp>") (srclocOf e) [] [] t
  fixOverloadedTypes $ typeVars t
  e'' <- updateTypes e'
  checkUnmatched e''
  causalityCheck e''
  literalOverflowCheck e''
  pure (tparams, e'')

-- Verify that all sum type constructors and empty array literals have
-- a size that is known (rigid or a type parameter).  This is to
-- ensure that we can actually determine their shape at run-time.
causalityCheck :: Exp -> TermTypeM ()
causalityCheck binding_body = do
  constraints <- getConstraints

  let checkCausality what known t loc
        | (d, dloc) : _ <-
            mapMaybe (unknown constraints known) $
              S.toList $ typeDimNames $ toStruct t =
          Just $ lift $ causality what (locOf loc) d dloc t
        | otherwise = Nothing

      checkParamCausality known p =
        checkCausality (ppr p) known (patternType p) (locOf p)

      onExp ::
        S.Set VName ->
        Exp ->
        StateT (S.Set VName) (Either TypeError) Exp

      onExp known (Var v (Info t) loc)
        | Just bad <- checkCausality (pquote (ppr v)) known t loc =
          bad
      onExp known (ProjectSection _ (Info t) loc)
        | Just bad <- checkCausality "projection section" known t loc =
          bad
      onExp known (IndexSection _ (Info t) loc)
        | Just bad <- checkCausality "projection section" known t loc =
          bad
      onExp known (OpSectionRight _ (Info t) _ _ _ loc)
        | Just bad <- checkCausality "operator section" known t loc =
          bad
      onExp known (OpSectionLeft _ (Info t) _ _ _ loc)
        | Just bad <- checkCausality "operator section" known t loc =
          bad
      onExp known (ArrayLit [] (Info t) loc)
        | Just bad <- checkCausality "empty array" known t loc =
          bad
      onExp known (Lambda params _ _ _ _)
        | bad : _ <- mapMaybe (checkParamCausality known) params =
          bad
      onExp known e@(AppExp (LetPat _ _ bindee_e body_e _) (Info res)) = do
        sequencePoint known bindee_e body_e $ appResExt res
        pure e
      onExp known e@(AppExp (Apply f arg (Info (_, p)) _) (Info res)) = do
        sequencePoint known arg f $ maybeToList p ++ appResExt res
        pure e
      onExp
        known
        e@(AppExp (BinOp (f, floc) ft (x, Info (_, xp)) (y, Info (_, yp)) _) (Info res)) = do
          args_known <-
            lift $
              execStateT (sequencePoint known x y $ catMaybes [xp, yp]) mempty
          void $ onExp (args_known <> known) (Var f ft floc)
          modify ((args_known <> S.fromList (appResExt res)) <>)
          pure e
      onExp known e@(AppExp e' (Info res)) = do
        recurse known e'
        modify (<> S.fromList (appResExt res))
        pure e
      onExp known e = do
        recurse known e
        pure e

      recurse known = void . astMap mapper
        where
          mapper = identityMapper {mapOnExp = onExp known}

      sequencePoint known x y ext = do
        new_known <- lift $ execStateT (onExp known x) mempty
        void $ onExp (new_known <> known) y
        modify ((new_known <> S.fromList ext) <>)

  either throwError (const $ pure ()) $
    evalStateT (onExp mempty binding_body) mempty
  where
    unknown constraints known v = do
      guard $ v `S.notMember` known
      loc <- unknowable constraints v
      pure (v, loc)

    unknowable constraints v =
      case snd <$> M.lookup v constraints of
        Just (UnknowableSize loc _) -> Just loc
        _ -> Nothing

    causality what loc d dloc t =
      Left . TypeError loc mempty . withIndexLink "causality-check" $
        "Causality check: size" <+/> pquote (pprName d)
          <+/> "needed for type of"
          <+> what <> colon
          </> indent 2 (ppr t)
          </> "But"
          <+> pquote (pprName d)
          <+> "is computed at"
          <+/> text (locStrRel loc dloc) <> "."
          </> ""
          </> "Hint:"
          <+> align
            ( textwrap "Bind the expression producing" <+> pquote (pprName d)
                <+> "with 'let' beforehand."
            )

-- | Traverse the expression, emitting warnings if any of the literals overflow
-- their inferred types
--
-- Note: currently unable to detect float underflow (such as 1e-400 -> 0)
literalOverflowCheck :: Exp -> TermTypeM ()
literalOverflowCheck = void . check
  where
    check e@(IntLit x ty loc) =
      e <$ case ty of
        Info (Scalar (Prim t)) -> warnBounds (inBoundsI x t) x t loc
        _ -> error "Inferred type of int literal is not a number"
    check e@(FloatLit x ty loc) =
      e <$ case ty of
        Info (Scalar (Prim (FloatType t))) -> warnBounds (inBoundsF x t) x t loc
        _ -> error "Inferred type of float literal is not a float"
    check e@(Negate (IntLit x ty loc1) loc2) =
      e <$ case ty of
        Info (Scalar (Prim t)) -> warnBounds (inBoundsI (- x) t) (- x) t (loc1 <> loc2)
        _ -> error "Inferred type of int literal is not a number"
    check e = astMap identityMapper {mapOnExp = check} e
    bitWidth ty = 8 * intByteSize ty :: Int
    inBoundsI x (Signed t) = x >= -2 ^ (bitWidth t - 1) && x < 2 ^ (bitWidth t - 1)
    inBoundsI x (Unsigned t) = x >= 0 && x < 2 ^ bitWidth t
    inBoundsI x (FloatType Float16) = not $ isInfinite (fromIntegral x :: Half)
    inBoundsI x (FloatType Float32) = not $ isInfinite (fromIntegral x :: Float)
    inBoundsI x (FloatType Float64) = not $ isInfinite (fromIntegral x :: Double)
    inBoundsI _ Bool = error "Inferred type of int literal is not a number"
    inBoundsF x Float16 = not $ isInfinite (realToFrac x :: Float)
    inBoundsF x Float32 = not $ isInfinite (realToFrac x :: Float)
    inBoundsF x Float64 = not $ isInfinite x
    warnBounds inBounds x ty loc =
      unless inBounds $
        typeError loc mempty . withIndexLink "literal-out-of-bounds" $
          "Literal " <> ppr x
            <> " out of bounds for inferred type "
            <> ppr ty
            <> "."

-- | Type-check a top-level (or module-level) function definition.
-- Despite the name, this is also used for checking constant
-- definitions, by treating them as 0-ary functions.
checkFunDef ::
  ( Name,
    Maybe UncheckedTypeExp,
    [UncheckedTypeParam],
    [UncheckedPat],
    UncheckedExp,
    SrcLoc
  ) ->
  TypeM
    ( VName,
      [TypeParam],
      [Pat],
      Maybe (TypeExp VName),
      StructRetType,
      Exp
    )
checkFunDef (fname, maybe_retdecl, tparams, params, body, loc) =
  fmap fst . runTermTypeM $ do
    (tparams', params', maybe_retdecl', RetType dims rettype', body') <-
      checkBinding (fname, maybe_retdecl, tparams, params, body, loc)

    -- Since this is a top-level function, we also resolve overloaded
    -- types, using either defaults or complaining about ambiguities.
    fixOverloadedTypes $
      typeVars rettype' <> foldMap (typeVars . patternType) params'

    -- Then replace all inferred types in the body and parameters.
    body'' <- updateTypes body'
    params'' <- updateTypes params'
    maybe_retdecl'' <- traverse updateTypes maybe_retdecl'
    rettype'' <- normTypeFully rettype'

    -- Check if pattern matches are exhaustive and yield
    -- errors if not.
    checkUnmatched body''

    -- Check if the function body can actually be evaluated.
    causalityCheck body''

    literalOverflowCheck body''

    bindSpaced [(Term, fname)] $ do
      fname' <- checkName Term fname loc
      when (nameToString fname `elem` doNotShadow) $
        typeError loc mempty . withIndexLink "may-not-be-redefined" $
          "The" <+> pprName fname <+> "operator may not be redefined."

      pure (fname', tparams', params'', maybe_retdecl'', RetType dims rettype'', body'')

-- | This is "fixing" as in "setting them", not "correcting them".  We
-- only make very conservative fixing.
fixOverloadedTypes :: Names -> TermTypeM ()
fixOverloadedTypes tyvars_at_toplevel =
  getConstraints >>= mapM_ fixOverloaded . M.toList . M.map snd
  where
    fixOverloaded (v, Overloaded ots usage)
      | Signed Int32 `elem` ots = do
        unify usage (Scalar (TypeVar () Nonunique (typeName v) [])) $
          Scalar $ Prim $ Signed Int32
        when (v `S.member` tyvars_at_toplevel) $
          warn usage "Defaulting ambiguous type to i32."
      | FloatType Float64 `elem` ots = do
        unify usage (Scalar (TypeVar () Nonunique (typeName v) [])) $
          Scalar $ Prim $ FloatType Float64
        when (v `S.member` tyvars_at_toplevel) $
          warn usage "Defaulting ambiguous type to f64."
      | otherwise =
        typeError usage mempty . withIndexLink "ambiguous-type" $
          "Type is ambiguous (could be one of" <+> commasep (map ppr ots) <> ")."
            </> "Add a type annotation to disambiguate the type."
    fixOverloaded (v, NoConstraint _ usage) = do
      -- See #1552.
      unify usage (Scalar (TypeVar () Nonunique (typeName v) [])) $
        Scalar $ tupleRecord []
      when (v `S.member` tyvars_at_toplevel) $
        warn usage "Defaulting ambiguous type to ()."
    fixOverloaded (_, Equality usage) =
      typeError usage mempty . withIndexLink "ambiguous-type" $
        "Type is ambiguous (must be equality type)."
          </> "Add a type annotation to disambiguate the type."
    fixOverloaded (_, HasFields fs usage) =
      typeError usage mempty . withIndexLink "ambiguous-type" $
        "Type is ambiguous.  Must be record with fields:"
          </> indent 2 (stack $ map field $ M.toList fs)
          </> "Add a type annotation to disambiguate the type."
      where
        field (l, t) = ppr l <> colon <+> align (ppr t)
    fixOverloaded (_, HasConstrs cs usage) =
      typeError usage mempty . withIndexLink "ambiguous-type" $
        "Type is ambiguous (must be a sum type with constructors:"
          <+> ppr (Sum cs) <> ")."
          </> "Add a type annotation to disambiguate the type."
    fixOverloaded (v, Size Nothing usage) =
      typeError usage mempty $ "Size" <+> pquote (pprName v) <+> "is ambiguous.\n"
    fixOverloaded _ = pure ()

hiddenParamNames :: [Pat] -> Names
hiddenParamNames params = hidden
  where
    param_all_names = mconcat $ map patNames params
    named (Named x, _) = Just x
    named (Unnamed, _) = Nothing
    param_names =
      S.fromList $ mapMaybe (named . patternParam) params
    hidden = param_all_names `S.difference` param_names

inferredReturnType :: SrcLoc -> [Pat] -> PatType -> TermTypeM StructType
inferredReturnType loc params t =
  -- The inferred type may refer to names that are bound by the
  -- parameter patterns, but which will not be visible in the type.
  -- These we must turn into fresh type variables, which will be
  -- existential in the return type.
  fmap (toStruct . fst) $
    unscopeType
      loc
      (M.filterWithKey (const . (`S.member` hidden)) $ foldMap patternMap params)
      $ inferReturnUniqueness params t
  where
    hidden = hiddenParamNames params

checkReturnAlias :: SrcLoc -> TypeBase () () -> [Pat] -> PatType -> TermTypeM ()
checkReturnAlias loc rettp params =
  foldM_ (checkReturnAlias' params) S.empty . returnAliasing rettp
  where
    checkReturnAlias' params' seen (Unique, names)
      | any (`S.member` S.map snd seen) $ S.toList names =
        uniqueReturnAliased loc
      | otherwise = do
        notAliasingParam params' names
        pure $ seen `S.union` tag Unique names
    checkReturnAlias' _ seen (Nonunique, names)
      | any (`S.member` seen) $ S.toList $ tag Unique names =
        uniqueReturnAliased loc
      | otherwise = pure $ seen `S.union` tag Nonunique names

    notAliasingParam params' names =
      forM_ params' $ \p ->
        let consumedNonunique p' =
              not (unique $ unInfo $ identType p') && (identName p' `S.member` names)
         in case find consumedNonunique $ S.toList $ patIdents p of
              Just p' ->
                returnAliased (baseName $ identName p') loc
              Nothing ->
                pure ()

    tag u = S.map (u,)

    returnAliasing (Scalar (Record ets1)) (Scalar (Record ets2)) =
      concat $ M.elems $ M.intersectionWith returnAliasing ets1 ets2
    returnAliasing expected got =
      [(uniqueness expected, S.map aliasVar $ aliases got)]

checkBinding ::
  ( Name,
    Maybe UncheckedTypeExp,
    [UncheckedTypeParam],
    [UncheckedPat],
    UncheckedExp,
    SrcLoc
  ) ->
  TermTypeM
    ( [TypeParam],
      [Pat],
      Maybe (TypeExp VName),
      StructRetType,
      Exp
    )
checkBinding (fname, maybe_retdecl, tparams, params, body, loc) =
  noUnique . incLevel . bindingParams tparams params $ \tparams' params' -> do
    maybe_retdecl' <- traverse checkTypeExpNonrigid maybe_retdecl

    body' <-
      checkFunBody
        params'
        body
        ((\(_, x, _) -> x) <$> maybe_retdecl')
        (maybe loc srclocOf maybe_retdecl)

    params'' <- mapM updateTypes params'
    body_t <- expTypeFully body'

    (maybe_retdecl'', rettype) <- case maybe_retdecl' of
      Just (retdecl', ret, _) -> do
        let rettype_structural = toStructural ret
        checkReturnAlias loc rettype_structural params'' body_t

        when (null params) $ nothingMustBeUnique loc rettype_structural

        ret' <- normTypeFully ret

        pure (Just retdecl', ret')
      Nothing
        | null params ->
          pure (Nothing, toStruct $ body_t `setUniqueness` Nonunique)
        | otherwise -> do
          body_t' <- inferredReturnType loc params'' body_t
          pure (Nothing, body_t')

    verifyFunctionParams (Just fname) params''

    (tparams'', params''', rettype'') <-
      letGeneralise fname loc tparams' params'' rettype

    checkGlobalAliases params'' body_t loc

    pure (tparams'', params''', maybe_retdecl'', rettype'', body')

-- | Extract all the shape names that occur in positive position
-- (roughly, left side of an arrow) in a given type.
typeDimNamesPos :: TypeBase (DimDecl VName) als -> S.Set VName
typeDimNamesPos (Scalar (Arrow _ _ t1 (RetType _ t2))) = onParam t1 <> typeDimNamesPos t2
  where
    onParam :: TypeBase (DimDecl VName) als -> S.Set VName
    onParam (Scalar Arrow {}) = mempty
    onParam (Scalar (Record fs)) = mconcat $ map onParam $ M.elems fs
    onParam (Scalar (TypeVar _ _ _ targs)) = mconcat $ map onTypeArg targs
    onParam t = typeDimNames t
    onTypeArg (TypeArgDim (NamedDim d) _) = S.singleton $ qualLeaf d
    onTypeArg (TypeArgDim _ _) = mempty
    onTypeArg (TypeArgType t _) = onParam t
typeDimNamesPos _ = mempty

checkGlobalAliases :: [Pat] -> PatType -> SrcLoc -> TermTypeM ()
checkGlobalAliases params body_t loc = do
  vtable <- asks $ scopeVtable . termScope
  let isLocal v = case v `M.lookup` vtable of
        Just (BoundV Local _ _) -> True
        _ -> False
  let als =
        filter (not . isLocal) . S.toList $
          boundArrayAliases body_t `S.difference` foldMap patNames params
  case als of
    v : _
      | not $ null params ->
        typeError loc mempty . withIndexLink "alias-free-variable" $
          "Function result aliases the free variable "
            <> pquote (pprName v)
            <> "."
            </> "Use" <+> pquote "copy" <+> "to break the aliasing."
    _ ->
      pure ()

inferReturnUniqueness :: [Pat] -> PatType -> PatType
inferReturnUniqueness params t =
  let forbidden = aliasesMultipleTimes t
      uniques = uniqueParamNames params
      delve (Scalar (Record fs)) =
        Scalar $ Record $ M.map delve fs
      delve t'
        | all (`S.member` uniques) (boundArrayAliases t'),
          not $ any ((`S.member` forbidden) . aliasVar) (aliases t') =
          t'
        | otherwise =
          t' `setUniqueness` Nonunique
   in delve t

-- An alias inhibits uniqueness if it is used in disjoint values.
aliasesMultipleTimes :: PatType -> Names
aliasesMultipleTimes = S.fromList . map fst . filter ((> 1) . snd) . M.toList . delve
  where
    delve (Scalar (Record fs)) =
      foldl' (M.unionWith (+)) mempty $ map delve $ M.elems fs
    delve t =
      M.fromList $ zip (map aliasVar $ S.toList (aliases t)) $ repeat (1 :: Int)

uniqueParamNames :: [Pat] -> Names
uniqueParamNames =
  S.map identName
    . S.filter (unique . unInfo . identType)
    . foldMap patIdents

boundArrayAliases :: PatType -> S.Set VName
boundArrayAliases (Array als _ _ _) = boundAliases als
boundArrayAliases (Scalar Prim {}) = mempty
boundArrayAliases (Scalar (Record fs)) = foldMap boundArrayAliases fs
boundArrayAliases (Scalar (TypeVar als _ _ _)) = boundAliases als
boundArrayAliases (Scalar Arrow {}) = mempty
boundArrayAliases (Scalar (Sum fs)) =
  mconcat $ concatMap (map boundArrayAliases) $ M.elems fs

nothingMustBeUnique :: SrcLoc -> TypeBase () () -> TermTypeM ()
nothingMustBeUnique loc = check
  where
    check (Array _ Unique _ _) = bad
    check (Scalar (TypeVar _ Unique _ _)) = bad
    check (Scalar (Record fs)) = mapM_ check fs
    check (Scalar (Sum fs)) = mapM_ (mapM_ check) fs
    check _ = pure ()
    bad = typeError loc mempty "A top-level constant cannot have a unique type."

-- | Verify certain restrictions on function parameters, and bail out
-- on dubious constructions.
--
-- These restrictions apply to all functions (anonymous or otherwise).
-- Top-level functions have further restrictions that are checked
-- during let-generalisation.
verifyFunctionParams :: Maybe Name -> [Pat] -> TermTypeM ()
verifyFunctionParams fname params =
  onFailure (CheckingParams fname) $
    verifyParams (foldMap patNames params) =<< mapM updateTypes params
  where
    verifyParams forbidden (p : ps)
      | d : _ <- S.toList $ patternDimNames p `S.intersection` forbidden =
        typeError p mempty . withIndexLink "inaccessible-size" $
          "Parameter" <+> pquote (ppr p)
            <+/> "refers to size" <+> pquote (pprName d)
            <> comma
            <+/> textwrap "which will not be accessible to the caller"
            <> comma
            <+/> textwrap "possibly because it is nested in a tuple or record."
            <+/> textwrap "Consider ascribing an explicit type that does not reference "
            <> pquote (pprName d)
            <> "."
      | otherwise = verifyParams forbidden' ps
      where
        forbidden' =
          case patternParam p of
            (Named v, _) -> forbidden `S.difference` S.singleton v
            _ -> forbidden
    verifyParams _ [] = pure ()

-- | Move existentials down to the level where they are actually used
-- (i.e. have their "witnesses").  E.g. changes
--
-- @
-- ?[n].bool -> [n]bool
-- @
--
-- to
--
-- @
-- bool -> ?[n].[n]bool
-- @
injectExt :: [VName] -> StructType -> StructRetType
injectExt [] ret = RetType [] ret
injectExt ext ret = RetType ext_here $ deeper ret
  where
    (immediate, _) = dimUses ret
    (ext_here, ext_there) = partition (`S.member` immediate) ext
    deeper (Scalar (Prim t)) = Scalar $ Prim t
    deeper (Scalar (Record fs)) = Scalar $ Record $ M.map deeper fs
    deeper (Scalar (Sum cs)) = Scalar $ Sum $ M.map (map deeper) cs
    deeper (Scalar (Arrow als p t1 (RetType t2_ext t2))) =
      Scalar $ Arrow als p t1 $ injectExt (ext_there <> t2_ext) t2
    deeper (Scalar (TypeVar as u tn targs)) =
      Scalar $ TypeVar as u tn $ map deeperArg targs
    deeper t@Array {} = t

    deeperArg (TypeArgType t loc) = TypeArgType (deeper t) loc
    deeperArg (TypeArgDim d loc) = TypeArgDim d loc

-- | Find all type variables in the given type that are covered by the
-- constraints, and produce type parameters that close over them.
--
-- The passed-in list of type parameters is always prepended to the
-- produced list of type parameters.
closeOverTypes ::
  Name ->
  SrcLoc ->
  [TypeParam] ->
  [StructType] ->
  StructType ->
  Constraints ->
  TermTypeM ([TypeParam], StructRetType)
closeOverTypes defname defloc tparams paramts ret substs = do
  (more_tparams, retext) <-
    partitionEithers . catMaybes
      <$> mapM closeOver (M.toList $ M.map snd to_close_over)
  let mkExt (NamedDim v) =
        case M.lookup (qualLeaf v) substs of
          Just (_, UnknowableSize {}) -> Just $ qualLeaf v
          _ -> Nothing
      mkExt ConstDim {} = Nothing
      mkExt AnyDim {} = error "closeOverTypes: AnyDim"
  return
    ( tparams ++ more_tparams,
      injectExt (retext ++ mapMaybe mkExt (nestedDims ret)) ret
    )
  where
    t = foldFunType paramts $ RetType [] ret
    to_close_over = M.filterWithKey (\k _ -> k `S.member` visible) substs
    visible = typeVars t <> typeDimNames t

    (produced_sizes, param_sizes) = dimUses t

    -- Avoid duplicate type parameters.
    closeOver (k, _)
      | k `elem` map typeParamName tparams =
        pure Nothing
    closeOver (k, NoConstraint l usage) =
      pure $ Just $ Left $ TypeParamType l k $ srclocOf usage
    closeOver (k, ParamType l loc) =
      pure $ Just $ Left $ TypeParamType l k loc
    closeOver (k, Size Nothing usage) =
      pure $ Just $ Left $ TypeParamDim k $ srclocOf usage
    closeOver (k, UnknowableSize _ _)
      | k `S.member` param_sizes,
        k `S.notMember` produced_sizes = do
        notes <- dimNotes defloc $ NamedDim $ qualName k
        typeError defloc notes . withIndexLink "unknowable-param-def" $
          "Unknowable size" <+> pquote (pprName k)
            <+> "in parameter of"
            <+> pquote (pprName defname)
            <> ", which is inferred as:"
            </> indent 2 (ppr t)
      | k `S.member` produced_sizes =
        pure $ Just $ Right k
    closeOver (_, _) =
      pure Nothing

letGeneralise ::
  Name ->
  SrcLoc ->
  [TypeParam] ->
  [Pat] ->
  StructType ->
  TermTypeM ([TypeParam], [Pat], StructRetType)
letGeneralise defname defloc tparams params rettype =
  onFailure (CheckingLetGeneralise defname) $ do
    now_substs <- getConstraints

    -- Candidates for let-generalisation are those type variables that
    --
    -- (1) were not known before we checked this function, and
    --
    -- (2) are not used in the (new) definition of any type variables
    -- known before we checked this function.
    --
    -- (3) are not referenced from an overloaded type (for example,
    -- are the element types of an incompletely resolved record type).
    -- This is a bit more restrictive than I'd like, and SML for
    -- example does not have this restriction.
    --
    -- Criteria (1) and (2) is implemented by looking at the binding
    -- level of the type variables.
    let keep_type_vars = overloadedTypeVars now_substs

    cur_lvl <- curLevel
    let candidate k (lvl, _) = (k `S.notMember` keep_type_vars) && lvl >= cur_lvl
        new_substs = M.filterWithKey candidate now_substs

    (tparams', RetType ret_dims rettype') <-
      closeOverTypes
        defname
        defloc
        tparams
        (map patternStructType params)
        rettype
        new_substs

    rettype'' <- updateTypes rettype'

    let used_sizes =
          foldMap typeDimNames $ rettype'' : map patternStructType params
    case filter ((`S.notMember` used_sizes) . typeParamName) $
      filter isSizeParam tparams' of
      [] -> pure ()
      tp : _ -> unusedSize $ SizeBinder (typeParamName tp) (srclocOf tp)

    -- We keep those type variables that were not closed over by
    -- let-generalisation.
    modifyConstraints $ M.filterWithKey $ \k _ -> k `notElem` map typeParamName tparams'

    pure (tparams', params, RetType ret_dims rettype'')

checkFunBody ::
  [Pat] ->
  UncheckedExp ->
  Maybe StructType ->
  SrcLoc ->
  TermTypeM Exp
checkFunBody params body maybe_rettype loc = do
  body' <- noSizeEscape $ checkExp body

  -- Unify body return type with return annotation, if one exists.
  case maybe_rettype of
    Just rettype -> do
      body_t <- expTypeFully body'
      -- We need to turn any sizes provided by "hidden" parameter
      -- names into existential sizes instead.
      let hidden = hiddenParamNames params
      (body_t', _) <-
        unscopeType
          loc
          ( M.filterWithKey (const . (`S.member` hidden)) $
              foldMap patternMap params
          )
          body_t

      let usage = mkUsage (srclocOf body) "return type annotation"
      onFailure (CheckingReturn rettype (toStruct body_t')) $
        expect usage rettype $ toStruct body_t'

      -- We also have to make sure that uniqueness matches.  This is done
      -- explicitly, because uniqueness is ignored by unification.
      rettype' <- normTypeFully rettype
      body_t'' <- normTypeFully body_t' -- Substs may have changed.
      unless (toStructural body_t'' `subtypeOf` toStructural rettype') $
        typeError (srclocOf body) mempty $
          "Body type" </> indent 2 (ppr body_t'')
            </> "is not a subtype of annotated type"
            </> indent 2 (ppr rettype')
    Nothing -> pure ()

  pure body'

arrayOfM ::
  (Pretty (ShapeDecl dim), Monoid as) =>
  SrcLoc ->
  TypeBase dim as ->
  ShapeDecl dim ->
  Uniqueness ->
  TermTypeM (TypeBase dim as)
arrayOfM loc t shape u = do
  arrayElemType (mkUsage loc "use as array element") "type used in array" t
  pure $ arrayOf t shape u
