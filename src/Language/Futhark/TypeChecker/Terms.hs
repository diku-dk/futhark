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

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bitraversable
import Data.Either
import Data.List (find, foldl', genericLength, partition)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Util (mapAccumLM)
import Futhark.Util.Pretty hiding (space)
import Language.Futhark
import Language.Futhark.Primitive (intByteSize)
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Match
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Terms.DoLoop
import Language.Futhark.TypeChecker.Terms.Monad
import Language.Futhark.TypeChecker.Terms.Pat
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify
import Prelude hiding (mod)

overloadedTypeVars :: Constraints -> Names
overloadedTypeVars = mconcat . map f . M.elems
  where
    f (_, HasFields _ fs _) = mconcat $ map typeVars $ M.elems fs
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
  TypeBase Size as ->
  TermTypeM (TypeBase Size as, [VName])
sliceShape r slice t@(Array als u (Shape orig_dims) et) =
  runStateT (setDims <$> adjustDims slice orig_dims) []
  where
    setDims [] = stripArray (length orig_dims) t
    setDims dims' = Array als u (Shape dims') et

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
          lift $ NamedSize . qualName <$> newDimVar loc Nonrigid "slice_dim"
        Nothing -> do
          v <- lift $ newID "slice_anydim"
          modify (v :)
          pure $ NamedSize $ qualName v
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
  let isGlobal v = case v `M.lookup` vtable of
        Just (BoundV Global _ _) -> True
        Just EqualityF {} -> True
        Just OverloadedF {} -> True
        Just (BoundV Local _ _) -> False
        Just (BoundV Nonlocal _ _) -> False
        Just WasConsumed {} -> False
        Nothing -> False
  pure . S.map AliasBound . S.filter (not . isGlobal) $
    allOccurring closure S.\\ mconcat (map patNames params)

noAliasesIfOverloaded :: PatType -> TermTypeM PatType
noAliasesIfOverloaded t@(Scalar (TypeVar _ u tn [])) = do
  subst <- fmap snd . M.lookup (qualLeaf tn) <$> getConstraints
  case subst of
    Just Overloaded {} -> pure $ Scalar $ TypeVar mempty u tn []
    _ -> pure t
noAliasesIfOverloaded t =
  pure t

checkAscript ::
  SrcLoc ->
  UncheckedTypeExp ->
  UncheckedExp ->
  TermTypeM (TypeExp Info VName, Exp)
checkAscript loc te e = do
  (te', decl_t, _) <- checkTypeExpNonrigid te
  e' <- checkExp e
  e_t <- toStruct <$> expTypeFully e'

  onFailure (CheckingAscription decl_t e_t) $
    unify (mkUsage loc "type ascription") decl_t e_t

  pure (te', e')

checkCoerce ::
  SrcLoc ->
  UncheckedTypeExp ->
  UncheckedExp ->
  TermTypeM (TypeExp Info VName, StructType, Exp)
checkCoerce loc te e = do
  (te', te_t, ext) <- checkTypeExpNonrigid te
  e' <- checkExp e
  e_t <- toStruct <$> expTypeFully e'

  te_t_nonrigid <- makeNonExtFresh ext te_t

  onFailure (CheckingAscription te_t e_t) $
    unify (mkUsage loc "size coercion") e_t te_t_nonrigid

  -- If the type expression had any anonymous dimensions, these will
  -- now be in 'ext'.  Those we keep nonrigid and unify with e_t.
  -- This ensures that 'x :> [1][]i32' does not make the second
  -- dimension unknown.  Use of matchDims is sensible because the
  -- structure of e_t' will be fully known due to the unification, and
  -- te_t because type expressions are complete.
  pure (te', te_t, e')
  where
    makeNonExtFresh ext = bitraverse onDim pure
      where
        onDim d@(NamedSize v)
          | qualLeaf v `elem` ext = pure d
        onDim _ = do
          v <- newTypeName "coerce"
          constrain v . Size Nothing $
            mkUsage
              loc
              "a size coercion where the underlying expression size cannot be determined"
          pure $ NamedSize $ qualName v

unscopeType ::
  SrcLoc ->
  M.Map VName Ident ->
  PatType ->
  TermTypeM (PatType, [VName])
unscopeType tloc unscoped t = do
  (t', m) <- runStateT (traverseDims onDim t) mempty
  pure (t' `addAliases` S.map unAlias, M.elems m)
  where
    onDim bound _ (NamedSize d)
      | Just loc <- srclocOf <$> M.lookup (qualLeaf d) unscoped,
        not $ qualLeaf d `S.member` bound =
          inst loc $ qualLeaf d
    onDim _ _ d = pure d

    inst loc d = do
      prev <- gets $ M.lookup d
      case prev of
        Just d' -> pure $ NamedSize $ qualName d'
        Nothing -> do
          d' <- lift $ newDimVar tloc (Rigid $ RigidOutOfScope loc d) "d"
          modify $ M.insert d d'
          pure $ NamedSize $ qualName d'

    unAlias (AliasBound v) | v `M.member` unscoped = AliasFree v
    unAlias a = a

checkExp :: UncheckedExp -> TermTypeM Exp
checkExp (Literal val loc) =
  pure $ Literal val loc
checkExp (Hole _ loc) = do
  t <- newTypeVar loc "t"
  pure $ Hole (Info t) loc
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
            "Field"
              <+> dquotes (pretty f)
              <+> "previously defined at"
              <+> pretty (locStrRel rloc sloc) <> "."
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
      t <- arrayOfM loc et (Shape [ConstSize 0]) Nonunique
      pure $ ArrayLit [] (Info t) loc
    e : es -> do
      e' <- checkExp e
      et <- expType e'
      es' <- mapM (unifies "type of first array element" (toStruct et) <=< checkExp) es
      et' <- normTypeFully et
      t <- arrayOfM loc et' (Shape [ConstSize $ genericLength all_es]) Nonunique
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
        pure (NamedSize $ qualName d, Just d)

  t <- arrayOfM loc start_t (Shape [dim]) Nonunique
  let res = AppRes (t `setAliases` mempty) (maybeToList retext)

  pure $ AppExp (Range start' maybe_step' end' loc) (Info res)
checkExp (Ascript e te loc) = do
  (te', e') <- checkAscript loc te e
  pure $ Ascript e' te' loc
checkExp (AppExp (Coerce e te loc) _) = do
  (te', te_t, e') <- checkCoerce loc te e
  t <- expTypeFully e'
  t' <- matchDims (const . const pure) t $ fromStruct te_t
  pure $ AppExp (Coerce e' te' loc) (Info $ AppRes t' [])
checkExp (AppExp (BinOp (op, oploc) NoInfo (e1, _) (e2, _) loc) NoInfo) = do
  (op', ftype) <- lookupVar oploc op
  e1_arg <- checkArg e1
  e2_arg <- checkArg e2

  -- Note that the application to the first operand cannot fix any
  -- existential sizes, because it must by necessity be a function.
  (_, p1_t, rt, p1_ext, _) <- checkApply loc (Just op', 0) ftype e1_arg
  (_, p2_t, rt', p2_ext, retext) <- checkApply loc (Just op', 1) rt e2_arg

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
  kt <- mustHaveField (mkUsage loc $ docText $ "projection of field " <> dquotes (pretty k)) k t
  pure $ Project k e' (Info kt) loc
checkExp (AppExp (If e1 e2 e3 loc) _) =
  sequentially checkCond $ \e1' _ -> do
    ((e2', e3'), dflow) <- tapOccurrences $ checkExp e2 `alternative` checkExp e3

    (brancht, retext) <- unifyBranches loc e2' e3'
    let t' = addAliases brancht $ S.filter $ (`S.notMember` allConsumed dflow) . aliasVar

    zeroOrderType
      (mkUsage loc "returning value of this type from 'if' expression")
      "type returned from branch"
      (toStruct t')

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
        "Module" <+> pretty modname <+> " is a parametric module."
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
      let usage = mkUsage loc $ docText $ "projection of field " <> dquotes (pretty k)
      kt <- mustHaveField usage k t
      pure $ Project k e (Info kt) loc
checkExp (Negate arg loc) = do
  arg' <- require "numeric negation" anyNumberType =<< checkExp arg
  pure $ Negate arg' loc
checkExp (Not arg loc) = do
  arg' <- require "logical negation" (Bool : anyIntType) =<< checkExp arg
  pure $ Not arg' loc
checkExp (AppExp (Apply fe args loc) NoInfo) = do
  fe' <- checkExp fe
  args' <- mapM (checkArg . snd) args
  t <- expType fe'
  let fname =
        case fe' of
          Var v _ _ -> Just v
          _ -> Nothing
  ((_, exts, rt), args'') <- mapAccumLM (onArg fname) (0, [], t) args'
  pure $ AppExp (Apply fe' args'' loc) $ Info $ AppRes rt exts
  where
    onArg fname (i, all_exts, t) arg' = do
      (d1, _, rt, argext, exts) <- checkApply loc (fname, i) t arg'
      pure
        ( (i + 1, all_exts <> exts, rt),
          (Info (d1, argext), argExp arg')
        )
checkExp (AppExp (LetPat sizes pat e body loc) _) =
  sequentially (checkExp e) $ \e' e_occs -> do
    -- Not technically an ascription, but we want the pattern to have
    -- exactly the type of 'e'.
    t <- expType e'
    case anyConsumption e_occs of
      Just c ->
        zeroOrderType
          (mkUsage loc "consumption in right-hand side of 'let'-binding")
          ("type computed with consumption at " <> locText (location c))
          (toStruct t)
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

        let ftype = funType params' rettype
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
  sequentially ((,) <$> checkIdent src <*> checkSlice slice) $ \(src', slice') _ -> do
    (t, _) <- newArrayType (srclocOf src) "src" $ sliceDims slice'
    unify (mkUsage loc "type of target array") t $ toStruct $ unInfo $ identType src'

    -- Need the fully normalised type here to get the proper aliasing information.
    src_t <- normTypeFully $ unInfo $ identType src'

    (elemt, _) <- sliceShape (Just (loc, Nonrigid)) slice' =<< normTypeFully t

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
          </> indent 2 (pretty src)
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
  pure $ Assert e1' e2' (Info (prettyText e1)) loc
checkExp (Lambda params body rettype_te NoInfo loc) = do
  (params', body', body_t, rettype', info) <-
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

      closure' <- lexicalClosure params'' closure

      pure (params'', body', body_t, rettype', Info (closure', rettype_st))

  checkGlobalAliases params' body_t loc
  verifyFunctionParams Nothing params'

  pure $ Lambda params' body' rettype' info loc
  where
    -- Inferring the sizes of the return type of a lambda is a lot
    -- like let-generalisation.  We wish to remove any rigid sizes
    -- that were created when checking the body, except for those that
    -- are visible in types that existed before we entered the body,
    -- are parameters, or are used in parameters.
    inferReturnSizes params' ret = do
      cur_lvl <- curLevel
      let named (Named x, _, _) = Just x
          named (Unnamed, _, _) = Nothing
          param_names = mapMaybe (named . patternParam) params'
          pos_sizes =
            sizeNamesPos $ foldFunTypeFromParams params' $ RetType [] ret
          hide k (lvl, _) =
            lvl >= cur_lvl && k `notElem` param_names && k `S.notMember` pos_sizes

      hidden_sizes <-
        S.fromList . M.keys . M.filterWithKey hide <$> getConstraints

      let onDim name
            | name `S.member` hidden_sizes = S.singleton name
          onDim _ = mempty

      pure $ RetType (S.toList $ foldMap onDim $ freeInType ret) ret
checkExp (OpSection op _ loc) = do
  (op', ftype) <- lookupVar loc op
  pure $ OpSection op' (Info ftype) loc
checkExp (OpSectionLeft op _ e _ _ loc) = do
  (op', ftype) <- lookupVar loc op
  e_arg <- checkArg e
  (_, t1, rt, argext, retext) <- checkApply loc (Just op', 0) ftype e_arg
  case (ftype, rt) of
    (Scalar (Arrow _ m1 _ _ _), Scalar (Arrow _ m2 _ t2 rettype)) ->
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
        "Operator section with invalid operator of type" <+> pretty ftype
checkExp (OpSectionRight op _ e _ NoInfo loc) = do
  (op', ftype) <- lookupVar loc op
  e_arg <- checkArg e
  case ftype of
    Scalar (Arrow as1 m1 d1 t1 (RetType [] (Scalar (Arrow as2 m2 d2 t2 (RetType dims2 ret))))) -> do
      (_, t2', ret', argext, _) <-
        checkApply
          loc
          (Just op', 1)
          (Scalar $ Arrow as2 m2 d2 t2 $ RetType [] $ Scalar $ Arrow as1 m1 d1 t1 $ RetType [] ret)
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
        "Operator section with invalid operator of type" <+> pretty ftype
checkExp (ProjectSection fields NoInfo loc) = do
  a <- newTypeVar loc "a"
  let usage = mkUsage loc "projection at"
  b <- foldM (flip $ mustHaveField usage) a fields
  let ft = Scalar $ Arrow mempty Unnamed Observe (toStruct a) $ RetType [] b
  pure $ ProjectSection fields (Info ft) loc
checkExp (IndexSection slice NoInfo loc) = do
  slice' <- checkSlice slice
  (t, _) <- newArrayType loc "e" $ sliceDims slice'
  (t', retext) <- sliceShape Nothing slice' t
  let ft = Scalar $ Arrow mempty Unnamed Observe t $ RetType retext $ fromStruct t'
  pure $ IndexSection slice' (Info ft) loc
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
      (toStruct t)
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
      pure (NE.singleton c', t, retext)
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
  pretty um = case um of
    (UnmatchedNum p nums) -> pretty' p <+> "where p is not one of" <+> pretty nums
    (UnmatchedBool p) -> pretty' p
    (UnmatchedConstr p) -> pretty' p
    (Unmatched p) -> pretty' p
    where
      pretty' (PatAscription p t _) = pretty p <> ":" <+> pretty t
      pretty' (PatParens p _) = parens $ pretty' p
      pretty' (PatAttr _ p _) = parens $ pretty' p
      pretty' (Id v _ _) = prettyName v
      pretty' (TuplePat pats _) = parens $ commasep $ map pretty' pats
      pretty' (RecordPat fs _) = braces $ commasep $ map ppField fs
        where
          ppField (name, t) = pretty (nameToString name) <> equals <> pretty' t
      pretty' Wildcard {} = "_"
      pretty' (PatLit e _ _) = pretty e
      pretty' (PatConstr n _ ps _) = "#" <> pretty n <+> sep (map pretty' ps)

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
      maybe (pure Nothing) $
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
  RetTypeBase Size als ->
  TermTypeM (TypeBase Size als, [VName])
instantiateDimsInReturnType tloc fname =
  instantiateEmptyArrayDims tloc $ Rigid $ RigidRet fname

-- Some information about the function/operator we are trying to
-- apply, and how many arguments it has previously accepted.  Used for
-- generating nicer type errors.
type ApplyOp = (Maybe (QualName VName), Int)

-- | Extract all those names that are bound inside the type.
boundInsideType :: TypeBase Size as -> S.Set VName
boundInsideType (Array _ _ _ t) = boundInsideType (Scalar t)
boundInsideType (Scalar Prim {}) = mempty
boundInsideType (Scalar (TypeVar _ _ _ targs)) = foldMap f targs
  where
    f (TypeArgType t _) = boundInsideType t
    f TypeArgDim {} = mempty
boundInsideType (Scalar (Record fs)) = foldMap boundInsideType fs
boundInsideType (Scalar (Sum cs)) = foldMap (foldMap boundInsideType) cs
boundInsideType (Scalar (Arrow _ pn _ t1 (RetType dims t2))) =
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
    f bound _ (NamedSize v) | qualLeaf v `S.member` bound = pure ()
    f _ PosImmediate (NamedSize v) = modify ((S.singleton (qualLeaf v), mempty) <>)
    f _ PosParam (NamedSize v) = modify ((mempty, S.singleton (qualLeaf v)) <>)
    f _ _ _ = pure ()

checkApply ::
  SrcLoc ->
  ApplyOp ->
  PatType ->
  Arg ->
  TermTypeM (Diet, StructType, PatType, Maybe VName, [VName])
checkApply
  loc
  (fname, _)
  (Scalar (Arrow as pname d1 tp1 tp2))
  (argexp, argtype, dflow, argloc) =
    onFailure (CheckingApply fname argexp tp1 (toStruct argtype)) $ do
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
            </> indent 2 (pretty (RetType ext tp2'))
            </> textwrap "This is usually because a higher-order function is used with functional arguments that return existential sizes or locally named sizes, which are then used as parameters of other function arguments."

      occur [observation as loc]

      checkOccurrences dflow

      case anyConsumption dflow of
        Just c ->
          let msg = "type of expression with consumption at " <> locText (location c)
           in zeroOrderType (mkUsage argloc "potential consumption in expression") msg tp1
        _ -> pure ()

      arg_consumed <- consumedByArg (locOf argloc) argtype' d1
      checkIfConsumable loc $ mconcat arg_consumed
      occur $ dflow `seqOccurrences` map (`consumption` argloc) arg_consumed

      -- Unification ignores uniqueness in higher-order arguments, so
      -- we check for that here.
      unless (toStructural argtype' `subtypeOf` setUniqueness (toStructural tp1') Nonunique) $
        typeError loc mempty "Difference in whether argument is consumed."

      (argext, parsubst) <-
        case pname of
          Named pname'
            | (Scalar (Prim (Signed Int64))) <- tp1' -> do
                (d, argext) <- sizeFromArg fname argexp
                pure
                  ( argext,
                    (`M.lookup` M.singleton pname' (SizeSubst d))
                  )
          _ -> pure (Nothing, const Nothing)

      -- In case a function result is not immediately bound to a name,
      -- we need to invent a name for it so we can track it during
      -- aliasing (uniqueness-error54.fut, uniqueness-error55.fut,
      -- uniqueness-error60.fut).
      v <- newID "internal_app_result"
      modify $ \s -> s {stateNames = M.insert v (NameAppRes fname loc) $ stateNames s}
      let appres = S.singleton $ AliasFree v
      let tp2'' = applySubst parsubst $ returnType appres tp2' d1 argtype'

      pure (d1, tp1', tp2'', argext, ext)
checkApply loc fname tfun@(Scalar TypeVar {}) arg = do
  tv <- newTypeVar loc "b"
  -- Change the uniqueness of the argument type because we never want
  -- to infer that a function is consuming.
  let argt_nonunique = toStruct (argType arg) `setUniqueness` Nonunique
  unify (mkUsage loc "use as function") (toStruct tfun) $
    Scalar (Arrow mempty Unnamed Observe argt_nonunique $ RetType [] tv)
  tfun' <- normPatType tfun
  checkApply loc fname tfun' arg
checkApply loc (fname, prev_applied) ftype (argexp, _, _, _) = do
  let fname' = maybe "expression" (dquotes . pretty) fname

  typeError loc mempty $
    if prev_applied == 0
      then
        "Cannot apply"
          <+> fname'
          <+> "as function, as it has type:"
          </> indent 2 (pretty ftype)
      else
        "Cannot apply"
          <+> fname'
          <+> "to argument #" <> pretty (prev_applied + 1)
          <+> dquotes (shorten $ group $ pretty argexp) <> ","
          </> "as"
          <+> fname'
          <+> "only takes"
          <+> pretty prev_applied
          <+> arguments <> "."
  where
    arguments
      | prev_applied == 1 = "argument"
      | otherwise = "arguments"

aliasParts :: PatType -> [Aliasing]
aliasParts (Scalar (Record ts)) = foldMap aliasParts $ M.elems ts
aliasParts t = [aliases t]

consumedByArg :: Loc -> PatType -> Diet -> TermTypeM [Aliasing]
consumedByArg loc at Consume = do
  let parts = aliasParts at
  foldM_ check mempty parts
  pure parts
  where
    check seen als
      | any (`S.member` seen) als =
          typeError loc mempty . withIndexLink "self-aliasing-arg" $
            "Argument passed for consuming parameter is self-aliased."
      | otherwise = pure $ als <> seen
consumedByArg _ _ _ = pure []

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
  localChecks e''
  causalityCheck e''
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
              S.toList $
                freeInType $
                  toStruct t =
            Just $ lift $ causality what (locOf loc) d dloc t
        | otherwise = Nothing

      checkParamCausality known p =
        checkCausality (pretty p) known (patternType p) (locOf p)

      onExp ::
        S.Set VName ->
        Exp ->
        StateT (S.Set VName) (Either TypeError) Exp

      onExp known (Var v (Info t) loc)
        | Just bad <- checkCausality (dquotes (pretty v)) known t loc =
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
      onExp known (Hole (Info t) loc)
        | Just bad <- checkCausality "hole" known t loc =
            bad
      onExp known (Lambda params _ _ _ _)
        | bad : _ <- mapMaybe (checkParamCausality known) params =
            bad
      onExp known e@(AppExp (LetPat _ _ bindee_e body_e _) (Info res)) = do
        sequencePoint known bindee_e body_e $ appResExt res
        pure e
      onExp known e@(AppExp (Apply f args _) (Info res)) = do
        seqArgs known $ reverse $ NE.toList args
        pure e
        where
          seqArgs known' [] = do
            void $ onExp known' f
            modify (S.fromList (appResExt res) <>)
          seqArgs known' ((Info (_, p), x) : xs) = do
            new_known <- lift $ execStateT (onExp known' x) mempty
            void $ seqArgs (new_known <> known') xs
            modify ((new_known <> S.fromList (maybeToList p)) <>)
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
        "Causality check: size"
          </> dquotes (prettyName d)
          </> "needed for type of"
          <+> what <> colon
          </> indent 2 (pretty t)
          </> "But"
          <+> dquotes (prettyName d)
          <+> "is computed at"
          </> pretty (locStrRel loc dloc) <> "."
          </> ""
          </> "Hint:"
          <+> align
            ( textwrap "Bind the expression producing"
                <+> dquotes (prettyName d)
                <+> "with 'let' beforehand."
            )

-- | Traverse the expression, emitting warnings and errors for various
-- problems:
--
-- * Unmatched cases.
--
-- * If any of the literals overflow their inferred types. Note:
--  currently unable to detect float underflow (such as 1e-400 -> 0)
localChecks :: Exp -> TermTypeM ()
localChecks = void . check
  where
    check e@(AppExp (Match _ cs loc) _) = do
      let ps = fmap (\(CasePat p _ _) -> p) cs
      case unmatched $ NE.toList ps of
        [] -> recurse e
        ps' ->
          typeError loc mempty . withIndexLink "unmatched-cases" $
            "Unmatched cases in match expression:"
              </> indent 2 (stack (map pretty ps'))
    check e@(IntLit x ty loc) =
      e <$ case ty of
        Info (Scalar (Prim t)) -> errorBounds (inBoundsI x t) x t loc
        _ -> error "Inferred type of int literal is not a number"
    check e@(FloatLit x ty loc) =
      e <$ case ty of
        Info (Scalar (Prim (FloatType t))) -> errorBounds (inBoundsF x t) x t loc
        _ -> error "Inferred type of float literal is not a float"
    check e@(Negate (IntLit x ty loc1) loc2) =
      e <$ case ty of
        Info (Scalar (Prim t)) -> errorBounds (inBoundsI (-x) t) (-x) t (loc1 <> loc2)
        _ -> error "Inferred type of int literal is not a number"
    check e@(AppExp (BinOp (QualName [] v, _) _ (_, Info (Array {}, _)) _ loc) _)
      | baseName v == "==",
        baseTag v <= maxIntrinsicTag = do
          warn loc $
            textwrap
              "Comparing arrays with \"==\" is deprecated and will stop working in a future revision of the language."
          recurse e
    check e = recurse e
    recurse = astMap identityMapper {mapOnExp = check}

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

    errorBounds inBounds x ty loc =
      unless inBounds $
        typeError loc mempty . withIndexLink "literal-out-of-bounds" $
          "Literal "
            <> pretty x
            <> " out of bounds for inferred type "
            <> pretty ty
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
      Maybe (TypeExp Info VName),
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

    -- Check if the function body can actually be evaluated.
    causalityCheck body''

    -- Check for various problems.
    localChecks body''

    bindSpaced [(Term, fname)] $ do
      fname' <- checkName Term fname loc
      when (nameToString fname `elem` doNotShadow) $
        typeError loc mempty . withIndexLink "may-not-be-redefined" $
          "The" <+> prettyName fname <+> "operator may not be redefined."

      pure (fname', tparams', params'', maybe_retdecl'', RetType dims rettype'', body'')

-- | This is "fixing" as in "setting them", not "correcting them".  We
-- only make very conservative fixing.
fixOverloadedTypes :: Names -> TermTypeM ()
fixOverloadedTypes tyvars_at_toplevel =
  getConstraints >>= mapM_ fixOverloaded . M.toList . M.map snd
  where
    fixOverloaded (v, Overloaded ots usage)
      | Signed Int32 `elem` ots = do
          unify usage (Scalar (TypeVar () Nonunique (qualName v) [])) $
            Scalar $
              Prim $
                Signed Int32
          when (v `S.member` tyvars_at_toplevel) $
            warn usage "Defaulting ambiguous type to i32."
      | FloatType Float64 `elem` ots = do
          unify usage (Scalar (TypeVar () Nonunique (qualName v) [])) $
            Scalar $
              Prim $
                FloatType Float64
          when (v `S.member` tyvars_at_toplevel) $
            warn usage "Defaulting ambiguous type to f64."
      | otherwise =
          typeError usage mempty . withIndexLink "ambiguous-type" $
            "Type is ambiguous (could be one of"
              <+> commasep (map pretty ots) <> ")."
              </> "Add a type annotation to disambiguate the type."
    fixOverloaded (v, NoConstraint _ usage) = do
      -- See #1552.
      unify usage (Scalar (TypeVar () Nonunique (qualName v) [])) $
        Scalar $
          tupleRecord []
      when (v `S.member` tyvars_at_toplevel) $
        warn usage "Defaulting ambiguous type to ()."
    fixOverloaded (_, Equality usage) =
      typeError usage mempty . withIndexLink "ambiguous-type" $
        "Type is ambiguous (must be equality type)."
          </> "Add a type annotation to disambiguate the type."
    fixOverloaded (_, HasFields _ fs usage) =
      typeError usage mempty . withIndexLink "ambiguous-type" $
        "Type is ambiguous.  Must be record with fields:"
          </> indent 2 (stack $ map field $ M.toList fs)
          </> "Add a type annotation to disambiguate the type."
      where
        field (l, t) = pretty l <> colon <+> align (pretty t)
    fixOverloaded (_, HasConstrs _ cs usage) =
      typeError usage mempty . withIndexLink "ambiguous-type" $
        "Type is ambiguous (must be a sum type with constructors:"
          <+> pretty (Sum cs) <> ")."
          </> "Add a type annotation to disambiguate the type."
    fixOverloaded (v, Size Nothing (Usage Nothing loc)) =
      typeError loc mempty . withIndexLink "ambiguous-size" $
        "Ambiguous size" <+> dquotes (prettyName v) <> "."
    fixOverloaded (v, Size Nothing (Usage (Just u) loc)) =
      typeError loc mempty . withIndexLink "ambiguous-size" $
        "Ambiguous size" <+> dquotes (prettyName v) <+> "arising from" <+> pretty u <> "."
    fixOverloaded _ = pure ()

hiddenParamNames :: [Pat] -> Names
hiddenParamNames params = hidden
  where
    param_all_names = mconcat $ map patNames params
    named (Named x, _, _) = Just x
    named (Unnamed, _, _) = Nothing
    param_names =
      S.fromList $ mapMaybe (named . patternParam) params
    hidden = param_all_names `S.difference` param_names

inferredReturnType :: SrcLoc -> [Pat] -> PatType -> TermTypeM StructType
inferredReturnType loc params t = do
  -- The inferred type may refer to names that are bound by the
  -- parameter patterns, but which will not be visible in the type.
  -- These we must turn into fresh type variables, which will be
  -- existential in the return type.
  fmap (toStruct . fst) $
    unscopeType loc hidden_params $
      inferReturnUniqueness params t
  where
    hidden_params = M.filterWithKey (const . (`S.member` hidden)) $ foldMap patternMap params
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
              not (consumableParamType $ unInfo $ identType p') && (identName p' `S.member` names)
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

    consumableParamType (Array _ u _ _) = u == Unique
    consumableParamType (Scalar Prim {}) = True
    consumableParamType (Scalar (TypeVar _ u _ _)) = u == Unique
    consumableParamType (Scalar (Record fs)) = all consumableParamType fs
    consumableParamType (Scalar (Sum fs)) = all (all consumableParamType) fs
    consumableParamType (Scalar Arrow {}) = False

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
      Maybe (TypeExp Info VName),
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
            pure (Nothing, toStruct body_t)
        | otherwise -> do
            body_t' <- inferredReturnType loc params'' body_t
            pure (Nothing, body_t')

    verifyFunctionParams (Just fname) params''

    (tparams'', params''', rettype') <-
      letGeneralise fname loc tparams' params'' rettype

    checkGlobalAliases params'' body_t loc

    pure (tparams'', params''', maybe_retdecl'', rettype', body')

-- | Extract all the shape names that occur in positive position
-- (roughly, left side of an arrow) in a given type.
sizeNamesPos :: TypeBase Size als -> S.Set VName
sizeNamesPos (Scalar (Arrow _ _ _ t1 (RetType _ t2))) = onParam t1 <> sizeNamesPos t2
  where
    onParam :: TypeBase Size als -> S.Set VName
    onParam (Scalar Arrow {}) = mempty
    onParam (Scalar (Record fs)) = mconcat $ map onParam $ M.elems fs
    onParam (Scalar (TypeVar _ _ _ targs)) = mconcat $ map onTypeArg targs
    onParam t = freeInType t
    onTypeArg (TypeArgDim (NamedSize d) _) = S.singleton $ qualLeaf d
    onTypeArg (TypeArgDim _ _) = mempty
    onTypeArg (TypeArgType t _) = onParam t
sizeNamesPos _ = mempty

checkGlobalAliases :: [Pat] -> PatType -> SrcLoc -> TermTypeM ()
checkGlobalAliases params body_t loc = do
  vtable <- asks $ scopeVtable . termScope
  let isGlobal v = case v `M.lookup` vtable of
        Just (BoundV Global _ _) -> True
        _ -> False
  let als =
        filter isGlobal . S.toList $
          boundArrayAliases body_t `S.difference` foldMap patNames params
  unless (null params) $ case als of
    v : _ ->
      typeError loc mempty . withIndexLink "alias-free-variable" $
        "Function result aliases the free variable "
          <> dquotes (prettyName v)
          <> "."
          </> "Use"
          <+> dquotes "copy"
          <+> "to break the aliasing."
    _ ->
      pure ()

inferReturnUniqueness :: [Pat] -> PatType -> PatType
inferReturnUniqueness params t =
  let forbidden = aliasesMultipleTimes t
      uniques = uniqueParamNames params
      delve (Scalar (Record fs)) =
        Scalar $ Record $ M.map delve fs
      delve (Scalar (Sum cs)) =
        Scalar $ Sum $ M.map (map delve) cs
      delve t'
        | all (`S.member` uniques) (boundArrayAliases t'),
          not $ any ((`S.member` forbidden) . aliasVar) (aliases t') =
            t' `setUniqueness` Unique
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
      | d : _ <- S.toList $ freeInPat p `S.intersection` forbidden =
          typeError p mempty . withIndexLink "inaccessible-size" $
            "Parameter"
              <+> dquotes (pretty p)
              </> "refers to size"
              <+> dquotes (prettyName d)
                <> comma
              </> textwrap "which will not be accessible to the caller"
                <> comma
              </> textwrap "possibly because it is nested in a tuple or record."
              </> textwrap "Consider ascribing an explicit type that does not reference "
                <> dquotes (prettyName d)
                <> "."
      | otherwise = verifyParams forbidden' ps
      where
        forbidden' =
          case patternParam p of
            (Named v, _, _) -> forbidden `S.difference` S.singleton v
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
    deeper (Scalar (Arrow als p d1 t1 (RetType t2_ext t2))) =
      Scalar $ Arrow als p d1 t1 $ injectExt (ext_there <> t2_ext) t2
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
  let mkExt v =
        case M.lookup v substs of
          Just (_, UnknowableSize {}) -> Just v
          _ -> Nothing
  pure
    ( tparams ++ more_tparams,
      injectExt (retext ++ mapMaybe mkExt (S.toList $ freeInType ret)) ret
    )
  where
    -- Diet does not matter here.
    t = foldFunType (zip (repeat Observe) paramts) $ RetType [] ret
    to_close_over = M.filterWithKey (\k _ -> k `S.member` visible) substs
    visible = typeVars t <> freeInType t

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
          notes <- dimNotes defloc $ NamedSize $ qualName k
          typeError defloc notes . withIndexLink "unknowable-param-def" $
            "Unknowable size"
              <+> dquotes (prettyName k)
              <+> "in parameter of"
              <+> dquotes (prettyName defname)
                <> ", which is inferred as:"
              </> indent 2 (pretty t)
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
          foldMap freeInType $ rettype'' : map patternStructType params
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
        expect usage rettype $
          toStruct body_t'
    Nothing -> pure ()

  pure body'

arrayOfM ::
  (Pretty (Shape dim), Monoid as) =>
  SrcLoc ->
  TypeBase dim as ->
  Shape dim ->
  Uniqueness ->
  TermTypeM (TypeBase dim as)
arrayOfM loc t shape u = do
  arrayElemType (mkUsage loc "use as array element") "type used in array" t
  pure $ arrayOf u shape t
