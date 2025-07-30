-- | Facilities for type-checking Futhark terms.  Checking a term
-- requires a little more context to track uniqueness and such.
--
-- Type inference is implemented through a variation of
-- Hindley-Milner.  The main complication is supporting the rich
-- number of built-in language constructs, as well as uniqueness
-- types.  This is mostly done in an ad hoc way, and many programs
-- will require the programmer to fall back on type annotations.
--
-- The strategy is to split type checking into sveral (main) passes:
--
-- 1) A size-agnostic pass implemented in
-- "Language.Futhark.TypeChecker.Terms.Unsized".
--
-- 2) Pass (1) has given us a program where we know the types of
-- everything, but the sizes of nothing. Pass (2) then does
-- essentially size inference, with the benefit of already knowing the
-- full unsized type of everything. This is done using a syntax-driven
-- approach, similar to Algorithm W.
--
-- 3) The program is then checked for violation of uniqueness
-- properties, which is implemented in
-- "Language.Futhark.TypeChecker.Consumption".
module Language.Futhark.TypeChecker.Terms
  ( checkOneExp,
    checkSizeExp,
    checkFunDef,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bitraversable
import Data.Char (isAscii)
import Data.Either
import Data.List (delete, find, genericLength, partition)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Util (debugTraceM, mapAccumLM, nubOrd)
import Futhark.Util.Pretty hiding (space)
import Language.Futhark
import Language.Futhark.Primitive (intByteSize)
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Consumption qualified as Consumption
import Language.Futhark.TypeChecker.Match
import Language.Futhark.TypeChecker.Monad hiding (BoundV, lookupMod)
import Language.Futhark.TypeChecker.Terms.Loop
import Language.Futhark.TypeChecker.Terms.Monad
import Language.Futhark.TypeChecker.Terms.Pat
import Language.Futhark.TypeChecker.Terms.Unsized qualified as Unsized
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify
import Prelude hiding (mod)

hasBinding :: Exp -> Bool
hasBinding Lambda {} = True
hasBinding (AppExp LetPat {} _) = True
hasBinding (AppExp LetFun {} _) = True
hasBinding (AppExp Loop {} _) = True
hasBinding (AppExp LetWith {} _) = True
hasBinding (AppExp Match {} _) = True
hasBinding e = isNothing $ astMap m e
  where
    m =
      identityMapper {mapOnExp = \e' -> if hasBinding e' then Nothing else Just e'}

--- Basic checking

-- | Determine if the two types are identical, ignoring uniqueness.
-- Mismatched dimensions are turned into fresh rigid type variables.
-- Causes a 'TypeError' if they fail to match, and otherwise returns
-- one of them.
unifyBranchTypes :: SrcLoc -> StructType -> StructType -> TermTypeM (StructType, [VName])
unifyBranchTypes loc t1 t2 =
  onFailure (CheckingBranches t1 t2) $
    unifyMostCommon (mkUsage loc "unification of branch results") t1 t2

unifyBranches :: SrcLoc -> Exp -> Exp -> TermTypeM (StructType, [VName])
unifyBranches loc e1 e2 = do
  e1_t <- expTypeFully e1
  e2_t <- expTypeFully e2
  unifyBranchTypes loc e1_t e2_t

sliceShape ::
  Maybe (SrcLoc, Rigidity) ->
  [DimIndex] ->
  TypeBase Size as ->
  TermTypeM (TypeBase Size as, [VName])
sliceShape r slice t@(Array u (Shape orig_dims) et) =
  runStateT (setDims <$> adjustDims slice orig_dims) []
  where
    setDims [] = stripArray (length orig_dims) t
    setDims dims' = Array u (Shape dims') et

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
          lift $
            flip sizeFromName loc . qualName
              <$> newFlexibleDim (mkUsage loc "size of slice") "slice_dim"
        Nothing -> do
          v <- lift $ newID "slice_anydim"
          modify (v :)
          pure $ sizeFromName (qualName v) mempty
      where
        -- The original size does not matter if the slice is fully specified.
        orig_d'
          | isJust i, isJust j = Nothing
          | otherwise = Just orig_d

    warnIfBinding binds d i j stride size =
      if binds
        then do
          lift . warn (srclocOf size) $
            withIndexLink
              "size-expression-bind"
              "Size expression with binding is replaced by unknown size."
          (:) <$> sliceSize d i j stride
        else pure (size :)

    adjustDims (DimFix {} : idxes') (_ : dims) =
      adjustDims idxes' dims
    -- Pat match some known slices to be non-existential.
    adjustDims (DimSlice i j stride : idxes') (d : dims)
      | refine_sizes,
        maybe True ((== Just 0) . isInt64) i,
        maybe True ((== Just 1) . isInt64) stride = do
          let binds = maybe False hasBinding j
          warnIfBinding binds d i j stride (fromMaybe d j)
            <*> adjustDims idxes' dims
    adjustDims ((DimSlice i j stride) : idxes') (d : dims)
      | refine_sizes,
        Just i' <- i, -- if i ~ 0, previous case
        maybe True ((== Just 1) . isInt64) stride = do
          let j' = fromMaybe d j
              binds = hasBinding j' || hasBinding i'
          warnIfBinding binds d i j stride (sizeMinus j' i')
            <*> adjustDims idxes' dims
    -- stride == -1
    adjustDims ((DimSlice Nothing Nothing stride) : idxes') (d : dims)
      | refine_sizes,
        maybe True ((== Just (-1)) . isInt64) stride =
          (d :) <$> adjustDims idxes' dims
    adjustDims ((DimSlice (Just i) (Just j) stride) : idxes') (d : dims)
      | refine_sizes,
        maybe True ((== Just (-1)) . isInt64) stride = do
          let binds = hasBinding i || hasBinding j
          warnIfBinding binds d (Just i) (Just j) stride (sizeMinus i j)
            <*> adjustDims idxes' dims
    -- existential
    adjustDims ((DimSlice i j stride) : idxes') (d : dims) =
      (:) <$> sliceSize d i j stride <*> adjustDims idxes' dims
    adjustDims _ dims =
      pure dims

    sizeMinus j i =
      AppExp
        ( BinOp
            (qualName (intrinsicVar "-"), mempty)
            sizeBinOpInfo
            (j, Info (Nothing, mempty))
            (i, Info (Nothing, mempty))
            mempty
        )
        $ Info
        $ AppRes i64 []
    i64 = Scalar $ Prim $ Signed Int64
    sizeBinOpInfo = Info $ foldFunType [i64, i64] $ RetType [] i64
sliceShape _ _ t = pure (t, [])

--- Main checkers

checkAscript ::
  SrcLoc ->
  TypeExp Exp VName ->
  Exp ->
  TermTypeM (TypeExp Exp VName, Exp)
checkAscript loc te e = do
  (te', decl_t, _) <- checkTypeExpNonrigid te
  e' <- checkExp e
  e_t <- expTypeFully e'

  onFailure (CheckingAscription (toStruct decl_t) e_t) $
    unify (mkUsage loc "type ascription") (toStruct decl_t) e_t

  pure (te', e')

checkCoerce ::
  SrcLoc ->
  TypeExp Exp VName ->
  Exp ->
  TermTypeM (TypeExp Exp VName, StructType, Exp)
checkCoerce loc te e = do
  (te', te_t, ext) <- checkTypeExpNonrigid te
  e' <- checkExp e
  e_t <- expTypeFully e'

  te_t_nonrigid <- makeNonExtFresh ext $ toStruct te_t

  onFailure (CheckingAscription (toStruct te_t) e_t) $
    unify (mkUsage loc "size coercion") e_t te_t_nonrigid

  -- If the type expression had any anonymous dimensions, these will
  -- now be in 'ext'.  Those we keep nonrigid and unify with e_t.
  -- This ensures that 'x :> [1][]i32' does not make the second
  -- dimension unknown.  Use of matchDims is sensible because the
  -- structure of e_t' will be fully known due to the unification, and
  -- te_t because type expressions are complete.
  pure (te', toStruct te_t, e')
  where
    makeNonExtFresh ext = bitraverse onDim pure
      where
        onDim d@(Var v _ _)
          | qualLeaf v `elem` ext = pure d
        onDim d = do
          v <- newTypeName "coerce"
          constrain v . Size Nothing $
            mkUsage
              loc
              "a size coercion where the underlying expression size cannot be determined"
          pure $ sizeFromName (qualName v) (srclocOf d)

-- Used to remove unknown sizes from function body types before we
-- perform let-generalisation.  This is because if a function is
-- inferred to return something of type '[x+y]t' where 'x' or 'y' are
-- unknown, we want to turn that into '[z]t', where ''z' is a fresh
-- unknown, which is then by let-generalisation turned into
-- '?[z].[z]t'.
unscopeUnknown ::
  TypeBase Size u ->
  TermTypeM (TypeBase Size u)
unscopeUnknown t = do
  constraints <- getConstraints
  -- These sizes will be immediately turned into existentials, so we
  -- do not need to care about their location.
  fst <$> sizeFree mempty (expKiller constraints) t
  where
    expKiller _ Var {} = Nothing
    expKiller constraints e =
      S.lookupMin $ S.filter (isUnknown constraints) $ (`S.difference` witnesses) $ fvVars $ freeInExp e
    isUnknown constraints vn
      | Just UnknownSize {} <- snd <$> M.lookup vn constraints = True
    isUnknown _ _ = False
    (witnesses, _) = determineSizeWitnesses $ toStruct t

unscopeType ::
  SrcLoc ->
  [VName] ->
  TypeBase Size as ->
  TermTypeM (TypeBase Size as, [VName])
unscopeType tloc unscoped =
  sizeFree tloc $ find (`elem` unscoped) . fvVars . freeInExp

checkExp :: Exp -> TermTypeM Exp
checkExp (Var qn (Info t) loc) = do
  t' <- lookupVar loc qn t
  pure $ Var qn (Info t') loc
checkExp (Literal val loc) =
  pure $ Literal val loc
checkExp (Hole (Info t) loc) = do
  t' <- replaceTyVars loc t
  pure $ Hole (Info t') loc
checkExp (StringLit vs loc) =
  pure $ StringLit vs loc
checkExp (IntLit val (Info t) loc) = do
  t' <- replaceTyVars loc t
  pure $ IntLit val (Info t') loc
checkExp (FloatLit val (Info t) loc) = do
  t' <- replaceTyVars loc t
  pure $ FloatLit val (Info t') loc
checkExp (TupLit es loc) =
  TupLit <$> mapM checkExp es <*> pure loc
checkExp (RecordLit fs loc) =
  RecordLit <$> mapM checkField fs <*> pure loc
  where
    checkField (RecordFieldExplicit f e rloc) =
      RecordFieldExplicit f <$> checkExp e <*> pure rloc
    checkField (RecordFieldImplicit name (Info t) rloc) = do
      t' <- lookupVar rloc (qualName (unLoc name)) t
      pure $ RecordFieldImplicit name (Info t') rloc
-- No need to type check this, as these are only produced by the
-- parser if the elements are monomorphic and all match.
checkExp (ArrayVal vs t loc) =
  pure $ ArrayVal vs t loc
checkExp (ArrayLit all_es _ loc) =
  -- Construct the result type and unify all elements with it.  We
  -- only create a type variable for empty arrays; otherwise we use
  -- the type of the first element.  This significantly cuts down on
  -- the number of type variables generated for pathologically large
  -- multidimensional array literals.
  case all_es of
    [] -> do
      et <- newTypeVar loc "t"
      t <- arrayOfM loc et (Shape [sizeFromInteger 0 mempty])
      mustBeUnlifted (locOf loc) et
      pure $ ArrayLit [] (Info t) loc
    e : es -> do
      e' <- checkExp e
      et <- expType e'
      es' <- mapM (unifies "type of first array element" et <=< checkExp) es
      t <- arrayOfM loc et (Shape [sizeFromInteger (genericLength all_es) mempty])
      mustBeUnlifted (locOf loc) et
      pure $ ArrayLit (e' : es') (Info t) loc
checkExp (AppExp (Range start maybe_step end loc) _) = do
  start' <- checkExp start
  start_t <- expType start'
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
  let warnIfBinding binds size =
        if binds
          then do
            warn (srclocOf size) $
              withIndexLink
                "size-expression-bind"
                "Size expression with binding is replaced by unknown size."
            d <- newRigidDim loc RigidRange "range_dim"
            pure (sizeFromName (qualName d) mempty, Just d)
          else pure (size, Nothing)
  (dim, retext) <-
    case (isInt64 start', isInt64 <$> maybe_step', end') of
      (Just 0, Just (Just 1), UpToExclusive end'')
        | Scalar (Prim (Signed Int64)) <- end_t ->
            warnIfBinding (hasBinding end'') end''
      (Just 0, Nothing, UpToExclusive end'')
        | Scalar (Prim (Signed Int64)) <- end_t ->
            warnIfBinding (hasBinding end'') end''
      (_, Nothing, UpToExclusive end'')
        | Scalar (Prim (Signed Int64)) <- end_t ->
            warnIfBinding (hasBinding end'' || hasBinding start') $ sizeMinus end'' start'
      (_, Nothing, ToInclusive end'')
        -- No stride means we assume a stride of one.
        | Scalar (Prim (Signed Int64)) <- end_t ->
            warnIfBinding (hasBinding end'' || hasBinding start') $ sizeMinusInc end'' start'
      (Just 1, Just (Just 2), ToInclusive end'')
        | Scalar (Prim (Signed Int64)) <- end_t ->
            warnIfBinding (hasBinding end'') end''
      _ -> do
        d <- newRigidDim loc RigidRange "range_dim"
        pure (sizeFromName (qualName d) mempty, Just d)

  t <- arrayOfM loc start_t (Shape [dim])
  let res = AppRes t (maybeToList retext)

  pure $ AppExp (Range start' maybe_step' end' loc) (Info res)
  where
    i64 = Scalar $ Prim $ Signed Int64
    mkBinOp op t x y =
      AppExp
        ( BinOp
            (qualName (intrinsicVar op), mempty)
            sizeBinOpInfo
            (x, Info (Nothing, mempty))
            (y, Info (Nothing, mempty))
            mempty
        )
        (Info $ AppRes t [])
    mkSub = mkBinOp "-" i64
    mkAdd = mkBinOp "+" i64
    sizeMinus j i = j `mkSub` i
    sizeMinusInc j i = (j `mkSub` i) `mkAdd` sizeFromInteger 1 mempty
    sizeBinOpInfo = Info $ foldFunType [i64, i64] $ RetType [] i64
checkExp (Ascript e te loc) = do
  (te', e') <- checkAscript loc te e
  pure $ Ascript e' te' loc
checkExp (Coerce e te _ loc) = do
  (te', te_t, e') <- checkCoerce loc te e
  t <- expTypeFully e'
  t' <- matchDims (const . const pure) t te_t
  pure $ Coerce e' te' (Info t') loc
checkExp (AppExp (Apply fe args loc) _) = do
  fe' <- checkExp fe
  let ams = fmap (snd . unInfo . fst) args
  args' <- mapM (checkExp . snd) args
  t <- expType fe'
  let fname =
        case fe' of
          Var v _ _ -> Just v
          _ -> Nothing
  ((_, exts, rt), args'') <- mapAccumLM (onArg fname) (0, [], t) (NE.zip args' ams)

  pure $ AppExp (Apply fe' args'' loc) $ Info $ AppRes rt exts
  where
    onArg fname (i, all_exts, t) (arg', am) = do
      (_, rt, argext, exts, am') <- checkApply loc (fname, i) t arg' am
      pure
        ( (i + 1, all_exts <> exts, rt),
          (Info (argext, am'), arg')
        )
checkExp (AppExp (BinOp (op, oploc) (Info op_t) (e1, Info (_, xam)) (e2, Info (_, yam)) loc) _) = do
  ftype <- lookupVar oploc op op_t
  e1' <- checkExp e1
  e2' <- checkExp e2
  -- Note that the application to the first operand cannot fix any
  -- existential sizes, because it must by necessity be a function.
  (_, rt, p1_ext, _, am1) <- checkApply loc (Just op, 0) ftype e1' xam
  (_, rt', p2_ext, retext, am2) <- checkApply loc (Just op, 1) rt e2' yam

  pure $
    AppExp
      ( BinOp
          (op, oploc)
          (Info ftype)
          (e1', Info (p1_ext, am1))
          (e2', Info (p2_ext, am2))
          loc
      )
      (Info (AppRes rt' retext))
checkExp (Project k e _ loc) = do
  e' <- checkExp e
  t <- expType e'
  case t of
    Scalar (Record fs)
      | Just kt <- M.lookup k fs ->
          pure $ Project k e' (Info kt) loc
    _ -> error $ "checkExp Project: " <> show t
checkExp (Parens e loc) =
  Parens <$> checkExp e <*> pure loc
checkExp (QualParens (modname, modnameloc) e loc) = do
  mod <- lookupMod modname
  case mod of
    ModEnv env -> local (`withEnv` env) $ do
      e' <- checkExp e
      pure $ QualParens (modname, modnameloc) e' loc
    ModFun {} ->
      typeError loc mempty . withIndexLink "module-is-parametric" $
        "Module" <+> pretty modname <+> " is a parametric module."
checkExp (Negate arg loc) = do
  arg' <- checkExp arg
  pure $ Negate arg' loc
checkExp (Not arg loc) = do
  arg' <- checkExp arg
  pure $ Not arg' loc
checkExp (AppExp (LetPat sizes pat e body loc) _) = do
  e' <- checkExp e

  -- Not technically an ascription, but we want the pattern to have
  -- exactly the type of 'e'.
  t <- expType e'
  bindingSizes sizes . incLevel . bindingPat sizes pat t $ \pat' -> do
    body' <- incLevel $ checkExp body
    body_t <- expTypeFully body'

    -- If the bound expression is of type i64, then we replace the
    -- pattern name with the expression in the type of the body.
    -- Otherwise, we need to come up with unknown sizes for the
    -- sizes going out of scope.
    t' <- normType t -- Might be overloaded integer until now.
    (body_t', retext) <-
      case (t', patNames pat') of
        (Scalar (Prim (Signed Int64)), [v])
          | not $ hasBinding e' -> do
              let f x = if x == v then Just (ExpSubst e') else Nothing
              pure (applySubst f body_t, [])
        _ ->
          unscopeType loc (map sizeName sizes <> patNames pat') body_t

    pure $
      AppExp
        (LetPat sizes (fmap toStruct pat') e' body' loc)
        (Info $ AppRes body_t' retext)
checkExp (AppExp (LetFun name (tparams, params, maybe_retdecl, _, e) body loc) _) = do
  (tparams', params', maybe_retdecl', rettype, e') <-
    checkBinding (name, maybe_retdecl, tparams, params, e, loc)

  let entry = BoundV tparams' $ funType params' rettype
      bindF scope =
        scope
          { scopeVtable = M.insert name entry $ scopeVtable scope
          }
  body' <- localScope bindF $ checkExp body

  (body_t, ext) <- unscopeType loc [name] =<< expTypeFully body'

  pure $
    AppExp
      ( LetFun
          name
          (tparams', params', maybe_retdecl', Info rettype, e')
          body'
          loc
      )
      (Info $ AppRes body_t ext)
checkExp (AppExp (LetWith dest src slice ve body loc) _) = do
  src_t <- lookupVar loc (qualName (identName src)) (unInfo (identType src))
  let src' = src {identType = Info src_t}
      dest' = dest {identType = Info src_t}
  slice' <- checkSlice slice
  (t, _) <- newArrayType (mkUsage src' "type of source array") "src" $ sliceDims slice'
  unify (mkUsage loc "type of target array") t $ unInfo $ identType src'

  (elemt, _) <- sliceShape (Just (loc, Nonrigid)) slice' =<< normTypeFully t

  ve' <- unifies "type of target array" elemt =<< checkExp ve

  bindingIdent dest' $ do
    body' <- checkExp body
    (body_t, ext) <- unscopeType loc [identName dest'] =<< expTypeFully body'
    pure $ AppExp (LetWith dest' src' slice' ve' body' loc) (Info $ AppRes body_t ext)
checkExp (Update src slice ve loc) = do
  slice' <- checkSlice slice
  (t, _) <- newArrayType (mkUsage' src) "src" $ sliceDims slice'
  (elemt, _) <- sliceShape (Just (loc, Nonrigid)) slice' =<< normTypeFully t
  ve' <- unifies "type of target array" elemt =<< checkExp ve
  src' <- unifies "type of target array" t =<< checkExp src
  pure $ Update src' slice' ve' loc

-- Record updates are a bit hacky, because we do not have row typing
-- (yet?).  For now, we only permit record updates where we know the
-- full type up to the field we are updating.
checkExp (RecordUpdate src fields ve _ loc) = do
  src' <- checkExp src
  ve' <- checkExp ve
  ve_t <- expType ve'
  updated_t <- updateField fields ve_t =<< expTypeFully src'
  pure $ RecordUpdate src' fields ve' (Info updated_t) loc
  where
    usage = mkUsage loc "record update"
    updateField [] ve_t src_t = do
      (src_t', _) <- allDimsFreshInType usage Nonrigid "any" src_t
      onFailure (CheckingRecordUpdate fields src_t' ve_t) $
        unify usage src_t' ve_t
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
  (t, _) <- newArrayType (mkUsage' loc) "e" $ sliceDims slice'
  e' <- unifies "being indexed at" t =<< checkExp e
  -- XXX, the RigidSlice here will be overridden in sliceShape with a proper value.
  (t', retext) <-
    sliceShape (Just (loc, Rigid (RigidSlice Nothing ""))) slice'
      =<< expTypeFully e'

  pure $ AppExp (Index e' slice' loc) (Info $ AppRes t' retext)
checkExp (Assert e1 e2 _ loc) = do
  e1' <- checkExp e1
  e2' <- checkExp e2
  pure $ Assert e1' e2' (Info (prettyText e1)) loc
checkExp (Lambda params body rettype_te (Info (RetType _ rt)) loc) = do
  (params', body', rettype', RetType dims ty) <-
    incLevel . bindingParams [] params $ \params' -> do
      rt' <- replaceTyVars loc rt
      rettype_checked <- traverse checkTypeExpNonrigid rettype_te
      declared_rettype <-
        case rettype_checked of
          Just (_, st, _) -> do
            unify (mkUsage body "lambda return type ascription") (toStruct rt') (toStruct st)
            pure $ Just st
          Nothing -> pure Nothing
      body' <- checkFunBody params' body declared_rettype loc
      body_t <- expTypeFully body'

      unify (mkUsage body "inferred return type") (toStruct rt') body_t

      params'' <- mapM updateTypes params'

      (rettype', rettype_st) <- case rettype_checked of
        Just (te, ret, ext) -> do
          ret' <- normTypeFully ret
          pure (Just te, RetType ext ret')
        Nothing -> do
          ret <- inferReturnSizes params'' $ toRes Nonunique body_t
          pure (Nothing, ret)

      pure (params'', body', rettype', rettype_st)

  verifyFunctionParams Nothing params'

  (ty', dims') <- unscopeType loc dims ty

  pure $ Lambda params' body' rettype' (Info (RetType dims' ty')) loc
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
            sizeNamesPos $ funType params' $ RetType [] ret
          hide k (lvl, _) =
            lvl >= cur_lvl && k `notElem` param_names && k `S.notMember` pos_sizes

      hidden_sizes <-
        S.fromList . M.keys . M.filterWithKey hide <$> getConstraints

      let onDim name
            | name `S.member` hidden_sizes = S.singleton name
          onDim _ = mempty

      pure $ RetType (S.toList $ foldMap onDim $ fvVars $ freeInType ret) ret
checkExp (OpSection op (Info op_t) loc) = do
  ftype <- lookupVar loc op op_t
  pure $ OpSection op (Info ftype) loc
checkExp (OpSectionLeft op (Info op_t) e (Info (_, _, _, am), _) _ loc) = do
  ftype <- lookupVar loc op op_t
  e' <- checkExp e
  (t1, rt, argext, retext, am') <- checkApply loc (Just op, 0) ftype e' am
  case (ftype, rt) of
    (Scalar (Arrow _ m1 d1 _ _), Scalar (Arrow _ m2 d2 t2 (RetType ds rt2))) ->
      pure $
        OpSectionLeft
          op
          (Info ftype)
          e'
          (Info (m1, toParam d1 t1, argext, am'), Info (m2, toParam d2 t2))
          (Info $ RetType ds $ arrayOfWithAliases (uniqueness rt2) (autoFrame am') rt2, Info retext)
          loc
    _ ->
      typeError loc mempty $
        "Operator section with invalid operator of type" <+> pretty ftype
checkExp (OpSectionRight op (Info op_t) e (_, Info (_, _, _, am)) _ loc) = do
  ftype <- lookupVar loc op op_t
  e' <- checkExp e
  case ftype of
    Scalar (Arrow _ m1 d1 t1 (RetType [] (Scalar (Arrow _ m2 d2 t2 (RetType dims2 ret))))) -> do
      (t2', arrow', argext, _, am') <-
        checkApply
          loc
          (Just op, 1)
          (Scalar $ Arrow mempty m2 d2 t2 $ RetType [] $ Scalar $ Arrow Nonunique m1 d1 t1 $ RetType dims2 ret)
          e'
          am
      case arrow' of
        Scalar (Arrow _ _ _ t1' (RetType dims2' ret')) ->
          pure $
            OpSectionRight
              op
              (Info ftype)
              e'
              (Info (m1, toParam d1 t1'), Info (m2, toParam d2 t2', argext, am'))
              (Info $ RetType dims2' $ arrayOfWithAliases (uniqueness ret') (autoFrame am') ret')
              loc
        _ -> error $ "OpSectionRight: impossible type\n" <> prettyString arrow'
    _ ->
      typeError loc mempty $
        "Operator section with invalid operator of type" <+> pretty ftype
checkExp (ProjectSection fields (Info t) loc) = do
  t' <- replaceTyVars loc t
  case t' of
    Scalar (Arrow _ _ _ t'' (RetType _ rt))
      | Just ft <- recordField fields t'' ->
          unify (mkUsage loc "result of projection") ft $ toStruct rt
    _ -> error $ "checkExp ProjectSection: " <> show t'
  pure $ ProjectSection fields (Info t') loc
checkExp (IndexSection slice _ loc) = do
  slice' <- checkSlice slice
  (t, _) <- newArrayType (mkUsage' loc) "e" $ sliceDims slice'
  (t', retext) <- sliceShape Nothing slice' t
  let ft = Scalar $ Arrow mempty Unnamed Observe t $ RetType retext $ toRes Nonunique t'
  pure $ IndexSection slice' (Info ft) loc
checkExp (AppExp (Loop _ mergepat loopinit form loopbody loc) _) = do
  ((sparams, mergepat', loopinit', form', loopbody'), appres) <-
    checkLoop checkExp (mergepat, loopinit, form, loopbody) loc
  pure $
    AppExp
      (Loop sparams mergepat' loopinit' form' loopbody' loc)
      (Info appres)
checkExp (Constr name es (Info t) loc) = do
  t' <- replaceTyVars loc t
  es' <- mapM checkExp es
  case t' of
    Scalar (Sum cs)
      | Just name_ts <- M.lookup name cs ->
          zipWithM_ (unify $ mkUsage loc "inferred variant") name_ts $
            map typeOf es'
    _ ->
      error $ "checkExp Constr: " <> prettyString t'
  pure $ Constr name es' (Info t') loc
checkExp (AppExp (If e1 e2 e3 loc) _) = do
  e1' <- checkExp e1
  e2' <- checkExp e2
  e3' <- checkExp e3

  let bool = Scalar $ Prim Bool
  e1_t <- expType e1'
  onFailure (CheckingRequired [bool] e1_t) $
    unify (mkUsage e1' "use as 'if' condition") bool e1_t

  (t, retext) <- unifyBranches loc e2' e3'

  mustBeOrderZero (locOf loc) t

  pure $ AppExp (If e1' e2' e3' loc) (Info $ AppRes t retext)
checkExp (AppExp (Match e cs loc) _) = do
  e' <- checkExp e
  mt <- expType e'
  (cs', t, retext) <- checkCases mt cs

  mustBeOrderZero (locOf loc) t

  pure $ AppExp (Match e' cs' loc) (Info $ AppRes t retext)
checkExp (Attr info e loc) =
  Attr <$> checkAttr info <*> checkExp e <*> pure loc

checkCase ::
  StructType ->
  CaseBase Info VName ->
  TermTypeM (CaseBase Info VName, StructType, [VName])
checkCase mt (CasePat p e loc) =
  bindingPat [] p mt $ \p' -> do
    e' <- checkExp e
    e_t <- expTypeFully e'
    (e_t', retext) <- unscopeType loc (patNames p') e_t
    pure (CasePat (fmap toStruct p') e' loc, e_t', retext)

checkCases ::
  StructType ->
  NE.NonEmpty (CaseBase Info VName) ->
  TermTypeM (NE.NonEmpty (CaseBase Info VName), StructType, [VName])
checkCases mt rest_cs =
  case NE.uncons rest_cs of
    (c, Nothing) -> do
      (c', t, retext) <- checkCase mt c
      pure (NE.singleton c', t, retext)
    (c, Just cs) -> do
      ((c', c_t, _), (cs', cs_t, _)) <-
        (,) <$> checkCase mt c <*> checkCases mt cs
      (brancht, retext) <- unifyBranchTypes (srclocOf c) c_t cs_t
      pure (NE.cons c' cs', brancht, retext)

-- | An unmatched pattern. Used in in the generation of
-- unmatched pattern warnings by the type checker.
data Unmatched p
  = UnmatchedNum p [PatLit]
  | UnmatchedBool p
  | UnmatchedConstr p
  | Unmatched p
  deriving (Functor, Show)

instance Pretty (Unmatched (Pat StructType)) where
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
          ppField (L _ name, t) = pretty (nameToString name) <> equals <> pretty' t
      pretty' Wildcard {} = "_"
      pretty' (PatLit e _ _) = pretty e
      pretty' (PatConstr n _ ps _) = "#" <> pretty n <+> sep (map pretty' ps)

checkSlice :: SliceBase Info VName -> TermTypeM [DimIndex]
checkSlice = mapM checkDimIndex
  where
    checkDimIndex (DimFix i) =
      DimFix <$> checkExp i
    checkDimIndex (DimSlice i j s) =
      DimSlice <$> traverse checkExp i <*> traverse checkExp j <*> traverse checkExp s

-- The number of dimensions affected by this slice (so the minimum
-- rank of the array we are slicing).
sliceDims :: [DimIndex] -> Int
sliceDims = length

instantiateDimsInReturnType ::
  SrcLoc ->
  Maybe (QualName VName) ->
  ResRetType ->
  TermTypeM (ResType, [VName])
instantiateDimsInReturnType loc fname (RetType dims t)
  | null dims =
      pure (t, mempty)
  | otherwise = do
      dims' <- mapM new dims
      pure (first (onDim $ zip dims $ map (ExpSubst . (`sizeFromName` loc) . qualName) dims') t, dims')
  where
    new =
      newRigidDim loc (RigidRet fname)
        . nameFromString
        . takeWhile isAscii
        . baseString
    onDim dims' = applySubst (`lookup` dims')

-- Some information about the function/operator we are trying to
-- apply, and how many arguments it has previously accepted.  Used for
-- generating nicer type errors.
type ApplyOp = (Maybe (QualName VName), Int)

-- | Extract all those names that are bound inside the type.
boundInsideType :: TypeBase Size as -> S.Set VName
boundInsideType (Array _ _ t) = boundInsideType (Scalar t)
boundInsideType (Scalar Prim {}) = mempty
boundInsideType (Scalar (TypeVar _ _ targs)) = foldMap f targs
  where
    f (TypeArgType t) = boundInsideType t
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
dimUses :: TypeBase Size u -> (Names, Names)
dimUses = flip execState mempty . traverseDims f
  where
    f bound pos e =
      case pos of
        PosImmediate ->
          modify ((fvVars fv, mempty) <>)
        PosParam ->
          modify ((mempty, fvVars fv) <>)
        PosReturn -> pure ()
      where
        fv = freeInExp e `freeWithout` bound

splitArrayAt :: Int -> StructType -> (Shape Size, StructType)
splitArrayAt x t =
  (Shape $ take x $ shapeDims $ arrayShape t, stripArray x t)

checkApply ::
  SrcLoc ->
  ApplyOp ->
  StructType ->
  Exp ->
  AutoMap ->
  TermTypeM (StructType, StructType, Maybe VName, [VName], AutoMap)
checkApply loc fn@(fname, _) ft@(Scalar (Arrow _ pname _ tp1 tp2)) argexp am = do
  let argtype = typeOf argexp
  onFailure (CheckingApply fname argexp tp1 argtype) $ do
    -- argtype = arg_frame argtype'
    -- tp1 = f_frame tp1'
    --
    -- Rep case:
    -- R arg_frame argtype' = f_frame tp1'
    -- ==> R = (autoRepRank am)-length prefix of tp1
    -- ==> frame = f_frame = (autoFrameRank am)-length prefix of tp1
    --
    -- Map case:
    -- arg_frame argtype' = M f_frame tp1'
    -- ==> M = (autoMapRank am)-length prefix of argtype
    -- ==> frame = M f_frame = (autoFrameRank am)-length prefix of argtype
    (am_map_shape, argtype_with_frame) <- splitArrayAt (autoMapRank am) <$> normTypeFully argtype
    (am_rep_shape, tp1_with_frame) <- splitArrayAt (autoRepRank am) <$> normTypeFully tp1
    (am_frame_shape, _) <-
      if autoMapRank am == 0
        then splitArrayAt (autoFrameRank am) <$> normTypeFully tp1
        else splitArrayAt (autoFrameRank am) <$> normTypeFully argtype

    debugTraceM 3 $
      unlines
        [ "## checkApply",
          "## fn",
          prettyString fn,
          "## ft",
          prettyString ft,
          "## tp1_with_frame",
          prettyString tp1_with_frame,
          "## argtype_with_frame",
          prettyString argtype_with_frame,
          "## am",
          show am
        ]

    unify (mkUsage argexp "use as function argument") tp1_with_frame argtype_with_frame

    -- Perform substitutions of instantiated variables in the types.
    (tp2', ext) <- instantiateDimsInReturnType loc fname =<< normTypeFully tp2
    argtype' <- normTypeFully argtype

    -- Check whether this would produce an impossible return type.
    let (tp2_produced_dims, tp2_paramdims) = dimUses tp2'
        problematic = S.fromList ext <> boundInsideType argtype'
        problem = any (`S.member` problematic) (tp2_paramdims `S.difference` tp2_produced_dims)
    when (not (S.null problematic) && problem) $ do
      typeError loc mempty . withIndexLink "existential-param-ret" $
        "Existential size would appear in function parameter of return type:"
          </> indent 2 (pretty (RetType ext tp2'))
          </> textwrap "This is usually because a higher-order function is used with functional arguments that return existential sizes or locally named sizes, which are then used as parameters of other function arguments."

    (argext, tp2'') <-
      case pname of
        Named pname'
          | S.member pname' (fvVars $ freeInType tp2') ->
              if hasBinding argexp
                then do
                  warn (srclocOf argexp) $
                    withIndexLink
                      "size-expression-bind"
                      "Size expression with binding is replaced by unknown size."
                  d <- newRigidDim argexp (RigidArg fname $ prettyTextOneLine $ bareExp argexp) "n"
                  let parsubst v =
                        if v == pname'
                          then Just $ ExpSubst $ sizeFromName (qualName d) $ srclocOf argexp
                          else Nothing
                  pure (Just d, applySubst parsubst $ toStruct tp2')
                else
                  let parsubst v =
                        if v == pname'
                          then Just $ ExpSubst $ fromMaybe argexp $ stripExp argexp
                          else Nothing
                   in pure (Nothing, applySubst parsubst $ toStruct tp2')
        _ -> pure (Nothing, toStruct tp2')

    let am' =
          AutoMap
            { autoRep = am_rep_shape,
              autoMap = am_map_shape,
              autoFrame = am_frame_shape
            }

    pure (tp1, distribute (arrayOf (autoMap am') tp2''), argext, ext, am')
  where
    distribute :: TypeBase dim u -> TypeBase dim u
    distribute (Array u s (Arrow _ _ _ ta (RetType rd tr))) =
      Scalar $
        Arrow
          u
          Unnamed
          mempty
          (arrayOf s ta)
          (RetType rd $ distribute (arrayOfWithAliases (uniqueness tr) s tr))
    distribute t = t
checkApply _ _ _ _ _ =
  error "checkApply: array"

-- | Type-check a single expression in isolation.  This expression may
-- turn out to be polymorphic, in which case the list of type
-- parameters will be non-empty.
checkOneExp :: ExpBase NoInfo VName -> TypeM ([TypeParam], Exp)
checkOneExp e = do
  (maybe_tysubsts, e') <- Unsized.checkSingleExp e
  case maybe_tysubsts of
    Left err -> throwError err
    Right (_generalised, tysubsts) -> runTermTypeM checkExp tysubsts $ do
      e'' <- checkExp e'
      let t = typeOf e''
      (tparams, _, RetType _ t') <-
        letGeneralise (nameFromString "<exp>") (srclocOf e) [] [] $ toRes Nonunique t
      fixOverloadedTypes $ typeVars t'
      e''' <- normTypeFully e''
      localChecks e'''
      causalityCheck e'''
      pure (tparams, e''')

-- | Type-check a single size expression in isolation.  This expression may
-- turn out to be polymorphic, in which case it is unified with i64.
checkSizeExp :: ExpBase NoInfo VName -> TypeM Exp
checkSizeExp e = do
  (maybe_tysubsts, e') <- Unsized.checkSizeExp e
  case maybe_tysubsts of
    Left err -> throwError err
    Right (_generalised, tysubsts) -> runTermTypeM checkExp tysubsts $ do
      e'' <- checkExp e'
      when (hasBinding e'') $
        typeError (srclocOf e'') mempty . withIndexLink "size-expression-bind" $
          "Size expression with binding is forbidden."
      normTypeFully e''

-- Verify that all sum type constructors and empty array literals have
-- a size that is known (rigid or a type parameter).  This is to
-- ensure that we can actually determine their shape at run-time.
causalityCheck :: Exp -> TermTypeM ()
causalityCheck binding_body = do
  constraints <- getConstraints

  let checkCausality what known t loc
        | (d, dloc) : _ <-
            mapMaybe (unknown constraints known) $
              S.toList (fvVars $ freeInType t) =
            Just $ lift $ causality what (locOf loc) d dloc t
        | otherwise = Nothing

      checkParamCausality known p =
        checkCausality (pretty p) known (patternType p) (locOf p)

      collectingNewKnown = lift . flip execStateT mempty

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
      onExp known e@(Lambda params body _ _ _)
        | bad : _ <- mapMaybe (checkParamCausality known) params =
            bad
        | otherwise = do
            -- Existentials coming into existence in the lambda body
            -- are not known outside of it.
            void $ collectingNewKnown $ onExp known body
            pure e
      onExp known e@(AppExp (LetPat _ _ bindee_e body_e _) (Info res)) = do
        sequencePoint known bindee_e body_e $ appResExt res
        pure e
      onExp known e@(AppExp (Match scrutinee cs _) (Info res)) = do
        new_known <- collectingNewKnown $ onExp known scrutinee
        void $ recurse (new_known <> known) cs
        modify ((new_known <> S.fromList (appResExt res)) <>)
        pure e
      onExp known e@(AppExp (Apply f args _) (Info res)) = do
        seqArgs known $ reverse $ NE.toList args
        pure e
        where
          seqArgs known' [] = do
            void $ onExp known' f
            modify (S.fromList (appResExt res) <>)
          seqArgs known' ((Info (p, _), x) : xs) = do
            new_known <- collectingNewKnown $ onExp known' x
            void $ seqArgs (new_known <> known') xs
            modify ((new_known <> S.fromList (maybeToList p)) <>)
      onExp known e@(Constr v args (Info t) loc) = do
        seqArgs known args
        pure e
        where
          seqArgs known' []
            | Just bad <- checkCausality (dquotes ("#" <> pretty v)) known' t loc =
                bad
            | otherwise =
                pure ()
          seqArgs known' (x : xs) = do
            new_known <- collectingNewKnown $ onExp known' x
            void $ seqArgs (new_known <> known') xs
            modify (new_known <>)
      onExp
        known
        e@(AppExp (BinOp (f, floc) ft (x, Info (xp, _)) (y, Info (yp, _)) _) (Info res)) = do
          args_known <-
            collectingNewKnown $ sequencePoint known x y $ catMaybes [xp, yp]
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
        new_known <- collectingNewKnown $ onExp known x
        void $ onExp (new_known <> known) y
        modify ((new_known <> S.fromList ext) <>)

  either throwError (const $ pure ()) $
    evalStateT (onExp mempty binding_body) mempty
  where
    unknown constraints known v = do
      guard $ v `S.notMember` known
      loc <- case snd <$> M.lookup v constraints of
        Just (UnknownSize loc _) -> Just loc
        _ -> Nothing
      pure (v, loc)

    causality what loc d dloc t =
      Left . TypeError loc mempty . withIndexLink "causality-check" $
        "Causality check: size"
          <+> dquotes (prettyName d)
          <+> "needed for type of"
          <+> what
          <> colon
            </> indent 2 (pretty t)
            </> "But"
            <+> dquotes (prettyName d)
            <+> "is computed at"
            <+> pretty (locStrRel loc dloc)
          <> "."
            </> ""
            </> "Hint:"
            <+> align
              ( textwrap "Bind the expression producing"
                  <+> dquotes (prettyName d)
                  <+> "with 'let' beforehand."
              )

mustBeIrrefutable :: (MonadTypeChecker f) => Pat StructType -> f ()
mustBeIrrefutable p = do
  case unmatched [p] of
    [] -> pure ()
    ps' ->
      typeError p mempty . withIndexLink "refutable-pattern" $
        "Refutable pattern not allowed here.\nUnmatched cases:"
          </> indent 2 (stack (map pretty ps'))

supportsEquality :: TypeBase dim u -> Bool
supportsEquality (Array _ _ t) = supportsEquality $ Scalar t
supportsEquality (Scalar Prim {}) = True
supportsEquality (Scalar TypeVar {}) = False
supportsEquality (Scalar (Record fs)) = all supportsEquality fs
supportsEquality (Scalar (Sum fs)) = all (all supportsEquality) fs
supportsEquality (Scalar Arrow {}) = False

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
    check e@(AppExp (LetPat _ p _ _ _) _) =
      mustBeIrrefutable p *> recurse e
    check e@(AppExp (BinOp (v, loc) _ (x, _) _ _) _)
      | qualLeaf v == intrinsicVar "==" =
          checkEquality loc (typeOf x) *> recurse e
    check e@(Var v (Info t) loc)
      | qualLeaf v == intrinsicVar "==" =
          checkEquality loc t *> recurse e
    check e@(Lambda ps _ _ _ _) =
      mapM_ (mustBeIrrefutable . fmap toStruct) ps *> recurse e
    check e@(AppExp (LetFun _ (_, ps, _, _, _) _ _) _) =
      mapM_ (mustBeIrrefutable . fmap toStruct) ps *> recurse e
    check e@(AppExp (Loop _ p _ form _ _) _) = do
      mustBeIrrefutable (fmap toStruct p)
      case form of
        ForIn form_p _ -> mustBeIrrefutable form_p
        _ -> pure ()
      recurse e
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
    check e = recurse e
    recurse = astMap identityMapper {mapOnExp = check}

    checkEquality loc t =
      unless (supportsEquality t) $
        typeError loc mempty $
          "Comparing equality of values of type"
            </> indent 2 (pretty t)
            </> "which does not support equality."

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

-- | This is "fixing" as in "setting them", not "correcting them".  We
-- only make very conservative fixing.
fixOverloadedTypes :: Names -> TermTypeM ()
fixOverloadedTypes tyvars_at_toplevel =
  getConstraints >>= mapM_ fixOverloaded . M.toList . M.map snd
  where
    fixOverloaded (v, NoConstraint _ usage) = do
      -- See #1552.
      unify usage (Scalar (TypeVar mempty (qualName v) [])) $
        Scalar (tupleRecord [])
      when (v `S.member` tyvars_at_toplevel) $
        warn usage "Defaulting ambiguous type to ()."
    fixOverloaded (v, Size Nothing (Usage Nothing loc)) =
      typeError loc mempty . withIndexLink "ambiguous-size" $
        "Ambiguous size" <+> dquotes (prettyName v) <> "."
    fixOverloaded (v, Size Nothing (Usage (Just u) loc)) =
      typeError loc mempty . withIndexLink "ambiguous-size" $
        "Ambiguous size" <+> dquotes (prettyName v) <+> "arising from" <+> pretty u <> "."
    fixOverloaded _ = pure ()

hiddenParamNames :: [Pat ParamType] -> [VName]
hiddenParamNames params = hidden
  where
    param_all_names = mconcat $ map patNames params
    named (Named x, _, _) = Just x
    named (Unnamed, _, _) = Nothing
    param_names =
      S.fromList $ mapMaybe (named . patternParam) params
    hidden = filter (`notElem` param_names) param_all_names

inferredReturnType :: SrcLoc -> [Pat ParamType] -> StructType -> TermTypeM StructType
inferredReturnType loc params t = do
  -- The inferred type may refer to names that are bound by the
  -- parameter patterns, but which will not be visible in the type.
  -- These we must turn into fresh type variables, which will be
  -- existential in the return type.
  fst <$> unscopeType loc hidden_params t
  where
    hidden_params = filter (`elem` hidden) $ foldMap patNames params
    hidden = hiddenParamNames params

checkBinding ::
  ( VName,
    Maybe (TypeExp Exp VName),
    [TypeParam],
    [PatBase Info VName ParamType],
    ExpBase Info VName,
    SrcLoc
  ) ->
  TermTypeM
    ( [TypeParam],
      [Pat ParamType],
      Maybe (TypeExp Exp VName),
      ResRetType,
      Exp
    )
checkBinding (fname, maybe_retdecl, tparams, params, body, loc) =
  incLevel . bindingParams tparams params $ \params' -> do
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
        ret' <- normTypeFully ret
        pure (Just retdecl', ret')
      Nothing
        | null params ->
            pure (Nothing, toRes Nonunique body_t)
        | otherwise -> do
            body_t' <- inferredReturnType loc params'' body_t
            pure (Nothing, toRes Nonunique body_t')

    verifyFunctionParams (Just fname) params''

    (tparams', params''', rettype') <-
      letGeneralise (baseName fname) loc tparams params''
        =<< unscopeUnknown rettype

    when
      ( null params
          && any isSizeParam tparams'
          && not (null (retDims rettype'))
      )
      $ typeError loc mempty
      $ textwrap "A size-polymorphic value binding may not have a type with an existential size."
        </> "Type of this binding is:"
        </> indent 2 (pretty rettype')
        </> "with the following type parameters:"
        </> indent 2 (sep $ map pretty $ filter isSizeParam tparams')

    pure (tparams', params''', maybe_retdecl'', rettype', body')

-- | Extract all the shape names that occur in positive position
-- (roughly, left side of an arrow) in a given type.
sizeNamesPos :: TypeBase Size als -> S.Set VName
sizeNamesPos (Scalar (Arrow _ _ _ t1 (RetType _ t2))) = onParam t1 <> sizeNamesPos t2
  where
    onParam :: TypeBase Size als -> S.Set VName
    onParam (Scalar Arrow {}) = mempty
    onParam (Scalar (Record fs)) = mconcat $ map onParam $ M.elems fs
    onParam (Scalar (TypeVar _ _ targs)) = mconcat $ map onTypeArg targs
    onParam t = fvVars $ freeInType t
    onTypeArg (TypeArgDim (Var d _ _)) = S.singleton $ qualLeaf d
    onTypeArg (TypeArgDim _) = mempty
    onTypeArg (TypeArgType t) = onParam t
sizeNamesPos _ = mempty

-- | Verify certain restrictions on function parameters, and bail out
-- on dubious constructions.
--
-- These restrictions apply to all functions (anonymous or otherwise).
-- Top-level functions have further restrictions that are checked
-- during let-generalisation.
verifyFunctionParams :: Maybe VName -> [Pat ParamType] -> TermTypeM ()
verifyFunctionParams fname params =
  onFailure (CheckingParams (baseName <$> fname)) $
    verifyParams (foldMap patNames params) =<< mapM updateTypes params
  where
    verifyParams forbidden (p : ps)
      | d : _ <- filter (`elem` forbidden) $ S.toList $ fvVars $ freeInPat p =
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
            (Named v, _, _) -> delete v forbidden
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
injectExt :: [VName] -> TypeBase Size u -> RetTypeBase Size u
injectExt [] ret = RetType [] ret
injectExt ext ret = RetType ext_here $ deeper ret
  where
    (immediate, _) = dimUses ret
    (ext_here, ext_there) = partition (`S.member` immediate) ext
    deeper :: TypeBase Size u -> TypeBase Size u
    deeper (Scalar (Prim t)) = Scalar $ Prim t
    deeper (Scalar (Record fs)) = Scalar $ Record $ M.map deeper fs
    deeper (Scalar (Sum cs)) = Scalar $ Sum $ M.map (map deeper) cs
    deeper (Scalar (Arrow als p d1 t1 (RetType t2_ext t2))) =
      Scalar $ Arrow als p d1 t1 $ injectExt (nubOrd (ext_there <> t2_ext)) t2
    deeper (Scalar (TypeVar u tn targs)) =
      Scalar $ TypeVar u tn $ map deeperArg targs
    deeper t@Array {} = t

    deeperArg (TypeArgType t) = TypeArgType $ deeper t
    deeperArg (TypeArgDim d) = TypeArgDim d

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
  ResType ->
  Constraints ->
  TermTypeM ([TypeParam], ResRetType)
closeOverTypes defname defloc tparams paramts ret substs = do
  (more_tparams, retext) <-
    partitionEithers . catMaybes
      <$> mapM closeOver (M.toList $ M.map snd to_close_over)
  let mkExt v =
        case M.lookup v substs of
          Just (_, UnknownSize {}) -> Just v
          _ -> Nothing

  pure
    ( tparams
        ++ more_tparams,
      injectExt (nubOrd $ retext ++ mapMaybe mkExt (S.toList $ fvVars $ freeInType ret)) ret
    )
  where
    -- Diet does not matter here.
    t = foldFunType (map (toParam Observe) paramts) $ RetType [] ret
    visible = typeVars t <> fvVars (freeInType t)
    to_close_over =
      M.filterWithKey (\k _ -> k `S.member` visible) substs

    (produced_sizes, param_sizes) = dimUses t

    -- Avoid duplicate type parameters.
    closeOver (k, _)
      | k `elem` map typeParamName tparams =
          pure Nothing
    closeOver (k, NoConstraint l usage) =
      pure $ Just $ Left $ TypeParamType l k $ srclocOf usage
    closeOver (k, ParamType l loc) =
      pure $ Just $ Left $ TypeParamType l k $ srclocOf loc
    closeOver (k, Size Nothing usage) =
      pure $ Just $ Left $ TypeParamDim k $ srclocOf usage
    closeOver (k, UnknownSize _ _)
      | k `S.member` param_sizes,
        k `S.notMember` produced_sizes = do
          notes <- dimNotes defloc $ sizeFromName (qualName k) mempty
          typeError defloc notes . withIndexLink "unknown-param-def" $
            "Unknown size"
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
  [Pat ParamType] ->
  ResType ->
  TermTypeM ([TypeParam], [Pat ParamType], ResRetType)
letGeneralise defname defloc tparams params restype =
  onFailure (CheckingLetGeneralise defname) $ do
    now_substs <- getConstraints

    -- Candidates for let-generalisation are those type variables that
    --
    -- (1) were not known before we checked this function, and
    --
    -- (2) are not used in the (new) definition of any type variables
    -- known before we checked this function.

    -- Criteria (1) and (2) is implemented by looking at the binding
    -- level of the type variables.

    cur_lvl <- curLevel
    let candidate (lvl, _) = lvl >= (cur_lvl - length params)
        new_substs = M.filter candidate now_substs

    (tparams', RetType ret_dims restype') <-
      closeOverTypes
        defname
        defloc
        tparams
        (map patternStructType params)
        restype
        new_substs

    restype'' <- updateTypes restype'

    let used_sizes =
          freeInType restype'' <> foldMap (freeInType . patternType) params
    case filter ((`S.notMember` fvVars used_sizes) . typeParamName) $
      filter isSizeParam tparams' of
      [] -> pure ()
      tp : _ -> unusedSize $ SizeBinder (typeParamName tp) (srclocOf tp)

    -- We keep those type variables that were not closed over by
    -- let-generalisation.
    modifyConstraints $ M.filterWithKey $ \k _ -> k `notElem` map typeParamName tparams'

    pure (tparams', params, RetType ret_dims restype'')

checkFunBody ::
  [Pat ParamType] ->
  Exp ->
  Maybe ResType ->
  SrcLoc ->
  TermTypeM Exp
checkFunBody params body maybe_rettype loc = do
  body' <- checkExp body

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
          (filter (`elem` hidden) $ foldMap patNames params)
          body_t
      case find (`elem` hidden) $ fvVars $ freeInType rettype of
        Just v ->
          typeError loc mempty $
            "The return type annotation"
              </> indent 2 (align (pretty rettype))
              </> "refers to the name"
              <+> dquotes (prettyName v)
              <+> "which is bound to an inner component of a function parameter."
        Nothing -> do
          let usage = mkUsage body "return type annotation"
          onFailure (CheckingReturn rettype body_t') $
            unify usage (toStruct rettype) body_t'
    Nothing -> pure ()

  pure body'

arrayOfM ::
  SrcLoc ->
  StructType ->
  Shape Size ->
  TermTypeM StructType
arrayOfM loc t shape = do
  arrayElemType (mkUsage loc "use as array element") "type used in array" t
  pure $ arrayOf shape t

-- | Type-check a top-level (or module-level) function definition.
-- Despite the name, this is also used for checking constant
-- definitions, by treating them as 0-ary functions.
checkFunDef ::
  ( VName,
    Maybe (TypeExp (ExpBase NoInfo VName) VName),
    [TypeParam],
    [PatBase NoInfo VName ParamType],
    ExpBase NoInfo VName,
    SrcLoc
  ) ->
  TypeM
    ( [TypeParam],
      [Pat ParamType],
      Maybe (TypeExp Exp VName),
      ResRetType,
      Exp
    )
checkFunDef (fname, retdecl, tparams, params, body, loc) =
  doChecks =<< Unsized.checkValDef (fname, retdecl, tparams, params, body, loc)
  where
    -- TODO: Print out the possibilities. (And also potentially eliminate
    --- some of the possibilities to disambiguate).

    doChecks (maybe_tysubsts, params', retdecl', body') =
      case maybe_tysubsts of
        Left err -> throwError err
        Right (generalised, tysubsts) ->
          runTermTypeM checkExp tysubsts $ do
            (tparams', params'', retdecl'', RetType dims rettype', body'') <-
              checkBinding (fname, retdecl', generalised <> tparams, params', body', loc)

            -- Since this is a top-level function, we also resolve overloaded
            -- types, using either defaults or complaining about ambiguities.
            fixOverloadedTypes $
              typeVars rettype' <> foldMap (typeVars . patternType) params''

            -- Then replace all inferred types in the body and parameters.
            body''' <- normTypeFully body''
            params''' <- mapM normTypeFully params''
            retdecl''' <- traverse updateTypes retdecl''
            rettype'' <- normTypeFully rettype'

            -- Check if the function body can actually be evaluated.
            causalityCheck body'''

            -- Check for various problems.
            mapM_ (mustBeIrrefutable . fmap toStruct) params''
            localChecks body'''

            let ((body'''', updated_ret), errors) =
                  Consumption.checkValDef
                    ( fname,
                      params''',
                      body''',
                      RetType dims rettype'',
                      retdecl''',
                      loc
                    )

            mapM_ throwError errors

            pure (tparams', params''', retdecl''', updated_ret, body'''')
