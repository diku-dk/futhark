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
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bitraversable
import Data.Char (isAscii)
import Data.Either
import Data.List (delete, find, genericLength, partition)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.Util (mapAccumLM, nubOrd, topologicalSort)
import Futhark.Util.Pretty hiding (space)
import Language.Futhark
import Language.Futhark.Primitive (intByteSize)
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Consumption qualified as Consumption
import Language.Futhark.TypeChecker.Match
import Language.Futhark.TypeChecker.Monad hiding (BoundV, lookupAbsTy, lookupMod)
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
  e1_t <- expType e1
  e2_t <- expType e2
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
            (j, Info Nothing)
            (i, Info Nothing)
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

-- Expressions witnessed by type, topologically sorted.
topWit :: TypeBase Exp u -> [Exp]
topWit = topologicalSort depends . witnessedExps
  where
    witnessedExps t = execState (traverseDims onDim t) mempty
      where
        onDim _ PosImmediate e = modify (e :)
        onDim _ _ _ = pure ()
    depends a b = any (sameExp b) $ subExps a

sizeFree ::
  (MonadUnify m) =>
  SrcLoc ->
  (Exp -> Maybe VName) ->
  TypeBase Size u ->
  m (TypeBase Size u, [VName])
sizeFree tloc expKiller orig_t = do
  runReaderT (toBeReplaced orig_t $ onType orig_t) mempty `runStateT` mempty
  where
    lookReplacement e repl = snd <$> L.find (sameExp e . fst) repl
    expReplace mapping e
      | Just e' <- lookReplacement e mapping = e'
      | otherwise = runIdentity $ astMap mapper e
      where
        mapper = identityMapper {mapOnExp = pure . expReplace mapping}

    replacing e = do
      e' <- asks (`expReplace` e)
      case expKiller e' of
        Nothing -> pure e'
        Just cause -> do
          vn <- lift $ lift $ newRigidDim tloc (RigidOutOfScope (locOf e) cause) "d"
          modify (vn :)
          pure $ sizeFromName (qualName vn) (srclocOf e)

    toBeReplaced t m' = foldl f m' $ topWit t
      where
        f m e = do
          e' <- replacing e
          local ((e, e') :) m

    onScalar (Record fs) =
      Record <$> traverse onType fs
    onScalar (Sum cs) =
      Sum <$> (traverse . traverse) onType cs
    onScalar (Arrow as pn d argT (RetType dims retT)) = do
      argT' <- onType argT
      old_bound <- get
      retT' <- toBeReplaced retT $ onType retT
      rl <- state $ L.partition (`notElem` old_bound)
      let dims' = dims <> rl
      pure $ Arrow as pn d argT' (RetType dims' retT')
    onScalar (TypeVar u v args) =
      TypeVar u v <$> mapM onTypeArg args
      where
        onTypeArg (TypeArgDim d) = TypeArgDim <$> replacing d
        onTypeArg (TypeArgType ty) = TypeArgType <$> onType ty
    onScalar (Prim pt) = pure $ Prim pt

    onType ::
      (MonadUnify m) =>
      TypeBase Size u ->
      ReaderT [(Exp, Exp)] (StateT [VName] m) (TypeBase Size u)
    onType (Array u shape scalar) =
      Array u <$> traverse replacing shape <*> onScalar scalar
    onType (Scalar ty) =
      Scalar <$> onScalar ty

-- Remove unknown sizes from function body types before we perform
-- let-generalisation. This is because if a function is inferred to return
-- something of type '[x+y]t' where 'x' or 'y' are unknown, we want to turn that
-- into '[z]t', where 'z' is a fresh unknown, which is then by
-- let-generalisation turned into '?[z].[z]t'.
unscopeUnknown ::
  TypeBase Size u ->
  TermTypeM (TypeBase Size u)
unscopeUnknown t = do
  constraints <- getConstraints
  -- The killer only ever fires on an unknown-size variable, so if none occurs
  -- free in the type there is nothing to do and we can skip the traversal (and
  -- the witness computation) entirely.
  if not (any (isUnknown constraints) (fvVars (freeInType t)))
    then pure t
    else -- These sizes will be immediately turned into existentials, so we do
    -- not need to care about their location.
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
unscopeType tloc unscoped t
  -- Fast-path for common case where 't' has no free variables in unscoped.
  | not (any (`elem` unscoped) (fvVars (freeInType t))) = pure (t, [])
  | otherwise =
      sizeFree tloc (find (`elem` unscoped) . fvVars . freeInExp) t

checkExp :: Exp -> TermTypeM Exp
checkExp (Var qn (Info t) loc) = do
  t' <- lookupVar loc qn t
  pure $ Var qn (Info t') loc
checkExp (Literal val loc) =
  pure $ Literal val loc
checkExp (Hole (Info t) loc) = do
  t' <- replaceTyVarsAbsorbable loc t
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
checkExp (ArrayLit all_es (Info t) loc) =
  -- We only consult the type inferred by the unsized type checker
  -- for empty arrays; otherwise we use the type of the first
  -- element.  This significantly cuts down on the number of
  -- inferred types we have to instantiate for pathologically large
  -- multidimensional array literals.
  case all_es of
    [] -> do
      t' <- replaceTyVars loc t
      case peelArray 1 t' of
        Just et -> do
          let t'' = arrayOf (Shape [sizeFromInteger 0 mempty]) et
          unify (mkUsage loc "empty array literal") t'' t'
          pure $ ArrayLit [] (Info t'') loc
        Nothing -> error $ "checkExp ArrayLit: " <> prettyString t'
    e : es -> do
      e' <- checkExp e
      et <- expType e'
      es' <- mapM (unifies "type of first array element" et <=< checkExp) es
      let arr_t = arrayOf (Shape [sizeFromInteger (genericLength all_es) mempty]) et
      pure $ ArrayLit (e' : es') (Info arr_t) loc
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

  let t = arrayOf (Shape [dim]) start_t
      res = AppRes t (maybeToList retext)

  pure $ AppExp (Range start' maybe_step' end' loc) (Info res)
  where
    i64 = Scalar $ Prim $ Signed Int64
    mkBinOp op t x y =
      AppExp
        ( BinOp
            (qualName (intrinsicVar op), mempty)
            sizeBinOpInfo
            (x, Info Nothing)
            (y, Info Nothing)
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
  args' <- mapM (checkExp . snd) args
  t <- expType fe'
  let fname =
        case fe' of
          Var v _ _ -> Just v
          _ -> Nothing
  ((_, exts, rt), args'') <- mapAccumLM (onArg fname) (0, [], t) args'

  pure $ AppExp (Apply fe' args'' loc) $ Info $ AppRes rt exts
  where
    onArg fname (i, all_exts, t) arg' = do
      (_, rt, argext, exts) <- checkApply loc (fname, i) t arg'
      pure
        ( (i + 1, all_exts <> exts, rt),
          (Info argext, arg')
        )
checkExp (AppExp (BinOp (op, oploc) (Info op_t) (e1, _) (e2, _) loc) _) = do
  ftype <- lookupVar oploc op op_t
  e1' <- checkExp e1
  e2' <- checkExp e2
  -- Note that the application to the first operand cannot fix any
  -- existential sizes, because it must by necessity be a function.
  (_, rt, p1_ext, _) <- checkApply loc (Just op, 0) ftype e1'
  (_, rt', p2_ext, retext) <- checkApply loc (Just op, 1) rt e2'

  pure $
    AppExp
      ( BinOp
          (op, oploc)
          (Info ftype)
          (e1', Info p1_ext)
          (e2', Info p2_ext)
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
    (body_t', retext) <-
      case (t, patNames pat') of
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
    checkBinding (fst name, maybe_retdecl, tparams, params, e, loc)

  let entry = BoundV tparams' $ funType params' rettype
      bindF scope =
        scope
          { scopeVtable = M.insert (fst name) entry $ scopeVtable scope
          }
  body' <- localScope bindF $ checkExp body

  (body_t, ext) <- unscopeType loc [fst name] =<< expTypeFully body'

  pure $
    AppExp
      ( LetFun
          name
          (tparams', params', maybe_retdecl', Info rettype, e')
          body'
          loc
      )
      (Info $ AppRes body_t ext)
checkExp (AppExp (LetWith dest src steps ve body loc) _) = do
  -- The type recorded in the AST is the unsized type from the
  -- unsized type checker; we must consult the scope to get the
  -- actual type of the source variable.
  src_t <-
    normTypeFully
      =<< lookupVar (srclocOf src) (qualName $ identName src) (unInfo $ identType src)
  let src' = src {identType = Info src_t}

  case mapAndUnzipM isField steps of
    Just (steps', names) -> do
      ve' <- checkExp ve
      ve_t <- expType ve'
      updated_t <- updateFieldPath src names ve_t src_t

      let dest' = dest {identType = Info updated_t}
      bindingIdent dest' $ do
        body' <- checkExp body
        (body_t, ext) <- unscopeType loc [identName dest'] =<< expTypeFully body'
        pure $ AppExp (LetWith dest' src' steps' ve' body' loc) (Info $ AppRes body_t ext)
    Nothing -> do
      (steps', target_t) <- checkUpdateSteps loc src_t steps
      ve' <- unifies "type of update target" target_t =<< checkExp ve

      let dest' = dest {identType = Info src_t}
      bindingIdent dest' $ do
        body' <- checkExp body
        (body_t, ext) <- unscopeType loc [identName dest'] =<< expTypeFully body'
        pure $ AppExp (LetWith dest' src' steps' ve' body' loc) (Info $ AppRes body_t ext)
  where
    isField (UpdateStepField f) = Just (UpdateStepField f, f)
    isField _ = Nothing

-- Record updates are a bit hacky, because we do not have row typing
-- (yet?).  For now, we only permit record updates where we know the
-- full type up to the field we are updating.
checkExp (Update src steps ve _ loc) = do
  src' <- checkExp src
  src_t <- expTypeFully src'
  case mapAndUnzipM isField steps of
    Just (steps', names) -> do
      ve' <- checkExp ve
      ve_t <- expType ve'
      updated_t <- updateFieldPath src names ve_t src_t
      pure $ Update src' steps' ve' (Info updated_t) loc
    Nothing -> do
      (steps', target_t) <- checkUpdateSteps loc src_t steps
      ve' <- unifies "type of update target" target_t =<< checkExp ve
      src_t' <- expTypeFully src'
      pure $ Update src' steps' ve' (Info src_t') loc
  where
    isField (UpdateStepField f) = Just (UpdateStepField f, f)
    isField _ = Nothing
checkExp (AppExp (Index e slice loc) _) = do
  slice' <- checkSlice slice
  e' <- checkExp e
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
      -- The sizes of the return type are absorbable, as a lambda
      -- returns whatever type the context requires. See Note [Size
      -- Inference].
      rt' <- replaceTyVarsAbsorbable loc rt
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
          -- Only rigid sizes computed by the body can be hidden. A
          -- size that is still flexible has not been determined yet,
          -- and hiding it would sever its connection to whatever the
          -- enclosing context determines it to be.
          rigid UnknownSize {} = True
          rigid _ = False
          hide k (lvl, c) =
            rigid c && lvl >= cur_lvl && k `notElem` param_names && k `S.notMember` pos_sizes

      hidden_sizes <-
        S.fromList . M.keys . M.filterWithKey hide <$> getConstraints

      let onDim name
            | name `S.member` hidden_sizes = S.singleton name
          onDim _ = mempty

      pure $ RetType (S.toList $ foldMap onDim $ fvVars $ freeInType ret) ret
checkExp (OpSection op (Info op_t) loc) = do
  ftype <- lookupVar loc op op_t
  pure $ OpSection op (Info ftype) loc
checkExp (OpSectionLeft op (Info op_t) e _ _ loc) = do
  ftype <- lookupVar loc op op_t
  e' <- checkExp e
  (t1, rt, argext, retext) <- checkApply loc (Just op, 0) ftype e'
  case (ftype, rt) of
    (Scalar (Arrow _ m1 d1 _ _), Scalar (Arrow _ m2 d2 t2 (RetType ds rt2))) ->
      pure $
        OpSectionLeft
          op
          (Info ftype)
          e'
          (Info (m1, toParam d1 t1, argext), Info (m2, toParam d2 t2))
          (Info $ RetType ds rt2, Info retext)
          loc
    _ ->
      typeError loc mempty $
        "Operator section with invalid operator of type" <+> pretty ftype
checkExp (OpSectionRight op (Info op_t) e _ _ loc) = do
  ftype <- lookupVar loc op op_t
  e' <- checkExp e
  case ftype of
    Scalar (Arrow _ m1 d1 t1 (RetType [] (Scalar (Arrow _ m2 d2 t2 (RetType dims2 ret))))) -> do
      (t2', arrow', argext, _) <-
        checkApply
          loc
          (Just op, 1)
          (Scalar $ Arrow mempty m2 d2 t2 $ RetType [] $ Scalar $ Arrow Nonunique m1 d1 t1 $ RetType dims2 ret)
          e'
      case arrow' of
        Scalar (Arrow _ _ _ t1' (RetType dims2' ret')) ->
          pure $
            OpSectionRight
              op
              (Info ftype)
              e'
              (Info (m1, toParam d1 t1'), Info (m2, toParam d2 t2', argext))
              (Info $ RetType dims2' ret')
              loc
        _ -> error $ "OpSectionRight: impossible type\n" <> prettyString arrow'
    _ ->
      typeError loc mempty $
        "Operator section with invalid operator of type" <+> pretty ftype
checkExp (UpdateSection steps (Info ft) loc) = do
  -- The unsized type checker has already determined the type of the
  -- parameter; we just have to instantiate its sizes. The result
  -- type is then computed by walking the steps, such that its sizes
  -- are those of the corresponding components of the parameter type.
  a <- case ft of
    Scalar (Arrow _ _ _ pt _) -> replaceTyVars loc pt
    _ -> error $ "checkExp UpdateSection: " <> prettyString ft
  (steps', b, retext) <- checkSectionSteps a steps
  let ft' = Scalar $ Arrow mempty Unnamed Observe a $ RetType retext $ toRes Nonunique b
  pure $ UpdateSection steps' (Info ft') loc
  where
    checkSectionSteps t [] =
      pure ([], t, [])
    checkSectionSteps t (step : rest) =
      case step of
        UpdateStepField f -> do
          t' <- normTypeFully t
          case t' of
            Scalar (Record fs)
              | Just f_t <- M.lookup f fs -> do
                  (rest', target_t, retext) <- checkSectionSteps f_t rest
                  pure (UpdateStepField f : rest', target_t, retext)
            _ ->
              error $
                "checkExp UpdateSection: cannot project field "
                  <> prettyString f
                  <> " from "
                  <> prettyString t'
        UpdateStepSlice slice -> do
          slice' <- checkSlice slice
          (t', retext) <- sliceShape Nothing slice' =<< normTypeFully t
          (rest', target_t, retext_rest) <- checkSectionSteps t' rest
          pure (UpdateStepSlice slice' : rest', target_t, retext <> retext_rest)
checkExp (AppExp (Loop _ mergepat loopinit form loopbody loc) _) = do
  ((sparams, mergepat', loopinit', form', loopbody'), appres) <-
    checkLoop checkExp (mergepat, loopinit, form, loopbody) loc
  pure $
    AppExp
      (Loop sparams mergepat' loopinit' form' loopbody' loc)
      (Info appres)
checkExp (Constr name es (Info t) loc) = do
  -- The sizes are absorbable: those of the payloads of the other
  -- constructors (and any not determined by the arguments) are
  -- adopted from the context, like the sizes of a hole. See Note
  -- [Size Inference].
  t' <- replaceTyVarsAbsorbable loc t
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
  (t, retext) <- unifyBranches loc e2' e3'
  pure $ AppExp (If e1' e2' e3' loc) (Info $ AppRes t retext)
checkExp (AppExp (Match e cs loc) _) = do
  e' <- checkExp e
  mt <- expType e'
  (cs', t, retext) <- checkCases mt cs

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

updateFieldPath ::
  (Pretty a, Located a) =>
  a ->
  [Name] ->
  StructType ->
  StructType ->
  TermTypeM StructType
updateFieldPath src all_fs ve_t = recurse [] all_fs
  where
    recurse seen [] t = do
      (t', _) <- allDimsFreshInType usage Nonrigid "any" t
      onFailure (CheckingRecordUpdate seen t' ve_t) $
        unify usage t' ve_t
      pure ve_t
      where
        usage = mkUsage (locOf src) "record update"
    recurse seen (f : fs) (Scalar (Record m))
      | Just f_t <- M.lookup f m = do
          f_t' <- recurse (seen ++ [f]) fs f_t
          pure $ Scalar $ Record $ M.insert f f_t' m
    recurse _ _ _ =
      typeError (locOf src) mempty . withIndexLink "record-type-not-known" $
        "Full type of"
          </> indent 2 (pretty src)
          </> textwrap " is not known at this point.  Add a type annotation to the original record to disambiguate."

checkUpdateSteps ::
  SrcLoc ->
  StructType ->
  [UpdateStep Info VName] ->
  TermTypeM ([UpdateStep Info VName], StructType)
checkUpdateSteps _ t [] =
  pure ([], t)
checkUpdateSteps loc t (step : rest) =
  case step of
    UpdateStepSlice slice -> do
      slice' <- checkSlice slice
      (elem_t, _) <- sliceShape (Just (loc, Nonrigid)) slice' =<< normTypeFully t
      (rest', target_t) <- checkUpdateSteps loc elem_t rest
      pure (UpdateStepSlice slice' : rest', target_t)
    UpdateStepField f -> do
      t' <- normTypeFully t
      case t' of
        Scalar (Record fs) | Just f_t <- M.lookup f fs -> do
          (rest', target_t) <- checkUpdateSteps loc f_t rest
          pure (UpdateStepField f : rest', target_t)
        _ -> error $ "checkUpdateSteps: " <> show t'

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

checkSlice :: SliceBase Info VName -> TermTypeM [DimIndex]
checkSlice = mapM checkDimIndex
  where
    checkDimIndex (DimFix i) =
      DimFix <$> checkExp i
    checkDimIndex (DimSlice i j s) =
      DimSlice <$> traverse checkExp i <*> traverse checkExp j <*> traverse checkExp s

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
        . nameFromText
        . T.takeWhile isAscii
        . baseText
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

checkApply ::
  SrcLoc ->
  ApplyOp ->
  StructType ->
  Exp ->
  TermTypeM (StructType, StructType, Maybe VName, [VName])
checkApply loc (fname, _) (Scalar (Arrow _ pname _ tp1 tp2)) argexp = do
  let argtype = typeOf argexp
  onFailure (CheckingApply fname argexp tp1 argtype) $ do
    unify (mkUsage argexp "use as function argument") tp1 argtype

    -- Perform substitutions of instantiated variables in the types.
    (tp2_inst, ext) <- instantiateDimsInReturnType loc fname =<< normTypeFully tp2
    argtype' <- normTypeFully argtype

    -- Unification against the argument type may have determined that
    -- some instantiated sizes are existential. Their occurrences in
    -- the return type are replaced with fresh rigid sizes, bound at
    -- the innermost possible position; those bound at the top level
    -- become existentials of the application. The pending size
    -- variables themselves are left alone; occurrences of them
    -- remaining in the AST are existentially bound by
    -- 'bindExistentialInsts' at the end.
    constraints <- getConstraints
    let (inst_pending, inst_reps) = pendingInstSizes constraints
        repOf v = ExpSubst . flip sizeFromName (srclocOf loc) . qualName <$> M.lookup v inst_reps
        tp2_subst = applySubst repOf tp2_inst
    (tp2', inst_ext) <-
      -- 'sizeFree' can only change the type if a pending instantiated size
      -- occurs free in it, so check for a fast path.
      if any inst_pending (fvVars (freeInType tp2_subst))
        then sizeFree loc (find inst_pending . fvVars . freeInExp) tp2_subst
        else pure (tp2_subst, [])
    let ext' = ext <> inst_ext

    -- Check whether this would produce an impossible return type.
    let (tp2_produced_dims, tp2_paramdims) = dimUses tp2'
        problematic = S.fromList ext' <> boundInsideType argtype'
        problem = any (`S.member` problematic) (tp2_paramdims `S.difference` tp2_produced_dims)
    when (not (S.null problematic) && problem) $ do
      typeError loc mempty . withIndexLink "existential-param-ret" $
        "Existential size would appear in function parameter of return type:"
          </> indent 2 (pretty (RetType ext' tp2'))
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

    pure (tp1, tp2'', argext, ext')
checkApply _ _ _ _ =
  error "checkApply: array"

-- | Type-check a single expression in isolation.  This expression may
-- turn out to be polymorphic, in which case the list of type
-- parameters will be non-empty.
checkOneExp :: ExpBase NoInfo VName -> TypeM ([TypeParam], Exp)
checkOneExp e = do
  (maybe_tysubsts, e') <- Unsized.checkSingleExp e
  case maybe_tysubsts of
    Left err -> throwError err
    Right (generalised, tysubsts) -> runTermTypeM checkExp tysubsts $ do
      e'' <- checkExp e'
      let t = typeOf e''
      (tparams, _, _) <-
        letGeneralise (nameFromString "<exp>") (srclocOf e) generalised [] $ toRes Nonunique t
      detectAmbiguousSizes
      e''' <- bindExistentialInsts =<< normTypeFully e''
      localChecks tparams e'''
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
      onExp known (UpdateSection _ (Info t) loc)
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
          seqArgs known' ((Info p, x) : xs) = do
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
        e@(AppExp (BinOp (f, floc) ft (x, Info xp) (y, Info yp) _) (Info res)) = do
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

-- | Check that a type is non-functional, looking up the liftedness of type
-- variables.
orderZeroM :: [TypeParam] -> StructType -> TermTypeM Bool
orderZeroM tparams t = do
  (orderZero t &&) . and <$> mapM isUnlifted (typeQualVars t)
  where
    isUnlifted qv = do
      case find ((== qualLeaf qv) . typeParamName) tparams of
        Just (TypeParamType l _ _) -> pure $ l < Lifted
        _ -> (< Lifted) <$> lookupAbsTy qv

-- | Traverse the expression, emitting warnings and errors for various
-- problems:
--
-- * Unmatched cases.
--
-- * If any of the literals overflow their inferred types. Note:
--  currently unable to detect float underflow (such as 1e-400 -> 0)
--
-- * Function types appearing in places where they are not allowed (e.g.
--   returned from branches), and more generally lifted types used as
--   array elements.
--
-- The rationale is that it is easier to check for these things after all of the
-- type inference has been done, as they complicate the logic. Further, it is
-- also easier to produce good error messages here. The key is that we can only
-- enforce rules that do not affect type inference.
localChecks :: [TypeParam] -> Exp -> TermTypeM ()
localChecks tparams orig_body = void $ check orig_body
  where
    check e@(AppExp (Match _ cs loc) (Info rt)) = do
      ok <- orderZeroM tparams (appResType rt)
      unless ok . typeError loc mempty $
        "Match-expression returns type"
          </> indent 2 (align (pretty (appResType rt)))
          </> "but match-results may not be of function type."
      let ps = fmap (\(CasePat p _ _) -> p) cs
      case unmatched $ NE.toList ps of
        [] -> recurse e
        ps' ->
          typeError loc mempty . withIndexLink "unmatched-cases" $
            "Unmatched cases in match expression:"
              </> indent 2 (stack (map pretty ps'))
    check e@(AppExp (If _ _ _ loc) (Info rt)) = do
      ok <- orderZeroM tparams (appResType rt)
      unless ok . typeError loc mempty $
        "If-expression returns type"
          </> indent 2 (align (pretty (appResType rt)))
          </> "but if-results may not be of function type."
      recurse e
    check e@(ArrayLit _ (Info t) loc) = do
      mapM_ (checkArrayElem loc) $ peelArray 1 t
      recurse e
    check e@(AppExp (LetPat _ p _ _ _) _) =
      mustBeIrrefutable p *> recurse e
    check e@(AppExp (BinOp (v, loc) _ (x, _) _ _) _)
      | qualLeaf v == intrinsicVar "==" = do
          case typeOf x of
            Array {} -> do
              warn loc $
                textwrap
                  "Comparing arrays with \"==\" is deprecated and will stop working in a future revision of the language."
            _ -> pure ()
          checkEquality loc (typeOf x) *> recurse e
    check e@(Var v (Info t) loc)
      | qualLeaf v == intrinsicVar "==" = do
          checkEquality loc t *> recurse e
    check e@(Lambda ps _ _ _ _) =
      mapM_ (mustBeIrrefutable . fmap toStruct) ps *> recurse e
    check e@(AppExp (LetFun _ (tparams', ps, _, _, e1) e2 _) _) = do
      mapM_ (mustBeIrrefutable . fmap toStruct) ps
      localChecks (tparams' <> tparams) e1
      void $ check e2
      pure e
    check e@(AppExp (Loop _ p _ form _ _) _) = do
      mustBeIrrefutable (fmap toStruct p)
      case form of
        ForIn form_p _ -> mustBeIrrefutable form_p
        _ -> pure ()
      ok <- orderZeroM tparams (patternStructType p)
      unless ok . typeError (locOf p) mempty $
        "Loop parameter inferred to have type"
          </> indent 2 (align (pretty p))
          </> "but a loop parameter may not be of function type."
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

    -- Array elements must be unlifted: of non-varying size, and in
    -- particular not functions. This is a stricter requirement than
    -- 'orderZeroM', which permits size-lifted type parameters.
    checkArrayElem loc et = do
      unless (orderZero et) . typeError loc mempty $
        "Type" </> indent 2 (pretty et) </> "found to be functional."
      mapM_ checkElemVar $ typeQualVars et
      where
        checkElemVar qv = do
          l <- case find ((== qualLeaf qv) . typeParamName) tparams of
            Just (TypeParamType l _ tploc) ->
              pure $ Left (l, locOf tploc)
            _ -> Right <$> lookupAbsTy qv
          case l of
            Left (l', tploc)
              | l' /= Unlifted ->
                  typeError loc mempty $
                    "Type parameter"
                      <+> dquotes (pretty qv)
                      <+> "bound at"
                      <+> pretty (locStr tploc)
                      <+> "is lifted and cannot be an array element."
            Right l'
              | l' /= Unlifted ->
                  typeError loc mempty $
                    "Type"
                      <+> dquotes (pretty qv)
                      <+> "is lifted and cannot be an array element."
            _ -> pure ()

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

-- | Check that any recursive applications of a higher-order function are
-- invariant with respect to the higher-order arguments. This is done as a
-- syntactic check.
--
-- 'fname' and 'params' are the name and parameters of the function whose body
-- this is; if the function is not recursive, 'fname' does not occur and this is
-- a no-op regardless.
recursionCheck :: [TypeParam] -> VName -> [Pat ParamType] -> Exp -> TermTypeM ()
recursionCheck tparams fname params body = do
  higher_order <-
    mapM (fmap not . orderZeroM tparams . patternStructType) params
  when (or higher_order) $ void $ check higher_order body
  where
    check ho e@(AppExp (Apply f args _) _)
      | Var v _ _ <- f,
        qualLeaf v == fname = do
          checkRecApply ho (locOf e) $ map snd $ NE.toList args
          mapM_ (check ho . snd) args
          pure e
    check _ (Var v _ loc)
      | qualLeaf v == fname =
          typeError loc mempty $
            "Recursive reference to"
              <+> dquotes (prettyName fname)
              <+> "must be a fully saturated application, as it has a higher-order parameter."
    check ho e = recurse ho e
    recurse ho = astMap identityMapper {mapOnExp = check ho}

    checkRecApply ho loc args = do
      unless (length args == length params) . typeError loc mempty $
        "Recursive application of"
          <+> dquotes (prettyName fname)
          <+> "is not fully saturated: expected"
          <+> pretty (length params)
          <+> "arguments but got"
          <+> pretty (length args)
          <> "."
      forM_ (zip3 ho params args) $ \(is_ho, p, arg) ->
        when is_ho $
          case arg of
            Var v _ _ | qualLeaf v `elem` patNames p -> pure ()
            _ ->
              typeError (locOf arg) mempty $
                "Higher-order argument in recursive application of"
                  <+> dquotes (prettyName fname)
                  <+> "must be passed unchanged, i.e. be the corresponding parameter."

-- | Instantiated sizes that unification has determined to be existential
-- ("pending"), and a mapping from pending copies to a representative: copies
-- from the same occurrence of an instantiated type parameter that were absorbed
-- from the same source denote the same existential size. See Note [Size
-- Inference].
pendingInstSizes :: Constraints -> (VName -> Bool, M.Map VName VName)
pendingInstSizes constraints = (pending, reps)
  where
    key v = case snd <$> M.lookup v constraints of
      Just (ExistentialSize k _ _) -> Just k
      _ -> Nothing
    pending v = case snd <$> M.lookup v constraints of
      Just ExistentialSize {} -> True
      Just (CopySize c _ _) -> isJust $ key c
      _ -> False
    groups =
      M.fromListWith
        (<>)
        [ ((occ, k), [v])
        | (v, (_, CopySize c occ _)) <- M.toList constraints,
          Just (Just k) <- [key c]
        ]
    reps =
      M.fromList
        [ (v, rep)
        | vs <- M.elems groups,
          let rep = minimum vs,
          v <- vs,
          v /= rep
        ]

-- | Instantiated sizes (at or above the given level) that are still pending and
-- have not been determined to be existential behave like ordinary sizes from
-- here on: canonical sizes are plain size variables, and copies are equal to
-- their canonical variable. Existential ones are left alone; they are handled
-- by 'bindExistentialInsts'.
collapseInstSizes :: Level -> TermTypeM ()
collapseInstSizes min_lvl = do
  constraints <- getConstraints
  let nonExistential c = case snd <$> M.lookup c constraints of
        Just ExistentialSize {} -> False
        _ -> True
      collapse (lvl, CopySize c _ usage)
        | lvl >= min_lvl,
          nonExistential c =
            (lvl, Size (Just $ sizeFromName (qualName c) $ srclocOf usage) usage)
      collapse (lvl, InstSize _ usage)
        | lvl >= min_lvl = (lvl, Size Nothing usage)
      collapse x = x
  modifyConstraints $ M.map collapse

-- | Instantiated sizes that unification determined to be existential may remain
-- free in some types recorded in the AST - in particular the instantiated types
-- of higher-order functions, where the existential size occurs in the return
-- type of a function-typed parameter. Existentially bind such sizes at the
-- innermost possible position, mirroring what the type of the function argument
-- looks like.
bindExistentialInsts :: (ASTMappable e) => e -> TermTypeM e
bindExistentialInsts x = do
  -- 'pendingInstSizes' scans the entire constraint set, but its result
  -- is invariant across this traversal (any rigid sizes we introduce
  -- below are not instantiated sizes), so we compute it once instead of
  -- once per type in the AST.
  constraints <- getConstraints
  let (pending, reps) = pendingInstSizes constraints
      repOf v = ExpSubst . flip sizeFromName mempty . qualName <$> M.lookup v reps
      relevant v = pending v || v `M.member` reps

      onType ::
        (Substitutable (TypeBase Size u)) =>
        TypeBase Size u ->
        TermTypeM (TypeBase Size u, [VName])
      onType t
        -- Fast path: this type mentions no pending or copied
        -- instantiated size, so 'applySubst'/'sizeFree' would be
        -- no-ops. Most types take this path.
        | not (any relevant $ fvVars $ freeInType t) = pure (t, [])
        | otherwise =
            sizeFree mempty (find pending . fvVars . freeInExp) $ applySubst repOf t

      onStruct ::
        (Substitutable (TypeBase Size u)) =>
        TypeBase Size u ->
        TermTypeM (TypeBase Size u)
      onStruct t = do
        (t', ext) <- onType t
        -- Existential sizes at the top level of a type have nowhere to
        -- be bound. Those that absorbed a rigid unknown size stand for
        -- a size that is actually computed at the recorded location,
        -- so they become rigid unknown sizes there, subjecting them to
        -- the causality check. The rest (absorbed from declared
        -- existentials, e.g. by a hole) are left alone.
        if null ext
          then pure t'
          else do
            let computedAt v = case snd <$> M.lookup v constraints of
                  Just (ExistentialSize _ mloc _) -> mloc
                  Just (CopySize c _ _)
                    | Just (ExistentialSize _ mloc _) <- snd <$> M.lookup c constraints ->
                        mloc
                  _ -> Nothing
            repls <- fmap (M.fromList . catMaybes) . forM (S.toList $ fvVars $ freeInType t) $ \v ->
              case computedAt v of
                Just dloc -> do
                  v' <- newRigidDim dloc (RigidRet Nothing) "d"
                  pure $ Just (v, ExpSubst $ sizeFromName (qualName v') $ srclocOf dloc)
                Nothing -> pure Nothing
            pure $ applySubst (`M.lookup` repls) t

      tv =
        ASTMapper
          { mapOnExp = astMap tv,
            mapOnName = pure,
            mapOnStructType = onStruct,
            mapOnParamType = onStruct,
            mapOnResRetType = \(RetType dims t) -> do
              (t', ext) <- onType t
              pure $ RetType (dims <> ext) t'
          }
  -- Global fast path: with no instantiated-size constraints at all, 'relevant'
  -- is false everywhere, so the traversal would just rebuild an identical copy.
  -- Skip it entirely - this is the common case.
  if any (isInstSize . snd) constraints
    then astMap tv x
    else pure x
  where
    isInstSize ExistentialSize {} = True
    isInstSize CopySize {} = True
    isInstSize _ = False

detectAmbiguousSizes :: TermTypeM ()
detectAmbiguousSizes = do
  collapseInstSizes 0
  constraints <- getConstraints
  mapM_ (notice constraints) $ M.toList constraints
  where
    -- Sizes that arise from instantiating inferred types have
    -- uninformative provenance. If a size variable with better
    -- provenance (e.g. a source-level size binder, or an
    -- instantiated size parameter) has been unified with the
    -- ambiguous size, we report that variable instead. This affects
    -- only the error message; which sizes are ambiguous is already
    -- settled.
    uninformative (Usage Nothing _) = True
    uninformative (Usage (Just u) _) = u `elem` ["replaceTyVars", "instantiation"]

    chase constraints w = case snd <$> M.lookup w constraints of
      Just (Size (Just (Var w' _ _)) _) -> chase constraints (qualLeaf w')
      _ -> w

    blame constraints v usage
      | uninformative usage,
        (v', usage') : _ <-
          [ (w, w_usage)
          | (w, (_, Size (Just (Var w1 _ _)) w_usage)) <- M.toList constraints,
            not $ uninformative w_usage,
            chase constraints (qualLeaf w1) == v
          ] =
          (v', usage')
      | otherwise = (v, usage)

    notice constraints (v, (_, Size Nothing usage)) =
      case blame constraints v usage of
        (v', Usage Nothing loc) ->
          typeError loc mempty . withIndexLink "ambiguous-size" $
            "Ambiguous size" <+> dquotes (prettyName v') <> "."
        (v', Usage (Just u) loc) ->
          typeError loc mempty . withIndexLink "ambiguous-size" $
            "Ambiguous size" <+> dquotes (prettyName v') <+> "arising from" <+> pretty u <> "."
    notice _ _ = pure ()

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

    -- Bind the name in scope of its own body so it may recurse. Harmless even
    -- when the function is not actually recursive, as name resolution has
    -- hooked things up properly anyway. See Note [Checking recursive
    -- functions].
    let self_binding = case maybe_retdecl' of
          Just (_, ret, ext) -> BoundV tparams $ funType params' $ RetType ext ret
          Nothing -> RecursiveV
    body' <-
      localScope (\scope -> scope {scopeVtable = M.insert fname self_binding $ scopeVtable scope}) $
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
--
-- The parameters are assumed to already have their types normalised
-- ('updateTypes'), which both callers do immediately beforehand.
verifyFunctionParams :: Maybe VName -> [Pat ParamType] -> TermTypeM ()
verifyFunctionParams fname params =
  onFailure (CheckingParams (baseName <$> fname)) $
    verifyParams (foldMap patNames params) params
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

-- | Find all size variables in the given type that are covered by the
-- constraints, and produce size parameters that close over them.
--
-- The passed-in list of type parameters is always prepended to the
-- produced list of type parameters.
closeOverSizes ::
  Name ->
  SrcLoc ->
  [TypeParam] ->
  [StructType] ->
  ResType ->
  Constraints ->
  TermTypeM ([TypeParam], ResRetType)
closeOverSizes defname defloc tparams paramts ret substs = do
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
    closeOver (k, Size Nothing _) =
      pure $ Just $ Left $ TypeParamDim k mempty
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
    cur_lvl <- curLevel
    collapseInstSizes $ cur_lvl - length params

    -- Re-normalise the types so that any instantiated sizes
    -- collapsed above are expressed in terms of their canonical
    -- variables, which can then be closed over.
    params' <- mapM updateTypes params
    restype' <- normTypeFully restype

    now_substs <- getConstraints

    -- Candidates for let-generalisation are those size variables that
    --
    -- (1) were not known before we checked this function, and
    --
    -- (2) are not used in the (new) definition of any size variables
    -- known before we checked this function.

    -- Criteria (1) and (2) is implemented by looking at the binding
    -- level of the size variables.
    let candidate (lvl, _) = lvl >= (cur_lvl - length params)
        new_substs = M.filter candidate now_substs

    (tparams', RetType ret_dims restype'') <-
      closeOverSizes
        defname
        defloc
        tparams
        (map patternStructType params')
        restype'
        new_substs

    restype''' <- updateTypes restype''

    let used_sizes =
          freeInType restype''' <> foldMap (freeInType . patternType) params'
    case filter ((`S.notMember` fvVars used_sizes) . typeParamName) $
      filter isSizeParam tparams' of
      [] -> pure ()
      tp : _ -> unusedSize $ SizeBinder (typeParamName tp) (srclocOf tp)

    -- We keep those type variables that were not closed over by
    -- let-generalisation.
    modifyConstraints $ M.filterWithKey $ \k _ -> k `notElem` map typeParamName tparams'

    pure (tparams', params', RetType ret_dims restype''')

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
    doChecks (maybe_tysubsts, params', retdecl', body') =
      case maybe_tysubsts of
        Left err -> throwError err
        Right (generalised, tysubsts) ->
          runTermTypeM checkExp tysubsts $ do
            (tparams', params'', retdecl'', RetType dims rettype', body'') <-
              checkBinding (fname, retdecl', generalised <> tparams, params', body', loc)

            -- Since this is a top-level function, we also resolve overloaded
            -- types, using either defaults or complaining about ambiguities.
            detectAmbiguousSizes

            -- Then replace all inferred types in the body and parameters.
            body''' <- bindExistentialInsts =<< normTypeFully body''
            params''' <- mapM normTypeFully params''
            retdecl''' <- traverse updateTypes retdecl''
            rettype'' <- normTypeFully rettype'

            -- Check if the function body can actually be evaluated.
            causalityCheck body'''

            -- Check for various problems.
            mapM_ (mustBeIrrefutable . fmap toStruct) params''
            localChecks tparams' body'''
            recursionCheck tparams' fname params''' body'''

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

-- Note [Size Inference]
--
-- Type checking of terms is split into two passes. The unsized type checker
-- (Language.Futhark.TypeChecker.Terms.Unsized) infers types while ignoring
-- sizes entirely - its solution maps type variables to types whose dimensions
-- are all vacuous. The sized type checker (this module) receives that solution
-- (the 'termTyVars' field) and is responsible only for inferring sizes: the
-- concrete size of every dimension, and where existential quantifiers go. It
-- never re-infers anything besides sizes and where existential quantifiers go.
--
-- Whenever the size checker needs the type of something that the unsized
-- checker inferred, it instantiates the unsized type by replacing every
-- dimension with a fresh size variable ('instTyVars' when instantiating a type
-- scheme, 'replaceTyVars' elsewhere). Ordinary size unification then determines
-- what these variables stand for. This works out simply enough, except for
-- existential sizes.
--
-- ## Existential sizes
--
-- Consider
--
--   def (|>) '^a '^b (x: a) (f: a -> b) : b = f x
--
--   def main (xs: []i32) = xs |> filter (> 0)
--
-- where "b" is instantiated with the type of "filter (> 0)", which is [n]i32 ->
-- ?[m].[m]i32. At instantiation time we only know the unsized type []i32 for
-- "b" - existential sizes are invisible to the unsized pass, so we cannot know
-- that the size of "b" might be existential to the function. Worse, if the
-- instantiation does turn out to contain an existential size, then every
-- occurrence of "b" in the instantiated type scheme denotes a *distinct*
-- existential:
--
--   [n]i32 -> ([n]i32 -> ?[m].[m]i32) -> ?[m'].[m']i32
--
-- But if the instantiation turns out to have an ordinary size (say "xs |> map
-- (+1)"), all occurrences denote the *same* size, and we must not lose that
-- connection, or we would infer needlessly existential types.
--
-- We address this via three constraint forms (see 'Constraint'):
--
-- - InstSize: a canonical instantiated size, i.e. a dimension of the first
--   occurrence of an instantiated type parameter (or an instantiated size
--   parameter, which has the same nature).
--
-- - CopySize: a dimension of a later occurrence. These are given distinct names
--   precisely so that occurrences can become distinct existentials, but as long
--   as the size is not existential, a copy is equal to its canonical variable
--   (and unification treats it so, by redirecting links to the canonical
--   variable).
--
-- - ExistentialSize: When unification would otherwise fail by linking an
--   instantiated size to a size bound locally in the other type (an existential
--   or a parameter of a function type in the argument), it instead marks the
--   canonical variable with this ('unifySizes'). We say that the instantiated
--   size *absorbs* the locally bound size, and we call size variables that are
--   permitted to do so *absorbable* (see below). Absorption is refused for
--   unlifted type parameters, which cannot have existential sizes.
--
-- The pending existentials are then turned into proper sizes at the places that
-- can bind them:
--
-- - checkApply binds pending sizes in the return type of an application using
--   sizeFree: at the innermost RetType where possible, with the remainder
--   becoming existentials of the application itself (AppRes).
--
-- - bindExistentialInsts does the same for pending sizes that remain in types
--   recorded in the AST, in particular instantiated higher-order function
--   types, where the existential occurs in the return type of a function-typed
--   *parameter* and hence never passes through checkApply's return type.
--
-- - letGeneralise demotes instantiated sizes that are still pending and never
--   became existential to ordinary sizes (collapseInstSizes), at which point
--   they can be closed over as hidden size parameters. For local functions this
--   recreates the per-use size freshness that the old type checker obtained
--   from let-generalising type variables.
--
-- Two occurrences of an instantiated type parameter absorbed from the same
-- source must moreover denote the *same* existential, or we would infer
-- unwitnessed existentials where witnessed ones are possible. This is why
-- ExistentialSize records the size it was unified with and CopySize records
-- which occurrence it belongs to: pending sizes from the same occurrence with
-- the same source are given a single fresh name ('pendingInstSizes' in Terms).
--
-- ## Dependent function types
--
-- A related problem is instantiating a type parameter with a dependent function
-- type such as (n: i64) -> [n]i32 -> [n]i32. The unsized pass preserves
-- parameter names, but the connection between the sizes and the binder is
-- exactly what was erased. Since every instantiated size variable occurs
-- exactly once, and binders are cloned between occurrences of the instantiated
-- type, it is safe to link an instantiated size to a binder of the instantiated
-- type itself - such binders are registered when the type is instantiated
-- ('registerBinders'), and 'unifySizes' permits exactly those links. Linking to
-- any *other* locally bound size is what signifies an existential (see above),
-- or an error for sizes with no such privileges.
--
-- ## Absorption privileges
--
-- To make the term of art explicit: a fresh size variable is *absorbable* if
-- unification may determine that it stands for an existential size. When an
-- absorbable size meets a size that is bound locally within the type it is
-- unified with, the mismatch is not an error; instead the absorbable size
-- absorbs the locally bound size - it is marked as a pending existential
-- (ExistentialSize), and is eventually existentially bound by the machinery
-- above. A size variable that is not absorbable must be resolved to an ordinary
-- size that is in scope, and encountering a locally bound size is an error for
-- it. Mechanically, absorbable sizes are exactly those constrained by InstSize
-- (or CopySize referring to one).
--
-- Which fresh sizes are absorbable is a fine line:
--
-- - Sizes arising from instantiation - of type parameters and of size
--   parameters - are absorbable. They occur exactly once, and stand for
--   "whatever size the context provides", which may well be existential.
--
-- - The sizes of a hole ('replaceTyVarsAbsorbable') are absorbable, as a hole
--   adopts whatever type the context provides. This includes adopting declared
--   existentials: a flexible existential that unification resolves to an
--   absorbable size is absorbed by it (see the end of the arrow case of
--   'unifyWith').
--
-- - The sizes of the inferred return type of a lambda and of a constructor
--   expression are likewise absorbable: a lambda returns whatever type the
--   context requires (in particular a lambda body ending in "#None" may well
--   have an existential option type), and the payload sizes of a constructor
--   application that are not determined by its arguments - notably those of
--   the *other* constructors - are adopted from the context. Relatedly, the
--   inferred return type of a lambda only hides (existentially binds) sizes
--   that are rigid; a still-flexible size has not been determined yet, and
--   hiding it would sever its connection to whatever the enclosing context
--   determines it to be (see 'inferReturnSizes').
--
-- - Sizes in the types of lambda parameters and patterns ('replaceTyVars') are
--   not absorbable. They name the sizes of actual bound values, and must be
--   resolved to real sizes. For example, this is what rejects
--
--     def f : (k: i64) -> [k]i32 -> i64 = \_ xs -> length xs
--
--   where the size of "xs" would otherwise silently absorb "k", which is not in
--   scope in the function body (tests/shapes/paramsize1.fut). Contrast with
--   tests/issue1168.fut, where the same shape of program must be accepted
--   because the inner function is size-generalised, and the *instantiation* of
--   its hidden size parameter is what absorbs the bound size of the expected
--   type.
--
-- ## Absorption and causality
--
-- Absorbing an existential is not always innocent. Whether the size that was
-- absorbed is a *declared* existential (of an ascribed type) or a *rigid
-- unknown* size (one whose value is computed at a specific point in the
-- program, such as the result of applying a function with an existential
-- return type) makes a difference:
--
--   def ite b t f = if b then t () else f ()
--   def f : () -> option ([]i32) = \() -> #None                    -- fine
--   def g b = ite b (\() -> #None) (\() -> #Some (gen ()))         -- rejected
--
-- In g, the type of "#None" is forced (via the instantiation of the type
-- parameter of "ite") to have the size produced by "gen ()" inside the other
-- lambda - a size that is only computed elsewhere, so the constructed value
-- cannot know its payload size (tests/sumtypes/sumtype52.fut). ExistentialSize
-- therefore records the location when the absorbed size was rigid, and when
-- such a pending existential remains in a type with no position to bind it,
-- 'bindExistentialInsts' turns it into a rigid unknown size at the recorded
-- location. The causality check then rejects expressions that need it (a sum
-- constructor, say) before it is computed, with no knowledge of this
-- machinery. Two subtleties: the ext variables temporarily introduced in the
-- arrow case of 'unifyWith' shadow the registered constraints of the binders,
-- so the rigidity of a binder is determined from the constraints as they were
-- before ('rigidPre'); and a pending size that checkApply has already bound in
-- a remaining parameter type (of a partially applied function) is registered
-- as a rigid unknown size by 'sizeFree', which is what carries the obligation
-- across applications.

-- Note [Checking recursive functions]
--
-- A function may refer to itself in its own body. The difficulty is that the
-- function's type is not fully known until we have checked that body, so we
-- cannot simply look the name up like any other. We follow the textbook
-- Hindley-Milner treatment on monomoprhic recursion, where all occurrences of
-- the function within its own body share a single type. However, we add the
-- twist that *size parameters* are allowed to differ.
--
-- The handling is spread across name resolution and the two type-checking
-- passes:
--
-- 1. Name resolution brings the function name into scope of its own body, but
--    only for *syntactic* functions (those with parameters).
--
-- 2. The unsized pass binds the name to a fresh monomorphic type variable while
--    checking the body, then emits a single constraint equating that variable
--    with the actual function type. This is standard monomorphic recursion.
--
-- 3. The sized pass considers two cases, distinguished by whether the function
--    has a declared return type:
--
--    * With a declared return type, the function's size-precise type is known
--      before we check the body, so we bind the name to that type scheme
--      ('BoundV'). Recursive occurrences are then instantiated like calls to
--      any other function: sizes are refreshed per occurrence, but the size
--      *relationships* of the signature are kept - e.g. that '[n]i32 -> [n]i32'
--      returns an array the size of its argument. This permits size-polymorphic
--      recursion.
--
--    * Without a declared return type, we cannot know the return type in
--      advance, so we bind the name to 'RecursiveV' . 'lookupVar' then resolves
--      each occurrence with 'replaceTyVars' on the type the unsized pass
--      recorded, which creates *fresh, unrelated* sizes for every dimension,
--      which is OK whenever no size relationship in the return type matters.
--      This means we cannot *infer* size constraints for recursive functions.
--
-- Only top-level self-recursion is handled. Mutual recursion and local
-- (let-bound) recursion are not.
