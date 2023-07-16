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
import Data.List (find, genericLength, partition)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Util (mapAccumLM, topologicalSort)
import Futhark.Util.Pretty hiding (space)
import Language.Futhark
import Language.Futhark.Primitive (intByteSize)
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker.Consumption qualified as Consumption
import Language.Futhark.TypeChecker.Match
import Language.Futhark.TypeChecker.Monad hiding (BoundV)
import Language.Futhark.TypeChecker.Terms.DoLoop
import Language.Futhark.TypeChecker.Terms.Monad
import Language.Futhark.TypeChecker.Terms.Pat
import Language.Futhark.TypeChecker.Types
import Language.Futhark.TypeChecker.Unify
import Prelude hiding (mod)

hasBinding :: Exp -> Bool
hasBinding Lambda {} = True
hasBinding (AppExp LetPat {} _) = True
hasBinding (AppExp LetFun {} _) = True
hasBinding (AppExp DoLoop {} _) = True
hasBinding (AppExp LetWith {} _) = True
hasBinding (AppExp Match {} _) = True
hasBinding e = isNothing $ astMap m e
  where
    m =
      identityMapper {mapOnExp = \e' -> if hasBinding e' then Nothing else Just e'}

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
  UncheckedTypeExp ->
  UncheckedExp ->
  TermTypeM (TypeExp Info VName, Exp)
checkAscript loc te e = do
  (te', decl_t, _) <- checkTypeExpNonrigid te
  e' <- checkExp e
  e_t <- expTypeFully e'

  onFailure (CheckingAscription (toStruct decl_t) e_t) $
    unify (mkUsage loc "type ascription") (toStruct decl_t) e_t

  pure (te', e')

checkCoerce ::
  SrcLoc ->
  UncheckedTypeExp ->
  UncheckedExp ->
  TermTypeM (TypeExp Info VName, StructType, Exp)
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

sizeFree ::
  SrcLoc ->
  (Exp -> Maybe VName) ->
  TypeBase Size as ->
  TermTypeM (TypeBase Size as, [VName])
sizeFree tloc expKiller orig_t = do
  runReaderT (to_be_replaced orig_t $ onType orig_t) mempty `runStateT` mempty
  where
    same_exp e1 e2
      | Just es <- similarExps e1 e2 =
          all (uncurry same_exp) es
      | otherwise = False

    witnessed_exps t = execState (traverseDims onDim t) mempty
      where
        onDim _ PosImmediate e = modify (e :)
        onDim _ _ _ = pure ()
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
    depends a b = any (same_exp b) $ subExps a
    top_wit =
      topologicalSort depends . witnessed_exps

    lookReplacement e repl = snd <$> find (same_exp e . fst) repl
    expReplace mapping e
      | Just e' <- lookReplacement e mapping = e'
      | otherwise = runIdentity $ astMap mapper e
      where
        mapper = identityMapper {mapOnExp = pure . expReplace mapping}

    -- using ReaderT [(Exp, Exp)] (StateT [VName] TermTypeM) a
    replacing e = do
      e' <- asks (`expReplace` e)
      case expKiller e' of
        Nothing -> pure e'
        Just cause -> do
          vn <- lift $ lift $ newRigidDim tloc (RigidOutOfScope (srclocOf e) cause) "d"
          modify (vn :)
          pure $ sizeFromName (qualName vn) (srclocOf e)

    to_be_replaced t m' = do
      foldl f m' $ top_wit t
      where
        f m e = do
          e' <- replacing e
          local ((e, e') :) m

    onScalar (Record fs) =
      Record <$> traverse onType fs
    onScalar (Sum cs) =
      Sum <$> (traverse . traverse) onType cs
    onScalar (Arrow as argName d argT (RetType dims retT)) = do
      argT' <- onType argT
      old_bound <- get
      retT' <- to_be_replaced retT $ onType retT
      rl <- state $ partition (`notElem` old_bound)
      let dims' = dims <> rl
      pure $ Arrow as argName d argT' (RetType dims' retT')
    onScalar (TypeVar u v args) =
      TypeVar u v <$> mapM onTypeArg args
      where
        onTypeArg (TypeArgDim d) = TypeArgDim <$> replacing d
        onTypeArg (TypeArgType ty) = TypeArgType <$> onType ty
    onScalar (Prim pt) = pure $ Prim pt

    onType ::
      TypeBase Size u ->
      ReaderT [(Exp, Exp)] (StateT [VName] TermTypeM) (TypeBase Size u)
    onType (Array u shape scalar) =
      Array u <$> traverse replacing shape <*> onScalar scalar
    onType (Scalar ty) =
      Scalar <$> onScalar ty

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
  S.Set VName ->
  TypeBase Size as ->
  TermTypeM (TypeBase Size as, [VName])
unscopeType tloc unscoped =
  sizeFree tloc $ S.lookupMin . S.intersection unscoped . fvVars . freeInExp

reboundI64 ::
  ASTMappable (TypeBase Size as) =>
  SrcLoc ->
  S.Set VName ->
  TypeBase Size as ->
  TermTypeM (TypeBase Size as, [VName])
reboundI64 tloc unscoped =
  fmap (fmap M.elems) . (`runStateT` mempty) . astMap mapper
  where
    mapper =
      ASTMapper
        { mapOnExp,
          mapOnName = pure,
          mapOnStructType = astMap mapper,
          mapOnParamType = astMap mapper,
          mapOnResRetType = astMap mapper
        }

    mapOnExp :: Exp -> StateT (M.Map VName VName) TermTypeM Exp
    mapOnExp (Var (QualName _ vn) _ loc)
      | vn `S.member` unscoped = do
          prev <- gets $ M.lookup vn
          case prev of
            Just vn' -> pure $ sizeFromName (qualName vn') loc
            Nothing -> do
              vn' <- lift $ newRigidDim tloc (RigidOutOfScope loc vn) "d"
              modify $ M.insert vn vn'
              pure $ sizeFromName (qualName vn') loc
    mapOnExp e = astMap mapper e

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
  pure $ IntLit val (Info t) loc
checkExp (FloatLit val NoInfo loc) = do
  t <- newTypeVar loc "t"
  mustBeOneOf anyFloatType (mkUsage loc "float literal") t
  pure $ FloatLit val (Info t) loc
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
      t <- arrayOfM loc et (Shape [sizeFromInteger 0 mempty])
      pure $ ArrayLit [] (Info t) loc
    e : es -> do
      e' <- checkExp e
      et <- expType e'
      es' <- mapM (unifies "type of first array element" et <=< checkExp) es
      et' <- normTypeFully et
      t <- arrayOfM loc et' (Shape [sizeFromInteger (genericLength all_es) mempty])
      pure $ ArrayLit (e' : es') (Info t) loc
checkExp (AppExp (Range start maybe_step end loc) _) = do
  start' <- require "use in range expression" anySignedType =<< checkExp start
  start_t <- expTypeFully start'
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
checkExp (Coerce e te NoInfo loc) = do
  (te', te_t, e') <- checkCoerce loc te e
  t <- expTypeFully e'
  t' <- matchDims (const . const pure) t te_t
  pure $ Coerce e' te' (Info t') loc
checkExp (AppExp (BinOp (op, oploc) NoInfo (e1, _) (e2, _) loc) NoInfo) = do
  (op', ftype) <- lookupVar oploc op
  e1' <- checkExp e1
  e2' <- checkExp e2

  -- Note that the application to the first operand cannot fix any
  -- existential sizes, because it must by necessity be a function.
  (_, _, rt, p1_ext, _) <- checkApply loc (Just op', 0) ftype e1'
  (_, _, rt', p2_ext, retext) <- checkApply loc (Just op', 1) rt e2'

  pure $
    AppExp
      ( BinOp
          (op', oploc)
          (Info ftype)
          (e1', Info p1_ext)
          (e2', Info p2_ext)
          loc
      )
      (Info (AppRes rt' retext))
checkExp (Project k e NoInfo loc) = do
  e' <- checkExp e
  t <- expType e'
  kt <- mustHaveField (mkUsage loc $ docText $ "projection of field " <> dquotes (pretty k)) k t
  pure $ Project k e' (Info kt) loc
checkExp (AppExp (If e1 e2 e3 loc) _) = do
  e1' <- checkExp e1
  e2' <- checkExp e2
  e3' <- checkExp e3

  let bool = Scalar $ Prim Bool
  e1_t <- expType e1'
  onFailure (CheckingRequired [bool] e1_t) $
    unify (mkUsage e1' "use as 'if' condition") bool e1_t

  (brancht, retext) <- unifyBranches loc e2' e3'

  zeroOrderType
    (mkUsage loc "returning value of this type from 'if' expression")
    "type returned from branch"
    brancht

  pure $ AppExp (If e1' e2' e3' loc) (Info $ AppRes brancht retext)
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
-- Handle common case specially for efficiency.
checkExp (Var qn@(QualName [] _) NoInfo loc) = do
  (qn', t) <- lookupVar loc qn
  pure $ Var qn' (Info t) loc
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
      (d1, _, rt, argext, exts) <- checkApply loc (fname, i) t arg'
      pure
        ( (i + 1, all_exts <> exts, rt),
          (Info (d1, argext), arg')
        )
checkExp (AppExp (LetPat sizes pat e body loc) _) = do
  e' <- checkExp e

  -- Not technically an ascription, but we want the pattern to have
  -- exactly the type of 'e'.
  t <- expType e'
  incLevel . bindingSizes sizes $ \sizes' ->
    bindingPat sizes' pat (Ascribed t) $ \pat' -> do
      body' <- checkExp body
      let (i64, noni64) = partition i64Ident $ patIdents pat'
      (body_t, retext) <-
        reboundI64 loc (S.fromList (map sizeName sizes' <> map identName i64))
          =<< expTypeFully body'
      (body_t', retext') <- unscopeType loc (S.fromList (map identName noni64)) body_t

      pure $
        AppExp
          (LetPat sizes' (fmap toStruct pat') e' body' loc)
          (Info $ AppRes body_t' (retext <> retext'))
  where
    i64Ident (Ident _ ty _) =
      ty == Info (Scalar $ Prim $ Signed Int64)
checkExp (AppExp (LetFun name (tparams, params, maybe_retdecl, NoInfo, e) body loc) _) = do
  (tparams', params', maybe_retdecl', rettype, e') <-
    checkBinding (name, maybe_retdecl, tparams, params, e, loc)

  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc

    let entry = BoundV tparams' $ funType params' rettype
        bindF scope =
          scope
            { scopeVtable =
                M.insert name' entry $ scopeVtable scope,
              scopeNameMap =
                M.insert (Term, name) (qualName name') $
                  scopeNameMap scope
            }
    body' <- localScope bindF $ checkExp body

    (body_t, ext) <-
      unscopeType loc (S.singleton name')
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
checkExp (AppExp (LetWith dest src slice ve body loc) _) = do
  src' <- checkIdent src
  slice' <- checkSlice slice
  (t, _) <- newArrayType (mkUsage src "type of source array") "src" $ sliceDims slice'
  unify (mkUsage loc "type of target array") t $ unInfo $ identType src'

  (elemt, _) <- sliceShape (Just (loc, Nonrigid)) slice' =<< normTypeFully t

  ve' <- unifies "type of target array" elemt =<< checkExp ve

  bindingIdent dest (unInfo (identType src')) $ \dest' -> do
    body' <- checkExp body
    (body_t, ext) <-
      unscopeType loc (S.singleton (identName dest'))
        =<< expTypeFully body'
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
checkExp (Assert e1 e2 NoInfo loc) = do
  e1' <- require "being asserted" [Bool] =<< checkExp e1
  e2' <- checkExp e2
  pure $ Assert e1' e2' (Info (prettyText e1)) loc
checkExp (Lambda params body rettype_te NoInfo loc) = do
  (params', body', rettype', RetType dims ty) <-
    incLevel . bindingParams [] params $ \_ params' -> do
      rettype_checked <- traverse checkTypeExpNonrigid rettype_te
      let declared_rettype =
            case rettype_checked of
              Just (_, st, _) -> Just st
              Nothing -> Nothing
      body' <- checkFunBody params' body declared_rettype loc
      body_t <- expTypeFully body'

      params'' <- mapM updateTypes params'

      (rettype', rettype_st) <-
        case rettype_checked of
          Just (te, st, ext) ->
            pure (Just te, RetType ext st)
          Nothing -> do
            ret <- inferReturnSizes params'' $ toRes Nonunique body_t
            pure (Nothing, ret)

      pure (params'', body', rettype', rettype_st)

  verifyFunctionParams Nothing params'

  (ty', dims') <- unscopeType loc (S.fromList dims) ty

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
checkExp (OpSection op _ loc) = do
  (op', ftype) <- lookupVar loc op
  pure $ OpSection op' (Info ftype) loc
checkExp (OpSectionLeft op _ e _ _ loc) = do
  (op', ftype) <- lookupVar loc op
  e' <- checkExp e
  (_, t1, rt, argext, retext) <- checkApply loc (Just op', 0) ftype e'
  case (ftype, rt) of
    (Scalar (Arrow _ m1 d1 _ _), Scalar (Arrow _ m2 d2 t2 rettype)) ->
      pure $
        OpSectionLeft
          op'
          (Info ftype)
          e'
          (Info (m1, toParam d1 t1, argext), Info (m2, toParam d2 t2))
          (Info rettype, Info retext)
          loc
    _ ->
      typeError loc mempty $
        "Operator section with invalid operator of type" <+> pretty ftype
checkExp (OpSectionRight op _ e _ NoInfo loc) = do
  (op', ftype) <- lookupVar loc op
  e' <- checkExp e
  case ftype of
    Scalar (Arrow _ m1 d1 t1 (RetType [] (Scalar (Arrow _ m2 d2 t2 (RetType dims2 ret))))) -> do
      (_, t2', arrow', argext, _) <-
        checkApply
          loc
          (Just op', 1)
          (Scalar $ Arrow mempty m2 d2 t2 $ RetType [] $ Scalar $ Arrow Nonunique m1 d1 t1 $ RetType dims2 ret)
          e'
      case arrow' of
        Scalar (Arrow _ _ _ t1' (RetType dims2' ret')) ->
          pure $
            OpSectionRight
              op'
              (Info ftype)
              e'
              (Info (m1, toParam d1 t1'), Info (m2, toParam d2 t2', argext))
              (Info $ RetType dims2' ret')
              loc
        _ -> error $ "OpSectionRight: impossible type\n" <> prettyString arrow'
    _ ->
      typeError loc mempty $
        "Operator section with invalid operator of type" <+> pretty ftype
checkExp (ProjectSection fields NoInfo loc) = do
  a <- newTypeVar loc "a"
  let usage = mkUsage loc "projection at"
  b <- foldM (flip $ mustHaveField usage) a fields
  let ft = Scalar $ Arrow mempty Unnamed Observe a $ RetType [] $ toRes Nonunique b
  pure $ ProjectSection fields (Info ft) loc
checkExp (IndexSection slice NoInfo loc) = do
  slice' <- checkSlice slice
  (t, _) <- newArrayType (mkUsage' loc) "e" $ sliceDims slice'
  (t', retext) <- sliceShape Nothing slice' t
  let ft = Scalar $ Arrow mempty Unnamed Observe t $ RetType retext $ toRes Nonunique t'
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
  mustHaveConstr (mkUsage loc "use of constructor") name t ets
  pure $ Constr name es' (Info t) loc
checkExp (AppExp (Match e cs loc) _) = do
  e' <- checkExp e
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
  StructType ->
  NE.NonEmpty (CaseBase NoInfo Name) ->
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

checkCase ::
  StructType ->
  CaseBase NoInfo Name ->
  TermTypeM (CaseBase Info VName, StructType, [VName])
checkCase mt (CasePat p e loc) =
  bindingPat [] p (Ascribed mt) $ \p' -> do
    e' <- checkExp e
    let (i64, noni64) = partition i64Ident $ patIdents p'
    (t, retext) <-
      reboundI64 loc (S.fromList (map identName i64)) =<< expTypeFully e'
    (t', retext') <- unscopeType loc (S.fromList (map identName noni64)) t
    pure (CasePat (fmap toStruct p') e' loc, t', retext <> retext')
  where
    i64Ident (Ident _ ty _) =
      ty == Info (Scalar $ Prim $ Signed Int64)

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
          ppField (name, t) = pretty (nameToString name) <> equals <> pretty' t
      pretty' Wildcard {} = "_"
      pretty' (PatLit e _ _) = pretty e
      pretty' (PatConstr n _ ps _) = "#" <> pretty n <+> sep (map pretty' ps)

checkIdent :: IdentBase NoInfo Name StructType -> TermTypeM (Ident StructType)
checkIdent (Ident name _ loc) = do
  (QualName _ name', vt) <- lookupVar loc (qualName name)
  pure $ Ident name' (Info vt) loc

checkSlice :: UncheckedSlice -> TermTypeM [DimIndex]
checkSlice = mapM checkDimIndex
  where
    checkDimIndex (DimFix i) = do
      DimFix <$> (require "use as index" anySignedType =<< checkExp i)
    checkDimIndex (DimSlice i j s) =
      DimSlice <$> check i <*> check j <*> check s

    check =
      maybe (pure Nothing) $
        fmap Just . unifies "use as index" (Scalar $ Prim $ Signed Int64) <=< checkExp

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
    f bound _ (Var v _ _) | qualLeaf v `S.member` bound = pure ()
    f _ PosImmediate (Var v _ _) = modify ((S.singleton (qualLeaf v), mempty) <>)
    f _ PosParam (Var v _ _) = modify ((mempty, S.singleton (qualLeaf v)) <>)
    f _ _ _ = pure ()

checkApply ::
  SrcLoc ->
  ApplyOp ->
  StructType ->
  Exp ->
  TermTypeM (Diet, StructType, StructType, Maybe VName, [VName])
checkApply loc (fname, _) (Scalar (Arrow _ pname d1 tp1 tp2)) argexp = do
  let argtype = typeOf argexp
  onFailure (CheckingApply fname argexp tp1 argtype) $ do
    unify (mkUsage argexp "use as function argument") tp1 argtype

    -- Perform substitutions of instantiated variables in the types.
    (tp2', ext) <- instantiateDimsInReturnType loc fname =<< normTypeFully tp2
    argtype' <- normTypeFully argtype

    -- Check whether this would produce an impossible return type.
    let (tp2_produced_dims, tp2_paramdims) = dimUses $ toStruct tp2'
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

    pure (d1, tp1, tp2'', argext, ext)
checkApply loc fname tfun@(Scalar TypeVar {}) arg = do
  tv <- newTypeVar loc "b"
  unify (mkUsage loc "use as function") tfun $
    Scalar (Arrow mempty Unnamed Observe (typeOf arg) $ RetType [] $ paramToRes tv)
  tfun' <- normType tfun
  checkApply loc fname tfun' arg
checkApply loc (fname, prev_applied) ftype argexp = do
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

-- | Type-check a single expression in isolation.  This expression may
-- turn out to be polymorphic, in which case the list of type
-- parameters will be non-empty.
checkOneExp :: UncheckedExp -> TypeM ([TypeParam], Exp)
checkOneExp e = runTermTypeM checkExp $ do
  e' <- checkExp e
  let t = typeOf e'
  (tparams, _, _) <-
    letGeneralise (nameFromString "<exp>") (srclocOf e) [] [] $ toRes Nonunique t
  fixOverloadedTypes $ typeVars t
  e'' <- updateTypes e'
  localChecks e''
  causalityCheck e''
  pure (tparams, e'')

-- | Type-check a single size expression in isolation.  This expression may
-- turn out to be polymorphic, in which case it is unified with i64.
checkSizeExp :: UncheckedExp -> TypeM Exp
checkSizeExp e = runTermTypeM checkExp $ do
  e' <- checkExp e
  let t = typeOf e'
  when (hasBinding e') $
    typeError (srclocOf e') mempty . withIndexLink "size-expression-bind" $
      "Size expression with binding is forbidden."
  unify (mkUsage e' "Size expression") t (Scalar (Prim (Signed Int64)))
  updateTypes e'

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
          seqArgs known' ((Info (_, p), x) : xs) = do
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
          <+> what <> colon
          </> indent 2 (pretty t)
          </> "But"
          <+> dquotes (prettyName d)
          <+> "is computed at"
          <+> pretty (locStrRel loc dloc) <> "."
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
    check e@(AppExp (BinOp (QualName [] v, _) _ (x, _) _ loc) _)
      | baseName v == "==",
        Array {} <- typeOf x,
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
    [UncheckedPat ParamType],
    UncheckedExp,
    SrcLoc
  ) ->
  TypeM
    ( VName,
      [TypeParam],
      [Pat ParamType],
      Maybe (TypeExp Info VName),
      ResRetType,
      Exp
    )
checkFunDef (fname, maybe_retdecl, tparams, params, body, loc) =
  runTermTypeM checkExp $ do
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

      let ((body''', updated_ret), errors) =
            Consumption.checkValDef
              ( fname',
                params'',
                body'',
                RetType dims rettype'',
                maybe_retdecl'',
                loc
              )

      mapM_ throwError errors

      pure (fname', tparams', params'', maybe_retdecl'', updated_ret, body''')

-- | This is "fixing" as in "setting them", not "correcting them".  We
-- only make very conservative fixing.
fixOverloadedTypes :: Names -> TermTypeM ()
fixOverloadedTypes tyvars_at_toplevel =
  getConstraints >>= mapM_ fixOverloaded . M.toList . M.map snd
  where
    fixOverloaded (v, Overloaded ots usage)
      | Signed Int32 `elem` ots = do
          unify usage (Scalar (TypeVar mempty (qualName v) [])) $
            Scalar (Prim $ Signed Int32)
          when (v `S.member` tyvars_at_toplevel) $
            warn usage "Defaulting ambiguous type to i32."
      | FloatType Float64 `elem` ots = do
          unify usage (Scalar (TypeVar mempty (qualName v) [])) $
            Scalar (Prim $ FloatType Float64)
          when (v `S.member` tyvars_at_toplevel) $
            warn usage "Defaulting ambiguous type to f64."
      | otherwise =
          typeError usage mempty . withIndexLink "ambiguous-type" $
            "Type is ambiguous (could be one of"
              <+> commasep (map pretty ots) <> ")."
              </> "Add a type annotation to disambiguate the type."
    fixOverloaded (v, NoConstraint _ usage) = do
      -- See #1552.
      unify usage (Scalar (TypeVar mempty (qualName v) [])) $
        Scalar (tupleRecord [])
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

hiddenParamNames :: [Pat ParamType] -> Names
hiddenParamNames params = hidden
  where
    param_all_names = mconcat $ map patNames params
    named (Named x, _, _) = Just x
    named (Unnamed, _, _) = Nothing
    param_names =
      S.fromList $ mapMaybe (named . patternParam) params
    hidden = param_all_names `S.difference` param_names

inferredReturnType :: SrcLoc -> [Pat ParamType] -> StructType -> TermTypeM StructType
inferredReturnType loc params t = do
  -- The inferred type may refer to names that are bound by the
  -- parameter patterns, but which will not be visible in the type.
  -- These we must turn into fresh type variables, which will be
  -- existential in the return type.
  fst <$> unscopeType loc hidden_params t
  where
    hidden_params = S.filter (`S.member` hidden) $ foldMap patNames params
    hidden = hiddenParamNames params

checkBinding ::
  ( Name,
    Maybe UncheckedTypeExp,
    [UncheckedTypeParam],
    [UncheckedPat ParamType],
    UncheckedExp,
    SrcLoc
  ) ->
  TermTypeM
    ( [TypeParam],
      [Pat ParamType],
      Maybe (TypeExp Info VName),
      ResRetType,
      Exp
    )
checkBinding (fname, maybe_retdecl, tparams, params, body, loc) =
  incLevel . bindingParams tparams params $ \tparams' params' -> do
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

    (tparams'', params''', rettype') <-
      letGeneralise fname loc tparams' params''
        =<< unscopeUnknown rettype

    pure (tparams'', params''', maybe_retdecl'', rettype', body')

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
verifyFunctionParams :: Maybe Name -> [Pat ParamType] -> TermTypeM ()
verifyFunctionParams fname params =
  onFailure (CheckingParams fname) $
    verifyParams (foldMap patNames params) =<< mapM updateTypes params
  where
    verifyParams forbidden (p : ps)
      | d : _ <- S.toList $ fvVars (freeInPat p) `S.intersection` forbidden =
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
      Scalar $ Arrow als p d1 t1 $ injectExt (ext_there <> t2_ext) t2
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
    ( tparams ++ more_tparams,
      injectExt (retext ++ mapMaybe mkExt (S.toList $ fvVars $ freeInType ret)) ret
    )
  where
    -- Diet does not matter here.
    t = foldFunType (map (toParam Observe) paramts) $ RetType [] ret
    to_close_over = M.filterWithKey (\k _ -> k `S.member` visible) substs
    visible = typeVars t <> fvVars (freeInType t)

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
          freeInType rettype'' <> foldMap (freeInType . patternType) params
    case filter ((`S.notMember` fvVars used_sizes) . typeParamName) $
      filter isSizeParam tparams' of
      [] -> pure ()
      tp : _ -> unusedSize $ SizeBinder (typeParamName tp) (srclocOf tp)

    -- We keep those type variables that were not closed over by
    -- let-generalisation.
    modifyConstraints $ M.filterWithKey $ \k _ -> k `notElem` map typeParamName tparams'

    pure (tparams', params, RetType ret_dims rettype'')

checkFunBody ::
  [Pat ParamType] ->
  UncheckedExp ->
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
          (S.filter (`S.member` hidden) $ foldMap patNames params)
          body_t

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
