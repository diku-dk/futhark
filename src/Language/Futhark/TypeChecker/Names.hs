-- | Resolve names.
--
-- This also performs a small amount of rewriting; specifically
-- turning 'Var's with qualified names into 'Project's, based on
-- whether they are referencing a module or not.
--
-- Also checks for other name-related problems, such as duplicate
-- names.
module Language.Futhark.TypeChecker.Names
  ( resolveValBind,
    resolveTypeParams,
    resolveTypeExp,
    resolveExp,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.Semantic (includeToFilePath)
import Language.Futhark.TypeChecker.Monad
import Prelude hiding (mod)

-- | Names that may not be shadowed.
doNotShadow :: [Name]
doNotShadow = ["&&", "||"]

checkDoNotShadow :: (Located a) => a -> Name -> TypeM ()
checkDoNotShadow loc v =
  when (v `elem` doNotShadow) $
    typeError loc mempty . withIndexLink "may-not-be-redefined" $
      "The" <+> prettyName v <+> "operator may not be redefined."

-- | Check whether the type contains arrow types that define the same
-- parameter.  These might also exist further down, but that's not
-- really a problem - we mostly do this checking to help the user,
-- since it is likely an error, but it's easy to assign a semantics to
-- it (normal name shadowing).
checkForDuplicateNamesInType :: TypeExp (ExpBase NoInfo Name) Name -> TypeM ()
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

resolveQualName :: QualName Name -> SrcLoc -> TypeM (QualName VName)
resolveQualName v loc = do
  v' <- checkValName v loc
  case v' of
    QualName (q : _) _
      | baseTag q <= maxIntrinsicTag -> do
          me <- askImportName
          unless (isBuiltin (includeToFilePath me)) $
            warn loc "Using intrinsic functions directly can easily crash the compiler or result in wrong code generation."
    _ -> pure ()
  pure v'

resolveName :: Name -> SrcLoc -> TypeM VName
resolveName v loc = qualLeaf <$> resolveQualName (qualName v) loc

resolveAttrAtom :: AttrAtom Name -> TypeM (AttrAtom VName)
resolveAttrAtom (AtomName v) = pure $ AtomName v
resolveAttrAtom (AtomInt x) = pure $ AtomInt x

resolveAttrInfo :: AttrInfo Name -> TypeM (AttrInfo VName)
resolveAttrInfo (AttrAtom atom loc) =
  AttrAtom <$> resolveAttrAtom atom <*> pure loc
resolveAttrInfo (AttrComp name infos loc) =
  AttrComp name <$> mapM resolveAttrInfo infos <*> pure loc

resolveSizeExp :: SizeExp (ExpBase NoInfo Name) -> TypeM (SizeExp (ExpBase NoInfo VName))
resolveSizeExp (SizeExpAny loc) = pure $ SizeExpAny loc
resolveSizeExp (SizeExp e loc) = SizeExp <$> resolveExp e <*> pure loc

-- | Resolve names in a single type expression.
resolveTypeExp ::
  TypeExp (ExpBase NoInfo Name) Name ->
  TypeM (TypeExp (ExpBase NoInfo VName) VName)
resolveTypeExp orig = checkForDuplicateNamesInType orig >> f orig
  where
    f (TEVar v loc) =
      TEVar <$> checkQualName Type v loc <*> pure loc
    f (TEParens te loc) =
      TEParens <$> f te <*> pure loc
    f (TETuple tes loc) =
      TETuple <$> mapM f tes <*> pure loc
    f (TERecord fs loc) =
      TERecord <$> mapM (traverse f) fs <*> pure loc
    f (TEUnique te loc) =
      TEUnique <$> f te <*> pure loc
    f (TEApply te1 args loc) =
      TEApply <$> f te1 <*> onArg args <*> pure loc
      where
        onArg (TypeArgExpSize size) = TypeArgExpSize <$> resolveSizeExp size
        onArg (TypeArgExpType te) = TypeArgExpType <$> f te
    f (TEArrow Nothing te1 te2 loc) =
      TEArrow Nothing <$> f te1 <*> f te2 <*> pure loc
    f (TEArrow (Just v) te1 te2 loc) =
      bindSpaced1 Term v loc $ \v' -> do
        usedName v'
        TEArrow (Just v') <$> f te1 <*> f te2 <*> pure loc
    f (TESum cs loc) =
      TESum <$> mapM (traverse $ mapM f) cs <*> pure loc
    f (TEDim vs te loc) =
      bindSpaced (map (Term,,loc) vs) $ \vs' ->
        TEDim vs' <$> f te <*> pure loc
    f (TEArray size te loc) =
      TEArray <$> resolveSizeExp size <*> f te <*> pure loc

-- | Resolve names in a single expression.
resolveExp :: ExpBase NoInfo Name -> TypeM (ExpBase NoInfo VName)
--
-- First all the trivial cases.
resolveExp (Literal x loc) = pure $ Literal x loc
resolveExp (IntLit x NoInfo loc) = pure $ IntLit x NoInfo loc
resolveExp (FloatLit x NoInfo loc) = pure $ FloatLit x NoInfo loc
resolveExp (StringLit x loc) = pure $ StringLit x loc
resolveExp (Hole NoInfo loc) = pure $ Hole NoInfo loc
--
-- The main interesting cases (except for the ones in AppExp).
resolveExp (Var qn NoInfo loc) = do
  -- The qualifiers of a variable is divided into two parts: first a
  -- possibly-empty sequence of module qualifiers, followed by a
  -- possible-empty sequence of record field accesses.  We use scope
  -- information to perform the split, by taking qualifiers off the
  -- end until we find something that is not a module.
  (qn', fields) <- findRootVar (qualQuals qn) (qualLeaf qn)
  when ("_" `T.isPrefixOf` nameToText (qualLeaf qn)) $
    underscoreUse loc qn
  pure $ L.foldl' project (Var qn' NoInfo loc) fields
  where
    findRootVar qs name =
      (whenFound <$> resolveQualName (QualName qs name) loc)
        `catchError` notFound qs name

    whenFound qn' = (qn', [])

    notFound qs name err
      | null qs = throwError err
      | otherwise = do
          (qn', fields) <-
            findRootVar (init qs) (last qs) `catchError` const (throwError err)
          pure (qn', fields ++ [name])

    project e k = Project k e NoInfo loc
--
resolveExp (Lambda params body ret NoInfo loc) = do
  checkForDuplicateNames [] params
  resolveParams params $ \params' -> do
    body' <- resolveExp body
    ret' <- traverse resolveTypeExp ret
    pure $ Lambda params' body' ret' NoInfo loc
--
resolveExp (QualParens (modname, modnameloc) e loc) = do
  (modname', mod) <- lookupMod loc modname
  case mod of
    ModEnv env -> localEnv (qualifyEnv modname' env) $ do
      e' <- resolveExp e
      pure $ QualParens (modname', modnameloc) e' loc
    ModFun {} ->
      typeError loc mempty . withIndexLink "module-is-parametric" $
        "Module" <+> pretty modname <+> " is a parametric module."
  where
    qualifyEnv modname' env =
      env {envNameMap = qualify' modname' <$> envNameMap env}
    qualify' modname' (QualName qs name) =
      QualName (qualQuals modname' ++ [qualLeaf modname'] ++ qs) name

--
-- The tedious recursive cases.
resolveExp (Parens e loc) =
  Parens <$> resolveExp e <*> pure loc
resolveExp (Attr attr e loc) =
  Attr <$> resolveAttrInfo attr <*> resolveExp e <*> pure loc
resolveExp (TupLit es loc) =
  TupLit <$> mapM resolveExp es <*> pure loc
resolveExp (ArrayVal vs t loc) =
  pure $ ArrayVal vs t loc
resolveExp (ArrayLit es NoInfo loc) =
  ArrayLit <$> mapM resolveExp es <*> pure NoInfo <*> pure loc
resolveExp (Negate e loc) =
  Negate <$> resolveExp e <*> pure loc
resolveExp (Not e loc) =
  Not <$> resolveExp e <*> pure loc
resolveExp (Assert e1 e2 NoInfo loc) =
  Assert <$> resolveExp e1 <*> resolveExp e2 <*> pure NoInfo <*> pure loc
resolveExp (RecordLit fs loc) =
  RecordLit <$> mapM resolveField fs <*> pure loc
  where
    resolveField (RecordFieldExplicit k e floc) =
      RecordFieldExplicit k <$> resolveExp e <*> pure floc
    resolveField (RecordFieldImplicit vn NoInfo floc) =
      RecordFieldImplicit <$> resolveName vn floc <*> pure NoInfo <*> pure floc
resolveExp (Project k e NoInfo loc) =
  Project k <$> resolveExp e <*> pure NoInfo <*> pure loc
resolveExp (Constr k es NoInfo loc) =
  Constr k <$> mapM resolveExp es <*> pure NoInfo <*> pure loc
resolveExp (Update e1 slice e2 loc) =
  Update <$> resolveExp e1 <*> resolveSlice slice <*> resolveExp e2 <*> pure loc
resolveExp (RecordUpdate e1 fs e2 NoInfo loc) =
  RecordUpdate <$> resolveExp e1 <*> pure fs <*> resolveExp e2 <*> pure NoInfo <*> pure loc
resolveExp (OpSection v NoInfo loc) =
  OpSection <$> resolveQualName v loc <*> pure NoInfo <*> pure loc
resolveExp (OpSectionLeft v info1 e info2 info3 loc) =
  OpSectionLeft
    <$> resolveQualName v loc
    <*> pure info1
    <*> resolveExp e
    <*> pure info2
    <*> pure info3
    <*> pure loc
resolveExp (OpSectionRight v info1 e info2 info3 loc) =
  OpSectionRight
    <$> resolveQualName v loc
    <*> pure info1
    <*> resolveExp e
    <*> pure info2
    <*> pure info3
    <*> pure loc
resolveExp (ProjectSection ks info loc) =
  pure $ ProjectSection ks info loc
resolveExp (IndexSection slice info loc) =
  IndexSection <$> resolveSlice slice <*> pure info <*> pure loc
resolveExp (Ascript e te loc) =
  Ascript <$> resolveExp e <*> resolveTypeExp te <*> pure loc
resolveExp (Coerce e te info loc) =
  Coerce <$> resolveExp e <*> resolveTypeExp te <*> pure info <*> pure loc
resolveExp (AppExp e NoInfo) =
  AppExp <$> resolveAppExp e <*> pure NoInfo

sizeBinderToParam :: SizeBinder Name -> UncheckedTypeParam
sizeBinderToParam (SizeBinder v loc) = TypeParamDim v loc

patternExp :: UncheckedPat t -> TypeM (ExpBase NoInfo VName)
patternExp (Id v _ loc) =
  Var <$> resolveQualName (qualName v) loc <*> pure NoInfo <*> pure loc
patternExp (TuplePat pats loc) = TupLit <$> mapM patternExp pats <*> pure loc
patternExp (Wildcard _ loc) = typeError loc mempty "Cannot have wildcard here."
patternExp (PatLit _ _ loc) = typeError loc mempty "Cannot have literal here."
patternExp (PatConstr _ _ _ loc) = typeError loc mempty "Cannot have constructor here."
patternExp (PatAttr _ p _) = patternExp p
patternExp (PatAscription pat _ _) = patternExp pat
patternExp (PatParens pat _) = patternExp pat
patternExp (RecordPat fs loc) = RecordLit <$> mapM field fs <*> pure loc
  where
    field (name, pat) = RecordFieldExplicit name <$> patternExp pat <*> pure loc

resolveAppExp :: AppExpBase NoInfo Name -> TypeM (AppExpBase NoInfo VName)
resolveAppExp (Apply f args loc) =
  Apply <$> resolveExp f <*> traverse (traverse resolveExp) args <*> pure loc
resolveAppExp (Range e1 e2 e3 loc) =
  Range
    <$> resolveExp e1
    <*> traverse resolveExp e2
    <*> traverse resolveExp e3
    <*> pure loc
resolveAppExp (If e1 e2 e3 loc) =
  If <$> resolveExp e1 <*> resolveExp e2 <*> resolveExp e3 <*> pure loc
resolveAppExp (Match e cases loc) =
  Match <$> resolveExp e <*> mapM resolveCase cases <*> pure loc
  where
    resolveCase (CasePat p body cloc) =
      resolvePat p $ \p' -> CasePat p' <$> resolveExp body <*> pure cloc
resolveAppExp (LetPat sizes p e1 e2 loc) = do
  checkForDuplicateNames (map sizeBinderToParam sizes) [p]
  resolveSizes sizes $ \sizes' -> do
    e1' <- resolveExp e1
    resolvePat p $ \p' -> do
      e2' <- resolveExp e2
      pure $ LetPat sizes' p' e1' e2' loc
resolveAppExp (LetFun fname (tparams, params, ret, NoInfo, fbody) body loc) = do
  checkForDuplicateNames tparams params
  checkDoNotShadow loc fname
  (tparams', params', ret', fbody') <-
    resolveTypeParams tparams $ \tparams' ->
      resolveParams params $ \params' -> do
        ret' <- traverse resolveTypeExp ret
        (tparams',params',ret',) <$> resolveExp fbody
  bindSpaced1 Term fname loc $ \fname' -> do
    body' <- resolveExp body
    pure $ LetFun fname' (tparams', params', ret', NoInfo, fbody') body' loc
resolveAppExp (LetWith (Ident dst _ dstloc) (Ident src _ srcloc) slice e1 e2 loc) = do
  src' <- Ident <$> resolveName src srcloc <*> pure NoInfo <*> pure srcloc
  e1' <- resolveExp e1
  slice' <- resolveSlice slice
  bindSpaced1 Term dst loc $ \dstv -> do
    let dst' = Ident dstv NoInfo dstloc
    e2' <- resolveExp e2
    pure $ LetWith dst' src' slice' e1' e2' loc
resolveAppExp (BinOp (f, floc) finfo (e1, info1) (e2, info2) loc) = do
  f' <- resolveQualName f floc
  e1' <- resolveExp e1
  e2' <- resolveExp e2
  pure $ BinOp (f', floc) finfo (e1', info1) (e2', info2) loc
resolveAppExp (Index e1 slice loc) =
  Index <$> resolveExp e1 <*> resolveSlice slice <*> pure loc
resolveAppExp (Loop sizes pat loopinit form body loc) = do
  e' <- case loopinit of
    LoopInitExplicit e -> LoopInitExplicit <$> resolveExp e
    LoopInitImplicit NoInfo -> LoopInitExplicit <$> patternExp pat
  case form of
    For (Ident i _ iloc) bound -> do
      bound' <- resolveExp bound
      bindSpaced1 Term i iloc $ \iv -> do
        let i' = Ident iv NoInfo iloc
        resolvePat pat $ \pat' -> do
          body' <- resolveExp body
          pure $ Loop sizes pat' e' (For i' bound') body' loc
    ForIn elemp arr -> do
      arr' <- resolveExp arr
      resolvePat elemp $ \elemp' -> resolvePat pat $ \pat' -> do
        body' <- resolveExp body
        pure $ Loop sizes pat' e' (ForIn elemp' arr') body' loc
    While cond -> resolvePat pat $ \pat' -> do
      cond' <- resolveExp cond
      body' <- resolveExp body
      pure $ Loop sizes pat' e' (While cond') body' loc

resolveSlice :: SliceBase NoInfo Name -> TypeM (SliceBase NoInfo VName)
resolveSlice = mapM onDimIndex
  where
    onDimIndex (DimFix e) = DimFix <$> resolveExp e
    onDimIndex (DimSlice e1 e2 e3) =
      DimSlice
        <$> traverse resolveExp e1
        <*> traverse resolveExp e2
        <*> traverse resolveExp e3

resolvePat :: PatBase NoInfo Name t -> (PatBase NoInfo VName t -> TypeM a) -> TypeM a
resolvePat outer m = do
  outer' <- resolve outer
  bindIdents (patIdents outer') $ m outer'
  where
    resolve (Id v NoInfo loc) = do
      checkDoNotShadow loc v
      Id <$> newID v <*> pure NoInfo <*> pure loc
    resolve (Wildcard NoInfo loc) =
      pure $ Wildcard NoInfo loc
    resolve (PatParens p loc) =
      PatParens <$> resolve p <*> pure loc
    resolve (TuplePat ps loc) =
      TuplePat <$> mapM resolve ps <*> pure loc
    resolve (RecordPat ps loc) =
      RecordPat <$> mapM (traverse resolve) ps <*> pure loc
    resolve (PatAscription p t loc) =
      PatAscription <$> resolve p <*> resolveTypeExp t <*> pure loc
    resolve (PatLit l NoInfo loc) =
      pure $ PatLit l NoInfo loc
    resolve (PatConstr k NoInfo ps loc) =
      PatConstr k NoInfo <$> mapM resolve ps <*> pure loc
    resolve (PatAttr attr p loc) =
      PatAttr <$> resolveAttrInfo attr <*> resolve p <*> pure loc

resolveParams :: [PatBase NoInfo Name ParamType] -> ([PatBase NoInfo VName ParamType] -> TypeM a) -> TypeM a
resolveParams [] m = m []
resolveParams (p : ps) m = resolvePat p $ \p' -> resolveParams ps (m . (p' :))

-- | @resolveTypeParams ps m@ resolves the type parameters @ps@, then
-- invokes the continuation @m@ with the resolveed parameters, while
-- extending the monadic name map with @ps@.
resolveTypeParams ::
  [TypeParamBase Name] -> ([TypeParamBase VName] -> TypeM a) -> TypeM a
resolveTypeParams ps m =
  bindSpaced (map typeParamSpace ps) $ \_ ->
    m =<< evalStateT (mapM checkTypeParam ps) mempty
  where
    typeParamSpace (TypeParamDim pv loc) = (Term, pv, loc)
    typeParamSpace (TypeParamType _ pv loc) = (Type, pv, loc)

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

resolveSizes :: [SizeBinder Name] -> ([SizeBinder VName] -> TypeM a) -> TypeM a
resolveSizes [] m = m [] -- Minor optimisation.
resolveSizes sizes m = do
  foldM_ lookForDuplicates mempty sizes
  bindSpaced (map sizeWithSpace sizes) $ \sizes' ->
    m $ zipWith SizeBinder sizes' $ map srclocOf sizes
  where
    lookForDuplicates prev size
      | Just (_, prevloc) <- L.find ((== sizeName size) . fst) prev =
          typeError size mempty $
            "Size name also bound at "
              <> pretty (locStrRel (srclocOf size) prevloc)
              <> "."
      | otherwise =
          pure $ (sizeName size, srclocOf size) : prev

    sizeWithSpace size =
      (Term, sizeName size, srclocOf size)

-- | Resolve names in a value binding. If this succeeds, then it is
-- guaranteed that all names references things that are in scope.
resolveValBind :: ValBindBase NoInfo Name -> TypeM (ValBindBase NoInfo VName)
resolveValBind (ValBind entry fname ret NoInfo tparams params body doc attrs loc) = do
  attrs' <- mapM resolveAttrInfo attrs
  checkForDuplicateNames tparams params
  checkDoNotShadow loc fname
  resolveTypeParams tparams $ \tparams' ->
    resolveParams params $ \params' -> do
      ret' <- traverse resolveTypeExp ret
      body' <- resolveExp body
      bindSpaced1 Term fname loc $ \fname' -> do
        usedName fname'
        pure $ ValBind entry fname' ret' NoInfo tparams' params' body' doc attrs' loc
