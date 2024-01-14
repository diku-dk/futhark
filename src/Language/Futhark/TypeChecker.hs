-- | The type checker checks whether the program is type-consistent
-- and adds type annotations and various other elaborations.  The
-- program does not need to have any particular properties for the
-- type checker to function; in particular it does not need unique
-- names.
module Language.Futhark.TypeChecker
  ( checkProg,
    checkExp,
    checkDec,
    checkModExp,
    Notes,
    TypeError (..),
    prettyTypeError,
    prettyTypeErrorNoLoc,
    Warnings,
    initialEnv,
    envWithImports,
  )
where

import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.Either
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Data.Set qualified as S
import Futhark.FreshNames hiding (newName)
import Futhark.Util.Pretty hiding (space)
import Language.Futhark
import Language.Futhark.Semantic
import Language.Futhark.TypeChecker.Modules
import Language.Futhark.TypeChecker.Monad
import Language.Futhark.TypeChecker.Terms
import Language.Futhark.TypeChecker.Types
import Prelude hiding (abs, mod)

--- The main checker

-- | Type check a program containing no type information, yielding
-- either a type error or a program with complete type information.
-- Accepts a mapping from file names (excluding extension) to
-- previously type checked results.  The 'ImportName' is used to resolve
-- relative @import@s.
checkProg ::
  Imports ->
  VNameSource ->
  ImportName ->
  UncheckedProg ->
  (Warnings, Either TypeError (FileModule, VNameSource))
checkProg files src name prog =
  runTypeM initialEnv files' name src checkSizeExp $ checkProgM prog
  where
    files' = M.map fileEnv $ M.fromList files

-- | Type check a single expression containing no type information,
-- yielding either a type error or the same expression annotated with
-- type information.  Also returns a list of type parameters, which
-- will be nonempty if the expression is polymorphic.  See also
-- 'checkProg'.
checkExp ::
  Imports ->
  VNameSource ->
  Env ->
  UncheckedExp ->
  (Warnings, Either TypeError ([TypeParam], Exp))
checkExp files src env e =
  second (fmap fst) $ runTypeM env files' (mkInitialImport "") src checkSizeExp $ checkOneExp e
  where
    files' = M.map fileEnv $ M.fromList files

-- | Type check a single declaration containing no type information,
-- yielding either a type error or the same declaration annotated with
-- type information along the Env produced by that declaration.  See
-- also 'checkProg'.
checkDec ::
  Imports ->
  VNameSource ->
  Env ->
  ImportName ->
  UncheckedDec ->
  (Warnings, Either TypeError (Env, Dec, VNameSource))
checkDec files src env name d =
  second (fmap massage) $
    runTypeM env files' name src checkSizeExp $ do
      (_, env', d') <- checkOneDec d
      pure (env' <> env, d')
  where
    massage ((env', d'), src') =
      (env', d', src')
    files' = M.map fileEnv $ M.fromList files

-- | Type check a single module expression containing no type information,
-- yielding either a type error or the same expression annotated with
-- type information along the Env produced by that declaration.  See
-- also 'checkProg'.
checkModExp ::
  Imports ->
  VNameSource ->
  Env ->
  ModExpBase NoInfo Name ->
  (Warnings, Either TypeError (MTy, ModExpBase Info VName))
checkModExp files src env me =
  second (fmap fst) . runTypeM env files' (mkInitialImport "") src checkSizeExp $ do
    (_abs, mty, me') <- checkOneModExp me
    pure (mty, me')
  where
    files' = M.map fileEnv $ M.fromList files

-- | An initial environment for the type checker, containing
-- intrinsics and such.
initialEnv :: Env
initialEnv =
  intrinsicsModule
    { envModTable = initialModTable,
      envNameMap =
        M.insert
          (Term, nameFromString "intrinsics")
          (qualName intrinsics_v)
          topLevelNameMap
    }
  where
    initialTypeTable = M.fromList $ mapMaybe addIntrinsicT $ M.toList intrinsics
    initialModTable = M.singleton intrinsics_v (ModEnv intrinsicsModule)

    intrinsics_v = VName (nameFromString "intrinsics") 0

    intrinsicsModule = Env mempty initialTypeTable mempty mempty intrinsicsNameMap

    addIntrinsicT (name, IntrinsicType l ps t) =
      Just (name, TypeAbbr l ps $ RetType [] t)
    addIntrinsicT _ =
      Nothing

-- | Produce an environment, based on the one passed in, where all of
-- the provided imports have been @open@ened in order.  This could in principle
-- also be done with 'checkDec', but this is more precise.
envWithImports :: Imports -> Env -> Env
envWithImports imports env =
  mconcat (map (fileEnv . snd) (reverse imports)) <> env

checkProgM :: UncheckedProg -> TypeM FileModule
checkProgM (Prog doc decs) = do
  checkForDuplicateDecs decs
  (abs, env, decs', full_env) <- checkDecs decs
  pure (FileModule abs env (Prog doc decs') full_env)

dupDefinitionError ::
  (MonadTypeChecker m) =>
  Namespace ->
  Name ->
  SrcLoc ->
  SrcLoc ->
  m a
dupDefinitionError space name loc1 loc2 =
  typeError loc1 mempty $
    "Duplicate definition of"
      <+> pretty space
      <+> prettyName name
      <> "."
        </> "Previously defined at"
        <+> pretty (locStr loc2)
      <> "."

checkForDuplicateDecs :: [DecBase NoInfo Name] -> TypeM ()
checkForDuplicateDecs =
  foldM_ (flip f) mempty
  where
    check namespace name loc known =
      case M.lookup (namespace, name) known of
        Just loc' ->
          dupDefinitionError namespace name loc loc'
        _ -> pure $ M.insert (namespace, name) loc known

    f (ValDec vb) =
      check Term (valBindName vb) (srclocOf vb)
    f (TypeDec (TypeBind name _ _ _ _ _ loc)) =
      check Type name loc
    f (ModTypeDec (ModTypeBind name _ _ loc)) =
      check Signature name loc
    f (ModDec (ModBind name _ _ _ _ loc)) =
      check Term name loc
    f OpenDec {} = pure
    f LocalDec {} = pure
    f ImportDec {} = pure

bindingTypeParams :: [TypeParam] -> TypeM a -> TypeM a
bindingTypeParams tparams = localEnv env
  where
    env = mconcat $ map typeParamEnv tparams

    typeParamEnv (TypeParamDim v _) =
      mempty
        { envVtable =
            M.singleton v $ BoundV [] (Scalar $ Prim $ Signed Int64)
        }
    typeParamEnv (TypeParamType l v _) =
      mempty
        { envTypeTable =
            M.singleton v $
              TypeAbbr l [] . RetType [] . Scalar $
                TypeVar mempty (qualName v) []
        }

checkTypeDecl ::
  UncheckedTypeExp ->
  TypeM ([VName], TypeExp Info VName, StructType, Liftedness)
checkTypeDecl te = do
  (te', svars, RetType dims st, l) <- checkTypeExp te
  pure (svars ++ dims, te', toStruct st, l)

-- In this function, after the recursion, we add the Env of the
-- current Spec *after* the one that is returned from the recursive
-- call.  This implements the behaviour that specs later in a module
-- type can override those earlier (it rarely matters, but it affects
-- the specific structure of substitutions in case some module type is
-- redundantly imported multiple times).
checkSpecs :: [SpecBase NoInfo Name] -> TypeM (TySet, Env, [SpecBase Info VName])
checkSpecs [] = pure (mempty, mempty, [])
checkSpecs (ValSpec name tparams vtype NoInfo doc loc : specs) =
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    (tparams', vtype', vtype_t) <-
      checkTypeParams tparams $ \tparams' -> bindingTypeParams tparams' $ do
        (ext, vtype', vtype_t, _) <- checkTypeDecl vtype

        unless (null ext) $
          typeError loc mempty $
            "All function parameters must have non-anonymous sizes."
              </> "Hint: add size parameters to"
              <+> dquotes (prettyName name')
              <> "."

        pure (tparams', vtype', vtype_t)

    let binding = BoundV tparams' vtype_t
        valenv =
          mempty
            { envVtable = M.singleton name' binding,
              envNameMap = M.singleton (Term, name) $ qualName name'
            }
    (abstypes, env, specs') <- localEnv valenv $ checkSpecs specs
    pure
      ( abstypes,
        env <> valenv,
        ValSpec name' tparams' vtype' (Info vtype_t) doc loc : specs'
      )
checkSpecs (TypeAbbrSpec tdec : specs) =
  bindSpaced [(Type, typeAlias tdec)] $ do
    (tenv, tdec') <- checkTypeBind tdec
    (abstypes, env, specs') <- localEnv tenv $ checkSpecs specs
    pure
      ( abstypes,
        env <> tenv,
        TypeAbbrSpec tdec' : specs'
      )
checkSpecs (TypeSpec l name ps doc loc : specs) =
  checkTypeParams ps $ \ps' ->
    bindSpaced [(Type, name)] $ do
      name' <- checkName Type name loc
      let tenv =
            mempty
              { envNameMap =
                  M.singleton (Type, name) $ qualName name',
                envTypeTable =
                  M.singleton name' $
                    TypeAbbr l ps' . RetType [] . Scalar $
                      TypeVar mempty (qualName name') $
                        map typeParamToArg ps'
              }
      (abstypes, env, specs') <- localEnv tenv $ checkSpecs specs
      pure
        ( M.insert (qualName name') l abstypes,
          env <> tenv,
          TypeSpec l name' ps' doc loc : specs'
        )
checkSpecs (ModSpec name sig doc loc : specs) =
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    (_sig_abs, mty, sig') <- checkModTypeExp sig
    let senv =
          mempty
            { envNameMap = M.singleton (Term, name) $ qualName name',
              envModTable = M.singleton name' $ mtyMod mty
            }
    (abstypes, env, specs') <- localEnv senv $ checkSpecs specs
    pure
      ( M.mapKeys (qualify name') (mtyAbs mty) <> abstypes,
        env <> senv,
        ModSpec name' sig' doc loc : specs'
      )
checkSpecs (IncludeSpec e loc : specs) = do
  (e_abs, env_abs, e_env, e') <- checkModTypeExpToEnv e

  mapM_ (warnIfShadowing . fmap baseName) $ M.keys env_abs

  (abstypes, env, specs') <- localEnv e_env $ checkSpecs specs
  pure
    ( e_abs <> env_abs <> abstypes,
      env <> e_env,
      IncludeSpec e' loc : specs'
    )
  where
    warnIfShadowing qn =
      (lookupType loc qn >> warnAbout qn)
        `catchError` \_ -> pure ()
    warnAbout qn =
      warn loc $ "Inclusion shadows type" <+> dquotes (pretty qn) <+> "."

checkModTypeExp :: ModTypeExpBase NoInfo Name -> TypeM (TySet, MTy, ModTypeExpBase Info VName)
checkModTypeExp (ModTypeParens e loc) = do
  (abs, mty, e') <- checkModTypeExp e
  pure (abs, mty, ModTypeParens e' loc)
checkModTypeExp (ModTypeVar name NoInfo loc) = do
  (name', mty) <- lookupMTy loc name
  (mty', substs) <- newNamesForMTy mty
  pure (mtyAbs mty', mty', ModTypeVar name' (Info substs) loc)
checkModTypeExp (ModTypeSpecs specs loc) = do
  checkForDuplicateSpecs specs
  (abstypes, env, specs') <- checkSpecs specs
  pure (abstypes, MTy abstypes $ ModEnv env, ModTypeSpecs specs' loc)
checkModTypeExp (ModTypeWith s (TypeRef tname ps te trloc) loc) = do
  (abs, s_abs, s_env, s') <- checkModTypeExpToEnv s
  checkTypeParams ps $ \ps' -> do
    (ext, te', te_t, _) <- bindingTypeParams ps' $ checkTypeDecl te
    unless (null ext) $
      typeError te' mempty "Anonymous dimensions are not allowed here."
    (tname', s_abs', s_env') <- refineEnv loc s_abs s_env tname ps' te_t
    pure (abs, MTy s_abs' $ ModEnv s_env', ModTypeWith s' (TypeRef tname' ps' te' trloc) loc)
checkModTypeExp (ModTypeArrow maybe_pname e1 e2 loc) = do
  (e1_abs, MTy s_abs e1_mod, e1') <- checkModTypeExp e1
  (env_for_e2, maybe_pname') <-
    case maybe_pname of
      Just pname -> bindSpaced [(Term, pname)] $ do
        pname' <- checkName Term pname loc
        pure
          ( mempty
              { envNameMap = M.singleton (Term, pname) $ qualName pname',
                envModTable = M.singleton pname' e1_mod
              },
            Just pname'
          )
      Nothing ->
        pure (mempty, Nothing)
  (e2_abs, e2_mod, e2') <- localEnv env_for_e2 $ checkModTypeExp e2
  pure
    ( e1_abs <> e2_abs,
      MTy mempty $ ModFun $ FunModType s_abs e1_mod e2_mod,
      ModTypeArrow maybe_pname' e1' e2' loc
    )

checkModTypeExpToEnv ::
  ModTypeExpBase NoInfo Name ->
  TypeM (TySet, TySet, Env, ModTypeExpBase Info VName)
checkModTypeExpToEnv e = do
  (abs, MTy mod_abs mod, e') <- checkModTypeExp e
  case mod of
    ModEnv env -> pure (abs, mod_abs, env, e')
    ModFun {} -> unappliedFunctor $ srclocOf e

checkModTypeBind :: ModTypeBindBase NoInfo Name -> TypeM (TySet, Env, ModTypeBindBase Info VName)
checkModTypeBind (ModTypeBind name e doc loc) = do
  (abs, env, e') <- checkModTypeExp e
  bindSpaced [(Signature, name)] $ do
    name' <- checkName Signature name loc
    pure
      ( abs,
        mempty
          { envModTypeTable = M.singleton name' env,
            envNameMap = M.singleton (Signature, name) (qualName name')
          },
        ModTypeBind name' e' doc loc
      )

checkOneModExp ::
  ModExpBase NoInfo Name ->
  TypeM (TySet, MTy, ModExpBase Info VName)
checkOneModExp (ModParens e loc) = do
  (abs, mty, e') <- checkOneModExp e
  pure (abs, mty, ModParens e' loc)
checkOneModExp (ModDecs decs loc) = do
  checkForDuplicateDecs decs
  (abstypes, env, decs', _) <- checkDecs decs
  pure
    ( abstypes,
      MTy abstypes $ ModEnv env,
      ModDecs decs' loc
    )
checkOneModExp (ModVar v loc) = do
  (v', env) <- lookupMod loc v
  when
    ( baseName (qualLeaf v') == nameFromString "intrinsics"
        && baseTag (qualLeaf v') <= maxIntrinsicTag
    )
    $ typeError loc mempty "The 'intrinsics' module may not be used in module expressions."
  pure (mempty, MTy mempty env, ModVar v' loc)
checkOneModExp (ModImport name NoInfo loc) = do
  (name', env) <- lookupImport loc name
  pure
    ( mempty,
      MTy mempty $ ModEnv env,
      ModImport name (Info name') loc
    )
checkOneModExp (ModApply f e NoInfo NoInfo loc) = do
  (f_abs, f_mty, f') <- checkOneModExp f
  case mtyMod f_mty of
    ModFun functor -> do
      (e_abs, e_mty, e') <- checkOneModExp e
      (mty, psubsts, rsubsts) <- applyFunctor (locOf loc) functor e_mty
      pure
        ( mtyAbs mty <> f_abs <> e_abs,
          mty,
          ModApply f' e' (Info psubsts) (Info rsubsts) loc
        )
    _ ->
      typeError loc mempty "Cannot apply non-parametric module."
checkOneModExp (ModAscript me se NoInfo loc) = do
  (me_abs, me_mod, me') <- checkOneModExp me
  (se_abs, se_mty, se') <- checkModTypeExp se
  match_subst <- badOnLeft $ matchMTys me_mod se_mty (locOf loc)
  pure (se_abs <> me_abs, se_mty, ModAscript me' se' (Info match_subst) loc)
checkOneModExp (ModLambda param maybe_fsig_e body_e loc) =
  withModParam param $ \param' param_abs param_mod -> do
    (abs, maybe_fsig_e', body_e', mty) <-
      checkModBody (fst <$> maybe_fsig_e) body_e loc
    pure
      ( abs,
        MTy mempty $ ModFun $ FunModType param_abs param_mod mty,
        ModLambda param' maybe_fsig_e' body_e' loc
      )

checkOneModExpToEnv :: ModExpBase NoInfo Name -> TypeM (TySet, Env, ModExpBase Info VName)
checkOneModExpToEnv e = do
  (e_abs, MTy abs mod, e') <- checkOneModExp e
  case mod of
    ModEnv env -> pure (e_abs <> abs, env, e')
    ModFun {} -> unappliedFunctor $ srclocOf e

withModParam ::
  ModParamBase NoInfo Name ->
  (ModParamBase Info VName -> TySet -> Mod -> TypeM a) ->
  TypeM a
withModParam (ModParam pname psig_e NoInfo loc) m = do
  (_abs, MTy p_abs p_mod, psig_e') <- checkModTypeExp psig_e
  bindSpaced [(Term, pname)] $ do
    pname' <- checkName Term pname loc
    let in_body_env = mempty {envModTable = M.singleton pname' p_mod}
    localEnv in_body_env $
      m (ModParam pname' psig_e' (Info $ map qualLeaf $ M.keys p_abs) loc) p_abs p_mod

withModParams ::
  [ModParamBase NoInfo Name] ->
  ([(ModParamBase Info VName, TySet, Mod)] -> TypeM a) ->
  TypeM a
withModParams [] m = m []
withModParams (p : ps) m =
  withModParam p $ \p' pabs pmod ->
    withModParams ps $ \ps' -> m $ (p', pabs, pmod) : ps'

checkModBody ::
  Maybe (ModTypeExpBase NoInfo Name) ->
  ModExpBase NoInfo Name ->
  SrcLoc ->
  TypeM
    ( TySet,
      Maybe (ModTypeExp, Info (M.Map VName VName)),
      ModExp,
      MTy
    )
checkModBody maybe_fsig_e body_e loc = enteringModule $ do
  (body_e_abs, body_mty, body_e') <- checkOneModExp body_e
  case maybe_fsig_e of
    Nothing ->
      pure
        ( mtyAbs body_mty <> body_e_abs,
          Nothing,
          body_e',
          body_mty
        )
    Just fsig_e -> do
      (fsig_abs, fsig_mty, fsig_e') <- checkModTypeExp fsig_e
      fsig_subst <- badOnLeft $ matchMTys body_mty fsig_mty (locOf loc)
      pure
        ( fsig_abs <> body_e_abs,
          Just (fsig_e', Info fsig_subst),
          body_e',
          fsig_mty
        )

checkModBind :: ModBindBase NoInfo Name -> TypeM (TySet, Env, ModBindBase Info VName)
checkModBind (ModBind name [] maybe_fsig_e e doc loc) = do
  (e_abs, maybe_fsig_e', e', mty) <- checkModBody (fst <$> maybe_fsig_e) e loc
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    pure
      ( e_abs,
        mempty
          { envModTable = M.singleton name' $ mtyMod mty,
            envNameMap = M.singleton (Term, name) $ qualName name'
          },
        ModBind name' [] maybe_fsig_e' e' doc loc
      )
checkModBind (ModBind name (p : ps) maybe_fsig_e body_e doc loc) = do
  (abs, params', maybe_fsig_e', body_e', funsig) <-
    withModParam p $ \p' p_abs p_mod ->
      withModParams ps $ \params_stuff -> do
        let (ps', ps_abs, ps_mod) = unzip3 params_stuff
        (abs, maybe_fsig_e', body_e', mty) <- checkModBody (fst <$> maybe_fsig_e) body_e loc
        let addParam (x, y) mty' = MTy mempty $ ModFun $ FunModType x y mty'
        pure
          ( abs,
            p' : ps',
            maybe_fsig_e',
            body_e',
            FunModType p_abs p_mod $ foldr addParam mty $ zip ps_abs ps_mod
          )
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    pure
      ( abs,
        mempty
          { envModTable =
              M.singleton name' $ ModFun funsig,
            envNameMap =
              M.singleton (Term, name) $ qualName name'
          },
        ModBind name' params' maybe_fsig_e' body_e' doc loc
      )

checkForDuplicateSpecs :: [SpecBase NoInfo Name] -> TypeM ()
checkForDuplicateSpecs =
  foldM_ (flip f) mempty
  where
    check namespace name loc known =
      case M.lookup (namespace, name) known of
        Just loc' ->
          dupDefinitionError namespace name loc loc'
        _ -> pure $ M.insert (namespace, name) loc known

    f (ValSpec name _ _ _ _ loc) =
      check Term name loc
    f (TypeAbbrSpec (TypeBind name _ _ _ _ _ loc)) =
      check Type name loc
    f (TypeSpec _ name _ _ loc) =
      check Type name loc
    f (ModSpec name _ _ loc) =
      check Term name loc
    f IncludeSpec {} =
      pure

checkTypeBind ::
  TypeBindBase NoInfo Name ->
  TypeM (Env, TypeBindBase Info VName)
checkTypeBind (TypeBind name l tps te NoInfo doc loc) =
  checkTypeParams tps $ \tps' -> do
    (te', svars, RetType dims t, l') <- bindingTypeParams tps' $ checkTypeExp te

    let (witnessed, _) = determineSizeWitnesses $ toStruct t
    case L.find (`S.notMember` witnessed) svars of
      Just _ ->
        typeError (locOf te) mempty . withIndexLink "anonymous-nonconstructive" $
          "Type abbreviation contains an anonymous size not used constructively as an array size."
      Nothing ->
        pure ()

    let elab_t = RetType (svars ++ dims) $ toStruct t

    let used_dims = fvVars $ freeInType t
    case filter ((`S.notMember` used_dims) . typeParamName) $
      filter isSizeParam tps' of
      [] -> pure ()
      tp : _ ->
        typeError loc mempty $
          "Size parameter" <+> dquotes (pretty tp) <+> "unused."

    case (l, l') of
      (_, Lifted)
        | l < Lifted ->
            typeError loc mempty $
              "Non-lifted type abbreviations may not contain functions."
                </> "Hint: consider using 'type^'."
      (_, SizeLifted)
        | l < SizeLifted ->
            typeError loc mempty $
              "Non-size-lifted type abbreviations may not contain size-lifted types."
                </> "Hint: consider using 'type~'."
      (Unlifted, _)
        | not $ null $ svars ++ dims ->
            typeError loc mempty $
              "Non-lifted type abbreviations may not use existential sizes in their definition."
                </> "Hint: use 'type~' or add size parameters to"
                <+> dquotes (prettyName name)
                <> "."
      _ -> pure ()

    bindSpaced [(Type, name)] $ do
      name' <- checkName Type name loc
      pure
        ( mempty
            { envTypeTable =
                M.singleton name' $ TypeAbbr l tps' elab_t,
              envNameMap =
                M.singleton (Type, name) $ qualName name'
            },
          TypeBind name' l tps' te' (Info elab_t) doc loc
        )

entryPoint :: [Pat ParamType] -> Maybe (TypeExp Info VName) -> ResRetType -> EntryPoint
entryPoint params orig_ret_te (RetType _ret orig_ret) =
  EntryPoint (map patternEntry params ++ more_params) rettype'
  where
    (more_params, rettype') = onRetType orig_ret_te $ toStruct orig_ret

    patternEntry (PatParens p _) =
      patternEntry p
    patternEntry (PatAscription p te _) =
      EntryParam (patternName p) $ EntryType (patternStructType p) (Just te)
    patternEntry p =
      EntryParam (patternName p) $ EntryType (patternStructType p) Nothing

    patternName (Id x _ _) = baseName x
    patternName (PatParens p _) = patternName p
    patternName _ = "_"

    pname (Named v) = baseName v
    pname Unnamed = "_"
    onRetType (Just (TEArrow p t1_te t2_te _)) (Scalar (Arrow _ _ _ t1 (RetType _ t2))) =
      let (xs, y) = onRetType (Just t2_te) $ toStruct t2
       in (EntryParam (maybe "_" baseName p) (EntryType t1 (Just t1_te)) : xs, y)
    onRetType _ (Scalar (Arrow _ p _ t1 (RetType _ t2))) =
      let (xs, y) = onRetType Nothing $ toStruct t2
       in (EntryParam (pname p) (EntryType t1 Nothing) : xs, y)
    onRetType te t =
      ([], EntryType t te)

checkEntryPoint ::
  SrcLoc ->
  [TypeParam] ->
  [Pat ParamType] ->
  Maybe (TypeExp Info VName) ->
  ResRetType ->
  TypeM ()
checkEntryPoint loc tparams params maybe_tdecl rettype
  | any isTypeParam tparams =
      typeError loc mempty $
        withIndexLink
          "polymorphic-entry"
          "Entry point functions may not be polymorphic."
  | not (all orderZero param_ts)
      || not (orderZero rettype') =
      typeError loc mempty $
        withIndexLink
          "higher-order-entry"
          "Entry point functions may not be higher-order."
  | sizes_only_in_ret <-
      S.fromList (map typeParamName tparams)
        `S.intersection` fvVars (freeInType rettype')
        `S.difference` foldMap (fvVars . freeInType) param_ts,
    not $ S.null sizes_only_in_ret =
      typeError loc mempty $
        withIndexLink
          "size-polymorphic-entry"
          "Entry point functions must not be size-polymorphic in their return type."
  | (constructive, _) <- foldMap (determineSizeWitnesses . toStruct) param_ts,
    Just p <- L.find (flip S.notMember constructive . typeParamName) tparams =
      typeError p mempty . withIndexLink "nonconstructive-entry" $
        "Entry point size parameter "
          <> pretty p
          <> " only used non-constructively."
  | p : _ <- filter nastyParameter params =
      warn p $
        "Entry point parameter\n"
          </> indent 2 (pretty p)
          </> "\nwill have an opaque type, so the entry point will likely not be callable."
  | nastyReturnType maybe_tdecl rettype_t =
      warn loc $
        "Entry point return type\n"
          </> indent 2 (pretty rettype)
          </> "\nwill have an opaque type, so the result will likely not be usable."
  | otherwise =
      pure ()
  where
    (RetType _ rettype_t) = rettype
    (rettype_params, rettype') = unfoldFunType rettype_t
    param_ts = map patternType params ++ rettype_params

checkValBind :: ValBindBase NoInfo Name -> TypeM (Env, ValBind)
checkValBind (ValBind entry fname maybe_tdecl NoInfo tparams params body doc attrs loc) = do
  top_level <- atTopLevel
  when (not top_level && isJust entry) $
    typeError loc mempty $
      withIndexLink "nested-entry" "Entry points may not be declared inside modules."

  (fname', tparams', params', maybe_tdecl', rettype, body') <-
    checkFunDef (fname, maybe_tdecl, tparams, params, body, loc)

  let entry' = Info (entryPoint params' maybe_tdecl' rettype) <$ entry

  case entry' of
    Just _ -> checkEntryPoint loc tparams' params' maybe_tdecl' rettype
    _ -> pure ()

  attrs' <- mapM checkAttr attrs
  let vb = ValBind entry' fname' maybe_tdecl' (Info rettype) tparams' params' body' doc attrs' loc
  pure
    ( mempty
        { envVtable =
            M.singleton fname' $ uncurry BoundV $ valBindTypeScheme vb,
          envNameMap =
            M.singleton (Term, fname) $ qualName fname'
        },
      vb
    )

nastyType :: (Monoid als) => TypeBase dim als -> Bool
nastyType (Scalar Prim {}) = False
nastyType t@Array {} = nastyType $ stripArray 1 t
nastyType _ = True

nastyReturnType :: (Monoid als) => Maybe (TypeExp Info VName) -> TypeBase dim als -> Bool
nastyReturnType Nothing (Scalar (Arrow _ _ _ t1 (RetType _ t2))) =
  nastyType t1 || nastyReturnType Nothing t2
nastyReturnType (Just (TEArrow _ te1 te2 _)) (Scalar (Arrow _ _ _ t1 (RetType _ t2))) =
  (not (niceTypeExp te1) && nastyType t1)
    || nastyReturnType (Just te2) t2
nastyReturnType (Just te) _
  | niceTypeExp te = False
nastyReturnType te t
  | Just ts <- isTupleRecord t =
      case te of
        Just (TETuple tes _) -> or $ zipWith nastyType' (map Just tes) ts
        _ -> any nastyType ts
  | otherwise = nastyType' te t
  where
    nastyType' (Just te') _ | niceTypeExp te' = False
    nastyType' _ t' = nastyType t'

nastyParameter :: Pat ParamType -> Bool
nastyParameter p = nastyType (patternType p) && not (ascripted p)
  where
    ascripted (PatAscription _ te _) = niceTypeExp te
    ascripted (PatParens p' _) = ascripted p'
    ascripted _ = False

niceTypeExp :: TypeExp Info VName -> Bool
niceTypeExp (TEVar (QualName [] _) _) = True
niceTypeExp (TEApply te TypeArgExpSize {} _) = niceTypeExp te
niceTypeExp (TEArray _ te _) = niceTypeExp te
niceTypeExp (TEUnique te _) = niceTypeExp te
niceTypeExp (TEDim _ te _) = niceTypeExp te
niceTypeExp _ = False

checkOneDec :: DecBase NoInfo Name -> TypeM (TySet, Env, DecBase Info VName)
checkOneDec (ModDec struct) = do
  (abs, modenv, struct') <- checkModBind struct
  pure (abs, modenv, ModDec struct')
checkOneDec (ModTypeDec sig) = do
  (abs, sigenv, sig') <- checkModTypeBind sig
  pure (abs, sigenv, ModTypeDec sig')
checkOneDec (TypeDec tdec) = do
  (tenv, tdec') <- checkTypeBind tdec
  pure (mempty, tenv, TypeDec tdec')
checkOneDec (OpenDec x loc) = do
  (x_abs, x_env, x') <- checkOneModExpToEnv x
  pure (x_abs, x_env, OpenDec x' loc)
checkOneDec (LocalDec d loc) = do
  (abstypes, env, d') <- checkOneDec d
  pure (abstypes, env, LocalDec d' loc)
checkOneDec (ImportDec name NoInfo loc) = do
  (name', env) <- lookupImport loc name
  when (isBuiltin name) $
    typeError loc mempty $
      pretty name <+> "may not be explicitly imported."
  pure (mempty, env, ImportDec name (Info name') loc)
checkOneDec (ValDec vb) = do
  (env, vb') <- checkValBind vb
  pure (mempty, env, ValDec vb')

checkDecs :: [DecBase NoInfo Name] -> TypeM (TySet, Env, [DecBase Info VName], Env)
checkDecs (d : ds) = do
  (d_abstypes, d_env, d') <- checkOneDec d
  (ds_abstypes, ds_env, ds', full_env) <- localEnv d_env $ checkDecs ds
  pure
    ( d_abstypes <> ds_abstypes,
      case d' of
        LocalDec {} -> ds_env
        ImportDec {} -> ds_env
        _ -> ds_env <> d_env,
      d' : ds',
      full_env
    )
checkDecs [] = do
  full_env <- askEnv
  pure (mempty, mempty, [], full_env)
