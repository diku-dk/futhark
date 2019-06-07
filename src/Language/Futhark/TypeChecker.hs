{-# LANGUAGE FlexibleContexts, TupleSections #-}
-- | The type checker checks whether the program is type-consistent
-- and adds type annotations and various other elaborations.  The
-- program does not need to have any particular properties for the
-- type checker to function; in particular it does not need unique
-- names.
module Language.Futhark.TypeChecker
  ( checkProg
  , checkExp
  , checkDec
  , checkModExp
  , TypeError
  , Warnings
  , initialEnv
  )
  where

import Control.Monad.Except
import Control.Monad.Writer
import Data.List
import Data.Loc
import Data.Maybe
import Data.Either
import Data.Ord
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Prelude hiding (abs, mod)

import Language.Futhark
import Language.Futhark.Semantic
import Futhark.FreshNames hiding (newName)
import Language.Futhark.TypeChecker.Monad
import Language.Futhark.TypeChecker.Terms
import Language.Futhark.TypeChecker.Unify (doUnification)
import Language.Futhark.TypeChecker.Types

--- The main checker

-- | Type check a program containing no type information, yielding
-- either a type error or a program with complete type information.
-- Accepts a mapping from file names (excluding extension) to
-- previously type checker results.  The 'FilePath' is used to resolve
-- relative @import@s.
checkProg :: Imports
          -> VNameSource
          -> ImportName
          -> UncheckedProg
          -> Either TypeError (FileModule, Warnings, VNameSource)
checkProg files src name prog =
  runTypeM initialEnv files' name src $ checkProgM prog
  where files' = M.map fileEnv $ M.fromList files

-- | Type check a single expression containing no type information,
-- yielding either a type error or the same expression annotated with
-- type information.  Also returns a list of type parameters, which
-- will be nonempty if the expression is polymorphic.  See also
-- 'checkProg'.
checkExp :: Imports
         -> VNameSource
         -> Env
         -> UncheckedExp
         -> Either TypeError ([TypeParam], Exp)
checkExp files src env e = do
  (e', _, _) <- runTypeM env files' (mkInitialImport "") src $ checkOneExp e
  return e'
  where files' = M.map fileEnv $ M.fromList files

-- | Type check a single declaration containing no type information,
-- yielding either a type error or the same declaration annotated with
-- type information along the Env produced by that declaration.  See
-- also 'checkProg'.
checkDec :: Imports
         -> VNameSource
         -> Env
         -> ImportName
         -> UncheckedDec
         -> Either TypeError (Env, Dec, VNameSource)
checkDec files src env name d = do
  ((env', d'), _, src') <- runTypeM env files' name src $ do
    (_, env', d') <- checkOneDec d
    return (env' <> env, d')
  return (env', d', src')
  where files' = M.map fileEnv $ M.fromList files

-- | Type check a single module expression containing no type information,
-- yielding either a type error or the same expression annotated with
-- type information along the Env produced by that declaration.  See
-- also 'checkProg'.
checkModExp :: Imports
            -> VNameSource
            -> Env
            -> ModExpBase NoInfo Name
            -> Either TypeError (MTy, ModExpBase Info VName)
checkModExp files src env me = do
  (x, _, _) <- runTypeM env files' (mkInitialImport "") src $ checkOneModExp me
  return x
  where files' = M.map fileEnv $ M.fromList files

-- | An initial environment for the type checker, containing
-- intrinsics and such.
initialEnv :: Env
initialEnv = intrinsicsModule
               { envModTable = initialModTable
               , envNameMap = M.insert
                              (Term, nameFromString "intrinsics")
                              (qualName intrinsics_v)
                              topLevelNameMap
               }
  where initialTypeTable = M.fromList $ mapMaybe addIntrinsicT $ M.toList intrinsics
        initialModTable = M.singleton intrinsics_v (ModEnv intrinsicsModule)

        intrinsics_v = VName (nameFromString "intrinsics") 0

        intrinsicsModule = Env mempty initialTypeTable mempty mempty intrinsicsNameMap

        addIntrinsicT (name, IntrinsicType t) =
          Just (name, TypeAbbr Unlifted [] $ Prim t)
        addIntrinsicT _ =
          Nothing

checkProgM :: UncheckedProg -> TypeM FileModule
checkProgM (Prog doc decs) = do
  checkForDuplicateDecs decs
  (abs, env, decs') <- checkDecs decs
  return (FileModule abs env $ Prog doc decs')

dupDefinitionError :: MonadTypeChecker m =>
                      Namespace -> Name -> SrcLoc -> SrcLoc -> m a
dupDefinitionError space name pos1 pos2 =
  throwError $ TypeError pos1 $
  "Duplicate definition of " ++ ppSpace space ++ " " ++
  nameToString name ++ ".  Previously defined at " ++ locStr pos2

checkForDuplicateDecs :: [DecBase NoInfo Name] -> TypeM ()
checkForDuplicateDecs =
  foldM_ (flip f) mempty
  where check namespace name loc known =
          case M.lookup (namespace, name) known of
            Just loc' ->
              dupDefinitionError namespace name loc loc'
            _ -> return $ M.insert (namespace, name) loc known

        f (ValDec (ValBind _ name _ _ _ _ _ _ loc)) =
          check Term name loc

        f (TypeDec (TypeBind name _ _ _ loc)) =
          check Type name loc

        f (SigDec (SigBind name _ _ loc)) =
          check Signature name loc

        f (ModDec (ModBind name _ _ _ _ loc)) =
          check Term name loc

        f OpenDec{} = return
        f LocalDec{} = return
        f ImportDec{} = return

bindingTypeParams :: [TypeParam] -> TypeM a -> TypeM a
bindingTypeParams tparams = localEnv env
  where env = mconcat $ map typeParamEnv tparams

        typeParamEnv (TypeParamDim v _) =
          mempty { envVtable =
                     M.singleton v $ BoundV [] (Prim (Signed Int32)) }
        typeParamEnv (TypeParamType l v _) =
          mempty { envTypeTable =
                     M.singleton v $ TypeAbbr l [] $ TypeVar () Nonunique (typeName v) [] }

-- In this function, after the recursion, we add the Env of the
-- current Spec *after* the one that is returned from the recursive
-- call.  This implements the behaviour that specs later in a module
-- type can override those earlier (it rarely matters, but it affects
-- the specific structure of substitutions in case some module type is
-- redundantly imported multiple times).
checkSpecs :: [SpecBase NoInfo Name] -> TypeM (TySet, Env, [SpecBase Info VName])

checkSpecs [] = return (mempty, mempty, [])

checkSpecs (ValSpec name tparams vtype doc loc : specs) =
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    (tparams', rettype') <-
      checkTypeParams tparams $ \tparams' -> bindingTypeParams tparams' $ do
        (vtype', _) <- checkTypeDecl tparams' vtype
        return (tparams', vtype')

    let binding = BoundV tparams' $ unInfo $ expandedType rettype'
        valenv =
          mempty { envVtable = M.singleton name' binding
                 , envNameMap = M.singleton (Term, name) $ qualName name'
                 }
    (abstypes, env, specs') <- localEnv valenv $ checkSpecs specs
    return (abstypes,
            env <> valenv,
            ValSpec name' tparams' rettype' doc loc : specs')

checkSpecs (TypeAbbrSpec tdec : specs) =
  bindSpaced [(Type, typeAlias tdec)] $ do
    (tenv, tdec') <- checkTypeBind tdec
    (abstypes, env, specs') <- localEnv tenv $ checkSpecs specs
    return (abstypes,
            env <> tenv,
            TypeAbbrSpec tdec' : specs')

checkSpecs (TypeSpec l name ps doc loc : specs) =
  checkTypeParams ps $ \ps' ->
  bindSpaced [(Type, name)] $ do
    name' <- checkName Type name loc
    let tenv = mempty
               { envNameMap =
                   M.singleton (Type, name) $ qualName name'
               , envTypeTable =
                   M.singleton name' $ TypeAbbr l ps' $
                   TypeVar () Nonunique (typeName name') $ map typeParamToArg ps'
               }
    (abstypes, env, specs') <- localEnv tenv $ checkSpecs specs
    return (M.insert (qualName name') l abstypes,
            env <> tenv,
            TypeSpec l name' ps' doc loc : specs')

checkSpecs (ModSpec name sig doc loc : specs) =
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    (mty, sig') <- checkSigExp sig
    let senv = mempty { envNameMap = M.singleton (Term, name) $ qualName name'
                      , envModTable = M.singleton name' $ mtyMod mty
                      }
    (abstypes, env, specs') <- localEnv senv $ checkSpecs specs
    return (M.mapKeys (qualify name') (mtyAbs mty) <> abstypes,
            env <> senv,
            ModSpec name' sig' doc loc : specs')

checkSpecs (IncludeSpec e loc : specs) = do
  (e_abs, e_env, e') <- checkSigExpToEnv e

  mapM_ (warnIfShadowing . fmap baseName) $ M.keys e_abs

  (abstypes, env, specs') <- localEnv e_env $ checkSpecs specs
  return (abstypes <> e_abs,
          env <> e_env,
          IncludeSpec e' loc : specs')
  where warnIfShadowing qn =
          (lookupType loc qn >> warnAbout qn)
          `catchError` \_ -> return ()
        warnAbout qn =
          warn loc $ "Inclusion shadows type " ++ quote (pretty qn) ++ "."

checkSigExp :: SigExpBase NoInfo Name -> TypeM (MTy, SigExpBase Info VName)
checkSigExp (SigParens e loc) = do
  (mty, e') <- checkSigExp e
  return (mty, SigParens e' loc)
checkSigExp (SigVar name loc) = do
  (name', mty) <- lookupMTy loc name
  (mty', _) <- newNamesForMTy mty
  return (mty', SigVar name' loc)
checkSigExp (SigSpecs specs loc) = do
  checkForDuplicateSpecs specs
  (abstypes, env, specs') <- checkSpecs specs
  return (MTy abstypes $ ModEnv env, SigSpecs specs' loc)
checkSigExp (SigWith s (TypeRef tname ps td trloc) loc) = do
  (s_abs, s_env, s') <- checkSigExpToEnv s
  checkTypeParams ps $ \ps' -> do
    (td', _) <- bindingTypeParams ps' $ checkTypeDecl ps' td
    (tname', s_abs', s_env') <- refineEnv loc s_abs s_env tname ps' $ unInfo $ expandedType td'
    return (MTy s_abs' $ ModEnv s_env', SigWith s' (TypeRef tname' ps' td' trloc) loc)
checkSigExp (SigArrow maybe_pname e1 e2 loc) = do
  (MTy s_abs e1_mod, e1') <- checkSigExp e1
  (env_for_e2, maybe_pname') <-
    case maybe_pname of
      Just pname -> bindSpaced [(Term, pname)] $ do
        pname' <- checkName Term pname loc
        return (mempty { envNameMap = M.singleton (Term, pname) $ qualName pname'
                       , envModTable = M.singleton pname' e1_mod
                       },
                Just pname')
      Nothing ->
        return (mempty, Nothing)
  (e2_mod, e2') <- localEnv env_for_e2 $ checkSigExp e2
  return (MTy mempty $ ModFun $ FunSig s_abs e1_mod e2_mod,
          SigArrow maybe_pname' e1' e2' loc)

checkSigExpToEnv :: SigExpBase NoInfo Name -> TypeM (TySet, Env, SigExpBase Info VName)
checkSigExpToEnv e = do
  (MTy abs mod, e') <- checkSigExp e
  case mod of
    ModEnv env -> return (abs, env, e')
    ModFun{}   -> unappliedFunctor $ srclocOf e

checkSigBind :: SigBindBase NoInfo Name -> TypeM (Env, SigBindBase Info VName)
checkSigBind (SigBind name e doc loc) = do
  (env, e') <- checkSigExp e
  bindSpaced [(Signature, name)] $ do
    name' <- checkName Signature name loc
    return (mempty { envSigTable = M.singleton name' env
                   , envNameMap = M.singleton (Signature, name) (qualName name')
                   },
            SigBind name' e' doc loc)

checkOneModExp :: ModExpBase NoInfo Name -> TypeM (MTy, ModExpBase Info VName)
checkOneModExp (ModParens e loc) = do
  (mty, e') <- checkOneModExp e
  return (mty, ModParens e' loc)
checkOneModExp (ModDecs decs loc) = do
  checkForDuplicateDecs decs
  (abstypes, env, decs') <- checkDecs decs
  return (MTy abstypes $ ModEnv env,
          ModDecs decs' loc)
checkOneModExp (ModVar v loc) = do
  (v', env) <- lookupMod loc v
  when (baseName (qualLeaf v') == nameFromString "intrinsics" &&
        baseTag (qualLeaf v') <= maxIntrinsicTag) $
    throwError $ TypeError loc "The 'intrinsics' module may not be used in module expressions."
  return (MTy mempty env, ModVar v' loc)
checkOneModExp (ModImport name NoInfo loc) = do
  (name', env) <- lookupImport loc name
  return (MTy mempty $ ModEnv env,
          ModImport name (Info name') loc)
checkOneModExp (ModApply f e NoInfo NoInfo loc) = do
  (f_mty, f') <- checkOneModExp f
  case mtyMod f_mty of
    ModFun functor -> do
      (e_mty, e') <- checkOneModExp e
      (mty, psubsts, rsubsts) <- applyFunctor loc functor e_mty
      return (mty, ModApply f' e' (Info psubsts) (Info rsubsts) loc)
    _ ->
      throwError $ TypeError loc "Cannot apply non-parametric module."
checkOneModExp (ModAscript me se NoInfo loc) = do
  (me_mod, me') <- checkOneModExp me
  (se_mty, se') <- checkSigExp se
  match_subst <- badOnLeft $ matchMTys me_mod se_mty loc
  return (se_mty, ModAscript me' se' (Info match_subst) loc)
checkOneModExp (ModLambda param maybe_fsig_e body_e loc) =
  withModParam param $ \param' param_abs param_mod -> do
  (maybe_fsig_e', body_e', mty) <- checkModBody (fst <$> maybe_fsig_e) body_e loc
  return (MTy mempty $ ModFun $ FunSig param_abs param_mod mty,
          ModLambda param' maybe_fsig_e' body_e' loc)

checkOneModExpToEnv :: ModExpBase NoInfo Name -> TypeM (TySet, Env, ModExpBase Info VName)
checkOneModExpToEnv e = do
  (MTy abs mod, e') <- checkOneModExp e
  case mod of
    ModEnv env -> return (abs, env, e')
    ModFun{}   -> unappliedFunctor $ srclocOf e

withModParam :: ModParamBase NoInfo Name
             -> (ModParamBase Info VName -> TySet -> Mod -> TypeM a)
             -> TypeM a
withModParam (ModParam pname psig_e NoInfo loc) m = do
  (MTy p_abs p_mod, psig_e') <- checkSigExp psig_e
  bindSpaced [(Term, pname)] $ do
    pname' <- checkName Term pname loc
    let in_body_env = mempty { envModTable = M.singleton pname' p_mod }
    localEnv in_body_env $
      m (ModParam pname' psig_e' (Info $ map qualLeaf $ M.keys p_abs) loc) p_abs p_mod

withModParams :: [ModParamBase NoInfo Name]
              -> ([(ModParamBase Info VName, TySet, Mod)] -> TypeM a)
              -> TypeM a
withModParams [] m = m []
withModParams (p:ps) m =
  withModParam p $ \p' pabs pmod ->
  withModParams ps $ \ps' -> m $ (p',pabs,pmod) : ps'

checkModBody :: Maybe (SigExpBase NoInfo Name)
             -> ModExpBase NoInfo Name
             -> SrcLoc
             -> TypeM (Maybe (SigExp, Info (M.Map VName VName)),
                       ModExp, MTy)
checkModBody maybe_fsig_e body_e loc = do
  (body_mty, body_e') <- checkOneModExp body_e
  case maybe_fsig_e of
    Nothing ->
      return (Nothing, body_e', body_mty)
    Just fsig_e -> do
      (fsig_mty, fsig_e') <- checkSigExp fsig_e
      fsig_subst <- badOnLeft $ matchMTys body_mty fsig_mty loc
      return (Just (fsig_e', Info fsig_subst), body_e', fsig_mty)

applyFunctor :: SrcLoc
             -> FunSig
             -> MTy
             -> TypeM (MTy,
                       M.Map VName VName,
                       M.Map VName VName)
applyFunctor applyloc (FunSig p_abs p_mod body_mty) a_mty = do
  p_subst <- badOnLeft $ matchMTys a_mty (MTy p_abs p_mod) applyloc

  -- Apply type abbreviations from a_mty to body_mty.
  let a_abbrs = mtyTypeAbbrs a_mty
      isSub v = case M.lookup v a_abbrs of
                  Just abbr -> Just $ TypeSub abbr
                  _  -> Just $ DimSub $ NamedDim $ qualName v
      type_subst = M.mapMaybe isSub p_subst
      body_mty' = substituteTypesInMTy type_subst body_mty
  (body_mty'', body_subst) <- newNamesForMTy body_mty'
  return (body_mty'', p_subst, body_subst)

checkModBind :: ModBindBase NoInfo Name -> TypeM (TySet, Env, ModBindBase Info VName)
checkModBind (ModBind name [] maybe_fsig_e e doc loc) = do
  (maybe_fsig_e', e', mty) <- checkModBody (fst <$> maybe_fsig_e) e loc
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    return (mtyAbs mty,
            mempty { envModTable = M.singleton name' $ mtyMod mty
                   , envNameMap = M.singleton (Term, name) $ qualName name'
                   },
            ModBind name' [] maybe_fsig_e' e' doc loc)
checkModBind (ModBind name (p:ps) maybe_fsig_e body_e doc loc) = do
  (params', maybe_fsig_e', body_e', funsig) <-
    withModParam p $ \p' p_abs p_mod ->
    withModParams ps $ \params_stuff -> do
    let (ps', ps_abs, ps_mod) = unzip3 params_stuff
    (maybe_fsig_e', body_e', mty) <- checkModBody (fst <$> maybe_fsig_e) body_e loc
    let addParam (x,y) mty' = MTy mempty $ ModFun $ FunSig x y mty'
    return (p' : ps', maybe_fsig_e', body_e',
            FunSig p_abs p_mod $ foldr addParam mty $ zip ps_abs ps_mod)
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    return (mempty,
            mempty { envModTable =
                       M.singleton name' $ ModFun funsig
                   , envNameMap =
                       M.singleton (Term, name) $ qualName name'
                   },
            ModBind name' params' maybe_fsig_e' body_e' doc loc)

checkForDuplicateSpecs :: [SpecBase NoInfo Name] -> TypeM ()
checkForDuplicateSpecs =
  foldM_ (flip f) mempty
  where check namespace name loc known =
          case M.lookup (namespace, name) known of
            Just loc' ->
              dupDefinitionError namespace name loc loc'
            _ -> return $ M.insert (namespace, name) loc known

        f (ValSpec name _ _ _ loc) =
          check Term name loc

        f (TypeAbbrSpec (TypeBind name _ _ _ loc)) =
          check Type name loc

        f (TypeSpec _ name _ _ loc) =
          check Type name loc

        f (ModSpec name _ _ loc) =
          check Term name loc

        f IncludeSpec{} =
          return

checkTypeBind :: TypeBindBase NoInfo Name
              -> TypeM (Env, TypeBindBase Info VName)
checkTypeBind (TypeBind name tps (TypeDecl t NoInfo) doc loc) =
  checkTypeParams tps $ \tps' -> do
    (td', l) <- bindingTypeParams tps' $ do
      checkForDuplicateNamesInType t
      (t', st, l) <- checkTypeExp t
      checkShapeParamUses typeExpUses tps' [t']
      return (TypeDecl t' $ Info st, l)
    bindSpaced [(Type, name)] $ do
      name' <- checkName Type name loc
      return (mempty { envTypeTable =
                         M.singleton name' $ TypeAbbr l tps' $ unInfo $ expandedType td',
                       envNameMap =
                         M.singleton (Type, name) $ qualName name'
                     },
              TypeBind name' tps' td' doc loc)

checkValBind :: ValBindBase NoInfo Name -> TypeM (Env, ValBind)
checkValBind (ValBind entry fname maybe_tdecl NoInfo tparams params body doc loc) = do
  (fname', tparams', params', maybe_tdecl', rettype, body') <-
    checkFunDef (fname, maybe_tdecl, tparams, params, body, loc)

  when (entry && any isTypeParam tparams') $
    throwError $ TypeError loc "Entry point functions may not be polymorphic."

  let (rettype_params, rettype') = unfoldFunType rettype
  when (entry && (any (not . patternOrderZero) params' ||
                  any (not . orderZero) rettype_params ||
                  not (orderZero rettype'))) $
    throwError $ TypeError loc "Entry point functions may not be higher-order."

  case (entry, filter nastyParameter params') of
    (True, p : _) -> warn loc $ "Entry point parameter\n\n  " <>
                     pretty p <> "\n\nwill have an opaque type, so the entry point will likely not be callable."
    _ -> return ()

  when (entry && nastyReturnType maybe_tdecl' rettype) $
    warn loc $ "Entry point return type\n\n  " <>
    pretty rettype <> "\n\nwill have an opaque type, so the result will likely not be usable."

  return (mempty { envVtable =
                     M.singleton fname' $
                     BoundV tparams' $ foldr (uncurry (Arrow ()) . patternParam) rettype params'
                 , envNameMap =
                     M.singleton (Term, fname) $ qualName fname'
                 },
           ValBind entry fname' maybe_tdecl' (Info rettype) tparams' params' body' doc loc)

nastyType :: Monoid als => TypeBase dim als -> Bool
nastyType Prim{} = False
nastyType t@Array{} = nastyType $ stripArray 1 t
nastyType _ = True

nastyReturnType :: Monoid als => Maybe (TypeExp VName) -> TypeBase dim als -> Bool
nastyReturnType _ (Arrow _ _ t1 t2) =
  nastyType t1 || nastyReturnType Nothing t2
nastyReturnType (Just te) _
  | niceTypeExp te = False
nastyReturnType te t
  | Just ts <- isTupleRecord t =
      case te of
        Just (TETuple tes _) -> or $ zipWith nastyType' (map Just tes) ts
        _ -> any nastyType ts
  | otherwise = nastyType' te t
  where nastyType' (Just te') _ | niceTypeExp te' = False
        nastyType' _ t' = nastyType t'

nastyParameter :: Pattern -> Bool
nastyParameter p = nastyType (patternType p) && not (ascripted p)
  where ascripted (PatternAscription _ (TypeDecl te _) _) = niceTypeExp te
        ascripted (PatternParens p' _) = ascripted p'
        ascripted _ = False

niceTypeExp :: TypeExp VName -> Bool
niceTypeExp (TEVar (QualName [] _) _) = True
niceTypeExp (TEApply te TypeArgExpDim{} _) = niceTypeExp te
niceTypeExp (TEArray te _ _) = niceTypeExp te
niceTypeExp _ = False

checkOneDec :: DecBase NoInfo Name -> TypeM (TySet, Env, DecBase Info VName)
checkOneDec (ModDec struct) = do
  (abs, modenv, struct') <- checkModBind struct
  return (abs, modenv, ModDec struct')

checkOneDec (SigDec sig) = do
  (sigenv, sig') <- checkSigBind sig
  return (mempty, sigenv, SigDec sig')

checkOneDec (TypeDec tdec) = do
  (tenv, tdec') <- checkTypeBind tdec
  return (mempty, tenv, TypeDec tdec')

checkOneDec (OpenDec x loc) = do
  (x_abs, x_env, x') <- checkOneModExpToEnv x
  return (x_abs, x_env, OpenDec x' loc)

checkOneDec (LocalDec d loc) = do
  (abstypes, env, d') <- checkOneDec d
  return (abstypes, env, LocalDec d' loc)

checkOneDec (ImportDec name NoInfo loc) = do
  (name', env) <- lookupImport loc name
  when ("/futlib" `isPrefixOf` name) $
    warn loc $ name ++ " is already implicitly imported."
  return (mempty, env, ImportDec name (Info name') loc)

checkOneDec (ValDec vb) = do
  (env, vb') <- checkValBind vb
  return (mempty, env, ValDec vb')

checkDecs :: [DecBase NoInfo Name] -> TypeM (TySet, Env, [DecBase Info VName])
checkDecs (LocalDec d loc:ds) = do
  (d_abstypes, d_env, d') <- checkOneDec d
  (ds_abstypes, ds_env, ds') <- localEnv d_env $ checkDecs ds
  return (d_abstypes <> ds_abstypes,
          ds_env,
          LocalDec d' loc : ds')

checkDecs (d:ds) = do
  (d_abstypes, d_env, d') <- checkOneDec d
  (ds_abstypes, ds_env, ds') <- localEnv d_env $ checkDecs ds
  return (d_abstypes <> ds_abstypes,
          ds_env <> d_env,
          d' : ds')

checkDecs [] =
  return (mempty, mempty, [])

--- Signature matching

-- Return new renamed/abstracted env, as well as a mapping from
-- names in the signature to names in the new env.  This is used for
-- functor application.  The first env is the module env, and the
-- second the env it must match.
matchMTys :: MTy -> MTy -> SrcLoc
          -> Either TypeError (M.Map VName VName)
matchMTys = matchMTys' mempty
  where
    matchMTys' :: TypeSubs -> MTy -> MTy -> SrcLoc
               -> Either TypeError (M.Map VName VName)

    matchMTys' _ (MTy _ ModFun{}) (MTy _ ModEnv{}) loc =
      Left $ TypeError loc "Cannot match parametric module with non-paramatric module type."

    matchMTys' _ (MTy _ ModEnv{}) (MTy _ ModFun{}) loc =
      Left $ TypeError loc "Cannot match non-parametric module with paramatric module type."

    matchMTys' old_abs_subst_to_type (MTy mod_abs mod) (MTy sig_abs sig) loc = do
      -- Check that abstract types in 'sig' have an implementation in
      -- 'mod'.  This also gives us a substitution that we use to check
      -- the types of values.
      abs_substs <- resolveAbsTypes mod_abs mod sig_abs loc

      let abs_subst_to_type = old_abs_subst_to_type <>
                              M.map (TypeSub . snd) abs_substs
          abs_name_substs   = M.map (qualLeaf . fst) abs_substs
      substs <- matchMods abs_subst_to_type mod sig loc
      return (substs <> abs_name_substs)

    matchMods :: TypeSubs -> Mod -> Mod -> SrcLoc
              -> Either TypeError (M.Map VName VName)
    matchMods _ ModEnv{} ModFun{} loc =
      Left $ TypeError loc "Cannot match non-parametric module with paramatric module type."
    matchMods _ ModFun{} ModEnv{} loc =
      Left $ TypeError loc "Cannot match parametric module with non-paramatric module type."

    matchMods abs_subst_to_type (ModEnv mod) (ModEnv sig) loc =
      matchEnvs abs_subst_to_type mod sig loc

    matchMods old_abs_subst_to_type
              (ModFun (FunSig mod_abs mod_pmod mod_mod))
              (ModFun (FunSig sig_abs sig_pmod sig_mod))
              loc = do
      abs_substs <- resolveAbsTypes mod_abs mod_pmod sig_abs loc
      let abs_subst_to_type = old_abs_subst_to_type <>
                              M.map (TypeSub . snd) abs_substs
          abs_name_substs   = M.map (qualLeaf . fst) abs_substs
      pmod_substs <- matchMods abs_subst_to_type mod_pmod sig_pmod loc
      mod_substs <- matchMTys' abs_subst_to_type mod_mod sig_mod loc
      return (pmod_substs <> mod_substs <> abs_name_substs)

    matchEnvs :: TypeSubs
              -> Env -> Env -> SrcLoc
              -> Either TypeError (M.Map VName VName)
    matchEnvs abs_subst_to_type env sig loc = do
      -- XXX: we only want to create substitutions for visible names.
      -- This must be wrong in some cases.  Probably we need to
      -- rethink how we do shadowing for module types.
      let visible = S.fromList $ map qualLeaf $ M.elems $ envNameMap sig
          isVisible name = name `S.member` visible

      -- Check that all values are defined correctly, substituting the
      -- abstract types first.
      val_substs <- fmap M.fromList $ forM (M.toList $ envVtable sig) $ \(name, spec_bv) -> do
        let spec_bv' = substituteTypesInBoundV abs_subst_to_type spec_bv
        case findBinding envVtable Term (baseName name) env of
          Just (name', bv) -> matchVal loc name spec_bv' name' bv
          _ -> missingVal loc (baseName name)

      -- Check that all type abbreviations are correctly defined.
      abbr_name_substs <- fmap M.fromList $
                          forM (filter (isVisible . fst) $ M.toList $
                                envTypeTable sig) $ \(name, TypeAbbr _ spec_ps spec_t) ->
        case findBinding envTypeTable Type (baseName name) env of
          Just (name', TypeAbbr _ ps t) ->
            matchTypeAbbr loc abs_subst_to_type val_substs name spec_ps spec_t name' ps t
          Nothing -> missingType loc $ baseName name

      -- Check for correct modules.
      mod_substs <- fmap M.unions $ forM (M.toList $ envModTable sig) $ \(name, modspec) ->
        case findBinding envModTable Term (baseName name) env of
          Just (name', mod) ->
            M.insert name name' <$> matchMods abs_subst_to_type mod modspec loc
          Nothing ->
            missingMod loc $ baseName name

      return $ val_substs <> mod_substs <> abbr_name_substs

    matchTypeAbbr :: SrcLoc -> TypeSubs -> M.Map VName VName
                  -> VName -> [TypeParam] -> StructType
                  -> VName -> [TypeParam] -> StructType
                  -> Either TypeError (VName, VName)
    matchTypeAbbr loc abs_subst_to_type val_substs spec_name spec_ps spec_t name ps t = do
      -- We have to create substitutions for the type parameters, too.
      unless (length spec_ps == length ps) nomatch
      param_substs <- mconcat <$> zipWithM matchTypeParam spec_ps ps
      let val_substs' = M.map (DimSub . NamedDim . qualName) val_substs
          spec_t' = substituteTypes (val_substs'<>param_substs<>abs_subst_to_type) spec_t
      if spec_t' == t
        then return (spec_name, name)
        else nomatch
        where nomatch = mismatchedType loc (M.keys abs_subst_to_type)
                        (baseName spec_name) (spec_ps, spec_t) (ps, t)

              matchTypeParam (TypeParamDim x _) (TypeParamDim y _) =
                pure $ M.singleton x $ DimSub $ NamedDim $ qualName y
              matchTypeParam (TypeParamType Unlifted x _) (TypeParamType Unlifted y _) =
                pure $ M.singleton x $ TypeSub $ TypeAbbr Unlifted [] $
                TypeVar () Nonunique (typeName y) []
              matchTypeParam (TypeParamType _ x _) (TypeParamType Lifted y _) =
                pure $ M.singleton x $ TypeSub $ TypeAbbr Lifted [] $
                TypeVar () Nonunique (typeName y) []
              matchTypeParam _ _ =
                nomatch

    matchVal :: SrcLoc
             -> VName -> BoundV
             -> VName -> BoundV
             -> Either TypeError (VName, VName)
    matchVal loc spec_name spec_t name t
      | matchFunBinding loc spec_t t = return (spec_name, name)
    matchVal loc spec_name spec_v _ v =
      Left $ TypeError loc $ unlines $
      ["Module type specifies"] ++
      map ("  "++) (lines $ ppValBind spec_name spec_v) ++
      ["but module provides"] ++
      map ("  "++) (lines $ppValBind spec_name v)

    matchFunBinding :: SrcLoc -> BoundV -> BoundV -> Bool
    matchFunBinding loc (BoundV _ orig_spec_t) (BoundV tps orig_t) =
      -- Would be nice if we could propagate the actual error here.
      case doUnification loc tps
           (toStructural orig_spec_t) (toStructural orig_t) of
        Left _ -> False
        Right t -> t `subtypeOf` toStructural orig_spec_t

    missingType loc name =
      Left $ TypeError loc $
      "Module does not define a type named " ++ pretty name ++ "."

    missingVal loc name =
      Left $ TypeError loc $
      "Module does not define a value named " ++ pretty name ++ "."

    missingMod loc name =
      Left $ TypeError loc $
      "Module does not define a module named " ++ pretty name ++ "."

    mismatchedType loc abs name spec_t env_t =
      Left $ TypeError loc $
      unlines ["Module defines",
               indent $ ppTypeAbbr abs name env_t,
               "but module type requires",
               indent $ ppTypeAbbr abs name spec_t]

    indent = intercalate "\n" . map ("  "++) . lines

    resolveAbsTypes :: TySet -> Mod -> TySet -> SrcLoc
                    -> Either TypeError (M.Map VName (QualName VName, TypeBinding))
    resolveAbsTypes mod_abs mod sig_abs loc = do
      let abs_mapping = M.fromList $ zip
                        (map (fmap baseName . fst) $ M.toList mod_abs) (M.toList mod_abs)
      fmap M.fromList $ forM (M.toList sig_abs) $ \(name, name_l) ->
        case findTypeDef (fmap baseName name) mod of
          Just (name', TypeAbbr mod_l ps t)
            | Unlifted <- name_l,
              not (orderZero t) || mod_l == Lifted ->
                mismatchedLiftedness loc (map qualLeaf $ M.keys mod_abs) name (ps, t)
            | Just (abs_name, _) <- M.lookup (fmap baseName name) abs_mapping ->
                return (qualLeaf name, (abs_name, TypeAbbr name_l ps t))
            | otherwise ->
                return (qualLeaf name, (name', TypeAbbr name_l ps t))
          _ ->
            missingType loc $ fmap baseName name

    mismatchedLiftedness loc abs name mod_t =
      Left $ TypeError loc $
      unlines ["Module defines",
               indent $ ppTypeAbbr abs name mod_t,
               "but module type requires this type to be non-functional."]

    ppValBind v (BoundV tps t) =
      unwords $ ["val", prettyName v] ++ map pretty tps ++ [":", pretty t]

    ppTypeAbbr abs name (ps, t) =
      "type " ++ unwords (pretty name : map pretty ps) ++ t'
      where t' = case t of
                   TypeVar () _ tn args
                     | typeLeaf tn `elem` abs,
                       map typeParamToArg ps == args -> ""
                   _ -> " = " ++ pretty t

findBinding :: (Env -> M.Map VName v)
            -> Namespace -> Name
            -> Env
            -> Maybe (VName, v)
findBinding table namespace name the_env = do
  QualName _ name' <- M.lookup (namespace, name) $ envNameMap the_env
  (name',) <$> M.lookup name' (table the_env)

findTypeDef :: QualName Name -> Mod -> Maybe (QualName VName, TypeBinding)
findTypeDef _ ModFun{} = Nothing
findTypeDef (QualName [] name) (ModEnv the_env) = do
  (name', tb) <- findBinding envTypeTable Type name the_env
  return (qualName name', tb)
findTypeDef (QualName (q:qs) name) (ModEnv the_env) = do
  (q', q_mod) <- findBinding envModTable Term q the_env
  (QualName qs' name', tb) <- findTypeDef (QualName qs name) q_mod
  return (QualName (q':qs') name', tb)

typeParamToArg :: TypeParam -> StructTypeArg
typeParamToArg (TypeParamDim v ploc) =
  TypeArgDim (NamedDim $ qualName v) ploc
typeParamToArg (TypeParamType _ v ploc) =
  TypeArgType (TypeVar () Nonunique (typeName v) []) ploc

substituteTypesInMod :: TypeSubs -> Mod -> Mod
substituteTypesInMod substs (ModEnv e) =
  ModEnv $ substituteTypesInEnv substs e
substituteTypesInMod substs (ModFun (FunSig abs mod mty)) =
  ModFun $ FunSig abs (substituteTypesInMod substs mod) (substituteTypesInMTy substs mty)

substituteTypesInMTy :: TypeSubs -> MTy -> MTy
substituteTypesInMTy substs (MTy abs mod) = MTy abs $ substituteTypesInMod substs mod

substituteTypesInEnv :: TypeSubs -> Env -> Env
substituteTypesInEnv substs env =
  env { envVtable    = M.map (substituteTypesInBoundV substs) $ envVtable env
      , envTypeTable = M.mapWithKey subT $ envTypeTable env
      , envModTable  = M.map (substituteTypesInMod substs) $ envModTable env
      }
  where subT name _
          | Just (TypeSub (TypeAbbr l ps t)) <- M.lookup name substs = TypeAbbr l ps t
        subT _ (TypeAbbr l ps t) = TypeAbbr l ps $ substituteTypes substs t

substituteTypesInBoundV :: TypeSubs -> BoundV -> BoundV
substituteTypesInBoundV substs (BoundV tps t) =
  BoundV tps (substituteTypes substs t)

allNamesInMTy :: MTy -> S.Set VName
allNamesInMTy (MTy abs mod) =
  S.fromList (map qualLeaf $ M.keys abs) <> allNamesInMod mod

allNamesInMod :: Mod -> S.Set VName
allNamesInMod (ModEnv env) = allNamesInEnv env
allNamesInMod ModFun{} = mempty

-- All names defined anywhere in the env.
allNamesInEnv :: Env -> S.Set VName
allNamesInEnv (Env vtable ttable stable modtable _names) =
  S.fromList (M.keys vtable ++ M.keys ttable ++
              M.keys stable ++ M.keys modtable) <>
  mconcat (map allNamesInMTy (M.elems stable) ++
           map allNamesInMod (M.elems modtable) ++
           map allNamesInType (M.elems ttable))
  where allNamesInType (TypeAbbr _ ps _) = S.fromList $ map typeParamName ps

newNamesForMTy :: MTy -> TypeM (MTy, M.Map VName VName)
newNamesForMTy orig_mty = do
  -- Create unique renames for the module type.
  pairs <- forM (S.toList $ allNamesInMTy orig_mty) $ \v -> do
    v' <- newName v
    return (v, v')
  let substs = M.fromList pairs
      rev_substs = M.fromList $ map (uncurry $ flip (,)) pairs

  return (substituteInMTy substs orig_mty, rev_substs)

  where
    substituteInMTy :: M.Map VName VName -> MTy -> MTy
    substituteInMTy substs (MTy mty_abs mty_mod) =
      MTy (M.mapKeys (fmap substitute) mty_abs) (substituteInMod mty_mod)
      where
        substituteInEnv (Env vtable ttable _stable modtable names) =
          let vtable' = substituteInMap substituteInBinding vtable
              ttable' = substituteInMap substituteInTypeBinding ttable
              mtable' = substituteInMap substituteInMod modtable
          in Env { envVtable = vtable'
                 , envTypeTable = ttable'
                 , envSigTable = mempty
                 , envModTable = mtable'
                 , envNameMap = M.map (fmap substitute) names
                 }

        substitute v =
          fromMaybe v $ M.lookup v substs

        substituteInMap f m =
          let (ks, vs) = unzip $ M.toList m
          in M.fromList $
             zip (map (\k -> fromMaybe k $ M.lookup k substs) ks)
                 (map f vs)

        substituteInBinding (BoundV ps t) =
          BoundV (map substituteInTypeParam ps) (substituteInType t)

        substituteInMod (ModEnv env) =
          ModEnv $ substituteInEnv env
        substituteInMod (ModFun funsig) =
          ModFun $ substituteInFunSig funsig

        substituteInFunSig (FunSig abs mod mty) =
          FunSig (M.mapKeys (fmap substitute) abs)
          (substituteInMod mod) (substituteInMTy substs mty)

        substituteInTypeBinding (TypeAbbr l ps t) =
          TypeAbbr l (map substituteInTypeParam ps) $ substituteInType t

        substituteInTypeParam (TypeParamDim p loc) =
          TypeParamDim (substitute p) loc
        substituteInTypeParam (TypeParamType l p loc) =
          TypeParamType l (substitute p) loc

        substituteInType :: StructType -> StructType
        substituteInType (TypeVar () u (TypeName qs v) targs) =
          TypeVar () u (TypeName (map substitute qs) $ substitute v) $ map substituteInTypeArg targs
        substituteInType (Prim t) =
          Prim t
        substituteInType (Record ts) =
          Record $ fmap substituteInType ts
        substituteInType (Enum cs) =
          Enum cs
        substituteInType (Array () u (ArrayPrimElem t) shape) =
          Array () u (ArrayPrimElem t) (substituteInShape shape)
        substituteInType (Array () u (ArrayPolyElem (TypeName qs v) targs) shape) =
          Array () u (ArrayPolyElem
                      (TypeName (map substitute qs) $ substitute v)
                      (map substituteInTypeArg targs))
                     (substituteInShape shape)
        substituteInType (Array () u (ArrayRecordElem ts) shape) =
          let ts' = fmap (substituteInType . recordArrayElemToType) ts
          in case arrayOf (Record ts') (substituteInShape shape) u of
            Just t' -> t'
            _ -> error "substituteInType: Cannot create array after substitution."
        substituteInType (Array () u (ArrayEnumElem cs) shape) =
          Array () u (ArrayEnumElem cs) (substituteInShape shape)
        substituteInType (Arrow als v t1 t2) =
          Arrow als v (substituteInType t1) (substituteInType t2)

        substituteInShape (ShapeDecl ds) =
          ShapeDecl $ map substituteInDim ds
        substituteInDim (NamedDim (QualName qs v)) =
          NamedDim $ QualName (map substitute qs) $ substitute v
        substituteInDim d = d

        substituteInTypeArg (TypeArgDim (NamedDim (QualName qs v)) loc) =
          TypeArgDim (NamedDim $ QualName (map substitute qs) $ substitute v) loc
        substituteInTypeArg (TypeArgDim (ConstDim x) loc) =
          TypeArgDim (ConstDim x) loc
        substituteInTypeArg (TypeArgDim AnyDim loc) =
          TypeArgDim AnyDim loc
        substituteInTypeArg (TypeArgType t loc) =
          TypeArgType (substituteInType t) loc

mtyTypeAbbrs :: MTy -> M.Map VName TypeBinding
mtyTypeAbbrs (MTy _ mod) = modTypeAbbrs mod

modTypeAbbrs :: Mod -> M.Map VName TypeBinding
modTypeAbbrs (ModEnv env) =
  envTypeAbbrs env
modTypeAbbrs (ModFun (FunSig _ mod mty)) =
  modTypeAbbrs mod <> mtyTypeAbbrs mty

envTypeAbbrs :: Env -> M.Map VName TypeBinding
envTypeAbbrs env =
  envTypeTable env <>
  (mconcat . map modTypeAbbrs . M.elems . envModTable) env

-- | Refine the given type name in the given env.
refineEnv :: SrcLoc -> TySet -> Env -> QualName Name -> [TypeParam] -> StructType
          -> TypeM (QualName VName, TySet, Env)
refineEnv loc tset env tname ps t
  | Just (tname', TypeAbbr l cur_ps (TypeVar () _ (TypeName qs v) _)) <-
      findTypeDef tname (ModEnv env),
    QualName (qualQuals tname') v `M.member` tset =
      if paramsMatch cur_ps ps then
        return (tname',
                QualName qs v `M.delete` tset,
                substituteTypesInEnv
                (M.fromList [(qualLeaf tname',
                              TypeSub $ TypeAbbr l cur_ps t),
                              (v, TypeSub $ TypeAbbr l ps t)])
                env)
      else throwError $ TypeError loc $ "Cannot refine a type having " <>
           tpMsg ps <> " with a type having " <> tpMsg cur_ps <> "."
  | otherwise =
      throwError $ TypeError loc $
      pretty tname ++ " is not an abstract type in the module type."
  where tpMsg [] = "no type parameters"
        tpMsg xs = "type parameters " <> unwords (map pretty xs)

paramsMatch :: [TypeParam] -> [TypeParam] -> Bool
paramsMatch ps1 ps2 = length ps1 == length ps2 && all match (zip ps1 ps2)
  where match (TypeParamType l1 _ _, TypeParamType l2 _ _) = l1 <= l2
        match (TypeParamDim _ _, TypeParamDim _ _) = True
        match _ = False
