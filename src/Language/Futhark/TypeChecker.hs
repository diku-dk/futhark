{-# LANGUAGE FlexibleContexts, TupleSections #-}
-- | The type checker checks whether the program is type-consistent
-- and adds type annotations and various other elaborations.  The
-- program does not need to have any particular properties for the
-- type checker to function; in particular it does not need unique
-- names.
module Language.Futhark.TypeChecker
  ( checkProg
  , TypeError
  , Warnings
  , FileModule(..)
  , Imports
  )
  where

import Control.Applicative
import Control.Monad.Except hiding (mapM)
import Control.Monad.Writer hiding (mapM)
import Data.List
import Data.Loc
import Data.Maybe
import Data.Either
import Data.Ord
import Data.Traversable (mapM)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Prelude hiding (abs, mod, mapM)

import Language.Futhark
import Futhark.FreshNames hiding (newName)
import Language.Futhark.TypeChecker.Monad
import Language.Futhark.TypeChecker.Terms
import Language.Futhark.TypeChecker.Types

--- The main checker

-- | The result of type checking some file.  Can be passed to further
-- invocations of the type checker.
data FileModule = FileModule { fileEnv :: Env
                             , fileProg :: Prog
                             }

-- | A mapping from import names to imports.  The ordering is significant.
type Imports = [(String, FileModule)]

-- | Type check a program containing no type information, yielding
-- either a type error or a program with complete type information.
-- Accepts a mapping from file names (excluding extension) to
-- previously type checker results.  The 'FilePath' is used to resolve
-- relative @import@s.
checkProg :: Bool
          -> Imports
          -> VNameSource
          -> FilePath
          -> UncheckedProg
          -> Either TypeError (FileModule, Warnings, VNameSource)
checkProg permit_recursion files src fpath prog =
  runTypeM permit_recursion initialEnv files' fpath src $ checkProgM prog
  where files' = M.map fileEnv $ M.fromList files

initialEnv :: Env
initialEnv = intrinsicsModule
               { envModTable = initialModTable
               , envNameMap = M.insert
                              (Term, nameFromString "intrinsics")
                              intrinsics_v
                              topLevelNameMap
               }
  where initialTypeTable = M.fromList $ mapMaybe addIntrinsicT $ M.toList intrinsics
        initialModTable = M.singleton intrinsics_v (ModEnv intrinsicsModule)

        intrinsics_v = VName (nameFromString "intrinsics") 0

        intrinsicsModule = Env mempty initialTypeTable mempty mempty intrinsicsNameMap

        addIntrinsicT (name, IntrinsicType t) =
          Just (name, TypeAbbr [] $ Prim t)
        addIntrinsicT _ =
          Nothing

checkProgM :: UncheckedProg -> TypeM FileModule
checkProgM (Prog doc decs) = do
  checkForDuplicateDecs decs
  (_, env, decs') <- checkDecs decs
  return (FileModule env $ Prog doc decs')

checkForDuplicateDecs :: [DecBase NoInfo Name] -> TypeM ()
checkForDuplicateDecs =
  foldM_ (flip f) mempty
  where check namespace name loc known =
          case M.lookup (namespace, name) known of
            Just loc' ->
              bad $ DupDefinitionError namespace name loc loc'
            _ -> return $ M.insert (namespace, name) loc known

        f (FunDec (FunBind _ name _ _ _ _ _ _ loc)) =
          check Term name loc

        f (ValDec (ValBind _ name _ _ _ _ loc)) =
          check Term name loc

        f (TypeDec (TypeBind name _ _ _ loc)) =
          check Type name loc

        f (SigDec (SigBind name _ _ loc)) =
          check Signature name loc

        f (ModDec (ModBind name _ _ _ _ loc)) =
          check Term name loc

        f OpenDec{} = return

        f LocalDec{} = return

bindingTypeParams :: [TypeParam] -> TypeM a -> TypeM a
bindingTypeParams tparams = localEnv env
  where env = mconcat $ map typeParamEnv tparams

        typeParamEnv (TypeParamDim v _) =
          mempty { envVtable =
                     M.singleton v $ BoundV (Prim (Signed Int32)) }
        typeParamEnv (TypeParamType v _) =
          mempty { envTypeTable =
                     M.singleton v $ TypeAbbr [] $ TypeVar (typeName v) [] }

checkSpecs :: [SpecBase NoInfo Name] -> TypeM (TySet, Env, [SpecBase Info VName])

checkSpecs [] = return (mempty, mempty, [])

checkSpecs (ValSpec name tparams params rettype doc loc : specs) =
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    (tparams', params', rettype') <-
      checkTypeParams tparams $ \tparams' -> bindingTypeParams tparams' $
      checkParams params $ \params' -> do
        rettype' <- checkTypeDecl rettype
        return (tparams', params', rettype')
    let paramtypes = map (unInfo . expandedType . paramType) params'
        rettype'' = unInfo $ expandedType rettype'
        valenv =
          mempty { envVtable = M.singleton name' $
                               if null params'
                               then BoundV rettype''
                               else BoundF (tparams',
                                            zip (map paramName params') paramtypes,
                                            rettype'')
                 , envNameMap = M.singleton (Term, name) name'
                 }
    (abstypes, env, specs') <- localEnv valenv $ checkSpecs specs
    return (abstypes,
            env <> valenv,
            ValSpec name' tparams' params' rettype' doc loc : specs')

checkSpecs (TypeAbbrSpec tdec : specs) =
  bindSpaced [(Type, typeAlias tdec)] $ do
    (tenv, tdec') <- checkTypeBind tdec
    (abstypes, env, specs') <- localEnv tenv $ checkSpecs specs
    return (abstypes,
            tenv <> env,
            TypeAbbrSpec tdec' : specs')

checkSpecs (TypeSpec name ps doc loc : specs) =
  checkTypeParams ps $ \ps' ->
  bindSpaced [(Type, name)] $ do
    name' <- checkName Type name loc
    abs_name <- newName name'
    let tenv = mempty
               { envNameMap =
                   M.singleton (Type, name) name'
               , envTypeTable =
                   M.singleton name' $ TypeAbbr ps' $ TypeVar (typeName abs_name) $ map paramToArg ps'
               }
    (abstypes, env, specs') <- localEnv tenv $ checkSpecs specs
    return (S.insert (qualName abs_name) abstypes,
            tenv <> env,
            TypeSpec name' ps' doc loc : specs')
      where paramToArg (TypeParamDim v ploc) =
              TypeArgDim (NamedDim $ qualName v) ploc
            paramToArg (TypeParamType v ploc) =
              TypeArgType (TypeVar (typeName v) []) ploc

checkSpecs (ModSpec name sig loc : specs) =
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    (mty, sig') <- checkSigExp sig
    let senv = mempty { envNameMap = M.singleton (Term, name) name'
                      , envModTable = M.singleton name' $ mtyMod mty
                      }
    (abstypes, env, specs') <- localEnv senv $ checkSpecs specs
    return (S.map (qualify name) (mtyAbs mty) <> abstypes,
            senv <> env,
            ModSpec name' sig' loc : specs')

checkSpecs (IncludeSpec e loc : specs) = do
  (e_abs, e_env, e') <- checkSigExpToEnv e

  (abstypes, env, specs') <- localEnv e_env $ checkSpecs specs
  return (e_abs <> abstypes,
          e_env <> env,
          IncludeSpec e' loc : specs')

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
checkSigExp (SigWith s (TypeRef tname td) loc) = do
  (s_abs, s_env, s') <- checkSigExpToEnv s
  td' <- checkTypeDecl td
  (tname', s_abs', s_env') <- refineEnv loc s_abs s_env tname $ unInfo $ expandedType td'
  return (MTy s_abs' $ ModEnv s_env', SigWith s' (TypeRef tname' td') loc)
checkSigExp (SigArrow maybe_pname e1 e2 loc) = do
  (MTy s_abs e1_mod, e1') <- checkSigExp e1
  (env_for_e2, maybe_pname') <-
    case maybe_pname of
      Just pname -> bindSpaced [(Term, pname)] $ do
        pname' <- checkName Term pname loc
        return (mempty { envNameMap = M.singleton (Term, pname) pname'
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
    ModFun{}   -> bad $ UnappliedFunctor $ srclocOf e

checkSigBind :: SigBindBase NoInfo Name -> TypeM (Env, SigBindBase Info VName)
checkSigBind (SigBind name e doc loc) = do
  (env, e') <- checkSigExp e
  bindSpaced [(Signature, name)] $ do
    name' <- checkName Signature name loc
    -- As a small convenience(?), binding a signature also implicitly
    -- binds a structure of the same name, which contains the type
    -- abbreviations in the signature.
    let sigmod = typeAbbrEnvFromSig env
    return (mempty { envSigTable = M.singleton name' env
                   , envModTable = M.singleton name' $ ModEnv sigmod
                   , envNameMap = M.fromList [((Signature, name), name'),
                                               ((Term, name), name')]
                   },
            SigBind name' e' doc loc)
  where typeAbbrEnvFromSig (MTy _ (ModEnv env)) =
          let types = envTypeAbbrs env
              names = M.fromList $ map nameMapping $ M.toList types
          in mempty { envNameMap = names
                    , envTypeTable = types }
        typeAbbrEnvFromSig _ = mempty
        nameMapping (v, _) = ((Type, baseName v), v)

checkModExp :: ModExpBase NoInfo Name -> TypeM (MTy, ModExpBase Info VName)
checkModExp (ModParens e loc) = do
  (mty, e') <- checkModExp e
  return (mty, ModParens e' loc)
checkModExp (ModDecs decs loc) = do
  checkForDuplicateDecs decs
  (abstypes, env, decs') <- checkDecs decs
  return (MTy abstypes $ ModEnv env,
          ModDecs decs' loc)
checkModExp (ModVar v loc) = do
  (v', env) <- lookupMod loc v
  when (baseName (qualLeaf v') == nameFromString "intrinsics" &&
        baseTag (qualLeaf v') <= maxIntrinsicTag) $
    bad $ TypeError loc "The 'intrinsics' module may not be used in module expressions."
  return (MTy mempty env, ModVar v' loc)
checkModExp (ModImport name loc) = do
  env <- lookupImport loc name
  return (MTy mempty $ ModEnv env, ModImport name loc)
checkModExp (ModApply f e NoInfo NoInfo loc) = do
  (f_mty, f') <- checkModExp f
  case mtyMod f_mty of
    ModFun functor -> do
      (e_mty, e') <- checkModExp e
      (mty, psubsts, rsubsts) <- applyFunctor loc functor e_mty
      return (mty, ModApply f' e' (Info psubsts) (Info rsubsts) loc)
    _ ->
      bad $ TypeError loc "Cannot apply non-parametric module."
checkModExp (ModAscript me se NoInfo loc) = do
  (me_mod, me') <- checkModExp me
  (se_mty, se') <- checkSigExp se
  match_subst <- badOnLeft $ matchMTys me_mod se_mty loc
  return (se_mty, ModAscript me' se' (Info match_subst) loc)
checkModExp (ModLambda param maybe_fsig_e body_e loc) =
  withModParam param $ \param' param_abs param_mod -> do
  (maybe_fsig_e', body_e', mty) <- checkModBody (fst <$> maybe_fsig_e) body_e loc
  return (MTy mempty $ ModFun $ FunSig param_abs param_mod mty,
          ModLambda param' maybe_fsig_e' body_e' loc)

checkModExpToEnv :: ModExpBase NoInfo Name -> TypeM (TySet, Env, ModExpBase Info VName)
checkModExpToEnv e = do
  (MTy abs mod, e') <- checkModExp e
  case mod of
    ModEnv env -> return (abs, env, e')
    ModFun{}   -> bad $ UnappliedFunctor $ srclocOf e

withModParam :: ModParamBase NoInfo Name
             -> (ModParamBase Info VName -> TySet -> Mod -> TypeM a)
             -> TypeM a
withModParam (ModParam pname psig_e loc) m = do
  (MTy p_abs p_mod, psig_e') <- checkSigExp psig_e
  bindSpaced [(Term, pname)] $ do
    pname' <- checkName Term pname loc
    let in_body_env = mempty { envModTable = M.singleton pname' p_mod }
    localEnv in_body_env $ m (ModParam pname' psig_e' loc) p_abs p_mod

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
  (body_mty, body_e') <- checkModExp body_e
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
  let type_subst = M.mapMaybe (fmap TypeSub . (`M.lookup` a_abbrs)) p_subst
  let body_mty' = substituteTypesInMTy type_subst body_mty
  (body_mty'', body_subst) <- newNamesForMTy body_mty'
  return (body_mty'', p_subst, body_subst)

checkModBind :: ModBindBase NoInfo Name -> TypeM (Env, ModBindBase Info VName)
checkModBind (ModBind name [] maybe_fsig_e e doc loc) = do
  (maybe_fsig_e', e', mty) <- checkModBody (fst <$> maybe_fsig_e) e loc
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    return (mempty { envModTable = M.singleton name' $ mtyMod mty
                   , envNameMap = M.singleton (Term, name) name'
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
    return (mempty { envModTable =
                       M.singleton name' $ ModFun funsig
                   , envNameMap =
                       M.singleton (Term, name) name'
                   },
            ModBind name' params' maybe_fsig_e' body_e' doc loc)


checkForDuplicateSpecs :: [SpecBase NoInfo Name] -> TypeM ()
checkForDuplicateSpecs =
  foldM_ (flip f) mempty
  where check namespace name loc known =
          case M.lookup (namespace, name) known of
            Just loc' ->
              bad $ DupDefinitionError namespace name loc loc'
            _ -> return $ M.insert (namespace, name) loc known

        f (ValSpec name _ _ _ _ loc) =
          check Term name loc

        f (TypeAbbrSpec (TypeBind name _ _ _ loc)) =
          check Type name loc

        f (TypeSpec name _ _ loc) =
          check Type name loc

        f (ModSpec name _ loc) =
          check Term name loc

        f IncludeSpec{} =
          return

checkTypeBind :: TypeBindBase NoInfo Name
              -> TypeM (Env, TypeBindBase Info VName)
checkTypeBind (TypeBind name ps td doc loc) =
  checkTypeParams ps $ \ps' -> do
    td' <- bindingTypeParams ps' $ checkTypeDecl td
    bindSpaced [(Type, name)] $ do
      name' <- checkName Type name loc
      return (mempty { envTypeTable =
                         M.singleton name' $ TypeAbbr ps' $ unInfo $ expandedType td',
                       envNameMap =
                         M.singleton (Type, name) name'
                     },
              TypeBind name' ps' td' doc loc)

checkValBind :: ValBindBase NoInfo Name -> TypeM (Env, ValBind)
checkValBind (ValBind entry name maybe_t NoInfo e doc loc) = do
  name' <- bindSpaced [(Term, name)] $ checkName Term name loc
  (maybe_t', e') <- case maybe_t of
    Just t  -> do
      (tdecl, tdecl_type) <- checkTypeExp t

      let t_structural = toStructural tdecl_type
      when (anythingUnique t_structural) $
        bad $ UniqueConstType loc name t_structural
      e' <- require [t_structural] =<< checkOneExp e
      return (Just tdecl, e')
    Nothing -> do
      e' <- checkOneExp e
      return (Nothing, e')
  let e_t = vacuousShapeAnnotations $ toStructural $ typeOf e'
  return (mempty { envVtable =
                     M.singleton name' (BoundV e_t)
                 , envNameMap =
                     M.singleton (Term, name) name'
                 },
          ValBind entry name' maybe_t' (Info e_t) e' doc loc)
  where anythingUnique (Record fs) = any anythingUnique fs
        anythingUnique et          = unique et

checkFunBind :: FunBindBase NoInfo Name -> TypeM (Env, FunBind)
checkFunBind (FunBind entry fname maybe_retdecl NoInfo tparams params body doc loc) = do
  (fname', tparams', params', maybe_retdecl', rettype, body') <-
    bindSpaced [(Term, fname)] $
    checkFunDef (fname, maybe_retdecl, tparams, params, body, loc)

  when (entry && any isTypeParam tparams) $
    throwError $ TypeError loc "Entry point functions may not be polymorphic."

  return (mempty { envVtable =
                     M.singleton fname'
                     (BoundF (tparams', map patternParam params', rettype))
                 , envNameMap =
                     M.singleton (Term, fname) fname'
                 },
           FunBind entry fname' maybe_retdecl' (Info rettype) tparams' params' body' doc loc)

  where isTypeParam TypeParamType{} = True
        isTypeParam _ = False

checkDec :: DecBase NoInfo Name -> TypeM (TySet, Env, DecBase Info VName)
checkDec (ModDec struct) = do
  (modenv, struct') <- checkModBind struct
  return (mempty, modenv,
          ModDec struct')

checkDec (SigDec sig) = do
  (sigenv, sig') <- checkSigBind sig
  return (mempty, sigenv,
          SigDec sig')

checkDec (TypeDec tdec) = do
  (tenv, tdec') <- checkTypeBind tdec
  return (mempty, tenv,
          TypeDec tdec')

checkDec (OpenDec x xs NoInfo loc) = do
  (x_abs, x_env, x') <- checkModExpToEnv x
  (xs_abs, xs_envs, xs') <- unzip3 <$> mapM checkModExpToEnv xs
   -- We cannot use mconcat, as mconcat is a right-fold.
  let env_ext = foldl (flip mappend) x_env xs_envs
      names = S.toList $ S.unions $ map allNamesInEnv $ x_env:xs_envs
  return (x_abs <> mconcat xs_abs,
          env_ext,
          OpenDec x' xs' (Info names) loc)

checkDec (LocalDec d loc) = do
  (abstypes, env, d') <- checkDec d
  return (abstypes, env, LocalDec d' loc)

checkDec (ValDec vb) = do
  (env, vb') <- checkValBind vb
  return (mempty, env, ValDec vb')

checkDec (FunDec fb) = do
  (env, fb') <- checkFunBind fb
  return (mempty, env, FunDec fb')

checkDecs :: [DecBase NoInfo Name] -> TypeM (TySet, Env, [DecBase Info VName])
checkDecs (LocalDec d loc:ds) = do
  (d_abstypes, d_env, d') <- checkDec d
  (ds_abstypes, ds_env, ds') <- localEnv d_env $ checkDecs ds
  return (d_abstypes <> ds_abstypes,
          ds_env,
          LocalDec d' loc : ds')

checkDecs (d:ds) = do
  (d_abstypes, d_env, d') <- checkDec d
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
      let visible = S.fromList $ M.elems $ envNameMap sig
          isVisible name = name `S.member` visible

      -- Check that all values are defined correctly, substituting the
      -- abstract types first.
      val_substs <- fmap M.fromList $ forM (M.toList $ envVtable sig) $ \(name, spec_bv) -> do
        let spec_bv' = substituteTypesInValBinding abs_subst_to_type spec_bv
        case findBinding envVtable Term (baseName name) env of
          Just (name', bv) -> matchVal loc name spec_bv' name' bv
          _ -> missingVal loc (baseName name)

      -- Check that all type abbreviations are correctly defined.
      abbr_name_substs <- fmap M.fromList $
                          forM (filter (isVisible . fst) $ M.toList $
                                envTypeTable sig) $ \(name,TypeAbbr spec_ps spec_t) ->
        case findBinding envTypeTable Type (baseName name) env of
          Just (name', TypeAbbr ps t) ->
            matchTypeAbbr loc abs_subst_to_type val_substs name spec_ps spec_t name' ps t
          Nothing -> missingType loc $ baseName name

      -- Check for correct modules.
      mod_substs <- fmap M.unions $ forM (M.toList $ envModTable sig) $ \(name, modspec) ->
        case findBinding envModTable Term (baseName name) env of
          Just (name', mod) -> do
            mod_substs <- matchMods abs_subst_to_type mod modspec loc
            return (M.insert name name' mod_substs)
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
        where nomatch = mismatchedType loc (baseName spec_name) (spec_ps, spec_t) (ps, t)

              matchTypeParam (TypeParamDim x _) (TypeParamDim y _) =
                pure $ M.singleton x $ DimSub $ NamedDim $ qualName y
              matchTypeParam (TypeParamType x _) (TypeParamType y _) =
                pure $ M.singleton x $ TypeSub $ TypeAbbr [] $ TypeVar (typeName y) []
              matchTypeParam _ _ =
                nomatch

    matchVal :: SrcLoc
             -> VName -> ValBinding
             -> VName -> ValBinding
             -> Either TypeError (VName, VName)
    matchVal _ spec_name (BoundV spec_t) name (BoundV t)
      | toStructural t `subtypeOf` toStructural spec_t =
          return (spec_name, name)
    matchVal loc spec_name (BoundF spec_f) name (BoundF f)
      | matchFunBinding loc spec_f f =
          return (spec_name, name)
    matchVal loc spec_name spec_v _ v =
      Left $ TypeError loc $ "Value " ++ baseString spec_name ++ " specified as type " ++
      ppValBind spec_v ++ " in signature, but has " ++ ppValBind v ++ " in structure."

    matchFunBinding :: SrcLoc -> FunBinding -> FunBinding -> Bool
    matchFunBinding loc (_, spec_pts, spec_ret) (tps, pts, ret)
      | length spec_pts == length pts,
        Just substs_and_locs <-
          foldM match mempty $
          zip (map (toStructural . snd) spec_pts) (map (toStructural . snd) pts) =
          let substs = M.map (TypeSub . TypeAbbr [] . vacuousShapeAnnotations . fst)
                       substs_and_locs
              -- This relies on the property that there can be no new
              -- type variables in the return type.
          in toStructural (substituteTypes substs ret)
             `subtypeOf` toStructural spec_ret
      where tnames = map typeParamName tps
            match substs (spec_t, t) =
              case instantiatePolymorphic tnames loc substs t spec_t of
                Right substs' -> Just substs'
                Left _        -> Nothing
    matchFunBinding _ _ _ = False

    missingType loc name =
      Left $ TypeError loc $
      "Module does not define a type named " ++ pretty name ++ "."

    missingVal loc name =
      Left $ TypeError loc $
      "Module does not define a value named " ++ pretty name ++ "."

    missingMod loc name =
      Left $ TypeError loc $
      "Module does not define a module named " ++ pretty name ++ "."

    mismatchedType loc name spec_t env_t =
      Left $ TypeError loc $ "Type " ++ pretty name ++ " specified as " ++
      ppTypeAbbr spec_t ++ " in signature, but " ++ ppTypeAbbr env_t ++ " in structure."

    resolveAbsTypes :: TySet -> Mod -> TySet -> SrcLoc
                    -> Either TypeError (M.Map VName (QualName VName, TypeBinding))
    resolveAbsTypes mod_abs mod sig_abs loc = do
      let abs_mapping = M.fromList $ zip
                        (map (fmap baseName) $ S.toList mod_abs) (S.toList mod_abs)
      fmap M.fromList $ forM (S.toList sig_abs) $ \name ->
        case findTypeDef (fmap baseName name) mod of
          Just (name', TypeAbbr ps t)
            | Just abs_name <- M.lookup (fmap baseName name) abs_mapping ->
                return (qualLeaf name, (abs_name, TypeAbbr ps t))
            | otherwise -> return (qualLeaf name, (qualName name', TypeAbbr ps t))
          _ ->
            missingType loc $ fmap baseName name

    ppValBind (BoundV t) = pretty t
    ppValBind (BoundF (tps, pts, t)) =
      unwords $ map pretty tps ++ intersperse "->" (map ppParam $ pts ++ [(Nothing, t)])
      where ppParam (Nothing, pt) = pretty pt
            ppParam (Just v, pt) = "(" ++ pretty (baseName v) ++ ": " ++ pretty pt ++ ")"

    ppTypeAbbr (ps, t) =
      "type " ++ unwords (map pretty ps) ++ " = " ++ pretty t


findBinding :: (Env -> M.Map VName v)
            -> Namespace -> Name
            -> Env
            -> Maybe (VName, v)
findBinding table namespace name the_env = do
  name' <- M.lookup (namespace, name) $ envNameMap the_env
  (name',) <$> M.lookup name' (table the_env)

findTypeDef :: QualName Name -> Mod -> Maybe (VName, TypeBinding)
findTypeDef _ ModFun{} = Nothing
findTypeDef (QualName [] name) (ModEnv the_env) =
  findBinding envTypeTable Type name the_env
findTypeDef (QualName (q:qs) name) (ModEnv the_env) = do
  (_, q_mod) <- findBinding envModTable Term q the_env
  findTypeDef (QualName qs name) q_mod

substituteTypesInMod :: TypeSubs -> Mod -> Mod
substituteTypesInMod substs (ModEnv e) =
  ModEnv $ substituteTypesInEnv substs e
substituteTypesInMod substs (ModFun (FunSig abs mod mty)) =
  ModFun $ FunSig abs (substituteTypesInMod substs mod) (substituteTypesInMTy substs mty)

substituteTypesInMTy :: TypeSubs -> MTy -> MTy
substituteTypesInMTy substs (MTy abs mod) = MTy abs $ substituteTypesInMod substs mod

substituteTypesInEnv :: TypeSubs -> Env -> Env
substituteTypesInEnv substs env =
  env { envVtable    = M.map (substituteTypesInValBinding substs) $ envVtable env
      , envTypeTable = M.mapWithKey subT $ envTypeTable env
      , envModTable  = M.map (substituteTypesInMod substs) $ envModTable env
      }
  where subT name _
          | Just (TypeSub (TypeAbbr ps t)) <- M.lookup name substs = TypeAbbr ps t
        subT _ (TypeAbbr ps t) = TypeAbbr ps $ substituteTypes substs t

allNamesInMTy :: MTy -> S.Set VName
allNamesInMTy (MTy abs mod) =
  S.map qualLeaf abs <> allNamesInMod mod

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
  where allNamesInType (TypeAbbr ps _) = S.fromList $ map typeParamName ps

newNamesForMTy :: MTy -> TypeM (MTy, M.Map VName VName)
newNamesForMTy orig_mty = do
  -- Create unique renames for the module type.
  pairs <- forM (S.toList $ allNamesInMTy orig_mty) $ \v -> do
    v' <- newName v
    return (v, v')
  let substs = M.fromList pairs

  return (substituteInMTy substs orig_mty, substs)

  where
    substituteInMTy :: M.Map VName VName -> MTy -> MTy
    substituteInMTy substs (MTy mty_abs mty_mod) =
      MTy (S.map (fmap substitute) mty_abs) (substituteInMod mty_mod)
      where
        substituteInEnv (Env vtable ttable _stable modtable names) =
          let vtable' = substituteInMap substituteInBinding vtable
              ttable' = substituteInMap substituteInTypeBinding ttable
              mtable' = substituteInMap substituteInMod modtable
          in Env { envVtable = vtable'
                 , envTypeTable = ttable'
                 , envSigTable = mempty
                 , envModTable = mtable'
                 , envNameMap = M.map substitute names
                 }

        substitute v =
          fromMaybe v $ M.lookup v substs

        substituteInMap f m =
          let (ks, vs) = unzip $ M.toList m
          in M.fromList $
             zip (map (\k -> fromMaybe k $ M.lookup k substs) ks)
                 (map f vs)

        substituteInBinding (BoundV t) =
          BoundV $ substituteInType t
        substituteInBinding (BoundF (ps, pts,t)) =
          BoundF (map substituteInTypeParam ps,
                  map (fmap substituteInType) pts,
                  substituteInType t)

        substituteInMod (ModEnv env) =
          ModEnv $ substituteInEnv env
        substituteInMod (ModFun funsig) =
          ModFun $ substituteInFunSig funsig

        substituteInFunSig (FunSig abs mod mty) =
          FunSig (S.map (fmap substitute) abs)
          (substituteInMod mod) (substituteInMTy substs mty)

        substituteInTypeBinding (TypeAbbr ps t) =
          TypeAbbr (map substituteInTypeParam ps) $ substituteInType t

        substituteInTypeParam (TypeParamDim p loc) =
          TypeParamDim (substitute p) loc
        substituteInTypeParam (TypeParamType p loc) =
          TypeParamType (substitute p) loc

        substituteInType :: StructType -> StructType
        substituteInType (TypeVar (TypeName qs v) targs) =
          TypeVar (TypeName qs $ substitute v) $ map substituteInTypeArg targs
        substituteInType (Prim t) =
          Prim t
        substituteInType (Record ts) =
          Record $ fmap substituteInType ts
        substituteInType (Array (PrimArray t shape u ())) =
          Array $ PrimArray t (substituteInShape shape) u ()
        substituteInType (Array (PolyArray (TypeName qs v) targs shape u ())) =
          Array $ PolyArray (TypeName qs $ substitute v)
          (map substituteInTypeArg targs) (substituteInShape shape) u ()
        substituteInType (Array (RecordArray ts shape u)) =
          Array $ RecordArray ts' (substituteInShape shape) u
          where ts' = fmap (flip typeToRecordArrayElem u .
                            substituteInType . recordArrayElemToType) ts

        substituteInShape (ShapeDecl ds) =
          ShapeDecl $ map substituteInDim ds
        substituteInDim (NamedDim (QualName qs v)) =
          NamedDim $ QualName qs $ substitute v
        substituteInDim d = d

        substituteInTypeArg (TypeArgDim (NamedDim (QualName qs v)) loc) =
          TypeArgDim (NamedDim $ QualName qs $ substitute v) loc
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
refineEnv :: SrcLoc -> TySet -> Env -> QualName Name -> StructType
          -> TypeM (QualName VName, TySet, Env)
refineEnv loc tset env tname t
  | Just (tname', TypeAbbr [] (TypeVar (TypeName qs v) _)) <-
      findTypeDef tname (ModEnv env),
    QualName (qualQuals tname) v `S.member` tset =
      return (tname { qualLeaf = tname' },
              QualName qs v `S.delete` tset,
              substituteTypesInEnv
               (M.fromList [(tname', TypeSub $ TypeAbbr [] t),
                            (v, TypeSub $ TypeAbbr [] t)])
              env)
  | otherwise =
      bad $ TypeError loc $
      pretty tname ++ " is not an abstract type in the module type."
