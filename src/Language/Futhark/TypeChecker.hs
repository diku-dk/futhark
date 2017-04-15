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
  , FileModule
  , Imports
  )
  where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad.Except hiding (mapM)
import Control.Monad.Reader hiding (mapM)
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

localEnv :: (Env -> Env) -> TypeM a -> TypeM a
localEnv = local . first

-- | The (abstract) result of type checking some file.  Can be passed
-- to further invocations of the type checker.
newtype FileModule = FileModule { fileEnv :: Env }

-- | A mapping from import names to imports.
type Imports = M.Map FilePath FileModule

-- | Type check a program containing no type information, yielding
-- either a type error or a program with complete type information.
-- Accepts a mapping from file names (excluding extension) to
-- previously type checker results.
checkProg :: Imports
          -> VNameSource
          -> UncheckedProg
          -> Either TypeError ((FileModule, Prog), Warnings, VNameSource)
checkProg files src prog =
  runTypeM initialEnv (M.map fileEnv files) src $ checkProgM prog

initialEnv :: Env
initialEnv = intrinsicsModule
               { envModTable = initialModTable
               , envNameMap = M.insert
                              (Structure, nameFromString "intrinsics")
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

checkProgM :: UncheckedProg -> TypeM (FileModule, Prog)
checkProgM (Prog decs) = do
  checkForDuplicateDecs decs
  (env, decs') <- checkDecs decs
  return (FileModule env, Prog decs')

checkForDuplicateDecs :: [DecBase NoInfo Name] -> TypeM ()
checkForDuplicateDecs =
  foldM_ (flip f) mempty
  where check namespace name loc known =
          case M.lookup (namespace, name) known of
            Just loc' ->
              bad $ DupDefinitionError namespace name loc loc'
            _ -> return $ M.insert (namespace, name) loc known

        f (FunDec (FunBind _ name _ _ _ _ loc)) =
          check Term name loc

        f (ValDec (ValBind name _ _ _ loc)) =
          check Term name loc

        f (TypeDec (TypeBind name _ _ loc)) =
          check Type name loc

        f (SigDec (SigBind name _ loc)) =
          check Signature name loc

        f (ModDec (ModBind name _ _ _ loc)) =
          check Structure name loc

        f OpenDec{} = return

checkSpecs :: [SpecBase NoInfo Name] -> TypeM (TySet, Env, [SpecBase Info VName])

checkSpecs [] = return (mempty, mempty, [])

checkSpecs (ValSpec name paramtypes rettype loc : specs) =
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    paramtypes' <- mapM (checkTypeDecl loc) paramtypes
    rettype' <- checkTypeDecl loc rettype
    let paramtypes'' = map (unInfo . expandedType) paramtypes'
        rettype'' = unInfo $ expandedType rettype'
        valenv =
          mempty { envVtable = M.singleton name' $
                               if null paramtypes''
                               then BoundV $ removeShapeAnnotations $
                                    rettype'' `addAliases` mempty
                               else BoundF (paramtypes'', rettype'')
                 , envNameMap = M.singleton (Term, name) name'
                 }
    (abstypes, env, specs') <- localEnv (valenv<>) $ checkSpecs specs
    return (abstypes,
            env <> valenv,
            ValSpec name' paramtypes' rettype' loc : specs')

checkSpecs (TypeAbbrSpec tdec : specs) =
  bindSpaced [(Type, typeAlias tdec)] $ do
    (tenv, tdec') <- checkTypeBind tdec
    (abstypes, env, specs') <- localEnv (tenv<>) $ checkSpecs specs
    return (abstypes,
            tenv <> env,
            TypeAbbrSpec tdec' : specs')

checkSpecs (TypeSpec name ps loc : specs) =
  checkTypeParams ps $ \ps' ->
  bindSpaced [(Type, name)] $ do
    name' <- checkName Type name loc
    let tenv = mempty
               { envNameMap =
                   M.singleton (Type, name) name'
               , envTypeTable =
                   M.singleton name' $ TypeAbbr ps' $ TypeVar (typeName name') $ map paramToArg ps'
               }
    (abstypes, env, specs') <- localEnv (tenv<>) $ checkSpecs specs
    return (S.insert (qualName name') abstypes,
            tenv <> env,
            TypeSpec name' ps' loc : specs')
      where paramToArg (TypeParamDim v ploc) =
              TypeArgDim (NamedDim $ qualName v) ploc
            paramToArg (TypeParamType v ploc) =
              TypeArgType (TypeVar (typeName v) []) ploc

checkSpecs (ModSpec name sig loc : specs) =
  bindSpaced [(Structure, name)] $ do
    name' <- checkName Structure name loc
    (mty, sig') <- checkSigExp sig
    let senv = mempty { envNameMap = M.singleton (Structure, name) name'
                      , envModTable = M.singleton name' $ mtyMod mty
                      }
    (abstypes, env, specs') <- localEnv (senv<>) $ checkSpecs specs
    return (S.map (qualify name) (mtyAbs mty) <> abstypes,
            senv <> env,
            ModSpec name' sig' loc : specs')

checkSpecs (IncludeSpec e loc : specs) = do
  (e_abs, e_env, e') <- checkSigExpToEnv e

  (abstypes, env, specs') <- localEnv (e_env<>) $ checkSpecs specs
  return (e_abs <> abstypes,
          e_env <> env,
          IncludeSpec e' loc : specs')

checkSigExp :: SigExpBase NoInfo Name -> TypeM (MTy, SigExpBase Info VName)
checkSigExp (SigParens e loc) = do
  (mty, e') <- checkSigExp e
  return (mty, SigParens e' loc)
checkSigExp (SigVar name loc) = do
  (name', mty) <- lookupMTy loc name
  (mty', _) <- newNamesForMTy mempty mty
  return (mty', SigVar name' loc)
checkSigExp (SigSpecs specs loc) = do
  checkForDuplicateSpecs specs
  (abstypes, env, specs') <- checkSpecs specs
  return (MTy abstypes $ ModEnv env, SigSpecs specs' loc)
checkSigExp (SigWith s (TypeRef tname td) loc) = do
  (s_abs, s_env, s') <- checkSigExpToEnv s
  td' <- checkTypeDecl loc td
  tname' <- localEnv (s_env<>) $ snd <$> checkQualNameWithEnv Type tname loc
  (s_abs', s_env') <- refineEnv loc s_abs s_env tname' $ unInfo $ expandedType td'
  return (MTy s_abs' $ ModEnv s_env', SigWith s' (TypeRef tname' td') loc)
checkSigExp (SigArrow maybe_pname e1 e2 loc) = do
  (MTy s_abs e1_mod, e1') <- checkSigExp e1
  (env_for_e2, maybe_pname') <-
    case maybe_pname of
      Just pname -> bindSpaced [(Structure, pname)] $ do
        pname' <- checkName Structure pname loc
        return (mempty { envNameMap = M.singleton (Structure, pname) pname'
                       , envModTable = M.singleton pname' e1_mod
                       },
                Just pname')
      Nothing ->
        return (mempty, Nothing)
  (e2_mod, e2') <- localEnv (env_for_e2<>) $ checkSigExp e2
  return (MTy mempty $ ModFun $ FunSig s_abs e1_mod e2_mod,
          SigArrow maybe_pname' e1' e2' loc)

checkSigExpToEnv :: SigExpBase NoInfo Name -> TypeM (TySet, Env, SigExpBase Info VName)
checkSigExpToEnv e = do
  (MTy abs mod, e') <- checkSigExp e
  case mod of
    ModEnv env -> return (abs, env, e')
    ModFun{}   -> bad $ UnappliedFunctor $ srclocOf e

checkSigBind :: SigBindBase NoInfo Name -> TypeM (Env, SigBindBase Info VName)
checkSigBind (SigBind name e loc) = do
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
                                               ((Structure, name), name')]
                   },
            SigBind name' e' loc)
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
  (env, decs') <- checkDecs decs
  return (MTy mempty $ ModEnv env,
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
  bindSpaced [(Structure, pname)] $ do
    pname' <- checkName Structure pname loc
    let in_body_env = mempty { envModTable = M.singleton pname' p_mod }
    localEnv (in_body_env<>) $ m (ModParam pname' psig_e' loc) p_abs p_mod

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
  (body_mty'', body_subst) <- newNamesForMTy p_subst body_mty'

  return (body_mty'', p_subst, body_subst)

checkModBind :: ModBindBase NoInfo Name -> TypeM (Env, ModBindBase Info VName)
checkModBind (ModBind name [] maybe_fsig_e e loc) = do
  (maybe_fsig_e', e', mty) <- checkModBody (fst <$> maybe_fsig_e) e loc
  bindSpaced [(Structure, name)] $ do
    name' <- checkName Structure name loc
    return (mempty { envModTable = M.singleton name' $ mtyMod mty
                   , envNameMap = M.singleton (Structure, name) name'
                   },
            ModBind name' [] maybe_fsig_e' e' loc)
checkModBind (ModBind name (p:ps) maybe_fsig_e body_e loc) = do
  (params', maybe_fsig_e', body_e', funsig) <-
    withModParam p $ \p' p_abs p_mod ->
    withModParams ps $ \params_stuff -> do
    let (ps', ps_abs, ps_mod) = unzip3 params_stuff
    (maybe_fsig_e', body_e', mty) <- checkModBody (fst <$> maybe_fsig_e) body_e loc
    let addParam (x,y) mty' = MTy mempty $ ModFun $ FunSig x y mty'
    return (p' : ps', maybe_fsig_e', body_e',
            FunSig p_abs p_mod $ foldr addParam mty $ zip ps_abs ps_mod)
  bindSpaced [(Structure, name)] $ do
    name' <- checkName Structure name loc
    return (mempty { envModTable =
                       M.singleton name' $ ModFun funsig
                   , envNameMap =
                       M.singleton (Structure, name) name'
                   },
            ModBind name' params' maybe_fsig_e' body_e' loc)


checkForDuplicateSpecs :: [SpecBase NoInfo Name] -> TypeM ()
checkForDuplicateSpecs =
  foldM_ (flip f) mempty
  where check namespace name loc known =
          case M.lookup (namespace, name) known of
            Just loc' ->
              bad $ DupDefinitionError namespace name loc loc'
            _ -> return $ M.insert (namespace, name) loc known

        f (ValSpec name _ _ loc) =
          check Term name loc

        f (TypeAbbrSpec (TypeBind name _ _ loc)) =
          check Type name loc

        f (TypeSpec name _ loc) =
          check Type name loc

        f (ModSpec name _ loc) =
          check Structure name loc

        f IncludeSpec{} =
          return

checkTypeBind :: TypeBindBase NoInfo Name
              -> TypeM (Env, TypeBindBase Info VName)
checkTypeBind (TypeBind name ps td loc) =
  checkTypeParams ps $ \ps' -> do
    td' <- bindingTypeParams ps' $ checkTypeDecl loc td
    bindSpaced [(Type, name)] $ do
      name' <- checkName Type name loc
      return (mempty { envTypeTable =
                         M.singleton name' $ TypeAbbr ps' $ unInfo $ expandedType td',
                       envNameMap =
                         M.singleton (Type, name) name'
                     },
              TypeBind name' ps' td' loc)
  where bindingTypeParams ps' = localEnv $ \e -> e <> mconcat (map typeParamEnv ps')
        typeParamEnv (TypeParamDim pv _) =
          mempty { envVtable = M.singleton pv $ BoundV $ Prim $ Signed Int32 }
        typeParamEnv (TypeParamType pv _) =
          mempty { envTypeTable = M.singleton pv $ TypeAbbr [] $ TypeVar (typeName pv) [] }

checkValBind :: ValBindBase NoInfo Name -> TypeM (Env, ValBind)
checkValBind (ValBind name maybe_t NoInfo e loc) = do
  name' <- bindSpaced [(Term, name)] $ checkName Term name loc
  (maybe_t', e') <- case maybe_t of
    Just t  -> do
      (tdecl, tdecl_type, implicit) <- checkTypeExp t
      unless (M.null $ implicitNameMap implicit) $
        bad $ TypeError loc
        "Type ascription for let-binding may not have shape declarations."

      let t_structural = toStructural tdecl_type
      when (anythingUnique t_structural) $
        bad $ UniqueConstType loc name t_structural
      e' <- require [t_structural] =<< runTermTypeM (checkExp e)
      return (Just tdecl, e')
    Nothing -> do
      e' <- runTermTypeM $ checkExp e
      return (Nothing, e')
  let e_t = vacuousShapeAnnotations $ toStructural $ typeOf e'
  return (mempty { envVtable =
                     M.singleton name' (BoundV $ typeOf e' `setAliases` mempty)
                 , envNameMap =
                     M.singleton (Term, name) name'
                 },
          ValBind name' maybe_t' (Info e_t) e' loc)
  where anythingUnique (Record fs) = any anythingUnique fs
        anythingUnique et          = unique et

checkFunBind :: FunBindBase NoInfo Name -> TypeM (Env, FunBind)
checkFunBind (FunBind entry fname maybe_retdecl NoInfo params body loc) = do
  (fname', params', maybe_retdecl', rettype, body') <-
    bindSpaced [(Term, fname)] $
    runTermTypeM $ checkFunDef (fname, maybe_retdecl, params, body, loc)

  return (mempty { envVtable =
                     M.singleton fname'
                     (BoundF (map paramType params', rettype))
                 , envNameMap =
                     M.singleton (Term, fname) fname'
                 },
           FunBind entry fname' maybe_retdecl' (Info rettype) params' body' loc)

  where paramType :: Pattern -> StructType
        paramType = vacuousShapeAnnotations . toStruct . patternType

checkDecs :: [DecBase NoInfo Name] -> TypeM (Env, [DecBase Info VName])
checkDecs (ModDec struct:rest) = do
  (modenv, struct') <- checkModBind struct
  localEnv (modenv<>) $ do
    (env, rest') <- checkDecs rest
    return (env <> modenv, ModDec struct' : rest')

checkDecs (SigDec sig:rest) = do
  (sigenv, sig') <- checkSigBind sig
  localEnv (sigenv<>) $ do
    (env, rest') <- checkDecs rest
    return (env <> sigenv, SigDec sig' : rest')

checkDecs (TypeDec tdec:rest) = do
  (tenv, tdec') <- checkTypeBind tdec
  localEnv (tenv<>) $ do
    (env, rest') <- checkDecs rest
    return (env <> tenv, TypeDec tdec' : rest')

checkDecs (OpenDec x xs loc:rest) = do
  (_x_abs, x_env, x') <- checkModExpToEnv x
  (_xs_abs, xs_envs, xs') <- unzip3 <$> mapM checkModExpToEnv xs
   -- We cannot use mconcat, as mconcat is a right-fold.
  let env_ext = foldl (flip mappend) x_env xs_envs
  localEnv (env_ext<>) $ do
    (env, rest') <- checkDecs rest
    return (env <> env_ext,
            OpenDec x' xs' loc: rest')

checkDecs (ValDec vb:rest) = do
  (ext, vb') <- checkValBind vb
  localEnv (ext<>) $ do
    (env, vds') <- checkDecs rest
    return (env <> ext, ValDec vb' : vds')

checkDecs (FunDec fb:rest) = do
  (ext, fb') <- checkFunBind fb
  localEnv (ext<>) $ do
    (env, vds') <- checkDecs rest
    return (env <> ext, FunDec fb' : vds')

checkDecs [] =
  return (mempty, [])

checkTypeDecl :: SrcLoc -> TypeDeclBase NoInfo Name -> TypeM (TypeDeclBase Info VName)
checkTypeDecl loc (TypeDecl t NoInfo) = do
  (t', st, implicit) <- checkTypeExp t
  unless (M.null $ implicitNameMap implicit) $
    bad $ TypeError loc "Type may not have shape declarations here."
  return $ TypeDecl t' $ Info st

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

    matchMTys' _(MTy _ ModFun{}) (MTy _ ModEnv{}) loc =
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

      pmod_substs <- matchMods abs_subst_to_type mod_pmod sig_pmod loc
      mod_substs <- matchMTys' abs_subst_to_type mod_mod sig_mod loc
      return (pmod_substs <> mod_substs)

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
      val_substs <- fmap M.fromList $ forM (envVals sig) $ \(name, (spec_pts, spec_t)) -> do
        let spec_pts' = map (substituteTypes abs_subst_to_type) spec_pts
            spec_t'   = substituteTypes abs_subst_to_type spec_t
        case findBinding envVtable Term (baseName name) env of
          Just (name', BoundV t)
            | null spec_pts', toStructural t `subtypeOf` toStructural spec_t' ->
                return (name, name')
            | otherwise ->
                mismatchedVal loc name (spec_pts', spec_t') ([], t)
          Just (name', BoundF (pts, ret))
            | and (zipWith subtypeOf (map toStructural pts) (map toStructural spec_pts')),
              toStructural ret `subtypeOf` toStructural spec_t' ->
                return (name, name')
            | otherwise ->
                mismatchedVal loc (baseName name) (spec_pts', spec_t') (pts, ret)
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
      mod_substs <- fmap M.unions $ forM (envMods sig) $ \(name, modspec) ->
        case findBinding envModTable Structure (baseName name) env of
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

    mismatchedVal loc name spec_t env_t =
      Left $ TypeError loc $ "Value " ++ pretty name ++ " specified as type " ++
      ppFunType spec_t ++ " in signature, but has " ++ ppFunType env_t ++ " in structure."

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
      (_, q_mod) <- findBinding envModTable Structure q the_env
      findTypeDef (QualName qs name) q_mod

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

    ppFunType (paramts, ret) =
      intercalate " -> " $ map pretty $ paramts ++ [ret]

    ppTypeAbbr (ps, t) =
      "type " ++ unwords (map pretty ps) ++ " = " ++ pretty t

substituteTypesInMod :: TypeSubs -> Mod -> Mod
substituteTypesInMod substs (ModEnv e) =
  ModEnv $ substituteTypesInEnv substs e
substituteTypesInMod substs (ModFun (FunSig abs mod mty)) =
  ModFun $ FunSig abs (substituteTypesInMod substs mod) (substituteTypesInMTy substs mty)

substituteTypesInMTy :: TypeSubs -> MTy -> MTy
substituteTypesInMTy substs (MTy abs mod) = MTy abs $ substituteTypesInMod substs mod

substituteTypesInEnv :: TypeSubs -> Env -> Env
substituteTypesInEnv substs env =
  env { envVtable    = M.map subV $ envVtable env
      , envTypeTable = M.mapWithKey subT $ envTypeTable env
      , envModTable  = M.map (substituteTypesInMod substs) $ envModTable env
      }
  where subV (BoundV t) =
          BoundV $ fromStruct $ toStructural $
          substituteTypes substs $
          vacuousShapeAnnotations $ toStruct t
        subV (BoundF (ts, t)) =
          BoundF (map (substituteTypes substs) ts,
                  substituteTypes substs t)

        subT name _
          | Just (TypeSub (TypeAbbr ps t)) <- M.lookup name substs = TypeAbbr ps t
        subT _ (TypeAbbr ps t) = TypeAbbr ps $ substituteTypes substs t

allNamesInMTy :: MTy -> S.Set VName
allNamesInMTy (MTy abs mod) =
  S.map qualLeaf abs <> allNamesInMod mod

allNamesInMod :: Mod -> S.Set VName
allNamesInMod (ModEnv env) = allNamesInEnv env
allNamesInMod (ModFun (FunSig abs mod mty)) =
  S.map qualLeaf abs <> allNamesInMod mod <> allNamesInMTy mty

-- All names defined anywhere in the env.
allNamesInEnv :: Env -> S.Set VName
allNamesInEnv (Env vtable ttable stable modtable _names) =
  S.fromList (M.keys vtable ++ M.keys ttable ++
               M.keys stable ++ M.keys modtable) <>
  mconcat (map allNamesInMTy (M.elems stable) ++
           map allNamesInMod (M.elems modtable) ++
           map allNamesInType (M.elems ttable))
  where allNamesInType (TypeAbbr ps _) = S.fromList $ map typeParamName ps

newNamesForMTy :: M.Map VName VName -> MTy -> TypeM (MTy, M.Map VName VName)
newNamesForMTy except orig_mty = do
  -- Create unique renames for the module type.
  substs <- fmap M.fromList $ forM (S.toList $ allNamesInMTy orig_mty) $ \v ->
    case M.lookup v except of
      Just v' -> return (v, v')
      Nothing -> do v' <- newName v
                    return (v, v')

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
          BoundV $ fromStruct $ toStructural $

          substituteInType $ vacuousShapeAnnotations $ toStruct t
        substituteInBinding (BoundF (pts,t)) =
          BoundF (map substituteInType pts, substituteInType t)

        substituteInMod (ModEnv env) =
          ModEnv $ substituteInEnv env
        substituteInMod (ModFun funsig) =
          ModFun $ substituteInFunSig funsig

        substituteInFunSig (FunSig abs mod mty) =
          FunSig (S.map (fmap substitute) abs)
          (substituteInMod mod) (substituteInMTy substs mty)

        substituteInTypeBinding (TypeAbbr ps t) =
          TypeAbbr (map substituteInTypeParam ps) $ substituteInType t
          where substituteInTypeParam (TypeParamDim p loc) =
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
        substituteInDim (BoundDim v) =
          BoundDim $ substitute v
        substituteInDim d = d

        substituteInTypeArg (TypeArgDim (NamedDim (QualName qs v)) loc) =
          TypeArgDim (NamedDim $ QualName qs $ substitute v) loc
        substituteInTypeArg (TypeArgDim (BoundDim v) loc) =
          TypeArgDim (BoundDim $ substitute v) loc
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
refineEnv :: SrcLoc -> TySet -> Env -> QualName VName -> StructType
          -> TypeM (TySet, Env)
refineEnv loc tset env tname t
  | Just (TypeAbbr [] (TypeVar (TypeName qs v) _)) <-
      M.lookup (qualLeaf tname) $ envTypeTable env,
    QualName qs v `S.member` tset =
      return (QualName qs v `S.delete` tset,
              substituteTypesInEnv
               (M.fromList [(qualLeaf tname, TypeSub $ TypeAbbr [] t),
                             (v, TypeSub $ TypeAbbr [] t)])
              env)
  | otherwise =
      bad $ TypeError loc $
      pretty tname ++ " is not an abstract type in the module type."
