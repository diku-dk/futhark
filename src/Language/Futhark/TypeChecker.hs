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

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Prelude hiding (abs, mod, mapM)

import Language.Futhark
import Futhark.FreshNames hiding (newName)
import Language.Futhark.TypeChecker.Monad
import Language.Futhark.TypeChecker.Terms

--- The main checker

-- | Get leading values and functions with return type information.
chompDecs :: [DecBase NoInfo Name]
          -> ([ValDecBase NoInfo Name], [DecBase NoInfo Name])
chompDecs (ValDec dec : xs)
  | typed dec = let (valdecs, xs') = chompDecs xs
                in (dec : valdecs, xs')
  where typed (FunDec fb)   = isJust $ funBindRetDecl fb
        typed (ConstDec cb) = isJust $ constBindTypeDecl cb
chompDecs xs                = ([], xs)

boundByValDec :: ValDecBase f Name -> [(Namespace, Name)]
boundByValDec (FunDec dec)  = [(Term, funBindName dec)]
boundByValDec (ConstDec dec) = [(Term, constBindName dec)]

buildEnvFromDecs :: [ValDecBase NoInfo Name]
                 -> TypeM Env
buildEnvFromDecs = foldM expandV mempty
  where
    paramDeclaredType (PatternParens p _) =
      paramDeclaredType p
    paramDeclaredType (PatternAscription _ t) =
      Just $ declaredType t
    paramDeclaredType (TuplePattern ps loc) =
      TETuple <$> mapM paramDeclaredType ps <*> pure loc
    paramDeclaredType _ =
      Nothing

    expandV env (FunDec (FunBind _ fname maybe_ret _ params _ loc)) = do
      fname' <- checkName Term fname loc
      argtypes <- forM params $ \param ->
        case paramDeclaredType param of
          Just t -> return t
          Nothing -> bad $ TypeError (srclocOf param) $
                     "Missing type information for parameter " ++
                     pretty param
      argtypes' <- mapM (fmap snd . checkTypeExpNoDims) argtypes
      boundf <- case maybe_ret of
        Just ret -> do (_, ret') <- checkTypeExpNoDims ret
                       return $ BoundF (argtypes', ret')
        Nothing -> bad $ TypeError loc $
                   "Missing return type for function " ++ pretty fname
      return env { envVtable = HM.insert fname' boundf $
                                 envVtable env
                 , envNameMap = HM.insert (Term, fname) fname' $
                                envNameMap env
                 }

    expandV env (ConstDec (ConstBind cname maybe_t NoInfo _ loc)) = do
      cname' <- checkName Term cname loc
      entry <- case maybe_t of
        Just t -> do (_, st') <- checkTypeExpNoDims t
                     return $ BoundV $ removeShapeAnnotations $ fromStruct st'
        Nothing -> bad $ TypeError loc $
                   "Missing type information for variable " ++ pretty cname
      return env { envVtable = HM.insert cname' entry $
                                 envVtable env
                 , envNameMap = HM.insert (Term, cname) cname' $
                                envNameMap env
                 }

valDecEnv :: ValDecBase Info VName -> Env
valDecEnv (FunDec fb) =
  mempty { envVtable =
             HM.singleton (funBindName fb)
             (BoundF (map paramType (funBindParams fb),
                      unInfo $ funBindRetType fb))
         , envNameMap =
             HM.singleton (Term, baseName (funBindName fb)) (funBindName fb)
         }
  where paramType :: Pattern -> StructType
        paramType = vacuousShapeAnnotations . toStruct . patternType
valDecEnv (ConstDec cb) =
  mempty { envVtable =
             HM.singleton (constBindName cb)
             (BoundV $ removeShapeAnnotations $ fromStruct $ unInfo $ constBindType cb)
         , envNameMap =
             HM.singleton (Term, baseName (constBindName cb)) (constBindName cb)
         }

localEnv :: (Env -> Env) -> TypeM a -> TypeM a
localEnv = local . first

bindSpaced :: [(Namespace, Name)] -> TypeM a -> TypeM a
bindSpaced names body = do
  names' <- mapM (newID . snd) names
  let mapping = HM.fromList (zip names names')
  bindNameMap mapping body

bindNameMap :: NameMap -> TypeM a -> TypeM a
bindNameMap m = localEnv $ \scope -> scope { envNameMap = m <> envNameMap scope }

-- | The (abstract) result of type checking some file.  Can be passed
-- to further invocations of the type checker.
newtype FileModule = FileModule { fileEnv :: Env }

-- | A mapping from import names to imports.
type Imports = HM.HashMap FilePath FileModule

-- | Type check a program containing no type information, yielding
-- either a type error or a program with complete type information.
-- Accepts a mapping from file names (excluding extension) to
-- previously type checker results.
checkProg :: Imports
          -> VNameSource
          -> UncheckedProg
          -> Either TypeError ((FileModule, Prog), Warnings, VNameSource)
checkProg files src prog =
  runTypeM initialEnv (HM.map fileEnv files) src $ checkProgM prog

initialEnv :: Env
initialEnv = intrinsicsModule
               { envModTable = initialModTable
               , envNameMap = HM.insert
                              (Structure, nameFromString "intrinsics")
                              intrinsics_v
                              topLevelNameMap
               }
  where initialTypeTable = HM.fromList $ mapMaybe addIntrinsicT $ HM.toList intrinsics
        initialModTable = HM.singleton intrinsics_v (ModEnv intrinsicsModule)

        intrinsics_v = ID (nameFromString "intrinsics", 0)

        intrinsicsModule = Env mempty initialTypeTable mempty mempty intrinsicsNameMap

        addIntrinsicT (name, IntrinsicType t) =
          Just (name, TypeAbbr $ Prim t)
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
          case HM.lookup (namespace, name) known of
            Just loc' ->
              bad $ DupDefinitionError namespace name loc loc'
            _ -> return $ HM.insert (namespace, name) loc known

        f (ValDec (FunDec (FunBind _ name _ _ _ _ loc))) =
          check Term name loc

        f (ValDec (ConstDec (ConstBind name _ _ _ loc))) =
          check Term name loc

        f (TypeDec (TypeBind name _ loc)) =
          check Type name loc

        f (SigDec (SigBind name _ loc)) =
          check Signature name loc

        f (StructDec (StructBind name _ loc)) =
          check Structure name loc

        f (FunctorDec (FunctorBind name _ _ _ loc)) =
          check Structure name loc

        f OpenDec{} = return

checkSpecs :: [SpecBase NoInfo Name] -> TypeM (HS.HashSet VName, Env, [SpecBase Info VName])

checkSpecs [] = return (mempty, mempty, [])

checkSpecs (ValSpec name paramtypes rettype loc : specs) =
  bindSpaced [(Term, name)] $ do
    name' <- checkName Term name loc
    paramtypes' <- mapM checkTypeDecl paramtypes
    rettype' <- checkTypeDecl rettype
    let paramtypes'' = map (unInfo . expandedType) paramtypes'
        rettype'' = unInfo $ expandedType rettype'
        valenv =
          mempty { envVtable = HM.singleton name' $
                               if null paramtypes''
                               then BoundV $ removeShapeAnnotations $
                                    rettype'' `addAliases` mempty
                               else BoundF (paramtypes'', rettype'')
                 , envNameMap = HM.singleton (Term, name) name'
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

checkSpecs (TypeSpec name loc : specs) =
  bindSpaced [(Type, name)] $ do
    abs_name <- newID name
    name' <- checkName Type name loc
    let tenv = mempty
               { envNameMap =
                   HM.singleton (Type, name) name'
               , envTypeTable =
                   HM.singleton name' $ TypeAbbr $ TypeVar $ typeName abs_name
               }
    (abstypes, env, specs') <- localEnv (tenv<>) $ checkSpecs specs
    return (HS.insert abs_name abstypes,
            tenv <> env,
            TypeSpec name' loc : specs')

checkSpecs (ModSpec name sig loc : specs) =
  bindSpaced [(Structure, name)] $ do
    name' <- checkName Structure name loc
    (mty, sig') <- checkSigExp sig
    let senv = mempty { envNameMap = HM.singleton (Structure, name) name'
                      , envModTable = HM.singleton name' $ mtyMod mty
                      }
    (abstypes, env, specs') <- localEnv (senv<>) $ checkSpecs specs
    return (mtyAbs mty <> abstypes,
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
  (env, e') <- checkSigExp e
  return (env, SigParens e' loc)
checkSigExp (SigVar name loc) = do
  (name', env) <- lookupMTy loc name
  return (env, SigVar name' loc)
checkSigExp (SigSpecs specs loc) = do
  checkForDuplicateSpecs specs
  (abstypes, env, specs') <- checkSpecs specs
  return (MTy abstypes $ ModEnv env, SigSpecs specs' loc)
checkSigExp (SigWith s (TypeRef tname td) loc) = do
  (s_abs, s_env, s') <- checkSigExpToEnv s
  td' <- checkTypeDecl td
  tname' <- localEnv (s_env<>) $ snd <$> checkQualName Type tname loc
  (s_abs', s_env') <- refineEnv loc s_abs s_env tname' $ unInfo $ expandedType td'
  return (MTy s_abs' $ ModEnv s_env', SigWith s' (TypeRef tname' td') loc)
checkSigExp (SigArrow maybe_pname e1 e2 loc) = do
  (s_abs, e1_env, e1') <- checkSigExpToEnv e1
  (env_for_e2, maybe_pname') <-
    case maybe_pname of
      Just pname -> bindSpaced [(Structure, pname)] $ do
        pname' <- checkName Structure pname loc
        return (mempty { envNameMap = HM.singleton (Structure, pname) pname'
                       , envModTable = HM.singleton pname' $ ModEnv e1_env
                       },
                Just pname')
      Nothing ->
        return (mempty, Nothing)
  (e2_mod, e2') <- localEnv (env_for_e2<>) $ checkSigExp e2
  return (MTy mempty $ ModFun $ FunSig s_abs e1_env e2_mod,
          SigArrow maybe_pname' e1' e2' loc)

checkSigExpToEnv :: SigExpBase NoInfo Name -> TypeM (TySet, Env, SigExpBase Info VName)
checkSigExpToEnv e = do
  (mod, e') <- checkSigExp e
  case mtyMod mod of
    ModEnv env -> return (mtyAbs mod, env, e')
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
    return (mempty { envSigTable = HM.singleton name' env
                   , envModTable = HM.singleton name' $ ModEnv sigmod
                   , envNameMap = HM.fromList [((Signature, name), name'),
                                               ((Structure, name), name')]
                   },
            SigBind name' e' loc)
  where typeAbbrEnvFromSig (MTy _ (ModEnv env)) =
          let types = HM.map TypeAbbr $ envTypeAbbrs env
              names = HM.fromList $ map nameMapping $ HM.toList types
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
  (f', functor) <- lookupFunctor loc f
  (e_mty, e') <- checkModExp e
  (f_mty, psubsts, rsubsts) <- applyFunctor loc functor e_mty
  return (f_mty, ModApply f' e' (Info psubsts) (Info rsubsts) loc)
checkModExp (ModAscript me se NoInfo loc) = do
  (me_mod, me') <- checkModExp me
  (se_mty, se') <- checkSigExp se
  (mty, _) <- badOnLeft $ matchMTys me_mod se_mty loc
  -- See issue #262 for martinsubst justification.
  (mty', martinsubst) <- newNamesForMTy mempty mty
  return (mty', ModAscript me' se' (Info martinsubst) loc)
checkModExp (ModLambda (p, psig_e) maybe_fsig_e body_e loc) = do
  (p', p_abs, p_env, psig_e', maybe_fsig_e', body_e', mty) <-
    checkModFun p psig_e maybe_fsig_e body_e loc
  return (MTy mempty $ ModFun $ FunSig p_abs p_env mty,
          ModLambda (p', psig_e') maybe_fsig_e' body_e' loc)

checkModExpToEnv :: ModExpBase NoInfo Name -> TypeM (TySet, Env, ModExpBase Info VName)
checkModExpToEnv e = do
  (MTy abs mod, e') <- checkModExp e
  case mod of
    ModEnv env -> return (abs, env, e')
    ModFun{}   -> bad $ UnappliedFunctor $ srclocOf e

checkModFun :: Name
            -> SigExpBase NoInfo Name
            -> Maybe (SigExpBase NoInfo Name)
            -> ModExpBase NoInfo Name
            -> SrcLoc
            -> TypeM (VName, TySet, Env, SigExp, Maybe SigExp, ModExp, MTy)
checkModFun p psig_e maybe_fsig_e body_e loc = do
  (p_abs, p_env, psig_e') <- checkSigExpToEnv psig_e
  bindSpaced [(Structure, p)] $ do
    p' <- checkName Structure p loc
    let in_body_env = mempty { envModTable = HM.singleton p' $ ModEnv p_env }
    localEnv (in_body_env<>) $ do
      (body_env, body_e') <- checkModExp body_e
      case maybe_fsig_e of
        Nothing ->
          return (p', p_abs, p_env, psig_e', Nothing, body_e', body_env)
        Just fsig_e -> do
          (fsig_env, fsig_e') <- checkSigExp fsig_e
          (env', _) <- badOnLeft $ matchMTys body_env fsig_env loc
          return (p', p_abs, p_env, psig_e', Just fsig_e', body_e', env')

applyFunctor :: SrcLoc
             -> FunSig
             -> MTy
             -> TypeM (MTy,
                       HM.HashMap VName VName,
                       HM.HashMap VName VName)
applyFunctor applyloc (FunSig p_abs p_env body_mty) a_mty = do
  (_, sig_subst) <- badOnLeft $ matchMTys a_mty (MTy p_abs $ ModEnv p_env) applyloc

  -- Apply type abbreviations from a_mty to body_mty.
  let a_abbrs = HM.map TypeAbbr $ mtyTypeAbbrs a_mty
  let type_subst = HM.mapMaybe (`HM.lookup` a_abbrs) sig_subst
  let body_mty' = substituteTypesInMTy type_subst body_mty
  (body_mty'', body_subst) <- newNamesForMTy sig_subst body_mty'

  return (body_mty'', sig_subst, body_subst)

checkStructBind :: StructBindBase NoInfo Name -> TypeM (Env, StructBindBase Info VName)
checkStructBind (StructBind name e loc) = do
  (mty, e') <- checkModExp e
  bindSpaced [(Structure, name)] $ do
    name' <- checkName Structure name loc
    return (mempty { envModTable = HM.singleton name' $ mtyMod mty
                   , envNameMap = HM.singleton (Structure, name) name'
                   },
            StructBind name' e' loc)

checkForDuplicateSpecs :: [SpecBase NoInfo Name] -> TypeM ()
checkForDuplicateSpecs =
  foldM_ (flip f) mempty
  where check namespace name loc known =
          case HM.lookup (namespace, name) known of
            Just loc' ->
              bad $ DupDefinitionError namespace name loc loc'
            _ -> return $ HM.insert (namespace, name) loc known

        f (ValSpec name _ _ loc) =
          check Term name loc

        f (TypeAbbrSpec (TypeBind name _ loc)) =
          check Type name loc

        f (TypeSpec name loc) =
          check Type name loc

        f (ModSpec name _ loc) =
          check Structure name loc

        f IncludeSpec{} =
          return

checkFunctorBind :: FunctorBindBase NoInfo Name -> TypeM (Env, FunctorBindBase Info VName)
checkFunctorBind (FunctorBind name (p, psig_e) maybe_fsig_e body_e loc) = do
  (p', p_abs, p_env, psig_e', maybe_fsig_e', body_e', mty) <-
    checkModFun p psig_e maybe_fsig_e body_e loc
  bindSpaced [(Structure, name)] $ do
    name' <- checkName Structure name loc
    return (mempty { envModTable =
                       HM.singleton name' $ ModFun $ FunSig p_abs p_env mty
                   , envNameMap =
                       HM.singleton (Structure, name) name'
                   },
            FunctorBind name' (p', psig_e') maybe_fsig_e' body_e' loc)

checkTypeBind :: TypeBindBase NoInfo Name
              -> TypeM (Env, TypeBindBase Info VName)
checkTypeBind (TypeBind name td loc) = do
  td' <- checkTypeDecl td
  bindSpaced [(Type, name)] $ do
    name' <- checkName Type name loc
    return (mempty { envTypeTable =
                       HM.singleton name' $ TypeAbbr $ unInfo $ expandedType td',
                     envNameMap =
                       HM.singleton (Type, name) name'
                   },
            TypeBind name' td' loc)

checkDecs :: [DecBase NoInfo Name] -> TypeM (Env, [DecBase Info VName])
checkDecs (StructDec struct:rest) = do
  (modenv, struct') <- checkStructBind struct
  localEnv (modenv<>) $ do
    (env, rest') <- checkDecs rest
    return (env <> modenv, StructDec struct' : rest')

checkDecs (SigDec sig:rest) = do
  (sigenv, sig') <- checkSigBind sig
  localEnv (sigenv<>) $ do
    (env, rest') <- checkDecs rest
    return (env <> sigenv, SigDec sig' : rest')

checkDecs (FunctorDec func:rest) = do
  (funcenv, func') <- checkFunctorBind func
  localEnv (funcenv<>) $ do
    (env, rest') <- checkDecs rest
    return (funcenv <> env, FunctorDec func' : rest')

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

checkDecs [] =
  return (mempty, [])

checkDecs decs@(ValDec{}:_)
  | (t_and_f_decs@(_:_), rest) <- chompDecs decs = do
      let bound = concatMap boundByValDec t_and_f_decs
      bindSpaced bound $ do
        t_and_f_env <- buildEnvFromDecs t_and_f_decs
        localEnv (t_and_f_env<>) $ do
          (env, decs') <- checkValDecs t_and_f_decs rest
          return (env <> t_and_f_env,
                  decs')

checkDecs (ValDec vd:rest) =
  bindSpaced (boundByValDec vd) $ checkValDecs [vd] rest

checkValDecs :: [ValDecBase NoInfo Name]
             -> [DecBase NoInfo Name]
             -> TypeM (Env, [DecBase Info VName])
checkValDecs [] ds = checkDecs ds
checkValDecs (FunDec fundec:vds) ds = do
  fundec' <- checkFun fundec
  let ext = valDecEnv $ FunDec fundec'
  localEnv (ext<>) $ do
    (env, vds') <- checkValDecs vds ds
    return (env <> ext, ValDec (FunDec fundec') : vds')
checkValDecs (ConstDec constdec:vds) ds = do
  constdec' <- checkConst constdec
  let ext = valDecEnv $ ConstDec constdec'
  localEnv (ext<>) $ do
    (env, vds') <- checkValDecs vds ds
    return (env <> ext, ValDec (ConstDec constdec') : vds')

checkConst :: ConstBindBase NoInfo Name -> TypeM ConstBind
checkConst (ConstBind name maybe_t NoInfo e loc) = do
  name' <- checkName Term name loc
  (maybe_t', e') <- case maybe_t of
    Just t  -> do
      (tdecl, tdecl_type) <- runTermTypeM $ checkTypeExp t
      let t_structural = toStructural tdecl_type
      when (anythingUnique t_structural) $
        bad $ UniqueConstType loc name t_structural
      e' <- require [t_structural] =<< runTermTypeM (checkExp e)
      return (Just tdecl, e')
    Nothing -> do
      e' <- runTermTypeM $ checkExp e
      return (Nothing, e')
  let e_t = vacuousShapeAnnotations $ toStructural $ typeOf e'
  return $ ConstBind name' maybe_t' (Info e_t) e' loc
  where anythingUnique (Tuple ts) = any anythingUnique ts
        anythingUnique et         = unique et

checkFun :: FunBindBase NoInfo Name -> TypeM FunBind
checkFun (FunBind entry fname maybe_retdecl NoInfo params body loc) = do
  (fname', params', maybe_retdecl', rettype, body') <-
    runTermTypeM $ checkFunDef (fname, maybe_retdecl, params, body, loc)

  when entry $
    case maybe_retdecl of
      Just retdecl
        | Just problem <-
            find (not . (`HS.member` mconcat (map patNameSet params))) $
            mapMaybe dimDeclName $ arrayDims' retdecl ->
              bad $ EntryPointConstReturnDecl loc fname $ qualName problem
      _ -> return ()

  return $ FunBind entry fname' maybe_retdecl' (Info rettype) params' body' loc

  where dimDeclName (NamedDim name) = Just name
        dimDeclName _               = Nothing

checkTypeExpNoDims :: TypeExp Name -> TypeM (TypeExp VName, StructType)
checkTypeExpNoDims = expandType $ \_ _ -> return AnyDim

checkTypeDecl :: TypeDeclBase NoInfo Name -> TypeM (TypeDeclBase Info VName)
checkTypeDecl (TypeDecl t NoInfo) = do
  (t', st) <- runTermTypeM $ checkTypeExp t
  return $ TypeDecl t' $ Info st

--- Signature matching

type AbsToTypes = HM.HashMap VName TypeBinding
type AbsToNames = HM.HashMap VName TypeBinding

-- Return new renamed/abstracted env, as well as a mapping from
-- names in the signature to names in the new env.  This is used for
-- functor application.  The first env is the module env, and the
-- second the env it must match.
matchMTys :: MTy -> MTy -> SrcLoc
          -> Either TypeError (MTy, HM.HashMap VName VName)
matchMTys = matchMTys' mempty mempty
  where
    matchMTys' :: AbsToTypes -> AbsToNames -> MTy -> MTy -> SrcLoc
               -> Either TypeError (MTy, HM.HashMap VName VName)

    matchMTys' _ _(MTy _ ModFun{}) (MTy _ ModEnv{}) loc =
      Left $ TypeError loc "Cannot match parametric module with non-paramatric module type."

    matchMTys' _ _ (MTy _ ModEnv{}) (MTy _ ModFun{}) loc =
      Left $ TypeError loc "Cannot match non-parametric module with paramatric module type."

    matchMTys' old_abs_subst_to_type old_abs_subst_to_name
              (MTy mod_abs mod) (MTy sig_abs sig)
              loc = do
      -- Check that abstract types in 'sig' have an implementation in
      -- 'env'.  This also gives us a substitution that we use to check
      -- the types of values.
      let abs_mapping = HM.fromList $ zip
                        (map baseName $ HS.toList mod_abs) (HS.toList mod_abs)
      abs_substs <- fmap HM.fromList $ forM (HS.toList sig_abs) $ \name ->
        case findTypeDef (baseName name) mod of
          Just (name', TypeAbbr t) ->
            return (name, (name', TypeAbbr t))
          Nothing
            | Just name' <- HM.lookup (baseName name) abs_mapping ->
                return (name, (name', TypeAbbr $ TypeVar $ typeName name'))
            | otherwise ->
                missingType loc $ baseName name

      let abs_names = map fst $ HM.elems abs_substs
          abs_subst_to_type = old_abs_subst_to_type <> HM.map snd abs_substs
          abs_subst_to_name = old_abs_subst_to_name <>
                              HM.map (TypeAbbr . TypeVar . typeName . fst) abs_substs
          abs_name_substs   = HM.map fst abs_substs
          abs_types = mod_abs <> HS.fromList abs_names
      (res_env, substs) <- matchMods abs_subst_to_type abs_subst_to_name mod sig loc
      return (MTy abs_types res_env,
              substs <> abs_name_substs)

    matchMods :: AbsToTypes -> AbsToNames -> Mod -> Mod -> SrcLoc
              -> Either TypeError (Mod, HM.HashMap VName VName)
    matchMods _ _ ModEnv{} ModFun{} loc =
      Left $ TypeError loc "Cannot match non-parametric module with paramatric module type."
    matchMods _ _ ModFun{} ModEnv{} loc =
      Left $ TypeError loc "Cannot match parametric module with non-paramatric module type."

    matchMods abs_subst_to_type abs_subst_to_name (ModEnv mod) (ModEnv sig) loc = do
      (mod', substs) <- matchEnvs abs_subst_to_type abs_subst_to_name mod sig loc
      return (ModEnv mod', substs)

    matchMods old_abs_subst_to_type old_abs_subst_to_name
              (ModFun (FunSig mod_abs mod_pmod mod_mod))
              (ModFun (FunSig sig_abs sig_pmod sig_mod))
              loc = do
      let abs_mapping = HM.fromList $ zip
                        (map baseName $ HS.toList mod_abs) (HS.toList mod_abs)
      abs_substs <- fmap HM.fromList $ forM (HS.toList sig_abs) $ \name ->
        case findTypeDef (baseName name) (ModEnv mod_pmod) of
          _ | Just name' <- HM.lookup (baseName name) abs_mapping ->
                return (name, (name', TypeAbbr $ TypeVar $ typeName name'))
          Just (name', TypeAbbr t) ->
            return (name, (name', TypeAbbr t))
          Nothing ->
            missingType loc $ baseName name

      let abs_subst_to_type = old_abs_subst_to_type <> HM.map snd abs_substs
          abs_subst_to_name = old_abs_subst_to_name <>
                              HM.map (TypeAbbr . TypeVar . typeName . fst) abs_substs

      (mod_pmod', pmod_substs) <- matchEnvs abs_subst_to_type abs_subst_to_name mod_pmod sig_pmod loc
      (mod_mod', mod_substs) <- matchMTys' abs_subst_to_type abs_subst_to_name mod_mod sig_mod loc
      return (ModFun (FunSig mod_abs mod_pmod' mod_mod'),
              pmod_substs <> mod_substs)

    matchEnvs :: AbsToTypes -> AbsToNames
              -> Env -> Env -> SrcLoc
              -> Either TypeError (Env, HM.HashMap VName VName)
    matchEnvs abs_subst_to_type abs_subst_to_name env sig loc = do
      -- Check that all type abbreviations are correctly defined.
      abbr_substs <- fmap HM.fromList $ forM (HM.toList $ envTypeAbbrs sig) $ \(name,spec_t) -> do
        let spec_t' = substituteTypes abs_subst_to_type spec_t
        case findBinding envTypeTable Type (baseName name) env of
          Just (name', TypeAbbr t)
            | spec_t' == t ->
                return (name, (name', substituteTypes abs_subst_to_name spec_t))
            | otherwise ->
                mismatchedType loc (baseName name) ([], spec_t) ([], t)
            | otherwise ->
              Left $ TypeError loc $
              "Type abbreviation " ++ pretty (baseName name) ++ " = " ++ pretty spec_t ++
              " defined as abstract in module."
          Nothing -> missingType loc $ baseName name

      let abbrs = HM.map TypeAbbr $ HM.fromList $ HM.elems abbr_substs
          abbr_name_substs = HM.map fst abbr_substs

      -- Check that all values are defined correctly, substituting the
      -- types first.
      vals_and_substs <- fmap HM.fromList $ forM (envVals sig) $ \(name, (spec_pts, spec_t)) -> do
        let spec_pts' = map (substituteTypes abs_subst_to_type) spec_pts
            spec_t'   = substituteTypes abs_subst_to_type spec_t
            impl_pts' = map (substituteTypes abs_subst_to_name) spec_pts
            impl_t'   = substituteTypes abs_subst_to_name spec_t
        case findBinding envVtable Term (baseName name) env of
          Just (name', BoundV t)
            | null spec_pts', toStructural t `subtypeOf` toStructural spec_t' ->
                return (name, (name', BoundV $ removeShapeAnnotations $ fromStruct impl_t'))
            | otherwise ->
                mismatchedVal loc name (spec_pts', spec_t') ([], t)
          Just (name', BoundF (pts, ret))
            | and (zipWith subtypeOf (map toStructural pts) (map toStructural spec_pts')),
              toStructural ret `subtypeOf` toStructural spec_t' ->
                return (name, (name', BoundF (impl_pts', impl_t')))
            | otherwise ->
                mismatchedVal loc (baseName name) (spec_pts', spec_t') (pts, ret)
          _ -> missingVal loc (baseName name)

      -- Check for correct modules.
      mods_and_substs <- forM (envMods sig) $ \(name, modspec) ->
        case findBinding envModTable Structure (baseName name) env of
          Just (name', mod) -> do
            (mod', mod_substs) <-
              matchMods abs_subst_to_type abs_subst_to_name mod modspec loc
            return (HM.insert name name' mod_substs,
                    (name', mod'))
          Nothing ->
            missingMod loc $ baseName name

      let vals = HM.fromList $ HM.elems vals_and_substs
          val_substs = HM.map fst vals_and_substs

          mods = HM.fromList $ map snd mods_and_substs
          mod_substs = HM.unions $ map fst mods_and_substs

          names = HM.filter (isInSig sig) $ envNameMap env

          res_env = Env { envVtable = vals
                        , envTypeTable = abbrs
                        , envSigTable = mempty
                        , envModTable = mods
                        , envNameMap = names
                        }
          all_substs = val_substs <> mod_substs <> abbr_name_substs
      return (res_env, all_substs)

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
      ppFunType spec_t ++ " in signature, but " ++ ppFunType env_t ++ " in structure."

    mismatchedVal loc name spec_t env_t =
      Left $ TypeError loc $ "Value " ++ pretty name ++ " specified as type " ++
      ppFunType spec_t ++ " in signature, but has " ++ ppFunType env_t ++ " in structure."

    findBinding :: (Env -> HM.HashMap VName v)
                  -> Namespace -> Name
                  -> Env
                  -> Maybe (VName, v)
    findBinding table namespace name the_env = do
      name' <- HM.lookup (namespace, name) $ envNameMap the_env
      (name',) <$> HM.lookup name' (table the_env)

    findTypeDef :: Name -> Mod -> Maybe (VName, TypeBinding)
    findTypeDef _ ModFun{} = Nothing
    findTypeDef name (ModEnv the_env) =
      findBinding envTypeTable Type name the_env `mplus`
      msum (map (findTypeDef name) $
            HM.elems $ envModTable the_env)

    isInSig sig x = baseName x `elem` sig_names
      where sig_names = map baseName $ HM.elems $ envNameMap sig

    ppFunType (paramts, ret) =
      intercalate " -> " $ map pretty $ paramts ++ [ret]

substituteTypesInMod :: HM.HashMap VName TypeBinding -> Mod -> Mod
substituteTypesInMod substs (ModEnv e) =
  ModEnv $ substituteTypesInEnv substs e
substituteTypesInMod substs (ModFun (FunSig abs env mty)) =
  ModFun $ FunSig abs (substituteTypesInEnv substs env) (substituteTypesInMTy substs mty)

substituteTypesInMTy :: HM.HashMap VName TypeBinding -> MTy -> MTy
substituteTypesInMTy substs (MTy abs mod) = MTy abs $ substituteTypesInMod substs mod

substituteTypesInEnv :: HM.HashMap VName TypeBinding -> Env -> Env
substituteTypesInEnv substs env =
  env { envVtable    = HM.map subV $ envVtable env
      , envTypeTable = HM.mapWithKey subT $ envTypeTable env
      , envModTable  = HM.map (substituteTypesInMod substs) $ envModTable env
      }
  where subV (BoundV t) =
          BoundV $ fromStruct $ toStructural $
          substituteTypes substs $
          vacuousShapeAnnotations $ toStruct t
        subV (BoundF (ts, t)) =
          BoundF (map (substituteTypes substs) ts,
                  substituteTypes substs t)

        subT name _
          | Just t <- HM.lookup name substs = t
        subT _ (TypeAbbr t) = TypeAbbr $ substituteTypes substs t

substituteTypes :: HM.HashMap VName TypeBinding -> StructType -> StructType
substituteTypes substs (TypeVar v)
  | Just (TypeAbbr t) <-
      HM.lookup (qualLeaf (qualNameFromTypeName v)) substs = t
  | otherwise                                              = TypeVar v
substituteTypes _ (Prim t) = Prim t
substituteTypes substs (Array at) = substituteTypesInArray at
  where substituteTypesInArray (PrimArray t shape u ()) =
          Array $ PrimArray t shape u ()
        substituteTypesInArray (PolyArray v shape u ())
          | Just (TypeAbbr t) <- HM.lookup (qualLeaf (qualNameFromTypeName v)) substs =
              arrayOf t shape u
          | otherwise =
              Array $ PolyArray v shape u ()
        substituteTypesInArray (TupleArray ts shape u) =
          Array $ TupleArray ts' shape u
          where ts' = map (flip typeToTupleArrayElem u .
                            substituteTypes substs .
                            tupleArrayElemToType) ts
substituteTypes substs (Tuple ts) = Tuple $ map (substituteTypes substs) ts

allNamesInMTy :: MTy -> HS.HashSet VName
allNamesInMTy (MTy abs mod) =
  abs <> allNamesInMod mod

allNamesInMod :: Mod -> HS.HashSet VName
allNamesInMod (ModEnv env) = allNamesInEnv env
allNamesInMod (ModFun (FunSig abs env mty)) =
  abs <> allNamesInEnv env <> allNamesInMTy mty

-- All names defined anywhere in the env.
allNamesInEnv :: Env -> HS.HashSet VName
allNamesInEnv (Env vtable ttable stable modtable _names) =
  HS.fromList (HM.keys vtable ++ HM.keys ttable ++
               HM.keys stable ++ HM.keys modtable) <>
  mconcat (map allNamesInMTy (HM.elems stable) ++
           map allNamesInMod (HM.elems modtable))

newNamesForMTy :: HM.HashMap VName VName -> MTy -> TypeM (MTy, HM.HashMap VName VName)
newNamesForMTy except orig_mty = do
  -- Create unique renames for the module type.
  substs <- fmap HM.fromList $ forM (HS.toList $ allNamesInMTy orig_mty) $ \v ->
    case HM.lookup v except of
      Just v' -> return (v, v')
      Nothing -> do v' <- newName v

                    return (v, v')

  return (substituteInMTy substs orig_mty, substs)

  where substituteInMTy substs (MTy abs mod) =
          MTy (HS.map (substitute substs) abs) (substituteInMod substs mod)

        substituteInEnv :: HM.HashMap VName VName -> Env -> Env
        substituteInEnv substs (Env vtable ttable _stable modtable names) =
          let vtable' = substituteInMap substs substituteInBinding vtable
              ttable' = substituteInMap substs substituteInTypeBinding ttable
              mtable' = substituteInMap substs substituteInMod modtable
          in Env { envVtable = vtable'
                 , envTypeTable = ttable'
                 , envSigTable = mempty
                 , envModTable = mtable'
                 , envNameMap = HM.map (substitute substs) names
                 }

        substitute substs v =
          fromMaybe v $ HM.lookup v substs

        substituteInMap substs f m =
          let (ks, vs) = unzip $ HM.toList m
          in HM.fromList $
             zip (map (\k -> fromMaybe k $ HM.lookup k substs) ks)
                 (map (f substs) vs)

        substituteInBinding :: HM.HashMap VName VName -> ValBinding
                            -> ValBinding
        substituteInBinding substs (BoundV t) =
          BoundV $ fromStruct $ toStructural $
          substituteInType substs $
          vacuousShapeAnnotations $ toStruct t
        substituteInBinding substs (BoundF (pts,t)) =
          BoundF (map (substituteInType substs) pts, substituteInType substs t)

        substituteInMod :: HM.HashMap VName VName -> Mod
                        -> Mod
        substituteInMod substs (ModEnv env) =
          ModEnv $ substituteInEnv substs env
        substituteInMod substs (ModFun funsig) =
          ModFun $ substituteInFunSig substs funsig

        substituteInFunSig substs (FunSig abs env mty) =
          FunSig (HS.map (substitute substs) abs)
          (substituteInEnv substs env) (substituteInMTy substs mty)

        substituteInTypeBinding substs (TypeAbbr t) =
          TypeAbbr $ substituteInType substs t

        substituteInType :: HM.HashMap VName VName -> StructType
                         -> StructType
        substituteInType substs =
          substituteTypes $
          HM.map (TypeAbbr . TypeVar . typeNameFromQualName . qualName) substs

mtyTypeAbbrs :: MTy -> HM.HashMap VName StructType
mtyTypeAbbrs (MTy _ (ModEnv env)) = envTypeAbbrs env
mtyTypeAbbrs (MTy _ (ModFun (FunSig _ env mty))) =
  envTypeAbbrs env <> mtyTypeAbbrs mty

-- | Refine the given type name in the given env.
refineEnv :: SrcLoc -> TySet -> Env -> QualName VName -> StructType
          -> TypeM (TySet, Env)
refineEnv loc tset env tname t
  | Just (TypeAbbr (TypeVar (TypeName _ v))) <- HM.lookup (qualLeaf tname) $ envTypeTable env,
    v `HS.member` tset =
      return (v `HS.delete` tset,
              substituteTypesInEnv (HM.fromList [(qualLeaf tname, TypeAbbr t),
                                                 (v, TypeAbbr t)])
              env)
  | otherwise =
      bad $ TypeError loc $
      pretty tname ++ " is not an abstract type in the module type."
