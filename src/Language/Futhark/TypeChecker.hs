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

import Prelude hiding (mapM)

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
                              (ID (nameFromString "intrinsics", 0))
                              topLevelNameMap
               }
  where initialTypeTable = HM.fromList $ mapMaybe addIntrinsicT $ HM.toList intrinsics
        initialModTable = HM.singleton (nameFromString "intrinsics")
                          (ModMod intrinsicsModule)

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

checkSpecs :: [SpecBase NoInfo Name] -> TypeM (Env, [SpecBase Info VName])

checkSpecs [] = return (mempty, [])

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
    (env, specs') <- localEnv (valenv<>) $ checkSpecs specs
    return (env <> valenv,
            ValSpec name' paramtypes' rettype' loc : specs')

checkSpecs (TypeAbbrSpec tdec : specs) =
  bindSpaced [(Type, typeAlias tdec)] $ do
    (tenv, tdec') <- checkTypeBind tdec
    (env, specs') <- localEnv (tenv<>) $ checkSpecs specs
    return (tenv <> env,
            TypeAbbrSpec tdec' : specs')

checkSpecs (TypeSpec name loc : specs) =
  bindSpaced [(Type, name)] $ do
    name' <- checkName Type name loc
    let tenv = mempty { envNameMap = HM.singleton (Type, name) name'
                        , envTypeTable = HM.singleton name' TypeAbs
                        }
    (env, specs') <- localEnv (tenv<>) $ checkSpecs specs
    return (tenv <> env,
            TypeSpec name' loc : specs')

checkSpecs (IncludeSpec e loc : specs) = do
  (e_env, e') <- checkSigExp e
  (env, specs') <- localEnv (e_env<>) $ checkSpecs specs
  return (e_env <> env,
          IncludeSpec e' loc : specs')

checkSigExp :: SigExpBase NoInfo Name -> TypeM (Env, SigExpBase Info VName)
checkSigExp (SigVar name loc) = do
  (name', env) <- lookupSig loc name
  return (env, SigVar name' loc)
checkSigExp (SigSpecs specs loc) = do
  checkForDuplicateSpecs specs
  (env, specs') <- checkSpecs specs
  return (env, SigSpecs specs' loc)
checkSigExp (SigWith s (TypeRef tname td) loc) = do
  (s_env, s') <- checkSigExp s
  td' <- checkTypeDecl td
  tname' <- localEnv (s_env<>) $ snd <$> checkQualName Type tname loc
  s_env' <- refineEnv loc s_env tname' $ unInfo $ expandedType td'
  return (s_env', SigWith s' (TypeRef tname' td') loc)

checkSigBind :: SigBindBase NoInfo Name -> TypeM (Env, SigBindBase Info VName)
checkSigBind (SigBind name e loc) = do
  name' <- checkName Signature name loc
  (env, e') <- checkSigExp e
  -- As a small convenience(?), binding a signature also implicitly
  -- binds a structure of the same name, which contains the type
  -- abbreviations in the signature.
  let sigmod = typeAbbrEnvFromSig env
  return (mempty { envSigTable = HM.singleton name' env
                 , envModTable = HM.singleton name $ ModMod sigmod
                 , envNameMap = HM.singleton (Signature, name) name'
                 },
          SigBind name' e' loc)
  where typeAbbrEnvFromSig :: Env -> Env
        typeAbbrEnvFromSig env =
          let types = HM.map TypeAbbr $ HM.fromList $ envTypeAbbrs env
              names = HM.fromList $ map nameMapping $ HM.toList types
          in mempty { envNameMap = names
                    , envTypeTable = types }
        nameMapping (v, _) = ((Type, baseName v), v)

checkModExp :: ModExpBase NoInfo Name -> TypeM (Env, ModExpBase Info VName)
checkModExp (ModDecs decs loc) = do
  checkForDuplicateDecs decs
  (env, decs') <- checkDecs decs
  return (env, ModDecs decs' loc)
checkModExp (ModVar v loc) = do
  (v', env) <- lookupMod loc v
  when (baseName (qualLeaf v') == nameFromString "intrinsics" &&
        baseTag (qualLeaf v') <= maxIntrinsicTag) $
    bad $ TypeError loc "The 'intrinsics' module may not be used in module expressions."
  return (env, ModVar v' loc)
checkModExp (ModImport name loc) = do
  env <- lookupImport loc name
  return (env, ModImport name loc)
checkModExp (ModApply f e NoInfo NoInfo loc) = do
  (f', functor) <- lookupFunctor loc f
  (e_env, e') <- checkModExp e
  (f_env, psubsts, rsubsts) <- applyFunctor functor loc e_env
  return (f_env, ModApply f' e' (Info psubsts) (Info rsubsts) loc)
checkModExp (ModAscript me se NoInfo loc) = do
  (env, me') <- checkModExp me
  (sigenv, se') <- checkSigExp se
  (env', _) <- badOnLeft $ matchEnvs env sigenv loc
  -- See issue #262 for martinsubst justification.
  (env'', martinsubst) <- newNamesForEnv mempty env'
  return (env'', ModAscript me' se' (Info martinsubst) loc)

checkStructBind :: StructBindBase NoInfo Name -> TypeM (Env, StructBindBase Info VName)
checkStructBind (StructBind name e loc) = do
  name' <- checkName Structure name loc
  (env, e') <- checkModExp e
  return (mempty { envModTable = HM.singleton name $ ModMod env
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

        f IncludeSpec{} =
          return


checkFunctorBind :: FunctorBindBase NoInfo Name -> TypeM (Env, FunctorBindBase Info VName)
checkFunctorBind (FunctorBind name (p, psig_e) maybe_fsig_e body_e loc) = do
  name' <- checkName Structure name loc
  (p_env, psig_e') <- checkSigExp psig_e
  bindSpaced [(Structure, p)] $ do
    p' <- checkName Structure p loc
    let in_body_env = mempty { envModTable = HM.singleton p $ ModMod p_env }
    localEnv (in_body_env<>) $ do
      (body_env, body_e') <- checkModExp body_e
      (maybe_fsig_e', env') <-
        case maybe_fsig_e of
          Nothing ->
            return (Nothing, body_env)
          Just fsig_e -> do
            (fsig_env, fsig_e') <- checkSigExp fsig_e
            (env', _) <- badOnLeft $ matchEnvs body_env fsig_env loc
            return (Just fsig_e', env')
      return (mempty { envModTable =
                         HM.singleton name $
                         ModFunctor $ FunctorF $ apply env' p_env
                     , envNameMap =
                         HM.singleton (Structure, name) name'
                     },
              FunctorBind name' (p', psig_e') maybe_fsig_e' body_e' loc)

  where apply body_env p_sig applyloc a_env = do
          (_, sig_subst) <- badOnLeft $ matchEnvs a_env p_sig applyloc

          -- Type substitutions from the argument env.  Importantly,
          -- we use the env before it is restricted by the parameter
          -- signature ascription.  There are two important things we
          -- must do: turn abstract types into type abbreviations (if
          -- applicable), and fix references to abstract types.  The
          -- body_env will refer to names in p_sig, and we'll have
          -- to refer to those in a_env instead.  sig_subst gives us
          -- exactly the type name mappings, but we'll need some care
          -- for the abbreviations.
          let type_substs = envTypeAbbrs a_env
              typeSubst v
                | Just t <- lookup v type_substs = TypeAbbr t
                | otherwise                      = TypeAbbr $ TypeVar $ typeName v
              type_substs'' = HM.map typeSubst sig_subst

          (body_env', body_subst) <-
            newNamesForEnv sig_subst $
            substituteTypesInEnv type_substs'' body_env
          return (body_env', sig_subst, body_subst)

checkTypeBind :: TypeBindBase NoInfo Name
              -> TypeM (Env, TypeBindBase Info VName)
checkTypeBind (TypeBind name td loc) = do
  name' <- checkName Type name loc
  td' <- checkTypeDecl td
  return (mempty { envTypeTable =
                     HM.singleton name' $ TypeAbbr $ unInfo $ expandedType td',
                   envNameMap =
                     HM.singleton (Type, name) name'
                 },
          TypeBind name' td' loc)

checkDecs :: [DecBase NoInfo Name] -> TypeM (Env, [DecBase Info VName])
checkDecs (StructDec struct:rest) =
  bindSpaced [(Structure, structName struct)] $ do
    (modenv, struct') <- checkStructBind struct
    localEnv (modenv<>) $ do
      (env, rest') <- checkDecs rest
      return (env <> modenv, StructDec struct' : rest')

checkDecs (SigDec sig:rest) =
  bindSpaced [(Signature, sigName sig)] $ do
    (sigenv, sig') <- checkSigBind sig
    localEnv (sigenv<>) $ do
      (env, rest') <- checkDecs rest
      return (env <> sigenv, SigDec sig' : rest')

checkDecs (FunctorDec func:rest) =
  bindSpaced [(Structure, functorName func)] $ do
    (funcenv, func') <- checkFunctorBind func
    localEnv (funcenv<>) $ do
      (env, rest') <- checkDecs rest
      return (funcenv <> env, FunctorDec func' : rest')

checkDecs (TypeDec tdec:rest) =
  bindSpaced [(Type, typeAlias tdec)] $ do
    (tenv, tdec') <- checkTypeBind tdec
    localEnv (tenv<>) $ do
      (env, rest') <- checkDecs rest
      return (env <> tenv, TypeDec tdec' : rest')

checkDecs (OpenDec x xs loc:rest) = do
  (x_mod, x') <- checkModExp x
  (xs_mods, xs') <- unzip <$> mapM checkModExp xs
   -- We cannot use mconcat, as mconcat is a right-fold.
  let env_ext = foldl (flip mappend) x_mod xs_mods
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

-- Return new renamed/abstracted env, as well as a mapping from
-- names in the signature to names in the new env.  This is used for
-- functor application.  The first env is the module env, and the
-- second the env it must match.
matchEnvs :: Env -> Env -> SrcLoc
            -> Either TypeError (Env, HM.HashMap VName VName)
matchEnvs env sig loc = do
  -- Check that abstract types in 'sig' have an implementation in
  -- 'env'.  This also gives us a substitution that we use to check
  -- the types of values.
  abs_substs <- fmap HM.fromList $ forM (envAbsTypes sig) $ \name ->
    case findBinding envTypeTable Type (baseName name) of
      Just (name', TypeAbs) ->
        return (name, (name', TypeAbbr $ TypeVar $ typeName name'))
      Just (name', TypeAbbr t) ->
        return (name, (name', TypeAbbr t))
      Nothing ->
        missingType $ baseName name

  let abs_names = map fst $ HM.elems abs_substs
      abs_subst_to_type = HM.map snd abs_substs
      abs_subst_to_name = HM.map (TypeAbbr . TypeVar . typeName . fst) abs_substs
      abs_name_substs   = HM.map fst abs_substs

  -- Check that all type abbreviations are correctly defined.
  abbr_substs <- fmap HM.fromList $ forM (envTypeAbbrs sig) $ \(name,spec_t) -> do
    let spec_t' = substituteTypes abs_subst_to_type spec_t
    case findBinding envTypeTable Type (baseName name) of
      Just (name', TypeAbbr t)
        | spec_t' == t ->
            return (name, (name', substituteTypes abs_subst_to_type spec_t))
        | otherwise ->
            mismatchedType (baseName name) ([], spec_t) ([], t)
      Just (name', TypeAbs)
        | TypeVar (typeName name') == spec_t ->
            return (name, (name', substituteTypes abs_subst_to_type spec_t))
        | otherwise ->
          Left $ TypeError loc $
          "Type abbreviation " ++ pretty (baseName name) ++ " = " ++ pretty spec_t ++
          " defined as abstract in module."
      Nothing -> missingType $ baseName name

  let abbrs = HM.map TypeAbbr $ HM.fromList $ HM.elems abbr_substs
      abbr_name_substs = HM.map fst abbr_substs

  -- Check that all values are defined correctly, substituting the
  -- types first.
  vals_and_substs <- fmap HM.fromList $ forM (envVals sig) $ \(name, (spec_pts, spec_t)) -> do
    let spec_pts' = map (substituteTypes abs_subst_to_type) spec_pts
        spec_t'   = substituteTypes abs_subst_to_type spec_t
        impl_pts' = map (substituteTypes abs_subst_to_name) spec_pts
        impl_t'   = substituteTypes abs_subst_to_name spec_t
    case findBinding envVtable Term $ baseName name of
      Just (name', BoundV t)
        | null spec_pts', toStructural t `subtypeOf` toStructural spec_t' ->
            return (name, (name', BoundV $ removeShapeAnnotations $ fromStruct impl_t'))
        | otherwise ->
            mismatchedVal name (spec_pts', spec_t') ([], t)
      Just (name', BoundF (pts, ret))
        | and (zipWith subtypeOf (map toStructural pts) (map toStructural spec_pts')),
          toStructural ret `subtypeOf` toStructural spec_t' ->
            return (name, (name', BoundF (impl_pts', impl_t')))
        | otherwise ->
            mismatchedVal (baseName name) (spec_pts', spec_t') (pts, ret)
      _ -> missingVal (baseName name)

  let vals = HM.fromList $ HM.elems vals_and_substs
      val_substs = HM.map fst vals_and_substs

      names = HM.filter isInSig $ envNameMap env
      types = abbrs <> HM.fromList (zip abs_names $ repeat TypeAbs)
      res_env = Env { envVtable = vals
                        , envTypeTable = types
                        , envSigTable = mempty
                        , envModTable = mempty
                        , envNameMap = names
                        }
  return (res_env,
          abs_name_substs <> abbr_name_substs <> val_substs)
  where missingType name =
          Left $ TypeError loc $
          "Structure does not define a type named " ++ pretty name ++ "."

        missingVal name =
          Left $ TypeError loc $
          "Structure does not define a value named " ++ pretty name ++ "."

        mismatchedType name spec_t env_t =
          Left $ TypeError loc $ "Type " ++ pretty name ++ " specified as " ++
          ppFunType spec_t ++ " in signature, but " ++ ppFunType env_t ++ " in structure."

        mismatchedVal name spec_t env_t =
          Left $ TypeError loc $ "Value " ++ pretty name ++ " specified as type " ++
          ppFunType spec_t ++ " in signature, but has " ++ ppFunType env_t ++ " in structure."

        findBinding :: (Env -> HM.HashMap VName v)
                    -> Namespace -> Name
                    -> Maybe (VName, v)
        findBinding table namespace name = do
          name' <- HM.lookup (namespace, name) $ envNameMap env
          (name',) <$> HM.lookup name' (table env)

        isInSig x = baseName x `elem` sig_names
          where sig_names = map baseName $ HM.elems $ envNameMap sig

        ppFunType (paramts, ret) =
          intercalate " -> " $ map pretty $ paramts ++ [ret]

substituteTypesInEnv :: HM.HashMap VName TypeBinding -> Env -> Env
substituteTypesInEnv substs env =
  env { envVtable    = HM.map subV $ envVtable env
        , envTypeTable = HM.mapWithKey subT $ envTypeTable env }
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
        subT _ TypeAbs      = TypeAbs

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

-- New names for everything defined in the env, with passed-in
-- exceptions.  Removes signatures.
newNamesForEnv :: HM.HashMap VName VName -> Env -> TypeM (Env, HM.HashMap VName VName)
newNamesForEnv except orig_env = do
  -- Create unique renames for the env.
  let rename (k, v)
        | Just v' <- HM.lookup v except =
            return ((k,v'), (v,v'))
        | otherwise = do v' <- newName v
                         return ((k,v'), (v,v'))
  (names_list, substs_list) <-
    mapAndUnzipM rename $ HM.toList $ envNameMap orig_env
  let names = HM.fromList names_list
      substs = HM.fromList substs_list

  -- Now we have to substitute everything else in the env.
  return ((substituteInEnv substs orig_env) { envNameMap = names },
          substs)
  where substituteInEnv :: HM.HashMap VName VName -> Env -> Env
        substituteInEnv substs env =
          Env { envVtable = substituteInMap substs substituteInBinding $
                              envVtable env
                , envTypeTable = substituteInMap substs substituteInTypeBinding $
                                 envTypeTable env
                , envSigTable = mempty
                , envModTable = HM.map (substituteInModBinding substs) $
                                envModTable env
                , envNameMap = mempty
                }

        substituteInMap substs f m =
          let (ks, vs) = unzip $ HM.toList m
          in HM.fromList $
             zip (map (\k -> fromMaybe k $ HM.lookup k substs) ks)
                 (map (f substs) vs)

        substituteInBinding :: HM.HashMap VName VName -> ValBinding -> ValBinding
        substituteInBinding substs (BoundV t) =
          BoundV $ fromStruct $ toStructural $
          substituteInType substs $
          vacuousShapeAnnotations $ toStruct t
        substituteInBinding substs (BoundF (pts,t)) =
          BoundF (map (substituteInType substs) pts,
                  substituteInType substs t)

        substituteInModBinding :: HM.HashMap VName VName -> ModBinding -> ModBinding
        substituteInModBinding substs (ModMod env) =
          ModMod $ substituteInEnv substs env
        substituteInModBinding substs (ModFunctor f) =
          ModFunctor $ FunctorF $ \loc env -> do
          -- Nested functor or something?  This should never happen!
          (f_env, f_sig_subst, f_body_subst) <- applyFunctor f loc env
          return (substituteInEnv substs f_env,
                  substs <> f_sig_subst, substs <> f_body_subst)

        substituteInTypeBinding substs (TypeAbbr t) =
          TypeAbbr $ substituteInType substs t
        substituteInTypeBinding _ TypeAbs =
          TypeAbs

        substituteInType :: HM.HashMap VName VName -> StructType -> StructType
        substituteInType substs = substituteTypes $
                                  HM.map (TypeAbbr . TypeVar . typeNameFromQualName . qualName) substs

-- | Refine the given type name in the given env.
refineEnv :: SrcLoc -> Env -> QualName VName -> StructType -> TypeM Env
refineEnv loc env tname t =
  -- tname must be an abstract type in env.
  case HM.lookup (qualLeaf tname) $ envTypeTable env of
    Nothing ->
      bad $ TypeError loc $
      pretty tname ++ " is not a known type in the module type."
    Just TypeAbbr{} ->
      bad $ TypeError loc $
      pretty tname ++ " is not an abstract type in the module type."
    Just TypeAbs ->
      let subst = HM.singleton (qualLeaf tname) $ TypeAbbr t
      in return $ substituteTypesInEnv subst env
