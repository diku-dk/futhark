{-# LANGUAGE TupleSections #-}
module Language.Futhark.TypeChecker.Modules
  ( matchMTys
  , newNamesForMTy
  , refineEnv
  , applyFunctor
  ) where

import Control.Monad.Except
import Control.Monad.Writer hiding (Sum)
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
import Language.Futhark.TypeChecker.Monad
import Language.Futhark.TypeChecker.Unify (doUnification)
import Language.Futhark.TypeChecker.Types
import Futhark.Util.Pretty hiding (indent)


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
        substituteInType (Scalar (TypeVar () u (TypeName qs v) targs)) =
          Scalar $ TypeVar () u (TypeName (map substitute qs) $ substitute v) $ map substituteInTypeArg targs
        substituteInType (Scalar (Prim t)) =
          Scalar $ Prim t
        substituteInType (Scalar (Record ts)) =
          Scalar $ Record $ fmap substituteInType ts
        substituteInType (Scalar (Sum ts)) =
          Scalar $ Sum $ (fmap . fmap) substituteInType ts
        substituteInType (Array () u t shape) =
          arrayOf (substituteInType $ Scalar t) (substituteInShape shape) u
        substituteInType (Scalar (Arrow als v t1 t2)) =
          Scalar $ Arrow als v (substituteInType t1) (substituteInType t2)

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
  | Just (tname', TypeAbbr l cur_ps (Scalar (TypeVar () _ (TypeName qs v) _))) <-
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
            mismatchedLiftedness (map qualLeaf $ M.keys mod_abs) (qualLeaf name) (ps, t)
        | Just (abs_name, _) <- M.lookup (fmap baseName name) abs_mapping ->
            return (qualLeaf name, (abs_name, TypeAbbr name_l ps t))
        | otherwise ->
            return (qualLeaf name, (name', TypeAbbr name_l ps t))
      _ ->
        missingType loc $ fmap baseName name
  where mismatchedLiftedness abs name mod_t =
          Left $ TypeError loc $
          unlines ["Module defines",
                   indent $ ppTypeAbbr abs name mod_t,
                   "but module type requires this type to be non-functional."]

missingType :: Pretty a => SrcLoc -> a -> Either TypeError b
missingType loc name =
  Left $ TypeError loc $
  "Module does not define a type named " ++ pretty name ++ "."

missingVal :: Pretty a => SrcLoc -> a -> Either TypeError b
missingVal loc name =
  Left $ TypeError loc $
  "Module does not define a value named " ++ pretty name ++ "."

missingMod :: Pretty a => SrcLoc -> a -> Either TypeError b
missingMod loc name =
  Left $ TypeError loc $
  "Module does not define a module named " ++ pretty name ++ "."

mismatchedType :: SrcLoc
               -> [VName]
               -> VName
               -> ([TypeParam], StructType)
               -> ([TypeParam], StructType)
               -> Either TypeError b
mismatchedType loc abs name spec_t env_t =
  Left $ TypeError loc $
  unlines ["Module defines",
           indent $ ppTypeAbbr abs name env_t,
           "but module type requires",
           indent $ ppTypeAbbr abs name spec_t]

indent :: String -> String
indent = intercalate "\n" . map ("  "++) . lines

ppTypeAbbr :: [VName] -> VName -> ([TypeParam], StructType) -> String
ppTypeAbbr abs name (ps, Scalar (TypeVar () _ tn args))
  | typeLeaf tn `elem` abs,
    map typeParamToArg ps == args =
      pretty $ text "type" <+> pprName name <+> spread (map ppr ps)
ppTypeAbbr _ name (ps, t) =
  pretty $ text "type" <+> pprName name <+> spread (map ppr ps) <+> equals <+/>
  nest 2 (align (ppr t))

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
      Left $ TypeError loc "Cannot match parametric module with non-parametric module type."

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
                        spec_name (spec_ps, spec_t) (ps, t)

              matchTypeParam (TypeParamDim x _) (TypeParamDim y _) =
                pure $ M.singleton x $ DimSub $ NamedDim $ qualName y
              matchTypeParam (TypeParamType Unlifted x _) (TypeParamType Unlifted y _) =
                pure $ M.singleton x $ TypeSub $ TypeAbbr Unlifted [] $
                Scalar $ TypeVar () Nonunique (typeName y) []
              matchTypeParam (TypeParamType _ x _) (TypeParamType Lifted y _) =
                pure $ M.singleton x $ TypeSub $ TypeAbbr Lifted [] $
                Scalar $ TypeVar () Nonunique (typeName y) []
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

    ppValBind v (BoundV tps t) =
      unwords $ ["val", prettyName v] ++ map pretty tps ++ [":", pretty t]

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
