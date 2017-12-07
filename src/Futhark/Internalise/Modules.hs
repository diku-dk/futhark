-- | Partially evaluate all modules away from a source Futhark
-- program.  This is implemented as a source-to-source transformation.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Internalise.Modules (transformProg) where

import Control.Monad.RWS.Strict
import Control.Monad.Identity
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Loc

import Prelude hiding (mod, abs)

import Futhark.MonadFreshNames
import Language.Futhark
import Language.Futhark.Traversals
import Language.Futhark.TypeChecker (Imports, FileModule(..))

-- | A substitution from names in the original program to names in the
-- generated/residual program.
type Substitutions = M.Map VName VName

lookupSubst :: VName -> Substitutions -> VName
lookupSubst v substs = case M.lookup v substs of
                         Just v' | v' /= v -> lookupSubst v' substs
                         _ -> v

data Mod = ModFun TySet Scope ModParam ModExp
           -- ^ A pairing of a lexical closure and a module function.
         | ModMod Scope
           -- ^ A non-parametric module.
         deriving (Show)

modScope :: Mod -> Scope
modScope (ModMod scope) = scope
modScope ModFun{} = mempty

data Scope = Scope { scopeSubsts :: Substitutions
                   , scopeMods :: M.Map VName Mod
                   }
         deriving (Show)

lookupSubstInScope :: QualName VName -> Scope -> (QualName VName, Scope)
lookupSubstInScope qn@(QualName quals name) scope@(Scope substs mods) =
  case quals of
    [] -> (QualName [] $ lookupSubst name substs, scope)
    q:qs ->
      let q' = lookupSubst q substs
      in case M.lookup q' mods of
           Just (ModMod mod_scope) -> lookupSubstInScope (QualName qs name) mod_scope
           _ -> (qn, scope)

instance Monoid Scope where
  Scope ss1 mt1 `mappend` Scope ss2 mt2 =
    Scope (ss1<>ss2) (mt1<>mt2)
  mempty = Scope mempty mempty

type TySet = S.Set VName

data Env = Env { envScope :: Scope
               , envGenerating :: Bool
               , envImports :: M.Map String Scope
               , envAbs :: TySet
               }

newtype TransformM a = TransformM (RWS Env (DL.DList Dec) VNameSource a)
                   deriving (Applicative, Functor, Monad,
                             MonadFreshNames,
                             MonadReader Env,
                             MonadWriter (DL.DList Dec))

emit :: Dec -> TransformM ()
emit = tell . DL.singleton

askScope :: TransformM Scope
askScope = asks envScope

localScope :: (Scope -> Scope) -> TransformM a -> TransformM a
localScope f = local $ \env -> env { envScope = f $ envScope env }

extendScope :: Scope -> TransformM a -> TransformM a
extendScope (Scope substs mods) = localScope $ \scope ->
  scope { scopeSubsts = M.map (forward (scopeSubsts scope)) substs <> scopeSubsts scope
        , scopeMods = mods <> scopeMods scope }
  where forward old_substs v = fromMaybe v $ M.lookup v old_substs

substituting :: Substitutions -> TransformM a -> TransformM a
substituting substs = extendScope mempty { scopeSubsts = substs }

boundName :: VName -> TransformM VName
boundName v = do g <- asks envGenerating
                 if g then newName v else return v

bindingNames :: [VName] -> TransformM Scope -> TransformM Scope
bindingNames names m = do
  names' <- mapM boundName names
  let substs = M.fromList (zip names names')
  substituting substs $ mappend <$> m <*> pure (Scope substs mempty)

generating :: TransformM a -> TransformM a
generating = local $ \env -> env { envGenerating = True }

bindingImport :: String -> Scope -> TransformM a -> TransformM a
bindingImport name scope = local $ \env ->
  env { envImports = M.insert name scope $ envImports env }

bindingAbs :: TySet -> TransformM a -> TransformM a
bindingAbs abs = local $ \env ->
  env { envAbs = abs <> envAbs env }

lookupImport :: String -> TransformM Scope
lookupImport name = maybe bad return =<< asks (M.lookup name . envImports)
  where bad = fail $ "Unknown import: " ++ name

lookupMod' :: QualName VName -> Scope -> Either String Mod
lookupMod' mname scope =
  let (mname', scope') = lookupSubstInScope mname scope
  in maybe (Left $ bad mname') Right $ M.lookup (qualLeaf mname') $ scopeMods scope'
  where bad mname' = "Unknown module: " ++ pretty mname ++ " (" ++ pretty mname' ++ ")"

lookupMod :: QualName VName -> TransformM Mod
lookupMod mname = either fail return . lookupMod' mname =<< askScope

runTransformM :: VNameSource -> TransformM a -> (a, VNameSource, DL.DList Dec)
runTransformM src (TransformM m) = runRWS m env src
  where env = Env mempty False mempty mempty

maybeAscript :: SrcLoc -> Maybe (SigExp, Info (M.Map VName VName)) -> ModExp
             -> ModExp
maybeAscript loc (Just (mtye, substs)) me = ModAscript me mtye substs loc
maybeAscript _ Nothing me = me

substituteInMod :: Substitutions -> Mod -> Mod
substituteInMod substs (ModMod (Scope mod_substs mod_mods)) =
  -- Forward all substitutions.
  ModMod $ Scope substs' $ M.map (substituteInMod substs) mod_mods
  where forward v = lookupSubst v $ mod_substs <> substs
        substs' = M.map forward substs
substituteInMod substs (ModFun abs (Scope mod_substs mod_mods) mparam mbody) =
  ModFun abs (Scope (substs'<>mod_substs) mod_mods) mparam mbody
  where forward v = lookupSubst v mod_substs
        substs' = M.map forward substs

evalModExp :: ModExp -> TransformM Mod
evalModExp (ModVar qn _) = lookupMod qn
evalModExp (ModParens e _) = evalModExp e
evalModExp (ModDecs decs _) = ModMod <$> transformDecs decs
evalModExp (ModImport _ (Info fpath) _) = ModMod <$> lookupImport fpath
evalModExp (ModAscript me _ (Info ascript_substs) _) = do
  mod <- evalModExp me
  return $ substituteInMod ascript_substs mod
evalModExp (ModApply f arg (Info p_substs) (Info b_substs) loc) = do
  f_mod <- evalModExp f
  arg_mod <- evalModExp arg
  case f_mod of
    ModMod _ ->
      fail $ "Cannot apply non-parametric module at " ++ locStr loc
    ModFun f_abs f_closure f_p f_body ->
      bindingAbs (f_abs <> S.fromList (unInfo (modParamAbs f_p))) $
      extendScope f_closure $ generating $ do
        outer_substs <- scopeSubsts <$> askScope
        abs <- asks envAbs
        let forward (k,v) = (lookupSubst k outer_substs, v)
            p_substs' = M.fromList $ map forward $ M.toList p_substs
            abs_substs = M.filterWithKey (const . flip S.member abs) $
                         p_substs' <>
                         scopeSubsts f_closure <>
                         scopeSubsts (modScope arg_mod)
        extendScope (Scope abs_substs (M.singleton (modParamName f_p) $
                                       substituteInMod p_substs' arg_mod)) $ do
          substs <- scopeSubsts <$> askScope
          x <- evalModExp f_body
          return $ addSubsts abs abs_substs $ substituteInMod (b_substs <> substs) x
  where addSubsts abs substs (ModFun mabs (Scope msubsts mods) mp me) =
          ModFun (abs<>mabs) (Scope (substs<>msubsts) mods) mp me
        addSubsts _ substs (ModMod (Scope msubsts mods)) =
          ModMod $ Scope (substs<>msubsts) mods
evalModExp (ModLambda p ascript e loc) = do
  scope <- askScope
  abs <- asks envAbs
  return $ ModFun abs scope p $ maybeAscript loc ascript e

transformName :: VName -> TransformM VName
transformName v = lookupSubst v . scopeSubsts <$> askScope

-- | A general-purpose substitution of names.
transformNames :: ASTMappable x => x -> TransformM x
transformNames x = do
  scope <- askScope
  return $ runIdentity $ astMap (substituter scope) x
  where substituter scope =
          ASTMapper { mapOnExp = onExp scope
                    , mapOnLambda = astMap $ substituter scope
                    , mapOnName = \v ->
                        return $ qualLeaf $ fst $ lookupSubstInScope (qualName v) scope
                    , mapOnQualName = \v ->
                        return $ fst $ lookupSubstInScope v scope
                    }
        onExp scope e =
          -- One expression is tricky, because it interacts with scoping rules.
          case e of
            QualParens mn e' _ ->
              case lookupMod' mn scope of
                Left err -> fail err
                Right mod ->
                  astMap (substituter $ modScope mod<>scope) e'
            _ -> astMap (substituter scope) e

transformTypeExp :: TypeExp VName -> TransformM (TypeExp VName)
transformTypeExp = transformNames

transformStructType :: StructType -> TransformM StructType
transformStructType = transformNames

transformExp :: Exp -> TransformM Exp
transformExp = transformNames

transformValBits :: VName -> Maybe (TypeExp VName) -> StructType -> Exp
                 -> TransformM (VName, Maybe (TypeExp VName), StructType, Exp)
transformValBits name tdecl t e =
  (,,,) <$> transformName name <*> traverse transformTypeExp tdecl <*>
  transformStructType t <*> transformExp e

transformValBind :: ValBind -> TransformM ()
transformValBind (ValBind entry name tdecl (Info t) e doc loc) = do
  (name', tdecl', t', e') <- transformValBits name tdecl t e
  emit $ ValDec $ ValBind entry name' tdecl' (Info t') e' doc loc

transformFunBind :: FunBind -> TransformM ()
transformFunBind (FunBind entry name tdecl (Info t) tparams params e doc loc) = do
  (name', tdecl', t', e') <- transformValBits name tdecl t e
  tparams' <- traverse transformNames tparams
  params' <- traverse transformNames params
  emit $ FunDec $ FunBind entry name' tdecl' (Info t') tparams' params' e' doc loc

transformTypeDecl :: TypeDecl -> TransformM TypeDecl
transformTypeDecl (TypeDecl dt (Info et)) =
  TypeDecl <$> transformTypeExp dt <*> (Info <$> transformStructType et)

transformTypeBind :: TypeBind -> TransformM ()
transformTypeBind (TypeBind name tparams te doc loc) = do
  name' <- transformName name
  emit =<< TypeDec <$> (TypeBind name' <$> traverse transformNames tparams
                        <*> transformTypeDecl te <*> pure doc <*> pure loc)

transformModBind :: ModBind -> TransformM Scope
transformModBind mb = do
  let addParam p me = ModLambda p Nothing me $ srclocOf me
  mod <- evalModExp $ foldr addParam
         (maybeAscript (srclocOf mb) (modSignature mb) $ modExp mb) $
         modParams mb
  mname <- transformName $ modName mb
  return $ Scope mempty $ M.singleton mname mod

transformDecs :: [Dec] -> TransformM Scope
transformDecs ds =
  case ds of
    [] ->
      return mempty
    LocalDec d _ : ds' ->
      transformDecs $ d : ds'
    ValDec vdec : ds' ->
      bindingNames [constBindName vdec] $ do
        transformValBind vdec
        transformDecs ds'
    FunDec fdec : ds' ->
      bindingNames [funBindName fdec] $ do
        transformFunBind fdec
        transformDecs ds'
    TypeDec tb : ds' ->
      bindingNames [typeAlias tb] $ do
        transformTypeBind tb
        transformDecs ds'
    SigDec {} : ds' ->
      transformDecs ds'
    ModDec mb : ds' ->
      bindingNames [modName mb] $ do
        mod_scope <- transformModBind mb
        extendScope mod_scope $ mappend <$> transformDecs ds' <*> pure mod_scope
    OpenDec e es _ _ : ds' -> do
      mods <- mapM evalModExp $ e:es
      let scope = mconcat $ reverse $ map modScope mods
      extendScope scope $ mappend <$> transformDecs ds' <*> pure scope

transformImports :: Imports -> TransformM ()
transformImports [] = return ()
transformImports ((name,imp):imps) = do
  let abs = S.fromList $ map qualLeaf $ S.toList $ fileAbs imp
  scope <- censor (fmap maybeHideEntryPoint) $
           bindingAbs abs $ transformDecs $ progDecs $ fileProg imp
  bindingAbs abs $ bindingImport name scope $ transformImports imps
  where
    -- Only the "main" file (last import) is allowed to have entry points.
    permit_entry_points = null imps

    maybeHideEntryPoint (FunDec fdec) =
      FunDec fdec { funBindEntryPoint =
                      funBindEntryPoint fdec && permit_entry_points }
    maybeHideEntryPoint (ValDec vdec) =
      ValDec vdec { constBindEntryPoint =
                      constBindEntryPoint vdec && permit_entry_points }
    maybeHideEntryPoint d = d

transformProg :: MonadFreshNames m => Imports -> m [Dec]
transformProg prog = modifyNameSource $ \namesrc ->
  let ((), namesrc', prog') = runTransformM namesrc $ transformImports prog
  in (DL.toList prog', namesrc')
