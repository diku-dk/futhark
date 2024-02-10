-- | Lambda-lifting of typed, monomorphic Futhark programs without
-- modules.  After this pass, the program will no longer contain any
-- 'LetFun's or 'Lambda's.
module Futhark.Internalise.LiftLambdas (transformProg) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import Data.List (partition)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.IR.Pretty ()
import Futhark.MonadFreshNames
import Futhark.Util (nubOrd)
import Language.Futhark
import Language.Futhark.Traversals

data Env = Env
  { envReplace :: M.Map VName (StructType -> Exp),
    envVtable :: M.Map VName StructType
  }

initialEnv :: Env
initialEnv = Env mempty mempty

data LiftState = State
  { stateNameSource :: VNameSource,
    stateValBinds :: [ValBind],
    stateGlobal :: S.Set VName
  }

initialState :: VNameSource -> LiftState
initialState src = State src mempty $ S.fromList $ M.keys intrinsics

newtype LiftM a = LiftM (ReaderT Env (State LiftState) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState LiftState)

instance MonadFreshNames LiftM where
  putNameSource src = modify $ \s -> s {stateNameSource = src}
  getNameSource = gets stateNameSource

runLiftM :: VNameSource -> LiftM () -> ([ValBind], VNameSource)
runLiftM src (LiftM m) =
  let s = execState (runReaderT m initialEnv) (initialState src)
   in (reverse (stateValBinds s), stateNameSource s)

addValBind :: ValBind -> LiftM ()
addValBind vb = modify $ \s ->
  s
    { stateValBinds = vb : stateValBinds s,
      stateGlobal = foldl' (flip S.insert) (stateGlobal s) (valBindBound vb)
    }

replacing :: VName -> (StructType -> Exp) -> LiftM a -> LiftM a
replacing v e = local $ \env ->
  env {envReplace = M.insert v e $ envReplace env}

bindingParams :: [VName] -> [Pat ParamType] -> LiftM a -> LiftM a
bindingParams sizes params = local $ \env ->
  env
    { envVtable =
        M.fromList (map (second toStruct) (foldMap patternMap params) <> map (,i64) sizes)
          <> envVtable env
    }
  where
    i64 = Scalar $ Prim $ Signed Int64

bindingLetPat :: [VName] -> Pat StructType -> LiftM a -> LiftM a
bindingLetPat sizes pat = local $ \env ->
  env
    { envVtable =
        M.fromList (map (second toStruct) (patternMap pat) <> map (,i64) sizes)
          <> envVtable env
    }
  where
    i64 = Scalar $ Prim $ Signed Int64

bindingForm :: LoopFormBase Info VName -> LiftM a -> LiftM a
bindingForm (For i _) = bindingLetPat [] (Id (identName i) (identType i) mempty)
bindingForm (ForIn p _) = bindingLetPat [] p
bindingForm While {} = id

toRet :: TypeBase Size u -> TypeBase Size Uniqueness
toRet = second (const Nonunique)

liftFunction :: VName -> [TypeParam] -> [Pat ParamType] -> ResRetType -> Exp -> LiftM (StructType -> Exp)
liftFunction fname tparams params (RetType dims ret) funbody = do
  -- Find free variables
  vtable <- asks envVtable
  let isFree v = (v,) <$> M.lookup v vtable
      withTypes = mapMaybe isFree . S.toList . fvVars

  let free =
        let immediate_free = withTypes $ freeInExp funbody
            sizes_in_free = foldMap (freeInType . snd) immediate_free
            sizes =
              withTypes $
                sizes_in_free
                  <> foldMap freeInPat params
                  <> freeInType ret
         in nubOrd $ immediate_free <> sizes

      -- Those parameters that correspond to sizes must come first.
      sizes_in_types =
        foldMap freeInType (toStruct ret : map snd free ++ map patternStructType params)
      isSize (v, _) = v `S.member` fvVars sizes_in_types
      (free_dims, free_nondims) = partition isSize free

      free_ts = map (second (`setUniqueness` Nonunique)) $ free_dims ++ free_nondims

  addValBind $
    ValBind
      { valBindName = fname,
        valBindTypeParams = tparams,
        valBindParams = map mkParam free_ts ++ params,
        valBindRetDecl = Nothing,
        valBindRetType = Info (RetType dims ret),
        valBindBody = funbody,
        valBindDoc = Nothing,
        valBindAttrs = mempty,
        valBindLocation = mempty,
        valBindEntryPoint = Nothing
      }

  pure $ \orig_type ->
    apply
      orig_type
      (Var (qualName fname) (Info (augType free_ts orig_type)) mempty)
      $ free_dims ++ free_nondims
  where
    mkParam (v, t) = Id v (Info (toParam Observe t)) mempty
    freeVar (v, t) = Var (qualName v) (Info t) mempty
    augType rem_free orig_type = funType (map mkParam rem_free) $ RetType [] $ toRet orig_type

    apply :: StructType -> Exp -> [(VName, StructType)] -> Exp
    apply _ f [] = f
    apply orig_type f (p : rem_ps) =
      let inner_ret = AppRes (augType rem_ps orig_type) mempty
          inner = mkApply f [(Nothing, freeVar p)] inner_ret
       in apply orig_type inner rem_ps

transformSubExps :: ASTMapper LiftM
transformSubExps = identityMapper {mapOnExp = transformExp}

transformExp :: Exp -> LiftM Exp
transformExp (AppExp (LetFun fname (tparams, params, _, Info ret, funbody) body _) _) = do
  funbody' <- bindingParams (map typeParamName tparams) params $ transformExp funbody
  fname' <- newVName $ "lifted_" ++ baseString fname
  lifted_call <- liftFunction fname' tparams params ret funbody'
  replacing fname lifted_call $ transformExp body
transformExp e@(Lambda params body _ (Info ret) _) = do
  body' <- bindingParams [] params $ transformExp body
  fname <- newVName "lifted_lambda"
  liftFunction fname [] params ret body' <*> pure (typeOf e)
transformExp (AppExp (LetPat sizes pat e body loc) appres) = do
  e' <- transformExp e
  body' <- bindingLetPat (map sizeName sizes) pat $ transformExp body
  pure $ AppExp (LetPat sizes pat e' body' loc) appres
transformExp (AppExp (Match e cases loc) appres) = do
  e' <- transformExp e
  cases' <- mapM transformCase cases
  pure $ AppExp (Match e' cases' loc) appres
  where
    transformCase (CasePat case_pat case_e case_loc) =
      CasePat case_pat
        <$> bindingLetPat [] case_pat (transformExp case_e)
        <*> pure case_loc
transformExp (AppExp (Loop sizes pat args form body loc) appres) = do
  args' <- transformExp args
  bindingParams sizes [pat] $ do
    form' <- astMap transformSubExps form
    body' <- bindingForm form' $ transformExp body
    pure $ AppExp (Loop sizes pat args' form' body' loc) appres
transformExp e@(Var v (Info t) _) =
  -- Note that function-typed variables can only occur in expressions,
  -- not in other places where VNames/QualNames can occur.
  asks $ maybe e ($ t) . M.lookup (qualLeaf v) . envReplace
transformExp e = astMap transformSubExps e

transformValBind :: ValBind -> LiftM ()
transformValBind vb = do
  e <-
    bindingParams (map typeParamName $ valBindTypeParams vb) (valBindParams vb) $
      transformExp (valBindBody vb)
  addValBind $ vb {valBindBody = e}

{-# NOINLINE transformProg #-}

-- | Perform the transformation.
transformProg :: (MonadFreshNames m) => [ValBind] -> m [ValBind]
transformProg vbinds =
  modifyNameSource $ \namesrc ->
    runLiftM namesrc $ mapM_ transformValBind vbinds
