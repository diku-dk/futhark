-- | Converts identifiers of record type into record patterns (and
-- similarly for tuples).  This is to ensure that the closures
-- produced in lambda lifting and defunctionalisation do not carry
-- around huge records of which only a tiny part is needed.
module Futhark.Internalise.ReplaceRecords (transformProg) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Bitraversable
import Data.Map.Strict qualified as M
import Futhark.MonadFreshNames
import Language.Futhark
import Language.Futhark.Traversals

-- Mapping from record names to the variable names that contain the
-- fields.  This is used because the monomorphiser also expands all
-- record patterns.
type RecordReplacements = M.Map VName RecordReplacement

type RecordReplacement = M.Map Name (VName, StructType)

newtype Env = Env
  { envRecordReplacements :: RecordReplacements
  }

data S = S
  { stateNameSource :: VNameSource,
    stateStructTypeMemo :: M.Map StructType StructType,
    stateParamTypeMemo :: M.Map ParamType ParamType
  }

-- The monomorphization monad.
newtype RecordM a
  = RecordM (ReaderT Env (State S) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState S
    )

instance MonadFreshNames RecordM where
  getNameSource = RecordM $ gets stateNameSource
  putNameSource src = RecordM $ modify $ \s -> s {stateNameSource = src}

runRecordM :: VNameSource -> RecordM a -> (a, VNameSource)
runRecordM src (RecordM m) =
  second stateNameSource $
    runState (runReaderT m (Env mempty)) (S src mempty mempty)

withRecordReplacements :: RecordReplacements -> RecordM a -> RecordM a
withRecordReplacements rr = local $ \env ->
  env {envRecordReplacements = rr <> envRecordReplacements env}

lookupRecordReplacement :: VName -> RecordM (Maybe RecordReplacement)
lookupRecordReplacement v = asks $ M.lookup v . envRecordReplacements

wildcard :: TypeBase Size u -> SrcLoc -> Pat (TypeBase Size u)
wildcard (Scalar (Record fs)) loc =
  RecordPat (zip (M.keys fs) $ map ((`Wildcard` loc) . Info) $ M.elems fs) loc
wildcard t loc =
  Wildcard (Info t) loc

memoParamType :: ParamType -> RecordM ParamType -> RecordM ParamType
memoParamType t m = do
  prev <- gets $ M.lookup t . stateParamTypeMemo
  case prev of
    Just t' -> pure t'
    Nothing -> do
      t' <- m
      modify $ \s -> s {stateParamTypeMemo = M.insert t t' $ stateParamTypeMemo s}
      pure t'

memoStructType :: StructType -> RecordM StructType -> RecordM StructType
memoStructType t m = do
  prev <- gets $ M.lookup t . stateStructTypeMemo
  case prev of
    Just t' -> pure t'
    Nothing -> do
      t' <- m
      modify $ \s -> s {stateStructTypeMemo = M.insert t t' $ stateStructTypeMemo s}
      pure t'

-- No need to keep memoisation cache between top level functions.
memoClear :: RecordM ()
memoClear = modify $ \s ->
  s
    { stateStructTypeMemo = mempty,
      stateParamTypeMemo = mempty
    }

transformPat ::
  (TypeBase Size u -> RecordM (TypeBase Size u)) ->
  Pat (TypeBase Size u) ->
  RecordM (Pat (TypeBase Size u), RecordReplacements)
transformPat _ (Id v (Info (Scalar (Record fs))) loc) = do
  let fs' = M.toList fs
  (fs_ks, fs_ts) <- fmap unzip $
    forM fs' $ \(f, ft) ->
      (,) <$> newVName (nameToString f) <*> pure ft
  pure
    ( RecordPat
        (zip (map fst fs') (zipWith3 Id fs_ks (map Info fs_ts) $ repeat loc))
        loc,
      M.singleton v $ M.fromList $ zip (map fst fs') $ zip fs_ks $ map toStruct fs_ts
    )
transformPat onType (Id v t loc) = do
  t' <- traverse onType t
  pure (Id v t' loc, mempty)
transformPat onType (TuplePat pats loc) = do
  (pats', rrs) <- mapAndUnzipM (transformPat onType) pats
  pure (TuplePat pats' loc, mconcat rrs)
transformPat onType (RecordPat fields loc) = do
  let (field_names, field_pats) = unzip fields
  (field_pats', rrs) <- mapAndUnzipM (transformPat onType) field_pats
  pure (RecordPat (zip field_names field_pats') loc, mconcat rrs)
transformPat onType (PatParens pat loc) = do
  (pat', rr) <- transformPat onType pat
  pure (PatParens pat' loc, rr)
transformPat onType (PatAttr attr pat loc) = do
  (pat', rr) <- transformPat onType pat
  pure (PatAttr attr pat' loc, rr)
transformPat onType (Wildcard (Info t) loc) = do
  t' <- onType t
  pure (wildcard t' loc, mempty)
transformPat onType (PatAscription pat _ _) =
  transformPat onType pat
transformPat _ (PatLit e t loc) =
  pure (PatLit e t loc, mempty)
transformPat onType (PatConstr name t all_ps loc) = do
  (all_ps', rrs) <- mapAndUnzipM (transformPat onType) all_ps
  pure (PatConstr name t all_ps' loc, mconcat rrs)

transformParamType :: ParamType -> RecordM ParamType
transformParamType t = memoParamType t $ bitraverse transformExp pure t

transformStructType :: StructType -> RecordM StructType
transformStructType t = memoStructType t $ bitraverse transformExp pure t

transformExp :: Exp -> RecordM Exp
transformExp (Project n e t loc) = do
  maybe_fs <- case e of
    Var qn _ _ -> lookupRecordReplacement (qualLeaf qn)
    _ -> pure Nothing
  case maybe_fs of
    Just m
      | Just (v, _) <- M.lookup n m ->
          pure $ Var (qualName v) t loc
    _ -> do
      e' <- transformExp e
      pure $ Project n e' t loc
transformExp (Var fname t loc) = do
  maybe_fs <- lookupRecordReplacement $ qualLeaf fname
  case maybe_fs of
    Just fs -> do
      let toField (f, (f_v, f_t)) = do
            let f_v' = Var (qualName f_v) (Info f_t) loc
            pure $ RecordFieldExplicit f f_v' loc
      RecordLit <$> mapM toField (M.toList fs) <*> pure loc
    Nothing ->
      Var fname <$> traverse transformStructType t <*> pure loc
transformExp (AppExp (LetPat sizes pat e body loc) res) = do
  e' <- transformExp e
  (pat', rr) <- transformPat transformStructType pat
  body' <- withRecordReplacements rr $ transformExp body
  pure $ AppExp (LetPat sizes pat' e' body' loc) res
transformExp (AppExp (LetFun fname (tparams, params, retdecl, ret, funbody) body loc) res) = do
  (params', rrs) <- mapAndUnzipM (transformPat transformParamType) params
  funbody' <- withRecordReplacements (mconcat rrs) $ transformExp funbody
  body' <- transformExp body
  pure $ AppExp (LetFun fname (tparams, params', retdecl, ret, funbody') body' loc) res
transformExp (Lambda params body retdecl ret loc) = do
  (params', rrs) <- mapAndUnzipM (transformPat transformParamType) params
  body' <- withRecordReplacements (mconcat rrs) $ transformExp body
  pure $ Lambda params' body' retdecl ret loc
transformExp e = astMap m e
  where
    m = identityMapper {mapOnExp = transformExp}

onValBind :: ValBind -> RecordM ValBind
onValBind vb = do
  (params', rrs) <- mapAndUnzipM (transformPat transformParamType) $ valBindParams vb
  e' <- withRecordReplacements (mconcat rrs) $ transformExp $ valBindBody vb
  ret <- traverse (bitraverse transformExp pure) $ valBindRetType vb
  memoClear
  pure $
    vb
      { valBindBody = e',
        valBindParams = params',
        valBindRetType = ret
      }

-- | Monomorphise a list of top-level declarations. A module-free input program
-- is expected, so only value declarations and type declaration are accepted.
transformProg :: (MonadFreshNames m) => [ValBind] -> m [ValBind]
transformProg vbs =
  modifyNameSource $ \namesrc ->
    runRecordM namesrc $ mapM onValBind vbs
