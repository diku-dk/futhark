-- | Converts identifiers of record type into record patterns (and
-- similarly for tuples).  This is to ensure that the closures
-- produced in lambda lifting and defunctionalisation do not carry
-- around huge records of which only a tiny part is needed.
module Futhark.Internalise.ReplaceRecords (transformProg) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
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

-- The monomorphization monad.
newtype RecordM a
  = RecordM (ReaderT Env (State VNameSource) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env
    )

instance MonadFreshNames RecordM where
  getNameSource = RecordM get
  putNameSource = RecordM . put

runRecordM :: VNameSource -> RecordM a -> (a, VNameSource)
runRecordM src (RecordM m) =
  runState (runReaderT m (Env mempty)) src

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

transformPat :: Pat (TypeBase Size u) -> RecordM (Pat (TypeBase Size u), RecordReplacements)
transformPat (Id v (Info (Scalar (Record fs))) loc) = do
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
transformPat (Id v t loc) =
  pure (Id v t loc, mempty)
transformPat (TuplePat pats loc) = do
  (pats', rrs) <- mapAndUnzipM transformPat pats
  pure (TuplePat pats' loc, mconcat rrs)
transformPat (RecordPat fields loc) = do
  let (field_names, field_pats) = unzip fields
  (field_pats', rrs) <- mapAndUnzipM transformPat field_pats
  pure (RecordPat (zip field_names field_pats') loc, mconcat rrs)
transformPat (PatParens pat loc) = do
  (pat', rr) <- transformPat pat
  pure (PatParens pat' loc, rr)
transformPat (PatAttr attr pat loc) = do
  (pat', rr) <- transformPat pat
  pure (PatAttr attr pat' loc, rr)
transformPat (Wildcard (Info t) loc) =
  pure (wildcard t loc, mempty)
transformPat (PatAscription pat _ _) =
  transformPat pat
transformPat (PatLit e t loc) = pure (PatLit e t loc, mempty)
transformPat (PatConstr name t all_ps loc) = do
  (all_ps', rrs) <- mapAndUnzipM transformPat all_ps
  pure (PatConstr name t all_ps' loc, mconcat rrs)

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
transformExp e@(Var fname _ loc) = do
  maybe_fs <- lookupRecordReplacement $ qualLeaf fname
  case maybe_fs of
    Just fs -> do
      let toField (f, (f_v, f_t)) = do
            let f_v' = Var (qualName f_v) (Info f_t) loc
            pure $ RecordFieldExplicit f f_v' loc
      RecordLit <$> mapM toField (M.toList fs) <*> pure loc
    Nothing ->
      pure e
transformExp (AppExp (LetPat sizes pat e body loc) res) = do
  e' <- transformExp e
  (pat', rr) <- transformPat pat
  body' <- withRecordReplacements rr $ transformExp body
  pure $ AppExp (LetPat sizes pat' e' body' loc) res
transformExp (AppExp (LetFun {}) _) = do
  error "transformExp: LetFun is not supposed to occur"
transformExp (Lambda {}) =
  error "transformExp: Lambda is not supposed to occur"
transformExp e = astMap m e
  where
    m = identityMapper {mapOnExp = transformExp}

onValBind :: ValBind -> RecordM ValBind
onValBind vb = do
  (params', rrs) <- mapAndUnzipM transformPat $ valBindParams vb
  e' <- withRecordReplacements (mconcat rrs) $ transformExp $ valBindBody vb
  pure $ vb {valBindBody = e', valBindParams = params'}

-- | Monomorphise a list of top-level declarations. A module-free input program
-- is expected, so only value declarations and type declaration are accepted.
transformProg :: (MonadFreshNames m) => [ValBind] -> m [ValBind]
transformProg vbs =
  modifyNameSource $ \namesrc ->
    runRecordM namesrc $ mapM onValBind vbs
