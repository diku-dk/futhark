{-# LANGUAGE TypeFamilies #-}

-- | Do various kernel optimisations - mostly related to coalescing.
-- module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where
module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import Data.Foldable
import Data.IntMap.Strict qualified as S
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.IR.GPU
import Futhark.Pass
import Futhark.Tools
import Futhark.Util

printAST :: Pass GPU GPU
printAST =
  Pass
    "pretty print ast"
    "Pretty-print the ast at current stage in pipeline"
    $ pure . pTraceShowId

-- | The pass definition.
coalesceAccess :: Pass GPU GPU
coalesceAccess =
  Pass
    "coalesce access"
    "Transform kernel input arrays for better performance."
    -- return
    $ \prog ->
      let analysisRes = analyzeDimIdxPats prog
       in intraproceduralTransformation (onStms analysisRes) prog
  where
    onStms analysisRes scope stms = do
      -- let m = localScope scope $ transformStms mempty analysisRes stms
      -- modifyNameSource $ runState (runReaderT m M.empty)
      -- fmap fst $ modifyNameSource $ runState (runReaderT m M.empty)
      -- onStms scope stms = do
      let m = localScope scope $ transformStms analysisRes mempty stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m M.empty)

type CoalesceM = Builder GPU

type Ctx = IndexTable

transformStms :: Ctx -> ExpMap -> Stms GPU -> CoalesceM (Stms GPU)
transformStms ctx expmap stms = collectStms_ $ foldM_ transformStm (ctx, expmap) stms

-- | Map from variable names to defining expression.  We use this to
-- hackily determine whether something is transposed or otherwise
-- funky in memory (and we'd prefer it not to be).  If we cannot find
-- it in the map, we just assume it's all good.  HACK and FIXME, I
-- suppose.  We really should do this at the memory level.
type ExpMap = M.Map VName (Stm GPU)

transformStm :: (Ctx, ExpMap) -> Stm GPU -> CoalesceM (Ctx, ExpMap)
transformStm (ctx, expmap) stm@(Let pat aux (Op (SegOp op)))
  -- FIXME: We only make coalescing optimisations for SegThread
  -- SegOps, because that's what the analysis assumes.  For SegGroup
  -- we should probably look at the component SegThreads, but it
  -- apparently hasn't come up in practice yet.
  | SegThread {} <- segLevel op = do
      let mapper =
            identitySegOpMapper
              { mapOnSegOpBody =
                  transformKernelBody ctx expmap (segSpace op)
              }
      op' <- mapSegOpM mapper op
      let stm' = Let pat aux $ Op $ SegOp op'
      addStm stm'
      pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
-- \| SegGroup {} <- segLevel op = pure (ctx, mempty) -- TODO: Handle this
-- \| SegThreadInGroup {} <- segLevel op = pure (ctx, mempty) -- TODO: Handle this
transformStm (ctx, expmap) (Let pat aux e) = do
  e' <- mapExpM (transform ctx expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

transform :: Ctx -> ExpMap -> Mapper GPU GPU CoalesceM
transform ctx expmap =
  identityMapper {mapOnBody = \scope -> localScope scope . transformBody ctx expmap}

transformBody :: Ctx -> ExpMap -> Body GPU -> CoalesceM (Body GPU)
transformBody ctx expmap (Body () stms res) = do
  stms' <- transformStms ctx expmap stms
  pure $ Body () stms' res

transformKernelBody ::
  Ctx ->
  ExpMap ->
  SegSpace ->
  KernelBody GPU ->
  CoalesceM (KernelBody GPU)
transformKernelBody ctx expmap space kbody = do
  -- Go spelunking for accesses to arrays that are defined outside the
  -- kernel body and where the indices are kernel thread indices.
  scope <- askScope
  let thread_gids = map fst $ unSegSpace space
      thread_local = namesFromList $ segFlat space : thread_gids
      free_ker_vars = freeIn kbody `namesSubtract` getKerVariantIds space
  evalStateT
    ( traverseKernelBodyArrayIndexes
        free_ker_vars
        thread_local
        (scope <> scopeOfSegSpace space)
        (ensureCoalescedAccess ctx expmap (unSegSpace space))
        kbody
    )
    mempty
  where
    getKerVariantIds = namesFromList . M.keys . scopeOfSegSpace

traverseKernelBodyArrayIndexes ::
  forall f.
  (Monad f) =>
  Names ->
  Names ->
  Scope GPU ->
  ArrayIndexTransform f ->
  KernelBody GPU ->
  f (KernelBody GPU)
traverseKernelBodyArrayIndexes free_ker_vars thread_variant outer_scope coalesce (KernelBody () kstms kres) =
  KernelBody () . stmsFromList
    <$> mapM onStm (stmsToList kstms)
    <*> pure kres
  where
    onLambda :: Lambda GPU -> f (Lambda GPU)
    onLambda lam =
      (\body' -> lam {lambdaBody = body'})
        <$> onBody (lambdaBody lam)

    onBody (Body bdec stms bres) = do
      stms' <- stmsFromList <$> mapM onStm (stmsToList stms)
      pure $ Body bdec stms' bres

    onStm (Let pat dec (BasicOp (Index arr is))) =
      Let pat dec . oldOrNew <$> coalesce free_ker_vars outer_scope arr is
      where
        oldOrNew Nothing =
          BasicOp $ Index arr is
        oldOrNew (Just (arr', is')) =
          BasicOp $ Index arr' is'
    onStm (Let pat dec e) =
      Let pat dec <$> mapExpM mapper e

    onOp :: Op GPU -> f (Op GPU)
    onOp (OtherOp soac) =
      OtherOp <$> mapSOACM soacMapper soac
    onOp op = pure op

    soacMapper =
      (identitySOACMapper @GPU)
        { mapOnSOACLambda = onLambda
        }

    mapper =
      (identityMapper @GPU)
        { mapOnBody = const onBody,
          mapOnOp = onOp
        }

type Replacements = M.Map (VName, Slice SubExp) VName

type ArrayIndexTransform m =
  Names -> -- free_ker_vars
  Scope GPU -> -- outer_scope (type environment)
  VName -> -- arr
  Slice SubExp -> -- slice
  m (Maybe (VName, Slice SubExp))

ensureCoalescedAccess ::
  (MonadBuilder m) =>
  Ctx ->
  ExpMap ->
  [(VName, SubExp)] -> -- thread_space
  ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess
  ctx
  expmap
  thread_space
  free_ker_vars
  outer_scope
  arr
  slice = do
    seen <- gets $ M.lookup (arr, slice)
    case seen of
      -- Already took care of this case elsewhere.
      Just arr' -> pure $ Just (arr', slice)
      -- We have not seen this array before.
      Nothing -> do
        -- For each Index expression on the array, check if the array has the optimal layout in memory
        foldl1 (>>) (map f (optimalPermutation arr ctx))
        where
          f (is_optimal_perm, perm) = do
            -- If arr does not have the optimal layout, replace it with a manifest
            -- with the optimal permutation
            if is_optimal_perm
              then pure $ Just (arr, slice)
              else replace =<< lift (manifest perm arr)

          manifest perm arr =
            letExp (baseString arr ++ "_coalesced") $
              BasicOp $
                Manifest perm arr
    where
      (thread_gids, _thread_gdims) = unzip thread_space

      replace arr' = do
        modify $ M.insert (arr, slice) arr'
        pure $ Just (arr', slice)

optimalPermutation :: VName -> Ctx -> [(Bool, [Int])]
optimalPermutation arr ctx = do
  -- Example:
  --   Initial ordering:
  --     0: [par 1]
  --     1: [par 2]
  --     2: []
  --     3: [par 0 | seq 1]
  --   Optimal ordering:
  --     0: []
  --     1: [par 0 | seq 1]
  --     2: [par 1]
  --     3: [par 2]
  --   perm' = [2,3,0,1]

  -- If a dimension is dependent on two par with different levels, we take the highest level

  -- We want two dims with deps of same type and same level to keep their original ordering
  -- w.r.t to each other

  -- For GPU, we want threads to be in the innermost dimension (i.e., highest number)
  --    Given a dim with multiple deps of par and seq, we want the pars to take precedence
  -- FOr CPU, we want loop counters to be in the innermost dimension (i.e. highest number)
  --    Given a dim with multiple deps of par and seq, we want the seqs to take precedence

  -- We want consts (empty list) to be outermost
  -- =========================================================================================
  -- For each SegMap in ctx, look up the array.
  let idx_e = mapMaybe (\(_, m) -> M.lookup arr m) (M.toList ctx)
  map f idx_e
  where
    f idxs = do
      -- case idx_e of
      -- [] -> (True, [])
      -- _ -> do
      -- let idxs = last idx_e -- FIXME: Just take last for now
      -- FIXME: For now, just take the first index.
      let (_, mem_entry) = head (M.toList idxs)

      let dims = dimensions mem_entry
      let sorted = L.sort dims
      let perm = map snd $ L.sortOn fst (zip dims ([0 ..] :: [Int]))
      -- let perm' = map snd $ L.sortOn fst (zip perm ([0 ..] :: [Int]))

      -- Check if the existing ordering is already optimal
      let is_optimal = perm == [0 ..]
      (is_optimal, perm)
