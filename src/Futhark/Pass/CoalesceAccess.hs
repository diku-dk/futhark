{-# LANGUAGE TypeFamilies #-}

-- | Do various kernel optimisations - mostly related to coalescing.
-- module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where
module Futhark.Pass.CoalesceAccess (coalesceAccess, printAST) where

import Control.Monad
import Control.Monad.State.Strict
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Debug.Pretty.Simple
import Futhark.Analysis.AccessPattern
import Futhark.IR.GPU
import Futhark.Pass
import Futhark.Tools

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

type Ctx = IndexTable GPU

-- | Resulting manifest to be inserted in the AST. The first element
-- is the permutation to be applied to the array, and the second
-- element is the array name. The second element is used to indicate
-- that we have already inserted a manifest for this array.
type Manifest = [Int] (Maybe VName)

type ManifestTable rep =
  M.Map SegOpName (M.Map ArrayName (M.Map IndexExprName Manifest))

transformStms :: Ctx -> ExpMap -> Stms GPU -> CoalesceM (Stms GPU)
transformStms ctx expmap stms = collectStms_ $ foldM_ transformStm (ctx, expmap) stms

-- | Map from variable names to defining expression.  We use this to
-- hackily determine whether something is transposed or otherwise
-- funky in memory (and we'd prefer it not to be).  If we cannot find
-- it in the map, we just assume it's all good.  HACK and FIXME, I
-- suppose.  We really should do this at the memory level.
type ExpMap = M.Map VName (Stm GPU)

transformStm :: (Ctx, ExpMap) -> Stm GPU -> CoalesceM (Ctx, ExpMap)
transformStm (ctx, expmap) (Let pat aux (Op (SegOp op)))
  -- FIXME: We only make coalescing optimisations for SegThread
  -- SegOps, because that's what the analysis assumes.  For SegGroup
  -- we should probably look at the component SegThreads, but it
  -- apparently hasn't come up in practice yet.
  | SegThread {} <- segLevel op = do
      let mapper =
            identitySegOpMapper
              { mapOnSegOpBody =
                  transformSegThreadKernelBody ctx patternName
              }
      op' <- mapSegOpM mapper op
      let stm' = Let pat aux $ Op $ SegOp op'
      addStm stm'
      pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
  | SegGroup {} <- segLevel op = do
      let mapper =
            identitySegOpMapper
              { mapOnSegOpBody =
                  transformSegGroupKernelBody ctx expmap
              }
      op' <- mapSegOpM mapper op
      let stm' = Let pat aux $ Op $ SegOp op'
      addStm stm'
      pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
  | SegThreadInGroup {} <- segLevel op = undefined -- TODO: Handle this
  where
    patternName = patElemName . head $ patElems pat
transformStm (ctx, expmap) (Let pat aux e) = do
  e' <- mapExpM (transform ctx expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure (ctx, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

transform :: Ctx -> ExpMap -> Mapper GPU GPU CoalesceM
transform ctx expmap =
  identityMapper {mapOnBody = \scope -> localScope scope . transformBody ctx expmap}

-- | Recursively transform the statements in a body.
transformBody :: Ctx -> ExpMap -> Body GPU -> CoalesceM (Body GPU)
transformBody ctx expmap (Body () stms res) = do
  stms' <- transformStms ctx expmap stms
  pure $ Body () stms' res

-- | Recursively transform the statements in the body of a SegGroup kernel.
transformSegGroupKernelBody :: Ctx -> ExpMap -> KernelBody GPU -> CoalesceM (KernelBody GPU)
transformSegGroupKernelBody ctx expmap (KernelBody () stms res) = do
  stms' <- transformStms ctx expmap stms
  pure $ KernelBody () stms' res

-- | Transform the statements in the body of a SegThread kernel.
transformSegThreadKernelBody :: Ctx -> VName -> KernelBody GPU -> CoalesceM (KernelBody GPU)
transformSegThreadKernelBody ctx seg_name kbody = do
  evalStateT
    ( traverseKernelBodyArrayIndexes
        seg_name
        (ensureCoalescedAccess ctx)
        kbody
    )
    mempty

traverseKernelBodyArrayIndexes ::
  forall f.
  (Monad f) =>
  VName -> -- seg_name
  ArrayIndexTransform f ->
  KernelBody GPU ->
  f (KernelBody GPU)
traverseKernelBodyArrayIndexes seg_name coalesce (KernelBody () kstms kres) =
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
      Let pat dec . oldOrNew <$> coalesce seg_name patternName arr is
      where
        oldOrNew Nothing =
          BasicOp $ Index arr is
        oldOrNew (Just (arr', is')) =
          BasicOp $ Index arr' is'
        patternName = patElemName . head $ patElems pat
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
  VName -> -- seg_name (name of the SegThread expression's pattern)
  VName -> -- idx_name (name of the Index expression's pattern)
  VName -> -- arr (name of the array)
  Slice SubExp -> -- slice
  m (Maybe (VName, Slice SubExp))

ensureCoalescedAccess ::
  (MonadBuilder m) =>
  Ctx ->
  ArrayIndexTransform (StateT Replacements m)
ensureCoalescedAccess
  ctx
  seg_name
  idx_name
  arr
  slice = do
    seen <- gets $ M.lookup (arr, slice)
    case seen of
      -- Already took care of this case elsewhere.
      Just arr' -> pure $ Just (arr', slice)
      -- We have not seen this array before.
      Nothing -> do
        -- Check if the array has the optimal layout in memory.
        -- If it does not, replace it with a manifest to allocate
        -- it with the optimal layout
        let (optimal, perm) = optimalPermutation arr idx_name seg_name ctx
        if optimal
          then pure $ Just (arr, slice)
          else replace =<< lift (manifest perm arr)
    where
      replace arr' = do
        modify $ M.insert (arr, slice) arr'
        pure $ Just (arr', slice)

      manifest perm array =
        letExp (baseString array ++ "_coalesced") $
          BasicOp $
            Manifest perm array

optimalPermutation :: VName -> VName -> VName -> Ctx -> (Bool, [Int])
optimalPermutation arr_name idx_name seg_name ctx = do
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
  let default_res = (True, [0 ..])

  -- The result of the analysis a map in the following simplified form:
  -- SegMap |-> Array |-> Index

  -- Look for the current SegMap in the result of the analysis
  case M.lookup seg_name (M.mapKeys vnameFromSegOp ctx) of
    Nothing -> default_res
    Just segmap ->
      -- Look for the current array
      case M.lookup arr_name segmap of
        Nothing -> default_res
        Just idxs ->
          -- Look for the current Index expression
          case M.lookup idx_name idxs of
            Nothing -> default_res
            Just mem_entry ->
              do
                -- Compute optimal permutation of the array w.r.t. the Index expression
                let dims = dimensions mem_entry
                let perm = map snd $ L.sortOn fst (zip dims ([0 ..] :: [Int]))

                -- Check if the existing ordering is already optimal
                let is_optimal = perm `L.isPrefixOf` [0 ..]
                (is_optimal, perm)
