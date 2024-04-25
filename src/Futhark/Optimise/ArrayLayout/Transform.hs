-- | Do various kernel optimisations - mostly related to coalescing.
module Futhark.Optimise.ArrayLayout.Transform
  ( Transform,
    transformStms,
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Map.Strict qualified as M
import Futhark.Analysis.AccessPattern (IndexExprName, SegOpName (..))
import Futhark.Analysis.PrimExp.Table (PrimExpAnalysis)
import Futhark.Builder
import Futhark.Construct
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.MC
import Futhark.Optimise.ArrayLayout.Layout (Layout, LayoutTable, Permutation)

class (Layout rep, PrimExpAnalysis rep) => Transform rep where
  onOp ::
    (Monad m) =>
    SOACMapper rep rep m ->
    Op rep ->
    m (Op rep)
  transformOp ::
    LayoutTable ->
    ExpMap rep ->
    Stm rep ->
    Op rep ->
    TransformM rep (LayoutTable, ExpMap rep)

type TransformM rep = Builder rep

-- | A map from the name of an expression to the expression that defines it.
type ExpMap rep = M.Map VName (Stm rep)

instance Transform GPU where
  onOp soac_mapper (Futhark.IR.GPU.OtherOp soac) =
    Futhark.IR.GPU.OtherOp <$> mapSOACM soac_mapper soac
  onOp _ op = pure op
  transformOp perm_table expmap stm gpuOp
    | SegOp op <- gpuOp,
      -- TODO: handle non-segthread cases. This requires some care to
      -- avoid doing huge manifests at the block level.
      SegThread {} <- segLevel op =
        transformSegOpGPU perm_table expmap stm op
    | _ <- gpuOp = transformRestOp perm_table expmap stm

instance Transform MC where
  onOp soac_mapper (Futhark.IR.MC.OtherOp soac) =
    Futhark.IR.MC.OtherOp <$> mapSOACM soac_mapper soac
  onOp _ op = pure op
  transformOp perm_table expmap stm mcOp
    | ParOp maybe_par_segop seqSegOp <- mcOp =
        transformSegOpMC perm_table expmap stm maybe_par_segop seqSegOp
    | _ <- mcOp = transformRestOp perm_table expmap stm

transformSegOpGPU :: LayoutTable -> ExpMap GPU -> Stm GPU -> SegOp SegLevel GPU -> TransformM GPU (LayoutTable, ExpMap GPU)
transformSegOpGPU perm_table expmap stm@(Let pat aux _) op =
  -- Optimization: Only traverse the body of the SegOp if it is
  -- represented in the layout table
  case M.lookup patternName (M.mapKeys vnameFromSegOp perm_table) of
    Nothing -> do
      addStm stm
      pure (perm_table, M.fromList [(name, stm) | name <- patNames pat] <> expmap)
    Just _ -> do
      let mapper =
            identitySegOpMapper
              { mapOnSegOpBody = case segLevel op of
                  SegBlock {} -> transformSegGroupKernelBody perm_table expmap
                  _ -> transformSegThreadKernelBody perm_table patternName
              }
      op' <- mapSegOpM mapper op
      let stm' = Let pat aux $ Op $ SegOp op'
      addStm stm'
      pure (perm_table, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
  where
    patternName = patElemName . head $ patElems pat

transformSegOpMC :: LayoutTable -> ExpMap MC -> Stm MC -> Maybe (SegOp () MC) -> SegOp () MC -> TransformM MC (LayoutTable, ExpMap MC)
transformSegOpMC perm_table expmap (Let pat aux _) maybe_par_segop seqSegOp
  | Nothing <- maybe_par_segop = add Nothing
  | Just par_segop <- maybe_par_segop =
      -- Optimization: Only traverse the body of the SegOp if it is
      -- represented in the layout table
      case M.lookup patternName (M.mapKeys vnameFromSegOp perm_table) of
        Nothing -> add $ Just par_segop
        Just _ -> add . Just =<< mapSegOpM mapper par_segop
  where
    add maybe_par_segop' = do
      -- Map the sequential part of the ParOp
      seqSegOp' <- mapSegOpM mapper seqSegOp
      let stm' = Let pat aux $ Op $ ParOp maybe_par_segop' seqSegOp'
      addStm stm'
      pure (perm_table, M.fromList [(name, stm') | name <- patNames pat] <> expmap)
    mapper = identitySegOpMapper {mapOnSegOpBody = transformKernelBody perm_table expmap patternName}
    patternName = patElemName . head $ patElems pat

transformRestOp :: (Transform rep, BuilderOps rep) => LayoutTable -> ExpMap rep -> Stm rep -> TransformM rep (LayoutTable, ExpMap rep)
transformRestOp perm_table expmap (Let pat aux e) = do
  e' <- mapExpM (transform perm_table expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure (perm_table, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

transform :: (Transform rep, BuilderOps rep) => LayoutTable -> ExpMap rep -> Mapper rep rep (TransformM rep)
transform perm_table expmap =
  identityMapper {mapOnBody = \scope -> localScope scope . transformBody perm_table expmap}

-- | Recursively transform the statements in a body.
transformBody :: (Transform rep, BuilderOps rep) => LayoutTable -> ExpMap rep -> Body rep -> TransformM rep (Body rep)
transformBody perm_table expmap (Body b stms res) =
  Body b <$> transformStms perm_table expmap stms <*> pure res

-- | Recursively transform the statements in the body of a SegGroup kernel.
transformSegGroupKernelBody ::
  (Transform rep, BuilderOps rep) =>
  LayoutTable ->
  ExpMap rep ->
  KernelBody rep ->
  TransformM rep (KernelBody rep)
transformSegGroupKernelBody perm_table expmap (KernelBody b stms res) =
  KernelBody b <$> transformStms perm_table expmap stms <*> pure res

-- | Transform the statements in the body of a SegThread kernel.
transformSegThreadKernelBody ::
  (Transform rep, BuilderOps rep) =>
  LayoutTable ->
  VName ->
  KernelBody rep ->
  TransformM rep (KernelBody rep)
transformSegThreadKernelBody perm_table seg_name kbody = do
  evalStateT
    ( traverseKernelBodyArrayIndexes
        seg_name
        (ensureTransformedAccess perm_table)
        kbody
    )
    mempty

transformKernelBody ::
  (Transform rep, BuilderOps rep) =>
  LayoutTable ->
  ExpMap rep ->
  VName ->
  KernelBody rep ->
  TransformM rep (KernelBody rep)
transformKernelBody perm_table expmap seg_name (KernelBody b stms res) = do
  stms' <- transformStms perm_table expmap stms
  evalStateT
    ( traverseKernelBodyArrayIndexes
        seg_name
        (ensureTransformedAccess perm_table)
        (KernelBody b stms' res)
    )
    mempty

traverseKernelBodyArrayIndexes ::
  forall m rep.
  (Monad m, Transform rep) =>
  VName -> -- seg_name
  ArrayIndexTransform m ->
  KernelBody rep ->
  m (KernelBody rep)
traverseKernelBodyArrayIndexes seg_name coalesce (KernelBody b kstms kres) =
  KernelBody b . stmsFromList
    <$> mapM onStm (stmsToList kstms)
    <*> pure kres
  where
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

    soac_mapper =
      identitySOACMapper
        { mapOnSOACLambda = onLambda
        }

    mapper =
      (identityMapper @rep)
        { mapOnBody = const onBody,
          mapOnOp = onOp soac_mapper
        }

-- | Used to keep track of which pairs of arrays and permutations we have
-- already created manifests for, in order to avoid duplicates.
type Replacements = M.Map (VName, Permutation) VName

type ArrayIndexTransform m =
  VName -> -- seg_name (name of the SegThread expression's pattern)
  VName -> -- idx_name (name of the Index expression's pattern)
  VName -> -- arr (name of the array)
  Slice SubExp -> -- slice
  m (Maybe (VName, Slice SubExp))

ensureTransformedAccess ::
  (MonadBuilder m) =>
  LayoutTable ->
  ArrayIndexTransform (StateT Replacements m)
ensureTransformedAccess perm_table seg_name idx_name arr slice = do
  -- Check if the array has the optimal layout in memory.
  -- If it does not, replace it with a manifest to allocate
  -- it with the optimal layout
  case lookupPermutation perm_table seg_name idx_name arr of
    Nothing -> pure $ Just (arr, slice)
    Just perm -> do
      seen <- gets $ M.lookup (arr, perm)
      case seen of
        -- Already created a manifest for this array + permutation.
        -- So, just replace the name and don't make a new manifest.
        Just arr' -> pure $ Just (arr', slice)
        Nothing -> replace perm =<< lift (manifest perm arr)
  where
    replace perm arr' = do
      -- Store the fact that we have seen this array + permutation
      -- so we don't make duplicate manifests
      modify $ M.insert (arr, perm) arr'
      -- Return the new manifest
      pure $ Just (arr', slice)

    manifest perm array =
      letExp (baseString array ++ "_coalesced") $
        BasicOp (Manifest perm array)

lookupPermutation :: LayoutTable -> VName -> IndexExprName -> VName -> Maybe Permutation
lookupPermutation perm_table seg_name idx_name arr_name =
  case M.lookup seg_name (M.mapKeys vnameFromSegOp perm_table) of
    Nothing -> Nothing
    Just arrayNameMap ->
      -- Look for the current array
      case M.lookup arr_name (M.mapKeys (\(n, _, _) -> n) arrayNameMap) of
        Nothing -> Nothing
        Just idxs -> M.lookup idx_name idxs

transformStm ::
  (Transform rep, BuilderOps rep) =>
  (LayoutTable, ExpMap rep) ->
  Stm rep ->
  TransformM rep (LayoutTable, ExpMap rep)
transformStm (perm_table, expmap) (Let pat aux (Op op)) = transformOp perm_table expmap (Let pat aux (Op op)) op
transformStm (perm_table, expmap) (Let pat aux e) = do
  e' <- mapExpM (transform perm_table expmap) e
  let stm' = Let pat aux e'
  addStm stm'
  pure (perm_table, M.fromList [(name, stm') | name <- patNames pat] <> expmap)

transformStms ::
  (Transform rep, BuilderOps rep) =>
  LayoutTable ->
  ExpMap rep ->
  Stms rep ->
  TransformM rep (Stms rep)
transformStms perm_table expmap stms =
  collectStms_ $ foldM_ transformStm (perm_table, expmap) stms
