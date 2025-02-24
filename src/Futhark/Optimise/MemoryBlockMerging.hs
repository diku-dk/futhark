{-# LANGUAGE TypeFamilies #-}

-- | This module implements an optimization that tries to statically reuse
-- kernel-level allocations. The goal is to lower the static memory usage, which
-- might allow more programs to run using intra-group parallelism.
module Futhark.Optimise.MemoryBlockMerging (optimise) where

import Control.Exception
import Control.Monad.State.Strict
import Data.Function ((&))
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.Interference qualified as Interference
import Futhark.Builder.Class
import Futhark.Construct
import Futhark.IR.GPUMem
import Futhark.Optimise.MemoryBlockMerging.GreedyColoring qualified as GreedyColoring
import Futhark.Pass (Pass (..), PassM)
import Futhark.Pass qualified as Pass
import Futhark.Util (invertMap)

-- | A mapping from allocation names to their size and space.
type Allocs = Map VName (SubExp, Space)

getAllocsStm :: Stm GPUMem -> Allocs
getAllocsStm (Let (Pat [PatElem name _]) _ (Op (Alloc se sp))) =
  M.singleton name (se, sp)
getAllocsStm (Let _ _ (Op (Alloc _ _))) = error "impossible"
getAllocsStm (Let _ _ (Match _ cases defbody _)) =
  foldMap (foldMap getAllocsStm . bodyStms) $ defbody : map caseBody cases
getAllocsStm (Let _ _ (Loop _ _ body)) =
  foldMap getAllocsStm (bodyStms body)
getAllocsStm _ = mempty

getAllocsSegOp :: SegOp lvl GPUMem -> Allocs
getAllocsSegOp (SegMap _ _ _ body) =
  foldMap getAllocsStm (kernelBodyStms body)
getAllocsSegOp (SegRed _ _ _ body _) =
  foldMap getAllocsStm (kernelBodyStms body)
getAllocsSegOp (SegScan _ _ _ body _) =
  foldMap getAllocsStm (kernelBodyStms body)
getAllocsSegOp (SegHist _ _ _ body _) =
  foldMap getAllocsStm (kernelBodyStms body)

setAllocsStm :: Map VName SubExp -> Stm GPUMem -> Stm GPUMem
setAllocsStm m stm@(Let (Pat [PatElem name _]) _ (Op (Alloc _ _)))
  | Just s <- M.lookup name m =
      stm {stmExp = BasicOp $ SubExp s}
setAllocsStm _ stm@(Let _ _ (Op (Alloc _ _))) = stm
setAllocsStm m stm@(Let _ _ (Op (Inner (SegOp segop)))) =
  stm {stmExp = Op $ Inner $ SegOp $ setAllocsSegOp m segop}
setAllocsStm m stm@(Let _ _ (Match cond cases defbody dec)) =
  stm {stmExp = Match cond (map (fmap onBody) cases) (onBody defbody) dec}
  where
    onBody (Body () stms res) = Body () (setAllocsStm m <$> stms) res
setAllocsStm m stm@(Let _ _ (Loop merge form body)) =
  stm
    { stmExp =
        Loop merge form (body {bodyStms = setAllocsStm m <$> bodyStms body})
    }
setAllocsStm _ stm = stm

setAllocsSegOp ::
  Map VName SubExp ->
  SegOp lvl GPUMem ->
  SegOp lvl GPUMem
setAllocsSegOp m (SegMap lvl sp tps body) =
  SegMap lvl sp tps $
    body {kernelBodyStms = setAllocsStm m <$> kernelBodyStms body}
setAllocsSegOp m (SegRed lvl sp tps body segbinops) =
  SegRed
    lvl
    sp
    tps
    ( body {kernelBodyStms = setAllocsStm m <$> kernelBodyStms body}
    )
    segbinops
setAllocsSegOp m (SegScan lvl sp tps body segbinops) =
  SegScan
    lvl
    sp
    tps
    ( body {kernelBodyStms = setAllocsStm m <$> kernelBodyStms body}
    )
    segbinops
setAllocsSegOp m (SegHist lvl sp tps body seghistops) =
  SegHist
    lvl
    sp
    tps
    ( body {kernelBodyStms = setAllocsStm m <$> kernelBodyStms body}
    )
    seghistops

maxSubExp :: (MonadBuilder m) => Set SubExp -> m SubExp
maxSubExp = helper . S.toList
  where
    helper (s1 : s2 : sexps) = do
      z <- letSubExp "maxSubHelper" $ BasicOp $ BinOp (UMax Int64) s1 s2
      helper (z : sexps)
    helper [s] =
      pure s
    helper [] = error "impossible"

isKernelInvariant :: Scope GPUMem -> (SubExp, space) -> Bool
isKernelInvariant scope (Var vname, _) = vname `M.member` scope
isKernelInvariant _ _ = True

isScalarSpace :: (subExp, Space) -> Bool
isScalarSpace (_, ScalarSpace _ _) = True
isScalarSpace _ = False

onKernelBodyStms ::
  (MonadBuilder m) =>
  SegOp lvl GPUMem ->
  (Stms GPUMem -> m (Stms GPUMem)) ->
  m (SegOp lvl GPUMem)
onKernelBodyStms (SegMap lvl space ts body) f = do
  stms <- f $ kernelBodyStms body
  pure $ SegMap lvl space ts $ body {kernelBodyStms = stms}
onKernelBodyStms (SegRed lvl space ts body binops) f = do
  stms <- f $ kernelBodyStms body
  pure $ SegRed lvl space ts (body {kernelBodyStms = stms}) binops
onKernelBodyStms (SegScan lvl space ts body binops) f = do
  stms <- f $ kernelBodyStms body
  pure $ SegScan lvl space ts (body {kernelBodyStms = stms}) binops
onKernelBodyStms (SegHist lvl space ts body binops) f = do
  stms <- f $ kernelBodyStms body
  pure $ SegHist lvl space ts (body {kernelBodyStms = stms}) binops

-- | This is the actual optimiser. Given an interference graph and a @SegOp@,
-- replace allocations and references to memory blocks inside with a (hopefully)
-- reduced number of allocations.
optimiseKernel ::
  (MonadBuilder m, Rep m ~ GPUMem) =>
  Interference.Graph VName ->
  SegOp lvl GPUMem ->
  m (SegOp lvl GPUMem)
optimiseKernel graph segop0 = do
  segop <- onKernelBodyStms segop0 $ onKernels $ optimiseKernel graph
  scope_here <- askScope
  let allocs =
        M.filter (\alloc -> isKernelInvariant scope_here alloc && not (isScalarSpace alloc)) $
          getAllocsSegOp segop
      (colorspaces, coloring) =
        GreedyColoring.colorGraph
          (fmap snd allocs)
          graph
  (maxes, maxstms) <-
    invertMap coloring
      & M.elems
      & mapM (maxSubExp . S.map (fst . (allocs !)))
      & collectStms
  (colors, stms) <-
    assert (length maxes == M.size colorspaces) maxes
      & zip [0 ..]
      & mapM (\(i, x) -> letSubExp "color" $ Op $ Alloc x $ colorspaces ! i)
      & collectStms
  let segop' = setAllocsSegOp (fmap (colors !!) coloring) segop
  pure $ case segop' of
    SegMap lvl sp tps body ->
      SegMap lvl sp tps $
        body {kernelBodyStms = maxstms <> stms <> kernelBodyStms body}
    SegRed lvl sp tps body ops ->
      SegRed lvl sp tps body' ops
      where
        body' = body {kernelBodyStms = maxstms <> stms <> kernelBodyStms body}
    SegScan lvl sp tps body ops ->
      SegScan lvl sp tps body' ops
      where
        body' = body {kernelBodyStms = maxstms <> stms <> kernelBodyStms body}
    SegHist lvl sp tps body ops ->
      SegHist lvl sp tps body' ops
      where
        body' = body {kernelBodyStms = maxstms <> stms <> kernelBodyStms body}

-- | Helper function that modifies kernels found inside some statements.
onKernels ::
  (LocalScope GPUMem m) =>
  (SegOp SegLevel GPUMem -> m (SegOp SegLevel GPUMem)) ->
  Stms GPUMem ->
  m (Stms GPUMem)
onKernels f orig_stms = inScopeOf orig_stms $ mapM helper orig_stms
  where
    helper stm@Let {stmExp = Op (Inner (SegOp segop))} = do
      exp' <- f segop
      pure $ stm {stmExp = Op $ Inner $ SegOp exp'}
    helper stm@Let {stmExp = Match c cases defbody dec} = do
      cases' <- mapM (traverse onBody) cases
      defbody' <- onBody defbody
      pure $ stm {stmExp = Match c cases' defbody' dec}
      where
        onBody (Body () stms res) =
          Body () <$> f `onKernels` stms <*> pure res
    helper stm@Let {stmExp = Loop merge form body} = do
      body_stms <- f `onKernels` bodyStms body
      pure $ stm {stmExp = Loop merge form (body {bodyStms = body_stms})}
    helper stm = pure stm

-- | Perform the reuse-allocations optimization.
optimise :: Pass GPUMem GPUMem
optimise =
  Pass "memory block merging" "memory block merging allocations" $ \prog ->
    let graph = Interference.analyseProgGPU prog
     in Pass.intraproceduralTransformation (onStms graph) prog
  where
    onStms ::
      Interference.Graph VName ->
      Scope GPUMem ->
      Stms GPUMem ->
      PassM (Stms GPUMem)
    onStms graph scope stms = do
      let m = localScope scope $ optimiseKernel graph `onKernels` stms
      fmap fst $ modifyNameSource $ runState (runBuilderT m mempty)
