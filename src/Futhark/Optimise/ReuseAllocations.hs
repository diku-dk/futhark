{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Futhark.Optimise.ReuseAllocations (optimise) where

import Control.Arrow (first)
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Function ((&))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import qualified Futhark.Analysis.Interference as Interference
import qualified Futhark.Analysis.LastUse as LastUse
import Futhark.Binder.Class
import Futhark.Construct
import Futhark.IR.KernelsMem
import qualified Futhark.Optimise.ReuseAllocations.GreedyColoring as GreedyColoring
import Futhark.Pass (Pass (..), PassM)
import qualified Futhark.Pass as Pass

type Allocs = Map VName (SubExp, Space)

type ReuseAllocsM = Binder KernelsMem

getAllocsStm :: Stm KernelsMem -> Allocs
getAllocsStm (Let (Pattern [] [PatElem name _]) _ (Op (Alloc se sp))) =
  Map.singleton name (se, sp)
getAllocsStm (Let _ _ (Op (Alloc _ _))) = error "impossible"
getAllocsStm (Let _ _ (Op (Inner (SegOp segop)))) = getAllocsSegOp segop
getAllocsStm (Let _ _ (If _ then_body else_body _)) =
  foldMap getAllocsStm (bodyStms then_body)
    <> foldMap getAllocsStm (bodyStms else_body)
getAllocsStm (Let _ _ (DoLoop _ _ _ body)) =
  foldMap getAllocsStm (bodyStms body)
getAllocsStm _ = mempty

getAllocsSegOp :: SegOp lvl KernelsMem -> Allocs
getAllocsSegOp (SegMap _ _ _ body) =
  foldMap getAllocsStm (kernelBodyStms body)
getAllocsSegOp (SegRed _ _ _ _ body) =
  foldMap getAllocsStm (kernelBodyStms body)
getAllocsSegOp (SegScan _ _ _ _ body) =
  foldMap getAllocsStm (kernelBodyStms body)
getAllocsSegOp (SegHist _ _ _ _ body) =
  foldMap getAllocsStm (kernelBodyStms body)

setAllocsStm :: Map VName SubExp -> Stm KernelsMem -> Stm KernelsMem
setAllocsStm m stm@(Let (Pattern [] [PatElem name _]) _ (Op (Alloc _ _)))
  | Just s <- Map.lookup name m =
    stm {stmExp = BasicOp $ SubExp s}
setAllocsStm _ (Let _ _ (Op (Alloc _ _))) = error "impossible"
setAllocsStm m stm@(Let _ _ (Op (Inner (SegOp segop)))) =
  stm {stmExp = Op $ Inner $ SegOp $ setAllocsSegOp m segop}
setAllocsStm m stm@(Let _ _ (If cse then_body else_body dec)) =
  stm
    { stmExp =
        If
          cse
          (then_body {bodyStms = setAllocsStm m <$> bodyStms then_body})
          (else_body {bodyStms = setAllocsStm m <$> bodyStms else_body})
          dec
    }
setAllocsStm m stm@(Let _ _ (DoLoop ctx vals form body)) =
  stm
    { stmExp =
        DoLoop
          ctx
          vals
          form
          (body {bodyStms = setAllocsStm m <$> bodyStms body})
    }
setAllocsStm _ stm = stm

setAllocsSegOp ::
  Map VName SubExp ->
  SegOp lvl KernelsMem ->
  SegOp lvl KernelsMem
setAllocsSegOp m (SegMap lvl sp tps body) =
  SegMap lvl sp tps $
    body {kernelBodyStms = setAllocsStm m <$> kernelBodyStms body}
setAllocsSegOp m (SegRed lvl sp segbinops tps body) =
  SegRed lvl sp segbinops tps $
    body {kernelBodyStms = setAllocsStm m <$> kernelBodyStms body}
setAllocsSegOp m (SegScan lvl sp segbinops tps body) =
  SegScan lvl sp segbinops tps $
    body {kernelBodyStms = setAllocsStm m <$> kernelBodyStms body}
setAllocsSegOp m (SegHist lvl sp segbinops tps body) =
  SegHist lvl sp segbinops tps $
    body {kernelBodyStms = setAllocsStm m <$> kernelBodyStms body}

invertMap :: (Ord v, Ord k) => Map k v -> Map v (Set k)
invertMap m =
  Map.toList m
    & fmap (swap . first Set.singleton)
    & foldr (uncurry $ Map.insertWith (<>)) mempty

maxSubExp :: Set SubExp -> ReuseAllocsM SubExp
maxSubExp = helper . Set.toList
  where
    helper (s1 : s2 : sexps) = do
      z <- letSubExp "maxSubHelper" $ BasicOp $ BinOp (UMax Int64) s1 s2
      helper (z : sexps)
    helper [s] =
      return s
    helper [] = error "impossible"

optimiseKernel ::
  Interference.Graph VName ->
  SegOp lvl KernelsMem ->
  ReuseAllocsM (SegOp lvl KernelsMem)
optimiseKernel graph segop = do
  let allocs = getAllocsSegOp segop
      (colorspaces, coloring) =
        GreedyColoring.colorGraph
          (fmap snd allocs)
          graph
  (maxes, maxstms) <-
    invertMap coloring
      & Map.elems
      & mapM (maxSubExp . Set.map (fst . (allocs !)))
      & collectStms
  (colors, stms) <-
    assert (length maxes == Map.size colorspaces) maxes
      & zip [0 ..]
      & mapM (\(i, x) -> letSubExp "color" $ Op $ Alloc x $ colorspaces ! i)
      & collectStms
  let segop' = setAllocsSegOp (fmap (colors !!) coloring) segop
  return $ case segop' of
    SegMap lvl sp tps body ->
      SegMap lvl sp tps $
        body {kernelBodyStms = maxstms <> stms <> kernelBodyStms body}
    SegRed lvl sp binops tps body ->
      SegRed lvl sp binops tps $
        body {kernelBodyStms = maxstms <> stms <> kernelBodyStms body}
    SegScan lvl sp binops tps body ->
      SegScan lvl sp binops tps $
        body {kernelBodyStms = maxstms <> stms <> kernelBodyStms body}
    SegHist lvl sp binops tps body ->
      SegHist lvl sp binops tps $
        body {kernelBodyStms = maxstms <> stms <> kernelBodyStms body}

onKernels ::
  (SegOp SegLevel KernelsMem -> ReuseAllocsM (SegOp SegLevel KernelsMem)) ->
  Stms KernelsMem ->
  ReuseAllocsM (Stms KernelsMem)
onKernels f =
  mapM helper
  where
    helper stm@Let {stmExp = Op (Inner (SegOp segop))} =
      inScopeOf stm $ do
        exp' <- f segop
        return $ stm {stmExp = Op $ Inner $ SegOp exp'}
    helper stm@Let {stmExp = If c then_body else_body dec} =
      inScopeOf stm $ do
        then_body_stms <- f `onKernels` bodyStms then_body
        else_body_stms <- f `onKernels` bodyStms else_body
        return $
          stm
            { stmExp =
                If
                  c
                  (then_body {bodyStms = then_body_stms})
                  (else_body {bodyStms = else_body_stms})
                  dec
            }
    helper stm@Let {stmExp = DoLoop ctx vals form body} =
      inScopeOf stm $ do
        stms <- f `onKernels` bodyStms body
        return $ stm {stmExp = DoLoop ctx vals form (body {bodyStms = stms})}
    helper stm =
      inScopeOf stm $ return stm

optimise :: Pass KernelsMem KernelsMem
optimise =
  Pass "reuse allocations" "reuse allocations" $ \prog ->
    let (lumap, _) = LastUse.analyseProg prog
        (_, _, graph) =
          foldMap
            ( \f ->
                runReader
                  ( Interference.analyseKernels lumap $
                      bodyStms $ funDefBody f
                  )
                  $ scopeOf f
            )
            $ progFuns prog
     in Pass.intraproceduralTransformation (onStms graph) prog
  where
    onStms ::
      Interference.Graph VName ->
      Scope KernelsMem ->
      Stms KernelsMem ->
      PassM (Stms KernelsMem)
    onStms graph scope stms = do
      let m = localScope scope $ optimiseKernel graph `onKernels` stms
      fmap fst $ modifyNameSource $ runState (runBinderT m mempty)
