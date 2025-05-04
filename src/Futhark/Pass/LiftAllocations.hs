{-# LANGUAGE TypeFamilies #-}

-- | This pass attempts to lift allocations and asserts as far towards
-- the top in their body as possible. This helps memory short
-- circuiting do a better job, as it is sensitive to statement
-- ordering.  It does not try to hoist allocations outside across body
-- boundaries.
module Futhark.Pass.LiftAllocations
  ( liftAllocationsSeqMem,
    liftAllocationsGPUMem,
    liftAllocationsMCMem,
  )
where

import Control.Monad.Reader
import Data.Map qualified as M
import Data.Sequence (Seq (..))
import Futhark.Analysis.Alias (aliasAnalysis)
import Futhark.IR.Aliases
import Futhark.IR.GPUMem
import Futhark.IR.MCMem
import Futhark.IR.SeqMem
import Futhark.Pass (Pass (..))

liftInProg ::
  (AliasableRep rep, Mem rep inner, ASTConstraints (inner (Aliases rep))) =>
  (inner (Aliases rep) -> LiftM (inner (Aliases rep)) (inner (Aliases rep))) ->
  Prog rep ->
  Prog rep
liftInProg onOp prog =
  prog
    { progFuns = removeFunDefAliases . onFun <$> progFuns (aliasAnalysis prog)
    }
  where
    onFun f = f {funDefBody = onBody (funDefBody f)}
    onBody body = runReader (liftAllocationsInBody body) (Env onOp mempty)

liftAllocationsSeqMem :: Pass SeqMem SeqMem
liftAllocationsSeqMem =
  Pass "lift allocations" "lift allocations" $
    pure . liftInProg pure

liftAllocationsGPUMem :: Pass GPUMem GPUMem
liftAllocationsGPUMem =
  Pass "lift allocations gpu" "lift allocations gpu" $
    pure . liftInProg liftAllocationsInHostOp

liftAllocationsMCMem :: Pass MCMem MCMem
liftAllocationsMCMem =
  Pass "lift allocations mc" "lift allocations mc" $
    pure . liftInProg liftAllocationsInMCOp

data Env inner = Env
  { onInner :: inner -> LiftM inner inner,
    envAliases :: AliasesAndConsumed
  }

type LiftM inner a = Reader (Env inner) a

liftAllocationsInBody ::
  (Mem rep inner, Aliased rep) =>
  Body rep ->
  LiftM (inner rep) (Body rep)
liftAllocationsInBody body = do
  stms <- liftAllocationsInStms (bodyStms body)
  pure $ body {bodyStms = stms}

liftInsideStm ::
  (Mem rep inner, Aliased rep) =>
  Stm rep ->
  LiftM (inner rep) (Stm rep)
liftInsideStm stm@(Let _ _ (Op (Inner inner))) = do
  on_inner <- asks onInner
  inner' <- on_inner inner
  pure $ stm {stmExp = Op $ Inner inner'}
liftInsideStm stm@(Let _ _ (Match cond_ses cases body dec)) = do
  cases' <- mapM (\(Case p b) -> Case p <$> liftAllocationsInBody b) cases
  body' <- liftAllocationsInBody body
  pure stm {stmExp = Match cond_ses cases' body' dec}
liftInsideStm stm@(Let _ _ (Loop params form body)) = do
  body' <- liftAllocationsInBody body
  pure stm {stmExp = Loop params form body'}
liftInsideStm stm = pure stm

liftAllocationsInStms ::
  forall rep inner.
  (Mem rep inner, Aliased rep) =>
  Stms rep ->
  LiftM (inner rep) (Stms rep)
liftAllocationsInStms stms_orig = do
  outer_aliases <- asks envAliases
  let aliases = foldl trackAliases outer_aliases stms_orig
      go ::
        -- The input stms
        Stms rep ->
        -- The lifted allocations and associated statements
        Stms rep ->
        -- The other statements processed so far
        Stms rep ->
        -- (Names we need to lift, consumed names)
        (Names, Names) ->
        LiftM (inner rep) (Stms rep)
      go Empty lifted acc _ = pure $ lifted <> acc
      go (stms :|> stm) lifted acc (to_lift, consumed) = do
        stm' <- liftInsideStm stm
        case stmExp stm' of
          BasicOp Assert {} -> liftStm stm'
          Op Alloc {} -> liftStm stm'
          _ -> do
            let pat_names = namesFromList $ patNames $ stmPat stm'
                free_in_stm = freeIn stm
                expand v = maybe [v] namesToList $ M.lookup v $ fst aliases
            if (pat_names `namesIntersect` to_lift)
              || any (`nameIn` free_in_stm) (foldMap expand $ namesToList consumed)
              then liftStm stm'
              else dontLiftStm stm'
        where
          liftStm stm' =
            go stms (stm' :<| lifted) acc (to_lift', consumed')
            where
              to_lift' =
                freeIn stm'
                  <> (to_lift `namesSubtract` namesFromList (patNames (stmPat stm')))
              consumed' = consumed <> consumedInStm stm'
          dontLiftStm stm' =
            go stms lifted (stm' :<| acc) (to_lift, consumed)
  local (\env -> env {envAliases = aliases}) $
    go stms_orig mempty mempty mempty

liftAllocationsInSegOp ::
  (Mem rep inner, Aliased rep) =>
  SegOp lvl rep ->
  LiftM (inner rep) (SegOp lvl rep)
liftAllocationsInSegOp (SegMap lvl sp tps body) = do
  stms <- liftAllocationsInStms (kernelBodyStms body)
  pure $ SegMap lvl sp tps $ body {kernelBodyStms = stms}
liftAllocationsInSegOp (SegRed lvl sp tps body binops) = do
  stms <- liftAllocationsInStms (kernelBodyStms body)
  pure $ SegRed lvl sp tps (body {kernelBodyStms = stms}) binops
liftAllocationsInSegOp (SegScan lvl sp tps body binops) = do
  stms <- liftAllocationsInStms (kernelBodyStms body)
  pure $ SegScan lvl sp tps (body {kernelBodyStms = stms}) binops
liftAllocationsInSegOp (SegHist lvl sp tps body histops) = do
  stms <- liftAllocationsInStms (kernelBodyStms body)
  pure $ SegHist lvl sp tps (body {kernelBodyStms = stms}) histops

liftAllocationsInHostOp ::
  HostOp NoOp (Aliases GPUMem) ->
  LiftM (HostOp NoOp (Aliases GPUMem)) (HostOp NoOp (Aliases GPUMem))
liftAllocationsInHostOp (SegOp op) = SegOp <$> liftAllocationsInSegOp op
liftAllocationsInHostOp op = pure op

liftAllocationsInMCOp ::
  MCOp NoOp (Aliases MCMem) ->
  LiftM (MCOp NoOp (Aliases MCMem)) (MCOp NoOp (Aliases MCMem))
liftAllocationsInMCOp (ParOp par op) =
  ParOp <$> traverse liftAllocationsInSegOp par <*> liftAllocationsInSegOp op
liftAllocationsInMCOp op = pure op
