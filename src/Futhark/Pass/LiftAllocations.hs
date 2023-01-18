{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | This pass attempts to lift allocations as far towards the top in their body
-- as possible. It does not try to hoist allocations outside across body
-- boundaries.
module Futhark.Pass.LiftAllocations
  ( liftAllocationsSeqMem,
    liftAllocationsGPUMem,
    liftAllocationsMCMem,
  )
where

import Control.Monad.Reader
import Data.Sequence (Seq (..))
import Futhark.IR.GPUMem
import Futhark.IR.MCMem
import Futhark.IR.SeqMem
import Futhark.Pass (Pass (..))

liftAllocationsSeqMem :: Pass SeqMem SeqMem
liftAllocationsSeqMem =
  Pass "lift allocations" "lift allocations" $ \prog@Prog {progFuns} ->
    pure $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody} ->
                  f {funDefBody = runReader (liftAllocationsInBody funDefBody) (Env pure)}
              )
              progFuns
        }

liftAllocationsGPUMem :: Pass GPUMem GPUMem
liftAllocationsGPUMem =
  Pass "lift allocations gpu" "lift allocations gpu" $ \prog@Prog {progFuns} ->
    pure $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody} ->
                  f {funDefBody = runReader (liftAllocationsInBody funDefBody) (Env liftAllocationsInHostOp)}
              )
              progFuns
        }

liftAllocationsMCMem :: Pass MCMem MCMem
liftAllocationsMCMem =
  Pass "lift allocations mc" "lift allocations mc" $ \prog@Prog {progFuns} ->
    pure $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody} ->
                  f {funDefBody = runReader (liftAllocationsInBody funDefBody) (Env liftAllocationsInMCOp)}
              )
              progFuns
        }

newtype Env inner = Env
  {onInner :: inner -> LiftM inner inner}

type LiftM inner a = Reader (Env inner) a

liftAllocationsInBody ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  Body rep ->
  LiftM (inner rep) (Body rep)
liftAllocationsInBody body = do
  stms <- liftAllocationsInStms (bodyStms body) mempty mempty mempty
  pure $ body {bodyStms = stms}

liftAllocationsInStms ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  -- | The input stms
  Stms rep ->
  -- | The lifted allocations and associated statements
  Stms rep ->
  -- | The other statements processed so far
  Stms rep ->
  -- | Names we need to lift
  Names ->
  LiftM (inner rep) (Stms rep)
liftAllocationsInStms Empty lifted acc _ = pure $ lifted <> acc
liftAllocationsInStms (stms :|> stm@(Let (Pat [PatElem vname _]) _ (Op (Alloc _ _)))) lifted acc to_lift =
  liftAllocationsInStms stms (stm :<| lifted) acc ((freeIn stm <> to_lift) `namesSubtract` oneName vname)
liftAllocationsInStms (stms :|> stm@(Let pat _ (Op (Inner inner)))) lifted acc to_lift = do
  on_inner <- asks onInner
  inner' <- on_inner inner
  let stm' = stm {stmExp = Op $ Inner inner'}
      pat_names = namesFromList $ patNames pat
  if pat_names `namesIntersect` to_lift
    then liftAllocationsInStms stms (stm' :<| lifted) acc ((to_lift `namesSubtract` pat_names) <> freeIn stm)
    else liftAllocationsInStms stms lifted (stm' :<| acc) to_lift
liftAllocationsInStms (stms :|> stm@(Let pat aux (Match cond_ses cases body dec))) lifted acc to_lift = do
  cases' <- mapM (\(Case p b) -> Case p <$> liftAllocationsInBody b) cases
  body' <- liftAllocationsInBody body
  let stm' = stm {stmExp = Match cond_ses cases' body' dec}
      pat_names = namesFromList $ patNames pat
  if pat_names `namesIntersect` to_lift
    then
      liftAllocationsInStms
        stms
        (stm' :<| lifted)
        acc
        ( (to_lift `namesSubtract` pat_names)
            <> freeIn cond_ses
            <> freeIn cases
            <> freeIn body
            <> freeIn dec
            <> freeIn aux
        )
    else liftAllocationsInStms stms lifted (stm' :<| acc) to_lift
liftAllocationsInStms (stms :|> stm@(Let pat _ (DoLoop params form body))) lifted acc to_lift = do
  body' <- liftAllocationsInBody body
  let stm' = stm {stmExp = DoLoop params form body'}
      pat_names = namesFromList $ patNames pat
  if pat_names `namesIntersect` to_lift
    then liftAllocationsInStms stms (stm' :<| lifted) acc ((to_lift `namesSubtract` pat_names) <> freeIn stm)
    else liftAllocationsInStms stms lifted (stm' :<| acc) to_lift
liftAllocationsInStms (stms :|> stm@(Let pat _ _)) lifted acc to_lift = do
  let pat_names = namesFromList (patNames pat)
  if pat_names `namesIntersect` to_lift
    then liftAllocationsInStms stms (stm :<| lifted) acc ((to_lift `namesSubtract` pat_names) <> freeIn stm)
    else liftAllocationsInStms stms lifted (stm :<| acc) to_lift

liftAllocationsInSegOp ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  SegOp lvl rep ->
  LiftM (inner rep) (SegOp lvl rep)
liftAllocationsInSegOp (SegMap lvl sp tps body) = do
  stms <- liftAllocationsInStms (kernelBodyStms body) mempty mempty mempty
  pure $ SegMap lvl sp tps $ body {kernelBodyStms = stms}
liftAllocationsInSegOp (SegRed lvl sp binops tps body) = do
  stms <- liftAllocationsInStms (kernelBodyStms body) mempty mempty mempty
  pure $ SegRed lvl sp binops tps $ body {kernelBodyStms = stms}
liftAllocationsInSegOp (SegScan lvl sp binops tps body) = do
  stms <- liftAllocationsInStms (kernelBodyStms body) mempty mempty mempty
  pure $ SegScan lvl sp binops tps $ body {kernelBodyStms = stms}
liftAllocationsInSegOp (SegHist lvl sp histops tps body) = do
  stms <- liftAllocationsInStms (kernelBodyStms body) mempty mempty mempty
  pure $ SegHist lvl sp histops tps $ body {kernelBodyStms = stms}

liftAllocationsInHostOp :: HostOp NoOp GPUMem -> LiftM (HostOp NoOp GPUMem) (HostOp NoOp GPUMem)
liftAllocationsInHostOp (SegOp op) = SegOp <$> liftAllocationsInSegOp op
liftAllocationsInHostOp op = pure op

liftAllocationsInMCOp :: MCOp NoOp MCMem -> LiftM (MCOp NoOp MCMem) (MCOp NoOp MCMem)
liftAllocationsInMCOp (ParOp par op) =
  ParOp <$> traverse liftAllocationsInSegOp par <*> liftAllocationsInSegOp op
liftAllocationsInMCOp op = pure op
