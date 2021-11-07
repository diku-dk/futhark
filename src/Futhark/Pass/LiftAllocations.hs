{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | This pass attempts to lift allocations as far towards the top in their body
-- as possible. It does not try to hoist allocations outside across body
-- boundaries.
module Futhark.Pass.LiftAllocations (liftAllocationsSeqMem, liftAllocationsGPUMem) where

import Control.Monad.Reader
import Data.Sequence (Seq (..))
import Futhark.IR.GPUMem
import Futhark.IR.SeqMem
import Futhark.Pass (Pass (..))

liftAllocationsSeqMem :: Pass SeqMem SeqMem
liftAllocationsSeqMem =
  Pass "lift allocations" "lift allocations" $ \prog@Prog {progFuns} ->
    return $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody} ->
                  f {funDefBody = runReader (liftAllocationsInBody funDefBody) (Env return)}
              )
              progFuns
        }

liftAllocationsGPUMem :: Pass GPUMem GPUMem
liftAllocationsGPUMem =
  Pass "lift allocations gpu" "lift allocations gpu" $ \prog@Prog {progFuns} ->
    return $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody} ->
                  f {funDefBody = runReader (liftAllocationsInBody funDefBody) (Env liftAllocationsInHostOp)}
              )
              progFuns
        }

newtype Env inner = Env
  {onInner :: inner -> LiftM inner inner}

type LiftM inner a = Reader (Env inner) a

liftAllocationsInBody :: (Mem rep inner, LetDec rep ~ LetDecMem) => Body rep -> LiftM inner (Body rep)
liftAllocationsInBody body = do
  stms <- liftAllocationsInStms (bodyStms body) mempty mempty mempty
  return $ body {bodyStms = stms}

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
  LiftM inner (Stms rep)
liftAllocationsInStms Empty lifted acc _ = return $ lifted <> acc
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
liftAllocationsInStms (stms :|> stm@(Let pat _ (If cond then_body else_body dec))) lifted acc to_lift = do
  then_body' <- liftAllocationsInBody then_body
  else_body' <- liftAllocationsInBody else_body
  let stm' = stm {stmExp = If cond then_body' else_body' dec}
      pat_names = namesFromList $ patNames pat
  if pat_names `namesIntersect` to_lift
    then
      liftAllocationsInStms
        stms
        (stm' :<| lifted)
        acc
        ( (to_lift `namesSubtract` pat_names)
            <> freeIn cond
            <> freeIn then_body'
            <> freeIn else_body'
            <> freeIn dec
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

liftAllocationsInHostOp :: HostOp GPUMem () -> LiftM (HostOp GPUMem ()) (HostOp GPUMem ())
liftAllocationsInHostOp (SegOp (SegMap lvl sp tps body)) = do
  stms <- liftAllocationsInStms (kernelBodyStms body) mempty mempty mempty
  return $ SegOp $ SegMap lvl sp tps $ body {kernelBodyStms = stms}
liftAllocationsInHostOp (SegOp (SegRed lvl sp binops tps body)) = do
  stms <- liftAllocationsInStms (kernelBodyStms body) mempty mempty mempty
  return $ SegOp $ SegRed lvl sp binops tps $ body {kernelBodyStms = stms}
liftAllocationsInHostOp (SegOp (SegScan lvl sp binops tps body)) = do
  stms <- liftAllocationsInStms (kernelBodyStms body) mempty mempty mempty
  return $ SegOp $ SegScan lvl sp binops tps $ body {kernelBodyStms = stms}
liftAllocationsInHostOp (SegOp (SegHist lvl sp histops tps body)) = do
  stms <- liftAllocationsInStms (kernelBodyStms body) mempty mempty mempty
  return $ SegOp $ SegHist lvl sp histops tps $ body {kernelBodyStms = stms}
liftAllocationsInHostOp op = return op
