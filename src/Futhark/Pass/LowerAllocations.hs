{-# LANGUAGE TypeFamilies #-}

-- | This pass attempts to lower allocations as far towards the bottom of their
-- body as possible.
module Futhark.Pass.LowerAllocations
  ( lowerAllocationsSeqMem,
    lowerAllocationsGPUMem,
    lowerAllocationsMCMem,
  )
where

import Control.Monad.Reader
import Data.Function ((&))
import Data.Map qualified as M
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Futhark.IR.GPUMem
import Futhark.IR.MCMem
import Futhark.IR.SeqMem
import Futhark.Pass (Pass (..))

lowerInProg ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  (inner rep -> LowerM (inner rep) (inner rep)) ->
  Prog rep ->
  Prog rep
lowerInProg onOp prog =
  prog {progFuns = fmap onFun (progFuns prog)}
  where
    onFun f = f {funDefBody = onBody (funDefBody f)}
    onBody body = runReader (lowerAllocationsInBody body) (Env onOp)

lowerAllocationsSeqMem :: Pass SeqMem SeqMem
lowerAllocationsSeqMem =
  Pass "lower allocations" "lower allocations" $
    pure . lowerInProg pure

lowerAllocationsGPUMem :: Pass GPUMem GPUMem
lowerAllocationsGPUMem =
  Pass "lower allocations gpu" "lower allocations gpu" $
    pure . lowerInProg lowerAllocationsInHostOp

lowerAllocationsMCMem :: Pass MCMem MCMem
lowerAllocationsMCMem =
  Pass "lower allocations mc" "lower allocations mc" $
    pure . lowerInProg lowerAllocationsInMCOp

newtype Env inner = Env
  {onInner :: inner -> LowerM inner inner}

type LowerM inner a = Reader (Env inner) a

lowerAllocationsInBody ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  Body rep ->
  LowerM (inner rep) (Body rep)
lowerAllocationsInBody body = do
  stms <- lowerAllocationsInStms (bodyStms body) mempty mempty
  pure $ body {bodyStms = stms}

lowerAllocationsInStms ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  -- | The input stms
  Stms rep ->
  -- | The allocations currently being lowered
  M.Map VName (Stm rep) ->
  -- | The other statements processed so far
  Stms rep ->
  LowerM (inner rep) (Stms rep)
lowerAllocationsInStms Empty allocs acc = pure $ acc <> Seq.fromList (M.elems allocs)
lowerAllocationsInStms (stm@(Let (Pat [PatElem vname _]) _ (Op (Alloc _ _))) :<| stms) allocs acc =
  lowerAllocationsInStms stms (M.insert vname stm allocs) acc
lowerAllocationsInStms (stm0@(Let _ _ (Op (Inner inner))) :<| stms) alloc0 acc0 = do
  on_inner <- asks onInner
  inner' <- on_inner inner
  let stm = stm0 {stmExp = Op $ Inner inner'}
      (alloc, acc) = insertLoweredAllocs (freeIn stm0) alloc0 acc0
  lowerAllocationsInStms stms alloc (acc :|> stm)
lowerAllocationsInStms (stm@(Let _ _ (Match cond_ses cases body dec)) :<| stms) alloc acc = do
  cases' <- mapM (\(Case pat b) -> Case pat <$> lowerAllocationsInBody b) cases
  body' <- lowerAllocationsInBody body
  let stm' = stm {stmExp = Match cond_ses cases' body' dec}
      (alloc', acc') = insertLoweredAllocs (freeIn stm) alloc acc
  lowerAllocationsInStms stms alloc' (acc' :|> stm')
lowerAllocationsInStms (stm@(Let _ _ (Loop params form body)) :<| stms) alloc acc = do
  body' <- lowerAllocationsInBody body
  let stm' = stm {stmExp = Loop params form body'}
      (alloc', acc') = insertLoweredAllocs (freeIn stm) alloc acc
  lowerAllocationsInStms stms alloc' (acc' :|> stm')
lowerAllocationsInStms (stm :<| stms) alloc acc = do
  let (alloc', acc') = insertLoweredAllocs (freeIn stm) alloc acc
  lowerAllocationsInStms stms alloc' (acc' :|> stm)

insertLoweredAllocs :: Names -> M.Map VName (Stm rep) -> Stms rep -> (M.Map VName (Stm rep), Stms rep)
insertLoweredAllocs frees alloc acc =
  frees
    `namesIntersection` namesFromList (M.keys alloc)
    & namesToList
    & foldl
      ( \(alloc', acc') name ->
          ( M.delete name alloc',
            acc' :|> alloc' M.! name
          )
      )
      (alloc, acc)

lowerAllocationsInSegOp ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  SegOp lvl rep ->
  LowerM (inner rep) (SegOp lvl rep)
lowerAllocationsInSegOp (SegMap lvl sp tps body) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  pure $ SegMap lvl sp tps $ body {kernelBodyStms = stms}
lowerAllocationsInSegOp (SegRed lvl sp binops tps body) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  pure $ SegRed lvl sp binops tps $ body {kernelBodyStms = stms}
lowerAllocationsInSegOp (SegScan lvl sp binops tps body) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  pure $ SegScan lvl sp binops tps $ body {kernelBodyStms = stms}
lowerAllocationsInSegOp (SegHist lvl sp histops tps body) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  pure $ SegHist lvl sp histops tps $ body {kernelBodyStms = stms}

lowerAllocationsInHostOp :: HostOp NoOp GPUMem -> LowerM (HostOp NoOp GPUMem) (HostOp NoOp GPUMem)
lowerAllocationsInHostOp (SegOp op) = SegOp <$> lowerAllocationsInSegOp op
lowerAllocationsInHostOp op = pure op

lowerAllocationsInMCOp :: MCOp NoOp MCMem -> LowerM (MCOp NoOp MCMem) (MCOp NoOp MCMem)
lowerAllocationsInMCOp (ParOp par op) =
  ParOp <$> traverse lowerAllocationsInSegOp par <*> lowerAllocationsInSegOp op
lowerAllocationsInMCOp op = pure op
