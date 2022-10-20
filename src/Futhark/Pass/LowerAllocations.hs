{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | This pass attempts to lower allocations as far towards the bottom of their
-- body as possible.
module Futhark.Pass.LowerAllocations (lowerAllocationsSeqMem, lowerAllocationsGPUMem) where

import Control.Monad.Reader
import Data.Function ((&))
import Data.Map qualified as M
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Futhark.IR.GPUMem
import Futhark.IR.SeqMem
import Futhark.Pass (Pass (..))

lowerAllocationsSeqMem :: Pass SeqMem SeqMem
lowerAllocationsSeqMem =
  Pass "lower allocations" "lower allocations" $ \prog@Prog {progFuns} ->
    pure $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody} ->
                  f {funDefBody = runReader (lowerAllocationsInBody funDefBody) (Env pure)}
              )
              progFuns
        }

lowerAllocationsGPUMem :: Pass GPUMem GPUMem
lowerAllocationsGPUMem =
  Pass "lower allocations gpu" "lower allocations gpu" $ \prog@Prog {progFuns} ->
    pure $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody} ->
                  f {funDefBody = runReader (lowerAllocationsInBody funDefBody) (Env lowerAllocationsInHostOp)}
              )
              progFuns
        }

newtype Env inner = Env
  {onInner :: inner -> LowerM inner inner}

type LowerM inner a = Reader (Env inner) a

lowerAllocationsInBody :: (Mem rep inner, LetDec rep ~ LetDecMem) => Body rep -> LowerM inner (Body rep)
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
  LowerM inner (Stms rep)
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
lowerAllocationsInStms (stm@(Let _ _ (DoLoop params form body)) :<| stms) alloc acc = do
  body' <- lowerAllocationsInBody body
  let stm' = stm {stmExp = DoLoop params form body'}
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

lowerAllocationsInHostOp :: HostOp GPUMem () -> LowerM (HostOp GPUMem ()) (HostOp GPUMem ())
lowerAllocationsInHostOp (SegOp (SegMap lvl sp tps body)) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  pure $ SegOp $ SegMap lvl sp tps $ body {kernelBodyStms = stms}
lowerAllocationsInHostOp (SegOp (SegRed lvl sp binops tps body)) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  pure $ SegOp $ SegRed lvl sp binops tps $ body {kernelBodyStms = stms}
lowerAllocationsInHostOp (SegOp (SegScan lvl sp binops tps body)) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  pure $ SegOp $ SegScan lvl sp binops tps $ body {kernelBodyStms = stms}
lowerAllocationsInHostOp (SegOp (SegHist lvl sp histops tps body)) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  pure $ SegOp $ SegHist lvl sp histops tps $ body {kernelBodyStms = stms}
lowerAllocationsInHostOp op = pure op
