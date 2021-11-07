{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | This pass attempts to lower allocations as far towards the top in their body
-- as possible. It does not try to hoist allocations outside across body
-- boundaries.
module Futhark.Pass.LowerAllocations (lowerAllocationsSeqMem, lowerAllocationsGPUMem) where

import Control.Monad.Reader
import Data.Function ((&))
import qualified Data.Map as M
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Futhark.IR.GPUMem
import Futhark.IR.SeqMem
import Futhark.Pass (Pass (..))

lowerAllocationsSeqMem :: Pass SeqMem SeqMem
lowerAllocationsSeqMem =
  Pass "lower allocations" "lower allocations" $ \prog@Prog {progFuns} ->
    return $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody} ->
                  f {funDefBody = runReader (lowerAllocationsInBody funDefBody) (Env return)}
              )
              progFuns
        }

lowerAllocationsGPUMem :: Pass GPUMem GPUMem
lowerAllocationsGPUMem =
  Pass "lower allocations gpu" "lower allocations gpu" $ \prog@Prog {progFuns} ->
    return $
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
  return $ body {bodyStms = stms}

lowerAllocationsInStms ::
  (Mem rep inner, LetDec rep ~ LetDecMem) =>
  -- | The input stms
  Stms rep ->
  -- | The allocations currently being lowered
  M.Map VName (Stm rep) ->
  -- | The other statements processed so far
  Stms rep ->
  LowerM inner (Stms rep)
lowerAllocationsInStms Empty allocs acc = return $ acc <> Seq.fromList (M.elems allocs)
lowerAllocationsInStms (stm@(Let (Pat [PatElem vname _]) _ (Op (Alloc _ _))) :<| stms) allocs acc =
  lowerAllocationsInStms stms (M.insert vname stm allocs) acc
lowerAllocationsInStms (stm@(Let pat _ (Op (Inner inner))) :<| stms) alloc acc = do
  on_inner <- asks onInner
  inner' <- on_inner inner
  let stm' = stm {stmExp = Op $ Inner $ inner'}
      (alloc', acc') =
        freeIn stm `namesIntersection` namesFromList (M.keys alloc)
          & namesToList
          & foldl
            ( \(alloc', acc') name ->
                ( M.delete name alloc',
                  acc' :|> alloc' M.! name
                )
            )
            (alloc, acc)
  lowerAllocationsInStms stms alloc' (acc' :|> stm')
lowerAllocationsInStms (stm@(Let pat _ (If cond then_body else_body dec)) :<| stms) alloc acc = do
  then_body' <- lowerAllocationsInBody then_body
  else_body' <- lowerAllocationsInBody else_body
  let stm' = stm {stmExp = If cond then_body' else_body' dec}
      (alloc', acc') =
        freeIn stm `namesIntersection` namesFromList (M.keys alloc)
          & namesToList
          & foldl
            ( \(alloc', acc') name ->
                ( M.delete name alloc',
                  acc' :|> alloc' M.! name
                )
            )
            (alloc, acc)
  lowerAllocationsInStms stms alloc' (acc' :|> stm')
lowerAllocationsInStms (stm@(Let pat _ (DoLoop params form body)) :<| stms) alloc acc = do
  body' <- lowerAllocationsInBody body
  let stm' = stm {stmExp = DoLoop params form body'}
      (alloc', acc') =
        freeIn stm `namesIntersection` namesFromList (M.keys alloc)
          & namesToList
          & foldl
            ( \(alloc', acc') name ->
                ( M.delete name alloc',
                  acc' :|> alloc' M.! name
                )
            )
            (alloc, acc)
  lowerAllocationsInStms stms alloc' (acc' :|> stm')
lowerAllocationsInStms (stm@(Let pat _ _) :<| stms) alloc acc = do
  let (alloc', acc') =
        freeIn stm `namesIntersection` namesFromList (M.keys alloc)
          & namesToList
          & foldl
            ( \(alloc', acc') name ->
                ( M.delete name alloc',
                  acc' :|> alloc' M.! name
                )
            )
            (alloc, acc)
  lowerAllocationsInStms stms alloc' (acc' :|> stm)

lowerAllocationsInHostOp :: HostOp GPUMem () -> LowerM (HostOp GPUMem ()) (HostOp GPUMem ())
lowerAllocationsInHostOp (SegOp (SegMap lvl sp tps body)) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  return $ SegOp $ SegMap lvl sp tps $ body {kernelBodyStms = stms}
lowerAllocationsInHostOp (SegOp (SegRed lvl sp binops tps body)) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  return $ SegOp $ SegRed lvl sp binops tps $ body {kernelBodyStms = stms}
lowerAllocationsInHostOp (SegOp (SegScan lvl sp binops tps body)) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  return $ SegOp $ SegScan lvl sp binops tps $ body {kernelBodyStms = stms}
lowerAllocationsInHostOp (SegOp (SegHist lvl sp histops tps body)) = do
  stms <- lowerAllocationsInStms (kernelBodyStms body) mempty mempty
  return $ SegOp $ SegHist lvl sp histops tps $ body {kernelBodyStms = stms}
lowerAllocationsInHostOp op = return op
