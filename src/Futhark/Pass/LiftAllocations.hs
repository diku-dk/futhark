{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | This pass attempts to lift allocations as far towards the top in their body
-- as possible. It does not try to hoist allocations outside across body
-- boundaries.
module Futhark.Pass.LiftAllocations (liftAllocations) where

import Data.Sequence (Seq (..))
import Futhark.IR.SeqMem
import Futhark.Pass (Pass (..))

liftAllocations :: Pass SeqMem SeqMem
liftAllocations =
  Pass "lift allocations" "lift allocations" $ \prog@Prog {progFuns} ->
    return $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody} ->
                  f {funDefBody = liftAllocationsInBody funDefBody}
              )
              progFuns
        }

liftAllocationsInBody :: Body SeqMem -> Body SeqMem
liftAllocationsInBody body =
  body {bodyStms = liftAllocationsInStms (bodyStms body) mempty mempty mempty}

liftAllocationsInStms ::
  -- | The input stms
  Stms SeqMem ->
  -- | The lifted allocations and associated statements
  Stms SeqMem ->
  -- | The other statements processed so far
  Stms SeqMem ->
  -- | Names we need to lift
  Names ->
  Stms SeqMem
liftAllocationsInStms Empty lifted acc _ = lifted <> acc
liftAllocationsInStms (stms :|> stm@(Let (Pat [PatElem vname _]) _ (Op (Alloc _ _)))) lifted acc to_lift =
  liftAllocationsInStms stms (stm :<| lifted) acc ((freeIn stm <> to_lift) `namesSubtract` oneName vname)
liftAllocationsInStms (stms :|> stm@(Let pat _ (If cond then_body else_body dec))) lifted acc to_lift =
  let then_body' = liftAllocationsInBody then_body
      else_body' = liftAllocationsInBody else_body
      stm' = stm {stmExp = If cond then_body' else_body' dec}
      pat_names = namesFromList $ patNames pat
   in if pat_names `namesIntersect` to_lift
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
liftAllocationsInStms (stms :|> stm@(Let pat _ (DoLoop params form body))) lifted acc to_lift =
  let stm' = stm {stmExp = DoLoop params form $ liftAllocationsInBody body}
      pat_names = namesFromList $ patNames pat
   in if pat_names `namesIntersect` to_lift
        then liftAllocationsInStms stms (stm' :<| lifted) acc ((to_lift `namesSubtract` pat_names) <> freeIn stm)
        else liftAllocationsInStms stms lifted (stm' :<| acc) to_lift
liftAllocationsInStms (stms :|> stm@(Let pat _ _)) lifted acc to_lift =
  let pat_names = namesFromList (patNames pat)
   in if pat_names `namesIntersect` to_lift
        then liftAllocationsInStms stms (stm :<| lifted) acc ((to_lift `namesSubtract` pat_names) <> freeIn stm)
        else liftAllocationsInStms stms lifted (stm :<| acc) to_lift
