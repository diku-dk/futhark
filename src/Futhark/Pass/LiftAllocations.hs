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
  let stm' = stm {stmExp = If cond (liftAllocationsInBody then_body) (liftAllocationsInBody else_body) dec}
      pat_names = namesFromList $ patNames pat
   in if pat_names `namesIntersect` to_lift
        then liftAllocationsInStms stms (stm' :<| lifted) acc (to_lift `namesSubtract` pat_names)
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

-- reorderBody :: Names -> Body SeqMem -> Body SeqMem
-- reorderBody alreadyDefined body@Body {bodyResult, bodyStms} =
--   let m = statementMap bodyStms
--    in body
--         { bodyStms =
--             reorderStatements
--               (namesToList $ freeIn bodyResult)
--               alreadyDefined
--               m
--               mempty
--         }

-- reorderKernelBody :: Names -> KernelBody SeqMem -> KernelBody SeqMem
-- reorderKernelBody alreadyDefined kbody@KernelBody {kernelBodyResult, kernelBodyStms} =
--   let m = statementMap kernelBodyStms
--    in kbody
--         { kernelBodyStms =
--             reorderStatements
--               (namesToList $ freeIn kernelBodyResult)
--               alreadyDefined
--               m
--               mempty
--         }

-- -- | Computes a map from VName to the statement that defines it.
-- statementMap :: Stms lore -> Map VName (Stm lore)
-- statementMap stms =
--   toList stms
--     & concatMap
--       (\stm -> [(vname, stm) | vname <- namesInPat $ stmPattern stm])
--     & Map.fromList

-- -- | Attemts to reorder statements by maintaining a stack of `VName` we need to
-- -- compute.
-- --
-- -- When looking at the item at the top of the stack `x`, find the list of
-- -- `VName` used in the the computation of `x` and insert those at the front of
-- -- the stack. If no no `VName` is required, insert the statement defining `x`
-- -- and add `x` and all other `VName` defined in that statement to the set of
-- -- values in `scope`.
-- reorderStatements ::
--   [VName] ->
--   Names ->
--   Map VName (Stm SeqMem) ->
--   Stms SeqMem ->
--   Stms SeqMem
-- reorderStatements [] _ _ acc = acc
-- reorderStatements (x : xs) scope m acc =
--   if x `nameIn` scope
--     then reorderStatements xs scope m acc
--     else case Map.lookup x m of
--       Just stm -> case namesToList $ freeIn stm `namesSubtract` scope of
--         [] ->
--           let scope' =
--                 namesFromList (namesInPat (stmPattern stm)) <> scope
--            in reorderStatements
--                 xs
--                 scope'
--                 m
--                 $ acc
--                   |> stm
--                     { stmExp =
--                         mapExp (mapper scope') $
--                           stmExp stm
--                     }
--         todo -> reorderStatements (todo <> (x : xs)) scope m acc
--       Nothing ->
--         -- The variable doesn't appear in the statement-map. We therefore assume
--         -- that it comes from outside this body, and that it is already in
--         -- scope.
--         reorderStatements xs (oneName x <> scope) m acc

-- mapper :: Names -> Mapper SeqMem SeqMem Identity
-- mapper scope =
--   identityMapper
--     { mapOnBody = \_ b -> return $ reorderBody scope b,
--       mapOnOp = onOp
--     }
--   where
--     onOp (Inner (SegOp op)) =
--       Inner . SegOp <$> mapSegOpM opMapper op
--     onOp op = return op

--     opMapper =
--       identitySegOpMapper
--         { mapOnSegOpLambda = onLambda,
--           mapOnSegOpBody = onKernelBody
--         }

--     onKernelBody body = do
--       return $ reorderKernelBody scope body

--     onLambda lam = do
--       return lam {lambdaBody = reorderBody scope $ lambdaBody lam}
