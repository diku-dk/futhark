-- | This module contains an optimization that tries to reorder statements
-- within bodies such that related statements are moved as close to each other
-- as possible.
module Futhark.Optimise.ReorderStatements (optimise) where

import Data.Foldable
import Data.Function ((&))
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence ((|>))
import Futhark.Analysis.Alias (aliasAnalysis)
import Futhark.IR.Aliases (Aliases, removeProgAliases)
import Futhark.IR.KernelsMem
import Futhark.Pass (Pass (..))

reorderBody :: Names -> Body (Aliases KernelsMem) -> Body (Aliases KernelsMem)
reorderBody alreadyDefined body@Body {bodyResult = res, bodyStms = stms} =
  let m = statementMap stms
   in body
        { bodyStms =
            reorderStatements
              (namesToList $ freeIn res)
              alreadyDefined
              m
              mempty
        }

reorderKernelBody :: Names -> KernelBody (Aliases KernelsMem) -> KernelBody (Aliases KernelsMem)
reorderKernelBody alreadyDefined kbody =
  let m = statementMap $ kernelBodyStms kbody
   in kbody
        { kernelBodyStms =
            reorderStatements
              (namesToList $ freeIn $ kernelBodyResult kbody)
              alreadyDefined
              m
              mempty
        }

-- | Computes a map from VName to the statement that defines it.
statementMap :: Stms lore -> Map VName (Stm lore)
statementMap stms =
  toList stms
    & concatMap
      (\stm -> [(vname, stm) | vname <- patternNames $ stmPattern stm])
    & Map.fromList

-- | Attemts to reorder statements by maintaining a stack of `VName` we need to
-- compute.
--
-- When looking at the item at the top of the stack `x`, find the list of
-- `VName` used in the the computation of `x` and insert those at the front of
-- the stack. If no no `VName` is required, insert the statement defining `x`
-- and add `x` and all other `VName` defined in that statement to the set of
-- values in `vtable`.
reorderStatements ::
  [VName] ->
  Names ->
  Map VName (Stm (Aliases KernelsMem)) ->
  Stms (Aliases KernelsMem) ->
  Stms (Aliases KernelsMem)
reorderStatements [] _ _ acc = acc
reorderStatements (x : xs) vtable m acc =
  if x `nameIn` vtable
    then reorderStatements xs vtable m acc
    else case Map.lookup x m of
      Just stm -> case namesToList $ freeIn stm `namesSubtract` vtable of
        [] ->
          let vtable' = namesFromList (patternNames $stmPattern stm) <> vtable
              acc' = acc |> stm {stmExp = mapExp (mapper vtable') $ stmExp stm}
           in reorderStatements xs vtable' m acc'
        todo -> reorderStatements (todo <> (x : xs)) vtable m acc
      Nothing ->
        -- The variable doesn't appear in the statement-map. We therefore assume
        -- that it comes from outside this body, and that it is already in
        -- vtable.
        reorderStatements xs (oneName x <> vtable) m acc

mapper :: Names -> Mapper (Aliases KernelsMem) (Aliases KernelsMem) Identity
mapper vtable =
  identityMapper
    { mapOnBody = \_ b -> return $ reorderBody vtable b,
      mapOnOp = onOp
    }
  where
    onOp (Inner (SegOp op)) =
      Inner . SegOp <$> mapSegOpM opMapper op
    onOp op = return op

    opMapper =
      identitySegOpMapper
        { mapOnSegOpLambda = onLambda,
          mapOnSegOpBody = onKernelBody
        }

    onKernelBody body = do
      return $ reorderKernelBody vtable body

    onLambda lam = do
      return lam {lambdaBody = reorderBody vtable $ lambdaBody lam}

optimise :: Pass KernelsMem KernelsMem
optimise =
  Pass "reorder statements" "reorder statements" $ \prog ->
    let prog' = aliasAnalysis prog
     in prog' {progFuns = fmap funHelper $ progFuns prog'}
          & removeProgAliases
          & return
  where
    funHelper f =
      f {funDefBody = reorderBody (freeIn $ funDefParams f) $ funDefBody f}
