{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

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
import Futhark.IR.KernelsMem
import Futhark.Pass (Pass (..))

-- Computes the list of `VName` defined by a given `PatternT`
namesInPat :: PatternT lore -> [VName]
namesInPat p =
  fmap patElemName (patternContextElements p)
    <> fmap patElemName (patternValueElements p)

reorderBody :: Names -> Body KernelsMem -> Body KernelsMem
reorderBody alreadyDefined body@Body {bodyResult, bodyStms} =
  let m = statementMap bodyStms
   in body
        { bodyStms =
            reorderStatements
              (namesToList $ freeIn bodyResult)
              alreadyDefined
              m
              mempty
        }

reorderKernelBody :: Names -> KernelBody KernelsMem -> KernelBody KernelsMem
reorderKernelBody alreadyDefined kbody@KernelBody {kernelBodyResult, kernelBodyStms} =
  let m = statementMap kernelBodyStms
   in kbody
        { kernelBodyStms =
            reorderStatements
              (namesToList $ freeIn kernelBodyResult)
              alreadyDefined
              m
              mempty
        }

-- | Computes a map from VName to the statement that defines it.
statementMap :: Stms lore -> Map VName (Stm lore)
statementMap stms =
  toList stms
    & concatMap
      (\stm -> [(vname, stm) | vname <- namesInPat $ stmPattern stm])
    & Map.fromList

-- | Attemts to reorder statements by maintaining a stack of `VName` we need to
-- compute.
--
-- When looking at the item at the top of the stack `x`, find the list of
-- `VName` used in the the computation of `x` and insert those at the front of
-- the stack. If no no `VName` is required, insert the statement defining `x`
-- and add `x` and all other `VName` defined in that statement to the set of
-- values in `scope`.
reorderStatements ::
  [VName] ->
  Names ->
  Map VName (Stm KernelsMem) ->
  Stms KernelsMem ->
  Stms KernelsMem
reorderStatements [] _ _ acc = acc
reorderStatements (x : xs) scope m acc =
  if x `nameIn` scope
    then reorderStatements xs scope m acc
    else case Map.lookup x m of
      Just stm -> case namesToList $ freeIn stm `namesSubtract` scope of
        [] ->
          let scope' =
                namesFromList (namesInPat (stmPattern stm)) <> scope
           in reorderStatements
                xs
                scope'
                m
                $ acc
                  |> stm
                    { stmExp =
                        mapExp (mapper scope') $
                          stmExp stm
                    }
        todo -> reorderStatements (todo <> (x : xs)) scope m acc
      Nothing ->
        -- The variable doesn't appear in the statement-map. We therefore assume
        -- that it comes from outside this body, and that it is already in
        -- scope.
        reorderStatements xs (oneName x <> scope) m acc

mapper :: Names -> Mapper KernelsMem KernelsMem Identity
mapper scope =
  identityMapper
    { mapOnBody = \_ b -> return $ reorderBody scope b,
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
      return $ reorderKernelBody scope body

    onLambda lam = do
      return lam {lambdaBody = reorderBody scope $ lambdaBody lam}

optimise :: Pass KernelsMem KernelsMem
optimise =
  Pass "reorder statements" "reorder statements" $ \prog@Prog {progFuns} ->
    return $
      prog
        { progFuns =
            fmap
              ( \f@FunDef {funDefBody, funDefParams} ->
                  f {funDefBody = reorderBody (freeIn funDefParams) funDefBody}
              )
              progFuns
        }
