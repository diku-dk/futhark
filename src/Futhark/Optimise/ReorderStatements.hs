{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module contains an optimization that tries to reorder statements
-- within bodies such that related statements are moved as close to each other
-- as possible.
module Futhark.Optimise.ReorderStatements (optimise) where

import Control.Monad.Reader
import Data.Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Sequence ((|>))
import Futhark.Analysis.Alias (aliasAnalysis)
import Futhark.IR.Aliases (Aliases, removeProgAliases)
import Futhark.IR.KernelsMem
import Futhark.Pass (Pass (..), PassM, intraproceduralTransformationWithConsts)

type ReorderLore = Aliases KernelsMem

reorderBody :: LocalScope ReorderLore m => Names -> Body ReorderLore -> m (Body ReorderLore)
reorderBody alreadyDefined body@Body {bodyResult = res, bodyStms = stms} = do
  stms' <-
    reorderStatements
      (namesToList $ freeIn res)
      alreadyDefined
      (statementMap stms)
      mempty
  return $
    body
      { bodyStms = stms'
      }

reorderKernelBody ::
  LocalScope ReorderLore m =>
  Names ->
  KernelBody ReorderLore ->
  m (KernelBody ReorderLore)
reorderKernelBody alreadyDefined kbody = do
  stms' <-
    reorderStatements
      (namesToList $ freeIn $ kernelBodyResult kbody)
      alreadyDefined
      (statementMap $ kernelBodyStms kbody)
      mempty
  return $ kbody {kernelBodyStms = stms'}

-- | Computes a map from VName to the statement that defines it.
statementMap :: Stms lore -> Map VName (Stm lore)
statementMap stms =
  Map.fromList $ concatMap helper $ toList stms
  where
    helper stm = [(vname, stm) | vname <- patternNames $ stmPattern stm]

-- | Attemts to reorder statements by maintaining a stack of `VName` we need to
-- compute.
--
-- When looking at the item at the top of the stack `x`, find the list of
-- `VName` used in the the computation of `x` and insert those at the front of
-- the stack. If no no `VName` is required, insert the statement defining `x`
-- and add `x` and all other `VName` defined in that statement to the set of
-- values in `vtable`.
reorderStatements ::
  LocalScope ReorderLore m =>
  [VName] ->
  Names ->
  Map VName (Stm ReorderLore) ->
  Stms ReorderLore ->
  m (Stms ReorderLore)
reorderStatements [] _ _ acc = return acc
reorderStatements (x : xs) vtable m acc =
  if x `nameIn` vtable
    then reorderStatements xs vtable m acc
    else case Map.lookup x m of
      Just stm -> do
        let frees = freeIn stm
        mems <- mapM memInfo $ namesToList frees
        case namesToList $ (frees <> namesFromList (catMaybes mems)) `namesSubtract` vtable of
          [] -> do
            let vtable' = boundByStm stm <> vtable
            exp' <- mapExpM (mapper vtable') $ stmExp stm
            let acc' = acc |> stm {stmExp = exp'}
            inScopeOf stm $ reorderStatements xs vtable' m acc'
          todo -> reorderStatements (todo <> (x : xs)) vtable m acc
      Nothing ->
        -- The variable doesn't appear in the statement-map. We therefore assume
        -- that it comes from outside this body, and that it is already in
        -- vtable.
        reorderStatements xs (oneName x <> vtable) m acc

mapper :: LocalScope ReorderLore m => Names -> Mapper ReorderLore ReorderLore m
mapper vtable =
  identityMapper
    { mapOnBody = \s b ->
        localScope s $
          reorderBody vtable b,
      mapOnOp = onOp,
      mapOnRetType = return,
      mapOnBranchType = return,
      mapOnFParam = return,
      mapOnLParam = return,
      mapOnSubExp = return
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
      reorderKernelBody vtable body

    onLambda lam = do
      body' <- inScopeOf lam $ reorderBody vtable $ lambdaBody lam
      return $ lam {lambdaBody = body'}

optimise :: Pass KernelsMem KernelsMem
optimise =
  Pass "reorder statements" "reorder statements" $ \prog ->
    let prog' = aliasAnalysis prog
     in intraproceduralTransformationWithConsts return funHelper prog'
          <&> removeProgAliases
  where
    funHelper :: Stms ReorderLore -> FunDef ReorderLore -> PassM (FunDef ReorderLore)
    funHelper consts f = do
      let m = reorderBody (freeIn $ funDefParams f) $ funDefBody f
      return $ f {funDefBody = runReader m (scopeOf consts <> scopeOf f)}

nameInfoToMemInfo :: NameInfo ReorderLore -> MemBound NoUniqueness
nameInfoToMemInfo info =
  case info of
    FParamName summary -> noUniquenessReturns summary
    LParamName summary -> summary
    LetName (_, summary) -> summary
    IndexName it -> MemPrim $ IntType it

memInfo :: LocalScope ReorderLore m => VName -> m (Maybe VName)
memInfo vname = do
  summary <- asksScope (fmap nameInfoToMemInfo . Map.lookup vname)
  case summary of
    Just (MemArray _ _ _ (ArrayIn mem _)) ->
      return $ Just mem
    _ ->
      return Nothing
