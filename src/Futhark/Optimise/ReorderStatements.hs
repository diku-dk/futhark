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
import qualified Data.Set as Set
import Debug.Trace
import Futhark.Analysis.Alias (aliasAnalysis)
import Futhark.IR.Aliases (Aliases, aliasesOf, consumedInStm, lookupAliases, patternAliases, removeProgAliases)
import Futhark.IR.KernelsMem
import Futhark.Pass (Pass (..), PassM, intraproceduralTransformationWithConsts)

-- traceWith s a = trace (s ++ ": " ++ pretty a) a

traceWith s a = a

type ReorderLore = Aliases KernelsMem

reorderBody :: LocalScope ReorderLore m => Names -> Body ReorderLore -> m (Body ReorderLore)
reorderBody alreadyInserted body@Body {bodyResult = res, bodyStms = stms} = do
  stms' <-
    reorderStatements
      (namesToList $ freeIn res)
      alreadyInserted
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
reorderKernelBody alreadyInserted kbody = do
  stms' <-
    reorderStatements
      (namesToList $ freeIn $ kernelBodyResult kbody)
      alreadyInserted
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
-- values in `alreadyInserted`.
reorderStatements ::
  LocalScope ReorderLore m =>
  [VName] ->
  Names ->
  Map VName (Stm ReorderLore) ->
  Stms ReorderLore ->
  m (Stms ReorderLore)
reorderStatements [] _ _ acc = return acc
reorderStatements (x : xs) alreadyInserted stm_map acc =
  if x `nameIn` alreadyInserted
    then reorderStatements xs alreadyInserted stm_map acc
    else case Map.lookup (traceWith "x" x) stm_map of
      Just stm -> do
        frees <- freeWithMems stm
        mems' <- inScopeOf stm $ mapM memInfo $ patternNames $ stmPattern stm
        let mems = catMaybes mems'
        consumed_arrays <- (`namesSubtract` namesFromList (patternNames $ stmPattern stm)) <$> namesFromList <$> concat <$> mapM (consumeArraysIn stm_map) (traceWith ("x: " ++ pretty x ++ ", stm: " ++ pretty (patternNames $ stmPattern stm) ++ ", mems") mems)
        let aliases' = mconcat $ patternAliases $ stmPattern stm
        let aliasesOfAliases =
              Map.elems stm_map
                & fmap stmPattern
                & traceWith "stmPatterns"
                & trace ("and acc: " ++ (pretty $ fmap patternNames $ fmap stmPattern $ toList acc))
                & filter ((`namesIntersect` aliases') . mconcat . patternAliases)
                & fmap patternNames
                & mconcat
                & namesFromList
        let aliases = aliases' <> aliasesOfAliases
        let consumed = traceWith ("x: " ++ pretty x ++ ", consumedArrays: " ++ pretty consumed_arrays ++ ", aliases: " ++ pretty aliases ++ ", consumedInStm") $ (consumedInStm stm <> (consumed_arrays `namesSubtract` aliases)) `namesSubtract` alreadyInserted
        let stm_vnames_using_consumed =
              ( foldl (<>) mempty $
                  Map.mapWithKey
                    ( \vname stm' ->
                        if consumed `namesIntersect` freeIn stm'
                          then oneName vname
                          else mempty
                    )
                    stm_map
              )
                `namesSubtract` boundByStm stm
        case traceWith "todo" $ namesToList $ stm_vnames_using_consumed <> (frees `namesSubtract` alreadyInserted) of
          [] -> do
            let alreadyInserted' = boundByStm stm <> alreadyInserted
            exp' <- mapExpM (mapper alreadyInserted') $ stmExp stm
            let acc' = acc |> stm {stmExp = exp'}
            let stm_map' = stm_map `Map.withoutKeys` (Set.fromList $ namesToList $ boundByStm stm)
            inScopeOf stm $ reorderStatements xs alreadyInserted' stm_map' acc'
          todo -> reorderStatements (todo <> (x : xs)) alreadyInserted stm_map acc
      Nothing ->
        -- The variable doesn't appear in the statement-map. We therefore assume
        -- that it comes from outside this body, and that it is already in
        -- alreadyInserted.
        reorderStatements xs (oneName x <> alreadyInserted) stm_map acc

consumeArraysIn :: LocalScope ReorderLore m => Map VName (Stm ReorderLore) -> VName -> m [VName]
consumeArraysIn stm_map mem =
  Map.elems stm_map
    & mapM
      ( \stm -> do
          let frees = freeIn stm
          filterM
            ( \vname -> do
                mem' <- memInfo vname
                return $ mem' == Just mem
            )
            $ namesToList frees
      )
    <&> concat

freeWithMems :: LocalScope ReorderLore m => Stm ReorderLore -> m Names
freeWithMems stm = do
  let frees = freeIn stm
  mems <- mapM memInfo $ namesToList frees
  return $ frees <> namesFromList (catMaybes mems)

mapper :: LocalScope ReorderLore m => Names -> Mapper ReorderLore ReorderLore m
mapper alreadyInserted =
  identityMapper
    { mapOnBody = \s b ->
        localScope s $
          reorderBody alreadyInserted b,
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
      reorderKernelBody alreadyInserted body

    onLambda lam = do
      body' <- inScopeOf lam $ reorderBody alreadyInserted $ lambdaBody lam
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
