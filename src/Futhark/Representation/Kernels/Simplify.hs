{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Futhark.Representation.Kernels.Simplify
       ( simplifyKernels
       , simplifyFun
       )
where

import Control.Monad
import Data.Either
import Data.List hiding (any, all)
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

import Prelude hiding (any, all)

import Futhark.Representation.Kernels
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Optimise.Simplifier as Simplifier
import Futhark.Optimise.Simplifier.Rules
import Futhark.MonadFreshNames
import Futhark.Binder.Class
import Futhark.Construct
import Futhark.Optimise.Simplifier (simplifyProgWithRules, noExtraHoistBlockers)
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.Simplifier.RuleM
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST

simplifyKernels :: MonadFreshNames m => Prog -> m Prog
simplifyKernels =
  simplifyProgWithRules bindableSimpleOps kernelRules noExtraHoistBlockers

simplifyFun :: MonadFreshNames m => FunDec -> m FunDec
simplifyFun =
  Simplifier.simplifyFunWithRules bindableSimpleOps kernelRules Engine.noExtraHoistBlockers

instance (Attributes lore, Engine.SimplifiableOp lore (Op lore)) =>
         Engine.SimplifiableOp lore (Kernel lore) where
  simplifyOp (MapKernel cs w index ispace inps returns body) = do
    cs' <- Engine.simplify cs
    w' <- Engine.simplify w
    ispace' <- forM ispace $ \(i, bound) -> do
      bound' <- Engine.simplify bound
      return (i, bound')
    returns' <- forM returns $ \(t, perm) -> do
      t' <- Engine.simplify t
      return (t', perm)
    par_blocker <- Engine.asksEngineEnv $ Engine.blockHoistPar . Engine.envHoistBlockers
    Engine.enterLoop $ Engine.bindLoopVars ((index,w) : ispace) $ do
      inps' <- mapM simplifyKernelInput inps
      body' <- Engine.bindLParams (map kernelInputParam inps') $
               Engine.blockIf (Engine.hasFree bound_here `Engine.orIf`
                               Engine.isConsumed `Engine.orIf`
                               par_blocker) $
               Engine.simplifyBody (map (const Observe) returns) body
      return $ MapKernel cs' w' index ispace' inps' returns' body'
    where bound_here = HS.fromList $ map kernelInputName inps ++ map fst ispace

  simplifyOp (ReduceKernel cs w kernel_size parlam seqlam nes arrs) = do
    cs' <- Engine.simplify cs
    w' <- Engine.simplify w
    kernel_size' <- Engine.simplify kernel_size
    nes' <- mapM Engine.simplify nes
    arrs' <- mapM Engine.simplify arrs
    parlam' <- Engine.simplifyLambda parlam w' $ map (const Nothing) nes
    seqlam' <- Engine.simplifyLambda seqlam w' $ map (const Nothing) nes
    return $ ReduceKernel cs' w' kernel_size' parlam' seqlam' nes' arrs'

  simplifyOp (ScanKernel cs w kernel_size order lam input) = do
    let (nes, arrs) = unzip input
    cs' <- Engine.simplify cs
    w' <- Engine.simplify w
    kernel_size' <- Engine.simplify kernel_size
    nes' <- mapM Engine.simplify nes
    arrs' <- mapM Engine.simplify arrs
    lam' <- Engine.simplifyLambda lam w' $ map Just arrs'
    return $ ScanKernel cs' w' kernel_size' order lam' $ zip nes' arrs'

simplifyKernelInput :: Engine.MonadEngine m =>
                       KernelInput (Engine.InnerLore m) -> m (KernelInput (Lore m))
simplifyKernelInput (KernelInput param arr is) = do
  param' <- Engine.simplifyParam Engine.simplify param
  arr' <- Engine.simplify arr
  is' <- mapM Engine.simplify is
  return $ KernelInput param' arr' is'

instance Engine.Simplifiable KernelSize where
  simplify (KernelSize num_groups group_size thread_chunk
            num_elements offset_multiple num_threads) = do
    num_groups' <- Engine.simplify num_groups
    group_size' <- Engine.simplify group_size
    thread_chunk' <- Engine.simplify thread_chunk
    num_elements' <- Engine.simplify num_elements
    offset_multiple' <- Engine.simplify offset_multiple
    num_threads' <- Engine.simplify num_threads
    return $ KernelSize num_groups' group_size' thread_chunk' num_elements' offset_multiple' num_threads'

kernelRules :: (MonadBinder m,
                LocalTypeEnv (NameType (Lore m)) m,
                Op (Lore m) ~ Kernel (Lore m),
                Aliased (Lore m)) => RuleBook m
kernelRules = (std_td_rules <> topDownRules,
               std_bu_rules <> bottomUpRules)
  where (std_td_rules, std_bu_rules) = standardRules

topDownRules :: (MonadBinder m,
                 LocalTypeEnv (NameType (Lore m)) m,
                 Op (Lore m) ~ Kernel (Lore m),
                 Aliased (Lore m)) => TopDownRules m
topDownRules = [removeUnusedKernelInputs
               , simplifyKernelInputs
               , removeInvariantKernelOutputs
               ]

bottomUpRules :: (MonadBinder m,
                  LocalTypeEnv (NameType (Lore m)) m,
                  Op (Lore m) ~ Kernel (Lore m)) => BottomUpRules m
bottomUpRules = [
                ]

-- | Remove inputs that are not used inside the @kernel@.
removeUnusedKernelInputs :: (MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                            TopDownRule m
removeUnusedKernelInputs _ (Let pat _ (Op (MapKernel cs w index ispace inps returns body)))
  | (used,unused) <- partition usedInput inps,
    not (null unused) =
      letBind_ pat $ Op $ MapKernel cs w index ispace used returns body
  where used_in_body = freeInBody body
        usedInput inp = kernelInputName inp `HS.member` used_in_body
removeUnusedKernelInputs _ _ = cannotSimplify

-- | Kernel inputs are indexes into arrays.  Based on how those arrays
-- are defined, we may be able to simplify the input.
simplifyKernelInputs :: (MonadBinder m,
                         LocalTypeEnv (NameType (Lore m)) m,
                         Op (Lore m) ~ Kernel (Lore m), Aliased (Lore m)) =>
                        TopDownRule m
simplifyKernelInputs vtable (Let pat _ (Op (MapKernel cs w index ispace inps returns body)))
  | (inps', extra_cs, extra_bnds) <- unzip3 $ map simplifyInput inps,
    inps /= catMaybes inps' = do
      body' <- localTypeEnv index_env $ insertBindingsM $ do
         forM_ (catMaybes extra_bnds) $ \(name, se) ->
           letBindNames'_ [name] $ PrimOp $ SubExp se
         return body
      letBind_ pat $ Op $
        MapKernel (cs++concat extra_cs) w index ispace
        (catMaybes inps') returns body'
  where defOf = (`ST.lookupExp` vtable)
        seType (Var v) = ST.lookupType v vtable
        seType (Constant v) = Just $ Basic $ basicValueType v
        index_env = HM.fromList $ zip (map fst ispace) $ repeat IndexType
        consumed_in_body = consumedInBody body

        simplifyInput inp@(KernelInput param arr is) =
          case simplifyIndexing defOf seType arr is consumed of
            Just (IndexResult inp_cs arr' is') ->
              (Just $ KernelInput param arr' is', inp_cs, Nothing)
            Just (SubExpResult se) ->
              (Nothing, [], Just (paramName param, se))
            _ ->
              (Just inp, [], Nothing)
          where consumed = paramName param `HS.member` consumed_in_body
simplifyKernelInputs _ _ = cannotSimplify

removeInvariantKernelOutputs :: (MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                                TopDownRule m
removeInvariantKernelOutputs vtable (Let pat _ (Op (MapKernel cs w index ispace inps returns body)))
  | (invariant, variant) <-
      partitionEithers $ zipWith3 isInvariant
      (patternValueElements pat) returns $ bodyResult body,
    not $ null invariant = do
      let (variant_pat_elems, variant_returns, variant_result) =
            unzip3 variant
          pat' = Pattern [] variant_pat_elems
      forM_ invariant $ \(pat_elem, (t, perm), se) ->
        if perm /= sort perm
        then cannotSimplify
        else do
          flat <- letExp "kernel_invariant_flat" $ PrimOp $ Replicate w se
          let shape = map DimNew $ map snd ispace ++ arrayDims t
          letBind_ (Pattern [] [pat_elem]) $ PrimOp $ Reshape cs shape flat
      letBind_ pat' $ Op $
        MapKernel cs w index ispace inps variant_returns
        body { bodyResult = variant_result }
  where isInvariant pat_elem ret (Var v)
          | Just _ <- ST.lookupType v vtable = Left (pat_elem, ret, Var v)
        isInvariant pat_elem ret se = Right (pat_elem, ret, se)
removeInvariantKernelOutputs _ _ = cannotSimplify
