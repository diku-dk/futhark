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

import Control.Applicative
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
import Futhark.Tools
import Futhark.Optimise.Simplifier (simplifyProgWithRules, noExtraHoistBlockers)
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.Simplifier.RuleM
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT

simplifyKernels :: MonadFreshNames m => Prog -> m Prog
simplifyKernels =
  simplifyProgWithRules bindableSimpleOps kernelRules noExtraHoistBlockers

simplifyFun :: MonadFreshNames m => FunDef -> m FunDef
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
    where bound_here = HS.fromList $ index : map kernelInputName inps ++ map fst ispace

  simplifyOp (ReduceKernel cs w kernel_size comm parlam seqlam arrs) = do
    cs' <- Engine.simplify cs
    w' <- Engine.simplify w
    kernel_size' <- Engine.simplify kernel_size
    arrs' <- mapM Engine.simplify arrs
    parlam' <- Engine.simplifyLambda parlam Nothing $ map (const Nothing) arrs
    seqlam' <- Engine.simplifyLambda seqlam Nothing $ map Just arrs
    let consumed_in_seq = consumedInBody $ lambdaBody seqlam'
        arr_params = drop 1 $ lambdaParams seqlam'
    forM_ (zip arr_params arrs) $ \(p,arr) ->
      when (paramName p `HS.member` consumed_in_seq) $
      Engine.consumedName arr
    return $ ReduceKernel cs' w' kernel_size' comm parlam' seqlam' arrs'

  simplifyOp (ScanKernel cs w kernel_size lam foldlam nes arrs) = do
    arrs' <- mapM Engine.simplify arrs
    ScanKernel <$> Engine.simplify cs <*> Engine.simplify w <*>
      Engine.simplify kernel_size <*>
      Engine.simplifyLambda lam Nothing (map (const Nothing) arrs') <*>
      Engine.simplifyLambda foldlam Nothing (map Just arrs') <*>
      mapM Engine.simplify nes <*>
      pure arrs'

  simplifyOp (ChunkedMapKernel cs w kernel_size o lam arrs) = do
    arrs' <- mapM Engine.simplify arrs
    ChunkedMapKernel <$> Engine.simplify cs <*> Engine.simplify w <*>
      Engine.simplify kernel_size <*> pure o <*>
      Engine.simplifyLambda lam Nothing (map Just arrs') <*>
      pure arrs'

  simplifyOp (WriteKernel cs ts i vs as) = do
    cs' <- Engine.simplify cs
    ts' <- mapM Engine.simplify ts
    i' <- Engine.simplify i
    vs' <- mapM Engine.simplify vs
    as' <- mapM Engine.simplify as
    return $ WriteKernel cs' ts' i' vs' as'

  simplifyOp NumGroups = return NumGroups
  simplifyOp GroupSize = return GroupSize

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
                LocalScope (Lore m) m,
                Op (Lore m) ~ Kernel (Lore m),
                Aliased (Lore m)) => RuleBook m
kernelRules = (std_td_rules <> topDownRules,
               std_bu_rules <> bottomUpRules)
  where (std_td_rules, std_bu_rules) = standardRules

topDownRules :: (MonadBinder m,
                 LocalScope (Lore m) m,
                 Op (Lore m) ~ Kernel (Lore m),
                 Aliased (Lore m)) => TopDownRules m
topDownRules = [removeUnusedKernelInputs
               , simplifyKernelInputs
               , removeInvariantKernelOutputs
               , fuseReduceIota
               , fuseScanIota
               , fuseChunkedMapIota
               ]

bottomUpRules :: (MonadBinder m,
                  LocalScope (Lore m) m,
                  Op (Lore m) ~ Kernel (Lore m)) => BottomUpRules m
bottomUpRules = [ removeDeadKernelOutputs
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
                         LocalScope (Lore m) m,
                         Op (Lore m) ~ Kernel (Lore m), Aliased (Lore m)) =>
                        TopDownRule m
simplifyKernelInputs vtable (Let pat _ (Op (MapKernel cs w index ispace inps returns body)))
  | (inps', extra_cs, extra_bnds) <- unzip3 $ map simplifyInput inps,
    inps /= catMaybes inps' = do
      body' <- localScope index_env $ insertBindingsM $ do
         forM_ (catMaybes extra_bnds) $ \(name, se) ->
           letBindNames'_ [name] $ PrimOp $ SubExp se
         return body
      letBind_ pat $ Op $
        MapKernel (cs++concat extra_cs) w index ispace
        (catMaybes inps') returns body'
  where defOf = (`ST.lookupExp` vtable)
        seType (Var v) = ST.lookupType v vtable
        seType (Constant v) = Just $ Prim $ primValueType v
        index_env = HM.fromList $ zip (map fst ispace) $ repeat IndexInfo
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
        isInvariant pat_elem ret (Constant v) = Left (pat_elem, ret, Constant v)
        isInvariant pat_elem ret se = Right (pat_elem, ret, se)
removeInvariantKernelOutputs _ _ = cannotSimplify

removeDeadKernelOutputs :: (MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) => BottomUpRule m
removeDeadKernelOutputs (_, used) (Let pat _ (Op (MapKernel cs w index ispace inps returns body)))
  | (used_pat_elems, used_returns, used_result) <-
      unzip3 $ filter usedOutput pats_rets_and_ses,
    used_returns /= returns = do
      let pat' = pat { patternValueElements = used_pat_elems }
      letBind_ pat' $ Op $
        MapKernel cs w index ispace inps used_returns
        body { bodyResult = used_result }
  where pats_rets_and_ses = zip3 (patternValueElements pat) returns $ bodyResult body
        usedOutput (pat_elem, _, _) = patElemName pat_elem `UT.used` used
removeDeadKernelOutputs _ _ = cannotSimplify

fuseReduceIota :: (LocalScope (Lore m) m,
                   MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                  TopDownRule m
fuseReduceIota vtable (Let pat _ (Op (ReduceKernel cs w size comm redlam foldlam arrs)))
  | Just f <- fuseIota vtable size comm foldlam arrs = do
      (foldlam', arrs') <- f
      letBind_ pat $ Op $ ReduceKernel cs w size comm redlam foldlam' arrs'
fuseReduceIota _ _ = cannotSimplify

fuseChunkedMapIota :: (LocalScope (Lore m) m,
                       MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                      TopDownRule m
fuseChunkedMapIota vtable (Let pat _ (Op (ChunkedMapKernel cs w size o lam arrs)))
  | Just f <- fuseIota vtable size comm lam arrs = do
      (lam', arrs') <- f
      letBind_ pat $ Op $ ChunkedMapKernel cs w size o lam' arrs'
  where comm = case o of Disorder -> Commutative
                         InOrder -> Noncommutative
fuseChunkedMapIota _ _ = cannotSimplify

fuseIota :: (LocalScope (Lore m) m, MonadBinder m) =>
            ST.SymbolTable (Lore m)
         -> KernelSize
         -> Commutativity
         -> LambdaT (Lore m)
         -> [VName]
         -> Maybe (m (LambdaT (Lore m), [VName]))
fuseIota vtable size comm lam arrs
  | null iota_params = Nothing
  | otherwise = Just $ do
      fold_body <- (uncurry (flip mkBodyM) =<<) $ collectBindings $ inScopeOf lam $ do
        case comm of
          Noncommutative -> do
            start_offset <- letSubExp "iota_start_offset" $
              PrimOp $ BinOp (Mul Int32) (Var thread_index) elems_per_thread
            forM_ iota_params $ \(p, x) -> do
              start <- letSubExp "iota_start" $
                PrimOp $ BinOp (Add Int32) start_offset x
              letBindNames'_ [p] $
                PrimOp $ Iota (Var $ paramName chunk_param) start $
                constant (1::Int32)
          Commutative ->
            forM_ iota_params $ \(p, x) -> do
              start <- letSubExp "iota_start" $
                PrimOp $ BinOp (Add Int32) (Var thread_index) x
              letBindNames'_ [p] $
                PrimOp $ Iota (Var $ paramName chunk_param) start
                num_threads
        mapM_ addBinding $ bodyBindings $ lambdaBody lam
        return $ bodyResult $ lambdaBody lam
      let (arr_params', arrs') = unzip params_and_arrs
          lam' = lam { lambdaBody = fold_body
                     , lambdaParams = Param thread_index (paramAttr chunk_param) :
                       chunk_param :
                       arr_params'
                     }
      return (lam', arrs')
  where elems_per_thread = kernelElementsPerThread size
        num_threads = kernelNumThreads size
        (thread_index, chunk_param, params_and_arrs, iota_params) =
          chunkedIotaParams vtable lam arrs

chunkedIotaParams :: ST.SymbolTable lore
                  -> LambdaT lore
                  -> [VName]
                  -> (VName,
                      Param (LParamAttr lore),
                      [(ParamT (LParamAttr lore), VName)],
                      [(VName, SubExp)])
chunkedIotaParams vtable lam arrs = (thread_index, chunk_param, params_and_arrs, iota_params)
  where (thread_index, chunk_param, arr_params) =
          partitionChunkedKernelLambdaParameters $ lambdaParams lam
        (params_and_arrs, iota_params) =
          iotaParams vtable arr_params arrs

iotaParams :: ST.SymbolTable lore
           -> [ParamT attr]
           -> [VName]
           -> ([(ParamT attr, VName)], [(VName, SubExp)])
iotaParams vtable arr_params arrs = partitionEithers $ zipWith isIota arr_params arrs
  where isIota p v
          | Just (Iota _ x (Constant (IntValue (Int32Value 1)))) <- asPrimOp =<< ST.lookupExp v vtable =
            Right (paramName p, x)
          | otherwise =
            Left (p, v)

fuseScanIota :: (LocalScope (Lore m) m,
                 MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                TopDownRule m
fuseScanIota vtable (Let pat _ (Op (ScanKernel cs w size lam foldlam nes arrs)))
  | not $ null iota_params = do
      fold_body <- (uncurry (flip mkBodyM) =<<) $ collectBindings $ inScopeOf foldlam $ do
        forM_ iota_params $ \(p, x) ->
          letBindNames'_ [p] $
          PrimOp $ BinOp (Add Int32) (Var thread_index) x
        mapM_ addBinding $ bodyBindings $ lambdaBody foldlam
        return $ bodyResult $ lambdaBody foldlam
      let (arr_params', arrs') = unzip params_and_arrs
          foldlam' = foldlam { lambdaBody = fold_body
                             , lambdaParams =
                                 Param thread_index (paramAttr other_index_param) :
                                 other_index_param :
                                 acc_params ++ arr_params'
                             }
      letBind_ pat $ Op $ ScanKernel cs w size lam foldlam' nes arrs'
   where (thread_index, other_index_param, acc_params, arr_params) =
           partitionChunkedKernelFoldParameters (length nes) $ lambdaParams foldlam
         (params_and_arrs, iota_params) =
           iotaParams vtable arr_params arrs
fuseScanIota _ _ = cannotSimplify
