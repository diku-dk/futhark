{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Futhark.Representation.Kernels.Simplify
       ( simplifyKernels
       , simplifyFun
       )
where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Foldable (any)
import Data.List hiding (any, all)
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

import Prelude hiding (any, all)

import Futhark.Representation.Kernels
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Representation.AST.Attributes.Ranges
import Futhark.Representation.Aliases (Names'(..))
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Optimise.Simplifier as Simplifier
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.Lore (VarWisdom(..))
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Transform.Substitute
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

  simplifyOp (ScanKernel cs w kernel_size lam foldlam nes arrs) = do
    arrs' <- mapM Engine.simplify arrs
    ScanKernel <$> Engine.simplify cs <*> Engine.simplify w <*>
      Engine.simplify kernel_size <*>
      Engine.simplifyLambda lam Nothing (map (const Nothing) arrs') <*>
      Engine.simplifyLambda foldlam Nothing (map Just arrs') <*>
      mapM Engine.simplify nes <*>
      pure arrs'

  simplifyOp (WriteKernel cs len lam ivs as) = do
    cs' <- Engine.simplify cs
    len' <- Engine.simplify len
    lam' <- Engine.simplifyLambda lam Nothing [] -- FIXME: Is this okay?
    ivs' <- mapM Engine.simplify ivs
    as' <- mapM Engine.simplify as
    return $ WriteKernel cs' len' lam' ivs' as'

  simplifyOp (Kernel cs (num_groups, group_size, num_threads) ts thread_id kernel_body) = do
    cs' <- Engine.simplify cs
    num_groups' <- Engine.simplify num_groups
    group_size' <- Engine.simplify group_size
    num_threads' <- Engine.simplify num_threads
    ts' <- mapM Engine.simplify ts
    kernel_body' <- Engine.localVtable (<>scope_vtable) $
      simplifyKernelBody scope kernel_body
    return $ Kernel cs' (num_groups', group_size', num_threads') ts' thread_id kernel_body'
    where scope_vtable = ST.fromScope scope
          scope = HM.singleton thread_id IndexInfo

  simplifyOp NumGroups = return NumGroups
  simplifyOp GroupSize = return GroupSize

simplifyKernelBody :: Engine.MonadEngine m =>
                      Scope (Lore m)
                   -> KernelBody (Engine.InnerLore m) -> m (KernelBody (Lore m))
simplifyKernelBody scope (KernelBody stms res) =
  simplifyKernelStms scope stms $
    KernelBody [] <$> mapM Engine.simplify res

simplifyKernelStms :: Engine.MonadEngine m =>
                      Scope (Lore m)
                   -> [KernelStm (Engine.InnerLore m)]
                   -> m (KernelBody (Lore m))
                   -> m (KernelBody (Lore m))
simplifyKernelStms _ [] m = m
simplifyKernelStms scope (stm:stms) m = do
  stm' <- simplifyKernelStm scope stm
  let stm_scope = mconcat $ map scopeOf stm'
      scope' = scope<>stm_scope
      scope_vtable = ST.fromScope scope'
  KernelBody stms' res <-
    Engine.localVtable (<>scope_vtable) $
    simplifyKernelStms scope' stms m
  return $ KernelBody (stm'<>stms') res

simplifyKernelStm :: Engine.MonadEngine m =>
                     Scope (Lore m) -> KernelStm (Engine.InnerLore m)
                  -> m [KernelStm (Lore m)]
simplifyKernelStm _ (SplitArray (size, chunks) o w elems_per_thread arrs) =
  pure <$> (SplitArray <$>
            ((,) <$> Engine.simplify size <*> zipWithM inspect chunks arrs)
            <*> pure o
            <*> Engine.simplify w
            <*> Engine.simplify elems_per_thread
            <*> mapM Engine.simplify arrs)
  where inspect chunk arr =
          inspectPatElem chunk (Names' $ HS.singleton arr) range
          where range = (Just $ VarBound arr, Just $ VarBound arr)
simplifyKernelStm _ (SplitIndexSpace ispace) =
  pure <$> (SplitIndexSpace <$>
            (zip (map fst ispace) <$> mapM (Engine.simplify . snd) ispace))
simplifyKernelStm scope (Thread pes threads body) = do
  par_blocker <- Engine.asksEngineEnv $
                 Engine.blockHoistPar . Engine.envHoistBlockers
  body' <- Engine.blockIf (Engine.hasFree scope_bound
                            `Engine.orIf` par_blocker
                            `Engine.orIf` Engine.isConsumed) $
           Engine.simplifyBody (map (const Observe) pes) body
  pes' <- inspectPatElems pes $ zip (map Names' $ bodyAliases body') (rangesOf body')
  threads' <- Engine.simplify threads
  return [Thread pes' threads' body']
  where scope_bound = HS.fromList $ HM.keys scope
simplifyKernelStm _ (Combine pe v) = do
  pe' <- inspectPatElem pe mempty $ rangeOf v
  v' <- Engine.simplify v
  return [Combine pe' v']
simplifyKernelStm scope (GroupReduce pes w lam input) = do
  w' <- Engine.simplify w
  nes' <- mapM Engine.simplify nes
  arrs' <- mapM Engine.simplify arrs
  lam' <- Engine.simplifyLambdaNoHoistThese scope_bound lam (Just nes') (map (const Nothing) arrs')
  pes' <- inspectPatElems pes $ zip (repeat mempty) $ repeat (Nothing, Nothing)
  return [GroupReduce pes' w' lam' $ zip nes' arrs']
  where (nes,arrs) = unzip input
        scope_bound = HS.fromList $ HM.keys scope

inspectPatElem :: (Engine.Simplifiable attr, Engine.MonadEngine m) =>
                  PatElemT attr
               -> Names'
               -> Range
               -> m (PatElemT (VarWisdom, attr))
inspectPatElem (PatElem name bindage lore) als range = do
  bindage' <- Engine.simplify bindage
  lore'  <- Engine.simplify lore
  return $ PatElem name bindage' (VarWisdom als range, lore')

inspectPatElems :: (Engine.Simplifiable attr, Engine.MonadEngine m) =>
                   [PatElemT attr]
                -> [(Names', Range)]
                -> m [PatElemT (VarWisdom, attr)]
inspectPatElems = zipWithM inspect
  where inspect pe (als, range) = inspectPatElem pe als range


instance Engine.Simplifiable KernelResult where
  simplify (ThreadsReturn threads what) =
    ThreadsReturn <$> Engine.simplify threads <*> Engine.simplify what
  simplify (ConcatReturns o w pte what) =
    ConcatReturns o
    <$> Engine.simplify w
    <*> Engine.simplify pte
    <*> Engine.simplify what

instance Engine.Simplifiable ThreadSpace where
  simplify = mapM Engine.simplify

instance Engine.Simplifiable WhichThreads where
  simplify AllThreads =
    pure AllThreads
  simplify (OneThreadPerGroup which) =
    OneThreadPerGroup <$> Engine.simplify which
  simplify (ThreadsInSpace w space) =
    ThreadsInSpace <$> Engine.simplify w <*> Engine.simplify space

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
               , fuseScanIota
               , fuseWriteIota
               , fuseKernelIota
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
simplifyKernelInputs vtable (Let pat _ (Op (MapKernel cs w index ispace inps returns body))) = do
  ((inps', extra_cs), extra_bnds) <- localScope index_env $ collectBindings $
    unzip <$> mapM simplifyInput inps
  when (inps == catMaybes inps') cannotSimplify

  body' <- localScope index_env $ insertBindingsM $ do
    mapM_ addBinding extra_bnds
    return body
  letBind_ pat $ Op $
    MapKernel (cs++concat extra_cs) w index ispace
    (catMaybes inps') returns body'
  where defOf = (`ST.lookupExp` vtable)
        seType (Var v) = ST.lookupType v vtable
        seType (Constant v) = Just $ Prim $ primValueType v
        indices = map fst ispace
        index_env = HM.fromList $ zip indices $ repeat IndexInfo
        consumed_in_body = consumedInBody body

        simplifyInput inp@(KernelInput param arr is)
          | Just m <- simplifyIndexing defOf seType arr is consumed = do
              (res, bnds) <- collectBindings m
              let free_in_bnds = mconcat $ map freeInBinding bnds
              case res of
                IndexResult inp_cs arr' is'
                  | paramName param `HS.member` consumed_in_body,
                    any (`HS.member` free_in_bnds) indices ->
                      return (Just inp, [])
                  | otherwise -> do
                      mapM_ addBinding bnds
                      letBindNames'_ [name] $ PrimOp $ Index inp_cs arr' is'
                      return (Nothing, inp_cs)
                SubExpResult se -> do
                  mapM_ addBinding bnds
                  letBindNames'_ [name] $ PrimOp $ SubExp se
                  return (Nothing, [])
          | otherwise =
              return (Just inp, [])
          where name = paramName param
                consumed = name `HS.member` consumed_in_body

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

fuseWriteIota :: (LocalScope (Lore m) m,
                  MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                 TopDownRule m
fuseWriteIota vtable (Let pat _ (Op (WriteKernel cs len lam ivs as)))
  | not $ null iota_params = do
      let (ivs_params', ivs') = unzip params_and_arrs

      body <- (uncurry (flip mkBodyM) =<<) $ collectBindings $ inScopeOf lam $ do
        forM_ iota_params $ \(p, x) ->
          letBindNames'_ [p] $
            PrimOp $ BinOp (Add Int32) (Var thread_index) x
        mapM_ addBinding $ bodyBindings $ lambdaBody lam
        return $ bodyResult $ lambdaBody lam

      let lam' = lam { lambdaBody = body
                     , lambdaParams = thread_index_param : ivs_params'
                     }

      letBind_ pat $ Op $ WriteKernel cs len lam' ivs' as
    where (params_and_arrs, iota_params) = iotaParams vtable ivs_params ivs
          (thread_index_param : ivs_params) = lambdaParams lam
          thread_index = paramName thread_index_param
fuseWriteIota _ _ = cannotSimplify

-- | If an 'Iota' is input to a 'SplitArray' which is then used only
-- in 'Thread' bodies, inline the 'Iota' in those bodies.
fuseKernelIota :: (LocalScope (Lore m) m,
                  MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                 TopDownRule m
fuseKernelIota vtable (Let pat _ (Op (Kernel cs ksize ts thread_gid kbody)))
  | Just (iota_chunk_info, stms') <- extractSplitIota vtable $ kernelBodyStms kbody =
      localScope (HM.singleton thread_gid IndexInfo <>
                  mconcat (map scopeOf stms')) $ do
        stms'' <- mapM (inlineIotaInKernelStm iota_chunk_info) stms'
        let kbody' = kbody { kernelBodyStms = stms'' }
        letBind_ pat $ Op $ Kernel cs ksize ts thread_gid kbody'

  where (_, _, num_threads) = ksize
        inlineIotaInKernelStm (size, chunk, o, elems_per_thread, x) stm
          | patElemName chunk `HS.member` freeIn stm =
              case stm of
                Thread pes threads body -> do
                  -- Invent new name, substitute, and insert.
                  iota_chunk_name <- newVName $ baseString $ patElemName chunk
                  let subst = HM.singleton (patElemName chunk) iota_chunk_name
                  body' <- (uncurry (flip mkBodyM) =<<) $ collectBindings $ do
                    case o of
                      InOrder -> do
                        start_offset <- letSubExp "iota_start_offset" $
                          PrimOp $ BinOp (Mul Int32) (Var thread_gid) elems_per_thread
                        start <- letSubExp "iota_start" $
                          PrimOp $ BinOp (Add Int32) start_offset x
                        letBindNames'_ [iota_chunk_name] $
                          PrimOp $ Iota (Var size) start $ constant (1::Int32)
                      Disorder -> do
                          start <- letSubExp "iota_start" $
                            PrimOp $ BinOp (Add Int32) (Var thread_gid) x
                          letBindNames'_ [iota_chunk_name] $
                            PrimOp $ Iota (Var size) start num_threads
                    bodyBind $ substituteNames subst body
                  return $ Thread pes threads body'
                _ -> cannotSimplify
        inlineIotaInKernelStm _ stm =
          return stm

fuseKernelIota _ _ = cannotSimplify

extractSplitIota :: ST.SymbolTable lore
                 -> [KernelStm lore]
                 -> Maybe ((VName, PatElemT (LetAttr lore), StreamOrd,
                            SubExp, SubExp),
                            [KernelStm lore])
extractSplitIota vtable (SplitArray (size,chunks) o w per_thread_elems arrs : stms)
  | ([iota_chunk_info], chunks_and_arrs) <- partitionEithers $
                                            zipWith isIota chunks arrs =
      let (chunks', arrs') = unzip chunks_and_arrs
      in Just (iota_chunk_info,
               SplitArray (size, chunks') o w per_thread_elems arrs' : stms)
  where isIota chunk arr
          | Just (PrimOp (Iota _ x (Constant s))) <- ST.lookupExp arr vtable,
            oneIsh s =
              Left (size, chunk, o, per_thread_elems, x)
          | otherwise =
              Right (chunk, arr)
extractSplitIota vtable (stm:stms) = do
  (iota_chunk_info, stms') <- extractSplitIota vtable stms
  return (iota_chunk_info, stm:stms')
extractSplitIota _ [] = Nothing
