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

simplifyKernels :: MonadFreshNames m => Prog -> m Prog
simplifyKernels =
  simplifyProgWithRules bindableSimpleOps kernelRules noExtraHoistBlockers

simplifyFun :: MonadFreshNames m => FunDef -> m FunDef
simplifyFun =
  Simplifier.simplifyFunWithRules bindableSimpleOps kernelRules Engine.noExtraHoistBlockers

instance (Attributes lore, Engine.SimplifiableOp lore (Op lore)) =>
         Engine.SimplifiableOp lore (Kernel lore) where
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

  simplifyOp (Kernel cs (num_groups, group_size, num_threads) ts space kernel_body) = do
    cs' <- Engine.simplify cs
    num_groups' <- Engine.simplify num_groups
    group_size' <- Engine.simplify group_size
    num_threads' <- Engine.simplify num_threads
    ts' <- mapM Engine.simplify ts
    space' <- Engine.simplify space
    kernel_body' <- Engine.localVtable (<>scope_vtable) $
      simplifyKernelBody scope kernel_body
    return $ Kernel cs' (num_groups', group_size', num_threads') ts' space' kernel_body'
    where scope_vtable = ST.fromScope scope
          scope = scopeOfKernelSpace space

  simplifyOp NumGroups = return NumGroups
  simplifyOp GroupSize = return GroupSize

simplifyKernelBody :: (Engine.MonadEngine m, Engine.Simplifiable res) =>
                      Scope (Lore m)
                   -> GenKernelBody res (Engine.InnerLore m)
                   -> m (GenKernelBody res (Lore m))
simplifyKernelBody scope (KernelBody stms res) =
  simplifyKernelStms scope stms $
    KernelBody [] <$> mapM Engine.simplify res

simplifyKernelStms :: Engine.MonadEngine m =>
                      Scope (Lore m)
                   -> [KernelStm (Engine.InnerLore m)]
                   -> m (GenKernelBody res (Lore m))
                   -> m (GenKernelBody res (Lore m))
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

simplifyKernelStm _ (Combine pe w v) = do
  pe' <- inspectPatElem pe mempty $ rangeOf v
  w' <- Engine.simplify w
  v' <- Engine.simplify v
  return [Combine pe' w' v']

simplifyKernelStm scope (GroupReduce pes w lam input) = do
  w' <- Engine.simplify w
  nes' <- mapM Engine.simplify nes
  arrs' <- mapM Engine.simplify arrs
  lam' <- Engine.simplifyLambdaNoHoistThese scope_bound lam (Just nes') (map (const Nothing) arrs')
  pes' <- inspectPatElems pes $ zip (repeat mempty) $ repeat (Nothing, Nothing)
  return [GroupReduce pes' w' lam' $ zip nes' arrs']
  where (nes,arrs) = unzip input
        scope_bound = HS.fromList $ HM.keys scope

simplifyKernelStm _ (GroupStream pes w lam accs arrs) = do
  w' <- Engine.simplify w
  accs' <- mapM Engine.simplify accs
  arrs' <- mapM Engine.simplify arrs
  lam' <- simplifyGroupStreamLambda lam w
          w (map (const Nothing) arrs')
  pes' <- inspectPatElems pes $
          zip (repeat mempty) $ repeat (Nothing, Nothing)
  return [GroupStream pes' w' lam' accs' arrs']

simplifyGroupStreamLambda :: Engine.MonadEngine m =>
                             GroupStreamLambda (Engine.InnerLore m)
                          -> SubExp -> SubExp -> [Maybe VName]
                          -> m (GroupStreamLambda (Lore m))
simplifyGroupStreamLambda lam w max_chunk arrs = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
  body' <- Engine.enterLoop $
           Engine.bindLParams acc_params $
           Engine.bindArrayLParams (zip arr_params arrs) $
           Engine.bindLoopVar block_size max_chunk $
           Engine.bindLoopVar block_offset w $
           simplifyKernelBody (scopeOf lam) body
  acc_params' <- mapM (Engine.simplifyParam Engine.simplify) acc_params
  arr_params' <- mapM (Engine.simplifyParam Engine.simplify) arr_params
  return $ GroupStreamLambda block_size block_offset acc_params' arr_params' body'

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

instance Engine.Simplifiable KernelSpace where
  simplify (KernelSpace global_tid local_tid group_id space) =
    KernelSpace global_tid local_tid group_id <$>
    (zip space_is <$> mapM Engine.simplify space_dims)
    where (space_is, space_dims) = unzip space

instance Engine.Simplifiable WhichThreads where
  simplify AllThreads =
    pure AllThreads
  simplify (OneThreadPerGroup which) =
    OneThreadPerGroup <$> Engine.simplify which
  simplify (ThreadsPerGroup limit) =
    ThreadsPerGroup <$> Engine.simplify limit
  simplify ThreadsInSpace =
    pure ThreadsInSpace

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
topDownRules = [ fuseScanIota
               , fuseWriteIota
               , fuseKernelIota

               , removeInvariantKernelResults
               ]

bottomUpRules :: (MonadBinder m,
                  LocalScope (Lore m) m,
                  Op (Lore m) ~ Kernel (Lore m)) => BottomUpRules m
bottomUpRules = [
                ]

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
fuseKernelIota vtable (Let pat _ (Op (Kernel cs ksize ts space kbody)))
  | Just (iota_chunk_info, stms') <- extractSplitIota vtable $ kernelBodyStms kbody =
      localScope (scopeOfKernelSpace space <>
                  mconcat (map scopeOf stms')) $ do
        stms'' <- mapM (inlineIotaInKernelStm iota_chunk_info) stms'
        let kbody' = kbody { kernelBodyStms = stms'' }
        letBind_ pat $ Op $ Kernel cs ksize ts space kbody'

  where (_, _, num_threads) = ksize
        thread_gid = spaceGlobalId space
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

-- if a Thread statement produces something invariant to the kernel,
-- just move it outside the kernel.  If a kernel produces something
-- invariant to the kernel, turn it into a replicate.
removeInvariantKernelResults :: (LocalScope (Lore m) m,
                                 MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                                TopDownRule m
removeInvariantKernelResults vtable (Let (Pattern [] kpes) attr
                                      (Op (Kernel cs size ts space (KernelBody kstms kres)))) = do
  (kstms', invariant_threads) <-
    unzip <$> mapM checkForInvarianceStm kstms

  let isInvariant (Var v)
        | v `elem` concat invariant_threads = True
      isInvariant se = inVtable se

  (kpes', kres') <-
    unzip <$> filterM (checkForInvarianceResult isInvariant) (zip kpes kres)

  -- Check if we did anything at all.
  when (null (concat invariant_threads) && kres == kres')
    cannotSimplify

  addBinding $ Let (Pattern [] kpes') attr $ Op $ Kernel cs size ts space $
    KernelBody kstms' kres'
  where inVtable Constant{} = True
        inVtable (Var v) = isJust $ ST.lookup v vtable

        (_, _, num_threads) = size
        space_dims = map snd $ spaceDimensions space

        checkForInvarianceStm (Thread pes threads body)
          | (invariant, variant) <-
              partition (inVtable . snd) $ zip pes $ bodyResult body,
            not $ null invariant = do
              forM_ invariant $ \(pe, se) ->
                letBindNames'_ [patElemName pe] $ PrimOp $ SubExp se
              let (pes', variant_res) = unzip variant
                  body' = body { bodyResult = variant_res }
              return (Thread pes' threads body',
                       map (patElemName . fst) invariant)
        checkForInvarianceStm stm =
          return (stm, [])

        checkForInvarianceResult isInvariant (pe, ThreadsReturn threads se)
          | isInvariant se =
              case threads of
                AllThreads -> do
                  letBindNames'_ [patElemName pe] $ PrimOp $ Replicate num_threads se
                  return False
                ThreadsInSpace -> do
                  let rep a d = PrimOp . Replicate d <$> letSubExp "rep" a
                  letBindNames'_ [patElemName pe] =<<
                    foldM rep (PrimOp (SubExp se)) (reverse space_dims)
                  return False
                _ -> return True
        checkForInvarianceResult _ _ =
          return True
removeInvariantKernelResults _ _ = cannotSimplify
