{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.Kernels.Simplify
       ( simplifyKernels
       , simplifyFun
       )
where

import Control.Applicative
import Control.Arrow ((***))
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
import Futhark.Optimise.Simplifier (simplifyProgWithRules, noExtraHoistBlockers)
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.RuleM
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.Usage

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

  simplifyOp (Kernel cs space ts kernel_body) = do
    cs' <- Engine.simplify cs
    space' <- Engine.simplify space
    ts' <- mapM Engine.simplify ts
    kernel_body' <- Engine.localVtable (<>scope_vtable) $
      simplifyKernelBody True scope kernel_body
    return $ Kernel cs' space' ts' kernel_body'
    where scope_vtable = ST.fromScope scope
          scope = scopeOfKernelSpace space

  simplifyOp NumGroups = return NumGroups
  simplifyOp GroupSize = return GroupSize

hoistInKernelBody :: (Engine.MonadEngine m, FreeIn res, Engine.Simplifiable res) =>
                     Bool -> Scope (Lore m)
                  -> GenKernelBody res (Lore m)
                  -> m (GenKernelBody res (Lore m))
hoistInKernelBody par scope (KernelBody initial_stms kres) = do
  par_blocker <- if par then Engine.asksEngineEnv $
                             Engine.blockHoistPar . Engine.envHoistBlockers
                             else return $ Engine.isFalse True
  stms' <- hoistKernelStms (par_blocker `Engine.orIf` Engine.isConsumed)
           (HS.fromList $ HM.keys scope) live_stms
  return $ KernelBody stms' kres
  where hoistKernelStms block not_hoistable (Thread _ bnd : stms)
          | HS.null $ freeInBinding bnd `HS.intersection` not_hoistable,
            not $ block usage bnd = do
              addBinding bnd
              hoistKernelStms block not_hoistable stms
        hoistKernelStms block not_hoistable (stm:stms) = do
          rules <- Engine.asksEngineEnv Engine.envRules

          stm' <- case stm of
                    Thread threads bnd -> do
                      vtable <- Engine.getVtable
                      res <- bottomUpSimplifyBinding rules (vtable, usage) bnd
                      case res of Nothing -> return [stm]
                                  Just stm' -> return $ map (Thread threads) stm'
                    _ -> return [stm]

          let bound_by_stm = HS.fromList $ HM.keys $ scopeOf stm'
              not_hoistable' = not_hoistable <> bound_by_stm
          (stm'++) <$>
            Engine.localVtable (extendVtableWithStms [stm])
            (hoistKernelStms block not_hoistable' stms)
        hoistKernelStms _ _ [] =
          return []

        (usage, live_stms) = onlyLiveStms initial_stms

        usageInKernelStm (Thread _ bnd) =
          usageInBinding bnd
        usageInKernelStm (GroupReduce _ _ _ input) =
          mconcat $ map (UT.consumedUsage . snd) input
        usageInKernelStm _ =
          mempty

        onlyLiveStms [] = (UT.usages $ freeIn kres, [])
        onlyLiveStms (stm : stms) =
          let (usage', stms') = onlyLiveStms stms
          in if UT.contains usage' (HM.keys (scopeOf stm))
             then (usage' <> UT.usages (freeIn stm) <> usageInKernelStm stm,
                   stm : stms')
             else (usage',
                   stms')

simplifyKernelBody :: (Engine.MonadEngine m, FreeIn res, Engine.Simplifiable res) =>
                      Bool -> Scope (Lore m)
                   -> GenKernelBody res (Engine.InnerLore m)
                   -> m (GenKernelBody res (Lore m))
simplifyKernelBody par scope (KernelBody stms res) =
  hoistInKernelBody par scope =<<
  simplifyKernelStms scope stms
  (KernelBody [] <$> mapM Engine.simplify res)

simplifyKernelStms :: Engine.MonadEngine m =>
                      Scope (Lore m)
                   -> [KernelStm (Engine.InnerLore m)]
                   -> m (GenKernelBody res (Lore m))
                   -> m (GenKernelBody res (Lore m))
simplifyKernelStms _ [] m = m
simplifyKernelStms scope (stm:stms) m = do
  stm' <- concat <$> (mapM furtherSimplifyKernelStm =<< simplifyKernelStm scope stm)
  let scope' = scope <> scopeOf stm'
  KernelBody stms' res <-
    Engine.localVtable (extendVtableWithStms stm') $
    simplifyKernelStms scope' stms m
  return $ KernelBody (stm'<>stms') res

extendVtableWithStms :: Ranged lore =>
                        [KernelStm lore]
                     -> ST.SymbolTable lore -> ST.SymbolTable lore
extendVtableWithStms = flip $ foldl extendVtableWithStm
  where extendVtableWithStm vtable (Thread _ bnd) =
          ST.insertBinding bnd vtable
        extendVtableWithStm vtable stm =
          vtable <> ST.fromScope (scopeOf stm)

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

simplifyKernelStm _ (Thread threads bnd) = do
  threads' <- Engine.simplify threads
  bnds <- collectBindings_ $ Engine.simplifyBinding bnd
  return $ map (Thread threads') bnds

simplifyKernelStm _ (Combine pe cspace v) = do
  pe' <- inspectPatElem pe mempty $ rangeOf v
  cspace' <- mapM Engine.simplify cspace
  v' <- Engine.simplify v
  return [Combine pe' cspace' v']

simplifyKernelStm scope (GroupReduce pes w lam input) = do
  w' <- Engine.simplify w
  nes' <- mapM Engine.simplify nes
  arrs' <- mapM Engine.simplify arrs
  lam' <- Engine.simplifyLambdaNoHoistThese scope_bound lam (Just nes') (map (const Nothing) arrs')
  pes' <- inspectPatElems pes $ zip (repeat mempty) $ repeat (Nothing, Nothing)
  return [GroupReduce pes' w' lam' $ zip nes' arrs']
  where (nes,arrs) = unzip input
        scope_bound = HS.fromList $ HM.keys scope

simplifyKernelStm scope (GroupStream pes w maxchunk lam accs arrs) = do
  w' <- Engine.simplify w
  maxchunk' <- Engine.simplify maxchunk
  accs' <- mapM Engine.simplify accs
  arrs' <- mapM Engine.simplify arrs
  lam' <- simplifyGroupStreamLambda scope lam w
          w (map (const Nothing) arrs')
  pes' <- inspectPatElems pes $
          zip (repeat mempty) $ repeat (Nothing, Nothing)
  return [GroupStream pes' w' maxchunk' lam' accs' arrs']

simplifyKernelStm scope (GroupIf pes cond tb fb) = do
  pes' <- inspectPatElems pes $
          zip (repeat mempty) $ repeat (Nothing, Nothing)
  cond' <- Engine.simplify cond
  tb' <- simplifyKernelBody False scope tb
  fb' <- simplifyKernelBody False scope fb
  return [GroupIf pes' cond' tb' fb']

simplifyGroupStreamLambda :: Engine.MonadEngine m =>
                             Scope (Lore m)
                          -> GroupStreamLambda (Engine.InnerLore m)
                          -> SubExp -> SubExp -> [Maybe VName]
                          -> m (GroupStreamLambda (Lore m))
simplifyGroupStreamLambda scope lam w max_chunk arrs = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
  body' <- Engine.enterLoop $
           Engine.bindLParams acc_params $
           Engine.bindArrayLParams (zip arr_params arrs) $
           Engine.bindLoopVar block_size max_chunk $
           Engine.bindLoopVar block_offset w $
           simplifyKernelBody False (scope <> scopeOf lam) body
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

instance Engine.Simplifiable KernelSpace where
  simplify (KernelSpace gtid ltid gid num_threads num_groups group_size structure) =
    KernelSpace gtid ltid gid
    <$> Engine.simplify num_threads
    <*> Engine.simplify num_groups
    <*> Engine.simplify group_size
    <*> Engine.simplify structure

instance Engine.Simplifiable SpaceStructure where
  simplify (FlatSpace dims) =
    FlatSpace <$> (zip gtids <$> mapM Engine.simplify gdims)
    where (gtids, gdims) = unzip dims
  simplify (NestedSpace dims) =
    NestedSpace
    <$> (zip4 gtids
         <$> mapM Engine.simplify gdims
         <*> pure ltids
         <*> mapM Engine.simplify ldims)
    where (gtids, gdims, ltids, ldims) = unzip4 dims

instance Engine.Simplifiable KernelResult where
  simplify (ThreadsReturn threads what) =
    ThreadsReturn <$> Engine.simplify threads <*> Engine.simplify what
  simplify (ConcatReturns o w pte what) =
    ConcatReturns o
    <$> Engine.simplify w
    <*> Engine.simplify pte
    <*> Engine.simplify what

instance Engine.Simplifiable WhichThreads where
  simplify AllThreads =
    pure AllThreads
  simplify (OneThreadPerGroup which) =
    OneThreadPerGroup <$> Engine.simplify which
  simplify (ThreadsPerGroup limit) =
    ThreadsPerGroup <$> mapM Engine.simplify limit
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

-- | If an 'Iota' is input to a 'SplitArray', just inline the 'Iota'
-- instead.
fuseKernelIota :: (LocalScope (Lore m) m,
                  MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                 TopDownRule m
fuseKernelIota vtable (Let pat _ (Op (Kernel cs space ts kbody))) = do
  kstms' <- mapM inlineIota $ kernelBodyStms kbody

  when (concat kstms' == kernelBodyStms kbody)
    cannotSimplify

  letBind_ pat $ Op $ Kernel cs space ts kbody { kernelBodyStms = concat kstms' }
  where num_threads = spaceNumThreads space
        thread_gid = spaceGlobalId space
        inlineIota stm
          | Just ((size, chunk, o, elems_per_thread, x), stm') <- extractSplitIota vtable stm = do
              let iota_chunk_name = patElemName chunk
              bnds <- collectBindings_ $
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
              return $ stm' : map (Thread ThreadsInSpace) bnds

        inlineIota stm =
          return [stm]
fuseKernelIota _ _ = cannotSimplify

extractSplitIota :: ST.SymbolTable lore
                 -> KernelStm lore
                 -> Maybe ((VName, PatElemT (LetAttr lore), StreamOrd,
                            SubExp, SubExp),
                            KernelStm lore)
extractSplitIota vtable (SplitArray (size,chunks) o w per_thread_elems arrs)
  | ([iota_chunk_info], chunks_and_arrs) <- partitionEithers $
                                            zipWith isIota chunks arrs =
      let (chunks', arrs') = unzip chunks_and_arrs
      in Just (iota_chunk_info,
               SplitArray (size, chunks') o w per_thread_elems arrs')
  where isIota chunk arr
          | Just (PrimOp (Iota _ x (Constant s))) <- ST.lookupExp arr vtable,
            oneIsh s =
              Left (size, chunk, o, per_thread_elems, x)
          | otherwise =
              Right (chunk, arr)
extractSplitIota _ _ = Nothing

-- If a kernel produces something invariant to the kernel, turn it
-- into a replicate.
removeInvariantKernelResults :: (LocalScope (Lore m) m,
                                 MonadBinder m, Op (Lore m) ~ Kernel (Lore m)) =>
                                TopDownRule m

removeInvariantKernelResults vtable (Let (Pattern [] kpes) attr
                                      (Op (Kernel cs space ts (KernelBody kstms kres)))) = do
  (kpes', kres') <-
    unzip <$> filterM checkForInvarianceResult (zip kpes kres)

  -- Check if we did anything at all.
  when (kres == kres')
    cannotSimplify

  addBinding $ Let (Pattern [] kpes') attr $ Op $ Kernel cs space ts $
    KernelBody kstms kres'
  where isInvariant Constant{} = True
        isInvariant (Var v) = isJust $ ST.lookup v vtable

        num_threads = spaceNumThreads space
        space_dims = map snd $ spaceDimensions space

        checkForInvarianceResult (pe, ThreadsReturn threads se)
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
        checkForInvarianceResult _ =
          return True
removeInvariantKernelResults _ _ = cannotSimplify

-- We cannot apply simplification rules in the usual sense, so here's
-- a hack...
furtherSimplifyKernelStm :: Engine.MonadEngine m =>
                            KernelStm (Lore m)
                         -> m [KernelStm (Lore m)]
furtherSimplifyKernelStm stm = do
  vtable <- Engine.getVtable
  (stms, bnds) <- collectBindings $ tryFutherSimplifyKernelStm vtable stm
  if stms == [stm]
    then return [stm]
    else (map (Thread ThreadsInSpace) bnds++) <$>
         (concat <$> mapM furtherSimplifyKernelStm stms)

tryFutherSimplifyKernelStm :: Engine.MonadEngine m =>
                              ST.SymbolTable (Lore m)
                           -> KernelStm (Lore m)
                           -> m [KernelStm (Lore m)]
tryFutherSimplifyKernelStm vtable (GroupStream pes w maxchunk lam accs arrs)
  | ((arr_params, arrs'), (iota_arr_params, iotas)) <-
      extractStreamIota vtable (groupStreamArrParams lam) arrs,
    not $ null iota_arr_params = do
      let makeIotaBnd p x = do
            start <- letSubExp "iota_start" $
              PrimOp $ BinOp (Add Int32) (Var $ groupStreamChunkOffset lam) x
            letBindNames'_ [paramName p] $
              PrimOp $ Iota (Var $ groupStreamChunkSize lam) start $
              constant (1::Int32)
      iota_bnds <- collectBindings_ $
        zipWithM_ makeIotaBnd iota_arr_params iotas
      let lam_kbody = groupStreamLambdaBody lam
          lam_kbody' = lam_kbody { kernelBodyStms =
                                   map (Thread ThreadsInSpace) iota_bnds ++
                                   kernelBodyStms lam_kbody
                                 }
          lam' = lam { groupStreamLambdaBody = lam_kbody'
                     , groupStreamArrParams = arr_params
                     }
      return [GroupStream pes w maxchunk lam' accs arrs']

tryFutherSimplifyKernelStm _ stm = return [stm]

extractStreamIota :: ST.SymbolTable lore -> [Param attr] -> [VName]
                  -> (([Param attr], [VName]),
                      ([Param attr], [SubExp]))
extractStreamIota vtable arr_params arrs =
  (unzip *** unzip) $ partitionEithers $ zipWith isIotaParam arr_params arrs
  where isIotaParam p arr
          | Just (PrimOp (Iota _ x (Constant s))) <- ST.lookupExp arr vtable,
            oneIsh s =
             Right (p, x)
          | otherwise =
             Left (p, arr)
