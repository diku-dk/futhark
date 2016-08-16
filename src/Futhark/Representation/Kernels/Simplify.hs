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

       -- * Building blocks
       , simplifyKernelOp
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
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Optimise.Simplifier as Simplifier
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.Lore
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Optimise.Simplifier (simplifyProgWithRules, noExtraHoistBlockers)
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.RuleM
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Analysis.Rephrase (castBinding)

simplifyKernels :: MonadFreshNames m => Prog Kernels -> m (Prog Kernels)
simplifyKernels =
  simplifyProgWithRules bindableSimpleOps kernelRules noExtraHoistBlockers

simplifyFun :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
simplifyFun =
  Simplifier.simplifyFunWithRules bindableSimpleOps kernelRules Engine.noExtraHoistBlockers

instance Engine.SimplifiableOp Kernels (Kernel InKernel) where
  simplifyOp = simplifyKernelOp bindableSimpleOps inKernelEnv

simplifyKernelOp :: (Engine.MonadEngine (SimpleM lore),
                     Engine.MonadEngine m,
                     SameScope (Engine.InnerLore m) lore,
                     ExpAttr (Engine.InnerLore m) ~ ExpAttr lore,
                     BodyAttr (Engine.InnerLore m) ~ BodyAttr lore,
                     RetType (Engine.InnerLore m) ~ RetType lore,
                     BodyAttr lore ~ ()) =>
                    SimpleOps (SimpleM lore) -> Engine.Env (SimpleM lore)
                 -> Kernel lore -> m (Kernel (Wise lore))
simplifyKernelOp ops env (ScanKernel cs w kernel_size lam foldlam nes arrs) = do
  arrs' <- mapM Engine.simplify arrs
  outer_vtable <- Engine.getVtable
  (lam', lam_hoisted) <-
    subSimpleM ops env outer_vtable $
    Engine.simplifyLambda lam Nothing (map (const Nothing) arrs')
  (foldlam', foldlam_hoisted) <-
    subSimpleM ops env outer_vtable $
    Engine.simplifyLambda foldlam Nothing (map Just arrs')
  mapM_ processHoistedBinding lam_hoisted
  mapM_ processHoistedBinding foldlam_hoisted
  ScanKernel <$> Engine.simplify cs <*> Engine.simplify w <*>
    Engine.simplify kernel_size <*>
    pure lam' <*>
    pure foldlam' <*>
    mapM Engine.simplify nes <*>
    pure arrs'

simplifyKernelOp ops env (Kernel cs space ts kbody) = do
  cs' <- Engine.simplify cs
  space' <- Engine.simplify space
  ts' <- mapM Engine.simplify ts
  outer_vtable <- Engine.getVtable
  ((kbody_res', kbody_bnds'), kbody_hoisted) <-
    subSimpleM ops env outer_vtable $ do
      par_blocker <- Engine.asksEngineEnv $ Engine.blockHoistPar . Engine.envHoistBlockers
      Engine.localVtable (<>scope_vtable) $
        Engine.blockIf (Engine.hasFree bound_here
                        `Engine.orIf` Engine.isOp
                        `Engine.orIf` par_blocker
                        `Engine.orIf` Engine.isConsumed) $
        simplifyKernelBody kbody
  mapM_ processHoistedBinding kbody_hoisted
  return $ Kernel cs' space' ts' $ mkWiseKernelBody () kbody_bnds' kbody_res'
  where scope_vtable = ST.fromScope scope
        scope = scopeOfKernelSpace space
        bound_here = HS.fromList $ HM.keys scope

simplifyKernelOp _ _ NumGroups = return NumGroups
simplifyKernelOp _ _ GroupSize = return GroupSize

processHoistedBinding :: (PrettyLore from, MonadBinder m, ExpAttr from ~ ExpAttr (Lore m),
                          BodyAttr from ~ BodyAttr (Lore m), RetType from ~ RetType (Lore m),
                          LetAttr from ~ LetAttr (Lore m),
                          FParamAttr from ~ FParamAttr (Lore m),
                          LParamAttr from ~ LParamAttr (Lore m)) =>
                         Binding from -> m ()
processHoistedBinding bnd
  | Just bnd' <- castBinding bnd =
      addBinding bnd'
  | otherwise =
      fail $ "Cannot hoist binding: " ++ pretty bnd

mkWiseKernelBody :: (Attributes lore, CanBeWise (Op lore)) =>
                    BodyAttr lore -> [Binding (Wise lore)] -> [KernelResult] -> KernelBody (Wise lore)
mkWiseKernelBody attr bnds res =
  let Body attr' _ _ = mkWiseBody attr bnds res_vs
  in KernelBody attr' bnds res
  where res_vs = map resValue res
        resValue (ThreadsReturn _ se) = se
        resValue (WriteReturn _ _ _ se) = se
        resValue (ConcatReturns _ _ _ v) = Var v

inKernelEnv :: Engine.Env (SimpleM InKernel)
inKernelEnv = Engine.emptyEnv inKernelRules noExtraHoistBlockers

instance (Attributes lore,
          Engine.SimplifiableOp lore (Op lore)) =>
         (Engine.SimplifiableOp lore (KernelExp lore)) where
  simplifyOp (SplitArray o w i num_is elems_per_thread arrs) =
    SplitArray
    <$> pure o
    <*> Engine.simplify w
    <*> Engine.simplify i
    <*> Engine.simplify num_is
    <*> Engine.simplify elems_per_thread
    <*> mapM Engine.simplify arrs

  simplifyOp (SplitSpace o w i num_is elems_per_thread) =
    SplitSpace
    <$> pure o
    <*> Engine.simplify w
    <*> Engine.simplify i
    <*> Engine.simplify num_is
    <*> Engine.simplify elems_per_thread

  simplifyOp (Combine cspace ts body) = do
    (body_res', body_bnds') <-
      Engine.blockIf (Engine.isFalse False) $
      Engine.simplifyBody (map (const Observe) ts) body
    body' <- mkBodyM body_bnds' body_res'
    Combine
      <$> mapM Engine.simplify cspace
      <*> mapM Engine.simplify ts
      <*> pure body'

  simplifyOp (GroupReduce w lam input) = do
    w' <- Engine.simplify w
    nes' <- mapM Engine.simplify nes
    arrs' <- mapM Engine.simplify arrs
    lam' <- Engine.simplifyLambdaSeq lam (Just nes') (map (const Nothing) arrs')
    return $ GroupReduce w' lam' $ zip nes' arrs'
    where (nes,arrs) = unzip input

  simplifyOp (GroupStream w maxchunk lam accs arrs) = do
    w' <- Engine.simplify w
    maxchunk' <- Engine.simplify maxchunk
    accs' <- mapM Engine.simplify accs
    arrs' <- mapM Engine.simplify arrs
    lam' <- simplifyGroupStreamLambda lam w' maxchunk' $
            map (const Nothing) arrs'
    return $ GroupStream w' maxchunk' lam' accs' arrs'

simplifyKernelBody :: Engine.MonadEngine m =>
                      KernelBody (Engine.InnerLore m)
                   -> m [KernelResult]
simplifyKernelBody (KernelBody _ stms res) = do
  mapM_ Engine.simplifyBinding stms
  mapM Engine.simplify res

simplifyGroupStreamLambda :: Engine.MonadEngine m =>
                             GroupStreamLambda (Engine.InnerLore m)
                          -> SubExp -> SubExp -> [Maybe VName]
                          -> m (GroupStreamLambda (Lore m))
simplifyGroupStreamLambda lam w max_chunk arrs = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
      bound_here = HS.fromList $ block_size : block_offset :
                   map paramName (acc_params ++ arr_params)
  (body_res', body_bnds') <-
    Engine.enterLoop $
    Engine.bindLParams acc_params $
    Engine.bindArrayLParams (zip arr_params arrs) $
    Engine.bindLoopVar block_size max_chunk $
    Engine.bindLoopVar block_offset w $
    Engine.blockIf (Engine.hasFree bound_here `Engine.orIf` Engine.isConsumed) $
    Engine.simplifyBody (repeat Observe) body
  acc_params' <- mapM (Engine.simplifyParam Engine.simplify) acc_params
  arr_params' <- mapM (Engine.simplifyParam Engine.simplify) arr_params
  body' <- mkBodyM body_bnds' body_res'
  return $ GroupStreamLambda block_size block_offset acc_params' arr_params' body'

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
  simplify (WriteReturn w a i v) =
    WriteReturn <$>
    Engine.simplify w <*>
    Engine.simplify a <*>
    Engine.simplify i <*>
    Engine.simplify v
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
                Lore m ~ Wise Kernels) => RuleBook m
kernelRules = (std_td_rules <>
               [fuseScanIota
               , removeInvariantKernelResults
               ],
               std_bu_rules)
  where (std_td_rules, std_bu_rules) = standardRules


iotaParams :: ST.SymbolTable lore
           -> [ParamT attr]
           -> [VName]
           -> ([(ParamT attr, SubExp)], [(ParamT attr, VName)])
iotaParams vtable arr_params arrs = partitionEithers $ zipWith (isIota vtable) arr_params arrs

fuseScanIota :: (MonadBinder m,
                 Lore m ~ Wise Kernels,
                 LocalScope (Lore m) m,
                 Op (Lore m) ~ Kernel (Wise InKernel)) =>
                TopDownRule m
fuseScanIota vtable (Let pat _ (Op (ScanKernel cs w size lam foldlam nes arrs)))
  | not $ null iota_params = do
      (fold_body, []) <- subSimpleM bindableSimpleOps inKernelEnv vtable $
        (uncurry (flip mkBodyM) =<<) $ collectBindings $ localScope (castScope $ scopeOf foldlam) $ do
          forM_ iota_params $ \(p, x) ->
            letBindNames'_ [paramName p] $
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
         (iota_params, params_and_arrs) =
           iotaParams vtable arr_params arrs
fuseScanIota _ _ = cannotSimplify

-- | If an 'Iota' is input to a 'SplitArray', just inline the 'Iota'
-- instead.
fuseSplitIota :: (LocalScope (Lore m) m,
                  MonadBinder m, Lore m ~ Wise InKernel) =>
                 TopDownRule m
fuseSplitIota vtable (Let (Pattern [size] chunks) _
                       (Op (SplitArray o w i num_is elems_per_thread arrs)))
  | ([(iota_pe, iota_start)], chunks_and_arrs) <-
      partitionEithers $ zipWith (isIota vtable) chunks arrs = do

      let (chunks', arrs') = unzip chunks_and_arrs

      case chunks' of
        [] -> -- Can't create a binding that doesn't produce any values.
          letBind_ (Pattern [] [size]) $
          Op $ SplitSpace o w i num_is elems_per_thread
        _ ->
          letBind_ (Pattern [size] chunks') $
          Op $ SplitArray o w i num_is elems_per_thread arrs'

      case o of
        InOrder -> do
          start_offset <- letSubExp "iota_start_offset" $
            PrimOp $ BinOp (Mul Int32) i elems_per_thread
          start <- letSubExp "iota_start" $
            PrimOp $ BinOp (Add Int32) start_offset iota_start
          letBindNames'_ [patElemName iota_pe] $
            PrimOp $ Iota (Var $ patElemName size) start $ constant (1::Int32)
        Disorder -> do
          start <- letSubExp "iota_start" $
            PrimOp $ BinOp (Add Int32) i iota_start
          letBindNames'_ [patElemName iota_pe] $
            PrimOp $ Iota (Var $ patElemName size) start num_is
fuseSplitIota _ _ = cannotSimplify

fuseStreamIota :: (LocalScope (Lore m) m,
                  MonadBinder m, Lore m ~ Wise InKernel) =>
                 TopDownRule m
fuseStreamIota vtable (Let pat _ (Op (GroupStream w max_chunk lam accs arrs)))
  | ([(iota_param, iota_start)], params_and_arrs) <-
      partitionEithers $ zipWith (isIota vtable) (groupStreamArrParams lam) arrs = do

      let (arr_params', arrs') = unzip params_and_arrs
          chunk_size = groupStreamChunkSize lam
          offset = groupStreamChunkOffset lam

      body' <- insertBindingsM $ do
        start <- letSubExp "iota_start" $
            PrimOp $ BinOp (Add Int32) (Var offset) iota_start
        letBindNames'_ [paramName iota_param] $
          PrimOp $ Iota (Var chunk_size) start $ constant (1::Int32)
        return $ groupStreamLambdaBody lam
      let lam' = lam { groupStreamArrParams = arr_params',
                       groupStreamLambdaBody = body'
                     }
      letBind_ pat $ Op $ GroupStream w max_chunk lam' accs arrs'
fuseStreamIota _ _ = cannotSimplify

isIota :: ST.SymbolTable lore -> a -> VName -> Either (a, SubExp) (a, VName)
isIota vtable chunk arr
  | Just (PrimOp (Iota _ x (Constant s))) <- ST.lookupExp arr vtable,
    oneIsh s =
      Left (chunk, x)
  | otherwise =
      Right (chunk, arr)

-- If a kernel produces something invariant to the kernel, turn it
-- into a replicate.
removeInvariantKernelResults :: (LocalScope (Lore m) m,
                                 MonadBinder m,
                                 Lore m ~ Wise Kernels) =>
                                TopDownRule m

removeInvariantKernelResults vtable (Let (Pattern [] kpes) attr
                                      (Op (Kernel cs space ts (KernelBody _ kstms kres)))) = do
  (kpes', kres') <-
    unzip <$> filterM checkForInvarianceResult (zip kpes kres)

  -- Check if we did anything at all.
  when (kres == kres')
    cannotSimplify

  addBinding $ Let (Pattern [] kpes') attr $ Op $ Kernel cs space ts $
    mkWiseKernelBody () kstms kres'
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

inKernelRules :: (MonadBinder m,
                  LocalScope (Lore m) m,
                  Lore m ~ Wise InKernel) => RuleBook m
inKernelRules = (std_td_rules <>
                 [fuseSplitIota,
                  fuseStreamIota],
                 std_bu_rules)
  where (std_td_rules, std_bu_rules) = standardRules
