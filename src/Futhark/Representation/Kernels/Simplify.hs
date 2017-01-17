{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.Kernels.Simplify
       ( simplifyKernels
       , simplifyFun

       , simpleKernels

       -- * Building blocks
       , simplifyKernelOp
       , simplifyKernelExp
       )
where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

import Prelude

import Futhark.Representation.Kernels
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Optimise.Simplifier as Simplifier
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.Lore
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Optimise.Simplifier (simplifyProgWithRules, noExtraHoistBlockers)
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.RuleM
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.Rephrase (castStm)
import Futhark.Util.IntegralExp (quotRoundingUp)

simpleKernels :: Simplifier.SimpleOps Kernels
simpleKernels = Simplifier.bindableSimpleOps (simplifyKernelOp simpleInKernel inKernelEnv)

simpleInKernel :: Simplifier.SimpleOps InKernel
simpleInKernel = Simplifier.bindableSimpleOps simplifyKernelExp

simplifyKernels :: MonadFreshNames m => Prog Kernels -> m (Prog Kernels)
simplifyKernels =
  simplifyProgWithRules simpleKernels kernelRules noExtraHoistBlockers

simplifyFun :: MonadFreshNames m => FunDef Kernels -> m (FunDef Kernels)
simplifyFun =
  Simplifier.simplifyFunWithRules simpleKernels kernelRules noExtraHoistBlockers

simplifyKernelOp :: (Engine.SimplifiableLore lore,
                     Engine.SimplifiableLore outerlore,
                     BodyAttr outerlore ~ (), BodyAttr lore ~ (),
                     ExpAttr lore ~ ExpAttr outerlore,
                     SameScope lore outerlore,
                     RetType lore ~ RetType outerlore) =>
                    Engine.SimpleOps lore -> Engine.Env (Engine.SimpleM lore)
                 -> Kernel lore -> Engine.SimpleM outerlore (Kernel (Wise lore))
simplifyKernelOp ops env (Kernel desc cs space ts kbody) = do
  cs' <- Engine.simplify cs
  space' <- Engine.simplify space
  ts' <- mapM Engine.simplify ts
  outer_vtable <- Engine.getVtable
  ((kbody_res', kbody_bnds'), again, kbody_hoisted) <-
    Engine.subSimpleM ops env outer_vtable $ do
      par_blocker <- Engine.asksEngineEnv $ Engine.blockHoistPar . Engine.envHoistBlockers
      Engine.localVtable (<>scope_vtable) $
        Engine.blockIf (Engine.hasFree bound_here
                        `Engine.orIf` Engine.isOp
                        `Engine.orIf` par_blocker
                        `Engine.orIf` Engine.isConsumed) $
        simplifyKernelBody kbody
  when again Engine.changed
  mapM_ processHoistedStm kbody_hoisted
  return $ Kernel desc cs' space' ts' $ mkWiseKernelBody () kbody_bnds' kbody_res'
  where scope_vtable = ST.fromScope scope
        scope = scopeOfKernelSpace space
        bound_here = HS.fromList $ HM.keys scope

simplifyKernelOp _ _ NumGroups = return NumGroups
simplifyKernelOp _ _ GroupSize = return GroupSize
simplifyKernelOp _ _ TileSize = return TileSize
simplifyKernelOp _ _ (SufficientParallelism se) =
  SufficientParallelism <$> Engine.simplify se

processHoistedStm :: (PrettyLore from, MonadBinder m, ExpAttr from ~ ExpAttr (Lore m),
                      BodyAttr from ~ BodyAttr (Lore m), RetType from ~ RetType (Lore m),
                      LetAttr from ~ LetAttr (Lore m),
                      FParamAttr from ~ FParamAttr (Lore m),
                      LParamAttr from ~ LParamAttr (Lore m)) =>
                     Stm from -> m ()
processHoistedStm bnd
  | Just bnd' <- castStm bnd =
      addStm bnd'
  | otherwise =
      fail $ "Cannot hoist binding: " ++ pretty bnd

mkWiseKernelBody :: (Attributes lore, CanBeWise (Op lore)) =>
                    BodyAttr lore -> [Stm (Wise lore)] -> [KernelResult] -> KernelBody (Wise lore)
mkWiseKernelBody attr bnds res =
  let Body attr' _ _ = mkWiseBody attr bnds res_vs
  in KernelBody attr' bnds res
  where res_vs = map resValue res
        resValue (ThreadsReturn _ se) = se
        resValue (WriteReturn _ _ _ se) = se
        resValue (ConcatReturns _ _ _ _ v) = Var v
        resValue (KernelInPlaceReturn v) = Var v

inKernelEnv :: Engine.Env (Engine.SimpleM InKernel)
inKernelEnv = Engine.emptyEnv inKernelRules noExtraHoistBlockers

instance Engine.Simplifiable SplitOrdering where
  simplify SplitContiguous =
    return SplitContiguous
  simplify (SplitStrided stride) =
    SplitStrided <$> Engine.simplify stride

simplifyKernelExp :: Engine.SimplifiableLore lore =>
                     KernelExp lore -> Engine.SimpleM lore (KernelExp (Wise lore))
simplifyKernelExp (SplitArray o w i elems_per_thread arrs) =
  SplitArray
  <$> Engine.simplify o
  <*> Engine.simplify w
  <*> Engine.simplify i
  <*> Engine.simplify elems_per_thread
  <*> mapM Engine.simplify arrs

simplifyKernelExp (SplitSpace o w i elems_per_thread) =
  SplitSpace
  <$> Engine.simplify o
  <*> Engine.simplify w
  <*> Engine.simplify i
  <*> Engine.simplify elems_per_thread

simplifyKernelExp (Combine cspace ts active body) = do
  (body_res', body_bnds') <-
    Engine.blockIf Engine.isNotSafe $
    Engine.simplifyBody (map (const Observe) ts) body
  body' <- mkBodyM body_bnds' body_res'
  Combine
    <$> mapM Engine.simplify cspace
    <*> mapM Engine.simplify ts
    <*> Engine.simplify active
    <*> pure body'

simplifyKernelExp (GroupReduce w lam input) = do
  arrs' <- mapM Engine.simplify arrs
  nes' <- mapM Engine.simplify nes
  w' <- Engine.simplify w
  lam' <- Engine.simplifyLambdaSeq lam (Just nes') (map (const Nothing) arrs')
  return $ GroupReduce w' lam' $ zip nes' arrs'
  where (nes,arrs) = unzip input

simplifyKernelExp (GroupScan w lam input) = do
  w' <- Engine.simplify w
  nes' <- mapM Engine.simplify nes
  arrs' <- mapM Engine.simplify arrs
  lam' <- Engine.simplifyLambdaSeq lam (Just nes') (map (const Nothing) arrs')
  return $ GroupScan w' lam' $ zip nes' arrs'
  where (nes,arrs) = unzip input

simplifyKernelExp (GroupStream w maxchunk lam accs arrs) = do
  w' <- Engine.simplify w
  maxchunk' <- Engine.simplify maxchunk
  accs' <- mapM Engine.simplify accs
  arrs' <- mapM Engine.simplify arrs
  lam' <- simplifyGroupStreamLambda lam w' maxchunk' $
          map (const Nothing) arrs'
  return $ GroupStream w' maxchunk' lam' accs' arrs'

simplifyKernelBody :: Engine.SimplifiableLore lore =>
                      KernelBody lore
                   -> Engine.SimpleM lore [KernelResult]
simplifyKernelBody (KernelBody _ stms res) = do
  mapM_ Engine.simplifyStm stms
  mapM Engine.simplify res

simplifyGroupStreamLambda :: Engine.SimplifiableLore lore =>
                             GroupStreamLambda lore
                          -> SubExp -> SubExp -> [Maybe VName]
                          -> Engine.SimpleM lore (GroupStreamLambda (Wise lore))
simplifyGroupStreamLambda lam w max_chunk arrs = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
      bound_here = HS.fromList $ block_size : block_offset :
                   map paramName (acc_params ++ arr_params)
  (body_res', body_bnds') <-
    Engine.enterLoop $
    Engine.bindLParams acc_params $
    Engine.bindArrayLParams (zip arr_params arrs) $
    Engine.bindLoopVar block_size Int32 max_chunk $
    Engine.bindLoopVar block_offset Int32 w $
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
  simplify (FlatThreadSpace dims) =
    FlatThreadSpace <$> (zip gtids <$> mapM Engine.simplify gdims)
    where (gtids, gdims) = unzip dims
  simplify (FlatGroupSpace dims) =
    FlatGroupSpace <$> (zip gtids <$> mapM Engine.simplify gdims)
    where (gtids, gdims) = unzip dims
  simplify (NestedThreadSpace dims) =
    NestedThreadSpace
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
  simplify (ConcatReturns o w pte moffset what) =
    ConcatReturns
    <$> Engine.simplify o
    <*> Engine.simplify w
    <*> Engine.simplify pte
    <*> Engine.simplify moffset
    <*> Engine.simplify what
  simplify (KernelInPlaceReturn what) =
    KernelInPlaceReturn <$> Engine.simplify what

instance Engine.Simplifiable WhichThreads where
  simplify AllThreads =
    pure AllThreads
  simplify (OneThreadPerGroup which) =
    OneThreadPerGroup <$> Engine.simplify which
  simplify (ThreadsPerGroup limit) =
    ThreadsPerGroup <$> mapM Engine.simplify limit
  simplify ThreadsInSpace =
    pure ThreadsInSpace

kernelRules :: (MonadBinder m,
                LocalScope (Lore m) m,
                Lore m ~ Wise Kernels) => RuleBook m
kernelRules = standardRules <>
              RuleBook [ removeInvariantKernelResults ] [distributeKernelResults]

-- | If an 'Iota' is input to a 'SplitArray', just inline the 'Iota'
-- instead.
fuseSplitIota :: (LocalScope (Lore m) m,
                  MonadBinder m, Lore m ~ Wise InKernel) =>
                 TopDownRule m
fuseSplitIota vtable (Let (Pattern [size] chunks) _
                       (Op (SplitArray o w i elems_per_thread arrs)))
  | ([(iota_pe, iota_start)], chunks_and_arrs) <-
      partitionEithers $ zipWith (isIota vtable) chunks arrs = do

      let (chunks', arrs') = unzip chunks_and_arrs

      case chunks' of
        [] -> -- Can't create a binding that doesn't produce any values.
          letBind_ (Pattern [] [size]) $
          Op $ SplitSpace o w i elems_per_thread
        _ ->
          letBind_ (Pattern [size] chunks') $
          Op $ SplitArray o w i elems_per_thread arrs'

      case o of
        SplitContiguous -> do
          start_offset <- letSubExp "iota_start_offset" $
            BasicOp $ BinOp (Mul Int32) i elems_per_thread
          start <- letSubExp "iota_start" $
            BasicOp $ BinOp (Add Int32) start_offset iota_start
          letBindNames'_ [patElemName iota_pe] $
            BasicOp $ Iota (Var $ patElemName size) start (constant (1::Int32)) Int32
        SplitStrided stride -> do
          start <- letSubExp "iota_start" $
            BasicOp $ BinOp (Add Int32) i iota_start
          letBindNames'_ [patElemName iota_pe] $
            BasicOp $ Iota (Var $ patElemName size) start stride Int32
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

      body' <- insertStmsM $ do
        start <- letSubExp "iota_start" $
            BasicOp $ BinOp (Add Int32) (Var offset) iota_start
        letBindNames'_ [paramName iota_param] $
          BasicOp $ Iota (Var chunk_size) start (constant (1::Int32)) Int32
        return $ groupStreamLambdaBody lam
      let lam' = lam { groupStreamArrParams = arr_params',
                       groupStreamLambdaBody = body'
                     }
      letBind_ pat $ Op $ GroupStream w max_chunk lam' accs arrs'
fuseStreamIota _ _ = cannotSimplify

isIota :: ST.SymbolTable lore -> a -> VName -> Either (a, SubExp) (a, VName)
isIota vtable chunk arr
  | Just (BasicOp (Iota _ x (Constant s) Int32)) <- ST.lookupExp arr vtable,
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
                                      (Op (Kernel desc cs space ts (KernelBody _ kstms kres)))) = do
  (ts', kpes', kres') <-
    unzip3 <$> filterM checkForInvarianceResult (zip3 ts kpes kres)

  -- Check if we did anything at all.
  when (kres == kres')
    cannotSimplify

  addStm $ Let (Pattern [] kpes') attr $ Op $ Kernel desc cs space ts' $
    mkWiseKernelBody () kstms kres'
  where isInvariant Constant{} = True
        isInvariant (Var v) = isJust $ ST.lookup v vtable

        num_threads = spaceNumThreads space
        space_dims = map snd $ spaceDimensions space

        checkForInvarianceResult (_, pe, ThreadsReturn threads se)
          | isInvariant se =
              case threads of
                AllThreads -> do
                  letBindNames'_ [patElemName pe] $ BasicOp $
                    Replicate (Shape [num_threads]) se
                  return False
                ThreadsInSpace -> do
                  let rep a d = BasicOp . Replicate (Shape [d]) <$> letSubExp "rep" a
                  letBindNames'_ [patElemName pe] =<<
                    foldM rep (BasicOp (SubExp se)) (reverse space_dims)
                  return False
                _ -> return True
        checkForInvarianceResult _ =
          return True
removeInvariantKernelResults _ _ = cannotSimplify

-- Some kernel results can be moved outside the kernel, which can
-- simplify further analysis.
distributeKernelResults :: (LocalScope (Lore m) m, MonadBinder m,
                            Lore m ~ Wise Kernels) =>
                           BottomUpRule m
distributeKernelResults (vtable, used)
  (Let (Pattern [] kpes) attr
    (Op (Kernel desc kcs kspace kts (KernelBody _ kstms kres)))) = do
  -- Iterate through the bindings.  For each, we check whether it is
  -- in kres and can be moved outside.  If so, we remove it from kres
  -- and kpes and make it a binding outside.
  (kpes', kts', kres', kstms_rev) <- localScope (scopeOfKernelSpace kspace) $
    foldM distribute (kpes, kts, kres, []) kstms

  when (kpes' == kpes)
    cannotSimplify

  addStm $ Let (Pattern [] kpes') attr $
    Op $ Kernel desc kcs kspace kts' $ mkWiseKernelBody () (reverse kstms_rev) kres'
  where
    free_in_kstms = mconcat $ map freeInStm kstms

    distribute (kpes', kts', kres', kstms_rev) bnd
      | Let (Pattern [] [pe]) _ (BasicOp (Index cs arr slice)) <- bnd,
        kspace_slice <- map (DimFix . Var . fst) $ spaceDimensions kspace,
        kspace_slice `isPrefixOf` slice,
        remaining_slice <- drop (length kspace_slice) slice,
        all (isJust . flip ST.lookup vtable) $ HS.toList $ freeIn remaining_slice,
        Just (kpe, kpes'', kts'', kres'') <- isResult kpes' kts' kres' pe = do
          let outer_slice = map (\(_, d) -> DimSlice
                                            (constant (0::Int32))
                                            d
                                            (constant (1::Int32))) $
                            spaceDimensions kspace
              index kpe' = letBind_ (Pattern [] [kpe']) $ BasicOp $ Index (kcs<>cs) arr $
                           outer_slice <> remaining_slice
          if patElemName kpe `UT.isConsumed` used
            then do precopy <- newVName $ baseString (patElemName kpe) <> "_precopy"
                    index kpe { patElemName = precopy }
                    letBind_ (Pattern [] [kpe]) $ BasicOp $ Copy precopy
            else index kpe
          return (kpes'', kts'', kres'',
                  if patElemName pe `HS.member` free_in_kstms
                  then bnd : kstms_rev
                  else kstms_rev)

    distribute (kpes', kts', kres', kstms_rev) bnd =
      return (kpes', kts', kres', bnd : kstms_rev)

    isResult kpes' kts' kres' pe =
      case partition matches $ zip3 kpes' kts' kres' of
        ([(kpe,_,_)], kpes_and_kres)
          | (kpes'', kts'', kres'') <- unzip3 kpes_and_kres ->
              Just (kpe, kpes'', kts'', kres'')
        _ -> Nothing
      where matches (_, _, kre) = kre == ThreadsReturn ThreadsInSpace (Var $ patElemName pe)
distributeKernelResults _ _ = cannotSimplify

-- | Turn SplitSpace into direct code.  SplitSpace is slated for
-- destruction in the future.
inlineSplitSpace :: (LocalScope (Lore m) m,
                     MonadBinder m, Lore m ~ Wise InKernel) =>
                    TopDownRule m

inlineSplitSpace _ (Let pat _
                    (Op (SplitSpace (SplitStrided stride) w i elems_per_i))) = do
  remaining_elements <- letSubExp "remaining_elements" <=< toExp $
    (primExpFromSubExp int32 w - primExpFromSubExp int32 i)
    `quotRoundingUp`
    primExpFromSubExp int32 stride
  cond <- letSubExp "full" $ BasicOp $ CmpOp (CmpSlt Int32)
          elems_per_i remaining_elements
  t_branch <- mkBodyM [] [elems_per_i]
  f_branch <- mkBodyM [] [remaining_elements]
  letBind_ pat $ If cond t_branch f_branch [Prim int32]

inlineSplitSpace _ (Let pat _
                    (Op (SplitSpace SplitContiguous w i elems_per_i))) = do
  let w' = primExpFromSubExp int32 w
      i' = primExpFromSubExp int32 i
      elems_per_i' = primExpFromSubExp int32 elems_per_i

  starting_point <- letSubExp "starting_point" <=< toExp $
    i' * elems_per_i'
  let starting_point' = primExpFromSubExp int32 starting_point

  remaining_elements <- letSubExp "remaining_elements" <=< toExp $
    w' - primExpFromSubExp int32 starting_point
  let remaining_elements' = primExpFromSubExp int32 remaining_elements

  let no_remaining_elements = CmpOpExp (CmpSle Int32) remaining_elements' $
                              primExpFromSubExp int32 $ intConst Int32 0
      beyond_bounds = CmpOpExp (CmpSle Int32) w' starting_point'

  is_empty <- letSubExp "is_empty" <=< toExp $
    BinOpExp LogOr no_remaining_elements beyond_bounds

  empty_body <- mkBodyM [] [intConst Int32 0]
  nonempty_body <- insertStmsM $ do
    is_last_thread <- letSubExp "is_last_thread" <=< toExp $
      CmpOpExp (CmpSlt Int32) w' ((i' + 1) * elems_per_i')
    last_thread_body <- insertStmsM $ do
      last_thread_elements <- letSubExp "last_thread_elements" <=< toExp $
        w' - i' * elems_per_i'
      resultBodyM [last_thread_elements]
    not_last_thread_body <- mkBodyM [] [elems_per_i]
    res <- letSubExp "nonempty_res" $
           If is_last_thread last_thread_body not_last_thread_body [Prim int32]
    resultBodyM [res]

  letBind_ pat $ If is_empty empty_body nonempty_body [Prim int32]

inlineSplitSpace _ _ = cannotSimplify

inKernelRules :: (MonadBinder m,
                  LocalScope (Lore m) m,
                  Lore m ~ Wise InKernel) => RuleBook m
inKernelRules = standardRules <>
                RuleBook [fuseSplitIota, fuseStreamIota, inlineSplitSpace] []
