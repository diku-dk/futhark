{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Representation.Kernels.Simplify
       ( simplifyKernels
       , simplifyLambda

       -- * Building blocks
       , simplifyKernelOp
       , simplifyKernelExp
       )
where

import Control.Monad
import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set      as S

import Futhark.Representation.Kernels
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rules
import Futhark.Optimise.Simplify.Lore
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass
import qualified Futhark.Optimise.Simplify as Simplify
import Futhark.Optimise.Simplify.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Analysis.Rephrase (castStm)

simpleKernels :: Simplify.SimpleOps Kernels
simpleKernels = Simplify.bindableSimpleOps (simplifyKernelOp simpleInKernel inKernelEnv)

simpleInKernel :: KernelSpace -> Simplify.SimpleOps InKernel
simpleInKernel = Simplify.bindableSimpleOps . simplifyKernelExp

simplifyKernels :: Prog Kernels -> PassM (Prog Kernels)
simplifyKernels =
  Simplify.simplifyProg simpleKernels kernelRules Simplify.noExtraHoistBlockers

simplifyLambda :: (HasScope InKernel m, MonadFreshNames m) =>
                  KernelSpace -> Lambda InKernel -> [Maybe VName] -> m (Lambda InKernel)
simplifyLambda kspace =
  Simplify.simplifyLambda (simpleInKernel kspace)
  inKernelRules Engine.noExtraHoistBlockers

simplifyKernelOp :: (Engine.SimplifiableLore lore,
                     Engine.SimplifiableLore outerlore,
                     BodyAttr outerlore ~ (), BodyAttr lore ~ (),
                     ExpAttr lore ~ ExpAttr outerlore,
                     SameScope lore outerlore,
                     RetType lore ~ RetType outerlore,
                     BranchType lore ~ BranchType outerlore) =>
                    (KernelSpace -> Engine.SimpleOps lore) -> Engine.Env lore
                 -> Kernel lore -> Engine.SimpleM outerlore (Kernel (Wise lore), Stms (Wise outerlore))
simplifyKernelOp mk_ops env (Kernel desc space ts kbody) = do
  space' <- Engine.simplify space
  ts' <- mapM Engine.simplify ts
  outer_vtable <- Engine.askVtable
  ((kbody_stms, kbody_res), kbody_hoisted) <-
    Engine.subSimpleM (mk_ops space) env outer_vtable $ do
      par_blocker <- Engine.asksEngineEnv $ Engine.blockHoistPar . Engine.envHoistBlockers
      Engine.localVtable (<>scope_vtable) $
        Engine.blockIf (Engine.hasFree bound_here
                        `Engine.orIf` Engine.isOp
                        `Engine.orIf` par_blocker
                        `Engine.orIf` Engine.isConsumed) $
        simplifyKernelBodyM kbody
  kbody_hoisted' <- mapM processHoistedStm kbody_hoisted
  return (Kernel desc space' ts' $ mkWiseKernelBody () kbody_stms kbody_res,
          kbody_hoisted')
  where scope = scopeOfKernelSpace space
        scope_vtable = ST.fromScope scope
        bound_here = S.fromList $ M.keys scope

simplifyKernelOp mk_ops env (SegRed space comm red_op nes ts body) = do
  space' <- Engine.simplify space
  nes' <- mapM Engine.simplify nes
  ts' <- mapM Engine.simplify ts
  outer_vtable <- Engine.askVtable

  (red_op', red_op_hoisted) <-
    Engine.subSimpleM (mk_ops space) env outer_vtable $
    Engine.localVtable (<>scope_vtable) $
    Engine.simplifyLambda red_op $ replicate (length nes * 2) Nothing
  red_op_hoisted' <- mapM processHoistedStm red_op_hoisted

  (body', body_hoisted) <- hoistFromBody space' (mk_ops space') env ts body

  return (SegRed space' comm red_op' nes' ts' body',
          red_op_hoisted' <> body_hoisted)

  where scope_vtable = ST.fromScope scope
        scope = scopeOfKernelSpace space

simplifyKernelOp mk_ops env (SegGenRed space ops ts body) = do
  outer_vtable <- Engine.askVtable

  space' <- Engine.simplify space
  ts' <- mapM Engine.simplify ts

  (ops', ops_hoisted) <- fmap unzip $ forM ops $
    \(GenReduceOp w arrs nes dims lam) -> do
      w' <- Engine.simplify w
      arrs' <- Engine.simplify arrs
      nes' <- Engine.simplify nes
      dims' <- Engine.simplify dims
      (lam', op_hoisted) <-
        Engine.subSimpleM (mk_ops space) env outer_vtable $
        Engine.localVtable (<>scope_vtable) $
        Engine.simplifyLambda lam $
        replicate (length nes * 2) Nothing
      return (GenReduceOp w' arrs' nes' dims' lam',
              op_hoisted)

  red_op_hoisted' <- mapM processHoistedStm $ mconcat ops_hoisted

  (body', body_hoisted) <- hoistFromBody space' (mk_ops space') env ts body

  return (SegGenRed space' ops' ts' body',
          red_op_hoisted' <> body_hoisted)

  where scope_vtable = ST.fromScope scope
        scope = scopeOfKernelSpace space

simplifyKernelOp mk_ops env (Husk hspace kern red) = do
  hspace' <- Engine.simplify hspace
  scope <- scopeOfHuskSpace hspace'
  let scope_vtable = ST.fromScope scope

  (kern', kern_hoisted) <-
    Engine.localVtable (<>scope_vtable) $
    simplifyKernelOp mk_ops env kern
  -- TODO: ^ Should we actually hoist from kernel?

  (red', red_hoisted) <-
    Engine.localVtable (<>scope_vtable) $
    simplifyKernelOp mk_ops env red
  
  return (Husk hspace' kern' red', kern_hoisted <> red_hoisted)

simplifyKernelOp _ _ (GetSize key size_class) = return (GetSize key size_class, mempty)
simplifyKernelOp _ _ (GetSizeMax size_class) = return (GetSizeMax size_class, mempty)
simplifyKernelOp _ _ (CmpSizeLe key size_class x) = do
  x' <- Engine.simplify x
  return (CmpSizeLe key size_class x', mempty)

hoistFromBody :: (Engine.SimplifiableLore lore,
                  SameScope lore outerlore,
                  BodyAttr outerlore ~ (), BodyAttr lore ~ (),
                  ExpAttr lore ~ ExpAttr outerlore,
                  RetType lore ~ RetType outerlore,
                  BranchType lore ~ BranchType outerlore) =>
                 KernelSpace -> Simplify.SimpleOps lore -> Engine.Env lore
              -> [Type] -> Body lore
              -> Engine.SimpleM outerlore (Body (Wise lore), Stms (Wise outerlore))
hoistFromBody kspace ops env ts body = do
  outer_vtable <- Engine.askVtable

  ((body_stms, body_res), body_hoisted) <-
    Engine.subSimpleM ops env outer_vtable $ do
      par_blocker <- Engine.asksEngineEnv $ Engine.blockHoistPar . Engine.envHoistBlockers
      Engine.localVtable (<>scope_vtable) $
        Engine.blockIf (Engine.hasFree bound_here
                        `Engine.orIf` Engine.isOp
                        `Engine.orIf` par_blocker
                        `Engine.orIf` Engine.isConsumed) $
        Engine.simplifyBody (replicate (length ts) Observe) body

  body_hoisted' <- mapM processHoistedStm body_hoisted

  return (mkWiseBody () body_stms body_res,
          body_hoisted')

  where scope_vtable = ST.fromScope scope
        scope = scopeOfKernelSpace kspace
        bound_here = S.fromList $ M.keys scope

processHoistedStm :: (Monad m,
                      PrettyLore from,
                      ExpAttr from ~ ExpAttr to,
                      BodyAttr from ~ BodyAttr to,
                      RetType from ~ RetType to,
                      BranchType from ~ BranchType to,
                      LetAttr from ~ LetAttr to,
                      FParamAttr from ~ FParamAttr to,
                      LParamAttr from ~ LParamAttr to) =>
                     Stm from -> m (Stm to)
processHoistedStm bnd
  | Just bnd' <- castStm bnd = return bnd'
  | otherwise                = fail $ "Cannot hoist binding: " ++ pretty bnd

mkWiseKernelBody :: (Attributes lore, CanBeWise (Op lore)) =>
                    BodyAttr lore -> Stms (Wise lore) -> [KernelResult] -> KernelBody (Wise lore)
mkWiseKernelBody attr bnds res =
  let Body attr' _ _ = mkWiseBody attr bnds res_vs
  in KernelBody attr' bnds res
  where res_vs = map resValue res
        resValue (ThreadsReturn _ se) = se
        resValue (WriteReturn _ arr _) = Var arr
        resValue (ConcatReturns _ _ _ _ v) = Var v
        resValue (KernelInPlaceReturn v) = Var v

inKernelEnv :: Engine.Env InKernel
inKernelEnv = Engine.emptyEnv inKernelRules Simplify.noExtraHoistBlockers

instance Engine.Simplifiable SplitOrdering where
  simplify SplitContiguous =
    return SplitContiguous
  simplify (SplitStrided stride) =
    SplitStrided <$> Engine.simplify stride

instance Engine.Simplifiable CombineSpace where
  simplify (CombineSpace scatter cspace) =
    CombineSpace <$> mapM Engine.simplify scatter
                 <*> mapM (traverse Engine.simplify) cspace

simplifyKernelExp :: Engine.SimplifiableLore lore =>
                     KernelSpace -> KernelExp lore
                  -> Engine.SimpleM lore (KernelExp (Wise lore), Stms (Wise lore))

simplifyKernelExp _ (Barrier se) =
  (,) <$> (Barrier <$> Engine.simplify se) <*> pure mempty

simplifyKernelExp _ (SplitSpace o w i elems_per_thread) =
  (,) <$> (SplitSpace <$> Engine.simplify o <*> Engine.simplify w
           <*> Engine.simplify i <*> Engine.simplify elems_per_thread)
      <*> pure mempty

simplifyKernelExp kspace (Combine cspace ts active body) = do
  ((body_stms', body_res'), hoisted) <-
    wrapbody $ Engine.blockIf (Engine.hasFree bound_here `Engine.orIf`
                               maybeBlockUnsafe) $
    localScope (scopeOfCombineSpace cspace) $
    Engine.simplifyBody (map (const Observe) ts) body
  body' <- Engine.constructBody body_stms' body_res'
  (,) <$> (Combine <$> Engine.simplify cspace
           <*> mapM Engine.simplify ts
           <*> mapM Engine.simplify active
           <*> pure body') <*> pure hoisted
  where bound_here = S.fromList $ M.keys $ scopeOfCombineSpace cspace

        protectCombineHoisted checkIfActive m = do
          (x, stms) <- m
          runBinder $ do
            if any (not . safeExp . stmExp) stms
              then do is_active <- checkIfActive
                      mapM_ (Engine.protectIf (not . safeExp) is_active) stms
              else addStms stms
            return x

        (maybeBlockUnsafe, wrapbody)
          | [d] <- map snd $ cspaceDims cspace,
            d == spaceGroupSize kspace =
            (Engine.isFalse True,
             protectCombineHoisted $
              letSubExp "active" =<<
              foldBinOp LogAnd (constant True) =<<
              mapM (uncurry check) active)
          | otherwise =
              (Engine.isNotSafe, id)

        check v se =
          letSubExp "is_active" $ BasicOp $ CmpOp (CmpSlt Int32) (Var v) se

simplifyKernelExp _ (GroupReduce w lam input) = do
  arrs' <- mapM Engine.simplify arrs
  nes' <- mapM Engine.simplify nes
  w' <- Engine.simplify w
  (lam', hoisted) <- Engine.simplifyLambdaSeq lam (map (const Nothing) arrs')
  return (GroupReduce w' lam' $ zip nes' arrs', hoisted)
  where (nes,arrs) = unzip input

simplifyKernelExp _ (GroupScan w lam input) = do
  w' <- Engine.simplify w
  nes' <- mapM Engine.simplify nes
  arrs' <- mapM Engine.simplify arrs
  (lam', hoisted) <- Engine.simplifyLambdaSeq lam (map (const Nothing) arrs')
  return (GroupScan w' lam' $ zip nes' arrs', hoisted)
  where (nes,arrs) = unzip input

simplifyKernelExp _ (GroupGenReduce w dests op bucket vs locks) = do
  w' <- Engine.simplify w
  dests' <- mapM Engine.simplify dests
  (op', hoisted) <- Engine.simplifyLambdaSeq op (map (const Nothing) vs)
  bucket' <- Engine.simplify bucket
  vs' <- mapM Engine.simplify vs
  locks' <- Engine.simplify locks
  return (GroupGenReduce w' dests' op' bucket' vs' locks', hoisted)

simplifyKernelExp _ (GroupStream w maxchunk lam accs arrs) = do
  w' <- Engine.simplify w
  maxchunk' <- Engine.simplify maxchunk
  accs' <- mapM Engine.simplify accs
  arrs' <- mapM Engine.simplify arrs
  (lam', hoisted) <- simplifyGroupStreamLambda lam w' maxchunk' arrs'
  return (GroupStream w' maxchunk' lam' accs' arrs', hoisted)

simplifyKernelBodyM :: Engine.SimplifiableLore lore =>
                       KernelBody lore
                    -> Engine.SimpleM lore (Engine.SimplifiedBody lore [KernelResult])
simplifyKernelBodyM (KernelBody _ stms res) =
  Engine.simplifyStms stms $ do res' <- mapM Engine.simplify res
                                return ((res', UT.usages $ freeIn res'), mempty)

simplifyGroupStreamLambda :: Engine.SimplifiableLore lore =>
                             GroupStreamLambda lore
                          -> SubExp -> SubExp -> [VName]
                          -> Engine.SimpleM lore (GroupStreamLambda (Wise lore), Stms (Wise lore))
simplifyGroupStreamLambda lam w max_chunk arrs = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
      bound_here = S.fromList $ block_size : block_offset :
                   map paramName (acc_params ++ arr_params)
  ((body_stms', body_res'), hoisted) <-
    Engine.enterLoop $
    Engine.bindLoopVar block_size Int32 max_chunk $
    Engine.bindLoopVar block_offset Int32 w $
    Engine.bindLParams acc_params $
    Engine.bindChunkLParams block_offset (zip arr_params arrs) $
    Engine.blockIf (Engine.hasFree bound_here `Engine.orIf` Engine.isConsumed) $
    Engine.simplifyBody (replicate (length (bodyResult body)) Observe) body
  acc_params' <- mapM (Engine.simplifyParam Engine.simplify) acc_params
  arr_params' <- mapM (Engine.simplifyParam Engine.simplify) arr_params
  body' <- Engine.constructBody body_stms' body_res'
  return (GroupStreamLambda block_size block_offset acc_params' arr_params' body', hoisted)

instance Engine.Simplifiable KernelSpace where
  simplify (KernelSpace gtid ltid gid num_threads num_groups group_size structure) =
    KernelSpace gtid ltid gid
    <$> Engine.simplify num_threads
    <*> Engine.simplify num_groups
    <*> Engine.simplify group_size
    <*> Engine.simplify structure

instance Engine.Simplifiable HuskSpace where
  simplify (HuskSpace part_src src interm_res interm_ts num_nodes part_size) =
    HuskSpace part_src src interm_res interm_ts
    <$> Engine.simplify num_nodes
    <*> Engine.simplify part_size

instance Engine.Simplifiable SpaceStructure where
  simplify (FlatThreadSpace dims) =
    FlatThreadSpace <$> (zip gtids <$> mapM Engine.simplify gdims)
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
  simplify (WriteReturn ws a res) =
    WriteReturn <$> Engine.simplify ws <*> Engine.simplify a <*> Engine.simplify res
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
  simplify AllThreads = pure AllThreads
  simplify OneResultPerGroup = pure OneResultPerGroup
  simplify ThreadsInSpace = pure ThreadsInSpace
  simplify (ThreadsPerGroup limit) =
    ThreadsPerGroup <$> mapM Engine.simplify limit

instance BinderOps (Wise Kernels) where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance BinderOps (Wise InKernel) where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

kernelRules :: RuleBook (Wise Kernels)
kernelRules = standardRules <>
              ruleBook [RuleOp removeInvariantKernelResults]
                       [RuleOp distributeKernelResults,
                        RuleBasicOp removeUnnecessaryCopy]

fuseStreamIota :: TopDownRuleOp (Wise InKernel)
fuseStreamIota vtable pat _ (GroupStream w max_chunk lam accs arrs)
  | ([(iota_cs, iota_param, iota_start, iota_stride, iota_t)], params_and_arrs) <-
      partitionEithers $ zipWith (isIota vtable) (groupStreamArrParams lam) arrs = do

      let (arr_params', arrs') = unzip params_and_arrs
          chunk_size = groupStreamChunkSize lam
          offset = groupStreamChunkOffset lam

      body' <- insertStmsM $ inScopeOf lam $ certifying iota_cs $ do
        -- Convert index to appropriate type.
        offset' <- asIntS iota_t $ Var offset
        offset'' <- letSubExp "offset_by_stride" $
          BasicOp $ BinOp (Mul iota_t) offset' iota_stride
        start <- letSubExp "iota_start" $
            BasicOp $ BinOp (Add iota_t) offset'' iota_start
        letBindNames_ [paramName iota_param] $
          BasicOp $ Iota (Var chunk_size) start iota_stride iota_t
        return $ groupStreamLambdaBody lam
      let lam' = lam { groupStreamArrParams = arr_params',
                       groupStreamLambdaBody = body'
                     }
      letBind_ pat $ Op $ GroupStream w max_chunk lam' accs arrs'
fuseStreamIota _ _ _ _ = cannotSimplify

isIota :: ST.SymbolTable lore -> a -> VName
       -> Either (Certificates, a, SubExp, SubExp, IntType) (a, VName)
isIota vtable chunk arr
  | Just (BasicOp (Iota _ x s it), cs) <- ST.lookupExp arr vtable =
      Left (cs, chunk, x, s, it)
  | otherwise =
      Right (chunk, arr)

-- If a kernel produces something invariant to the kernel, turn it
-- into a replicate.
removeInvariantKernelResults :: TopDownRuleOp (Wise Kernels)
removeInvariantKernelResults vtable (Pattern [] kpes) attr
                                    (Kernel desc space ts (KernelBody _ kstms kres)) = do
  (ts', kpes', kres') <-
    unzip3 <$> filterM checkForInvarianceResult (zip3 ts kpes kres)

  -- Check if we did anything at all.
  when (kres == kres')
    cannotSimplify

  addStm $ Let (Pattern [] kpes') attr $ Op $ Kernel desc space ts' $
    mkWiseKernelBody () kstms kres'
  where isInvariant Constant{} = True
        isInvariant (Var v) = isJust $ ST.lookup v vtable

        num_threads = spaceNumThreads space
        space_dims = map snd $ spaceDimensions space

        checkForInvarianceResult (_, pe, ThreadsReturn threads se)
          | isInvariant se =
              case threads of
                AllThreads -> do
                  letBindNames_ [patElemName pe] $ BasicOp $
                    Replicate (Shape [num_threads]) se
                  return False
                ThreadsInSpace -> do
                  let rep a d = BasicOp . Replicate (Shape [d]) <$> letSubExp "rep" a
                  letBindNames_ [patElemName pe] =<<
                    foldM rep (BasicOp (SubExp se)) (reverse space_dims)
                  return False
                _ -> return True
        checkForInvarianceResult _ =
          return True
removeInvariantKernelResults _ _ _ _ = cannotSimplify

-- Some kernel results can be moved outside the kernel, which can
-- simplify further analysis.
distributeKernelResults :: BottomUpRuleOp (Wise Kernels)
distributeKernelResults (vtable, used)
  (Pattern [] kpes) attr (Kernel desc kspace kts (KernelBody _ kstms kres)) = do
  -- Iterate through the bindings.  For each, we check whether it is
  -- in kres and can be moved outside.  If so, we remove it from kres
  -- and kpes and make it a binding outside.
  (kpes', kts', kres', kstms_rev) <- localScope (scopeOfKernelSpace kspace) $
    foldM distribute (kpes, kts, kres, []) kstms

  when (kpes' == kpes)
    cannotSimplify

  addStm $ Let (Pattern [] kpes') attr $
    Op $ Kernel desc kspace kts' $ mkWiseKernelBody () (stmsFromList $ reverse kstms_rev) kres'
  where
    free_in_kstms = fold $ fmap freeInStm kstms

    distribute (kpes', kts', kres', kstms_rev) bnd
      | Let (Pattern [] [pe]) _ (BasicOp (Index arr slice)) <- bnd,
        kspace_slice <- map (DimFix . Var . fst) $ spaceDimensions kspace,
        kspace_slice `isPrefixOf` slice,
        remaining_slice <- drop (length kspace_slice) slice,
        all (isJust . flip ST.lookup vtable) $ S.toList $
          freeIn arr <> freeIn remaining_slice,
        Just (kpe, kpes'', kts'', kres'') <- isResult kpes' kts' kres' pe = do
          let outer_slice = map (\(_, d) -> DimSlice
                                            (constant (0::Int32))
                                            d
                                            (constant (1::Int32))) $
                            spaceDimensions kspace
              index kpe' = letBind_ (Pattern [] [kpe']) $ BasicOp $ Index arr $
                           outer_slice <> remaining_slice
          if patElemName kpe `UT.isConsumed` used
            then do precopy <- newVName $ baseString (patElemName kpe) <> "_precopy"
                    index kpe { patElemName = precopy }
                    letBind_ (Pattern [] [kpe]) $ BasicOp $ Copy precopy
            else index kpe
          return (kpes'', kts'', kres'',
                  if patElemName pe `S.member` free_in_kstms
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
distributeKernelResults _ _ _ _ = cannotSimplify

simplifyKnownIterationStream :: TopDownRuleOp (Wise InKernel)
-- Remove GroupStreams over single-element arrays.  Not much to stream
-- here, and no information to exploit.
simplifyKnownIterationStream _ pat _ (GroupStream (Constant v) _ lam accs arrs)
  | oneIsh v = do
      let GroupStreamLambda chunk_size chunk_offset acc_params arr_params body = lam

      letBindNames_ [chunk_size] $ BasicOp $ SubExp $ constant (1::Int32)

      letBindNames_ [chunk_offset] $ BasicOp $ SubExp $ constant (0::Int32)

      forM_ (zip acc_params accs) $ \(p,a) ->
        letBindNames_ [paramName p] $ BasicOp $ SubExp a

      forM_ (zip arr_params arrs) $ \(p,a) ->
        letBindNames_ [paramName p] $ BasicOp $ Index a $
        fullSlice (paramType p)
        [DimSlice (Var chunk_offset) (Var chunk_size) (constant (1::Int32))]

      res <- bodyBind body
      forM_ (zip (patternElements pat) res) $ \(pe,r) ->
        letBindNames_ [patElemName pe] $ BasicOp $ SubExp r
simplifyKnownIterationStream _ _ _ _ = cannotSimplify

removeUnusedStreamInputs :: TopDownRuleOp (Wise InKernel)
removeUnusedStreamInputs _ pat _ (GroupStream w maxchunk lam accs arrs)
  | (used,unused) <- partition (isUsed . paramName . fst) $ zip arr_params arrs,
    not $ null unused = do
      let (arr_params', arrs') = unzip used
          lam' = GroupStreamLambda chunk_size chunk_offset acc_params arr_params' body
      letBind_ pat $ Op $ GroupStream w maxchunk lam' accs arrs'
  where GroupStreamLambda chunk_size chunk_offset acc_params arr_params body = lam

        isUsed = (`S.member` freeInBody body)
removeUnusedStreamInputs _ _ _ _ = cannotSimplify

inKernelRules :: RuleBook (Wise InKernel)
inKernelRules = standardRules <>
                ruleBook [RuleOp fuseStreamIota,
                          RuleOp simplifyKnownIterationStream,
                          RuleOp removeUnusedStreamInputs] []
