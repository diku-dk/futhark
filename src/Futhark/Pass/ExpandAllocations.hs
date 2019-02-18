{-# LANGUAGE TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
-- | Expand allocations inside of maps when possible.
module Futhark.Pass.ExpandAllocations
       ( expandAllocations )
where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.List

import Prelude hiding (quot)

import Futhark.Analysis.Rephrase
import Futhark.Error
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Pass
import Futhark.Representation.AST
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.Simplify as ExplicitMemory
import qualified Futhark.Representation.Kernels as Kernels
import Futhark.Representation.Kernels.Simplify as Kernels
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Pass.ExtractKernels.BlockedKernel (blockedReduction)
import Futhark.Pass.ExplicitAllocations (explicitAllocationsInStms)
import Futhark.Util.IntegralExp
import Futhark.Util (mapAccumLM)


expandAllocations :: Pass ExplicitMemory ExplicitMemory
expandAllocations =
  Pass "expand allocations" "Expand allocations" $
  fmap Prog . mapM transformFunDef . progFunctions
  -- Cannot use intraproceduralTransformation because it might create
  -- duplicate size keys (which are not fixed by renamer, and size
  -- keys must currently be globally unique).

type ExpandM = ExceptT InternalError (ReaderT (Scope ExplicitMemory) (State VNameSource))

transformFunDef :: FunDef ExplicitMemory -> PassM (FunDef ExplicitMemory)
transformFunDef fundec = do
  body' <- either throwError return <=< modifyNameSource $
           runState $ runReaderT (runExceptT m) mempty
  return fundec { funDefBody = body' }
  where m = inScopeOf fundec $ transformBody $ funDefBody fundec

transformBody :: Body ExplicitMemory -> ExpandM (Body ExplicitMemory)
transformBody (Body () stms res) = Body () <$> transformStms stms <*> pure res

transformStms :: Stms ExplicitMemory -> ExpandM (Stms ExplicitMemory)
transformStms stms =
  inScopeOf stms $ mconcat <$> mapM transformStm (stmsToList stms)

transformStm :: Stm ExplicitMemory -> ExpandM (Stms ExplicitMemory)

transformStm (Let pat aux e) = do
  (bnds, e') <- transformExp =<< mapExpM transform e
  return $ bnds <> oneStm (Let pat aux e')
  where transform = identityMapper { mapOnBody = \scope -> localScope scope . transformBody
                                   }

nameInfoConv :: NameInfo ExplicitMemory -> NameInfo InKernel
nameInfoConv (LetInfo mem_info) = LetInfo mem_info
nameInfoConv (FParamInfo mem_info) = FParamInfo mem_info
nameInfoConv (LParamInfo mem_info) = LParamInfo mem_info
nameInfoConv (IndexInfo it) = IndexInfo it

transformExp :: Exp ExplicitMemory -> ExpandM (Stms ExplicitMemory, Exp ExplicitMemory)

transformExp (Op (Inner (Kernel desc kspace ts kbody))) = do
  let (kbody', allocs) = extractKernelBodyAllocations kbody
      variantAlloc (Var v) = v `S.member` bound_in_kernel
      variantAlloc _ = False
      (variant_allocs, invariant_allocs) = M.partition (variantAlloc . fst) allocs

  (alloc_stms, alloc_offsets) <-
    memoryRequirements kspace (kernelBodyStms kbody) variant_allocs invariant_allocs

  scope <- askScope
  let scope' = scopeOfKernelSpace kspace <> M.map nameInfoConv scope
  kbody'' <- either compilerLimitationS pure $
             runOffsetM scope' alloc_offsets $ offsetMemoryInKernelBody kbody'

  return (alloc_stms,
          Op $ Inner $ Kernel desc kspace ts kbody'')

  where bound_in_kernel =
          S.fromList $ M.keys $ scopeOfKernelSpace kspace <>
          scopeOf (kernelBodyStms kbody)

transformExp (Op (Inner (SegRed kspace comm red_op nes ts kbody))) = do
  let (kbody', kbody_allocs) = extractBodyAllocations kbody
      (red_op', red_op_allocs) = extractLambdaAllocations red_op
      variantAlloc (Var v) = v `S.member` bound_in_kernel
      variantAlloc _ = False
      allocs = kbody_allocs <> red_op_allocs
      (variant_allocs, invariant_allocs) = M.partition (variantAlloc . fst) allocs

  (alloc_stms, alloc_offsets) <-
    memoryRequirements kspace (bodyStms kbody) variant_allocs invariant_allocs

  scope <- askScope
  let scope' = scopeOfKernelSpace kspace <> M.map nameInfoConv scope
  either compilerLimitationS pure $ runOffsetM scope' alloc_offsets $ do
    kbody'' <- offsetMemoryInBody kbody'
    red_op'' <- localScope (scopeOf red_op') $ offsetMemoryInLambda red_op'

    return (alloc_stms,
            Op $ Inner $ SegRed kspace comm red_op'' nes ts kbody'')

  where bound_in_kernel =
          S.fromList $ map fst (spaceDimensions kspace) ++
          M.keys (scopeOfKernelSpace kspace <>
                  scopeOf (bodyStms kbody))

transformExp e =
  return (mempty, e)

memoryRequirements :: KernelSpace
                   -> Stms InKernel
                   -> M.Map VName (SubExp, Space)
                   -> M.Map VName (SubExp, Space)
                   -> ExpandM (Stms ExplicitMemory, RebaseMap)
memoryRequirements kspace kstms variant_allocs invariant_allocs = do
  num_threads64 <- newVName "num_threads64"
  let num_threads64_pat = Pattern [] [PatElem num_threads64 $ MemPrim int64]
      num_threads64_bnd = Let num_threads64_pat (defAux ()) $ BasicOp $
                          ConvOp (SExt Int32 Int64) (spaceNumThreads kspace)

  (invariant_alloc_stms, invariant_alloc_offsets) <-
    expandedInvariantAllocations
    (Var num_threads64, spaceNumGroups kspace, spaceGroupSize kspace)
    (spaceGlobalId kspace, spaceGroupId kspace, spaceLocalId kspace) invariant_allocs

  (variant_alloc_stms, variant_alloc_offsets) <-
    expandedVariantAllocations kspace kstms variant_allocs

  let alloc_offsets = invariant_alloc_offsets <> variant_alloc_offsets
      alloc_stms = invariant_alloc_stms <> variant_alloc_stms

  return (oneStm num_threads64_bnd <> alloc_stms, alloc_offsets)

-- | Extract allocations from 'Thread' statements with
-- 'extractThreadAllocations'.
extractKernelBodyAllocations :: KernelBody InKernel
                             -> (KernelBody InKernel,
                                 M.Map VName (SubExp, Space))
extractKernelBodyAllocations = extractGenericBodyAllocations kernelBodyStms $
  \stms kbody -> kbody { kernelBodyStms = stms }

extractBodyAllocations :: Body InKernel
                       -> (Body InKernel,
                           M.Map VName (SubExp, Space))
extractBodyAllocations = extractGenericBodyAllocations bodyStms $
  \stms body -> body { bodyStms = stms }

extractLambdaAllocations :: Lambda InKernel
                         -> (Lambda InKernel,
                             M.Map VName (SubExp, Space))
extractLambdaAllocations lam = (lam { lambdaBody = body' }, allocs)
  where (body', allocs) = extractGenericBodyAllocations bodyStms
                          (\stms body -> body { bodyStms = stms }) $ lambdaBody lam

extractGenericBodyAllocations :: (body -> Stms InKernel)
                              -> (Stms InKernel -> body -> body)
                              -> body
                              -> (body,
                                  M.Map VName (SubExp, Space))
extractGenericBodyAllocations get_stms set_stms body =
  let (allocs, stms) = mapAccumL extract M.empty $ stmsToList $ get_stms body
  in (set_stms (mconcat stms) body, allocs)
  where extract allocs bnd =
          let (bnds, body_allocs) = extractThreadAllocations $ oneStm bnd
          in (allocs <> body_allocs, bnds)

extractThreadAllocations :: Stms InKernel
                         -> (Stms InKernel, M.Map VName (SubExp, Space))
extractThreadAllocations bnds =
  let (allocs, bnds') = mapAccumL isAlloc M.empty $ stmsToList bnds
  in (stmsFromList $ catMaybes bnds', allocs)
  where isAlloc allocs (Let (Pattern [] [patElem]) _ (Op (Alloc size space))) =
          (M.insert (patElemName patElem) (size, space) allocs,
           Nothing)

        isAlloc allocs bnd =
          (allocs, Just bnd)

expandedInvariantAllocations :: (SubExp,SubExp, SubExp)
                             -> (VName, VName, VName)
                             -> M.Map VName (SubExp, Space)
                             -> ExpandM (Stms ExplicitMemory, RebaseMap)
expandedInvariantAllocations (num_threads64, num_groups, group_size)
                             (_thread_index, group_id, local_id)
                             invariant_allocs = do
  -- We expand the invariant allocations by adding an inner dimension
  -- equal to the number of kernel threads.
  (alloc_bnds, rebases) <- unzip <$> mapM expand (M.toList invariant_allocs)

  return (mconcat alloc_bnds, mconcat rebases)
  where expand (mem, (per_thread_size, Space "local")) = do
          let allocpat = Pattern [] [PatElem mem $
                                     MemMem per_thread_size $ Space "local"]
          return (oneStm $ Let allocpat (defAux ()) $
                   Op $ Alloc per_thread_size $ Space "local",
                  mempty)

        expand (mem, (per_thread_size, space)) = do
          total_size <- newVName "total_size"
          let sizepat = Pattern [] [PatElem total_size $ MemPrim int64]
              allocpat = Pattern [] [PatElem mem $
                                     MemMem (Var total_size) space]
          return (stmsFromList
                  [Let sizepat (defAux ()) $
                    BasicOp $ BinOp (Mul Int64) num_threads64 per_thread_size,
                   Let allocpat (defAux ()) $
                    Op $ Alloc (Var total_size) space],
                  M.singleton mem newBase)

        newBase (old_shape, _) =
          let num_dims = length old_shape
              perm = [0, num_dims+1] ++ [1..num_dims]
              root_ixfun = IxFun.iota (primExpFromSubExp int32 num_groups : old_shape
                                       ++ [primExpFromSubExp int32 group_size])
              permuted_ixfun = IxFun.permute root_ixfun perm
              untouched d = DimSlice (fromInt32 0) d (fromInt32 1)
              offset_ixfun = IxFun.slice permuted_ixfun $
                             [DimFix (LeafExp group_id int32),
                              DimFix (LeafExp local_id int32)] ++
                             map untouched old_shape
          in offset_ixfun

expandedVariantAllocations :: KernelSpace -> Stms InKernel
                           -> M.Map VName (SubExp, Space)
                           -> ExpandM (Stms ExplicitMemory, RebaseMap)
expandedVariantAllocations _ _ variant_allocs
  | null variant_allocs = return (mempty, mempty)
expandedVariantAllocations kspace kstms variant_allocs = do
  let sizes_to_blocks = removeCommonSizes variant_allocs
      variant_sizes = map fst sizes_to_blocks

  (slice_stms, offsets, size_sums) <-
    sliceKernelSizes variant_sizes kspace kstms
  -- Note the recursive call to expand allocations inside the newly
  -- produced kernels.
  slice_stms_tmp <- ExplicitMemory.simplifyStms =<< explicitAllocationsInStms slice_stms
  slice_stms' <- transformStms slice_stms_tmp

  let variant_allocs' :: [(VName, (SubExp, SubExp, Space))]
      variant_allocs' = concat $ zipWith memInfo (map snd sizes_to_blocks)
                        (zip offsets size_sums)
      memInfo blocks (offset, total_size) =
        [ (mem, (Var offset, Var total_size, space)) | (mem, space) <- blocks ]

  -- We expand the invariant allocations by adding an inner dimension
  -- equal to the sum of the sizes required by different threads.
  (alloc_bnds, rebases) <- unzip <$> mapM expand variant_allocs'

  return (slice_stms' <> stmsFromList alloc_bnds, mconcat rebases)
  where expand (mem, (offset, total_size, space)) = do
          let allocpat = Pattern [] [PatElem mem $
                                     MemMem total_size space]
          return (Let allocpat (defAux ()) $ Op $ Alloc total_size space,
                  M.singleton mem $ newBase offset)

        num_threads = primExpFromSubExp int32 $ spaceNumThreads kspace
        gtid = LeafExp (spaceGlobalId kspace) int32

        -- For the variant allocations, we add an inner dimension,
        -- which is then offset by a thread-specific amount.
        newBase size_per_thread (old_shape, pt) =
          let pt_size = fromInt32 $ primByteSize pt
              elems_per_thread = ConvOpExp (SExt Int64 Int32)
                                 (primExpFromSubExp int64 size_per_thread)
                                 `quot` pt_size
              root_ixfun = IxFun.iota [elems_per_thread, num_threads]
              offset_ixfun = IxFun.slice root_ixfun
                             [DimSlice (fromInt32 0) num_threads (fromInt32 1),
                              DimFix gtid]
              shapechange = if length old_shape == 1
                            then map DimCoercion old_shape
                            else map DimNew old_shape
          in IxFun.reshape offset_ixfun shapechange

-- | A map from memory block names to new index function bases.

type RebaseMap = M.Map VName (([PrimExp VName], PrimType) -> IxFun)

newtype OffsetM a = OffsetM (ReaderT (Scope InKernel)
                             (ReaderT RebaseMap (Either String)) a)
  deriving (Applicative, Functor, Monad,
            HasScope InKernel, LocalScope InKernel,
            MonadError String)

runOffsetM :: Scope InKernel -> RebaseMap -> OffsetM a -> Either String a
runOffsetM scope offsets (OffsetM m) =
  runReaderT (runReaderT m scope) offsets

askRebaseMap :: OffsetM RebaseMap
askRebaseMap = OffsetM $ lift ask

lookupNewBase :: VName -> ([PrimExp VName], PrimType) -> OffsetM (Maybe IxFun)
lookupNewBase name x = do
  offsets <- askRebaseMap
  return $ ($ x) <$> M.lookup name offsets

offsetMemoryInKernelBody :: KernelBody InKernel -> OffsetM (KernelBody InKernel)
offsetMemoryInKernelBody kbody = do
  scope <- askScope
  stms' <- stmsFromList . snd <$>
           mapAccumLM (\scope' -> localScope scope' . offsetMemoryInStm) scope
           (stmsToList $ kernelBodyStms kbody)
  return kbody { kernelBodyStms = stms' }

offsetMemoryInBody :: Body InKernel -> OffsetM (Body InKernel)
offsetMemoryInBody (Body attr stms res) = do
  scope <- askScope
  stms' <- stmsFromList . snd <$>
           mapAccumLM (\scope' -> localScope scope' . offsetMemoryInStm) scope
           (stmsToList stms)
  return $ Body attr stms' res

offsetMemoryInStm :: Stm InKernel -> OffsetM (Scope InKernel, Stm InKernel)
offsetMemoryInStm (Let pat attr e) = do
  pat' <- offsetMemoryInPattern pat
  e' <- localScope (scopeOfPattern pat') $ offsetMemoryInExp e
  scope <- askScope
  -- Try to recompute the index function.  Fall back to creating rebase
  -- operations with the RebaseMap.
  rts <- runReaderT (expReturns e') scope
  let pat'' = Pattern (patternContextElements pat')
              (zipWith pick (patternValueElements pat') rts)
      stm = Let pat'' attr e'
  let scope' = scopeOf stm <> scope
  return (scope', stm)
  where pick :: PatElemT (MemInfo SubExp NoUniqueness MemBind) ->
                ExpReturns -> PatElemT (MemInfo SubExp NoUniqueness MemBind)
        pick (PatElem name (MemArray pt s u _ret))
             (MemArray _ _ _ (Just (ReturnsInBlock m extixfun)))
          | Just ixfun <- instantiateIxFun extixfun =
              PatElem name (MemArray pt s u (ArrayIn m ixfun))
        pick p _ = p

        instantiateIxFun :: ExtIxFun -> Maybe IxFun
        instantiateIxFun = traverse (traverse inst)
          where inst Ext{} = Nothing
                inst (Free x) = return x

offsetMemoryInPattern :: Pattern InKernel -> OffsetM (Pattern InKernel)
offsetMemoryInPattern (Pattern ctx vals) = do
  mapM_ inspectCtx ctx
  Pattern ctx <$> mapM inspectVal vals
  where inspectVal patElem = do
          new_attr <- offsetMemoryInMemBound $ patElemAttr patElem
          return patElem { patElemAttr = new_attr }
        inspectCtx patElem
          | Mem _ space <- patElemType patElem,
            space /= Space "local" =
              throwError $ unwords ["Cannot deal with existential memory block",
                                    pretty (patElemName patElem),
                                    "when expanding inside kernels."]
          | otherwise = return ()

offsetMemoryInParam :: Param (MemBound u) -> OffsetM (Param (MemBound u))
offsetMemoryInParam fparam = do
  fparam' <- offsetMemoryInMemBound $ paramAttr fparam
  return fparam { paramAttr = fparam' }

offsetMemoryInMemBound :: MemBound u -> OffsetM (MemBound u)
offsetMemoryInMemBound summary@(MemArray pt shape u (ArrayIn mem ixfun)) = do
  new_base <- lookupNewBase mem (IxFun.base ixfun, pt)
  return $ fromMaybe summary $ do
    new_base' <- new_base
    return $ MemArray pt shape u $ ArrayIn mem $ IxFun.rebase new_base' ixfun
offsetMemoryInMemBound summary = return summary

offsetMemoryInBodyReturns :: BodyReturns -> OffsetM BodyReturns
offsetMemoryInBodyReturns br@(MemArray pt shape u (ReturnsInBlock mem ixfun))
  | Just ixfun' <- isStaticIxFun ixfun = do
      new_base <- lookupNewBase mem (IxFun.base ixfun', pt)
      return $ fromMaybe br $ do
        new_base' <- new_base
        return $
          MemArray pt shape u $ ReturnsInBlock mem $
          IxFun.rebase (fmap (fmap Free) new_base') ixfun
offsetMemoryInBodyReturns br = return br

offsetMemoryInLambda :: Lambda InKernel -> OffsetM (Lambda InKernel)
offsetMemoryInLambda lam = do
  body <- offsetMemoryInBody $ lambdaBody lam
  return $ lam { lambdaBody = body }

offsetMemoryInExp :: Exp InKernel -> OffsetM (Exp InKernel)
offsetMemoryInExp (DoLoop ctx val form body) = do
  let (ctxparams, ctxinit) = unzip ctx
      (valparams, valinit) = unzip val
  ctxparams' <- mapM offsetMemoryInParam ctxparams
  valparams' <- mapM offsetMemoryInParam valparams
  body' <- localScope (scopeOfFParams ctxparams' <> scopeOfFParams valparams' <> scopeOf form) (offsetMemoryInBody body)
  return $ DoLoop (zip ctxparams' ctxinit) (zip valparams' valinit) form body'
offsetMemoryInExp (Op (Inner (GroupStream w max_chunk lam accs arrs))) = do
  lam_accs <- mapM offsetMemoryInParam $ groupStreamAccParams lam
  lam_arrs <- mapM offsetMemoryInParam $ groupStreamArrParams lam
  let lam' = lam { groupStreamAccParams = lam_accs
                 , groupStreamArrParams = lam_arrs
                 }
  body <- localScope (scopeOf lam') $ offsetMemoryInBody $ groupStreamLambdaBody lam
  let lam'' = lam' { groupStreamLambdaBody = body }
  return $ Op $ Inner $ GroupStream w max_chunk lam'' accs arrs
offsetMemoryInExp (Op (Inner (GroupReduce w lam input))) = do
  body <- localScope (scopeOf lam) $ offsetMemoryInBody $ lambdaBody lam
  let lam' = lam { lambdaBody = body }
  return $ Op $ Inner $ GroupReduce w lam' input
offsetMemoryInExp (Op (Inner (GroupScan w lam input))) = do
  body <- localScope (scopeOf lam) $ offsetMemoryInBody $ lambdaBody lam
  let lam' = lam { lambdaBody = body }
  return $ Op $ Inner $ GroupScan w lam' input
offsetMemoryInExp (Op (Inner (GroupGenReduce w dests lam nes vals locks))) = do
  lam_params <- mapM offsetMemoryInParam $ lambdaParams lam
  let lam' = lam { lambdaParams = lam_params }
  body <- localScope (scopeOf lam') $ offsetMemoryInBody $ lambdaBody lam
  let lam'' = lam' { lambdaBody = body }
  return $ Op $ Inner $ GroupGenReduce w dests lam'' nes vals locks
offsetMemoryInExp (Op (Inner (Combine cspace ts active body))) =
  Op . Inner . Combine cspace ts active <$> offsetMemoryInBody body
offsetMemoryInExp e = mapExpM recurse e
  where recurse = identityMapper
                  { mapOnBody = \bscope -> localScope bscope . offsetMemoryInBody
                  , mapOnBranchType = offsetMemoryInBodyReturns
                  }

---- Slicing allocation sizes out of a kernel.

unAllocInKernelStms :: Stms InKernel
                    -> Either String (Stms Kernels.InKernel)
unAllocInKernelStms = unAllocStms False
  where
    unAllocBody (Body attr stms res) =
      Body attr <$> unAllocStms True stms <*> pure res

    unAllocStms nested =
      fmap (stmsFromList . catMaybes) . mapM (unAllocStm nested) . stmsToList

    unAllocStm nested stm@(Let _ _ (Op Alloc{}))
      | nested = throwError $ "Cannot handle nested allocation: " ++ pretty stm
      | otherwise = return Nothing
    unAllocStm _ (Let pat attr e) =
      Just <$> (Let <$> unAllocPattern pat <*> pure attr <*> mapExpM unAlloc' e)

    unAllocKernelExp (Barrier se) =
      return $ Barrier se
    unAllocKernelExp (SplitSpace o w i elems_per_thread) =
      return $ SplitSpace o w i elems_per_thread
    unAllocKernelExp (Combine cspace ts active body) =
      Combine cspace ts active <$> unAllocBody body
    unAllocKernelExp (GroupReduce w lam input) =
      GroupReduce w <$> unAllocLambda lam <*> pure input
    unAllocKernelExp (GroupScan w lam input) =
      GroupScan w <$> unAllocLambda lam <*> pure input
    unAllocKernelExp (GroupStream w maxchunk lam accs arrs) =
      GroupStream w maxchunk <$> unAllocStreamLambda lam <*> pure accs <*> pure arrs
    unAllocKernelExp (GroupGenReduce w arrs op bucket vals locks) =
      GroupGenReduce w arrs <$> unAllocLambda op <*>
      pure bucket <*> pure vals <*> pure locks

    unAllocStreamLambda (GroupStreamLambda chunk_size chunk_offset
                         acc_params arr_params body) =
      GroupStreamLambda chunk_size chunk_offset
                        (unParams acc_params) (unParams arr_params) <$>
                        unAllocBody body

    unAllocLambda (Lambda params body ret) =
      Lambda (unParams params) <$> unAllocBody body <*> pure ret

    unParams = mapMaybe $ traverse unAttr

    unAllocPattern pat@(Pattern ctx val) =
      Pattern <$> maybe bad return (mapM (rephrasePatElem unAttr) ctx)
              <*> maybe bad return (mapM (rephrasePatElem unAttr) val)
      where bad = Left $ "Cannot handle memory in pattern " ++ pretty pat

    unAllocOp Alloc{} = Left "unhandled Op"
    unAllocOp (Inner op) = unAllocKernelExp op

    unParam p = maybe bad return $ traverse unAttr p
      where bad = Left $ "Cannot handle memory-typed parameter '" ++ pretty p ++ "'"

    unT t = maybe bad return $ unAttr t
      where bad = Left $ "Cannot handle memory type '" ++ pretty t ++ "'"

    unAlloc' :: Mapper InKernel Kernels.InKernel (Either String)
    unAlloc' = Mapper { mapOnBody = const unAllocBody
                      , mapOnRetType = unT
                      , mapOnBranchType = unT
                      , mapOnFParam = unParam
                      , mapOnLParam = unParam
                      , mapOnOp = unAllocOp
                      , mapOnSubExp = Right
                      , mapOnVName = Right
                      , mapOnCertificates = Right
                      }

unAttr :: MemInfo d u ret -> Maybe (TypeBase (ShapeBase d) u)
unAttr (MemPrim pt) = Just $ Prim pt
unAttr (MemArray pt shape u _) = Just $ Array pt shape u
unAttr MemMem{} = Nothing

unAllocScope :: Scope ExplicitMemory -> Scope Kernels.InKernel
unAllocScope = M.mapMaybe unInfo
  where unInfo (LetInfo attr) = LetInfo <$> unAttr attr
        unInfo (FParamInfo attr) = FParamInfo <$> unAttr attr
        unInfo (LParamInfo attr) = LParamInfo <$> unAttr attr
        unInfo (IndexInfo it) = Just $ IndexInfo it

removeCommonSizes :: M.Map VName (SubExp, Space)
                  -> [(SubExp, [(VName, Space)])]
removeCommonSizes = M.toList . foldl' comb mempty . M.toList
  where comb m (mem, (size, space)) = M.insertWith (++) size [(mem, space)] m

sliceKernelSizes :: [SubExp] -> KernelSpace -> Stms InKernel
                 -> ExpandM (Stms Kernels.Kernels, [VName], [VName])
sliceKernelSizes sizes kspace kstms = do
  kstms' <- either compilerLimitationS return $ unAllocInKernelStms kstms
  let num_sizes = length sizes
      i64s = replicate num_sizes $ Prim int64
  inkernels_scope <- asks unAllocScope

  let kernels_scope = castScope inkernels_scope

  (max_lam, _) <- flip runBinderT inkernels_scope $ do
    xs <- replicateM num_sizes $ newParam "x" (Prim int64)
    ys <- replicateM num_sizes $ newParam "y" (Prim int64)
    (zs, stms) <- localScope (scopeOfLParams $ xs ++ ys) $ collectStms $
                  forM (zip xs ys) $ \(x,y) ->
      letSubExp "z" $ BasicOp $ BinOp (SMax Int64) (Var $ paramName x) (Var $ paramName y)
    return $ Lambda (xs ++ ys) (mkBody stms zs) i64s

  (size_lam', _) <- flip runBinderT inkernels_scope $ do
    params <- replicateM num_sizes $ newParam "x" (Prim int64)
    (zs, stms) <- localScope (scopeOfLParams params <>
                              scopeOfKernelSpace kspace) $ collectStms $ do
      mapM_ addStm kstms'
      return sizes
    localScope (scopeOfKernelSpace kspace) $
      Kernels.simplifyLambda kspace -- XXX, is this the right KernelSpace?
      (Lambda mempty (Body () stms zs) i64s) []

  ((maxes_per_thread, size_sums), slice_stms) <- flip runBinderT kernels_scope $ do
    space_size <- letSubExp "space_size" =<<
                  foldBinOp (Mul Int32) (intConst Int32 1)
                  (map snd $ spaceDimensions kspace)
    num_threads_64 <- letSubExp "num_threads" $
                      BasicOp $ ConvOp (SExt Int32 Int64) $ spaceNumThreads kspace

    pat <- basicPattern [] <$> replicateM num_sizes
           (newIdent "max_per_thread" $ Prim int64)

    addStms =<<
      blockedReduction pat space_size Commutative
      max_lam size_lam' (spaceDimensions kspace)
      (replicate num_sizes $ intConst Int64 0) []

    size_sums <- forM (patternNames pat) $ \threads_max ->
      letExp "size_sum" $
      BasicOp $ BinOp (Mul Int64) (Var threads_max) num_threads_64

    return (patternNames pat, size_sums)

  return (slice_stms, maxes_per_thread, size_sums)
