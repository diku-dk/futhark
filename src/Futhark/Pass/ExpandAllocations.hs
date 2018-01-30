{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
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
import Data.Monoid

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
  intraproceduralTransformation transformFunDef

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

transformExp :: Exp ExplicitMemory -> ExpandM (Stms ExplicitMemory, Exp ExplicitMemory)

transformExp (Op (Inner (Kernel desc kspace ts kbody))) = do
  let (kbody', allocs) = extractKernelBodyAllocations kbody
      variantAlloc (Var v) = v `S.member` bound_in_kernel
      variantAlloc _ = False
      (variant_allocs, invariant_allocs) = M.partition (variantAlloc . fst) allocs

  num_threads64 <- newVName "num_threads64"
  let num_threads64_pat = Pattern [] [PatElem num_threads64 $ MemPrim int64]
      num_threads64_bnd = Let num_threads64_pat (defAux ()) $ BasicOp $
                          ConvOp (SExt Int32 Int64) (spaceNumThreads kspace)

  (invariant_alloc_stms, invariant_alloc_offsets) <-
    expandedInvariantAllocations
    (Var num_threads64, spaceNumGroups kspace, spaceGroupSize kspace)
    (spaceGlobalId kspace, spaceGroupId kspace, spaceLocalId kspace) invariant_allocs

  (variant_alloc_stms, variant_alloc_offsets) <-
    expandedVariantAllocations kspace kbody variant_allocs

  let alloc_offsets = invariant_alloc_offsets <> variant_alloc_offsets
      alloc_stms = invariant_alloc_stms <> variant_alloc_stms

  kbody'' <-  either compilerLimitationS pure $
              offsetMemoryInKernelBody alloc_offsets
              kbody' { kernelBodyStms = kernelBodyStms kbody' }

  return (oneStm num_threads64_bnd <> alloc_stms,
          Op $ Inner $ Kernel desc kspace ts kbody'')

  where bound_in_kernel =
          S.fromList $ M.keys $ scopeOfKernelSpace kspace <>
          scopeOf (kernelBodyStms kbody)

transformExp e =
  return (mempty, e)

-- | Extract allocations from 'Thread' statements with
-- 'extractThreadAllocations'.
extractKernelBodyAllocations :: KernelBody InKernel
                             -> (KernelBody InKernel,
                                 M.Map VName (SubExp, Space))
extractKernelBodyAllocations kbody =
  let (allocs, stms) = mapAccumL extract M.empty $ stmsToList $ kernelBodyStms kbody
  in (kbody { kernelBodyStms = mconcat stms }, allocs)
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

expandedVariantAllocations :: KernelSpace -> KernelBody InKernel
                           -> M.Map VName (SubExp, Space)
                           -> ExpandM (Stms ExplicitMemory, RebaseMap)
expandedVariantAllocations _ _ variant_allocs
  | null variant_allocs = return (mempty, mempty)
expandedVariantAllocations kspace kbody variant_allocs = do
  let sizes_to_blocks = removeCommonSizes variant_allocs
      variant_sizes = map fst sizes_to_blocks

  (slice_stms, offsets, size_sums) <-
    sliceKernelSizes variant_sizes kspace kbody
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

lookupNewBase :: VName -> ([PrimExp VName], PrimType) -> RebaseMap -> Maybe IxFun
lookupNewBase name x = fmap ($x) . M.lookup name

offsetMemoryInKernelBody :: RebaseMap -> KernelBody InKernel
                         -> Either String (KernelBody InKernel)
offsetMemoryInKernelBody initial_offsets kbody = do
  stms' <- snd <$> mapAccumLM offsetMemoryInStm initial_offsets
           (stmsToList $ kernelBodyStms kbody)
  return kbody { kernelBodyStms = stmsFromList stms' }

offsetMemoryInBody :: RebaseMap -> Body InKernel -> Either String (Body InKernel)
offsetMemoryInBody offsets (Body attr stms res) = do
  stms' <- stmsFromList . snd <$> mapAccumLM offsetMemoryInStm offsets (stmsToList stms)
  return $ Body attr stms' res

offsetMemoryInStm :: RebaseMap -> Stm InKernel
                  -> Either String (RebaseMap, Stm InKernel)
offsetMemoryInStm offsets (Let pat attr e) = do
  (offsets', pat') <- offsetMemoryInPattern offsets pat
  e' <- offsetMemoryInExp offsets e
  return (offsets', Let pat' attr e')

offsetMemoryInPattern :: RebaseMap -> Pattern InKernel
                      -> Either String (RebaseMap, Pattern InKernel)
offsetMemoryInPattern offsets (Pattern ctx vals) = do
  offsets' <- foldM inspectCtx offsets ctx
  return (offsets', Pattern ctx $ map (inspectVal offsets') vals)
  where inspectVal offsets' = fmap $ offsetMemoryInMemBound offsets'
        inspectCtx ctx_offsets patElem
          | Mem _ space <- patElemType patElem,
            space /= Space "local" =
              throwError $ unwords ["Cannot deal with existential memory block",
                                    pretty (patElemName patElem),
                                    "when expanding inside kernels."]
          | otherwise =
              return ctx_offsets

offsetMemoryInParam :: RebaseMap -> Param (MemBound u) -> Param (MemBound u)
offsetMemoryInParam offsets fparam =
  fparam { paramAttr = offsetMemoryInMemBound offsets $ paramAttr fparam }

offsetMemoryInMemBound :: RebaseMap -> MemBound u -> MemBound u
offsetMemoryInMemBound offsets (MemArray pt shape u (ArrayIn mem ixfun))
  | Just new_base <- lookupNewBase mem (IxFun.base ixfun, pt) offsets =
      MemArray pt shape u $ ArrayIn mem $ IxFun.rebase new_base ixfun
offsetMemoryInMemBound _ summary =
  summary

offsetMemoryInBodyReturns :: RebaseMap -> BodyReturns -> BodyReturns
offsetMemoryInBodyReturns offsets (MemArray pt shape u (ReturnsInBlock mem ixfun))
  | Just ixfun' <- isStaticIxFun ixfun,
    Just new_base <- lookupNewBase mem (IxFun.base ixfun', pt) offsets =
      MemArray pt shape u $ ReturnsInBlock mem $
      IxFun.rebase (fmap (fmap Free) new_base) ixfun
offsetMemoryInBodyReturns _ br = br

offsetMemoryInExp :: RebaseMap -> Exp InKernel -> Either String (Exp InKernel)
offsetMemoryInExp offsets (DoLoop ctx val form body) = do
  body' <- offsetMemoryInBody offsets body
  return $ DoLoop (zip ctxparams' ctxinit) (zip valparams' valinit) form body'
  where (ctxparams, ctxinit) = unzip ctx
        (valparams, valinit) = unzip val
        ctxparams' = map (offsetMemoryInParam offsets) ctxparams
        valparams' = map (offsetMemoryInParam offsets) valparams
offsetMemoryInExp offsets (Op (Inner (GroupStream w max_chunk lam accs arrs))) = do
  body <- offsetMemoryInBody offsets $ groupStreamLambdaBody lam
  let lam' = lam { groupStreamLambdaBody = body
                 , groupStreamAccParams = map (offsetMemoryInParam offsets) $
                                          groupStreamAccParams lam
                 , groupStreamArrParams = map (offsetMemoryInParam offsets) $
                                          groupStreamArrParams lam
                 }
  return $ Op $ Inner $ GroupStream w max_chunk lam' accs arrs
offsetMemoryInExp offsets (Op (Inner (GroupReduce w lam input))) = do
  body <- offsetMemoryInBody offsets $ lambdaBody lam
  let lam' = lam { lambdaBody = body }
  return $ Op $ Inner $ GroupReduce w lam' input
offsetMemoryInExp offsets (Op (Inner (Combine cspace ts active body))) =
  Op . Inner . Combine cspace ts active <$> offsetMemoryInBody offsets body
offsetMemoryInExp offsets e = mapExpM recurse e
  where recurse = identityMapper
                  { mapOnBody = const $ offsetMemoryInBody offsets
                  , mapOnBranchType = return . offsetMemoryInBodyReturns offsets
                  }

---- Slicing allocation sizes out of a kernel.

unAllocInKernelBody :: KernelBody InKernel
                    -> Either String (KernelBody Kernels.InKernel)
unAllocInKernelBody = unAllocKernelBody False
  where
    unAllocBody (Body attr stms res) =
      Body attr <$> unAllocStms True stms <*> pure res

    unAllocKernelBody nested (KernelBody attr stms res) =
      KernelBody attr <$> unAllocStms nested stms <*> pure res

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

sliceKernelSizes :: [SubExp] -> KernelSpace -> KernelBody InKernel
                 -> ExpandM (Stms Kernels.Kernels, [VName], [VName])
sliceKernelSizes sizes kspace kbody = do
  kbody' <- either compilerLimitationS return $ unAllocInKernelBody kbody
  let num_sizes = length sizes
      i64s = replicate num_sizes $ Prim int64
  inkernels_scope <- unAllocScope <$> ask

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
      mapM_ addStm $ kernelBodyStms kbody'
      forM (zip params sizes) $ \(p, se) ->
        letSubExp "z" $ BasicOp $ BinOp (SMax Int64) (Var (paramName p)) se
    localScope (scopeOfKernelSpace kspace) $
      Kernels.simplifyLambda (Lambda params (Body () stms zs) i64s) []

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
