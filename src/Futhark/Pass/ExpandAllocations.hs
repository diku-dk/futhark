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
import Futhark.Pass.ExtractKernels.BlockedKernel (blockedScan)
import Futhark.Pass.ExplicitAllocations (explicitAllocationsInStms)
import Futhark.Util.IntegralExp

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

  (variant_alloc_stms, kernel_prologue_stms, variant_alloc_offsets) <-
    expandedVariantAllocations kspace kbody variant_allocs

  let alloc_offsets = invariant_alloc_offsets <> variant_alloc_offsets
      kbody'' = offsetMemoryInKernelBody alloc_offsets
                kbody' { kernelBodyStms = kernel_prologue_stms <> kernelBodyStms kbody' }
      alloc_stms = invariant_alloc_stms <> variant_alloc_stms

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
              untouched d = DimSlice (ValueExp $ value (0::Int32)) d
                                     (ValueExp $ value (1::Int32))
              offset_ixfun = IxFun.slice permuted_ixfun $
                             [DimFix (LeafExp group_id int32),
                              DimFix (LeafExp local_id int32)] ++
                             map untouched old_shape
          in offset_ixfun

expandedVariantAllocations :: KernelSpace -> KernelBody InKernel
                           -> M.Map VName (SubExp, Space)
                           -> ExpandM (Stms ExplicitMemory, Stms InKernel, RebaseMap)
expandedVariantAllocations _ _ variant_allocs
  | null variant_allocs = return (mempty, mempty, mempty)
expandedVariantAllocations kspace kbody variant_allocs = do
  let sizes_to_blocks = removeCommonSizes variant_allocs
      variant_sizes = map fst sizes_to_blocks

  (slice_stms, offsets, size_sums, offset_stms) <-
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

  return (slice_stms' <> stmsFromList alloc_bnds, offset_stms, mconcat rebases)
  where expand (mem, (offset, total_size, space)) = do
          let allocpat = Pattern [] [PatElem mem $
                                     MemMem total_size space]
          return (Let allocpat (defAux ()) $ Op $ Alloc total_size space,
                  M.singleton mem $ newBase offset total_size)

        -- For the variant allocations, we add an inner dimension,
        -- which is then offset by a thread-specific amount.
        newBase offset total_size (old_shape, pt) =
          let pt_size = ValueExp (IntValue $ Int32Value $ primByteSize pt)
              offset' = ConvOpExp (SExt Int64 Int32) (primExpFromSubExp int64 offset)
                        `quot` pt_size
              total_size' = ConvOpExp (SExt Int64 Int32) $ primExpFromSubExp int32 total_size
              ixfun_root = IxFun.iota [total_size' `quot` pt_size]
              ixfun_sliced = IxFun.offsetIndex ixfun_root offset'
              shape_change = if length old_shape == 1
                             then map DimCoercion old_shape
                             else map DimNew old_shape
          in IxFun.reshape ixfun_sliced shape_change

-- | A map from memory block names to new index function bases.

type RebaseMap = M.Map VName (([PrimExp VName], PrimType) -> IxFun)

lookupNewBase :: VName -> ([PrimExp VName], PrimType) -> RebaseMap -> Maybe IxFun
lookupNewBase name x = fmap ($x) . M.lookup name

offsetMemoryInKernelBody :: RebaseMap -> KernelBody InKernel
                         -> KernelBody InKernel
offsetMemoryInKernelBody initial_offsets kbody =
  kbody { kernelBodyStms = stmsFromList stms' }
  where stms' = snd $ mapAccumL offsetMemoryInStm initial_offsets $
                stmsToList $ kernelBodyStms kbody

offsetMemoryInBody :: RebaseMap -> Body InKernel -> Body InKernel
offsetMemoryInBody offsets (Body attr bnds res) =
  Body attr (stmsFromList $ snd $ mapAccumL offsetMemoryInStm offsets $ stmsToList bnds) res

offsetMemoryInStm :: RebaseMap -> Stm InKernel
                      -> (RebaseMap, Stm InKernel)
offsetMemoryInStm offsets (Let pat attr e) =
  (offsets', Let pat' attr $ offsetMemoryInExp offsets e)
  where (offsets', pat') = offsetMemoryInPattern offsets pat

offsetMemoryInPattern :: RebaseMap -> Pattern InKernel -> (RebaseMap, Pattern InKernel)
offsetMemoryInPattern offsets (Pattern ctx vals) =
  (offsets', Pattern ctx vals')
  where offsets' = foldl inspectCtx offsets ctx
        vals' = map inspectVal vals
        inspectVal patElem =
          patElem { patElemAttr =
                       offsetMemoryInMemBound offsets' $ patElemAttr patElem
                  }
        inspectCtx ctx_offsets patElem
          | Mem _ _ <- patElemType patElem =
              error $ unwords ["Cannot deal with existential memory block",
                               pretty (patElemName patElem),
                               "when expanding inside kernels."]
          | otherwise =
              ctx_offsets

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

offsetMemoryInExp :: RebaseMap -> Exp InKernel -> Exp InKernel
offsetMemoryInExp offsets (DoLoop ctx val form body) =
  DoLoop (zip ctxparams' ctxinit) (zip valparams' valinit) form body'
  where (ctxparams, ctxinit) = unzip ctx
        (valparams, valinit) = unzip val
        body' = offsetMemoryInBody offsets body
        ctxparams' = map (offsetMemoryInParam offsets) ctxparams
        valparams' = map (offsetMemoryInParam offsets) valparams
offsetMemoryInExp offsets (Op (Inner (GroupStream w max_chunk lam accs arrs))) =
  Op (Inner (GroupStream w max_chunk lam' accs arrs))
  where lam' =
          lam { groupStreamLambdaBody = offsetMemoryInBody offsets $
                                        groupStreamLambdaBody lam
              , groupStreamAccParams = map (offsetMemoryInParam offsets) $
                                       groupStreamAccParams lam
              , groupStreamArrParams = map (offsetMemoryInParam offsets) $
                                       groupStreamArrParams lam
              }
offsetMemoryInExp offsets (Op (Inner (GroupReduce w lam input))) =
  Op (Inner (GroupReduce w lam' input))
  where lam' = lam { lambdaBody = offsetMemoryInBody offsets $ lambdaBody lam }
offsetMemoryInExp offsets (Op (Inner (Combine cspace ts active body))) =
  Op $ Inner $ Combine cspace ts active $ offsetMemoryInBody offsets body
offsetMemoryInExp offsets e = mapExp recurse e
  where recurse = identityMapper
                  { mapOnBody = const $ return . offsetMemoryInBody offsets
                  , mapOnBranchType = return . offsetMemoryInBodyReturns offsets
                  }

---- Slicing allocation sizes out of a kernel.

unAllocInKernelBody :: KernelBody InKernel -> KernelBody Kernels.InKernel
unAllocInKernelBody = unAllocKernelBody
  where
    unAllocBody (Body attr stms res) =
      Body attr (unAllocStms stms) res

    unAllocKernelBody (KernelBody attr stms res) =
      KernelBody attr (unAllocStms stms) res

    unAllocStms = stmsFromList . mapMaybe unAlloc . stmsToList

    unAllocKernelExp (SplitSpace o w i elems_per_thread) =
      SplitSpace o w i elems_per_thread
    unAllocKernelExp (Combine cspace ts active body) =
      Combine cspace ts active $ unAllocBody body
    unAllocKernelExp (GroupReduce w lam input) =
      GroupReduce w (unAllocLambda lam) input
    unAllocKernelExp (GroupScan w lam input) =
      GroupScan w (unAllocLambda lam) input
    unAllocKernelExp (GroupStream w maxchunk lam accs arrs) =
      GroupStream w maxchunk (unAllocStreamLambda lam) accs arrs

    unAllocStreamLambda (GroupStreamLambda chunk_size chunk_offset
                         acc_params arr_params body) =
      GroupStreamLambda chunk_size chunk_offset
                        (unParams acc_params) (unParams arr_params) $
                        unAllocBody body

    unAllocLambda (Lambda params body ret) =
      Lambda (unParams params) (unAllocBody body) ret

    unParams = mapMaybe $ traverse unAttr

    unAllocPattern (Pattern ctx val) =
      Pattern (mapMaybe (rephrasePatElem unAttr) ctx)
              (mapMaybe (rephrasePatElem unAttr) val)

    unAllocOp :: Op InKernel -> Maybe (Op Kernels.InKernel)
    unAllocOp Alloc{} = Nothing
    unAllocOp (Inner op) = Just $ unAllocKernelExp op

    unAlloc (Let pat attr e) =
      Let (unAllocPattern pat) attr <$> mapExpM unAlloc' e

    unAlloc' :: Mapper InKernel Kernels.InKernel Maybe
    unAlloc' = Mapper { mapOnBody = \_ -> Just . unAllocBody
                      , mapOnRetType = unAttr
                      , mapOnBranchType = unAttr
                      , mapOnFParam = traverse unAttr
                      , mapOnLParam = traverse unAttr
                      , mapOnOp = unAllocOp
                      , mapOnSubExp = Just
                      , mapOnVName = Just
                      , mapOnCertificates = Just
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

memoryOffsetStms :: [VName] -> KernelSpace -> ExpandM ([VName], Stms InKernel)
memoryOffsetStms offsets kspace = runBinder $ forM offsets $ \offset_array -> do
  is_first <- letSubExp "cond" $ BasicOp $ CmpOp (CmpEq int32)
              (intConst Int32 0) (Var gtid)
  offset_index <- letSubExp "offset_index" $ BasicOp $
                  BinOp (Sub Int32) (Var gtid) (intConst Int32 1)
  offset <- newVName "offset"
  let index_stm = Let (Pattern [] [PatElem offset $ MemPrim int64]) (defAux ()) $
                  BasicOp $ Index offset_array [DimFix offset_index]
  letExp "offset" $ If is_first (Body () mempty [intConst Int64 0])
                                (Body () (oneStm index_stm) [Var offset]) $
    IfAttr [primBodyType int64] IfNormal
  where gtid = spaceGlobalId kspace

sliceKernelSizes :: [SubExp] -> KernelSpace -> KernelBody InKernel
                 -> ExpandM (Stms Kernels.Kernels, [VName], [VName], Stms InKernel)
sliceKernelSizes sizes kspace kbody = do
  let kbody' = unAllocInKernelBody kbody
      num_sizes = length sizes
      i64s = replicate num_sizes $ Prim int64
  inkernels_scope <- unAllocScope <$> ask

  let kernels_scope = castScope inkernels_scope

  (sum_lam, _) <- flip runBinderT inkernels_scope $ do
    xs <- replicateM num_sizes $ newParam "x" (Prim int64)
    ys <- replicateM num_sizes $ newParam "y" (Prim int64)
    (zs, stms) <- localScope (scopeOfLParams $ xs ++ ys) $ collectStms $
                  forM (zip xs ys) $ \(x,y) ->
      letSubExp "z" $ BasicOp $ BinOp (Add Int64) (Var $ paramName x) (Var $ paramName y)
    return $ Lambda (xs ++ ys) (mkBody stms zs) i64s

  (size_lam', _) <- flip runBinderT inkernels_scope $ do
    params <- replicateM num_sizes $ newParam "x" (Prim int64)
    (zs, stms) <- localScope (scopeOfLParams params) $ collectStms $ do
      mapM_ addStm $ kernelBodyStms kbody'
      forM (zip params sizes) $ \(p, se) ->
        letSubExp "z" $ BasicOp $ BinOp (Add Int64) (Var (paramName p)) se
    Kernels.simplifyLambda (Lambda params (Body () stms zs) i64s) []

  ((offsets, size_sums), slice_stms) <- flip runBinderT kernels_scope $ do
    space_size <- letSubExp "space_size" =<<
                  foldBinOp (Mul Int32) (intConst Int32 1)
                  (map snd $ spaceDimensions kspace)

    pat <- basicPattern [] <$> replicateM num_sizes
           (newIdent "offsets" $ Prim int64 `arrayOfRow` space_size)

    sums <- blockedScan pat space_size
      sum_lam size_lam' (intConst Int64 1) (spaceDimensions kspace) []
      (replicate num_sizes $ intConst Int64 0) []
    return (patternNames pat, sums)

  (offsets', offset_stms) <- memoryOffsetStms offsets kspace

  return (slice_stms, offsets', size_sums, offset_stms)
