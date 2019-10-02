{-# LANGUAGE TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
-- | Expand allocations inside of maps when possible.
module Futhark.Pass.ExpandAllocations
       ( expandAllocations )
where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map.Strict as M
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
import Futhark.Pass.ExtractKernels.BlockedKernel (segThread, nonSegRed)
import Futhark.Pass.ExplicitAllocations (explicitAllocationsInStms)
import Futhark.Transform.Rename (renameStm)
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

nameInfoConv :: NameInfo ExplicitMemory -> NameInfo ExplicitMemory
nameInfoConv (LetInfo mem_info) = LetInfo mem_info
nameInfoConv (FParamInfo mem_info) = FParamInfo mem_info
nameInfoConv (LParamInfo mem_info) = LParamInfo mem_info
nameInfoConv (IndexInfo it) = IndexInfo it

transformExp :: Exp ExplicitMemory -> ExpandM (Stms ExplicitMemory, Exp ExplicitMemory)

transformExp (Op (Inner (SegOp (SegMap lvl space ts kbody)))) = do
  (alloc_stms, (_, kbody')) <- transformScanRed lvl space [] kbody
  return (alloc_stms,
          Op $ Inner $ SegOp $ SegMap lvl space ts kbody')

transformExp (Op (Inner (SegOp (SegRed lvl space reds ts kbody)))) = do
  (alloc_stms, (lams, kbody')) <-
    transformScanRed lvl space (map segRedLambda reds) kbody
  let reds' = zipWith (\red lam -> red { segRedLambda = lam }) reds lams
  return (alloc_stms,
          Op $ Inner $ SegOp $ SegRed lvl space reds' ts kbody')

transformExp (Op (Inner (SegOp (SegScan lvl space scan_op nes ts kbody)))) = do
  (alloc_stms, (scan_op', kbody')) <- transformScanRed lvl space [scan_op] kbody
  return (alloc_stms,
          Op $ Inner $ SegOp $ SegScan lvl space (head scan_op') nes ts kbody')

transformExp (Op (Inner (SegOp (SegGenRed lvl space ops ts kbody)))) = do
  (alloc_stms, (lams', kbody')) <- transformScanRed lvl space lams kbody
  let ops' = zipWith onOp ops lams'
  return (alloc_stms,
          Op $ Inner $ SegOp $ SegGenRed lvl space ops' ts kbody')
  where lams = map genReduceOp ops
        onOp op lam = op { genReduceOp = lam }

transformExp e =
  return (mempty, e)

transformScanRed :: SegLevel -> SegSpace
                 -> [Lambda ExplicitMemory]
                 -> KernelBody ExplicitMemory
                 -> ExpandM (Stms ExplicitMemory, ([Lambda ExplicitMemory], KernelBody ExplicitMemory))
transformScanRed lvl space ops kbody = do
  bound_outside <- asks $ namesFromList . M.keys
  let (kbody', kbody_allocs) =
        extractKernelBodyAllocations (bound_outside<>bound_in_kernel) kbody
      (ops', ops_allocs) = unzip $ map (extractLambdaAllocations bound_outside) ops
      variantAlloc (Var v) = v `nameIn` bound_in_kernel
      variantAlloc _ = False
      allocs = kbody_allocs <> mconcat ops_allocs
      (variant_allocs, invariant_allocs) = M.partition (variantAlloc . fst) allocs

  allocsForBody variant_allocs invariant_allocs lvl space kbody' $ \alloc_stms kbody'' -> do
    ops'' <- forM ops' $ \op' ->
      localScope (scopeOf op') $ offsetMemoryInLambda op'
    return (alloc_stms, (ops'', kbody''))

  where bound_in_kernel = namesFromList $ M.keys $ scopeOfSegSpace space <>
                          scopeOf (kernelBodyStms kbody)

allocsForBody :: M.Map VName (SubExp, Space)
              -> M.Map VName (SubExp, Space)
              -> SegLevel -> SegSpace
              -> KernelBody ExplicitMemory
              -> (Stms ExplicitMemory -> KernelBody ExplicitMemory -> OffsetM b)
              -> ExpandM b
allocsForBody variant_allocs invariant_allocs lvl space kbody' m = do
  (alloc_offsets, alloc_stms) <-
    memoryRequirements lvl space
    (kernelBodyStms kbody') variant_allocs invariant_allocs

  scope <- askScope
  let scope' = scopeOfSegSpace space <> M.map nameInfoConv scope
  either compilerLimitationS pure $ runOffsetM scope' alloc_offsets $ do
    kbody'' <- offsetMemoryInKernelBody kbody'
    m alloc_stms kbody''

memoryRequirements :: SegLevel -> SegSpace
                   -> Stms ExplicitMemory
                   -> M.Map VName (SubExp, Space)
                   -> M.Map VName (SubExp, Space)
                   -> ExpandM (RebaseMap, Stms ExplicitMemory)
memoryRequirements lvl space kstms variant_allocs invariant_allocs = do
  ((num_threads, num_threads64), num_threads_stms) <- runBinder $ do
    num_threads <- letSubExp "num_threads" $ BasicOp $ BinOp (Mul Int32)
                   (unCount $ segNumGroups lvl) (unCount $ segGroupSize lvl)
    num_threads64 <- letSubExp "num_threads64" $ BasicOp $ ConvOp (SExt Int32 Int64) num_threads
    return (num_threads, num_threads64)

  (invariant_alloc_stms, invariant_alloc_offsets) <-
    inScopeOf num_threads_stms $ expandedInvariantAllocations
    (num_threads64, segNumGroups lvl, segGroupSize lvl)
    space invariant_allocs

  (variant_alloc_stms, variant_alloc_offsets) <-
    inScopeOf num_threads_stms $ expandedVariantAllocations num_threads space kstms variant_allocs

  return (invariant_alloc_offsets <> variant_alloc_offsets,
          num_threads_stms <> invariant_alloc_stms <> variant_alloc_stms)

-- | A description of allocations that have been extracted, and how
-- much memory (and which space) is needed.
type Extraction = M.Map VName (SubExp, Space)

-- | Extract allocations from 'Thread' statements with
-- 'extractThreadAllocations'.
extractKernelBodyAllocations :: Names -> KernelBody ExplicitMemory
                             -> (KernelBody ExplicitMemory,
                                 Extraction)
extractKernelBodyAllocations bound_outside =
  extractGenericBodyAllocations bound_outside kernelBodyStms $
  \stms kbody -> kbody { kernelBodyStms = stms }

extractBodyAllocations :: Names -> Body ExplicitMemory
                       -> (Body ExplicitMemory, Extraction)
extractBodyAllocations bound_outside =
  extractGenericBodyAllocations bound_outside bodyStms $
  \stms body -> body { bodyStms = stms }

extractLambdaAllocations :: Names -> Lambda ExplicitMemory
                         -> (Lambda ExplicitMemory, Extraction)
extractLambdaAllocations bound_outside lam = (lam { lambdaBody = body' }, allocs)
  where (body', allocs) = extractBodyAllocations bound_outside $ lambdaBody lam

extractGenericBodyAllocations :: Names
                              -> (body -> Stms ExplicitMemory)
                              -> (Stms ExplicitMemory -> body -> body)
                              -> body
                              -> (body,
                                  Extraction)
extractGenericBodyAllocations bound_outside get_stms set_stms body =
  let (stms, allocs) = runWriter $ fmap catMaybes $
                       mapM (extractStmAllocations bound_outside) $
                       stmsToList $ get_stms body
  in (set_stms (stmsFromList stms) body, allocs)

extractStmAllocations :: Names -> Stm ExplicitMemory
                      -> Writer Extraction (Maybe (Stm ExplicitMemory))
extractStmAllocations bound_outside (Let (Pattern [] [patElem]) _ (Op (Alloc size space)))
  | space `notElem`
    [Space "private", Space "local"] ++
    map Space (M.keys allScalarMemory),
    visibleOutside size = do
      tell $ M.singleton (patElemName patElem) (size, space)
      return Nothing

        where visibleOutside (Var v) = v `nameIn` bound_outside
              visibleOutside Constant{} = True

extractStmAllocations bound_outside stm = do
  e <- mapExpM expMapper $ stmExp stm
  return $ Just $ stm { stmExp = e }
  where expMapper = identityMapper { mapOnBody = const onBody
                                   , mapOnOp = onOp }

        onBody body = do
          let (body', allocs) = extractBodyAllocations bound_outside body
          tell allocs
          return body'

        onOp (Inner (SegOp op)) = Inner . SegOp <$> mapSegOpM opMapper op
        onOp op = return op

        opMapper = identitySegOpMapper { mapOnSegOpLambda = onLambda
                                       , mapOnSegOpBody = onKernelBody
                                       }

        onKernelBody body = do
          let (body', allocs) = extractKernelBodyAllocations bound_outside body
          tell allocs
          return body'

        onLambda lam = do
          body <- onBody $ lambdaBody lam
          return lam { lambdaBody = body }

expandedInvariantAllocations :: (SubExp, Count NumGroups SubExp, Count GroupSize SubExp)
                             -> SegSpace
                             -> Extraction
                             -> ExpandM (Stms ExplicitMemory, RebaseMap)
expandedInvariantAllocations (num_threads64, Count num_groups, Count group_size)
                             segspace
                             invariant_allocs = do
  -- We expand the invariant allocations by adding an inner dimension
  -- equal to the number of kernel threads.
  (alloc_bnds, rebases) <- unzip <$> mapM expand (M.toList invariant_allocs)

  return (mconcat alloc_bnds, mconcat rebases)
  where expand (mem, (per_thread_size, space)) = do
          total_size <- newVName "total_size"
          let sizepat = Pattern [] [PatElem total_size $ MemPrim int64]
              allocpat = Pattern [] [PatElem mem $ MemMem space]
          return (stmsFromList
                  [Let sizepat (defAux ()) $
                    BasicOp $ BinOp (Mul Int64) num_threads64 per_thread_size,
                   Let allocpat (defAux ()) $
                    Op $ Alloc (Var total_size) space],
                  M.singleton mem newBase)

        newBase (old_shape, _) =
          let num_dims = length old_shape
              perm = num_dims : [0..num_dims-1]
              root_ixfun = IxFun.iota (old_shape
                                       ++ [primExpFromSubExp int32 num_groups *
                                           primExpFromSubExp int32 group_size])
              permuted_ixfun = IxFun.permute root_ixfun perm
              untouched d = DimSlice (fromInt32 0) d (fromInt32 1)
              offset_ixfun = IxFun.slice permuted_ixfun $
                             DimFix (LeafExp (segFlat segspace) int32) :
                             map untouched old_shape
          in offset_ixfun

expandedVariantAllocations :: SubExp
                           -> SegSpace -> Stms ExplicitMemory
                           -> Extraction
                           -> ExpandM (Stms ExplicitMemory, RebaseMap)
expandedVariantAllocations _ _ _ variant_allocs
  | null variant_allocs = return (mempty, mempty)
expandedVariantAllocations num_threads kspace kstms variant_allocs = do
  let sizes_to_blocks = removeCommonSizes variant_allocs
      variant_sizes = map fst sizes_to_blocks

  (slice_stms, offsets, size_sums) <-
    sliceKernelSizes num_threads variant_sizes kspace kstms
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
          let allocpat = Pattern [] [PatElem mem $ MemMem space]
          return (Let allocpat (defAux ()) $ Op $ Alloc total_size space,
                  M.singleton mem $ newBase offset)

        num_threads' = primExpFromSubExp int32 num_threads
        gtid = LeafExp (segFlat kspace) int32

        -- For the variant allocations, we add an inner dimension,
        -- which is then offset by a thread-specific amount.
        newBase size_per_thread (old_shape, pt) =
          let pt_size = fromInt32 $ primByteSize pt
              elems_per_thread = ConvOpExp (SExt Int64 Int32)
                                 (primExpFromSubExp int64 size_per_thread)
                                 `quot` pt_size
              root_ixfun = IxFun.iota [elems_per_thread, num_threads']
              offset_ixfun = IxFun.slice root_ixfun
                             [DimSlice (fromInt32 0) num_threads' (fromInt32 1),
                              DimFix gtid]
              shapechange = if length old_shape == 1
                            then map DimCoercion old_shape
                            else map DimNew old_shape
          in IxFun.reshape offset_ixfun shapechange

-- | A map from memory block names to new index function bases.

type RebaseMap = M.Map VName (([PrimExp VName], PrimType) -> IxFun)

newtype OffsetM a = OffsetM (ReaderT (Scope ExplicitMemory)
                             (ReaderT RebaseMap (Either String)) a)
  deriving (Applicative, Functor, Monad,
            HasScope ExplicitMemory, LocalScope ExplicitMemory,
            MonadError String)

runOffsetM :: Scope ExplicitMemory -> RebaseMap -> OffsetM a -> Either String a
runOffsetM scope offsets (OffsetM m) =
  runReaderT (runReaderT m scope) offsets

askRebaseMap :: OffsetM RebaseMap
askRebaseMap = OffsetM $ lift ask

lookupNewBase :: VName -> ([PrimExp VName], PrimType) -> OffsetM (Maybe IxFun)
lookupNewBase name x = do
  offsets <- askRebaseMap
  return $ ($ x) <$> M.lookup name offsets

offsetMemoryInKernelBody :: KernelBody ExplicitMemory -> OffsetM (KernelBody ExplicitMemory)
offsetMemoryInKernelBody kbody = do
  scope <- askScope
  stms' <- stmsFromList . snd <$>
           mapAccumLM (\scope' -> localScope scope' . offsetMemoryInStm) scope
           (stmsToList $ kernelBodyStms kbody)
  return kbody { kernelBodyStms = stms' }

offsetMemoryInBody :: Body ExplicitMemory -> OffsetM (Body ExplicitMemory)
offsetMemoryInBody (Body attr stms res) = do
  scope <- askScope
  stms' <- stmsFromList . snd <$>
           mapAccumLM (\scope' -> localScope scope' . offsetMemoryInStm) scope
           (stmsToList stms)
  return $ Body attr stms' res

offsetMemoryInStm :: Stm ExplicitMemory -> OffsetM (Scope ExplicitMemory, Stm ExplicitMemory)
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

offsetMemoryInPattern :: Pattern ExplicitMemory -> OffsetM (Pattern ExplicitMemory)
offsetMemoryInPattern (Pattern ctx vals) = do
  mapM_ inspectCtx ctx
  Pattern ctx <$> mapM inspectVal vals
  where inspectVal patElem = do
          new_attr <- offsetMemoryInMemBound $ patElemAttr patElem
          return patElem { patElemAttr = new_attr }
        inspectCtx patElem
          | Mem space <- patElemType patElem,
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

offsetMemoryInLambda :: Lambda ExplicitMemory -> OffsetM (Lambda ExplicitMemory)
offsetMemoryInLambda lam = inScopeOf lam $ do
  body <- offsetMemoryInBody $ lambdaBody lam
  return $ lam { lambdaBody = body }

offsetMemoryInExp :: Exp ExplicitMemory -> OffsetM (Exp ExplicitMemory)
offsetMemoryInExp (DoLoop ctx val form body) = do
  let (ctxparams, ctxinit) = unzip ctx
      (valparams, valinit) = unzip val
  ctxparams' <- mapM offsetMemoryInParam ctxparams
  valparams' <- mapM offsetMemoryInParam valparams
  body' <- localScope (scopeOfFParams ctxparams' <> scopeOfFParams valparams' <> scopeOf form) (offsetMemoryInBody body)
  return $ DoLoop (zip ctxparams' ctxinit) (zip valparams' valinit) form body'
offsetMemoryInExp e = mapExpM recurse e
  where recurse = identityMapper
                  { mapOnBody = \bscope -> localScope bscope . offsetMemoryInBody
                  , mapOnBranchType = offsetMemoryInBodyReturns
                  , mapOnOp = onOp
                  }
        onOp (Inner (SegOp op)) = Inner . SegOp <$> mapSegOpM segOpMapper op
          where segOpMapper =
                  identitySegOpMapper { mapOnSegOpBody = offsetMemoryInKernelBody
                                      , mapOnSegOpLambda = offsetMemoryInLambda
                                      }
        onOp op = return op


---- Slicing allocation sizes out of a kernel.

unAllocKernelsStms :: Stms ExplicitMemory -> Either String (Stms Kernels.Kernels)
unAllocKernelsStms = unAllocStms False
  where
    unAllocBody (Body attr stms res) =
      Body attr <$> unAllocStms True stms <*> pure res

    unAllocKernelBody (KernelBody attr stms res) =
      KernelBody attr <$> unAllocStms True stms <*> pure res

    unAllocStms nested =
      fmap (stmsFromList . catMaybes) . mapM (unAllocStm nested) . stmsToList

    unAllocStm nested stm@(Let _ _ (Op Alloc{}))
      | nested = throwError $ "Cannot handle nested allocation: " ++ pretty stm
      | otherwise = return Nothing
    unAllocStm _ (Let pat attr e) =
      Just <$> (Let <$> unAllocPattern pat <*> pure attr <*> mapExpM unAlloc' e)

    unAllocLambda (Lambda params body ret) =
      Lambda (unParams params) <$> unAllocBody body <*> pure ret

    unParams = mapMaybe $ traverse unAttr

    unAllocPattern pat@(Pattern ctx val) =
      Pattern <$> maybe bad return (mapM (rephrasePatElem unAttr) ctx)
              <*> maybe bad return (mapM (rephrasePatElem unAttr) val)
      where bad = Left $ "Cannot handle memory in pattern " ++ pretty pat

    unAllocOp Alloc{} = Left "unAllocOp: unhandled Alloc"
    unAllocOp (Inner OtherOp{}) = Left "unAllocOp: unhandled OtherOp"
    unAllocOp (Inner (SizeOp op)) =
      return $ SizeOp op
    unAllocOp (Inner (SegOp op)) = SegOp <$> mapSegOpM mapper op
      where mapper = identitySegOpMapper { mapOnSegOpLambda = unAllocLambda
                                         , mapOnSegOpBody = unAllocKernelBody
                                         }

    unParam p = maybe bad return $ traverse unAttr p
      where bad = Left $ "Cannot handle memory-typed parameter '" ++ pretty p ++ "'"

    unT t = maybe bad return $ unAttr t
      where bad = Left $ "Cannot handle memory type '" ++ pretty t ++ "'"

    unAlloc' = Mapper { mapOnBody = const unAllocBody
                      , mapOnRetType = unT
                      , mapOnBranchType = unT
                      , mapOnFParam = unParam
                      , mapOnLParam = unParam
                      , mapOnOp = unAllocOp
                      , mapOnSubExp = Right
                      , mapOnVName = Right
                      }

unAttr :: MemInfo d u ret -> Maybe (TypeBase (ShapeBase d) u)
unAttr (MemPrim pt) = Just $ Prim pt
unAttr (MemArray pt shape u _) = Just $ Array pt shape u
unAttr MemMem{} = Nothing

unAllocScope :: Scope ExplicitMemory -> Scope Kernels.Kernels
unAllocScope = M.mapMaybe unInfo
  where unInfo (LetInfo attr) = LetInfo <$> unAttr attr
        unInfo (FParamInfo attr) = FParamInfo <$> unAttr attr
        unInfo (LParamInfo attr) = LParamInfo <$> unAttr attr
        unInfo (IndexInfo it) = Just $ IndexInfo it

removeCommonSizes :: Extraction
                  -> [(SubExp, [(VName, Space)])]
removeCommonSizes = M.toList . foldl' comb mempty . M.toList
  where comb m (mem, (size, space)) = M.insertWith (++) size [(mem, space)] m

sliceKernelSizes :: SubExp -> [SubExp] -> SegSpace -> Stms ExplicitMemory
                 -> ExpandM (Stms Kernels.Kernels, [VName], [VName])
sliceKernelSizes num_threads sizes space kstms = do
  kstms' <- either compilerLimitationS return $ unAllocKernelsStms kstms
  let num_sizes = length sizes
      i64s = replicate num_sizes $ Prim int64

  kernels_scope <- asks unAllocScope

  (max_lam, _) <- flip runBinderT kernels_scope $ do
    xs <- replicateM num_sizes $ newParam "x" (Prim int64)
    ys <- replicateM num_sizes $ newParam "y" (Prim int64)
    (zs, stms) <- localScope (scopeOfLParams $ xs ++ ys) $ collectStms $
                  forM (zip xs ys) $ \(x,y) ->
      letSubExp "z" $ BasicOp $ BinOp (SMax Int64) (Var $ paramName x) (Var $ paramName y)
    return $ Lambda (xs ++ ys) (mkBody stms zs) i64s

  flat_gtid_lparam <- Param <$> newVName "flat_gtid" <*> pure (Prim (IntType Int32))

  (size_lam', _) <- flip runBinderT kernels_scope $ do
    params <- replicateM num_sizes $ newParam "x" (Prim int64)
    (zs, stms) <- localScope (scopeOfLParams params <>
                              scopeOfLParams [flat_gtid_lparam]) $ collectStms $ do

      -- Even though this SegRed is one-dimensional, we need to
      -- provide indexes corresponding to the original potentially
      -- multi-dimensional construct.
      let (kspace_gtids, kspace_dims) = unzip $ unSegSpace space
          new_inds = unflattenIndex
                     (map (primExpFromSubExp int32) kspace_dims)
                     (primExpFromSubExp int32 $ Var $ paramName flat_gtid_lparam)
      zipWithM_ letBindNames_ (map pure kspace_gtids) =<< mapM toExp new_inds

      mapM_ addStm kstms'
      return sizes

    localScope (scopeOfSegSpace space) $
      Kernels.simplifyLambda (Lambda [flat_gtid_lparam] (Body () stms zs) i64s) []

  ((maxes_per_thread, size_sums), slice_stms) <- flip runBinderT kernels_scope $ do
    num_threads_64 <- letSubExp "num_threads" $
                      BasicOp $ ConvOp (SExt Int32 Int64) num_threads

    pat <- basicPattern [] <$> replicateM num_sizes
           (newIdent "max_per_thread" $ Prim int64)

    w <- letSubExp "size_slice_w" =<<
         foldBinOp (Mul Int32) (intConst Int32 1) (segSpaceDims space)

    thread_space_iota <- letExp "thread_space_iota" $ BasicOp $
                         Iota w (intConst Int32 0) (intConst Int32 1) Int32
    let red_op = SegRedOp Commutative max_lam
                 (replicate num_sizes $ intConst Int64 0) mempty
    lvl <- segThread "segred"

    addStms =<< mapM renameStm =<<
      nonSegRed lvl pat w [red_op] size_lam' [thread_space_iota]

    size_sums <- forM (patternNames pat) $ \threads_max ->
      letExp "size_sum" $
      BasicOp $ BinOp (Mul Int64) (Var threads_max) num_threads_64

    return (patternNames pat, size_sums)

  return (slice_stms, maxes_per_thread, size_sums)
