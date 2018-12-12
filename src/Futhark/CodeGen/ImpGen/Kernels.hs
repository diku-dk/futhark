{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.CodeGen.ImpGen.Kernels
  ( compileProg
  )
  where

import Control.Arrow ((&&&))
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List

import Prelude hiding (quot)

import Futhark.Error
import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpCode.Kernels (bytes)
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.CodeGen.ImpGen ((<--),
                               sFor, sWhile, sComment, sIf, sWhen, sUnless,
                               sOp,
                               dPrim, dPrim_, dPrimV)
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.CodeGen.SetDefaultSpace
import Futhark.Tools (partitionChunkedKernelLambdaParameters)
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem, IntegralExp)
import Futhark.Util (splitAt3)

type CallKernelGen = ImpGen.ImpM ExplicitMemory Imp.HostOp
type InKernelGen = ImpGen.ImpM InKernel Imp.KernelOp

callKernelOperations :: ImpGen.Operations ExplicitMemory Imp.HostOp
callKernelOperations =
  ImpGen.Operations { ImpGen.opsExpCompiler = expCompiler
                    , ImpGen.opsCopyCompiler = callKernelCopy
                    , ImpGen.opsOpCompiler = opCompiler
                    , ImpGen.opsStmsCompiler = ImpGen.defCompileStms
                    }

inKernelOperations :: KernelConstants -> ImpGen.Operations InKernel Imp.KernelOp
inKernelOperations constants = (ImpGen.defaultOperations $ compileInKernelOp constants)
                               { ImpGen.opsCopyCompiler = inKernelCopy
                               , ImpGen.opsExpCompiler = inKernelExpCompiler
                               , ImpGen.opsStmsCompiler = \_ -> compileKernelStms constants
                               }

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError Imp.Program)
compileProg prog =
  fmap (setDefaultSpace (Imp.Space "device")) <$>
  ImpGen.compileProg callKernelOperations (Imp.Space "device") prog

opCompiler :: Pattern ExplicitMemory -> Op ExplicitMemory
           -> CallKernelGen ()
opCompiler dest (Alloc e space) =
  ImpGen.compileAlloc dest e space
opCompiler dest (Inner kernel) =
  kernelCompiler dest kernel

compileInKernelOp :: KernelConstants -> Pattern InKernel -> Op InKernel
                  -> InKernelGen ()
compileInKernelOp _ (Pattern _ [mem]) Alloc{} =
  compilerLimitationS $ "Cannot allocate memory block " ++ pretty mem ++ " in kernel."
compileInKernelOp _ dest Alloc{} =
  compilerBugS $ "Invalid target for in-kernel allocation: " ++ show dest
compileInKernelOp constants pat (Inner op) =
  compileKernelExp constants pat op

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: Pattern ExplicitMemory -> Kernel InKernel
               -> CallKernelGen ()

kernelCompiler (Pattern _ [pe]) (GetSize key size_class) =
  sOp $ Imp.GetSize (patElemName pe) key size_class

kernelCompiler (Pattern _ [pe]) (CmpSizeLe key size_class x) =
  sOp . Imp.CmpSizeLe (patElemName pe) key size_class =<< ImpGen.compileSubExp x

kernelCompiler (Pattern _ [pe]) (GetSizeMax size_class) =
  sOp $ Imp.GetSizeMax (patElemName pe) size_class

kernelCompiler pat (Kernel desc space _ kernel_body) = do

  num_groups' <- ImpGen.subExpToDimSize $ spaceNumGroups space
  group_size' <- ImpGen.subExpToDimSize $ spaceGroupSize space
  num_threads' <- ImpGen.subExpToDimSize $ spaceNumThreads space

  let bound_in_kernel =
        M.keys $
        scopeOfKernelSpace space <>
        scopeOf (kernelBodyStms kernel_body)

  let global_tid = spaceGlobalId space
      local_tid = spaceLocalId space
      group_id = spaceGroupId space
  wave_size <- newVName "wave_size"
  inner_group_size <- newVName "group_size"
  thread_active <- newVName "thread_active"

  let (space_is, space_dims) = unzip $ spaceDimensions space
  space_dims' <- mapM ImpGen.compileSubExp space_dims
  let constants = KernelConstants global_tid local_tid group_id
                  group_size' num_threads'
                  (Imp.VarSize wave_size) (zip space_is space_dims')
                  (Imp.var thread_active Bool) mempty

  kernel_body' <-
    makeAllMemoryGlobal $ ImpGen.subImpM_ (inKernelOperations constants) $ do
    dPrim_ wave_size int32
    dPrim_ inner_group_size int32
    dPrim_ thread_active Bool
    ImpGen.dScope Nothing (scopeOfKernelSpace space)

    sOp (Imp.GetGlobalId global_tid 0)
    sOp (Imp.GetLocalId local_tid 0)
    sOp (Imp.GetLocalSize inner_group_size 0)
    sOp (Imp.GetLockstepWidth wave_size)
    sOp (Imp.GetGroupId group_id 0)

    setSpaceIndices space

    thread_active <-- isActive (spaceDimensions space)

    compileKernelBody pat constants kernel_body

  (uses, local_memory) <- computeKernelUses kernel_body' bound_in_kernel

  forM_ (kernelHints desc) $ \(s,v) -> do
    ty <- case v of
      Constant pv -> return $ Prim $ primValueType pv
      Var vn -> lookupType vn
    unless (primType ty) $ fail $ concat [ "debugKernelHint '", s, "'"
                                         , " in kernel '", kernelName desc, "'"
                                         , " did not have primType value." ]

    ImpGen.compileSubExp v >>= ImpGen.emit . Imp.DebugPrint s (elemType ty)

  sOp $ Imp.CallKernel $ Imp.AnyKernel Imp.Kernel
            { Imp.kernelBody = kernel_body'
            , Imp.kernelLocalMemory = local_memory
            , Imp.kernelUses = uses
            , Imp.kernelNumGroups = num_groups'
            , Imp.kernelGroupSize = group_size'
            , Imp.kernelName = global_tid
            , Imp.kernelDesc = kernelName desc
            }

kernelCompiler pat e =
  compilerBugS $ "ImpGen.kernelCompiler: Invalid pattern\n  " ++
  pretty pat ++ "\nfor expression\n  " ++ pretty e

expCompiler :: ImpGen.ExpCompiler ExplicitMemory Imp.HostOp
-- We generate a simple kernel for itoa and replicate.
expCompiler (Pattern _ [pe]) (BasicOp (Iota n x s et)) = do
  destloc <- ImpGen.entryArrayLocation <$> ImpGen.lookupArray (patElemName pe)
  let tag = Just $ baseTag $ patElemName pe
  thread_gid <- maybe (newVName "thread_gid") (return . VName (nameFromString "thread_gid")) tag

  makeAllMemoryGlobal $ do
    (destmem, destspace, destidx) <-
      ImpGen.fullyIndexArray' destloc [ImpGen.varIndex thread_gid] (IntType et)

    n' <- ImpGen.compileSubExp n
    x' <- ImpGen.compileSubExp x
    s' <- ImpGen.compileSubExp s

    let body = Imp.Write destmem destidx (IntType et) destspace Imp.Nonvolatile $
               Imp.ConvOpExp (SExt Int32 et) (Imp.var thread_gid int32) * s' + x'

    (group_size, num_groups) <- computeMapKernelGroups n'

    (body_uses, _) <- computeKernelUses
                      (freeIn body <> freeIn [n',x',s'])
                      [thread_gid]

    sOp $ Imp.CallKernel $ Imp.Map Imp.MapKernel
      { Imp.mapKernelThreadNum = thread_gid
      , Imp.mapKernelDesc = "iota"
      , Imp.mapKernelNumGroups = Imp.VarSize num_groups
      , Imp.mapKernelGroupSize = Imp.VarSize group_size
      , Imp.mapKernelSize = n'
      , Imp.mapKernelUses = body_uses
      , Imp.mapKernelBody = body
      }

expCompiler
  (Pattern _ [pe]) (BasicOp (Replicate (Shape ds) se)) = do
  constants <- simpleKernelConstants (Just $ baseTag $ patElemName pe) "replicate"

  t <- subExpType se
  let thread_gid = kernelGlobalThreadId constants
      row_dims = arrayDims t
      dims = ds ++ row_dims
      is' = unflattenIndex (map (ImpGen.compileSubExpOfType int32) dims) $
            ImpGen.varIndex thread_gid
  ds' <- mapM ImpGen.compileSubExp ds

  makeAllMemoryGlobal $ do
    body <- ImpGen.subImpM_ (inKernelOperations constants) $
      ImpGen.copyDWIM (patElemName pe) is' se $ drop (length ds) is'

    dims' <- mapM ImpGen.compileSubExp dims
    (group_size, num_groups) <- computeMapKernelGroups $ product dims'

    (body_uses, _) <- computeKernelUses
                      (freeIn body <> freeIn ds')
                      [thread_gid]

    sOp $ Imp.CallKernel $ Imp.Map Imp.MapKernel
      { Imp.mapKernelThreadNum = thread_gid
      , Imp.mapKernelDesc = "replicate"
      , Imp.mapKernelNumGroups = Imp.VarSize num_groups
      , Imp.mapKernelGroupSize = Imp.VarSize group_size
      , Imp.mapKernelSize = product dims'
      , Imp.mapKernelUses = body_uses
      , Imp.mapKernelBody = body
      }

-- Allocation in the "local" space is just a placeholder.
expCompiler _ (Op (Alloc _ (Space "local"))) =
  return ()

expCompiler dest e =
  ImpGen.defCompileExp dest e

callKernelCopy :: ImpGen.CopyCompiler ExplicitMemory Imp.HostOp
callKernelCopy bt
  destloc@(ImpGen.MemLocation destmem destshape destIxFun)
  srcloc@(ImpGen.MemLocation srcmem srcshape srcIxFun)
  n
  | Just (destoffset, srcoffset,
          num_arrays, size_x, size_y,
          src_elems, dest_elems) <- isMapTransposeKernel bt destloc srcloc =
  sOp $ Imp.CallKernel $
  Imp.MapTranspose bt
  destmem destoffset
  srcmem srcoffset
  num_arrays size_x size_y
  src_elems dest_elems

  | bt_size <- primByteSize bt,
    ixFunMatchesInnerShape
      (Shape $ map Imp.sizeToExp destshape) destIxFun,
    ixFunMatchesInnerShape
      (Shape $ map Imp.sizeToExp srcshape) srcIxFun,
    Just destoffset <-
      IxFun.linearWithOffset destIxFun bt_size,
    Just srcoffset  <-
      IxFun.linearWithOffset srcIxFun bt_size = do
        let row_size = product $ map ImpGen.dimSizeToExp $ drop 1 srcshape
        srcspace <- ImpGen.entryMemSpace <$> ImpGen.lookupMemory srcmem
        destspace <- ImpGen.entryMemSpace <$> ImpGen.lookupMemory destmem
        ImpGen.emit $ Imp.Copy
          destmem (bytes destoffset) destspace
          srcmem (bytes srcoffset) srcspace $
          (n * row_size) `Imp.withElemType` bt

  | otherwise = do
  global_thread_index <- newVName "copy_global_thread_index"

  -- Note that the shape of the destination and the source are
  -- necessarily the same.
  let shape = map Imp.sizeToExp srcshape
      shape_se = map (Imp.innerExp . ImpGen.dimSizeToExp) srcshape
      dest_is = unflattenIndex shape_se $ ImpGen.varIndex global_thread_index
      src_is = dest_is

  makeAllMemoryGlobal $ do
    (_, destspace, destidx) <- ImpGen.fullyIndexArray' destloc dest_is bt
    (_, srcspace, srcidx) <- ImpGen.fullyIndexArray' srcloc src_is bt

    let body = Imp.Write destmem destidx bt destspace Imp.Nonvolatile $
               Imp.index srcmem srcidx bt srcspace Imp.Nonvolatile

    destmem_size <- ImpGen.entryMemSize <$> ImpGen.lookupMemory destmem
    let writes_to = [Imp.MemoryUse destmem destmem_size]

    reads_from <- readsFromSet $
                  S.singleton srcmem <>
                  freeIn destIxFun <> freeIn srcIxFun <> freeIn destshape

    let kernel_size = Imp.innerExp n * product (drop 1 shape)
    (group_size, num_groups) <- computeMapKernelGroups kernel_size

    let bound_in_kernel = [global_thread_index]
    (body_uses, _) <- computeKernelUses (kernel_size, body) bound_in_kernel

    sOp $ Imp.CallKernel $ Imp.Map Imp.MapKernel
      { Imp.mapKernelThreadNum = global_thread_index
      , Imp.mapKernelDesc = "copy"
      , Imp.mapKernelNumGroups = Imp.VarSize num_groups
      , Imp.mapKernelGroupSize = Imp.VarSize group_size
      , Imp.mapKernelSize = kernel_size
      , Imp.mapKernelUses = nub $ body_uses ++ writes_to ++ reads_from
      , Imp.mapKernelBody = body
      }

-- | We have no bulk copy operation (e.g. memmove) inside kernels, so
-- turn any copy into a loop.
inKernelCopy :: ImpGen.CopyCompiler InKernel Imp.KernelOp
inKernelCopy = ImpGen.copyElementWise

inKernelExpCompiler :: ImpGen.ExpCompiler InKernel Imp.KernelOp
inKernelExpCompiler _ (BasicOp (Assert _ _ (loc, locs))) =
  compilerLimitationS $
  unlines [ "Cannot compile assertion at " ++
            intercalate " -> " (reverse $ map locStr $ loc:locs) ++
            " inside parallel kernel."
          , "As a workaround, surround the expression with 'unsafe'."]
-- The static arrays stuff does not work inside kernels.
inKernelExpCompiler (Pattern _ [dest]) (BasicOp (ArrayLit es _)) =
  forM_ (zip [0..] es) $ \(i,e) ->
  ImpGen.copyDWIM (patElemName dest) [fromIntegral (i::Int32)] e []
inKernelExpCompiler dest e =
  ImpGen.defCompileExp dest e

computeKernelUses :: FreeIn a =>
                     a -> [VName]
                  -> CallKernelGen ([Imp.KernelUse], [Imp.LocalMemoryUse])
computeKernelUses kernel_body bound_in_kernel = do
    let actually_free = freeIn kernel_body `S.difference` S.fromList bound_in_kernel

    -- Compute the variables that we need to pass to the kernel.
    reads_from <- readsFromSet actually_free

    -- Are we using any local memory?
    local_memory <- computeLocalMemoryUse actually_free
    return (nub reads_from, nub local_memory)

readsFromSet :: Names -> CallKernelGen [Imp.KernelUse]
readsFromSet free =
  fmap catMaybes $
  forM (S.toList free) $ \var -> do
    t <- lookupType var
    case t of
      Array {} -> return Nothing
      Mem _ (Space "local") -> return Nothing
      Mem memsize _ -> Just <$> (Imp.MemoryUse var <$>
                                 ImpGen.subExpToDimSize memsize)
      Prim bt ->
        isConstExp var >>= \case
          Just ce -> return $ Just $ Imp.ConstUse var ce
          Nothing | bt == Cert -> return Nothing
                  | otherwise  -> return $ Just $ Imp.ScalarUse var bt

computeLocalMemoryUse :: Names -> CallKernelGen [Imp.LocalMemoryUse]
computeLocalMemoryUse free =
  fmap catMaybes $
  forM (S.toList free) $ \var -> do
    t <- lookupType var
    case t of
      Mem memsize (Space "local") -> do
        memsize' <- localMemSize =<< ImpGen.subExpToDimSize memsize
        return $ Just (var, memsize')
      _ -> return Nothing

localMemSize :: Imp.MemSize -> CallKernelGen (Either Imp.MemSize Imp.KernelConstExp)
localMemSize (Imp.ConstSize x) =
  return $ Right $ ValueExp $ IntValue $ Int64Value x
localMemSize (Imp.VarSize v) = isConstExp v >>= \case
  Just e | isStaticExp e -> return $ Right e
  _ -> return $ Left $ Imp.VarSize v

-- | Only some constant expressions quality as *static* expressions,
-- which we can use for static memory allocation.  This is a bit of a
-- hack, as it is primarly motivated by what you can put as the size
-- when daring an array in C.
isStaticExp :: Imp.KernelConstExp -> Bool
isStaticExp LeafExp{} = True
isStaticExp ValueExp{} = True
isStaticExp (BinOpExp Add{} x y) = isStaticExp x && isStaticExp y
isStaticExp (BinOpExp Sub{} x y) = isStaticExp x && isStaticExp y
isStaticExp (BinOpExp Mul{} x y) = isStaticExp x && isStaticExp y
isStaticExp _ = False

isConstExp :: VName -> CallKernelGen (Maybe Imp.KernelConstExp)
isConstExp v = do
  vtable <- ImpGen.getVTable
  let lookupConstExp name = constExp =<< hasExp =<< M.lookup name vtable
      constExp (Op (Inner (GetSize key _))) = Just $ LeafExp (Imp.SizeConst key) int32
      constExp e = primExpFromExp lookupConstExp e
  return $ lookupConstExp v
  where hasExp (ImpGen.ArrayVar e _) = e
        hasExp (ImpGen.ScalarVar e _) = e
        hasExp (ImpGen.MemVar e _) = e

-- | Change every memory block to be in the global address space,
-- except those who are in the local memory space.  This only affects
-- generated code - we still need to make sure that the memory is
-- actually present on the device (and dared as variables in the
-- kernel).
makeAllMemoryGlobal :: CallKernelGen a -> CallKernelGen a
makeAllMemoryGlobal =
  local (\env -> env { ImpGen.envDefaultSpace = Imp.Space "global" }) .
  ImpGen.localVTable (M.map globalMemory)
  where globalMemory (ImpGen.MemVar _ entry)
          | ImpGen.entryMemSpace entry /= Space "local" =
              ImpGen.MemVar Nothing entry { ImpGen.entryMemSpace = Imp.Space "global" }
        globalMemory entry =
          entry

computeMapKernelGroups :: Imp.Exp -> CallKernelGen (VName, VName)
computeMapKernelGroups kernel_size = do
  group_size <- dPrim "group_size" int32
  let group_size_var = Imp.var group_size int32
  sOp $ Imp.GetSize group_size group_size Imp.SizeGroup
  num_groups <- dPrimV "num_groups" $ kernel_size `quotRoundingUp` Imp.ConvOpExp (SExt Int32 Int32) group_size_var
  return (group_size, num_groups)

isMapTransposeKernel :: PrimType -> ImpGen.MemLocation -> ImpGen.MemLocation
                     -> Maybe (Imp.Exp, Imp.Exp,
                               Imp.Exp, Imp.Exp, Imp.Exp,
                               Imp.Exp, Imp.Exp)
isMapTransposeKernel bt
  (ImpGen.MemLocation _ _ destIxFun)
  (ImpGen.MemLocation _ _ srcIxFun)
  | Just (dest_offset, perm_and_destshape) <- IxFun.rearrangeWithOffset destIxFun bt_size,
    (perm, destshape) <- unzip perm_and_destshape,
    srcshape' <- IxFun.shape srcIxFun,
    Just src_offset <- IxFun.linearWithOffset srcIxFun bt_size,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk (product srcshape') (product destshape) destshape swap r1 r2 dest_offset src_offset
  | Just dest_offset <- IxFun.linearWithOffset destIxFun bt_size,
    Just (src_offset, perm_and_srcshape) <- IxFun.rearrangeWithOffset srcIxFun bt_size,
    (perm, srcshape) <- unzip perm_and_srcshape,
    destshape' <- IxFun.shape destIxFun,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk (product srcshape) (product destshape') srcshape id r1 r2 dest_offset src_offset
  | otherwise =
    Nothing
  where bt_size = primByteSize bt
        swap (x,y) = (y,x)

        isOk src_elems dest_elems shape f r1 r2 dest_offset src_offset = do
          let (num_arrays, size_x, size_y) = getSizes shape f r1 r2
          return (dest_offset, src_offset,
                  num_arrays, size_x, size_y,
                  src_elems, dest_elems)

        getSizes shape f r1 r2 =
          let (mapped, notmapped) = splitAt r1 shape
              (pretrans, posttrans) = f $ splitAt r2 notmapped
          in (product mapped, product pretrans, product posttrans)

writeParamToLocalMemory :: Typed (MemBound u) =>
                           Imp.Exp -> (VName, t) -> Param (MemBound u)
                        -> ImpGen.ImpM lore op ()
writeParamToLocalMemory i (mem, _) param
  | Prim t <- paramType param =
      ImpGen.emit $
      Imp.Write mem (bytes i') bt (Space "local") Imp.Volatile $
      Imp.var (paramName param) t
  | otherwise =
      return ()
  where i' = i * Imp.LeafExp (Imp.SizeOf bt) int32
        bt = elemType $ paramType param

readParamFromLocalMemory :: Typed (MemBound u) =>
                            VName -> Imp.Exp -> Param (MemBound u) -> (VName, t)
                         -> ImpGen.ImpM lore op ()
readParamFromLocalMemory index i param (l_mem, _)
  | Prim _ <- paramType param =
      paramName param <--
      Imp.index l_mem (bytes i') bt (Space "local") Imp.Volatile
  | otherwise = index <-- i
  where i' = i * Imp.LeafExp (Imp.SizeOf bt) int32
        bt = elemType $ paramType param

computeThreadChunkSize :: SplitOrdering
                       -> Imp.Exp
                       -> Imp.Count Imp.Elements
                       -> Imp.Count Imp.Elements
                       -> VName
                       -> ImpGen.ImpM lore op ()
computeThreadChunkSize (SplitStrided stride) thread_index elements_per_thread num_elements chunk_var = do
  stride' <- ImpGen.compileSubExp stride
  chunk_var <--
    Imp.BinOpExp (SMin Int32)
    (Imp.innerExp elements_per_thread)
    ((Imp.innerExp num_elements - thread_index) `quotRoundingUp` stride')

computeThreadChunkSize SplitContiguous thread_index elements_per_thread num_elements chunk_var = do
  starting_point <- dPrimV "starting_point" $
    thread_index * Imp.innerExp elements_per_thread
  remaining_elements <- dPrimV "remaining_elements" $
    Imp.innerExp num_elements - Imp.var starting_point int32

  let no_remaining_elements = Imp.var remaining_elements int32 .<=. 0
      beyond_bounds = Imp.innerExp num_elements .<=. Imp.var starting_point int32

  sIf (no_remaining_elements .||. beyond_bounds)
    (chunk_var <-- 0)
    (sIf is_last_thread
       (chunk_var <-- Imp.innerExp last_thread_elements)
       (chunk_var <-- Imp.innerExp elements_per_thread))
  where last_thread_elements =
          num_elements - Imp.elements thread_index * elements_per_thread
        is_last_thread =
          Imp.innerExp num_elements .<.
          (thread_index + 1) * Imp.innerExp elements_per_thread

inBlockScan :: Imp.Exp
           -> Imp.Exp
           -> Imp.Exp
           -> VName
           -> [(VName, t)]
           -> Lambda InKernel
           -> InKernelGen ()
inBlockScan lockstep_width block_size active local_id acc_local_mem scan_lam = ImpGen.everythingVolatile $ do
  skip_threads <- dPrim "skip_threads" int32
  let in_block_thread_active =
        Imp.var skip_threads int32 .<=. in_block_id
      (scan_lam_i, other_index_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams scan_lam
      (x_params, y_params) =
        splitAt (length actual_params `div` 2) actual_params
      read_operands =
        zipWithM_ (readParamFromLocalMemory (paramName other_index_param) $
                   Imp.var local_id int32 - Imp.var skip_threads int32)
        x_params acc_local_mem

  -- Set initial y values
  sWhen active $
    zipWithM_ (readParamFromLocalMemory scan_lam_i $ Imp.var local_id int32)
    y_params acc_local_mem

  let op_to_y = ImpGen.compileBody' y_params $ lambdaBody scan_lam
      write_operation_result =
        zipWithM_ (writeParamToLocalMemory $ Imp.var local_id int32)
        acc_local_mem y_params
      maybeBarrier = sWhen (lockstep_width .<=. Imp.var skip_threads int32) $
                     sOp Imp.Barrier

  sComment "in-block scan (hopefully no barriers needed)" $ do
    skip_threads <-- 1
    sWhile (Imp.var skip_threads int32 .<. block_size) $ do
      sWhen (in_block_thread_active .&&. active) $ do
        sComment "read operands" read_operands
        sComment "perform operation" op_to_y

      maybeBarrier

      sWhen (in_block_thread_active .&&. active) $
        sComment "write result" write_operation_result

      maybeBarrier

      skip_threads <-- Imp.var skip_threads int32 * 2

  where block_id = Imp.var local_id int32 `quot` block_size
        in_block_id = Imp.var local_id int32 - block_id * block_size

data KernelConstants = KernelConstants
                       { kernelGlobalThreadId :: VName
                       , kernelLocalThreadId :: VName
                       , kernelGroupId :: VName
                       , kernelGroupSize :: Imp.DimSize
                       , _kernelNumThreads :: Imp.DimSize
                       , kernelWaveSize :: Imp.DimSize
                       , kernelDimensions :: [(VName, Imp.Exp)]
                       , kernelThreadActive :: Imp.Exp
                       , kernelStreamed :: [(VName, Imp.DimSize)]
                       -- ^ Chunk sizez and their maximum size.  Hint
                       -- for unrolling.
                       }

-- FIXME: wing a KernelConstants structure for use in Replicate
-- compilation.  This cannot be the best way to do this...
simpleKernelConstants :: MonadFreshNames m =>
                         Maybe Int -> String
                      -> m KernelConstants
simpleKernelConstants tag desc = do
  thread_gtid <- maybe (newVName $ desc ++ "_gtid")
                       (return . VName (nameFromString $ desc ++ "_gtid")) tag
  thread_ltid <- newVName $ desc ++ "_ltid"
  thread_gid <- newVName $ desc ++ "_gid"
  return $ KernelConstants
    thread_gtid thread_ltid thread_gid
    (Imp.ConstSize 0) (Imp.ConstSize 0) (Imp.ConstSize 0)
    [] (Imp.ValueExp $ BoolValue True) mempty

compileKernelBody :: Pattern InKernel
                  -> KernelConstants
                  -> KernelBody InKernel
                  -> InKernelGen ()
compileKernelBody pat constants kbody =
  compileKernelStms constants (stmsToList $ kernelBodyStms kbody) $
  zipWithM_ (compileKernelResult constants) (patternElements pat) $
  kernelBodyResult kbody

compileKernelStms :: KernelConstants -> [Stm InKernel]
                  -> InKernelGen a
                  -> InKernelGen a
compileKernelStms constants ungrouped_bnds m =
  compileGroupedKernelStms' $ groupStmsByGuard constants ungrouped_bnds
  where compileGroupedKernelStms' [] = m
        compileGroupedKernelStms' ((g, bnds):rest_bnds) = do
          ImpGen.dScopes (map ((Just . stmExp) &&& (castScope . scopeOf)) bnds)
          protect g $ mapM_ compileKernelStm bnds
          compileGroupedKernelStms' rest_bnds

        protect Nothing body_m =
          body_m
        protect (Just (Imp.ValueExp (BoolValue True))) body_m =
          body_m
        protect (Just g) body_m =
          sWhen g $ allThreads constants body_m

        compileKernelStm (Let pat _ e) = ImpGen.compileExp pat e

groupStmsByGuard :: KernelConstants
                     -> [Stm InKernel]
                     -> [(Maybe Imp.Exp, [Stm InKernel])]
groupStmsByGuard constants bnds =
  map collapse $ groupBy sameGuard $ zip (map bindingGuard bnds) bnds
  where bindingGuard (Let _ _ Op{}) = Nothing
        bindingGuard _ = Just $ kernelThreadActive constants

        sameGuard (g1, _) (g2, _) = g1 == g2

        collapse [] =
          (Nothing, [])
        collapse l@((g,_):_) =
          (g, map snd l)

compileKernelExp :: KernelConstants -> Pattern InKernel -> KernelExp InKernel
                 -> InKernelGen ()

compileKernelExp _ pat (Barrier ses) = do
  forM_ (zip (patternNames pat) ses) $ \(d, se) ->
    ImpGen.copyDWIM d [] se []
  sOp Imp.Barrier

compileKernelExp _ (Pattern [] [size]) (SplitSpace o w i elems_per_thread) = do
  num_elements <- Imp.elements <$> ImpGen.compileSubExp w
  i' <- ImpGen.compileSubExp i
  elems_per_thread' <- Imp.elements <$> ImpGen.compileSubExp elems_per_thread
  computeThreadChunkSize o i' elems_per_thread' num_elements (patElemName size)

compileKernelExp constants pat (Combine (CombineSpace scatter cspace) _ aspace body) = do
  -- First we compute how many times we have to iterate to cover
  -- cspace with our group size.  It is a fairly common case that
  -- we statically know that this requires 1 iteration, so we
  -- could detect it and not generate a loop in that case.
  -- However, it seems to have no impact on performance (an extra
  -- conditional jump), so for simplicity we just always generate
  -- the loop.
  let cspace_dims = map (streamBounded . snd) cspace
      num_iters
        | cspace_dims == [Imp.sizeToExp $ kernelGroupSize constants] = 1
        | otherwise = product cspace_dims `quotRoundingUp`
                      Imp.sizeToExp (kernelGroupSize constants)

  iter <- newVName "comb_iter"

  sFor iter Int32 num_iters $ do
    mapM_ ((`dPrim_` int32) . fst) cspace
    -- Compute the *flat* array index.
    cid <- dPrimV "flat_comb_id" $
      Imp.var iter int32 * Imp.sizeToExp (kernelGroupSize constants) +
      Imp.var (kernelLocalThreadId constants) int32

    -- Turn it into a nested array index.
    zipWithM_ (<--) (map fst cspace) $ unflattenIndex cspace_dims (Imp.var cid int32)

    -- Construct the body.  This is mostly about the book-keeping
    -- for the scatter-like part.
    let (scatter_ws, scatter_ns, _scatter_vs) = unzip3 scatter
        scatter_ws_repl = concat $ zipWith replicate scatter_ns scatter_ws
        (scatter_pes, normal_pes) =
          splitAt (sum scatter_ns) $ patternElements pat
        (res_is, res_vs, res_normal) =
          splitAt3 (sum scatter_ns) (sum scatter_ns) $ bodyResult body

    -- Execute the body if we are within bounds.
    sWhen (isActive cspace .&&. isActive aspace) $ allThreads constants $
      ImpGen.compileStms (freeIn $ bodyResult body) (stmsToList $ bodyStms body) $ do

      forM_ (zip4 scatter_ws_repl res_is res_vs scatter_pes) $
        \(w, res_i, res_v, scatter_pe) -> do
          let res_i' = ImpGen.compileSubExpOfType int32 res_i
              w'     = ImpGen.compileSubExpOfType int32 w
              -- We have to check that 'res_i' is in-bounds wrt. an array of size 'w'.
              in_bounds = 0 .<=. res_i' .&&. res_i' .<. w'
          sWhen in_bounds $ ImpGen.copyDWIM (patElemName scatter_pe) [res_i'] res_v []

      forM_ (zip normal_pes res_normal) $ \(pe, res) ->
        ImpGen.copyDWIM (patElemName pe) local_index res []

  sOp Imp.Barrier

  where streamBounded (Var v)
          | Just x <- lookup v $ kernelStreamed constants =
              Imp.sizeToExp x
        streamBounded se = ImpGen.compileSubExpOfType int32 se

        local_index = map (ImpGen.compileSubExpOfType int32 . Var . fst) cspace

compileKernelExp constants (Pattern _ dests) (GroupReduce w lam input) = do
  groupReduce constants w lam $ map snd input
  let (reduce_acc_params, _) =
        splitAt (length input) $ drop 2 $ lambdaParams lam
  forM_ (zip dests reduce_acc_params) $ \(dest, reduce_acc_param) ->
    ImpGen.copyDWIM (patElemName dest) [] (Var $ paramName reduce_acc_param) []

compileKernelExp constants _ (GroupScan w lam input) = do
  renamed_lam <- renameLambda lam
  w' <- ImpGen.compileSubExp w

  when (any (not . primType . paramType) $ lambdaParams lam) $
    compilerLimitationS "Cannot compile parallel scans with array element type."

  let local_tid = kernelLocalThreadId constants
      (_nes, arrs) = unzip input
      (lam_i, other_index_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (x_params, y_params) =
        splitAt (length input) actual_params

  ImpGen.dLParams (lambdaParams lam++lambdaParams renamed_lam)
  lam_i <-- Imp.var local_tid int32

  acc_local_mem <- flip zip (repeat ()) <$>
                   mapM (fmap (ImpGen.memLocationName . ImpGen.entryArrayLocation) .
                         ImpGen.lookupArray) arrs

  -- The scan works by splitting the group into blocks, which are
  -- scanned separately.  Typically, these blocks are smaller than
  -- the lockstep width, which enables barrier-free execution inside
  -- them.
  --
  -- We hardcode the block size here.  The only requirement is that
  -- it should not be less than the square root of the group size.
  -- With 32, we will work on groups of size 1024 or smaller, which
  -- fits every device Troels has seen.  Still, it would be nicer if
  -- it were a runtime parameter.  Some day.
  let block_size = Imp.ValueExp $ IntValue $ Int32Value 32
      simd_width = Imp.sizeToExp $ kernelWaveSize constants
      block_id = Imp.var local_tid int32 `quot` block_size
      in_block_id = Imp.var local_tid int32 - block_id * block_size
      doInBlockScan active = inBlockScan simd_width block_size active local_tid acc_local_mem
      lid_in_bounds = Imp.var local_tid int32 .<. w'

  doInBlockScan lid_in_bounds lam
  sOp Imp.Barrier

  let last_in_block = in_block_id .==. block_size - 1
  sComment "last thread of block 'i' writes its result to offset 'i'" $
    sWhen (last_in_block .&&. lid_in_bounds) $
    zipWithM_ (writeParamToLocalMemory block_id) acc_local_mem y_params

  sOp Imp.Barrier

  let is_first_block = block_id .==. 0
  ImpGen.comment
    "scan the first block, after which offset 'i' contains carry-in for warp 'i+1'" $
    doInBlockScan (is_first_block .&&. lid_in_bounds) renamed_lam

  sOp Imp.Barrier

  let read_carry_in =
        zipWithM_ (readParamFromLocalMemory
                   (paramName other_index_param) (block_id - 1))
        x_params acc_local_mem

  let op_to_y =
        ImpGen.compileBody' y_params $ lambdaBody lam
      write_final_result =
        zipWithM_ (writeParamToLocalMemory $ Imp.var local_tid int32) acc_local_mem y_params

  sComment "carry-in for every block except the first" $
    sUnless (is_first_block .||. Imp.UnOpExp Not lid_in_bounds) $ do
    sComment "read operands" read_carry_in
    sComment "perform operation" op_to_y
    sComment "write final result" write_final_result

  sOp Imp.Barrier

  sComment "restore correct values for first block" $
    sWhen is_first_block write_final_result

compileKernelExp constants (Pattern _ final) (GroupStream w maxchunk lam accs _arrs) = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
      block_offset' = Imp.var block_offset int32
  w' <- ImpGen.compileSubExp w
  max_block_size <- ImpGen.compileSubExp maxchunk

  ImpGen.dLParams (acc_params++arr_params)
  zipWithM_ ImpGen.compileSubExpTo (map paramName acc_params) accs
  dPrim_ block_size int32

  -- If the GroupStream is morally just a do-loop, generate simpler code.
  case mapM isSimpleThreadInSpace $ stmsToList $ bodyStms body of
    Just stms' | ValueExp x <- max_block_size, oneIsh x -> do
      let body' = body { bodyStms = stmsFromList stms' }
          body'' = allThreads constants $
                   ImpGen.compileLoopBody (map paramName acc_params) body'
      block_size <-- 1

      -- Check if loop is candidate for unrolling.
      let loop =
            case w of
              Var w_var | Just w_bound <- lookup w_var $ kernelStreamed constants,
                          w_bound /= Imp.ConstSize 1 ->
                          -- Candidate for unrolling, so generate two loops.
                          sIf (w' .==. Imp.sizeToExp w_bound)
                          (sFor block_offset Int32 (Imp.sizeToExp w_bound) body'')
                          (sFor block_offset Int32 w' body'')
              _ -> sFor block_offset Int32 w' body''

      if kernelThreadActive constants == Imp.ValueExp (BoolValue True)
        then loop
        else sWhen (kernelThreadActive constants) loop

    _ -> do
      dPrim_ block_offset int32
      let body' = streaming constants block_size maxchunk $
                  ImpGen.compileBody' acc_params body

      block_offset <-- 0

      let not_at_end = block_offset' .<. w'
          set_block_size =
            sIf (w' - block_offset' .<. max_block_size)
            (block_size <-- (w' - block_offset'))
            (block_size <-- max_block_size)
          increase_offset =
            block_offset <-- block_offset' + max_block_size

      -- Three cases to consider for simpler generated code based
      -- on max block size: (0) if full input size, do not
      -- generate a loop; (1) if one, generate for-loop (2)
      -- otherwise, generate chunked while-loop.
      if max_block_size == w' then
        (block_size <-- w') >> body'
      else if max_block_size == Imp.ValueExp (value (1::Int32)) then do
             block_size <-- w'
             sFor block_offset Int32 w' body'
           else
             sWhile not_at_end $
             set_block_size >> body' >> increase_offset

  forM_ (zip final acc_params) $ \(pe, p) ->
    ImpGen.copyDWIM (patElemName pe) [] (Var $ paramName p) []

  where isSimpleThreadInSpace (Let _ _ Op{}) = Nothing
        isSimpleThreadInSpace bnd = Just bnd

compileKernelExp _ _ (GroupGenReduce w arrs op bucket values locks) = do
  -- Check if bucket is in-bounds
  bucket' <- mapM ImpGen.compileSubExp bucket
  w' <- mapM ImpGen.compileSubExp w
  sWhen (indexInBounds bucket' w') $
    atomicUpdate arrs bucket op values locking
  where indexInBounds inds bounds =
          foldl1 (.&&.) $ zipWith checkBound inds bounds
          where checkBound ind bound = 0 .<=. ind .&&. ind .<. bound
        locking = Locking locks 0 1 0

compileKernelExp _ dest e =
  compilerBugS $ unlines ["Invalid target", "  " ++ show dest,
                          "for kernel expression", "  " ++ pretty e]

-- | Locking strategy used for an atomic update.
data Locking = Locking { lockingArray :: VName -- ^ Array containing the lock.
                       , lockingIsUnlocked :: Imp.Exp -- ^ Value for us to consider the lock free.
                       , lockingToLock :: Imp.Exp -- ^ What to write when we lock it.
                       , lockingToUnlock :: Imp.Exp -- ^ What to write when we unlock it.
                       }

groupReduce :: ExplicitMemorish lore =>
               KernelConstants
            -> SubExp
            -> Lambda lore
            -> [VName]
            -> ImpGen.ImpM lore Imp.KernelOp ()
groupReduce constants w lam arrs = do
  w' <- ImpGen.compileSubExp w

  let local_tid = kernelLocalThreadId constants
      (reduce_i, reduce_j_param, actual_reduce_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (reduce_acc_params, reduce_arr_params) =
        splitAt (length arrs) actual_reduce_params
      reduce_j = paramName reduce_j_param

  offset <- dPrim "offset" int32

  skip_waves <- dPrim "skip_waves" int32
  ImpGen.dLParams $ lambdaParams lam

  reduce_i <-- Imp.var local_tid int32

  let setOffset x = do
        offset <-- x
        reduce_j <-- Imp.var local_tid int32 + Imp.var offset int32

  setOffset 0

  sWhen (Imp.var local_tid int32 .<. w') $
    zipWithM_ (readReduceArgument offset) reduce_acc_params arrs

  let read_reduce_args = zipWithM_ (readReduceArgument offset)
                         reduce_arr_params arrs
      do_reduce = do ImpGen.comment "read array element" read_reduce_args
                     ImpGen.compileBody' reduce_acc_params $ lambdaBody lam
                     zipWithM_ (writeReduceOpResult local_tid)
                       reduce_acc_params arrs
      in_wave_reduce = ImpGen.everythingVolatile do_reduce

      wave_size = Imp.sizeToExp $ kernelWaveSize constants
      group_size = Imp.sizeToExp $ kernelGroupSize constants
      wave_id = Imp.var local_tid int32 `quot` wave_size
      in_wave_id = Imp.var local_tid int32 - wave_id * wave_size
      num_waves = (group_size + wave_size - 1) `quot` wave_size
      arg_in_bounds = Imp.var reduce_j int32 .<. w'

      doing_in_wave_reductions =
        Imp.var offset int32 .<. wave_size
      apply_in_in_wave_iteration =
        (in_wave_id .&. (2 * Imp.var offset int32 - 1)) .==. 0
      in_wave_reductions = do
        setOffset 1
        sWhile doing_in_wave_reductions $ do
          sWhen (arg_in_bounds .&&. apply_in_in_wave_iteration)
            in_wave_reduce
          setOffset $ Imp.var offset int32 * 2

      doing_cross_wave_reductions =
        Imp.var skip_waves int32 .<. num_waves
      is_first_thread_in_wave =
        in_wave_id .==. 0
      wave_not_skipped =
        (wave_id .&. (2 * Imp.var skip_waves int32 - 1)) .==. 0
      apply_in_cross_wave_iteration =
        arg_in_bounds .&&. is_first_thread_in_wave .&&. wave_not_skipped
      cross_wave_reductions = do
        skip_waves <-- 1
        sWhile doing_cross_wave_reductions $ do
          sOp Imp.Barrier
          setOffset (Imp.var skip_waves int32 * wave_size)
          sWhen apply_in_cross_wave_iteration
            do_reduce
          skip_waves <-- Imp.var skip_waves int32 * 2

  in_wave_reductions
  cross_wave_reductions
  where readReduceArgument offset param arr
          | Prim _ <- paramType param =
              ImpGen.copyDWIM (paramName param) [] (Var arr) [i]
          | otherwise =
              return ()
          where i = ImpGen.varIndex (kernelLocalThreadId constants) + ImpGen.varIndex offset

        writeReduceOpResult i param arr
          | Prim _ <- paramType param =
              ImpGen.copyDWIM arr [ImpGen.varIndex i] (Var $ paramName param) []
          | otherwise =
              return ()

atomicUpdate :: ExplicitMemorish lore =>
                [VName] -> [SubExp] -> Lambda lore -> [SubExp] -> Locking
             -> ImpGen.ImpM lore Imp.KernelOp ()
atomicUpdate [a] bucket op [v] _
  | [Prim t] <- lambdaReturnType op,
    primBitSize t == 32 = do
  -- If we have only one array and one non-array value (this is a
  -- one-to-one correspondance) then we need only one
  -- update. If operator has an atomic implementation we use
  -- that, otherwise it is still a binary operator which can
  -- be implemented by atomic compare-and-swap if 32 bits.

  -- Common variables.
  old <- dPrim "old" t
  bucket' <- mapM ImpGen.compileSubExp bucket

  (arr', _a_space, bucket_offset) <- ImpGen.fullyIndexArray a bucket'

  val' <- ImpGen.compileSubExp v
  case opHasAtomicSupport old arr' bucket_offset op of
    Just f -> sOp $ f val'

    Nothing -> do
      -- Code generation target:
      --
      -- old = d_his[idx];
      -- do {
      --   assumed = old;
      --   tmp = OP::apply(val, assumed);
      --   old = atomicCAS(&d_his[idx], assumed, tmp);
      -- } while(assumed != old);
      assumed <- dPrim "assumed" t
      run_loop <- dPrimV "run_loop" true
      ImpGen.copyDWIM old [] (Var a) bucket'

        -- Preparing parameters
      let (acc_p:arr_p:_) = lambdaParams op

      -- Critical section
      ImpGen.dLParams $ lambdaParams op

      -- While-loop: Try to insert your value
      let (toBits, fromBits) =
            case t of FloatType Float32 -> (\x -> Imp.FunExp "to_bits32" [x] int32,
                                            \x -> Imp.FunExp "from_bits32" [x] t)
                      _                 -> (id, id)
      sWhile (Imp.var run_loop Bool) $ do
        assumed <-- Imp.var old t
        paramName acc_p <-- val'
        paramName arr_p <-- Imp.var assumed t
        ImpGen.compileBody' [acc_p] $ lambdaBody op
        old_bits <- dPrim "old_bits" int32
        sOp $ Imp.Atomic $
          Imp.AtomicCmpXchg old_bits arr' bucket_offset
          (toBits (Imp.var assumed int32)) (toBits (Imp.var (paramName acc_p) int32))
        old <-- fromBits (Imp.var old_bits int32)
        sWhen (toBits (Imp.var assumed t) .==. Imp.var old_bits int32)
          (run_loop <-- false)
  where opHasAtomicSupport old arr' bucket' lam = do
          let atomic f = Imp.Atomic . f old arr' bucket'
          [BasicOp (BinOp bop _ _)] <-
            Just $ map stmExp $ stmsToList $ bodyStms $ lambdaBody lam
          atomic <$> Imp.atomicBinOp bop

atomicUpdate arrs bucket op values locking = do
  old <- dPrim "old" int32
  loop_done <- dPrimV "loop_done" 0

  -- Check if bucket is in-bounds
  bucket' <- mapM ImpGen.compileSubExp bucket

  -- Correctly index into locks.
  (locks', _locks_space, locks_offset) <-
    ImpGen.fullyIndexArray (lockingArray locking) bucket'

  -- Preparing parameters
  let (acc_params, arr_params) =
        splitAt (length values) $ lambdaParams op

  -- Critical section
  let try_acquire_lock =
        sOp $ Imp.Atomic $
        Imp.AtomicCmpXchg old locks' locks_offset (lockingIsUnlocked locking) (lockingToLock locking)
      lock_acquired = Imp.var old int32 .==. lockingIsUnlocked locking
      loop_cond = Imp.var loop_done int32 .==. 0
      release_lock = ImpGen.everythingVolatile $
                     ImpGen.sWrite (lockingArray locking) bucket' $ lockingToUnlock locking
      break_loop = loop_done <-- 1

  -- We copy the current value and the new value to the parameters
  -- unless they are array-typed.  If they are arrays, then the
  -- index functions should already be set up correctly, so there is
  -- nothing more to do.
  let bind_acc_params =
        forM_ (zip acc_params arrs) $ \(acc_p, arr) ->
        when (primType (paramType acc_p)) $
        ImpGen.copyDWIM (paramName acc_p) [] (Var arr) bucket'

  let bind_arr_params =
        forM_ (zip arr_params values) $ \(arr_p, val) ->
        when (primType (paramType arr_p)) $
        ImpGen.copyDWIM (paramName arr_p) [] val []

  let op_body = ImpGen.compileBody' acc_params $ lambdaBody op

      do_gen_reduce = zipWithM_ (writeArray bucket') arrs $ map (Var . paramName) acc_params

  -- While-loop: Try to insert your value
  sWhile loop_cond $ do
    try_acquire_lock
    sWhen lock_acquired $ do
      ImpGen.dLParams $ lambdaParams op
      bind_acc_params
      bind_arr_params
      op_body
      do_gen_reduce
      release_lock
      break_loop
    sOp Imp.MemFence
  where writeArray bucket' arr val =
          ImpGen.copyDWIM arr bucket' val []

allThreads :: KernelConstants -> InKernelGen () -> InKernelGen ()
allThreads constants = ImpGen.emit <=< ImpGen.subImpM_ (inKernelOperations constants')
  where constants' =
          constants { kernelThreadActive = Imp.ValueExp (BoolValue True) }

streaming :: KernelConstants -> VName -> SubExp -> InKernelGen () -> InKernelGen ()
streaming constants chunksize bound m = do
  bound' <- ImpGen.subExpToDimSize bound
  let constants' =
        constants { kernelStreamed = (chunksize, bound') : kernelStreamed constants }
  ImpGen.emit =<< ImpGen.subImpM_ (inKernelOperations constants') m

compileKernelResult :: KernelConstants -> PatElem InKernel -> KernelResult
                    -> InKernelGen ()

compileKernelResult constants pe (ThreadsReturn OneResultPerGroup what) = do
  i <- newVName "i"

  in_local_memory <- arrayInLocalMemory what
  let me = Imp.var (kernelLocalThreadId constants) int32

  if not in_local_memory then do
    who' <- ImpGen.compileSubExp $ intConst Int32 0
    sWhen (me .==. who') $
      ImpGen.copyDWIM (patElemName pe) [ImpGen.varIndex $ kernelGroupId constants] what []
    else do
      -- If the result of the group is an array in local memory, we
      -- store it by collective copying among all the threads of the
      -- group.  TODO: also do this if the array is in global memory
      -- (but this is a bit more tricky, synchronisation-wise).
      --
      -- We do the reads/writes multidimensionally, but the loop is
      -- single-dimensional.
      ws <- mapM ImpGen.compileSubExp . arrayDims =<< subExpType what
      -- Compute how many elements this thread is responsible for.
      -- Formula: (w - ltid) / group_size (rounded up).
      let w = product ws
          ltid = ImpGen.varIndex (kernelLocalThreadId constants)
          group_size = Imp.sizeToExp (kernelGroupSize constants)
          to_write = (w - ltid) `quotRoundingUp` group_size
          is = unflattenIndex ws $ ImpGen.varIndex i * group_size + ltid

      sFor i Int32 to_write $
        ImpGen.copyDWIM (patElemName pe) (ImpGen.varIndex (kernelGroupId constants) : is) what is

compileKernelResult constants pe (ThreadsReturn AllThreads what) =
  ImpGen.copyDWIM (patElemName pe) [ImpGen.varIndex $ kernelGlobalThreadId constants] what []

compileKernelResult constants pe (ThreadsReturn (ThreadsPerGroup limit) what) =
  sWhen (isActive limit) $
  ImpGen.copyDWIM (patElemName pe) [ImpGen.varIndex $ kernelGroupId constants] what []

compileKernelResult constants pe (ThreadsReturn ThreadsInSpace what) = do
  let is = map (ImpGen.varIndex . fst) $ kernelDimensions constants
  sWhen (kernelThreadActive constants) $ ImpGen.copyDWIM (patElemName pe) is what []

compileKernelResult constants pe (ConcatReturns SplitContiguous _ per_thread_elems moffset what) = do
  dest_loc <- ImpGen.entryArrayLocation <$> ImpGen.lookupArray (patElemName pe)
  let dest_loc_offset = ImpGen.offsetArray dest_loc offset
      dest' = ImpGen.arrayDestination dest_loc_offset
  ImpGen.copyDWIMDest dest' [] (Var what) []
  where offset = case moffset of
                   Nothing -> ImpGen.compileSubExpOfType int32 per_thread_elems *
                              ImpGen.varIndex (kernelGlobalThreadId constants)
                   Just se -> ImpGen.compileSubExpOfType int32 se

compileKernelResult constants pe (ConcatReturns (SplitStrided stride) _ _ moffset what) = do
  dest_loc <- ImpGen.entryArrayLocation <$> ImpGen.lookupArray (patElemName pe)
  let dest_loc' = ImpGen.strideArray
                  (ImpGen.offsetArray dest_loc offset) $
                  ImpGen.compileSubExpOfType int32 stride
      dest' = ImpGen.arrayDestination dest_loc'
  ImpGen.copyDWIMDest dest' [] (Var what) []
  where offset = case moffset of
                   Nothing -> ImpGen.varIndex (kernelGlobalThreadId constants)
                   Just se -> ImpGen.compileSubExpOfType int32 se

compileKernelResult constants pe (WriteReturn rws _arr dests) = do
  rws' <- mapM ImpGen.compileSubExp rws
  forM_ dests $ \(is, e) -> do
    is' <- mapM ImpGen.compileSubExp is
    let condInBounds i rw = 0 .<=. i .&&. i .<. rw
        write = foldl (.&&.) (kernelThreadActive constants) $
                zipWith condInBounds is' rws'
    sWhen write $ ImpGen.copyDWIM (patElemName pe) (map (ImpGen.compileSubExpOfType int32) is) e []

compileKernelResult _ _ KernelInPlaceReturn{} =
  -- Already in its place... said it was a hack.
  return ()

isActive :: [(VName, SubExp)] -> Imp.Exp
isActive limit = case actives of
                    [] -> Imp.ValueExp $ BoolValue True
                    x:xs -> foldl (.&&.) x xs
  where (is, ws) = unzip limit
        actives = zipWith active is $ map (ImpGen.compileSubExpOfType Bool) ws
        active i = (Imp.var i int32 .<.)

setSpaceIndices :: KernelSpace -> InKernelGen ()
setSpaceIndices space =
  case spaceStructure space of
    FlatThreadSpace is_and_dims ->
      flatSpaceWith gtid is_and_dims
    NestedThreadSpace is_and_dims -> do
      let (gtids, gdims, ltids, ldims) = unzip4 is_and_dims
      gdims' <- mapM ImpGen.compileSubExp gdims
      ldims' <- mapM ImpGen.compileSubExp ldims
      let (gtid_es, ltid_es) = unzip $ unflattenNestedIndex gdims' ldims' gtid
      zipWithM_ (<--) gtids gtid_es
      zipWithM_ (<--) ltids ltid_es
  where gtid = Imp.var (spaceGlobalId space) int32

        flatSpaceWith base is_and_dims = do
          let (is, dims) = unzip is_and_dims
          dims' <- mapM ImpGen.compileSubExp dims
          let index_expressions = unflattenIndex dims' base
          zipWithM_ (<--) is index_expressions

unflattenNestedIndex :: IntegralExp num => [num] -> [num] -> num -> [(num,num)]
unflattenNestedIndex global_dims group_dims global_id =
  zip global_is local_is
  where num_groups_dims = zipWith quotRoundingUp global_dims group_dims
        group_size = product group_dims
        group_id = global_id `Futhark.Util.IntegralExp.quot` group_size
        local_id = global_id `Futhark.Util.IntegralExp.rem` group_size

        group_is = unflattenIndex num_groups_dims group_id
        local_is = unflattenIndex group_dims local_id
        global_is = zipWith (+) local_is $ zipWith (*) group_is group_dims

arrayInLocalMemory :: SubExp -> InKernelGen Bool
arrayInLocalMemory (Var name) = do
  res <- ImpGen.lookupVar name
  case res of
    ImpGen.ArrayVar _ entry ->
      (Space "local"==) . ImpGen.entryMemSpace <$>
      ImpGen.lookupMemory (ImpGen.memLocationName (ImpGen.entryArrayLocation entry))
    _ -> return False
arrayInLocalMemory Constant{} = return False
