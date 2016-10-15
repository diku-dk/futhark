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
import Control.Applicative
import Data.Maybe
import Data.Monoid
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List

import Prelude hiding (quot)

import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Representation.ExplicitMemory hiding (kernelNumThreads)
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpCode.Kernels (bytes)
import qualified Futhark.CodeGen.ImpGen as ImpGen
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.CodeGen.SetDefaultSpace
import Futhark.Tools (partitionChunkedKernelLambdaParameters, fullSliceNum)
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem, IntegralExp)

type CallKernelGen = ImpGen.ImpM ExplicitMemory Imp.HostOp
type InKernelGen = ImpGen.ImpM InKernel Imp.KernelOp

callKernelOperations :: ImpGen.Operations ExplicitMemory Imp.HostOp
callKernelOperations =
  ImpGen.Operations { ImpGen.opsExpCompiler = expCompiler
                    , ImpGen.opsCopyCompiler = callKernelCopy
                    , ImpGen.opsOpCompiler = opCompiler
                    , ImpGen.opsBodyCompiler = ImpGen.defCompileBody
                    }

inKernelOperations :: KernelConstants -> ImpGen.Operations InKernel Imp.KernelOp
inKernelOperations constants = (ImpGen.defaultOperations $ compileInKernelOp constants)
                               { ImpGen.opsCopyCompiler = inKernelCopy
                               , ImpGen.opsExpCompiler = inKernelExpCompiler
                               , ImpGen.opsBodyCompiler = compileNestedKernelBody constants
                               }

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either String Imp.Program)
compileProg prog =
  fmap (setDefaultSpace (Imp.Space "device")) <$>
  ImpGen.compileProg callKernelOperations (Imp.Space "device") prog

opCompiler :: ImpGen.Destination -> Op ExplicitMemory
           -> CallKernelGen ()
opCompiler dest (Alloc e space) =
  ImpGen.compileAlloc dest e space
opCompiler dest (Inner kernel) =
  kernelCompiler dest kernel

compileInKernelOp :: KernelConstants -> ImpGen.Destination -> Op InKernel
                  -> InKernelGen ()
compileInKernelOp _ _ Alloc{} =
  throwError "Cannot allocate memory in kernel."
compileInKernelOp constants dest (Inner op) =
  compileKernelExp constants dest op

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: ImpGen.Destination -> Kernel InKernel
               -> CallKernelGen ()

kernelCompiler dest NumGroups = do
  [v] <- ImpGen.funcallTargets dest
  ImpGen.emit $ Imp.Op $ Imp.GetNumGroups v

kernelCompiler dest GroupSize = do
  [v] <- ImpGen.funcallTargets dest
  ImpGen.emit $ Imp.Op $ Imp.GetGroupSize v


kernelCompiler dest TileSize = do
  [v] <- ImpGen.funcallTargets dest
  ImpGen.emit $ Imp.Op $ Imp.GetTileSize v

kernelCompiler dest (SufficientParallelism se) = do
  [v] <- ImpGen.funcallTargets dest
  se' <- ImpGen.compileSubExp se
  ImpGen.emit $ Imp.SetScalar v $ Imp.CmpOpExp (CmpSlt Int32) (64*1024) se'

kernelCompiler dest (Kernel _ space _ kernel_body) = do

  num_groups' <- ImpGen.subExpToDimSize $ spaceNumGroups space
  group_size' <- ImpGen.subExpToDimSize $ spaceGroupSize space
  num_threads' <- ImpGen.subExpToDimSize $ spaceNumThreads space

  let bound_in_kernel =
        HM.keys $ mconcat $
        scopeOfKernelSpace space :
        map scopeOf (kernelBodyStms kernel_body)

  let global_tid = spaceGlobalId space
      local_tid = spaceLocalId space
      group_id = spaceGroupId space
  wave_size <- newVName "wave_size"
  inner_group_size <- newVName "group_size"
  thread_active <- newVName "thread_active"

  let (space_is, space_dims) = unzip $ spaceDimensions space
  space_dims' <- mapM ImpGen.compileSubExp space_dims
  let constants = KernelConstants global_tid local_tid group_id
                  (Imp.VarSize inner_group_size) num_threads'
                  (Imp.VarSize wave_size) (zip space_is space_dims')
                  (Imp.var thread_active Bool) mempty

  kernel_body' <-
    makeAllMemoryGlobal $
    ImpGen.subImpM_ (inKernelOperations constants) $
    ImpGen.declaringPrimVar wave_size int32 $
    ImpGen.declaringPrimVar inner_group_size int32 $
    ImpGen.declaringPrimVar thread_active Bool $
    ImpGen.declaringScope Nothing (scopeOfKernelSpace space) $ do

    ImpGen.emit $
      Imp.Op (Imp.GetGlobalId global_tid 0) <>
      Imp.Op (Imp.GetLocalId local_tid 0) <>
      Imp.Op (Imp.GetLocalSize inner_group_size 0) <>
      Imp.Op (Imp.GetLockstepWidth wave_size) <>
      Imp.Op (Imp.GetGroupId group_id 0)

    setSpaceIndices space

    ImpGen.emit $ Imp.SetScalar thread_active (isActive $ spaceDimensions space)

    compileKernelBody dest constants kernel_body

  (uses, local_memory) <- computeKernelUses kernel_body' bound_in_kernel

  ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.AnyKernel Imp.Kernel
            { Imp.kernelBody = kernel_body'
            , Imp.kernelLocalMemory = local_memory
            , Imp.kernelUses = uses
            , Imp.kernelNumGroups = num_groups'
            , Imp.kernelGroupSize = group_size'
            , Imp.kernelName = global_tid
            , Imp.kernelDesc = Nothing
            }

expCompiler :: ImpGen.ExpCompiler ExplicitMemory Imp.HostOp
-- We generate a simple kernel for itoa and replicate.
expCompiler
  (ImpGen.Destination
    [ImpGen.ArrayDestination (ImpGen.CopyIntoMemory destloc) _])
  (BasicOp (Iota n x s et)) = do
  thread_gid <- newVName "thread_gid"

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

    ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.Map Imp.MapKernel
      { Imp.mapKernelThreadNum = thread_gid
      , Imp.mapKernelDesc = "iota"
      , Imp.mapKernelNumGroups = Imp.VarSize num_groups
      , Imp.mapKernelGroupSize = Imp.VarSize group_size
      , Imp.mapKernelSize = n'
      , Imp.mapKernelUses = body_uses
      , Imp.mapKernelBody = body
      }
  return ImpGen.Done

expCompiler
  (ImpGen.Destination [dest]) (BasicOp (Replicate (Shape ds) se)) = do
  constants <- simpleKernelConstants "replicate"

  t <- subExpType se
  let thread_gid = kernelGlobalThreadId constants
      row_dims = arrayDims t
      dims = ds ++ row_dims
      is' = unflattenIndex (map (primExpFromSubExp int32) dims) $
            ImpGen.varIndex thread_gid
  ds' <- mapM ImpGen.compileSubExp ds

  makeAllMemoryGlobal $ do
    body <- ImpGen.subImpM_ (inKernelOperations constants) $
      ImpGen.copyDWIMDest dest is' se $ drop (length ds) is'

    dims' <- mapM ImpGen.compileSubExp dims
    (group_size, num_groups) <- computeMapKernelGroups $ product dims'

    (body_uses, _) <- computeKernelUses
                      (freeIn body <> freeIn ds')
                      [thread_gid]

    ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.Map Imp.MapKernel
      { Imp.mapKernelThreadNum = thread_gid
      , Imp.mapKernelDesc = "replicate"
      , Imp.mapKernelNumGroups = Imp.VarSize num_groups
      , Imp.mapKernelGroupSize = Imp.VarSize group_size
      , Imp.mapKernelSize = product dims'
      , Imp.mapKernelUses = body_uses
      , Imp.mapKernelBody = body
      }
  return ImpGen.Done

-- Allocation in the "local" space is just a placeholder.
expCompiler _ (Op (Alloc _ (Space "local"))) =
  return ImpGen.Done

expCompiler _ e =
  return $ ImpGen.CompileExp e

callKernelCopy :: ImpGen.CopyCompiler ExplicitMemory Imp.HostOp
callKernelCopy bt
  destloc@(ImpGen.MemLocation destmem destshape destIxFun)
  srcloc@(ImpGen.MemLocation srcmem srcshape srcIxFun)
  n
  | Just (destoffset, srcoffset,
          num_arrays, size_x, size_y,
          src_elems, dest_elems) <- isMapTransposeKernel bt destloc srcloc =
  ImpGen.emit $ Imp.Op $ Imp.CallKernel $
  Imp.MapTranspose bt
  destmem destoffset
  srcmem srcoffset
  num_arrays size_x size_y
  src_elems dest_elems

  | bt_size <- primByteSize bt,
    ixFunMatchesInnerShape
      (Shape $ map ImpGen.dimSizeToSubExp destshape) destIxFun,
    ixFunMatchesInnerShape
      (Shape $ map ImpGen.dimSizeToSubExp srcshape) srcIxFun,
    Just destoffset <-
      ImpGen.compilePrimExp <$>
      IxFun.linearWithOffset destIxFun bt_size,
    Just srcoffset  <-
      ImpGen.compilePrimExp <$>
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
      shape_se = map ImpGen.dimSizeToPrimExp srcshape
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
                  HS.singleton srcmem <>
                  freeIn destIxFun <> freeIn srcIxFun <> freeIn destshape

    let kernel_size = Imp.innerExp n * product (drop 1 shape)
    (group_size, num_groups) <- computeMapKernelGroups kernel_size

    let bound_in_kernel = [global_thread_index]
    (body_uses, _) <- computeKernelUses (kernel_size, body) bound_in_kernel

    ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.Map Imp.MapKernel
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
inKernelExpCompiler _ (BasicOp (Assert _ loc)) =
  fail $ "Cannot compile assertion at " ++ locStr loc ++ " inside parallel kernel."
inKernelExpCompiler _ e =
  return $ ImpGen.CompileExp e

computeKernelUses :: FreeIn a =>
                     a -> [VName]
                  -> CallKernelGen ([Imp.KernelUse], [Imp.LocalMemoryUse])
computeKernelUses kernel_body bound_in_kernel = do
    let actually_free = freeIn kernel_body `HS.difference` HS.fromList bound_in_kernel

    -- Compute the variables that we need to pass to the kernel.
    reads_from <- readsFromSet actually_free

    -- Are we using any local memory?
    local_memory <- computeLocalMemoryUse actually_free
    return (nub reads_from, nub local_memory)

readsFromSet :: Names -> CallKernelGen [Imp.KernelUse]
readsFromSet free =
  fmap catMaybes $
  forM (HS.toList free) $ \var -> do
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
  forM (HS.toList free) $ \var -> do
    t <- lookupType var
    case t of
      Mem memsize (Space "local") -> do
        memsize' <- localMemSize =<< ImpGen.subExpToDimSize memsize
        return $ Just (var, memsize')
      _ -> return Nothing

localMemSize :: Imp.MemSize -> CallKernelGen (Either Imp.MemSize Imp.KernelConstExp)
localMemSize (Imp.ConstSize x) =
  return $ Right $ ValueExp $ IntValue $ Int32Value x
localMemSize (Imp.VarSize v) = isConstExp v >>= \case
  Nothing -> return $ Left $ Imp.VarSize v
  Just e  -> return $ Right e

isConstExp :: VName -> CallKernelGen (Maybe Imp.KernelConstExp)
isConstExp v = do
  vtable <- asks ImpGen.envVtable
  let lookupConstExp name = constExp =<< hasExp =<< HM.lookup name vtable
      kernelConst (Op (Inner NumGroups)) = Just $ LeafExp Imp.NumGroupsConst int32
      kernelConst (Op (Inner GroupSize)) = Just $ LeafExp Imp.GroupSizeConst int32
      kernelConst (Op (Inner TileSize)) = Just $ LeafExp Imp.TileSizeConst int32
      kernelConst (BasicOp (SubExp (Var name))) = lookupConstExp name
      kernelConst _              = Nothing
      constExp = primExpFromExp kernelConst
  return $ lookupConstExp v
  where hasExp (ImpGen.ArrayVar e _) = e
        hasExp (ImpGen.ScalarVar e _) = e
        hasExp (ImpGen.MemVar e _) = e

-- | Change every memory block to be in the global address space,
-- except those who are in the local memory space.  This only affects
-- generated code - we still need to make sure that the memory is
-- actually present on the device (and declared as variables in the
-- kernel).
makeAllMemoryGlobal :: CallKernelGen a
                    -> CallKernelGen a
makeAllMemoryGlobal =
  local $ \env -> env { ImpGen.envVtable = HM.map globalMemory $ ImpGen.envVtable env
                      , ImpGen.envDefaultSpace = Imp.Space "global"
                      }
  where globalMemory (ImpGen.MemVar _ entry)
          | ImpGen.entryMemSpace entry /= Space "local" =
              ImpGen.MemVar Nothing entry { ImpGen.entryMemSpace = Imp.Space "global" }
        globalMemory entry =
          entry

computeMapKernelGroups :: Imp.Exp -> CallKernelGen (VName, VName)
computeMapKernelGroups kernel_size = do
  group_size <- newVName "group_size"
  num_groups <- newVName "num_groups"
  let group_size_var = Imp.var group_size int32
  ImpGen.emit $ Imp.DeclareScalar group_size int32
  ImpGen.emit $ Imp.DeclareScalar num_groups int32
  ImpGen.emit $ Imp.Op $ Imp.GetGroupSize group_size
  ImpGen.emit $ Imp.SetScalar num_groups $
    kernel_size `quotRoundingUp` Imp.ConvOpExp (SExt Int32 Int32) group_size_var
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
    destshape' <- map ImpGen.compilePrimExp destshape,
    srcshape' <- map ImpGen.compilePrimExp $ IxFun.shape srcIxFun,
    Just src_offset <- IxFun.linearWithOffset srcIxFun bt_size,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk (product srcshape') (product destshape') destshape' swap r1 r2 dest_offset src_offset
  | Just dest_offset <- IxFun.linearWithOffset destIxFun bt_size,
    Just (src_offset, perm_and_srcshape) <- IxFun.rearrangeWithOffset srcIxFun bt_size,
    (perm, srcshape) <- unzip perm_and_srcshape,
    srcshape' <- map ImpGen.compilePrimExp srcshape,
    destshape' <- map ImpGen.compilePrimExp $ IxFun.shape destIxFun,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk (product srcshape') (product destshape') srcshape' id r1 r2 dest_offset src_offset
  | otherwise =
    Nothing
  where bt_size = primByteSize bt
        swap (x,y) = (y,x)

        isOk src_elems dest_elems shape f r1 r2 dest_offset src_offset = do
          let dest_offset' = ImpGen.compilePrimExp dest_offset
              src_offset' = ImpGen.compilePrimExp src_offset
              (num_arrays, size_x, size_y) = getSizes shape f r1 r2
          return (dest_offset', src_offset',
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
      ImpGen.emit $
      Imp.SetScalar (paramName param) $
      Imp.index l_mem (bytes i') bt (Space "local") Imp.Volatile
  | otherwise =
      ImpGen.emit $
      Imp.SetScalar index i
  where i' = i * Imp.LeafExp (Imp.SizeOf bt) int32
        bt = elemType $ paramType param

computeThreadChunkSize :: Commutativity
                       -> Imp.Exp
                       -> Imp.Exp
                       -> Imp.Count Imp.Elements
                       -> Imp.Count Imp.Elements
                       -> VName
                       -> ImpGen.ImpM lore op ()
computeThreadChunkSize Commutative thread_index num_threads elements_per_thread num_elements chunk_var = do
  remaining_elements <- newVName "remaining_elements"
  ImpGen.emit $
    Imp.DeclareScalar remaining_elements int32
  ImpGen.emit $
    Imp.SetScalar remaining_elements $
    (Imp.innerExp num_elements - thread_index)
    `quotRoundingUp`
    num_threads
  ImpGen.emit $
    Imp.If (Imp.CmpOpExp (CmpSlt Int32)
            (Imp.innerExp elements_per_thread)
            (Imp.var remaining_elements int32))
    (Imp.SetScalar chunk_var (Imp.innerExp elements_per_thread))
    (Imp.SetScalar chunk_var (Imp.var remaining_elements int32))

computeThreadChunkSize Noncommutative thread_index _ elements_per_thread num_elements chunk_var = do
  starting_point <- newVName "starting_point"
  remaining_elements <- newVName "remaining_elements"

  ImpGen.emit $
    Imp.DeclareScalar starting_point int32
  ImpGen.emit $
    Imp.SetScalar starting_point $
    thread_index * Imp.innerExp elements_per_thread

  ImpGen.emit $
    Imp.DeclareScalar remaining_elements int32
  ImpGen.emit $
    Imp.SetScalar remaining_elements $
    Imp.innerExp num_elements - Imp.var starting_point int32

  let no_remaining_elements = Imp.CmpOpExp (CmpSle Int32)
                              (Imp.var remaining_elements int32) 0
      beyond_bounds = Imp.CmpOpExp (CmpSle Int32)
                      (Imp.innerExp num_elements)
                      (Imp.var starting_point int32)

  ImpGen.emit $
    Imp.If (Imp.BinOpExp LogOr no_remaining_elements beyond_bounds)
    (Imp.SetScalar chunk_var 0)
    (Imp.If is_last_thread
     (Imp.SetScalar chunk_var $ Imp.innerExp last_thread_elements)
     (Imp.SetScalar chunk_var $ Imp.innerExp elements_per_thread))
  where last_thread_elements =
          num_elements - Imp.elements thread_index * elements_per_thread
        is_last_thread =
          Imp.CmpOpExp (CmpSlt Int32)
          (Imp.innerExp num_elements)
          ((thread_index + 1) * Imp.innerExp elements_per_thread)

inWaveScan :: Imp.Exp
           -> VName
           -> [(VName, t)]
           -> Lambda InKernel
           -> InKernelGen ()
inWaveScan wave_size local_id acc_local_mem scan_lam = ImpGen.everythingVolatile $ do
  skip_threads <- newVName "skip_threads"
  let in_wave_thread_active =
        Imp.CmpOpExp (CmpSle Int32) (Imp.var skip_threads int32) in_wave_id
      (scan_lam_i, other_index_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams scan_lam
      (x_params, y_params) =
        splitAt (length actual_params `div` 2) actual_params
  read_operands <-
    ImpGen.collect $
    zipWithM_ (readParamFromLocalMemory (paramName other_index_param) $
               Imp.var local_id int32 - Imp.var skip_threads int32)
    x_params acc_local_mem
  scan_y_dest <- ImpGen.destinationFromParams y_params

  -- Set initial y values
  zipWithM_ (readParamFromLocalMemory scan_lam_i $ Imp.var local_id int32)
    y_params acc_local_mem

  op_to_y <- ImpGen.collect $ ImpGen.compileBody scan_y_dest $ lambdaBody scan_lam
  write_operation_result <-
    ImpGen.collect $
    zipWithM_ (writeParamToLocalMemory $ Imp.var local_id int32)
    acc_local_mem y_params
  ImpGen.emit $
    Imp.Comment "in-wave scan (no barriers needed)" $
    Imp.DeclareScalar skip_threads int32 <>
    Imp.SetScalar skip_threads 1 <>
    Imp.While (Imp.CmpOpExp (CmpSlt Int32) (Imp.var skip_threads int32) wave_size)
    (Imp.If in_wave_thread_active
     (Imp.Comment "read operands" read_operands <>
      Imp.Comment "perform operation" op_to_y <>
      Imp.Comment "write result" write_operation_result)
     mempty <>
     Imp.SetScalar skip_threads (Imp.var skip_threads int32 * 2))
  where wave_id = Imp.BinOpExp (SQuot Int32) (Imp.var local_id int32) wave_size
        in_wave_id = Imp.var local_id int32 - wave_id * wave_size

data KernelConstants = KernelConstants
                       { kernelGlobalThreadId :: VName
                       , kernelLocalThreadId :: VName
                       , kernelGroupId :: VName
                       , kernelGroupSize :: Imp.DimSize
                       , kernelNumThreads :: Imp.DimSize
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
                         String
                      -> m KernelConstants
simpleKernelConstants desc = do
  thread_gtid <- newVName $ desc ++ "_gtid"
  thread_ltid <- newVName $ desc ++ "_ltid"
  thread_gid <- newVName $ desc ++ "_gid"
  return $ KernelConstants
    thread_gtid thread_ltid thread_gid
    (Imp.ConstSize 0) (Imp.ConstSize 0) (Imp.ConstSize 0)
    [] (Imp.ValueExp $ BoolValue True) mempty

compileKernelBody :: ImpGen.Destination
                  -> KernelConstants
                  -> KernelBody InKernel
                  -> InKernelGen ()
compileKernelBody (ImpGen.Destination dest) constants kbody =
  compileKernelStms constants (kernelBodyStms kbody) $
  zipWithM_ (compileKernelResult constants) dest $
  kernelBodyResult kbody

compileNestedKernelBody :: KernelConstants
                        -> ImpGen.Destination
                        -> Body InKernel
                        -> InKernelGen ()
compileNestedKernelBody constants (ImpGen.Destination dest) kbody =
  compileKernelStms constants (bodyStms kbody) $
  zipWithM_ ImpGen.compileSubExpTo dest $ bodyResult kbody

compileKernelStms :: KernelConstants -> [Stm InKernel]
                  -> InKernelGen a
                  -> InKernelGen a
compileKernelStms constants ungrouped_bnds m =
  compileGroupedKernelStms' $ groupStmsByGuard constants ungrouped_bnds
  where compileGroupedKernelStms' [] = m
        compileGroupedKernelStms' ((g, bnds):rest_bnds) =
          ImpGen.declaringScopes
          (map ((Just . bindingExp) &&& (castScope . scopeOf)) bnds) $ do
            protect g $ mapM_ compileKernelStm bnds
            compileGroupedKernelStms' rest_bnds

        protect Nothing body_m =
          body_m
        protect (Just (Imp.ValueExp (BoolValue True))) body_m =
          body_m
        protect (Just g) body_m = do
          body <- allThreads constants body_m
          ImpGen.emit $ Imp.If g body mempty

        compileKernelStm (Let pat _ e) = do
          dest <- ImpGen.destinationFromPattern pat
          ImpGen.compileExp dest e $ return ()

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

compileKernelExp :: KernelConstants -> ImpGen.Destination -> KernelExp InKernel
                 -> InKernelGen ()

compileKernelExp constants dest (SplitArray o w i max_is elems_per_thread _arrs)
  | ImpGen.Destination (ImpGen.ArrayDestination _ (Just size:_):_) <- dest =
      compileKernelExp constants (ImpGen.Destination [ImpGen.ScalarDestination size]) $
      SplitSpace o w i max_is elems_per_thread

compileKernelExp _ dest (SplitSpace o w i max_is elems_per_thread)
  | ImpGen.Destination [ImpGen.ScalarDestination size] <- dest = do
      num_elements <- Imp.elements <$> ImpGen.compileSubExp w
      i' <- ImpGen.compileSubExp i
      max_is' <- ImpGen.compileSubExp max_is
      elems_per_thread' <- Imp.elements <$> ImpGen.compileSubExp elems_per_thread
      computeThreadChunkSize comm i' max_is' elems_per_thread' num_elements size
        where comm = case o of Disorder -> Commutative
                               InOrder -> Noncommutative

compileKernelExp constants dest (Combine cspace ts active body)
  | Just dest' <- ImpGen.Destination <$> zipWithM index ts (ImpGen.valueDestinations dest) = do
      copy <- allThreads constants $ ImpGen.compileBody dest' body
      ImpGen.emit $ Imp.Op Imp.Barrier
      ImpGen.emit $ Imp.If (Imp.BinOpExp LogAnd (isActive cspace) $
                            ImpGen.compileSubExpOfType Bool active) copy mempty
      ImpGen.emit $ Imp.Op Imp.Barrier
        where index t (ImpGen.ArrayDestination (ImpGen.CopyIntoMemory loc) shape) =
                let space_dims = map (ImpGen.varIndex . fst) cspace
                    t_dims = map (primExpFromSubExp int32) $ arrayDims t
                in Just $ ImpGen.ArrayDestination
                   (ImpGen.CopyIntoMemory
                     (ImpGen.sliceArray loc $
                      fullSliceNum (space_dims++t_dims) $
                      map (DimFix . ImpGen.varIndex . fst) cspace))
                   shape
              index _ _ = Nothing

compileKernelExp constants (ImpGen.Destination dests) (GroupReduce _ lam input) = do
  skip_waves <- newVName "skip_waves"

  let local_tid = kernelLocalThreadId constants
      (_nes, arrs) = unzip input
      (reduce_i, other_index_param, actual_reduce_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (reduce_acc_params, reduce_arr_params) =
        splitAt (length input) actual_reduce_params
      offset = paramName other_index_param

  ImpGen.Destination reduce_acc_targets <-
    ImpGen.destinationFromParams reduce_acc_params

  ImpGen.declaringPrimVar skip_waves int32 $
    ImpGen.declaringLParams (lambdaParams lam) $ do

    ImpGen.emit $ Imp.SetScalar reduce_i $ Imp.var local_tid int32

    ImpGen.emit $ Imp.SetScalar offset 0
    zipWithM_ (readReduceArgument offset) reduce_acc_params arrs

    let read_reduce_args = zipWithM_ (readReduceArgument offset)
                           reduce_arr_params arrs
        reduce_acc_dest = ImpGen.Destination reduce_acc_targets
        do_reduce = do ImpGen.comment "read array element" read_reduce_args
                       ImpGen.compileBody reduce_acc_dest $ lambdaBody lam
                       zipWithM_ (writeReduceOpResult local_tid)
                         reduce_acc_params arrs

    in_wave_reduce <- ImpGen.collect $ ImpGen.everythingVolatile do_reduce
    cross_wave_reduce <- ImpGen.collect do_reduce

    let wave_size = Imp.sizeToExp $ kernelWaveSize constants
        group_size = Imp.sizeToExp $ kernelGroupSize constants
        wave_id = Imp.var local_tid int32 `quot` wave_size
        in_wave_id = Imp.var local_tid int32 - wave_id * wave_size
        num_waves = (group_size + wave_size - 1) `quot` wave_size

        doing_in_wave_reductions =
                  Imp.CmpOpExp (CmpSlt Int32) (Imp.var offset int32) wave_size
        apply_in_in_wave_iteration =
          Imp.CmpOpExp (CmpEq int32)
          (Imp.BinOpExp (And Int32) in_wave_id (2 * Imp.var offset int32 - 1)) 0
        in_wave_reductions =
          Imp.SetScalar offset 1 <>
          Imp.While doing_in_wave_reductions
            (Imp.If apply_in_in_wave_iteration
             in_wave_reduce mempty <>
             Imp.SetScalar offset (Imp.var offset int32 * 2))

        doing_cross_wave_reductions =
          Imp.CmpOpExp (CmpSlt Int32) (Imp.var skip_waves int32) num_waves
        is_first_thread_in_wave =
          Imp.CmpOpExp (CmpEq int32) in_wave_id 0
        wave_not_skipped =
          Imp.CmpOpExp (CmpEq int32)
          (Imp.BinOpExp (And Int32) wave_id (2 * Imp.var skip_waves int32 - 1))
          0
        apply_in_cross_wave_iteration =
          Imp.BinOpExp LogAnd is_first_thread_in_wave wave_not_skipped
        cross_wave_reductions =
          Imp.SetScalar skip_waves 1 <>
          Imp.While doing_cross_wave_reductions
            (Imp.Op Imp.Barrier <>
             Imp.SetScalar offset (Imp.var skip_waves int32 * wave_size) <>
             Imp.If apply_in_cross_wave_iteration
             cross_wave_reduce mempty <>
             Imp.SetScalar skip_waves (Imp.var skip_waves int32 * 2))

    ImpGen.emit $
      in_wave_reductions <> cross_wave_reductions

    forM_ (zip dests reduce_acc_params) $ \(dest, reduce_acc_param) ->
      ImpGen.copyDWIMDest dest [] (Var $ paramName reduce_acc_param) []
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

compileKernelExp constants _ (GroupScan _ lam input) = do
  renamed_lam <- renameLambda lam

  let local_tid = kernelLocalThreadId constants
      (_nes, arrs) = unzip input
      (lam_i, other_index_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (x_params, y_params) =
        splitAt (length input) actual_params

  ImpGen.declaringLParams (lambdaParams lam++lambdaParams renamed_lam) $ do
    ImpGen.emit $ Imp.SetScalar lam_i $ Imp.var local_tid int32

    acc_local_mem <- flip zip (repeat ()) <$>
                     mapM (fmap (ImpGen.memLocationName . ImpGen.entryArrayLocation) .
                           ImpGen.lookupArray) arrs

    let wave_size = Imp.sizeToExp $ kernelWaveSize constants
        wave_id = Imp.var local_tid int32 `quot` wave_size
        in_wave_id = Imp.var local_tid int32 - wave_id * wave_size
        doInWaveScan = inWaveScan wave_size local_tid acc_local_mem

    doInWaveScan lam
    ImpGen.emit $ Imp.Op Imp.Barrier

    pack_wave_results <-
      ImpGen.collect $
      zipWithM_ (writeParamToLocalMemory wave_id) acc_local_mem y_params

    let last_in_wave =
          Imp.CmpOpExp (CmpEq int32) in_wave_id $ wave_size - 1
    ImpGen.comment
      "last thread of wave 'i' writes its result to offset 'i'" $
      ImpGen.emit $ Imp.If last_in_wave pack_wave_results mempty

    ImpGen.emit $ Imp.Op Imp.Barrier

    let is_first_wave = Imp.CmpOpExp (CmpEq int32) wave_id 0
    scan_first_wave <- ImpGen.collect $ doInWaveScan renamed_lam
    ImpGen.comment
      "scan the first wave, after which offset 'i' contains carry-in for warp 'i+1'" $
      ImpGen.emit $ Imp.If is_first_wave scan_first_wave mempty

    ImpGen.emit $ Imp.Op Imp.Barrier

    read_carry_in <-
      ImpGen.collect $
      zipWithM_ (readParamFromLocalMemory
                 (paramName other_index_param) (wave_id - 1))
      x_params acc_local_mem

    y_dest <- ImpGen.destinationFromParams y_params
    op_to_y <- ImpGen.collect $ ImpGen.compileBody y_dest $ lambdaBody lam
    write_final_result <- ImpGen.collect $
      zipWithM_ (writeParamToLocalMemory $ Imp.var local_tid int32) acc_local_mem y_params

    ImpGen.comment "carry-in for every wave except the first" $
      ImpGen.emit $ Imp.If is_first_wave mempty $
      Imp.Comment "read operands" read_carry_in <>
      Imp.Comment "perform operation" op_to_y <>
      Imp.Comment "write final result" write_final_result

    ImpGen.emit $ Imp.Op Imp.Barrier

    ImpGen.comment "restore correct values for first wave" $
      ImpGen.emit $ Imp.If is_first_wave write_final_result mempty


compileKernelExp constants (ImpGen.Destination final_targets) (GroupStream w maxchunk lam accs _arrs) = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
      block_offset' = Imp.var block_offset int32
  w' <- ImpGen.compileSubExp w
  max_block_size <- ImpGen.compileSubExp maxchunk
  acc_dest <- ImpGen.destinationFromParams acc_params

  ImpGen.declaringLParams (acc_params++arr_params) $ do
    zipWithM_ ImpGen.compileSubExpTo (ImpGen.valueDestinations acc_dest) accs
    ImpGen.declaringPrimVar block_size int32 $
      -- If the GroupStream is morally just a do-loop, generate simpler code.
      case mapM isSimpleThreadInSpace $ bodyStms body of
        Just stms' | ValueExp x <- max_block_size, oneIsh x -> do
          let body' = body { bodyStms = stms' }
          body'' <- ImpGen.withPrimVar block_offset int32 $
                    allThreads constants $ ImpGen.compileBody acc_dest body'
          ImpGen.emit $ Imp.SetScalar block_size 1

          -- Check if loop is candidate for unrolling.
          let loop =
                case w of
                  Var w_var | Just w_bound <- lookup w_var $ kernelStreamed constants,
                              w_bound /= Imp.ConstSize 1->
                              -- Candidate for unrolling, so generate two loops.
                              Imp.If (CmpOpExp (CmpEq int32) w' (Imp.sizeToExp w_bound))
                              (Imp.For block_offset Int32 (Imp.sizeToExp w_bound) body'')
                              (Imp.For block_offset Int32 w' body'')
                  _ -> Imp.For block_offset Int32 w' body''

          ImpGen.emit $
            if kernelThreadActive constants == Imp.ValueExp (BoolValue True)
            then loop
            else Imp.If (kernelThreadActive constants) loop mempty

        _ -> ImpGen.declaringPrimVar block_offset int32 $ do
          body' <- streaming constants block_size maxchunk $
                   ImpGen.compileBody acc_dest body

          ImpGen.emit $ Imp.SetScalar block_offset 0

          let not_at_end =
                Imp.CmpOpExp (CmpSlt Int32) block_offset' w'
              set_block_size =
                Imp.If (Imp.CmpOpExp (CmpSlt Int32)
                         (w' - block_offset')
                         max_block_size)
                (Imp.SetScalar block_size (w' - block_offset'))
                (Imp.SetScalar block_size max_block_size)
              increase_offset =
                Imp.SetScalar block_offset $
                block_offset' + max_block_size

          ImpGen.emit $
            Imp.While not_at_end $
            set_block_size <> body' <> increase_offset

    zipWithM_ ImpGen.compileSubExpTo final_targets $
      map (Var . paramName) acc_params

      where isSimpleThreadInSpace (Let _ _ Op{}) = Nothing
            isSimpleThreadInSpace bnd = Just bnd

compileKernelExp _ dest e =
  throwError $ unlines ["Invalid target",
                         "  " ++ show dest,
                         "for kernel expression",
                         "  " ++ pretty e]

allThreads :: KernelConstants -> InKernelGen () -> InKernelGen Imp.KernelCode
allThreads constants = ImpGen.subImpM_ $ inKernelOperations constants'
  where constants' =
          constants { kernelThreadActive = Imp.ValueExp (BoolValue True) }

streaming :: KernelConstants -> VName -> SubExp -> InKernelGen () -> InKernelGen Imp.KernelCode
streaming constants chunksize bound m = do
  bound' <- ImpGen.subExpToDimSize bound
  let constants' =
        constants { kernelStreamed = (chunksize, bound') : kernelStreamed constants }
  ImpGen.subImpM_ (inKernelOperations constants') m

compileKernelResult :: KernelConstants -> ImpGen.ValueDestination -> KernelResult
                    -> InKernelGen ()
compileKernelResult constants dest (ThreadsReturn (OneThreadPerGroup who) what) = do
  write_result <-
    ImpGen.collect $
    ImpGen.copyDWIMDest dest [ImpGen.varIndex $ kernelGroupId constants] what []

  let me = Imp.var (kernelLocalThreadId constants) int32
  who' <- ImpGen.compileSubExp who
  ImpGen.emit $
    Imp.If (Imp.CmpOpExp (CmpEq int32) me who')
    write_result mempty

compileKernelResult constants dest (ThreadsReturn AllThreads what) =
  ImpGen.copyDWIMDest dest [ImpGen.varIndex $ kernelGlobalThreadId constants] what []

compileKernelResult constants dest (ThreadsReturn (ThreadsPerGroup limit) what) = do
  write_result <-
    ImpGen.collect $
    ImpGen.copyDWIMDest dest [ImpGen.varIndex $ kernelGroupId constants] what []

  ImpGen.emit $ Imp.If (isActive limit) write_result mempty

compileKernelResult constants dest (ThreadsReturn ThreadsInSpace what) = do
  let is = map (ImpGen.varIndex . fst) $ kernelDimensions constants
  write_result <- ImpGen.collect $ ImpGen.copyDWIMDest dest is what []
  ImpGen.emit $ Imp.If (kernelThreadActive constants)
    write_result mempty

compileKernelResult constants dest (ConcatReturns InOrder _ per_thread_elems what) = do
  ImpGen.ArrayDestination (ImpGen.CopyIntoMemory dest_loc) x <- return dest
  let dest_loc_offset = ImpGen.offsetArray dest_loc $
                        primExpFromSubExp int32 per_thread_elems *
                        ImpGen.varIndex (kernelGlobalThreadId constants)
      dest' = ImpGen.ArrayDestination (ImpGen.CopyIntoMemory dest_loc_offset) x
  ImpGen.copyDWIMDest dest' [] (Var what) []

compileKernelResult constants dest (ConcatReturns Disorder _ _ what) = do
  ImpGen.ArrayDestination (ImpGen.CopyIntoMemory dest_loc) x <- return dest
  let dest_loc' = ImpGen.strideArray
                  (ImpGen.offsetArray dest_loc $
                   ImpGen.varIndex (kernelGlobalThreadId constants)) $
                  ImpGen.dimSizeToPrimExp (kernelNumThreads constants)
      dest' = ImpGen.ArrayDestination (ImpGen.CopyIntoMemory dest_loc') x
  ImpGen.copyDWIMDest dest' [] (Var what) []

compileKernelResult constants dest (WriteReturn rw _arr i e) = do
  i' <- ImpGen.compileSubExp i
  rw' <- ImpGen.compileSubExp rw
  let condInBounds0 = Imp.CmpOpExp (Imp.CmpSle Int32)
                      (Imp.ValueExp (IntValue (Int32Value 0)))
                      i'
      condInBounds1 = Imp.CmpOpExp (Imp.CmpSlt Int32)
                      i' rw'
      condInBounds = Imp.BinOpExp LogAnd condInBounds0 condInBounds1
      write = Imp.BinOpExp LogAnd (kernelThreadActive constants) condInBounds

  actual_body' <- ImpGen.collect $
    ImpGen.copyDWIMDest dest [primExpFromSubExp int32 i] e []
  ImpGen.emit $ Imp.If write actual_body' Imp.Skip

compileKernelResult _ _ KernelInPlaceReturn{} =
  -- Already in its place... said it was a hack.
  return ()

isActive :: [(VName, SubExp)] -> Imp.Exp
isActive limit = case actives of
                    [] -> Imp.ValueExp $ BoolValue True
                    x:xs -> foldl (Imp.BinOpExp LogAnd) x xs
  where (is, ws) = unzip limit
        actives = zipWith active is $ map (ImpGen.compileSubExpOfType Bool) ws
        active i = Imp.CmpOpExp (CmpSlt Int32) (Imp.var i Bool)

setSpaceIndices :: KernelSpace -> InKernelGen ()
setSpaceIndices space =
  case spaceStructure space of
    FlatSpace is_and_dims -> do
      let (is, dims) = unzip is_and_dims
      dims' <- mapM ImpGen.compileSubExp dims
      let index_expressions = unflattenIndex dims' $ Imp.var global_tid int32
      forM_ (zip is index_expressions) $ \(i, x) ->
        ImpGen.emit $ Imp.SetScalar i x
    NestedSpace is_and_dims -> do
      let (gtids, gdims, ltids, ldims) = unzip4 is_and_dims
      gdims' <- mapM ImpGen.compileSubExp gdims
      ldims' <- mapM ImpGen.compileSubExp ldims
      let (gtid_es, ltid_es) = unzip $ unflattenNestedIndex gdims' ldims' $
                               Imp.var global_tid int32
      forM_ (zip gtids gtid_es) $ \(i,e) ->
        ImpGen.emit $ Imp.SetScalar i e
      forM_ (zip ltids ltid_es) $ \(i,e) ->
        ImpGen.emit $ Imp.SetScalar i e
  where global_tid = spaceGlobalId space

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
