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
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.CodeGen.SetDefaultSpace
import Futhark.Tools (partitionChunkedKernelLambdaParameters, fullSliceNum)
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem, IntegralExp)
import Futhark.Util (splitAt3)

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

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError Imp.Program)
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
compileInKernelOp _ (ImpGen.Destination _ [ImpGen.MemoryDestination mem]) Alloc{} =
  compilerLimitationS $ "Cannot allocate memory block " ++ pretty mem ++ " in kernel."
compileInKernelOp _ dest Alloc{} =
  compilerBugS $ "Invalid target for in-kernel allocation: " ++ show dest
compileInKernelOp constants dest (Inner op) =
  compileKernelExp constants dest op

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: ImpGen.Destination -> Kernel InKernel
               -> CallKernelGen ()

kernelCompiler dest (GetSize key size_class) = do
  [v] <- ImpGen.funcallTargets dest
  ImpGen.emit $ Imp.Op $ Imp.GetSize v key size_class

kernelCompiler dest (CmpSizeLe key size_class x) = do
  [v] <- ImpGen.funcallTargets dest
  ImpGen.emit =<< Imp.Op . Imp.CmpSizeLe v key size_class <$> ImpGen.compileSubExp x

kernelCompiler dest (GetSizeMax size_class) = do
  [v] <- ImpGen.funcallTargets dest
  ImpGen.emit $ Imp.Op $ Imp.GetSizeMax v size_class

kernelCompiler dest (Kernel desc space _ kernel_body) = do

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

  forM_ (kernelHints desc) $ \(s,v) -> do
    ty <- case v of
      Constant pv -> return $ Prim $ primValueType pv
      Var vn -> lookupType vn
    unless (primType ty) $ fail $ concat [ "debugKernelHint '", s, "'"
                                         , " in kernel '", kernelName desc, "'"
                                         , " did not have primType value." ]

    ImpGen.compileSubExp v >>= ImpGen.emit . Imp.DebugPrint s (elemType ty)

  ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.AnyKernel Imp.Kernel
            { Imp.kernelBody = kernel_body'
            , Imp.kernelLocalMemory = local_memory
            , Imp.kernelUses = uses
            , Imp.kernelNumGroups = num_groups'
            , Imp.kernelGroupSize = group_size'
            , Imp.kernelName = global_tid
            , Imp.kernelDesc = kernelName desc
            }

expCompiler :: ImpGen.ExpCompiler ExplicitMemory Imp.HostOp
-- We generate a simple kernel for itoa and replicate.
expCompiler
  (ImpGen.Destination tag [ImpGen.ArrayDestination (Just destloc)])
  (BasicOp (Iota n x s et)) = do
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

    ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.Map Imp.MapKernel
      { Imp.mapKernelThreadNum = thread_gid
      , Imp.mapKernelDesc = "iota"
      , Imp.mapKernelNumGroups = Imp.VarSize num_groups
      , Imp.mapKernelGroupSize = Imp.VarSize group_size
      , Imp.mapKernelSize = n'
      , Imp.mapKernelUses = body_uses
      , Imp.mapKernelBody = body
      }

expCompiler
  (ImpGen.Destination tag [dest]) (BasicOp (Replicate (Shape ds) se)) = do
  constants <- simpleKernelConstants tag "replicate"

  t <- subExpType se
  let thread_gid = kernelGlobalThreadId constants
      row_dims = arrayDims t
      dims = ds ++ row_dims
      is' = unflattenIndex (map (ImpGen.compileSubExpOfType int32) dims) $
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
  ImpGen.emit $ Imp.Op $ Imp.CallKernel $
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
inKernelExpCompiler _ (BasicOp (Assert _ _ (loc, locs))) =
  compilerLimitationS $
  unlines [ "Cannot compile assertion at " ++
            intercalate " -> " (reverse $ map locStr $ loc:locs) ++
            " inside parallel kernel."
          , "As a workaround, surround the expression with 'unsafe'."]
-- The static arrays stuff does not work inside kernels.
inKernelExpCompiler (ImpGen.Destination _ [dest]) (BasicOp (ArrayLit es _)) =
  forM_ (zip [0..] es) $ \(i,e) ->
  ImpGen.copyDWIMDest dest [fromIntegral (i::Int32)] e []
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
-- when declaring an array in C.
isStaticExp :: Imp.KernelConstExp -> Bool
isStaticExp LeafExp{} = True
isStaticExp ValueExp{} = True
isStaticExp (BinOpExp Add{} x y) = isStaticExp x && isStaticExp y
isStaticExp (BinOpExp Sub{} x y) = isStaticExp x && isStaticExp y
isStaticExp (BinOpExp Mul{} x y) = isStaticExp x && isStaticExp y
isStaticExp _ = False

isConstExp :: VName -> CallKernelGen (Maybe Imp.KernelConstExp)
isConstExp v = do
  vtable <- asks ImpGen.envVtable
  let lookupConstExp name = constExp =<< hasExp =<< M.lookup name vtable
      kernelConst (Op (Inner (GetSize key _))) = Just $ LeafExp (Imp.SizeConst key) int32
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
  local $ \env -> env { ImpGen.envVtable = M.map globalMemory $ ImpGen.envVtable env
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
  ImpGen.emit $ Imp.Op $ Imp.GetSize group_size group_size Imp.SizeGroup
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
      ImpGen.emit $
      Imp.SetScalar (paramName param) $
      Imp.index l_mem (bytes i') bt (Space "local") Imp.Volatile
  | otherwise =
      ImpGen.emit $
      Imp.SetScalar index i
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
  ImpGen.emit $ Imp.SetScalar chunk_var $ Imp.BinOpExp (SMin Int32)
    (Imp.innerExp elements_per_thread) $
    (Imp.innerExp num_elements - thread_index)
    `quotRoundingUp`
    stride'

computeThreadChunkSize SplitContiguous thread_index elements_per_thread num_elements chunk_var = do
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

inBlockScan :: Imp.Exp
           -> Imp.Exp
           -> Imp.Exp
           -> VName
           -> [(VName, t)]
           -> Lambda InKernel
           -> InKernelGen ()
inBlockScan lockstep_width block_size active local_id acc_local_mem scan_lam = ImpGen.everythingVolatile $ do
  skip_threads <- newVName "skip_threads"
  let in_block_thread_active =
        Imp.CmpOpExp (CmpSle Int32) (Imp.var skip_threads int32) in_block_id
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
  read_my_initial <- ImpGen.collect $
                     zipWithM_ (readParamFromLocalMemory scan_lam_i $ Imp.var local_id int32)
                     y_params acc_local_mem
  ImpGen.emit $ Imp.If active read_my_initial mempty

  op_to_y <- ImpGen.collect $ ImpGen.compileBody scan_y_dest $ lambdaBody scan_lam
  write_operation_result <-
    ImpGen.collect $
    zipWithM_ (writeParamToLocalMemory $ Imp.var local_id int32)
    acc_local_mem y_params
  let andBlockActive = Imp.BinOpExp LogAnd active
      maybeBarrier = Imp.If (Imp.CmpOpExp (CmpSle Int32) lockstep_width (Imp.var skip_threads int32))
                     (Imp.Op Imp.Barrier) mempty

  ImpGen.emit $
    Imp.Comment "in-block scan (hopefully no barriers needed)" $
    Imp.DeclareScalar skip_threads int32 <>
    Imp.SetScalar skip_threads 1 <>
    Imp.While (Imp.CmpOpExp (CmpSlt Int32) (Imp.var skip_threads int32) block_size)
    (Imp.If (andBlockActive in_block_thread_active)
      (Imp.Comment "read operands" read_operands <>
       Imp.Comment "perform operation" op_to_y) mempty <>

     maybeBarrier <>

     Imp.If (andBlockActive in_block_thread_active)
      (Imp.Comment "write result" write_operation_result) mempty <>
     maybeBarrier <>
     Imp.SetScalar skip_threads (Imp.var skip_threads int32 * 2))
  where block_id = Imp.BinOpExp (SQuot Int32) (Imp.var local_id int32) block_size
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

compileKernelBody :: ImpGen.Destination
                  -> KernelConstants
                  -> KernelBody InKernel
                  -> InKernelGen ()
compileKernelBody (ImpGen.Destination _ dest) constants kbody =
  compileKernelStms constants (stmsToList $ kernelBodyStms kbody) $
  zipWithM_ (compileKernelResult constants) dest $
  kernelBodyResult kbody

compileNestedKernelBody :: KernelConstants
                        -> ImpGen.Destination
                        -> Body InKernel
                        -> InKernelGen ()
compileNestedKernelBody constants (ImpGen.Destination _ dest) kbody =
  compileKernelStms constants (stmsToList $ bodyStms kbody) $
  zipWithM_ ImpGen.compileSubExpTo dest $ bodyResult kbody

compileKernelStms :: KernelConstants -> [Stm InKernel]
                  -> InKernelGen a
                  -> InKernelGen a
compileKernelStms constants ungrouped_bnds m =
  compileGroupedKernelStms' $ groupStmsByGuard constants ungrouped_bnds
  where compileGroupedKernelStms' [] = m
        compileGroupedKernelStms' ((g, bnds):rest_bnds) =
          ImpGen.declaringScopes
          (map ((Just . stmExp) &&& (castScope . scopeOf)) bnds) $ do
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
          ImpGen.compileExp dest e

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

compileKernelExp _ (ImpGen.Destination _ dests) (Barrier ses) = do
  zipWithM_ ImpGen.compileSubExpTo dests ses
  ImpGen.emit $ Imp.Op Imp.Barrier

compileKernelExp _ dest (SplitSpace o w i elems_per_thread)
  | ImpGen.Destination _ [ImpGen.ScalarDestination size] <- dest = do
      num_elements <- Imp.elements <$> ImpGen.compileSubExp w
      i' <- ImpGen.compileSubExp i
      elems_per_thread' <- Imp.elements <$> ImpGen.compileSubExp elems_per_thread
      computeThreadChunkSize o i' elems_per_thread' num_elements size

compileKernelExp constants dest (Combine (CombineSpace scatter cspace) ts aspace body) = do
  -- First we compute how many times we have to iterate to cover
  -- cspace with our group size.  It is a fairly common case that
  -- we statically know that this requires 1 iteration, so we
  -- could detect it and not generate a loop in that case.
  -- However, it seems to have no impact on performance (an extra
  -- conditional jump), so for simplicity we just always generate
  -- the loop.
  let cspace_dims = map (streamBounded . snd) cspace
      num_iters = product cspace_dims `quotRoundingUp`
                  Imp.sizeToExp (kernelGroupSize constants)

  iter <- newVName "comb_iter"
  cid <- newVName "flat_comb_id"

  one_iteration <- ImpGen.collect $
    ImpGen.declaringPrimVars (zip (map fst cspace) $ repeat int32) $
    ImpGen.declaringPrimVar cid int32 $ do

      -- Compute the *flat* array index.
      ImpGen.emit $ Imp.SetScalar cid $
        Imp.var iter int32 * Imp.sizeToExp (kernelGroupSize constants) +
        Imp.var (kernelLocalThreadId constants) int32

      -- Turn it into a nested array index.
      forM_ (zip (map fst cspace) $ unflattenIndex cspace_dims (Imp.var cid int32)) $ \(v, x) ->
        ImpGen.emit $ Imp.SetScalar v x

      -- Construct the body.  This is mostly about the book-keeping
      -- for the scatter-like part.
      let (scatter_ws, scatter_ns, _scatter_vs) = unzip3 scatter
          scatter_ws_repl = concat $ zipWith replicate scatter_ns scatter_ws
          (scatter_dests, normal_dests) =
            splitAt (sum scatter_ns) $ ImpGen.valueDestinations dest
          (res_is, res_vs, res_normal) =
            splitAt3 (sum scatter_ns) (sum scatter_ns) $ bodyResult body
          scatter_is = map (pure . DimFix . ImpGen.compileSubExpOfType int32) res_is
          scatter_dests_repl = concat $ zipWith replicate scatter_ns scatter_dests
      (scatter_dests', normal_dests') <-
        case (sequence $ zipWith3 index scatter_is ts scatter_dests_repl,
              zipWithM (index local_index) (drop (sum scatter_ns*2) ts) normal_dests) of
          (Just x, Just y) -> return (x, y)
          _ -> fail "compileKernelExp combine: invalid destination."
      body' <- allThreads constants $
        ImpGen.compileStms (freeIn $ bodyResult body) (stmsToList $ bodyStms body) $ do

        forM_ (zip4 scatter_ws_repl res_is res_vs scatter_dests') $
          \(w, res_i, res_v, scatter_dest) -> do
            let res_i' = ImpGen.compileSubExpOfType int32 res_i
                w'     = ImpGen.compileSubExpOfType int32 w
                -- We have to check that 'res_i' is in-bounds wrt. an array of size 'w'.
                in_bounds = BinOpExp LogAnd (CmpOpExp (CmpSle Int32) 0 res_i')
                                            (CmpOpExp (CmpSlt Int32) res_i' w')
            when_in_bounds <- ImpGen.collect $ ImpGen.compileSubExpTo scatter_dest res_v
            ImpGen.emit $ Imp.If in_bounds when_in_bounds mempty

        zipWithM_ ImpGen.compileSubExpTo normal_dests' res_normal

      -- Execute the body if we are within bounds.
      ImpGen.emit $
        Imp.If (Imp.BinOpExp LogAnd (isActive cspace) (isActive aspace)) body' mempty

  ImpGen.emit $ Imp.For iter Int32 num_iters one_iteration
  ImpGen.emit $ Imp.Op Imp.Barrier

    where streamBounded (Var v)
            | Just x <- lookup v $ kernelStreamed constants =
                Imp.sizeToExp x
          streamBounded se = ImpGen.compileSubExpOfType int32 se

          local_index = map (DimFix . ImpGen.varIndex . fst) cspace

          index i t (ImpGen.ArrayDestination (Just loc)) =
            let space_dims = map (ImpGen.varIndex . fst) cspace
                t_dims = map (ImpGen.compileSubExpOfType int32) $ arrayDims t
            in Just $ ImpGen.ArrayDestination $
               Just $ ImpGen.sliceArray loc $
               fullSliceNum (space_dims++t_dims) i
          index _ _ _ = Nothing

compileKernelExp constants (ImpGen.Destination _ dests) (GroupReduce w lam input) = do
  skip_waves <- newVName "skip_waves"
  w' <- ImpGen.compileSubExp w

  let local_tid = kernelLocalThreadId constants
      (_nes, arrs) = unzip input
      (reduce_i, other_index_param, actual_reduce_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams lam
      (reduce_acc_params, reduce_arr_params) =
        splitAt (length input) actual_reduce_params
      offset = paramName other_index_param

  ImpGen.Destination _ reduce_acc_targets <-
    ImpGen.destinationFromParams reduce_acc_params

  ImpGen.declaringPrimVar skip_waves int32 $
    ImpGen.declaringLParams (lambdaParams lam) $ do

    ImpGen.emit $ Imp.SetScalar reduce_i $ Imp.var local_tid int32

    ImpGen.emit $ Imp.SetScalar offset 0
    set_init_params <- ImpGen.collect $
      zipWithM_ (readReduceArgument offset) reduce_acc_params arrs
    ImpGen.emit $
      Imp.If (Imp.CmpOpExp (CmpSlt Int32) (Imp.var local_tid int32) w')
      set_init_params mempty

    let read_reduce_args = zipWithM_ (readReduceArgument offset)
                           reduce_arr_params arrs
        reduce_acc_dest = ImpGen.Destination Nothing reduce_acc_targets
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
        arg_in_bounds = Imp.CmpOpExp (CmpSlt Int32)
                        (Imp.BinOpExp (Add Int32)
                          (Imp.var local_tid int32)
                          (Imp.var (paramName other_index_param) int32))
                        w'

        doing_in_wave_reductions =
          Imp.CmpOpExp (CmpSlt Int32) (Imp.var offset int32) wave_size
        apply_in_in_wave_iteration =
          Imp.CmpOpExp (CmpEq int32)
          (Imp.BinOpExp (And Int32) in_wave_id (2 * Imp.var offset int32 - 1)) 0
        in_wave_reductions =
          Imp.SetScalar offset 1 <>
          Imp.While doing_in_wave_reductions
            (Imp.If (Imp.BinOpExp LogAnd arg_in_bounds apply_in_in_wave_iteration)
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
          Imp.BinOpExp LogAnd arg_in_bounds $
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

compileKernelExp constants _ (GroupScan w lam input) = do
  renamed_lam <- renameLambda lam
  w' <- ImpGen.compileSubExp w

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
        lid_in_bounds = Imp.CmpOpExp (CmpSlt Int32) (Imp.var local_tid int32) w'

    doInBlockScan lid_in_bounds lam
    ImpGen.emit $ Imp.Op Imp.Barrier

    pack_block_results <-
      ImpGen.collect $
      zipWithM_ (writeParamToLocalMemory block_id) acc_local_mem y_params

    let last_in_block =
          Imp.CmpOpExp (CmpEq int32) in_block_id $ block_size - 1
    ImpGen.comment
      "last thread of block 'i' writes its result to offset 'i'" $
      ImpGen.emit $ Imp.If (Imp.BinOpExp LogAnd last_in_block lid_in_bounds) pack_block_results mempty

    ImpGen.emit $ Imp.Op Imp.Barrier

    let is_first_block = Imp.CmpOpExp (CmpEq int32) block_id 0
    ImpGen.comment
      "scan the first block, after which offset 'i' contains carry-in for warp 'i+1'" $
      doInBlockScan (Imp.BinOpExp LogAnd is_first_block lid_in_bounds) renamed_lam

    ImpGen.emit $ Imp.Op Imp.Barrier

    read_carry_in <-
      ImpGen.collect $
      zipWithM_ (readParamFromLocalMemory
                 (paramName other_index_param) (block_id - 1))
      x_params acc_local_mem

    y_dest <- ImpGen.destinationFromParams y_params
    op_to_y <- ImpGen.collect $ ImpGen.compileBody y_dest $ lambdaBody lam
    write_final_result <- ImpGen.collect $
      zipWithM_ (writeParamToLocalMemory $ Imp.var local_tid int32) acc_local_mem y_params

    ImpGen.comment "carry-in for every block except the first" $
      ImpGen.emit $ Imp.If (Imp.BinOpExp LogOr
                             is_first_block
                             (Imp.UnOpExp Not lid_in_bounds)) mempty $
      Imp.Comment "read operands" read_carry_in <>
      Imp.Comment "perform operation" op_to_y <>
      Imp.Comment "write final result" write_final_result

    ImpGen.emit $ Imp.Op Imp.Barrier

    ImpGen.comment "restore correct values for first block" $
      ImpGen.emit $ Imp.If is_first_block write_final_result mempty


compileKernelExp constants (ImpGen.Destination _ final_targets) (GroupStream w maxchunk lam accs _arrs) = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
      block_offset' = Imp.var block_offset int32
  w' <- ImpGen.compileSubExp w
  max_block_size <- ImpGen.compileSubExp maxchunk
  acc_dest <- ImpGen.destinationFromParams acc_params

  ImpGen.declaringLParams (acc_params++arr_params) $ do
    zipWithM_ ImpGen.compileSubExpTo (ImpGen.valueDestinations acc_dest) accs
    ImpGen.declaringPrimVar block_size int32 $
      -- If the GroupStream is morally just a do-loop, generate simpler code.
      case mapM isSimpleThreadInSpace $ stmsToList $ bodyStms body of
        Just stms' | ValueExp x <- max_block_size, oneIsh x -> do
          let body' = body { bodyStms = stmsFromList stms' }
          body'' <- ImpGen.withPrimVar block_offset int32 $
                    allThreads constants $ ImpGen.emit =<<
                    ImpGen.compileLoopBody (map paramName acc_params) body'
          ImpGen.emit $ Imp.SetScalar block_size 1

          -- Check if loop is candidate for unrolling.
          let loop =
                case w of
                  Var w_var | Just w_bound <- lookup w_var $ kernelStreamed constants,
                              w_bound /= Imp.ConstSize 1 ->
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

          -- Three cases to consider for simpler generated code based
          -- on max block size: (0) if full input size, do not
          -- generate a loop; (1) if one, generate for-loop (2)
          -- otherwise, generate chunked while-loop.
          ImpGen.emit $
            if max_block_size == w' then
              Imp.SetScalar block_size w' <> body'
            else if max_block_size == Imp.ValueExp (value (1::Int32)) then
                   Imp.SetScalar block_size w' <>
                   Imp.For block_offset Int32 w' body'
                 else
                   Imp.While not_at_end $
                   set_block_size <> body' <> increase_offset

    zipWithM_ ImpGen.compileSubExpTo final_targets $
      map (Var . paramName) acc_params

      where isSimpleThreadInSpace (Let _ _ Op{}) = Nothing
            isSimpleThreadInSpace bnd = Just bnd

compileKernelExp _ dest e =
  compilerBugS $ unlines ["Invalid target", "  " ++ show dest,
                          "for kernel expression", "  " ++ pretty e]

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

compileKernelResult constants dest (ThreadsReturn OneResultPerGroup what) = do
  i <- newVName "i"

  in_local_memory <- arrayInLocalMemory what
  let me = Imp.var (kernelLocalThreadId constants) int32

  if not in_local_memory then do
    write_result <-
      ImpGen.collect $
      ImpGen.copyDWIMDest dest [ImpGen.varIndex $ kernelGroupId constants] what []

    who' <- ImpGen.compileSubExp $ intConst Int32 0
    ImpGen.emit $
      Imp.If (Imp.CmpOpExp (CmpEq int32) me who') write_result mempty
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

      write_result <-
        ImpGen.collect $
        ImpGen.copyDWIMDest dest (ImpGen.varIndex (kernelGroupId constants) : is)
                            what is

      ImpGen.emit $ Imp.For i Int32 to_write write_result

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

compileKernelResult constants dest (ConcatReturns SplitContiguous _ per_thread_elems moffset what) = do
  ImpGen.ArrayDestination (Just dest_loc) <- return dest
  let dest_loc_offset = ImpGen.offsetArray dest_loc offset
      dest' = ImpGen.ArrayDestination $ Just dest_loc_offset
  ImpGen.copyDWIMDest dest' [] (Var what) []
  where offset = case moffset of
                   Nothing -> ImpGen.compileSubExpOfType int32 per_thread_elems *
                              ImpGen.varIndex (kernelGlobalThreadId constants)
                   Just se -> ImpGen.compileSubExpOfType int32 se

compileKernelResult constants dest (ConcatReturns (SplitStrided stride) _ _ moffset what) = do
  ImpGen.ArrayDestination (Just dest_loc) <- return dest
  let dest_loc' = ImpGen.strideArray
                  (ImpGen.offsetArray dest_loc offset) $
                  ImpGen.compileSubExpOfType int32 stride
      dest' = ImpGen.ArrayDestination $ Just dest_loc'
  ImpGen.copyDWIMDest dest' [] (Var what) []
  where offset = case moffset of
                   Nothing -> ImpGen.varIndex (kernelGlobalThreadId constants)
                   Just se -> ImpGen.compileSubExpOfType int32 se

compileKernelResult constants dest (WriteReturn rws _arr dests) = do
  rws' <- mapM ImpGen.compileSubExp rws
  forM_ dests $ \(is, e) -> do
    is' <- mapM ImpGen.compileSubExp is
    let condInBounds0 = Imp.CmpOpExp (Imp.CmpSle Int32) $
                        Imp.ValueExp (IntValue (Int32Value 0))
        condInBounds1 = Imp.CmpOpExp (Imp.CmpSlt Int32)
        condInBounds i rw = Imp.BinOpExp LogAnd (condInBounds0 i) (condInBounds1 i rw)
        write = foldl (Imp.BinOpExp LogAnd) (kernelThreadActive constants) $
                zipWith condInBounds is' rws'
    actual_body' <- ImpGen.collect $
      ImpGen.copyDWIMDest dest (map (ImpGen.compileSubExpOfType int32) is) e []
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
    FlatThreadSpace is_and_dims ->
      flatSpaceWith gtid is_and_dims
    NestedThreadSpace is_and_dims -> do
      let (gtids, gdims, ltids, ldims) = unzip4 is_and_dims
      gdims' <- mapM ImpGen.compileSubExp gdims
      ldims' <- mapM ImpGen.compileSubExp ldims
      let (gtid_es, ltid_es) = unzip $ unflattenNestedIndex gdims' ldims' gtid
      forM_ (zip gtids gtid_es) $ \(i,e) ->
        ImpGen.emit $ Imp.SetScalar i e
      forM_ (zip ltids ltid_es) $ \(i,e) ->
        ImpGen.emit $ Imp.SetScalar i e
  where gtid = Imp.var (spaceGlobalId space) int32

        flatSpaceWith base is_and_dims = do
          let (is, dims) = unzip is_and_dims
          dims' <- mapM ImpGen.compileSubExp dims
          let index_expressions = unflattenIndex dims' base
          forM_ (zip is index_expressions) $ \(i, x) ->
            ImpGen.emit $ Imp.SetScalar i x

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
