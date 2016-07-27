{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futhark.CodeGen.ImpGen.Kernels
  ( compileProg
  )
  where

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
import qualified Futhark.Analysis.ScalExp as SE
import qualified Futhark.Representation.ExplicitMemory.IndexFunction as IxFun
import Futhark.Pass.ExtractKernels.BlockedKernel (KernelInput(..)) -- XXX
import Futhark.CodeGen.SetDefaultSpace
import Futhark.Tools (partitionChunkedKernelLambdaParameters,
                      partitionChunkedFoldParameters)
import Futhark.Util (splitAt3)
import Futhark.Util.IntegralExp (quotRoundingUp, quot, rem, IntegralCond)

type CallKernelGen = ImpGen.ImpM Imp.HostOp
type InKernelGen = ImpGen.ImpM Imp.KernelOp

callKernelOperations :: ImpGen.Operations Imp.HostOp
callKernelOperations =
  ImpGen.Operations { ImpGen.opsExpCompiler = expCompiler
                    , ImpGen.opsCopyCompiler = callKernelCopy
                    , ImpGen.opsOpCompiler = opCompiler
                    }


inKernelOperations :: ImpGen.Operations Imp.KernelOp
inKernelOperations = (ImpGen.defaultOperations cannotAllocInKernel)
                     { ImpGen.opsCopyCompiler = inKernelCopy
                     , ImpGen.opsExpCompiler = inKernelExpCompiler
                     }

compileProg :: MonadFreshNames m => Prog -> m (Either String Imp.Program)
compileProg prog =
  fmap (setDefaultSpace (Imp.Space "device")) <$>
  ImpGen.compileProg callKernelOperations (Imp.Space "device") prog

opCompiler :: ImpGen.Destination -> Op ExplicitMemory
              -> ImpGen.ImpM Imp.HostOp ()
opCompiler dest (Alloc e space) =
  ImpGen.compileAlloc dest e space
opCompiler dest (Inner kernel) =
  kernelCompiler dest kernel

cannotAllocInKernel :: ImpGen.Destination -> Op ExplicitMemory
                    -> ImpGen.ImpM Imp.KernelOp ()
cannotAllocInKernel _ _ =
  throwError "Cannot allocate memory in kernel."

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: ImpGen.Destination -> Kernel ExplicitMemory
               -> ImpGen.ImpM Imp.HostOp ()

kernelCompiler dest NumGroups = do
  [v] <- ImpGen.funcallTargets dest
  ImpGen.emit $ Imp.Op $ Imp.GetNumGroups v

kernelCompiler dest GroupSize = do
  [v] <- ImpGen.funcallTargets dest
  ImpGen.emit $ Imp.Op $ Imp.GetGroupSize v

kernelCompiler
  (ImpGen.Destination dest)
  (Kernel _ space _ kernel_body) = do

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
      space_dims' = map ImpGen.compileSubExp space_dims
      constants = KernelConstants global_tid local_tid group_id
                  (Imp.VarSize inner_group_size) num_threads'
                  (Imp.VarSize wave_size) (zip space_is space_dims')
                  (Imp.ScalarVar thread_active)

  kernel_body' <-
    makeAllMemoryGlobal $
    ImpGen.subImpM_ inKernelOperations $
    ImpGen.declaringPrimVar wave_size int32 $
    ImpGen.declaringPrimVar inner_group_size int32 $
    ImpGen.declaringPrimVar thread_active Bool $
    ImpGen.declaringScope (scopeOfKernelSpace space) $ do

    ImpGen.emit $
      Imp.Op (Imp.GetGlobalId global_tid 0) <>
      Imp.Op (Imp.GetLocalId local_tid 0) <>
      Imp.Op (Imp.GetLocalSize inner_group_size 0) <>
      Imp.Op (Imp.GetLockstepWidth wave_size) <>
      Imp.Op (Imp.GetGroupId group_id 0)

    setSpaceIndices space

    ImpGen.emit $ Imp.SetScalar thread_active (isActive $ spaceDimensions space)

    compileKernelBody dest constants kernel_body

  (uses, local_memory) <- computeKernelUses dest kernel_body' bound_in_kernel
  let local_memory_aligned = map (ensureAlignment $ alignmentMap kernel_body') local_memory

  ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.AnyKernel Imp.Kernel
            { Imp.kernelBody = kernel_body'
            , Imp.kernelLocalMemory = local_memory_aligned
            , Imp.kernelUses = uses
            , Imp.kernelNumGroups = num_groups'
            , Imp.kernelGroupSize = group_size'
            , Imp.kernelName = global_tid
            , Imp.kernelDesc = Nothing
            }

kernelCompiler
  (ImpGen.Destination dest)
  (ScanKernel _ _ kernel_size lam foldlam nes arrs) = do
    let (arrs_dest, partials_dest, map_dest) =
          splitAt3 (length nes) (length nes) dest
    (local_id, group_id, wave_size, _skip_waves, global_id) <- kernelSizeNames
    thread_chunk_size <- newVName "thread_chunk_size"
    chunks_per_group <- newVName "chunks_per_group"
    chunk_index <- newVName "chunk_index"

    renamed_lam <- renameLambda lam

    (num_groups, local_size, _elements_per_thread,
     num_elements, _offset_multiple, num_threads) <-
      compileKernelSize kernel_size

    let (fold_lam_i, _, fold_actual_params) =
          partitionChunkedKernelLambdaParameters $ lambdaParams foldlam
        (fold_x_params, fold_y_params) =
          splitAt (length nes) fold_actual_params
        (lam_i, other_index_param, actual_params) =
          partitionChunkedKernelLambdaParameters $ lambdaParams lam
        (x_params, y_params) =
          splitAt (length nes) actual_params
        is_last_thread_in_group = Imp.CmpOp (CmpEq int32)
                                  (Imp.ScalarVar local_id)
                                  (Imp.sizeToExp local_size - 1)
        is_first_thread_in_group = Imp.CmpOp (CmpEq int32)
                                   (Imp.ScalarVar local_id)
                                   0
        num_waves = Imp.sizeToExp local_size `quot`
                    Imp.ScalarVar wave_size

    (acc_mem_params, acc_local_mem) <-
      unzip <$> mapM (createAccMem local_size) fold_x_params

    (call_with_body, body) <-
      makeAllMemoryGlobal $ ImpGen.subImpM inKernelOperations $
      ImpGen.declaringPrimVar local_id int32 $
      ImpGen.declaringPrimVar group_id int32 $
      ImpGen.declaringPrimVar wave_size int32 $
      ImpGen.declaringPrimVar thread_chunk_size int32 $
      ImpGen.declaringPrimVar chunks_per_group int32 $
      ImpGen.declaringPrimVar global_id int32 $
      ImpGen.withParams acc_mem_params $
      ImpGen.declaringLParams (lambdaParams lam) $
      ImpGen.declaringLParams (lambdaParams renamed_lam) $
      ImpGen.declaringLParams (lambdaParams foldlam) $ do

        ImpGen.emit $
          Imp.Op (Imp.GetLocalId local_id 0) <>
          Imp.Op (Imp.GetGroupId group_id 0) <>
          Imp.Op (Imp.GetGlobalId global_id 0) <>
          Imp.Op (Imp.GetLockstepWidth wave_size) <>
          Imp.SetScalar chunks_per_group
            (Imp.sizeToExp num_elements `quotRoundingUp`
             Imp.sizeToExp num_threads)

        ImpGen.Destination fold_x_targets <- ImpGen.destinationFromParams fold_x_params

        let indexMapTarget (ImpGen.ArrayDestination
                              (ImpGen.CopyIntoMemory dest_loc) (_:dest_dims)) = do
              let dest_loc' = ImpGen.sliceArray dest_loc [ImpGen.varIndex fold_lam_i]
              return $ ImpGen.ArrayDestination (ImpGen.CopyIntoMemory dest_loc') $
                Nothing : dest_dims
            indexMapTarget _ =
              throwError "indexMapTarget: invalid target for map-out."

        mapout_targets <- mapM indexMapTarget map_dest
        let fold_dest = ImpGen.Destination $ fold_x_targets <> mapout_targets

        y_dest <- ImpGen.destinationFromParams y_params

        set_fold_x_to_ne <- ImpGen.collect $
          zipWithM_ ImpGen.compileSubExpTo fold_x_targets nes

        ImpGen.emit set_fold_x_to_ne

        scan_chunk <- ImpGen.collect $ do
          -- Every thread reads an element from the input array and
          -- applies the fold function, writing the result to
          -- x_params.  If the element is beyond the end of the
          -- array, we write the neutral element instead.

          -- Compute our element index.
          ImpGen.emit $ Imp.SetScalar fold_lam_i $
            (Imp.ScalarVar group_id * Imp.ScalarVar chunks_per_group +
             Imp.ScalarVar chunk_index) * Imp.sizeToExp local_size +
            Imp.ScalarVar local_id

          let readFoldElement param inp_arr =
                ImpGen.copyDWIM (paramName param) []
                (Var inp_arr) [ImpGen.varIndex fold_lam_i]

          apply_fold_fun <- ImpGen.collect $ do
            zipWithM_ readFoldElement fold_y_params arrs
            ImpGen.compileBody fold_dest $ lambdaBody foldlam

          let is_in_bounds = Imp.CmpOp (CmpUlt Int32)
                             (Imp.ScalarVar fold_lam_i) (Imp.sizeToExp num_elements)
          ImpGen.comment
            "Apply the fold function if we are still within bounds" $
            ImpGen.emit $ Imp.If is_in_bounds apply_fold_fun mempty

          -- Write the fold_x_params to local memory for the parallel step.
          zipWithM_ (writeParamToLocalMemory $ Imp.ScalarVar local_id)
            acc_local_mem fold_x_params

          let wave_id = Imp.BinOp (SQuot Int32)
                        (Imp.ScalarVar local_id)
                        (Imp.ScalarVar wave_size)
              in_wave_id = Imp.ScalarVar local_id -
                           (wave_id * Imp.ScalarVar wave_size)
              doInWaveScan = inWaveScan (Imp.ScalarVar wave_size) local_id acc_local_mem

          doInWaveScan lam
          ImpGen.emit $ Imp.Op Imp.Barrier

          pack_wave_results <-
            ImpGen.collect $
            zipWithM_ (writeParamToLocalMemory wave_id) acc_local_mem y_params

          let last_in_wave =
                Imp.CmpOp (CmpEq int32) in_wave_id $ Imp.ScalarVar wave_size - 1
          ImpGen.comment
            "last thread of wave 'i' writes its result to offset 'i'" $
            ImpGen.emit $ Imp.If last_in_wave pack_wave_results mempty

          ImpGen.emit $ Imp.Op Imp.Barrier

          let is_first_wave = Imp.CmpOp (CmpEq int32) wave_id 0
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

          op_to_y <- ImpGen.collect $ ImpGen.compileBody y_dest $ lambdaBody lam
          ImpGen.comment "carry-in for every wave except the first" $
            ImpGen.emit $ Imp.If is_first_wave mempty $
            Imp.Comment "read operands" read_carry_in <>
            Imp.Comment "perform operation" op_to_y

          write_final_elem <- ImpGen.collect $
            zipWithM_ (writeFinalResult [fold_lam_i]) arrs_dest y_params
          ImpGen.comment "Write element result if we are still within bounds" $
            ImpGen.emit $ Imp.If is_in_bounds write_final_elem mempty

          read_global_carry_in <-
            ImpGen.collect $
            zipWithM_ (readParamFromLocalMemory fold_lam_i $ num_waves - 1)
            fold_x_params acc_local_mem
          ImpGen.comment
            "The first thread in each workgroup reads the carry-in for the next iteration.  The others reset it to the neutral element." $
            ImpGen.emit $ Imp.If is_first_thread_in_group
            read_global_carry_in set_fold_x_to_ne
          ImpGen.comment
            "Make sure every thread is done with this chunk, as we will write to local memory at the beginning of the next iteration." $
            ImpGen.emit $ Imp.Op Imp.Barrier


        ImpGen.emit $ Imp.For chunk_index (Imp.ScalarVar chunks_per_group) scan_chunk

        write_global_carry_out <- ImpGen.collect $
          zipWithM_ (writeFinalResult [group_id]) partials_dest y_params
        ImpGen.comment "The last thread in each workgroup writes its result as the carry-out of the group." $
          ImpGen.emit $ Imp.If is_last_thread_in_group
          write_global_carry_out mempty

        return $ \body -> do

          let bound_in_kernel = HM.keys (scopeOf lam) ++
                                HM.keys (scopeOf foldlam) ++
                                HM.keys (scopeOf renamed_lam) ++
                                [local_id,
                                 group_id,
                                 global_id] ++
                                map Imp.paramName acc_mem_params

          (uses, more_local_mem) <- computeKernelUses dest (freeIn body) bound_in_kernel
          let local_mem = map (ensureAlignment $ alignmentMap body) $
                          acc_local_mem <> more_local_mem

          ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.AnyKernel Imp.Kernel
            { Imp.kernelBody = body
            , Imp.kernelLocalMemory = local_mem
            , Imp.kernelUses = uses
            , Imp.kernelNumGroups = num_groups
            , Imp.kernelGroupSize = local_size
            , Imp.kernelName = lam_i
            , Imp.kernelDesc = Just "scan"
            }

    call_with_body body

kernelCompiler
  (ImpGen.Destination dests)
  (WriteKernel _cs len lam ivs input) = do

  let len' = ImpGen.compileSubExp len
      as_sizes = map (ImpGen.compileSubExp . fst) input
      (tid_param, [], real_params) =
        partitionChunkedFoldParameters 0 $ lambdaParams lam
      global_thread_index = paramName tid_param
      get_thread_index =
        ImpGen.emit $ Imp.Op $ Imp.GetGlobalId global_thread_index 0

      check_thread_index body =
        let cond = Imp.CmpOp (CmpSlt Int32)
              (Imp.ScalarVar global_thread_index) len'
        in Imp.If cond body Imp.Skip

      -- Fake kernel inputs.
      inps = zipWith (makeInput $ Var global_thread_index) real_params ivs

      read_params = mapM_ readKernelInput inps

      kernel_bnds = bodyBindings $ lambdaBody lam

      res = bodyResult $ lambdaBody lam
      (indexes, values) = splitAt (length res `div` 2) res

      writeResult index val a_size dest = do
        let index' = ImpGen.compileSubExp index
            condOutOfBounds0 = Imp.CmpOp (Imp.CmpUlt Int32)
              index'
              (Imp.Constant (IntValue (Int32Value 0)))
            condOutOfBounds1 = Imp.CmpOp (Imp.CmpUle Int32)
              a_size
              index'
            condOutOfBounds = Imp.BinOp LogOr condOutOfBounds0 condOutOfBounds1

        actual_body' <- ImpGen.collect
          $ ImpGen.copyDWIMDest dest [SE.subExpToScalExp index int32] val []

        ImpGen.emit $ Imp.If condOutOfBounds Imp.Skip actual_body'

  makeAllMemoryGlobal $ do
    body <- ImpGen.subImpM_ inKernelOperations $
      ImpGen.declaringLParams (lambdaParams lam) $ do

      body_actual <- ImpGen.collect $ do
        ImpGen.comment "read kernel parameters"
          read_params
        ImpGen.comment "find indexes and values" $
          ImpGen.compileBindings kernel_bnds $
          forM_ (zip4 indexes values as_sizes dests) $ \(index, val, a_size, dest) ->
            ImpGen.comment "write the result" $
            writeResult index val a_size dest

      ImpGen.comment "get thread index" get_thread_index
      ImpGen.comment "run actual body if thread index is okay"
        $ ImpGen.emit $ check_thread_index body_actual

    -- Compute the variables that we need to pass to and from the kernel.
    (uses, _) <- computeKernelUses dests (len', body) []

    (group_size, num_groups) <- computeMapKernelGroups len'

    kernel_name <- newVName "a_write_kernel"
    ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.AnyKernel Imp.Kernel
      { Imp.kernelBody = body
      , Imp.kernelLocalMemory = mempty
      , Imp.kernelUses = uses
      , Imp.kernelNumGroups = Imp.VarSize num_groups
      , Imp.kernelGroupSize = Imp.VarSize group_size
      , Imp.kernelName = kernel_name
      , Imp.kernelDesc = Just "write"
      }
  where makeInput i p arr = KernelInput (paramName p) (paramType p) arr [i]

expCompiler :: ImpGen.ExpCompiler Imp.HostOp
-- We generate a simple kernel for itoa and replicate.
expCompiler
  (ImpGen.Destination
    [ImpGen.ArrayDestination (ImpGen.CopyIntoMemory destloc) _])
  (PrimOp (Iota n x s)) = do
  thread_gid <- newVName "thread_gid"

  makeAllMemoryGlobal $ do
    (destmem, destspace, destidx) <-
      ImpGen.fullyIndexArray' destloc [ImpGen.varIndex thread_gid] int32

    let n' = ImpGen.compileSubExp n
        x' = ImpGen.compileSubExp x
        s' = ImpGen.compileSubExp s

        body = Imp.Write destmem destidx int32 destspace $
               Imp.ScalarVar thread_gid * s' + x'

    (group_size, num_groups) <- computeMapKernelGroups n'

    (body_uses, _) <- computeKernelUses []
                      (freeIn body <> freeIn [n',x',s'])
                      [thread_gid]

    ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.Map Imp.MapKernel {
        Imp.mapKernelThreadNum = thread_gid
      , Imp.mapKernelNumGroups = Imp.VarSize num_groups
      , Imp.mapKernelGroupSize = Imp.VarSize group_size
      , Imp.mapKernelSize = n'
      , Imp.mapKernelUses = body_uses
      , Imp.mapKernelBody = body
      }
  return ImpGen.Done

expCompiler
  (ImpGen.Destination [dest]) (PrimOp (Replicate n se)) = do
  thread_gid <- newVName "replicate_gid"

  t <- subExpType se
  let row_dims = arrayDims t
      dims = n : row_dims
      is' = unflattenIndex (map SE.intSubExpToScalExp dims) $
            ImpGen.varIndex thread_gid
      n' = ImpGen.compileSubExp n

  makeAllMemoryGlobal $ do
    body <- ImpGen.subImpM_ inKernelOperations $
      ImpGen.copyDWIMDest dest is' se $ drop 1 is'

    (group_size, num_groups) <- computeMapKernelGroups $
                                product $ map ImpGen.compileSubExp dims

    (body_uses, _) <- computeKernelUses []
                      (freeIn body <> freeIn [n'])
                      [thread_gid]

    ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.Map Imp.MapKernel {
        Imp.mapKernelThreadNum = thread_gid
      , Imp.mapKernelNumGroups = Imp.VarSize num_groups
      , Imp.mapKernelGroupSize = Imp.VarSize group_size
      , Imp.mapKernelSize = product $ map ImpGen.compileSubExp dims
      , Imp.mapKernelUses = body_uses
      , Imp.mapKernelBody = body
      }
  return ImpGen.Done

-- Allocation in the "local" space is just a placeholder.
expCompiler _ (Op (Alloc _ (Space "local"))) =
  return ImpGen.Done

expCompiler _ e =
  return $ ImpGen.CompileExp e

kernelSizeNames :: ImpGen.ImpM op (VName, VName, VName, VName, VName)
kernelSizeNames = do
  local_id <- newVName "local_id"
  group_id <- newVName "group_id"
  wave_size <- newVName "wave_size"
  skip_waves <- newVName "skip_waves"
  global_id <- newVName "global_id"
  return (local_id, group_id, wave_size, skip_waves, global_id)

compileKernelSize :: KernelSize
                  -> ImpGen.ImpM op (Imp.DimSize, Imp.DimSize, Imp.DimSize,
                                     Imp.DimSize, Imp.DimSize, Imp.DimSize)
compileKernelSize (KernelSize num_groups local_size per_thread_elements
                   num_elements offset_multiple num_threads) = do
  num_groups' <- ImpGen.subExpToDimSize num_groups
  local_size' <- ImpGen.subExpToDimSize local_size
  per_thread_elements' <- ImpGen.subExpToDimSize per_thread_elements
  num_elements' <- ImpGen.subExpToDimSize num_elements
  offset_multiple' <- ImpGen.subExpToDimSize offset_multiple
  num_threads' <- ImpGen.subExpToDimSize num_threads
  return (num_groups', local_size', per_thread_elements',
          num_elements', offset_multiple', num_threads')

callKernelCopy :: ImpGen.CopyCompiler Imp.HostOp
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
      ImpGen.scalExpToImpExp =<<
      IxFun.linearWithOffset destIxFun bt_size,
    Just srcoffset  <-
      ImpGen.scalExpToImpExp =<<
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
      shape_se = map ImpGen.sizeToScalExp srcshape
      dest_is = unflattenIndex shape_se $ ImpGen.varIndex global_thread_index
      src_is = dest_is

  makeAllMemoryGlobal $ do
    (_, destspace, destidx) <- ImpGen.fullyIndexArray' destloc dest_is bt
    (_, srcspace, srcidx) <- ImpGen.fullyIndexArray' srcloc src_is bt

    let body = Imp.Write destmem destidx bt destspace $
               Imp.Index srcmem srcidx bt srcspace

    destmem_size <- ImpGen.entryMemSize <$> ImpGen.lookupMemory destmem
    let writes_to = [Imp.MemoryUse destmem destmem_size]

    reads_from <- readsFromSet $
                  HS.singleton srcmem <>
                  freeIn destIxFun <> freeIn srcIxFun <> freeIn destshape

    let kernel_size = Imp.innerExp n * product (drop 1 shape)
    (group_size, num_groups) <- computeMapKernelGroups kernel_size

    let bound_in_kernel = [global_thread_index]
    (body_uses, _) <- computeKernelUses [] (kernel_size, body) bound_in_kernel

    ImpGen.emit $ Imp.Op $ Imp.CallKernel $ Imp.Map Imp.MapKernel {
        Imp.mapKernelThreadNum = global_thread_index
      , Imp.mapKernelNumGroups = Imp.VarSize num_groups
      , Imp.mapKernelGroupSize = Imp.VarSize group_size
      , Imp.mapKernelSize = kernel_size
      , Imp.mapKernelUses = nub $ body_uses ++ writes_to ++ reads_from
      , Imp.mapKernelBody = body
      }

-- | We have no bulk copy operation (e.g. memmove) inside kernels, so
-- turn any copy into a loop.
inKernelCopy :: ImpGen.CopyCompiler Imp.KernelOp
inKernelCopy = ImpGen.copyElementWise

inKernelExpCompiler :: ImpGen.ExpCompiler Imp.KernelOp
inKernelExpCompiler _ (PrimOp (Assert _ loc)) =
  fail $ "Cannot compile assertion at " ++ locStr loc ++ " inside parallel kernel."
inKernelExpCompiler _ e =
  return $ ImpGen.CompileExp e

computeKernelUses :: FreeIn a =>
                     [ImpGen.ValueDestination]
                  -> a -> [VName]
                  -> ImpGen.ImpM op ([Imp.KernelUse], [(VName, Imp.Size)])
computeKernelUses dest kernel_body bound_in_kernel = do
    -- Find the memory blocks containing the output arrays.
    let dest_mems = mapMaybe destMem dest
        destMem (ImpGen.ArrayDestination
                 (ImpGen.CopyIntoMemory
                  (ImpGen.MemLocation mem _ _)) _) =
          Just mem
        destMem _ =
          Nothing

        actually_free = freeIn kernel_body `HS.difference`
                        HS.fromList (dest_mems <> bound_in_kernel)

    -- Compute the variables that we need to pass to the kernel.
    reads_from <- readsFromSet actually_free

    -- Compute what memory to copy out.  Must be allocated on device
    -- before kernel execution anyway.
    writes_to <- fmap catMaybes $ forM dest $ \case
      (ImpGen.ArrayDestination
       (ImpGen.CopyIntoMemory
        (ImpGen.MemLocation mem _ _)) _) -> do
        memsize <- ImpGen.entryMemSize <$> ImpGen.lookupMemory mem
        return $ Just $ Imp.MemoryUse mem memsize
      _ ->
        return Nothing

    -- Are we using any local memory?
    local_memory <- localMemoryUse actually_free
    return (nub $ reads_from ++ writes_to,
            nub local_memory)

readsFromSet :: Names -> ImpGen.ImpM op [Imp.KernelUse]
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
        if bt == Cert
        then return Nothing
        else return $ Just $ Imp.ScalarUse var bt

localMemoryUse :: Names -> ImpGen.ImpM op [(VName, Imp.Size)]
localMemoryUse free =
  fmap catMaybes $
  forM (HS.toList free) $ \var -> do
    t <- lookupType var
    case t of
      Mem memsize (Space "local") -> do
        memsize' <- ImpGen.subExpToDimSize memsize
        return $ Just (var, memsize')
      _ -> return Nothing

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
  where globalMemory (ImpGen.MemVar entry)
          | ImpGen.entryMemSpace entry /= Space "local" =
              ImpGen.MemVar entry { ImpGen.entryMemSpace = Imp.Space "global" }
        globalMemory entry =
          entry

computeMapKernelGroups :: Imp.Exp -> ImpGen.ImpM Imp.HostOp (VName, VName)
computeMapKernelGroups kernel_size = do
  group_size <- newVName "group_size"
  num_groups <- newVName "num_groups"
  let group_size_var = Imp.ScalarVar group_size
  ImpGen.emit $ Imp.DeclareScalar group_size int32
  ImpGen.emit $ Imp.DeclareScalar num_groups int32
  ImpGen.emit $ Imp.Op $ Imp.GetGroupSize group_size
  ImpGen.emit $ Imp.SetScalar num_groups $
    kernel_size `quotRoundingUp` group_size_var
  return (group_size, num_groups)

readKernelInput :: KernelInput -> InKernelGen ()
readKernelInput inp =
  when (primType t) $ do
    (srcmem, space, srcoffset) <-
      ImpGen.fullyIndexArray arr $ map SE.intSubExpToScalExp is
    ImpGen.emit $ Imp.SetScalar name $
      Imp.Index srcmem srcoffset (elemType t) space
  where arr = kernelInputArray inp
        name = kernelInputName inp
        t = kernelInputType inp
        is = kernelInputIndices inp

isMapTransposeKernel :: PrimType -> ImpGen.MemLocation -> ImpGen.MemLocation
                     -> Maybe (Imp.Exp, Imp.Exp,
                               Imp.Exp, Imp.Exp, Imp.Exp,
                               Imp.Exp, Imp.Exp)
isMapTransposeKernel bt
  (ImpGen.MemLocation _ _ destIxFun)
  (ImpGen.MemLocation _ _ srcIxFun)
  | Just (dest_offset, perm_and_destshape) <- IxFun.rearrangeWithOffset destIxFun bt_size,
    (perm, destshape) <- unzip perm_and_destshape,
    Just destshape' <- mapM ImpGen.scalExpToImpExp destshape,
    Just srcshape' <- mapM ImpGen.scalExpToImpExp $ IxFun.shape srcIxFun,
    Just src_offset <- IxFun.linearWithOffset srcIxFun bt_size,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk (product srcshape') (product destshape') destshape' swap r1 r2 dest_offset src_offset
  | Just dest_offset <- IxFun.linearWithOffset destIxFun bt_size,
    Just (src_offset, perm_and_srcshape) <- IxFun.rearrangeWithOffset srcIxFun bt_size,
    (perm, srcshape) <- unzip perm_and_srcshape,
    Just srcshape' <- mapM ImpGen.scalExpToImpExp srcshape,
    Just destshape' <- mapM ImpGen.scalExpToImpExp $ IxFun.shape destIxFun,
    Just (r1, r2, _) <- isMapTranspose perm =
    isOk (product srcshape') (product destshape') srcshape' id r1 r2 dest_offset src_offset
  | otherwise =
    Nothing
  where bt_size = primByteSize bt
        swap (x,y) = (y,x)

        isOk src_elems dest_elems shape f r1 r2 dest_offset src_offset = do
          dest_offset' <- ImpGen.scalExpToImpExp dest_offset
          src_offset' <- ImpGen.scalExpToImpExp src_offset
          let (num_arrays, size_x, size_y) = getSizes shape f r1 r2
          return (dest_offset', src_offset',
                  num_arrays, size_x, size_y,
                  src_elems, dest_elems)

        getSizes shape f r1 r2 =
          let (mapped, notmapped) = splitAt r1 shape
              (pretrans, posttrans) = f $ splitAt r2 notmapped
          in (product mapped, product pretrans, product posttrans)

createAccMem :: Imp.DimSize
             -> LParam
             -> ImpGen.ImpM op (Imp.Param, (VName, Imp.Size))
createAccMem local_size param
  | Prim bt <- paramType param = do
      mem_shared <- newVName (baseString (paramName param) <> "_mem_local")
      total_size <- newVName "total_size"
      ImpGen.emit $
        Imp.DeclareScalar total_size int32
      ImpGen.emit $
        Imp.SetScalar total_size $
        Imp.SizeOf bt * Imp.innerExp (ImpGen.dimSizeToExp local_size)
      return (Imp.MemParam mem_shared (Imp.VarSize total_size) $ Space "local",
              (mem_shared, Imp.VarSize total_size))
  | ArrayMem _ _ _ mem _ <- paramAttr param = do
      mem_size <-
        ImpGen.entryMemSize <$> ImpGen.lookupMemory mem
      return (Imp.MemParam mem mem_size $ Space "local",
              (mem, mem_size))
  | otherwise =
      fail $ "createAccMem: cannot deal with accumulator param " ++
      pretty param

writeParamToLocalMemory :: Typed (MemBound u) =>
                           Imp.Exp -> (VName, t) -> Param (MemBound u)
                        -> ImpGen.ImpM op ()
writeParamToLocalMemory i (mem, _) param
  | Prim _ <- paramType param =
      ImpGen.emit $
      Imp.Write mem (bytes i') bt (Space "local") $
      Imp.ScalarVar (paramName param)
  | otherwise =
      return ()
  where i' = i * Imp.SizeOf bt
        bt = elemType $ paramType param

readParamFromLocalMemory :: Typed (MemBound u) =>
                            VName -> Imp.Exp -> Param (MemBound u) -> (VName, t)
                         -> ImpGen.ImpM op ()
readParamFromLocalMemory index i param (l_mem, _)
  | Prim _ <- paramType param =
      ImpGen.emit $
      Imp.SetScalar (paramName param) $
      Imp.Index l_mem (bytes i') bt (Space "local")
  | otherwise =
      ImpGen.emit $
      Imp.SetScalar index i
  where i' = i * Imp.SizeOf bt
        bt = elemType $ paramType param

writeFinalResult :: Typed (MemBound u) =>
                    [VName]
                 -> ImpGen.ValueDestination
                 -> Param (MemBound u)
                 -> ImpGen.ImpM op ()
writeFinalResult is (ImpGen.ArrayDestination memdest _) acc_param
  | ImpGen.CopyIntoMemory
    memloc@(ImpGen.MemLocation out_arr_mem out_shape ixfun) <- memdest = do
      target <-
        case arrayDims $ paramType acc_param of
        [] -> do
          (_, space, offset) <-
            ImpGen.fullyIndexArray' memloc (map ImpGen.varIndex is) bt
          return $
            ImpGen.ArrayElemDestination out_arr_mem bt space offset
        ds -> do
          let destloc = ImpGen.MemLocation out_arr_mem (drop 1 out_shape) $
                        IxFun.applyInd ixfun $ map ImpGen.varIndex is
          return $
            ImpGen.ArrayDestination (ImpGen.CopyIntoMemory destloc) $
            map (const Nothing) ds
      ImpGen.compileSubExpTo target $ Var $ paramName acc_param
  where bt = elemType $ paramType acc_param
writeFinalResult _ _ _ =
  fail "writeFinalResult: invalid destination"

computeThreadChunkSize :: Commutativity
                       -> Imp.Exp
                       -> Imp.Exp
                       -> Imp.Count Imp.Elements
                       -> Imp.Count Imp.Elements
                       -> VName
                       -> ImpGen.ImpM op ()
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
    Imp.If (Imp.CmpOp (CmpSlt Int32)
            (Imp.innerExp elements_per_thread)
            (Imp.ScalarVar remaining_elements))
    (Imp.SetScalar chunk_var (Imp.innerExp elements_per_thread))
    (Imp.SetScalar chunk_var (Imp.ScalarVar remaining_elements))

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
    Imp.innerExp num_elements - Imp.ScalarVar starting_point

  let no_remaining_elements = Imp.CmpOp (CmpSle Int32)
                              (Imp.ScalarVar remaining_elements) 0
      beyond_bounds = Imp.CmpOp (CmpSle Int32)
                      (Imp.innerExp num_elements)
                      (Imp.ScalarVar starting_point)

  ImpGen.emit $
    Imp.If (Imp.BinOp LogOr no_remaining_elements beyond_bounds)
    (Imp.SetScalar chunk_var 0)
    (Imp.If is_last_thread
     (Imp.SetScalar chunk_var $ Imp.innerExp last_thread_elements)
     (Imp.SetScalar chunk_var $ Imp.innerExp elements_per_thread))
  where last_thread_elements =
          num_elements - Imp.elements thread_index * elements_per_thread
        is_last_thread =
          Imp.CmpOp (CmpSlt Int32)
          (Imp.innerExp num_elements)
          ((thread_index + 1) * Imp.innerExp elements_per_thread)

inWaveScan :: Imp.Exp
           -> VName
           -> [(VName, t)]
           -> Lambda
           -> ImpGen.ImpM op ()
inWaveScan wave_size local_id acc_local_mem scan_lam = do
  skip_threads <- newVName "skip_threads"
  let in_wave_thread_active =
        Imp.CmpOp (CmpSle Int32) (Imp.ScalarVar skip_threads) in_wave_id
      (scan_lam_i, other_index_param, actual_params) =
        partitionChunkedKernelLambdaParameters $ lambdaParams scan_lam
      (x_params, y_params) =
        splitAt (length actual_params `div` 2) actual_params
  read_operands <-
    ImpGen.collect $
    zipWithM_ (readParamFromLocalMemory (paramName other_index_param) $
               Imp.ScalarVar local_id -
               Imp.ScalarVar skip_threads)
    x_params acc_local_mem
  scan_y_dest <- ImpGen.destinationFromParams y_params

  -- Set initial y values
  zipWithM_ (readParamFromLocalMemory scan_lam_i $ Imp.ScalarVar local_id)
    y_params acc_local_mem

  op_to_y <- ImpGen.collect $ ImpGen.compileBody scan_y_dest $ lambdaBody scan_lam
  write_operation_result <-
    ImpGen.collect $
    zipWithM_ (writeParamToLocalMemory $ Imp.ScalarVar local_id)
    acc_local_mem y_params
  ImpGen.emit $
    Imp.Comment "in-wave scan (no barriers needed)" $
    Imp.DeclareScalar skip_threads int32 <>
    Imp.SetScalar skip_threads 1 <>
    Imp.While (Imp.CmpOp (CmpSlt Int32) (Imp.ScalarVar skip_threads) wave_size)
    (Imp.If in_wave_thread_active
     (Imp.Comment "read operands" read_operands <>
      Imp.Comment "perform operation" op_to_y <>
      Imp.Comment "write result" write_operation_result)
     mempty <>
     Imp.SetScalar skip_threads (Imp.ScalarVar skip_threads * 2))
  where wave_id = Imp.BinOp (SQuot Int32) (Imp.ScalarVar local_id) wave_size
        in_wave_id = Imp.ScalarVar local_id - wave_id * wave_size

type AlignmentMap = HM.HashMap VName PrimType

lookupAlignment :: VName -> AlignmentMap -> PrimType
lookupAlignment = HM.lookupDefault smallestType

smallestType :: PrimType
smallestType = Bool

alignmentMap :: Imp.KernelCode  -> AlignmentMap
alignmentMap = HM.map alignment . Imp.memoryUsage (const mempty)
  where alignment = HS.foldr mostRestrictive smallestType
        mostRestrictive bt1 bt2 =
          if (primByteSize bt1 :: Int) > primByteSize bt2
          then bt1 else bt2

ensureAlignment :: AlignmentMap
                -> (VName, Imp.Size)
                -> Imp.LocalMemoryUse
ensureAlignment alignments (name, size) =
  (name, size, lookupAlignment name alignments)

data KernelConstants = KernelConstants
                       { kernelGlobalThreadId :: VName
                       , kernelLocalThreadId :: VName
                       , kernelGroupId :: VName
                       , kernelGroupSize :: Imp.DimSize
                       , kernelNumThreads :: Imp.DimSize
                       , kernelWaveSize :: Imp.DimSize
                       , kernelDimensions :: [(VName, Imp.Exp)]
                       , kernelThreadActive :: Imp.Exp
                       }

compileKernelBody :: [ImpGen.ValueDestination]
                  -> KernelConstants
                  -> KernelBody ExplicitMemory
                  -> ImpGen.ImpM Imp.KernelOp ()
compileKernelBody dest constants kbody =
  compileKernelStms constants (kernelBodyStms kbody) $
  zipWithM_ (compileKernelResult constants) dest $
  kernelBodyResult kbody

compileNestedKernelBody :: [ImpGen.ValueDestination]
                        -> KernelConstants
                        -> NestedKernelBody ExplicitMemory
                        -> ImpGen.ImpM Imp.KernelOp ()
compileNestedKernelBody dest constants kbody =
  compileKernelStms constants (kernelBodyStms kbody) $
  zipWithM_ ImpGen.compileSubExpTo dest $ kernelBodyResult kbody

compileKernelStms :: KernelConstants
                  -> [KernelStm ExplicitMemory]
                  -> ImpGen.ImpM Imp.KernelOp ()
                  -> ImpGen.ImpM Imp.KernelOp ()
compileKernelStms constants ungrouped_stms m =
  compileGroupedKernelStms' $ groupStmsByGuard constants ungrouped_stms
  where compileGroupedKernelStms' [] = m
        compileGroupedKernelStms' ((g, stms):rest_stms) =
          ImpGen.declaringScope (scopeOf stms) $ do
            protect g =<< ImpGen.collect (mapM_ (compileKernelStm constants) stms)
            compileGroupedKernelStms' rest_stms

        protect Nothing body =
          ImpGen.emit body
        protect (Just g) body =
          ImpGen.emit $ Imp.If g body mempty

groupStmsByGuard :: KernelConstants
                 -> [KernelStm ExplicitMemory]
                 -> [(Maybe Imp.Exp, [KernelStm ExplicitMemory])]
groupStmsByGuard constants stms =
  map collapse $ groupBy sameGuard $ map addGuard stms
  where addGuard stm@(Thread threads _) =
          (protect threads, stm)
        addGuard stm =
          (Nothing, stm)

        sameGuard (g1, _) (g2, _) = g1 == g2

        collapse [] =
          (Nothing, [])
        collapse l@((g,_):_) =
          (g, map snd l)

        protect AllThreads = Nothing
        protect (OneThreadPerGroup which) =
          let which' = ImpGen.compileSubExp which
              me = Imp.ScalarVar $ kernelLocalThreadId constants
          in Just $ Imp.CmpOp (CmpEq int32) me which'
        protect (ThreadsPerGroup limit) =
          Just $ isActive limit
        protect ThreadsInSpace =
          Just $ kernelThreadActive constants

compileKernelStm :: KernelConstants -> KernelStm ExplicitMemory
                 -> ImpGen.ImpM Imp.KernelOp ()
compileKernelStm constants (SplitArray (size,_) o w elems_per_thread _arrs) = do
  let num_elements = Imp.elements $ ImpGen.compileSubExp w
      elems_per_thread' = Imp.elements $ ImpGen.compileSubExp elems_per_thread
      thread_index' = Imp.ScalarVar $ kernelGlobalThreadId constants
      num_threads' = Imp.sizeToExp $ kernelNumThreads constants
  computeThreadChunkSize comm thread_index' num_threads' elems_per_thread' num_elements size
  where comm = case o of Disorder -> Commutative
                         InOrder -> Noncommutative

compileKernelStm _ (Thread _ bnd) = do
  dest <- ImpGen.destinationFromPattern $ bindingPattern bnd
  ImpGen.compileExp dest (bindingExp bnd) $ return ()

compileKernelStm _ (Combine pe cspace v) = do
  copy <- ImpGen.collect $
          ImpGen.copyDWIM (patElemName pe) (map ImpGen.varIndex is) v []
  ImpGen.emit $ Imp.Op Imp.Barrier
  ImpGen.emit $ Imp.If (isActive cspace) copy mempty
  ImpGen.emit $ Imp.Op Imp.Barrier
  where (is, _) = unzip cspace

compileKernelStm constants (GroupReduce pes _ lam input) = do
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

    ImpGen.emit $ Imp.SetScalar reduce_i $ Imp.ScalarVar local_tid

    ImpGen.emit $ Imp.SetScalar offset 0
    zipWithM_ (readReduceArgument offset) reduce_acc_params arrs

    let read_reduce_args = zipWithM_ (readReduceArgument offset)
                           reduce_arr_params arrs
        reduce_acc_dest = ImpGen.Destination reduce_acc_targets

    reduce_op <- ImpGen.collect $ do
      ImpGen.comment "read array element" read_reduce_args
      ImpGen.compileBody reduce_acc_dest $ lambdaBody lam

    write_reduce_op_result <-
      ImpGen.collect $
      zipWithM_ (writeReduceOpResult local_tid)
      reduce_acc_params arrs

    let wave_size = Imp.sizeToExp $ kernelWaveSize constants
        group_size = Imp.sizeToExp $ kernelGroupSize constants
        wave_id = Imp.BinOp (SQuot Int32)
                  (Imp.ScalarVar local_tid)
                  wave_size
        in_wave_id = Imp.ScalarVar local_tid -
                     (wave_id * wave_size)
        num_waves = Imp.BinOp (SQuot Int32)
                    (group_size + wave_size - 1)
                    wave_size

        doing_in_wave_reductions =
                  Imp.CmpOp (CmpSlt Int32) (Imp.ScalarVar offset) wave_size
        apply_in_in_wave_iteration =
          Imp.CmpOp (CmpEq int32)
          (Imp.BinOp (And Int32) in_wave_id (2 * Imp.ScalarVar offset - 1)) 0
        in_wave_reductions =
          Imp.SetScalar offset 1 <>
          Imp.While doing_in_wave_reductions
            (Imp.If apply_in_in_wave_iteration
             (reduce_op <> write_reduce_op_result) mempty <>
             Imp.SetScalar offset (Imp.ScalarVar offset * 2))

        doing_cross_wave_reductions =
          Imp.CmpOp (CmpSlt Int32) (Imp.ScalarVar skip_waves) num_waves
        is_first_thread_in_wave =
          Imp.CmpOp (CmpEq int32) in_wave_id 0
        wave_not_skipped =
          Imp.CmpOp (CmpEq int32)
          (Imp.BinOp (And Int32) wave_id (2 * Imp.ScalarVar skip_waves - 1))
          0
        apply_in_cross_wave_iteration =
          Imp.BinOp LogAnd is_first_thread_in_wave wave_not_skipped
        cross_wave_reductions =
          Imp.SetScalar skip_waves 1 <>
          Imp.While doing_cross_wave_reductions
            (Imp.Op Imp.Barrier <>
             Imp.SetScalar offset (Imp.ScalarVar skip_waves * wave_size) <>
             Imp.If apply_in_cross_wave_iteration
             (reduce_op <> write_reduce_op_result) mempty <>
             Imp.SetScalar skip_waves (Imp.ScalarVar skip_waves * 2))

    ImpGen.emit $
      in_wave_reductions <> cross_wave_reductions

    forM_ (zip pes reduce_acc_params) $ \(pe, reduce_acc_param) ->
      ImpGen.copyDWIM (patElemName pe) [] (Var $ paramName reduce_acc_param) []
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

compileKernelStm constants (GroupStream pes w maxchunk lam accs _arrs) = do
  let GroupStreamLambda block_size block_offset acc_params arr_params body = lam
      w' = ImpGen.compileSubExp w
      block_offset' = Imp.ScalarVar block_offset
      max_block_size = ImpGen.compileSubExp maxchunk
  ImpGen.Destination acc_targets <- ImpGen.destinationFromParams acc_params

  ImpGen.declaringLParams (acc_params++arr_params) $
    ImpGen.declaringPrimVar block_size int32 $
    ImpGen.declaringPrimVar block_offset int32 $ do
    body' <- ImpGen.collect $ compileNestedKernelBody acc_targets constants body

    ImpGen.emit $ Imp.SetScalar block_offset 0
    zipWithM_ ImpGen.compileSubExpTo acc_targets accs

    let not_at_end =
          Imp.CmpOp (CmpSlt Int32) block_offset' w'
        set_block_size =
          Imp.If (Imp.CmpOp (CmpSlt Int32)
                   (w' - block_offset')
                   max_block_size)
          (Imp.SetScalar block_size (w' - block_offset'))
          (Imp.SetScalar block_size max_block_size)
        increase_offset =
          Imp.SetScalar block_offset $
          block_offset' + max_block_size

    ImpGen.Destination final_targets <-
      ImpGen.destinationFromPattern $ Pattern [] pes

    ImpGen.emit $
      Imp.While not_at_end $
      set_block_size <> body' <> increase_offset

    zipWithM_ ImpGen.compileSubExpTo final_targets $
      map (Var . paramName) acc_params

compileKernelResult :: KernelConstants -> ImpGen.ValueDestination -> KernelResult
                    -> ImpGen.ImpM Imp.KernelOp ()
compileKernelResult constants dest (ThreadsReturn (OneThreadPerGroup who) what) = do
  write_result <-
    ImpGen.collect $
    ImpGen.copyDWIMDest dest [ImpGen.varIndex $ kernelGroupId constants] what []

  let me = Imp.ScalarVar $ kernelLocalThreadId constants
  ImpGen.emit $
    Imp.If (Imp.CmpOp (CmpEq int32) me (ImpGen.compileSubExp who))
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
                        SE.intSubExpToScalExp per_thread_elems *
                        ImpGen.varIndex (kernelGlobalThreadId constants)
      dest' = ImpGen.ArrayDestination (ImpGen.CopyIntoMemory dest_loc_offset) x
  ImpGen.copyDWIMDest dest' [] (Var what) []

compileKernelResult constants dest (ConcatReturns Disorder _ _ what) = do
  ImpGen.ArrayDestination (ImpGen.CopyIntoMemory dest_loc) x <- return dest
  let dest_loc' = ImpGen.strideArray
                  (ImpGen.offsetArray dest_loc $
                   ImpGen.varIndex (kernelGlobalThreadId constants)) $
                  ImpGen.sizeToScalExp (kernelNumThreads constants)
      dest' = ImpGen.ArrayDestination (ImpGen.CopyIntoMemory dest_loc') x
  ImpGen.copyDWIMDest dest' [] (Var what) []

isActive :: [(VName, SubExp)] -> Imp.Exp
isActive limit = case actives of
                    [] -> Imp.Constant $ BoolValue False
                    x:xs -> foldl (Imp.BinOp LogAnd) x xs
  where (is, ws) = unzip limit
        actives = zipWith active is $ map ImpGen.compileSubExp ws
        active i = Imp.CmpOp (CmpSlt Int32) (Imp.ScalarVar i)

setSpaceIndices :: KernelSpace -> ImpGen.ImpM Imp.KernelOp ()
setSpaceIndices space =
  case spaceStructure space of
    FlatSpace is_and_dims -> do
      let (is, dims) = unzip is_and_dims
          dims' = map ImpGen.compileSubExp dims
          index_expressions = unflattenIndex dims' $ Imp.ScalarVar global_tid
      forM_ (zip is index_expressions) $ \(i, x) ->
        ImpGen.emit $ Imp.SetScalar i x
    NestedSpace is_and_dims -> do
      let (gtids, gdims, ltids, ldims) = unzip4 is_and_dims
          gdims' = map ImpGen.compileSubExp gdims
          ldims' = map ImpGen.compileSubExp ldims
          (gtid_es, ltid_es) = unzip $ unflattenNestedIndex gdims' ldims' $
                               Imp.ScalarVar global_tid
      forM_ (zip gtids gtid_es) $ \(i,e) ->
        ImpGen.emit $ Imp.SetScalar i e
      forM_ (zip ltids ltid_es) $ \(i,e) ->
        ImpGen.emit $ Imp.SetScalar i e
  where global_tid = spaceGlobalId space

unflattenNestedIndex :: IntegralCond num => [num] -> [num] -> num -> [(num,num)]
unflattenNestedIndex global_dims group_dims global_id =
  zip global_is local_is
  where num_groups_dims = zipWith quotRoundingUp global_dims group_dims
        group_size = product group_dims
        group_id = global_id `Futhark.Util.IntegralExp.quot` group_size
        local_id = global_id `Futhark.Util.IntegralExp.rem` group_size

        group_is = unflattenIndex num_groups_dims group_id
        local_is = unflattenIndex group_dims local_id
        global_is = zipWith (+) local_is $ zipWith (*) group_is group_dims
