{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, LambdaCase #-}
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

import Prelude

import Futhark.MonadFreshNames
import Futhark.Transform.Rename
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.CodeGen.ImpCode.Kernels (bytes)
import qualified Futhark.CodeGen.ImpGen as ImpGen
import Futhark.Analysis.ScalExp as SE
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import Futhark.CodeGen.SetDefaultSpace
import Futhark.Tools (partitionChunkedLambdaParameters)

type CallKernelGen = ImpGen.ImpM Imp.CallKernel
type InKernelGen = ImpGen.ImpM Imp.InKernel

callKernelOperations :: ImpGen.Operations Imp.CallKernel
callKernelOperations =
  ImpGen.Operations { ImpGen.opsExpCompiler = kernelCompiler
                    , ImpGen.opsCopyCompiler = callKernelCopy
                    }

inKernelOperations :: ImpGen.Operations Imp.InKernel
inKernelOperations = ImpGen.defaultOperations
                     { ImpGen.opsCopyCompiler = inKernelCopy }

compileProg :: Prog -> Either String Imp.Program
compileProg = liftM (setDefaultSpace (Imp.Space "device")) .
              ImpGen.compileProg callKernelOperations
              (Imp.Space "device")

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: ImpGen.ExpCompiler Imp.CallKernel

kernelCompiler (ImpGen.Destination [ImpGen.MemoryDestination mem size]) (Op (Alloc e space)) = do
  ImpGen.compileAlloc mem size e space
  return ImpGen.Done

kernelCompiler
  (ImpGen.Destination dest)
  (LoopOp (MapKernel _ _ global_thread_index ispace inps returns body)) = do

  let kernel_size = product $ map (ImpGen.compileSubExp . snd) ispace

      global_thread_index_param = Imp.ScalarParam global_thread_index Int
      shape = map (ImpGen.compileSubExp . snd) ispace
      indices = map fst ispace

      indices_lparams = [ Param index (Scalar Int) | index <- indices ]
      bound_in_kernel = global_thread_index : indices ++ map kernelInputName inps
      kernel_bnds = bodyBindings body

      index_expressions = unflattenIndex shape $ Imp.ScalarVar global_thread_index
      set_indices = forM_ (zip indices index_expressions) $ \(i, x) ->
        ImpGen.emit $ Imp.SetScalar i x

      read_params = mapM_ readKernelInput inps

      perms = map snd returns
      write_result =
        sequence_ $ zipWith3 (writeThreadResult indices) perms dest $ bodyResult body

  makeAllMemoryGlobal $ do
    kernel_body <- liftM (setBodySpace $ Imp.Space "global") $
                   ImpGen.subImpM_ inKernelOperations $
                   ImpGen.withParams [global_thread_index_param] $
                   ImpGen.declaringLParams (indices_lparams ++ map kernelInputParam inps) $ do
                     ImpGen.comment "compute thread index" set_indices
                     ImpGen.comment "read kernel parameters" read_params
                     ImpGen.compileBindings kernel_bnds $
                      ImpGen.comment "write kernel result" write_result

    -- Compute the variables that we need to pass to and from the
    -- kernel.
    uses <- computeKernelUses dest kernel_body bound_in_kernel

    ImpGen.emit $ Imp.Op $ Imp.Map Imp.MapKernel {
        Imp.mapKernelThreadNum = global_thread_index
      , Imp.mapKernelBody = kernel_body
      , Imp.mapKernelUses = uses
      , Imp.mapKernelSize = kernel_size
      }
    return ImpGen.Done

kernelCompiler
  (ImpGen.Destination dest)
  (LoopOp (ReduceKernel _ _ kernel_size reduce_lam fold_lam nes _)) = do

    local_id <- newVName "local_id"
    group_id <- newVName "group_id"
    wave_size <- newVName "wave_size"
    skip_waves <- newVName "skip_waves"

    (num_groups, group_size, per_thread_chunk, num_elements, _, _) <-
      compileKernelSize kernel_size

    let fold_lparams = lambdaParams fold_lam
        (fold_chunk_param, _) =
          partitionChunkedLambdaParameters $ lambdaParams fold_lam

        reduce_lparams = lambdaParams reduce_lam
        (other_index_param, actual_reduce_params) =
          partitionChunkedLambdaParameters $ lambdaParams reduce_lam
        (reduce_acc_params, reduce_arr_params) =
          splitAt (length nes) actual_reduce_params

        offset = paramName other_index_param

    (acc_mem_params, acc_local_mem) <-
      unzip <$> mapM (createAccMem group_size) reduce_acc_params

    (call_with_prologue, prologue) <-
      makeAllMemoryGlobal $ ImpGen.subImpM inKernelOperations $
      ImpGen.withBasicVar local_id Int $
      ImpGen.declaringBasicVar local_id Int $
      ImpGen.declaringBasicVar group_id Int $
      ImpGen.declaringBasicVar wave_size Int $
      ImpGen.declaringBasicVar skip_waves Int $
      ImpGen.declaringBasicVar (lambdaIndex reduce_lam) Int $
      ImpGen.declaringBasicVar (lambdaIndex fold_lam) Int $
      ImpGen.withParams acc_mem_params $
      ImpGen.declaringLParams (fold_lparams++reduce_lparams) $ do

        ImpGen.emit $
          Imp.Op (Imp.GetLocalId local_id 0) <>
          Imp.Op (Imp.GetGroupId group_id 0) <>
          Imp.Op (Imp.GetGlobalId (lambdaIndex reduce_lam) 0) <>
          Imp.Op (Imp.GetGlobalId (lambdaIndex fold_lam) 0) <>
          Imp.Op (Imp.GetWaveSize wave_size)

        reduce_acc_dest <- ImpGen.destinationFromParams reduce_acc_params

        fold_op <-
          ImpGen.subImpM_ inKernelOperations $ do
            computeThreadChunkSize
              (Imp.ScalarVar $ lambdaIndex fold_lam)
              (ImpGen.dimSizeToExp per_thread_chunk)
              (ImpGen.dimSizeToExp num_elements) $
              paramName fold_chunk_param
            ImpGen.compileBody reduce_acc_dest $ lambdaBody fold_lam

        write_fold_result <-
          ImpGen.subImpM_ inKernelOperations $
          zipWithM_ (writeParamToLocalMemory $ Imp.ScalarVar local_id)
          acc_local_mem reduce_acc_params

        let read_reduce_args = zipWithM_ (readReduceArgument local_id offset)
                               reduce_arr_params acc_local_mem

        reduce_op <-
          ImpGen.subImpM_ inKernelOperations $ do
            ImpGen.comment "read array element" read_reduce_args
            ImpGen.compileBody reduce_acc_dest $ lambdaBody reduce_lam

        write_result <-
          ImpGen.subImpM_ inKernelOperations $
          zipWithM_ (writeFinalResult [group_id]) dest reduce_acc_params

        let bound_in_kernel = map paramName (lambdaParams fold_lam ++
                                             lambdaParams reduce_lam) ++
                              [lambdaIndex fold_lam,
                               lambdaIndex reduce_lam,
                               offset,
                               local_id,
                               group_id] ++
                              map Imp.paramName acc_mem_params

        return $ \prologue -> do
          -- wave_id, in_wave_id and num_waves will all be inlined
          -- whereever they are used.  This leads to ugly code, but
          -- declaring them as variables (and setting them) early in
          -- the kernel dramatically reduces performance on NVIDIAs
          -- OpenCL implementation.  I suspect it prevents unrolling
          -- of the in-wave reduction loop.  It is possible that we
          -- may be able to declare these as variables just preceding
          -- the loops where they are used, without losing
          -- performance.  This can be done when we become tired of
          -- looking at ugly kernel code.
          let wave_id = Imp.BinOp Quot
                        (Imp.ScalarVar local_id)
                        (Imp.ScalarVar wave_size)
              in_wave_id = Imp.ScalarVar local_id -
                           (wave_id * Imp.ScalarVar wave_size)
              num_waves = Imp.BinOp Quot
                          (Imp.innerExp (Imp.dimSizeToExp group_size) +
                           Imp.ScalarVar wave_size - 1)
                          (Imp.ScalarVar wave_size)

              doing_in_wave_reductions =
                Imp.BinOp Less (Imp.ScalarVar offset) $ Imp.ScalarVar wave_size
              apply_in_in_wave_iteration =
                Imp.BinOp Equal
                (Imp.BinOp Band in_wave_id (2 * Imp.ScalarVar offset - 1)) 0
              in_wave_reductions =
                Imp.SetScalar offset 1 <>
                Imp.While doing_in_wave_reductions
                  (Imp.If apply_in_in_wave_iteration
                   (reduce_op <> write_fold_result) mempty <>
                   Imp.SetScalar offset (Imp.ScalarVar offset * 2))

              doing_cross_wave_reductions =
                Imp.BinOp Less (Imp.ScalarVar skip_waves) num_waves
              is_first_thread_in_wave =
                Imp.BinOp Equal in_wave_id 0
              wave_not_skipped =
                Imp.BinOp Equal (Imp.BinOp Band wave_id
                                 (2 * Imp.ScalarVar skip_waves - 1))
                0
              apply_in_cross_wave_iteration =
                Imp.BinOp LogAnd is_first_thread_in_wave wave_not_skipped
              cross_wave_reductions =
                Imp.SetScalar skip_waves 1 <>
                Imp.While doing_cross_wave_reductions
                  (Imp.Op Imp.Barrier <>
                   Imp.SetScalar offset (Imp.ScalarVar skip_waves *
                                         Imp.ScalarVar wave_size) <>
                   Imp.If apply_in_cross_wave_iteration
                   (reduce_op <> write_fold_result) mempty <>
                   Imp.SetScalar skip_waves (Imp.ScalarVar skip_waves * 2))

              write_group_result =
                Imp.If (Imp.BinOp Equal (Imp.ScalarVar local_id) 0)
                write_result mempty

              body = mconcat [prologue,
                              fold_op,
                              write_fold_result,
                              in_wave_reductions,
                              cross_wave_reductions,
                              write_group_result]

              local_mem = map (ensureAlignment $ alignmentMap body) acc_local_mem

          uses <- computeKernelUses dest (freeIn body) bound_in_kernel

          ImpGen.emit $ Imp.Op $ Imp.CallKernel Imp.Kernel
            { Imp.kernelBody = body
            , Imp.kernelLocalMemory = local_mem
            , Imp.kernelUses = uses
            , Imp.kernelNumGroups = num_groups
            , Imp.kernelGroupSize = group_size
            , Imp.kernelName = lambdaIndex fold_lam
            }
          return ImpGen.Done
    call_with_prologue prologue
  where readReduceArgument local_id offset param (mem, _)
          | Basic _ <- paramType param =
              ImpGen.emit $
                Imp.SetScalar (paramName param) $
                Imp.Index mem (bytes i) bt (Space "local")
          | otherwise =
              return ()
          where i = (Imp.ScalarVar local_id + Imp.ScalarVar offset) * Imp.SizeOf bt
                bt = elemType $ paramType param

kernelCompiler
  (ImpGen.Destination dest)
  (LoopOp (ScanKernel _ _ kernel_size order lam input)) = do
    let (nes, arrs) = unzip input
        (arrs_dest, partials_dest) = splitAt (length input) dest
    local_id <- newVName "local_id"
    group_id <- newVName "group_id"
    wave_size <- newVName "wave_size"
    global_id <- newVName "global_id"
    thread_chunk_size <- newVName "thread_chunk_size"

    renamed_lam <- renameLambda lam

    (num_groups, local_size, elements_per_thread, num_elements, _, _) <-
      compileKernelSize kernel_size

    let (other_index_param, actual_params) =
          partitionChunkedLambdaParameters $ lambdaParams lam
        (x_params, y_params) =
          splitAt (length nes) actual_params

    (acc_mem_params, acc_local_mem) <-
      unzip <$> mapM (createAccMem local_size) x_params

    (call_with_body, body) <-
      makeAllMemoryGlobal $ ImpGen.subImpM inKernelOperations $
      ImpGen.declaringBasicVar local_id Int $
      ImpGen.declaringBasicVar group_id Int $
      ImpGen.declaringBasicVar wave_size Int $
      ImpGen.declaringBasicVar thread_chunk_size Int $
      ImpGen.declaringBasicVar (lambdaIndex lam) Int $
      ImpGen.declaringBasicVar global_id Int $
      ImpGen.withParams acc_mem_params $
      ImpGen.declaringLParams (lambdaParams lam) $
      ImpGen.declaringLParams (lambdaParams renamed_lam) $ do

        ImpGen.emit $
          Imp.Op (Imp.GetLocalId local_id 0) <>
          Imp.Op (Imp.GetGroupId group_id 0) <>
          Imp.Op (Imp.GetGlobalId global_id 0) <>
          Imp.Op (Imp.GetWaveSize wave_size)

        -- 'lambdaIndex lam' is the offset of the element that the
        -- current thread is responsible for.  Since a single
        -- workgroup processes more elements than it has threads, this
        -- will change over time.
        ImpGen.emit $
          Imp.SetScalar (lambdaIndex lam) $
          Imp.ScalarVar global_id *
          Imp.innerExp (Imp.dimSizeToExp elements_per_thread)

        x_dest <- ImpGen.destinationFromParams x_params
        y_dest <- ImpGen.destinationFromParams y_params

        let readScanElement param inp_arr
              | Basic _ <- paramType param = do
                  let global_i = ImpGen.varIndex $ lambdaIndex lam
                  read_input <-
                    ImpGen.readFromArray inp_arr [global_i]
                  ImpGen.emit $
                    Imp.SetScalar (paramName param) read_input
              | otherwise =
                  fail "readScanElement: cannot handle array accumulator yet."

        computeThreadChunkSize
          (Imp.ScalarVar global_id)
          (ImpGen.dimSizeToExp elements_per_thread)
          (ImpGen.dimSizeToExp num_elements)
          thread_chunk_size
        elements_scanned <- newVName "elements_scanned"

        zipWithM_ ImpGen.compileResultSubExp
          (ImpGen.valueDestinations x_dest) nes

        read_params <-
          ImpGen.collect $ zipWithM_ readScanElement y_params arrs

        let indices = case order of
              ScanTransposed -> [elements_scanned, global_id]
              ScanFlat       -> [global_id, elements_scanned]
            writeScanElement (ImpGen.ArrayDestination
                              (ImpGen.CopyIntoMemory (ImpGen.MemLocation mem dims ixfun))
                              setdims) =
              writeFinalResult indices $
              ImpGen.ArrayDestination
              (ImpGen.CopyIntoMemory (ImpGen.MemLocation mem dims ixfun'))
              setdims
              where ixfun' = explodeOuterDimension
                             (Shape $ map sizeToSubExp dims)
                             (kernelElementsPerThread kernel_size)
                             (kernelNumThreads kernel_size)
                             ixfun
            writeScanElement _ =
              const $ fail "writeScanElement: invalid destination"

            sizeToSubExp (Imp.ConstSize k) = Constant $ IntVal k
            sizeToSubExp (Imp.VarSize v)   = Var v

        write_arrs <-
          ImpGen.collect $ zipWithM_ writeScanElement arrs_dest x_params

        op_to_x <- ImpGen.collect $ ImpGen.compileBody x_dest $ lambdaBody lam
        ImpGen.emit $
          Imp.Comment "sequentially scan a chunk" $
          Imp.For elements_scanned (Imp.ScalarVar thread_chunk_size) $
            read_params <>
            op_to_x <>
            write_arrs <>
            Imp.SetScalar (lambdaIndex lam)
            (Imp.BinOp Plus (Imp.ScalarVar $ lambdaIndex lam) 1)

        zipWithM_ (writeParamToLocalMemory $ Imp.ScalarVar local_id)
          acc_local_mem x_params

        let wave_id = Imp.BinOp Quot
                      (Imp.ScalarVar local_id)
                      (Imp.ScalarVar wave_size)
            in_wave_id = Imp.ScalarVar local_id -
                         (wave_id * Imp.ScalarVar wave_size)
            inWaveScan' = inWaveScan (Imp.ScalarVar wave_size) local_id acc_local_mem

        inWaveScan' lam
        ImpGen.emit $ Imp.Op Imp.Barrier

        pack_wave_results <-
          ImpGen.collect $
          zipWithM_ (writeParamToLocalMemory wave_id) acc_local_mem y_params

        let last_in_wave =
              Imp.BinOp Equal in_wave_id $ Imp.ScalarVar wave_size - 1
        ImpGen.emit $ Imp.If last_in_wave pack_wave_results mempty

        ImpGen.emit $ Imp.Op Imp.Barrier

        let is_first_wave = Imp.BinOp Equal wave_id 0
        scan_first_wave <- ImpGen.collect $ inWaveScan' renamed_lam
        ImpGen.emit $ Imp.If is_first_wave scan_first_wave mempty

        ImpGen.emit $ Imp.Op Imp.Barrier

        read_carry_in <-
          ImpGen.collect $
          zipWithM_ (readParamFromLocalMemory
                     (paramName other_index_param) (wave_id - 1))
          x_params acc_local_mem

        op_to_y <- ImpGen.collect $ ImpGen.compileBody y_dest $ lambdaBody lam
        ImpGen.emit $
          Imp.If is_first_wave mempty $
          Imp.Comment "read operands" read_carry_in <>
          Imp.Comment "perform operation" op_to_y

        zipWithM_ (writeFinalResult [group_id, local_id]) partials_dest y_params

        return $ \body -> do

          let local_mem = map (ensureAlignment $ alignmentMap body) acc_local_mem
              bound_in_kernel = map paramName (lambdaParams lam) ++
                                map paramName (lambdaParams renamed_lam) ++
                                [lambdaIndex lam,
                                 local_id,
                                 group_id,
                                 global_id] ++
                                map Imp.paramName acc_mem_params

          uses <- computeKernelUses dest (freeIn body) bound_in_kernel

          ImpGen.emit $ Imp.Op $ Imp.CallKernel Imp.Kernel
            { Imp.kernelBody = body
            , Imp.kernelLocalMemory = local_mem
            , Imp.kernelUses = uses
            , Imp.kernelNumGroups = num_groups
            , Imp.kernelGroupSize = local_size
            , Imp.kernelName = lambdaIndex lam
            }
          return ImpGen.Done

    call_with_body body

-- We generate a simple kernel for itoa and replicate.
kernelCompiler target (PrimOp (Iota n)) = do
  i <- newVName "i"
  global_thread_index <- newVName "global_thread_index"
  kernelCompiler target $
    LoopOp $ MapKernel [] n global_thread_index [(i,n)] [] [(Basic Int,[0])] (Body () [] [Var i])

kernelCompiler target (PrimOp (Replicate n se)) = do
  global_thread_index <- newVName "global_thread_index"
  t <- subExpType se
  let row_rank = arrayRank t
      row_dims = arrayDims t
  i <- newVName "i"
  js <- replicateM row_rank $ newVName "j"
  let indices = (i,n) : zip js row_dims
  kernelCompiler target =<<
    case se of
      Var v | row_rank > 0 -> do
        input_name <- newVName "input"
        let input = KernelInput (Param input_name $ Scalar $ elemType t)
                    v (map Var js)
        return $
          LoopOp $ MapKernel [] n global_thread_index indices [input]
          [(t,[0..row_rank])] (Body () [] [Var input_name])
      _ ->
        return $
        LoopOp $ MapKernel [] n global_thread_index [(i,n)] []
        [(t,[0..arrayRank t])] (Body () [] [se])

-- Allocation in the "local" space is just a placeholder.
kernelCompiler _ (Op (Alloc _ (Space "local"))) =
  return ImpGen.Done

kernelCompiler _ e =
  return $ ImpGen.CompileExp e

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

callKernelCopy :: ImpGen.CopyCompiler Imp.CallKernel
callKernelCopy bt
  destloc@(ImpGen.MemLocation destmem destshape destIxFun)
  srcloc@(ImpGen.MemLocation srcmem srcshape srcIxFun)
  n
  | Just (destoffset, srcoffset,
          num_arrays, size_x, size_y) <- isMapTranspose bt destloc srcloc =
  ImpGen.emit $ Imp.Op $ Imp.MapTranspose bt destmem destoffset srcmem srcoffset
  num_arrays size_x size_y

  | bt_size <- ImpGen.basicScalarSize bt,
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
  let shape = map Imp.sizeToExp destshape
      shape_se = map ImpGen.sizeToScalExp destshape
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

    kernel_size <- newVName "copy_kernel_size"
    ImpGen.emit $ Imp.DeclareScalar kernel_size Int
    ImpGen.emit $ Imp.SetScalar kernel_size $
      Imp.innerExp n * product (drop 1 shape)

    ImpGen.emit $ Imp.Op $ Imp.Map Imp.MapKernel {
        Imp.mapKernelThreadNum = global_thread_index
      , Imp.mapKernelSize = Imp.ScalarVar kernel_size
      , Imp.mapKernelUses = nub $ reads_from ++ writes_to
      , Imp.mapKernelBody = body
      }

-- | We have no bulk copy operation (e.g. memmove) inside kernels, so
-- turn any copy into a loop.
inKernelCopy :: ImpGen.CopyCompiler Imp.InKernel
inKernelCopy = ImpGen.copyElementWise

computeKernelUses :: FreeIn a =>
                     [ImpGen.ValueDestination]
                  -> a -> [VName]
                  -> ImpGen.ImpM op [Imp.KernelUse]
computeKernelUses dest kernel_body bound_in_kernel = do
    -- Find the memory blocks containing the output arrays.
    let dest_mems = mapMaybe destMem dest
        destMem (ImpGen.ArrayDestination
                 (ImpGen.CopyIntoMemory
                  (ImpGen.MemLocation mem _ _)) _) =
          Just mem
        destMem _ =
          Nothing

    -- Compute the variables that we need to pass to the kernel.
    reads_from <- readsFromSet $
                  freeIn kernel_body `HS.difference`
                  HS.fromList (dest_mems <> bound_in_kernel)

    -- Compute what memory to copy out.  Must be allocated on device
    -- before kernel execution anyway.
    writes_to <- liftM catMaybes $ forM dest $ \case
      (ImpGen.ArrayDestination
       (ImpGen.CopyIntoMemory
        (ImpGen.MemLocation mem _ _)) _) -> do
        memsize <- ImpGen.entryMemSize <$> ImpGen.lookupMemory mem
        return $ Just $ Imp.MemoryUse mem memsize
      _ ->
        return Nothing
    return $ nub $ reads_from ++ writes_to

readsFromSet :: Names -> ImpGen.ImpM op [Imp.KernelUse]
readsFromSet free =
  liftM catMaybes $
  forM (HS.toList free) $ \var -> do
    t <- lookupType var
    case t of
      Array {} -> return Nothing
      Mem _ (Space "local") -> return Nothing
      Mem memsize _ -> Just <$> (Imp.MemoryUse var <$>
                                 ImpGen.subExpToDimSize memsize)
      Basic bt ->
        if bt == Cert
        then return Nothing
        else return $ Just $ Imp.ScalarUse var bt

-- | Change every memory block to be in the global address space.
-- This is fairly hacky and can be improved once the Futhark-level
-- memory representation supports address spaces.  This only affects
-- generated code - we still need to make sure that the memory is
-- actually present on the device (and declared as variables in the
-- kernel).
makeAllMemoryGlobal :: CallKernelGen a
                    -> CallKernelGen a
makeAllMemoryGlobal =
  local $ \env -> env { ImpGen.envVtable = HM.map globalMemory $ ImpGen.envVtable env
                      , ImpGen.envDefaultSpace = Imp.Space "global"
                      }
  where globalMemory (ImpGen.MemVar entry) =
          ImpGen.MemVar entry { ImpGen.entryMemSpace = Imp.Space "global" }
        globalMemory entry =
          entry

writeThreadResult :: [VName] -> [Int] -> ImpGen.ValueDestination -> SubExp
                  -> InKernelGen ()
writeThreadResult thread_idxs perm
  (ImpGen.ArrayDestination
   (ImpGen.CopyIntoMemory
    (ImpGen.MemLocation mem dims ixfun)) _) se = do
  set <- subExpType se

  let ixfun' = IxFun.permute ixfun perm
      destloc' = ImpGen.MemLocation mem (rearrangeShape perm dims) ixfun'

  space <- ImpGen.entryMemSpace <$> ImpGen.lookupMemory mem
  let is = map ImpGen.varIndex thread_idxs
  case set of
    Basic bt -> do
      (_, _, elemOffset) <-
        ImpGen.fullyIndexArray' destloc' is bt
      ImpGen.compileResultSubExp (ImpGen.ArrayElemDestination mem bt space elemOffset) se
    _ -> do
      memloc <- ImpGen.indexArray destloc' is
      let dest = ImpGen.ArrayDestination (ImpGen.CopyIntoMemory memloc) $
                 replicate (arrayRank set) Nothing
      ImpGen.compileResultSubExp dest se
writeThreadResult _ _ _ _ =
  fail "Cannot handle kernel that does not return an array."

readKernelInput :: KernelInput ExplicitMemory
                -> InKernelGen ()
readKernelInput inp =
  when (basicType t) $ do
    (srcmem, space, srcoffset) <-
      ImpGen.fullyIndexArray arr $ map SE.intSubExpToScalExp is
    ImpGen.emit $ Imp.SetScalar name $
      Imp.Index srcmem srcoffset (elemType t) space
  where arr = kernelInputArray inp
        name = kernelInputName inp
        t = kernelInputType inp
        is = kernelInputIndices inp

isMapTranspose :: BasicType -> ImpGen.MemLocation -> ImpGen.MemLocation
               -> Maybe (Imp.Exp, Imp.Exp,
                         Imp.Exp, Imp.Exp, Imp.Exp)
isMapTranspose bt
  (ImpGen.MemLocation _ destshape destIxFun)
  (ImpGen.MemLocation _ _ srcIxFun)
  | Just (dest_offset, perm) <- IxFun.rearrangeWithOffset destIxFun bt_size,
    Just src_offset <- IxFun.linearWithOffset srcIxFun bt_size,
    permIsTranspose perm =
    isOk dest_offset src_offset
  | Just dest_offset <- IxFun.linearWithOffset destIxFun bt_size,
    Just (src_offset, perm) <- IxFun.rearrangeWithOffset srcIxFun bt_size,
    permIsTranspose perm  =
    isOk dest_offset src_offset
  | otherwise =
    Nothing
  where bt_size = ImpGen.basicScalarSize bt
        permIsTranspose = (`elem` [ [0,2,1], [1,0] ])

        isOk dest_offset src_offset = do
          dest_offset' <- ImpGen.scalExpToImpExp dest_offset
          src_offset' <- ImpGen.scalExpToImpExp src_offset
          (num_arrays, size_x, size_y) <- getSizes
          return (dest_offset', src_offset',
                  num_arrays, size_x, size_y)
        getSizes =
          case map Imp.sizeToExp destshape of
            [num_arrays, size_x, size_y] -> Just (num_arrays, size_x, size_y)
            [size_x, size_y]             -> Just (1, size_x, size_y)
            _                            -> Nothing

createAccMem :: Imp.DimSize
             -> LParam
             -> ImpGen.ImpM op (Imp.Param, (VName, Imp.Size))
createAccMem local_size param
  | Basic bt <- paramType param = do
      mem_shared <- newVName (baseString (paramName param) <> "_mem_local")
      total_size <- newVName "total_size"
      ImpGen.emit $
        Imp.DeclareScalar total_size Int
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
  | Basic _ <- paramType param =
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
  | Basic _ <- paramType param =
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
      ImpGen.compileResultSubExp target $ Var $ paramName acc_param
  where bt = elemType $ paramType acc_param
writeFinalResult _ _ _ =
  fail "writeFinalResult: invalid destination"

computeThreadChunkSize :: Imp.Exp
                       -> Imp.Count Imp.Elements
                       -> Imp.Count Imp.Elements
                       -> VName
                       -> ImpGen.ImpM op ()
computeThreadChunkSize thread_index elements_per_thread num_elements chunk_var = do
  starting_point <- newVName "starting_point"
  remaining_elements <- newVName "remaining_elements"

  ImpGen.emit $
    Imp.DeclareScalar starting_point Int
  ImpGen.emit $
    Imp.SetScalar starting_point $
    thread_index * Imp.innerExp elements_per_thread

  ImpGen.emit $
    Imp.DeclareScalar remaining_elements Int
  ImpGen.emit $
    Imp.SetScalar remaining_elements $
    Imp.innerExp num_elements - Imp.ScalarVar starting_point

  let no_remaining_elements = Imp.BinOp Leq (Imp.ScalarVar remaining_elements) 0
      beyond_bounds = Imp.BinOp Leq (Imp.innerExp num_elements) (Imp.ScalarVar starting_point)

  ImpGen.emit $
    Imp.If (Imp.BinOp LogOr no_remaining_elements beyond_bounds)
    (Imp.SetScalar chunk_var 0)
    (Imp.If is_last_thread
     (Imp.SetScalar chunk_var $ Imp.innerExp last_thread_elements)
     (Imp.SetScalar chunk_var $ Imp.innerExp elements_per_thread))
  where last_thread_elements =
          num_elements - Imp.elements thread_index * elements_per_thread
        is_last_thread =
          Imp.BinOp Less (Imp.innerExp num_elements) ((thread_index + 1) * Imp.innerExp elements_per_thread)

inWaveScan :: Imp.Exp
           -> VName
           -> [(VName, t)]
           -> Lambda
           -> ImpGen.ImpM op ()
inWaveScan wave_size local_id acc_local_mem scan_lam = do
  skip_threads <- newVName "skip_threads"
  let in_wave_thread_active =
        Imp.BinOp Leq (Imp.ScalarVar skip_threads) in_wave_id
      (other_index_param, actual_params) =
        partitionChunkedLambdaParameters $ lambdaParams scan_lam
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
  zipWithM_ (readParamFromLocalMemory (lambdaIndex scan_lam) $ Imp.ScalarVar local_id)
    y_params acc_local_mem

  op_to_y <- ImpGen.collect $ ImpGen.compileBody scan_y_dest $ lambdaBody scan_lam
  write_operation_result <-
    ImpGen.collect $
    zipWithM_ (writeParamToLocalMemory $ Imp.ScalarVar local_id)
    acc_local_mem y_params
  ImpGen.emit $
    Imp.Comment "in-wave scan (no barriers needed)" $
    Imp.DeclareScalar skip_threads Int <>
    Imp.SetScalar skip_threads 1 <>
    Imp.While (Imp.BinOp Less (Imp.ScalarVar skip_threads) wave_size)
    (Imp.If in_wave_thread_active
     (Imp.Comment "read operands" read_operands <>
      Imp.Comment "perform operation" op_to_y <>
      Imp.Comment "write result" write_operation_result)
     mempty <>
     Imp.SetScalar skip_threads (Imp.ScalarVar skip_threads * 2))
  where wave_id = Imp.BinOp Quot (Imp.ScalarVar local_id) wave_size
        in_wave_id = Imp.ScalarVar local_id - wave_id * wave_size

type AlignmentMap = HM.HashMap VName BasicType

lookupAlignment :: VName -> AlignmentMap -> BasicType
lookupAlignment = HM.lookupDefault smallestType

smallestType :: BasicType
smallestType = Bool

alignmentMap :: Imp.KernelCode  -> AlignmentMap
alignmentMap = HM.map alignment . Imp.memoryUsage (const mempty)
  where alignment = HS.foldr mostRestrictive smallestType
        mostRestrictive bt1 bt2 =
          if (basicSize bt1 :: Int) > basicSize bt2
          then bt1 else bt2

ensureAlignment :: AlignmentMap
                -> (VName, Imp.Size)
                -> (VName, Imp.Size, BasicType)
ensureAlignment alignments (name, size) =
  (name, size, lookupAlignment name alignments)

explodeOuterDimension :: Shape -> SubExp -> SubExp -> IxFun.IxFun -> IxFun.IxFun
explodeOuterDimension orig_shape n m ixfun =
  IxFun.reshape ixfun explode_dims
  where explode_dims = reshapeOuter [DimNew n, DimNew m] 1 orig_shape
