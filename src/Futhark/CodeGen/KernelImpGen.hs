{-# LANGUAGE TypeFamilies, LambdaCase #-}
module Futhark.CodeGen.KernelImpGen
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
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.KernelImp as Imp
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
                    , ImpGen.opsCopyCompiler = copyCompiler
                    }

inKernelOperations :: ImpGen.Operations Imp.InKernel
inKernelOperations = ImpGen.defaultOperations

compileProg :: Prog -> Either String Imp.Program
compileProg = liftM (setDefaultSpace (Imp.Space "device")) .
              ImpGen.compileProg callKernelOperations
              (Imp.Space "device")

-- | Recognise kernels (maps), give everything else back.
kernelCompiler :: ImpGen.ExpCompiler Imp.CallKernel

kernelCompiler
  (ImpGen.Destination dest)
  (LoopOp (Kernel _ w global_thread_index ispace inps returns body)) = do

  kernel_size <- ImpGen.subExpToDimSize w

  let global_thread_index_param = Imp.ScalarParam global_thread_index Int
      shape = map (ImpGen.compileSubExp . snd) ispace
      indices = map fst ispace

  let indices_lparams = [ Param (Ident index $ Basic Int) Scalar | index <- indices ]
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
    kernel_body <- ImpGen.subImpM_ inKernelOperations $
                   ImpGen.withParams [global_thread_index_param] $
                   ImpGen.declaringLParams (indices_lparams++map kernelInputParam inps) $ do
                     ImpGen.comment "compute thread index" set_indices
                     ImpGen.comment "read kernel parameters" read_params
                     ImpGen.compileBindings kernel_bnds $
                      ImpGen.comment "write kernel result" write_result

    -- Compute the variables that we need to pass to and from the
    -- kernel.
    uses <- computeKernelUses dest kernel_body bound_in_kernel

    ImpGen.emit $ Imp.Op $ Imp.Kernel Imp.MapKernel {
        Imp.kernelThreadNum = global_thread_index
      , Imp.kernelBody = kernel_body
      , Imp.kernelUses = uses
      , Imp.kernelSize = kernel_size
      }
    return ImpGen.Done

kernelCompiler
  (ImpGen.Destination dest)
  (LoopOp (ReduceKernel _ w kernel_size reduce_lam fold_lam nes _)) = do

    local_id <- newVName "local_id"
    group_id <- newVName "group_id"
    global_id <- newVName "global_id"
    global_size <- newVName "global_size"
    offset <- newVName "offset"
    (num_groups, group_size, per_thread_chunk) <- compileKernelSize kernel_size

    let fold_bnds = bodyBindings $ lambdaBody fold_lam
        fold_lparams = lambdaParams fold_lam
        (fold_chunk_param, _) =
          partitionChunkedLambdaParameters $ lambdaParams fold_lam

        reduce_bnds = bodyBindings $ lambdaBody reduce_lam
        reduce_lparams = lambdaParams reduce_lam
        (reduce_acc_params, reduce_arr_params) =
          splitAt (length nes) $ lambdaParams reduce_lam

    ((local_mems_params, perthread_mems_params, acc_local_mem),
     compute_total_sizes) <-
      ImpGen.subImpM inKernelOperations $
      unzip3 <$> mapM createAccMem reduce_acc_params
    let acc_mem_params = local_mems_params <> perthread_mems_params
        num_elements = ImpGen.compileSubExp w

    (call_with_prologue, prologue) <-
      makeAllMemoryGlobal $ ImpGen.subImpM inKernelOperations $
      ImpGen.withBasicVar local_id Int $
      ImpGen.withBasicVar offset Int $
      ImpGen.declaringBasicVar local_id Int $
      ImpGen.declaringBasicVar group_id Int $
      ImpGen.declaringBasicVar global_id Int $
      ImpGen.declaringBasicVar global_size Int $
      ImpGen.withParams acc_mem_params $
      ImpGen.declaringLParams (fold_lparams++reduce_lparams) $ do

        ImpGen.emit $
          Imp.Op (Imp.GetLocalId local_id 0) <>
          Imp.Op (Imp.GetGroupId group_id 0) <>
          Imp.Op (Imp.GetGlobalSize global_size 0) <>
          Imp.Op (Imp.GetGlobalId global_id 0) <>
          compute_total_sizes

        let write_fold_op_result = zipWithM_ writeOpResult
                                   reduce_acc_params $
                                   bodyResult $ lambdaBody fold_lam

        apply_fold_op <-
          ImpGen.subImpM_ inKernelOperations $ do
            computeThreadChunkSize
              (ImpGen.dimSizeToExp per_thread_chunk) num_elements
              (Imp.ScalarVar global_id) $ paramName fold_chunk_param
            ImpGen.compileBindings fold_bnds $
              ImpGen.comment "write fold result" write_fold_op_result

        neutral_fold_op <-
          ImpGen.collect $ zipWithM_ writeOpResult reduce_acc_params nes

        let thread_is_in_bounds =
              Imp.BinOp Less
              (Imp.ScalarVar global_id *
               ImpGen.innerExp (ImpGen.dimSizeToExp per_thread_chunk))
              (ImpGen.compileSubExp w)
            fold_op = Imp.If thread_is_in_bounds apply_fold_op neutral_fold_op

        write_fold_result <-
          ImpGen.subImpM_ inKernelOperations $
          zipWithM_ (writeFoldResult local_id) acc_local_mem reduce_acc_params

        let read_reduce_args = zipWithM_ (readReduceArgument local_id offset)
                               reduce_arr_params acc_local_mem
            write_reduce_result = zipWithM_ writeOpResult
                                  reduce_acc_params $
                                  bodyResult $ lambdaBody reduce_lam

        init_accum <-
          ImpGen.subImpM_ inKernelOperations $
          zipWithM readAccParam reduce_acc_params nes

        reduce_op <-
          ImpGen.subImpM_ inKernelOperations $ do
            ImpGen.comment "read array element" read_reduce_args
            ImpGen.compileBindings reduce_bnds $
              ImpGen.comment "write reduction result" write_reduce_result

        (output_params, write_result) <-
          ImpGen.subImpM inKernelOperations $
          zipWithM (writeFinalResult group_id) dest reduce_acc_params

        let local_mem = acc_local_mem
            bound_in_kernel = map paramName (lambdaParams fold_lam ++
                                             lambdaParams reduce_lam) ++
                              [lambdaIndex fold_lam,
                               lambdaIndex reduce_lam,
                               offset,
                               local_id,
                               group_id,
                               global_size,
                               global_id] ++
                              map fst output_params ++
                              map Imp.paramName local_mems_params

        return $ \prologue -> do
          uses <- computeKernelUses dest [freeIn prologue,
                                          freeIn fold_op,
                                          freeIn write_fold_result,
                                          freeIn reduce_op,
                                          freeIn write_result
                                          ]
                  bound_in_kernel

          ImpGen.emit $ Imp.Op $ Imp.Reduce Imp.ReduceKernel
            { Imp.reductionThreadNum = lambdaIndex fold_lam
            , Imp.reductionOffsetName = offset
            , Imp.reductionOutputParams = output_params
            , Imp.reductionThreadLocalMemory = local_mem

            , Imp.reductionPrologue = prologue
            , Imp.reductionFoldOperation = fold_op
            , Imp.reductionWriteFoldResult = write_fold_result
            , Imp.reductionInitAccumulator = init_accum
            , Imp.reductionReduceOperation = reduce_op
            , Imp.reductionWriteFinalResult = write_result

            , Imp.reductionNumGroups = num_groups
            , Imp.reductionGroupSize = group_size

            , Imp.reductionUses = uses
            }
          return ImpGen.Done
    call_with_prologue prologue
  where createAccMem param
          | Basic bt <- paramType param = do
              mem_shared <- newVName (baseString (paramName param) <> "_mem_shared")
              mem_perthread <- newVName (baseString (paramName param) <> "_mem_perthread")
              let perthread_size = Imp.ConstSize 1
              total_size <- newVName "total_size"
              return (Imp.MemParam mem_shared (Imp.VarSize total_size) $ Space "local",
                      Imp.MemParam mem_perthread perthread_size $ Space "local",
                      (Imp.VarSize total_size,
                       perthread_size,
                       bt,
                       mem_shared,
                       mem_perthread))
          | otherwise =
            fail "createAccMem: non-basic accumulator not supported"

        readAccParam param ne
          | Basic _ <- paramType param =
              ImpGen.emit $ Imp.SetScalar (paramName param) $
                ImpGen.compileSubExp ne
          | otherwise =
            fail "readAccParam: non-basic accumulator not supported"

        writeOpResult param res
          | Basic _ <- paramType param =
              ImpGen.emit $ Imp.SetScalar (paramName param) $
                ImpGen.compileSubExp res
          | otherwise =
            fail "writeOpResult: non-basic result not supported"


        writeFoldResult local_id (_, _, bt, mem_shared, _mem_local) param
          | Basic _ <- paramType param =
              ImpGen.emit $
              Imp.Write mem_shared (Imp.ScalarVar local_id) bt (Space "local") $
              Imp.ScalarVar (paramName param)
          | otherwise =
              fail "writeFoldResult: non-basic result not supported"

        readReduceArgument local_id offset param (_, _, bt, mem_shared, _mem_local)
          | Basic _ <- paramType param =
              ImpGen.emit $
              Imp.SetScalar (paramName param) $
              Imp.Index mem_shared i bt (Space "local")
          | otherwise =
              fail "readReduceArgument: non-basic argument not supported"
          where i = Imp.ScalarVar local_id + Imp.ScalarVar offset

        writeFinalResult group_id (ImpGen.ArrayDestination memloc _) acc_param
          | Basic bt <- paramType acc_param,
            ImpGen.CopyIntoMemory (ImpGen.MemLocation out_arr_mem _ _ixfun) <- memloc = do
              let target = ImpGen.ArrayElemDestination
                           out_arr_mem bt (Imp.Space "global") $
                           ImpGen.bytes $ Imp.SizeOf bt * Imp.ScalarVar group_id
              ImpGen.compileResultSubExp target $ Var $ paramName acc_param
              return (out_arr_mem, Imp.ConstSize $ basicSize bt)
        writeFinalResult _ _ _ =
          fail "writeFinalResult: non-basic argument not supported"

-- We generate a simple kernel for itoa and replicate.
kernelCompiler target (PrimOp (Iota n)) = do
  i <- newVName "i"
  global_thread_index <- newVName "global_thread_index"
  kernelCompiler target $
    LoopOp $ Kernel [] n global_thread_index [(i,n)] [] [(Basic Int,[0])] (Body () [] [Var i])
kernelCompiler target (PrimOp (Replicate n v)) = do
  i <- newVName "i"
  global_thread_index <- newVName "global_thread_index"
  t <- subExpType v
  kernelCompiler target $
    LoopOp $ Kernel [] n global_thread_index [(i,n)] [] [(t,[0..arrayRank t])] (Body () [] [v])
kernelCompiler _ e =
  return $ ImpGen.CompileExp e

compileKernelSize :: KernelSize -> ImpGen.ImpM op (Imp.DimSize, Imp.DimSize, Imp.DimSize)
compileKernelSize (KernelSize num_groups group_size per_thread_elements) = do
  num_groups' <- ImpGen.subExpToDimSize num_groups
  group_size' <- ImpGen.subExpToDimSize group_size
  per_thread_elements' <- ImpGen.subExpToDimSize per_thread_elements
  return (num_groups', group_size', per_thread_elements')

copyCompiler :: ImpGen.CopyCompiler Imp.CallKernel
copyCompiler bt
  destloc@(ImpGen.MemLocation destmem destshape destIxFun)
  srcloc@(ImpGen.MemLocation srcmem _ srcIxFun)

  | Just (destoffset, srcoffset,
          num_arrays, size_x, size_y) <- isMapTranspose bt destloc srcloc =
  ImpGen.emit $ Imp.Op $ Imp.MapTranspose bt destmem destoffset srcmem srcoffset
  num_arrays size_x size_y

  | otherwise = do
  global_thread_index <- newVName "copy_global_thread_index"

  -- Note that the shape of the destination and the source are
  -- necessarily the same.
  let shape = map ImpGen.sizeToExp destshape
      shape_se = map ImpGen.sizeToScalExp destshape
      dest_is = unflattenIndex shape_se $ ImpGen.varIndex global_thread_index
      src_is = dest_is

  makeAllMemoryGlobal $ do
    (_, destspace, destidx) <- ImpGen.fullyIndexArray' destloc dest_is bt
    (_, srcspace, srcidx) <- ImpGen.fullyIndexArray' srcloc src_is bt

    let body = ImpGen.write destmem destidx bt destspace $
               ImpGen.index srcmem srcidx bt srcspace

    destmem_size <- ImpGen.entryMemSize <$> ImpGen.lookupMemory destmem
    let writes_to = [Imp.MemoryUse destmem destmem_size]

    reads_from <- readsFromSet $
                  HS.singleton srcmem <>
                  freeIn destIxFun <> freeIn srcIxFun <> freeIn destshape

    kernel_size <- newVName "copy_kernel_size"
    ImpGen.emit $ Imp.DeclareScalar kernel_size Int
    ImpGen.emit $ Imp.SetScalar kernel_size $ product shape

    ImpGen.emit $ Imp.Op $ Imp.Kernel Imp.MapKernel {
        Imp.kernelThreadNum = global_thread_index
      , Imp.kernelSize = Imp.VarSize kernel_size
      , Imp.kernelUses = nub $ reads_from ++ writes_to
      , Imp.kernelBody = body
      }

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
      ImpGen.index srcmem srcoffset (elemType t) space
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
  | Just (dest_offset, perm) <- IxFun.rearrangeWithOffset destIxFun,
    Just src_offset <- IxFun.linearWithOffset srcIxFun bt_size,
    permIsTranspose perm =
    isOk dest_offset src_offset
  | Just dest_offset <- IxFun.linearWithOffset destIxFun bt_size,
    Just (src_offset, perm) <- IxFun.rearrangeWithOffset srcIxFun,
    permIsTranspose perm  =
    isOk dest_offset src_offset
  | otherwise =
    Nothing
  where bt_size = ImpGen.basicScalarSize bt
        permIsTranspose = (`elem` [ [0, 2, 1], [1,0] ])

        isOk dest_offset src_offset = do
          dest_offset' <- ImpGen.scalExpToImpExp dest_offset
          src_offset' <- ImpGen.scalExpToImpExp src_offset
          (num_arrays, size_x, size_y) <- getSizes
          return (dest_offset', src_offset',
                  num_arrays, size_x, size_y)
        getSizes =
          case map ImpGen.sizeToExp destshape of
            [num_arrays, size_x, size_y] -> Just (num_arrays, size_x, size_y)
            [size_x, size_y]             -> Just (1, size_x, size_y)
            _                            -> Nothing

computeThreadChunkSize :: ImpGen.Count ImpGen.Elements -> Imp.Exp -> Imp.Exp
                       -> VName
                       -> ImpGen.ImpM op ()
computeThreadChunkSize chunk_per_thread num_elements thread_index chunk_var =
  ImpGen.emit $
  Imp.If is_last_thread
  (Imp.SetScalar chunk_var last_thread_chunk)
  (Imp.SetScalar chunk_var chunk_per_thread')
  where last_thread_chunk =
          num_elements - thread_index * chunk_per_thread'
        is_last_thread =
          Imp.BinOp Less num_elements ((thread_index + 1) * chunk_per_thread')
        chunk_per_thread' = ImpGen.innerExp chunk_per_thread
