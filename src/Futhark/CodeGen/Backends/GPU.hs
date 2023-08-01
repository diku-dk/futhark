{-# LANGUAGE QuasiQuotes #-}

-- | C code generation for GPU, in general.
--
-- This module generates codes that targets the tiny GPU API
-- abstraction layer we define in the runtime system.
module Futhark.CodeGen.Backends.GPU
  ( callKernel,
    copygpu2gpu,
  )
where

import Control.Monad
import Data.Bifunctor (bimap)
import Data.List (unzip4)
import Data.Maybe (catMaybes)
import Futhark.CodeGen.Backends.COpenCL.Boilerplate (sizeLoggingCode)
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.SimpleRep (primStorageType, toStorage)
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

genKernelFunction ::
  KernelName ->
  KernelSafety ->
  [C.Param] ->
  [(C.Exp, C.Exp)] ->
  GC.CompilerM op s Name
genKernelFunction kernel_name safety arg_params arg_set = do
  let kernel_fname = "gpu_kernel_" <> kernel_name
  GC.libDecl
    [C.cedecl|static int $id:kernel_fname
               (struct futhark_context* ctx,
                unsigned int grid_x, unsigned int grid_y, unsigned int grid_z,
                unsigned int block_x, unsigned int block_y, unsigned int block_z,
                unsigned int shared_bytes, $params:arg_params) {
    if (grid_x * grid_y * grid_z * block_x * block_y * block_z != 0) {
      void* args[$int:num_args] = { $inits:(failure_inits<>args_inits) };
      size_t args_sizes[$int:num_args] = { $inits:(failure_sizes<>args_sizes) };
      return gpu_launch_kernel(ctx, ctx->program->$id:kernel_name,
                               $string:(prettyString kernel_name),
                               (const typename int32_t[]){grid_x, grid_y, grid_z},
                               (const typename int32_t[]){block_x, block_y, block_z},
                               shared_bytes,
                               $int:num_args, args, args_sizes);
    }
    return FUTHARK_SUCCESS;
  }|]

  pure kernel_fname
  where
    num_args = numFailureParams safety + length arg_set
    expToInit e = [C.cinit|$exp:e|]
    (args_sizes, args_inits) = bimap (map expToInit) (map expToInit) $ unzip arg_set
    (failure_inits, failure_sizes) =
      unzip . take (numFailureParams safety) $
        [ ([C.cinit|&ctx->global_failure|], [C.cinit|sizeof(ctx->global_failure)|]),
          ([C.cinit|&ctx->failure_is_an_option|], [C.cinit|sizeof(ctx->failure_is_an_option)|]),
          ([C.cinit|&ctx->global_failure_args|], [C.cinit|sizeof(ctx->global_failure_args)|])
        ]

kernelConstToExp :: KernelConst -> C.Exp
kernelConstToExp (SizeConst key) =
  [C.cexp|*ctx->tuning_params.$id:key|]
kernelConstToExp (SizeMaxConst size_class) =
  [C.cexp|ctx->$id:field|]
  where
    field = "max_" <> prettyString size_class

compileGroupDim :: GroupDim -> GC.CompilerM op s C.Exp
compileGroupDim (Left e) = GC.compileExp e
compileGroupDim (Right kc) = pure $ kernelConstToExp kc

genLaunchKernel ::
  KernelSafety ->
  KernelName ->
  [KernelArg] ->
  [Exp] ->
  [GroupDim] ->
  GC.CompilerM op s ()
genLaunchKernel safety kernel_name args num_groups group_size = do
  (arg_params, arg_params_inits, call_args, shared_vars) <-
    unzip4 <$> zipWithM mkArgs [(0 :: Int) ..] args
  let (shared_sizes, shared_offsets) = unzip $ catMaybes shared_vars
      shared_offsets_sc = mkOffsets shared_sizes
      shared_args = zip shared_offsets shared_offsets_sc
      shared_bytes = last shared_offsets_sc
  forM_ shared_args $ \(arg, offset) ->
    GC.decl [C.cdecl|unsigned int $id:arg = $exp:offset;|]

  (grid_x, grid_y, grid_z) <- mkDims <$> mapM GC.compileExp num_groups
  (group_x, group_y, group_z) <- mkDims <$> mapM compileGroupDim group_size

  kernel_fname <- genKernelFunction kernel_name safety arg_params arg_params_inits

  GC.stm
    [C.cstm|{
           err = $id:kernel_fname(ctx,
                                  $exp:grid_x, $exp:grid_y, $exp:grid_z,
                                  $exp:group_x, $exp:group_y, $exp:group_z,
                                  $exp:shared_bytes,
                                  $args:call_args);
           if (err != FUTHARK_SUCCESS) { goto cleanup; }
           }|]

  when (safety >= SafetyFull) $
    GC.stm [C.cstm|ctx->failure_is_an_option = 1;|]
  where
    mkDims [] = ([C.cexp|0|], [C.cexp|0|], [C.cexp|0|])
    mkDims [x] = (x, [C.cexp|1|], [C.cexp|1|])
    mkDims [x, y] = (x, y, [C.cexp|1|])
    mkDims (x : y : z : _) = (x, y, z)
    addExp x y = [C.cexp|$exp:x + $exp:y|]
    alignExp e = [C.cexp|$exp:e + ((8 - ($exp:e % 8)) % 8)|]
    mkOffsets = scanl (\a b -> a `addExp` alignExp b) [C.cexp|0|]

    mkArgs i (ValueKArg e t) = do
      let arg = "arg" <> show i
      e' <- GC.compileExp e
      pure
        ( [C.cparam|$ty:(primStorageType t) $id:arg|],
          ([C.cexp|sizeof($id:arg)|], [C.cexp|&$id:arg|]),
          toStorage t e',
          Nothing
        )
    mkArgs i (MemKArg v) = do
      let arg = "arg" <> show i
      v' <- GC.rawMem v
      pure
        ( [C.cparam|typename gpu_mem $id:arg|],
          ([C.cexp|sizeof($id:arg)|], [C.cexp|&$id:arg|]),
          v',
          Nothing
        )
    mkArgs i (SharedMemoryKArg (Count c)) = do
      let arg = "arg" <> show i
      num_bytes <- GC.compileExp c
      size <- newVName "shared_size"
      offset <- newVName "shared_offset"
      GC.decl [C.cdecl|unsigned int $id:size = $exp:num_bytes;|]
      pure
        ( [C.cparam|unsigned int $id:arg|],
          ([C.cexp|sizeof($id:arg)|], [C.cexp|&$id:arg|]),
          [C.cexp|$id:offset|],
          Just (size, offset)
        )

callKernel :: GC.OpCompiler OpenCL ()
callKernel (GetSize v key) = do
  let e = kernelConstToExp $ SizeConst key
  GC.stm [C.cstm|$id:v = $exp:e;|]
callKernel (CmpSizeLe v key x) = do
  let e = kernelConstToExp $ SizeConst key
  x' <- GC.compileExp x
  GC.stm [C.cstm|$id:v = $exp:e <= $exp:x';|]
  sizeLoggingCode v key x'
callKernel (GetSizeMax v size_class) = do
  let e = kernelConstToExp $ SizeMaxConst size_class
  GC.stm [C.cstm|$id:v = $exp:e;|]
callKernel (LaunchKernel safety kernel_name args num_groups group_size) =
  genLaunchKernel safety kernel_name args num_groups group_size

copygpu2gpu :: GC.DoLMADCopy op s
copygpu2gpu _ t shape dst (dstoffset, dststride) src (srcoffset, srcstride) = do
  let fname :: String
      fname =
        case primByteSize t :: Int of
          1 -> "lmad_copy_gpu2gpu_1b"
          2 -> "lmad_copy_gpu2gpu_2b"
          4 -> "lmad_copy_gpu2gpu_4b"
          8 -> "lmad_copy_gpu2gpu_8b"
          k -> error $ "copygpu2gpu: " <> error (show k)
      r = length shape
      dststride_inits = [[C.cinit|$exp:e|] | Count e <- dststride]
      srcstride_inits = [[C.cinit|$exp:e|] | Count e <- srcstride]
      shape_inits = [[C.cinit|$exp:e|] | Count e <- shape]
  GC.stm
    [C.cstm|
         if ((err =
                $id:fname(ctx, $int:r,
                          $exp:dst, $exp:(unCount dstoffset),
                          (typename int64_t[]){ $inits:dststride_inits },
                          $exp:src, $exp:(unCount srcoffset),
                          (typename int64_t[]){ $inits:srcstride_inits },
                          (typename int64_t[]){ $inits:shape_inits })) != 0) {
           goto cleanup;
         }
     |]
