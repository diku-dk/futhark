{-# LANGUAGE QuasiQuotes #-}

-- | C code generation for GPU, in general.
--
-- This module generates codes that targets the tiny GPU API
-- abstraction layer we define in the runtime system.
module Futhark.CodeGen.Backends.GPU
  ( gpuOperations,
    gpuOptions,
    generateGPUBoilerplate,
  )
where

import Control.Monad
import Control.Monad.Identity
import Data.Bifunctor (bimap)
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Pretty (expText, idText)
import Futhark.CodeGen.Backends.SimpleRep (primStorageType, toStorage)
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.RTS.C (gpuH, gpuPrototypesH)
import Futhark.MonadFreshNames
import Futhark.Util (chunk)
import Futhark.Util.Pretty (prettyTextOneLine)
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

getParamByKey :: Name -> C.Exp
getParamByKey key = [C.cexp|*ctx->tuning_params.$id:key|]

kernelConstToExp :: KernelConst -> C.Exp
kernelConstToExp (SizeConst key _) =
  getParamByKey key
kernelConstToExp (SizeMaxConst size_class) =
  [C.cexp|ctx->$id:field|]
  where
    field = "max_" <> prettyString size_class

compileBlockDim :: BlockDim -> GC.CompilerM op s C.Exp
compileBlockDim (Left e) = GC.compileExp e
compileBlockDim (Right kc) = pure $ kernelConstToExp kc

genLaunchKernel ::
  KernelSafety ->
  KernelName ->
  Count Bytes (TExp Int64) ->
  [KernelArg] ->
  [Exp] ->
  [BlockDim] ->
  GC.CompilerM op s ()
genLaunchKernel safety kernel_name shared_memory args num_tblocks tblock_size = do
  (arg_params, arg_params_inits, call_args) <-
    unzip3 <$> zipWithM mkArgs [(0 :: Int) ..] args

  (grid_x, grid_y, grid_z) <- mkDims <$> mapM GC.compileExp num_tblocks
  (block_x, block_y, block_z) <- mkDims <$> mapM compileBlockDim tblock_size

  kernel_fname <- genKernelFunction kernel_name safety arg_params arg_params_inits

  shared_memory' <- GC.compileExp $ untyped $ unCount shared_memory

  GC.stm
    [C.cstm|{
           err = $id:kernel_fname(ctx,
                                  $exp:grid_x, $exp:grid_y, $exp:grid_z,
                                  $exp:block_x, $exp:block_y, $exp:block_z,
                                  $exp:shared_memory',
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

    mkArgs i (ValueKArg e t) = do
      let arg = "arg" <> show i
      e' <- GC.compileExp e
      pure
        ( [C.cparam|$ty:(primStorageType t) $id:arg|],
          ([C.cexp|sizeof($id:arg)|], [C.cexp|&$id:arg|]),
          toStorage t e'
        )
    mkArgs i (MemKArg v) = do
      let arg = "arg" <> show i
      v' <- GC.rawMem v
      pure
        ( [C.cparam|typename gpu_mem $id:arg|],
          ([C.cexp|sizeof($id:arg)|], [C.cexp|&$id:arg|]),
          v'
        )

callKernel :: GC.OpCompiler OpenCL ()
callKernel (GetSize v key) =
  GC.stm [C.cstm|$id:v = $exp:(getParamByKey key);|]
callKernel (CmpSizeLe v key x) = do
  x' <- GC.compileExp x
  GC.stm [C.cstm|$id:v = $exp:(getParamByKey key) <= $exp:x';|]
  -- Output size information if logging is enabled.  The autotuner
  -- depends on the format of this output, so use caution if changing
  -- it.
  GC.stm
    [C.cstm|if (ctx->logging) {
    fprintf(ctx->log, "Compared %s <= %ld: %s.\n", $string:(T.unpack (prettyTextOneLine key)), (long)$exp:x', $id:v ? "true" : "false");
    }|]
callKernel (GetSizeMax v size_class) = do
  let e = kernelConstToExp $ SizeMaxConst size_class
  GC.stm [C.cstm|$id:v = $exp:e;|]
callKernel (LaunchKernel safety kernel_name shared_memory args num_tblocks tblock_size) =
  genLaunchKernel safety kernel_name shared_memory args num_tblocks tblock_size

copygpu2gpu :: GC.DoCopy op s
copygpu2gpu _ t shape dst (dstoffset, dststride) src (srcoffset, srcstride) = do
  let fname = "lmad_copy_gpu2gpu_" <> show (primByteSize t :: Int) <> "b"
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

copyhost2gpu :: GC.DoCopy op s
copyhost2gpu sync t shape dst (dstoffset, dststride) src (srcoffset, srcstride) = do
  let r = length shape
      dststride_inits = [[C.cinit|$exp:e|] | Count e <- dststride]
      srcstride_inits = [[C.cinit|$exp:e|] | Count e <- srcstride]
      shape_inits = [[C.cinit|$exp:e|] | Count e <- shape]
  GC.stm
    [C.cstm|
         if ((err =
                lmad_copy_host2gpu
                         (ctx, $int:(primByteSize t::Int), $exp:sync', $int:r,
                          $exp:dst, $exp:(unCount dstoffset),
                          (typename int64_t[]){ $inits:dststride_inits },
                          $exp:src, $exp:(unCount srcoffset),
                          (typename int64_t[]){ $inits:srcstride_inits },
                          (typename int64_t[]){ $inits:shape_inits })) != 0) {
           goto cleanup;
         }
     |]
  where
    sync' = case sync of
      GC.CopyBarrier -> [C.cexp|true|]
      GC.CopyNoBarrier -> [C.cexp|false|]

copygpu2host :: GC.DoCopy op s
copygpu2host sync t shape dst (dstoffset, dststride) src (srcoffset, srcstride) = do
  let r = length shape
      dststride_inits = [[C.cinit|$exp:e|] | Count e <- dststride]
      srcstride_inits = [[C.cinit|$exp:e|] | Count e <- srcstride]
      shape_inits = [[C.cinit|$exp:e|] | Count e <- shape]
  GC.stm
    [C.cstm|
         if ((err =
                lmad_copy_gpu2host
                         (ctx, $int:(primByteSize t::Int), $exp:sync', $int:r,
                          $exp:dst, $exp:(unCount dstoffset),
                          (typename int64_t[]){ $inits:dststride_inits },
                          $exp:src, $exp:(unCount srcoffset),
                          (typename int64_t[]){ $inits:srcstride_inits },
                          (typename int64_t[]){ $inits:shape_inits })) != 0) {
           goto cleanup;
         }
     |]
  where
    sync' = case sync of
      GC.CopyBarrier -> [C.cexp|true|]
      GC.CopyNoBarrier -> [C.cexp|false|]

gpuCopies :: M.Map (Space, Space) (GC.DoCopy op s)
gpuCopies =
  M.fromList
    [ ((Space "device", Space "device"), copygpu2gpu),
      ((Space "device", DefaultSpace), copyhost2gpu),
      ((DefaultSpace, Space "device"), copygpu2host)
    ]

createKernels :: [KernelName] -> GC.CompilerM op s ()
createKernels kernels = forM_ kernels $ \name ->
  GC.contextFieldDyn
    (C.toIdent name mempty)
    [C.cty|typename gpu_kernel|]
    [C.cstm|gpu_create_kernel(ctx, &ctx->program->$id:name, $string:(T.unpack (idText (C.toIdent name mempty))));|]
    [C.cstm|gpu_free_kernel(ctx, ctx->program->$id:name);|]

allocateGPU :: GC.Allocate op ()
allocateGPU mem size tag "device" =
  GC.stm
    [C.cstm|(void)gpu_alloc(ctx, ctx->log,
                            (size_t)$exp:size, $exp:tag,
                            &$exp:mem, (size_t*)&$exp:size);|]
allocateGPU _ _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' memory space."

deallocateGPU :: GC.Deallocate op ()
deallocateGPU mem size tag "device" =
  GC.stm [C.cstm|(void)gpu_free(ctx, $exp:mem, $exp:size, $exp:tag);|]
deallocateGPU _ _ _ space =
  error $ "Cannot deallocate in '" ++ space ++ "' space"

-- It is often faster to do a blocking clEnqueueReadBuffer() than to
-- do an async clEnqueueReadBuffer() followed by a clFinish(), even
-- with an in-order command queue.  This is safe if and only if there
-- are no possible outstanding failures.
readScalarGPU :: GC.ReadScalar op ()
readScalarGPU mem i t "device" _ = do
  val <- newVName "read_res"
  GC.decl [C.cdecl|$ty:t $id:val;|]
  GC.stm
    [C.cstm|if ((err = gpu_scalar_from_device(ctx, &$id:val, $exp:mem, $exp:i * sizeof($ty:t), sizeof($ty:t))) != 0) { goto cleanup; }|]
  GC.stm
    [C.cstm|if (ctx->failure_is_an_option && futhark_context_sync(ctx) != 0)
            { err = 1; goto cleanup; }|]
  pure [C.cexp|$id:val|]
readScalarGPU _ _ _ space _ =
  error $ "Cannot read from '" ++ space ++ "' memory space."

-- TODO: Optimised special case when the scalar is a constant, in
-- which case we can do the write asynchronously.
writeScalarGPU :: GC.WriteScalar op ()
writeScalarGPU mem i t "device" _ val = do
  val' <- newVName "write_tmp"
  GC.item [C.citem|$ty:t $id:val' = $exp:val;|]
  GC.stm
    [C.cstm|if ((err = gpu_scalar_to_device(ctx, $exp:mem, $exp:i * sizeof($ty:t), sizeof($ty:t), &$id:val')) != 0) { goto cleanup; }|]
writeScalarGPU _ _ _ space _ _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

syncArg :: GC.CopyBarrier -> C.Exp
syncArg GC.CopyBarrier = [C.cexp|true|]
syncArg GC.CopyNoBarrier = [C.cexp|false|]

copyGPU :: GC.Copy OpenCL ()
copyGPU _ dstmem dstidx (Space "device") srcmem srcidx (Space "device") nbytes =
  GC.stm
    [C.cstm|err = gpu_memcpy(ctx, $exp:dstmem, $exp:dstidx, $exp:srcmem, $exp:srcidx, $exp:nbytes);|]
copyGPU b dstmem dstidx DefaultSpace srcmem srcidx (Space "device") nbytes =
  GC.stm
    [C.cstm|err = memcpy_gpu2host(ctx, $exp:(syncArg b), $exp:dstmem, $exp:dstidx, $exp:srcmem, $exp:srcidx, $exp:nbytes);|]
copyGPU b dstmem dstidx (Space "device") srcmem srcidx DefaultSpace nbytes =
  GC.stm
    [C.cstm|err = memcpy_host2gpu(ctx, $exp:(syncArg b), $exp:dstmem, $exp:dstidx, $exp:srcmem, $exp:srcidx, $exp:nbytes);|]
copyGPU _ _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

gpuOperations :: GC.Operations OpenCL ()
gpuOperations =
  GC.defaultOperations
    { GC.opsCompiler = callKernel,
      GC.opsWriteScalar = writeScalarGPU,
      GC.opsReadScalar = readScalarGPU,
      GC.opsAllocate = allocateGPU,
      GC.opsDeallocate = deallocateGPU,
      GC.opsCopy = copyGPU,
      GC.opsCopies = gpuCopies <> GC.opsCopies GC.defaultOperations,
      GC.opsFatMemory = True
    }

-- | Options that are common to multiple GPU-like backends.
gpuOptions :: [Option]
gpuOptions =
  [ Option
      { optionLongName = "device",
        optionShortName = Just 'd',
        optionArgument = RequiredArgument "NAME",
        optionDescription = "Use the first device whose name contains the given string.",
        optionAction = [C.cstm|futhark_context_config_set_device(cfg, optarg);|]
      },
    Option
      { optionLongName = "default-thread-block-size",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default size of thread blocks that are launched.",
        optionAction = [C.cstm|futhark_context_config_set_default_thread_block_size(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-grid-size",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default number of thread blocks that are launched.",
        optionAction = [C.cstm|futhark_context_config_set_default_grid_size(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-group-size",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "Alias for --default-thread-block-size.",
        optionAction = [C.cstm|futhark_context_config_set_default_group_size(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-num-groups",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "Alias for --default-num-thread-blocks.",
        optionAction = [C.cstm|futhark_context_config_set_default_num_groups(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-tile-size",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default tile size used when performing two-dimensional tiling.",
        optionAction = [C.cstm|futhark_context_config_set_default_tile_size(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-reg-tile-size",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default register tile size used when performing two-dimensional tiling.",
        optionAction = [C.cstm|futhark_context_config_set_default_reg_tile_size(cfg, atoi(optarg));|]
      },
    Option
      { optionLongName = "default-threshold",
        optionShortName = Nothing,
        optionArgument = RequiredArgument "INT",
        optionDescription = "The default parallelism threshold.",
        optionAction = [C.cstm|futhark_context_config_set_default_threshold(cfg, atoi(optarg));|]
      }
  ]

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

failureMsgFunction :: [FailureMsg] -> C.Definition
failureMsgFunction failures =
  let printfEscape =
        let escapeChar '%' = "%%"
            escapeChar c = [c]
         in concatMap escapeChar
      onPart (ErrorString s) = printfEscape $ T.unpack s
      -- FIXME: bogus for non-ints.
      onPart ErrorVal {} = "%lld"
      onFailure i (FailureMsg emsg@(ErrorMsg parts) backtrace) =
        let msg = concatMap onPart parts ++ "\n" ++ printfEscape backtrace
            msgargs = [[C.cexp|args[$int:j]|] | j <- [0 .. errorMsgNumArgs emsg - 1]]
         in [C.cstm|case $int:i: {return msgprintf($string:msg, $args:msgargs); break;}|]
      failure_cases =
        zipWith onFailure [(0 :: Int) ..] failures
   in [C.cedecl|static char* get_failure_msg(int failure_idx, typename int64_t args[]) {
                  (void)args;
                  switch (failure_idx) { $stms:failure_cases }
                  return strdup("Unknown error.  This is a compiler bug.");
                }|]

compileConstExp :: KernelConstExp -> C.Exp
compileConstExp e = runIdentity $ GC.compilePrimExp (pure . kernelConstToExp) e

-- | Called after most code has been generated to generate the bulk of
-- the boilerplate.
generateGPUBoilerplate ::
  T.Text ->
  [(Name, KernelConstExp)] ->
  T.Text ->
  [Name] ->
  [PrimType] ->
  [FailureMsg] ->
  GC.CompilerM OpenCL () ()
generateGPUBoilerplate gpu_program macros backendH kernels types failures = do
  createKernels kernels
  let gpu_program_fragments =
        -- Some C compilers limit the size of literal strings, so
        -- chunk the entire program into small bits here, and
        -- concatenate it again at runtime.
        [[C.cinit|$string:s|] | s <- chunk 2000 $ T.unpack gpu_program]
      program_fragments = gpu_program_fragments ++ [[C.cinit|NULL|]]
      f64_required
        | FloatType Float64 `elem` types = [C.cexp|1|]
        | otherwise = [C.cexp|0|]
      max_failure_args = foldl max 0 $ map (errorMsgNumArgs . failureError) failures

      setMacro i (name, e) =
        [C.cstm|{names[$int:i] = $string:(nameToString name);
                 values[$int:i] = $esc:e';}|]
        where
          e' = T.unpack $ expText $ compileConstExp e

  mapM_
    GC.earlyDecl
    [C.cunit|static const int max_failure_args = $int:max_failure_args;
             static const int f64_required = $exp:f64_required;
             static const char *gpu_program[] = {$inits:program_fragments};
             $esc:(T.unpack gpuPrototypesH)
             $esc:(T.unpack backendH)
             $esc:(T.unpack gpuH)
             static int gpu_macros(struct futhark_context *ctx, char*** names_out, typename int64_t** values_out) {
               int num_macros = $int:(length macros);
               char** names = malloc(num_macros * sizeof(char*));
               typename int64_t* values = malloc(num_macros * sizeof(int64_t));

               $stms:(zipWith setMacro [(0::Int)..] macros)

               *names_out = names;
               *values_out = values;
               return num_macros;
             }
            |]
  GC.earlyDecl $ failureMsgFunction failures

  GC.generateProgramStruct

  GC.onClear [C.citem|if (ctx->error == NULL) { gpu_free_all(ctx); }|]

  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_thread_block_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_grid_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_reg_tile_size(struct futhark_context_config *cfg, int size);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_default_threshold(struct futhark_context_config *cfg, int size);|]
