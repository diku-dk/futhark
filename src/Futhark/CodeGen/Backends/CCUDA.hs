{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Futhark.CodeGen.Backends.CCUDA
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import qualified Language.C.Quote.OpenCL as C
import Data.List

import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.ImpGen.CUDA as ImpGen
import Futhark.Error
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax, Param, paramName)
import Futhark.MonadFreshNames
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.Backends.CCUDA.Boilerplate
import Futhark.CodeGen.Backends.GenericC.Options

import Data.Maybe (catMaybes)

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Program cuda_code cuda_prelude kernel_names husk_funcs _ sizes prog') ->
      let extra = generateBoilerplate cuda_code cuda_prelude
                                      kernel_names husk_funcs sizes
      in Right <$> GC.compileProg operations extra cuda_includes
                   [Space "device", DefaultSpace] cliOptions prog'
  where
    operations :: GC.Operations OpenCL ()
    operations = GC.Operations
                 { GC.opsWriteScalar = writeCUDAScalar
                 , GC.opsReadScalar  = readCUDAScalar
                 , GC.opsAllocate    = allocateCUDABuffer
                 , GC.opsDeallocate  = deallocateCUDABuffer
                 , GC.opsCopy        = copyCUDAMemory
                 , GC.opsPeerCopy    = peerCopyCUDAMemory
                 , GC.opsStaticArray = staticCUDAArray
                 , GC.opsMemoryType  = cudaMemoryType
                 , GC.opsCompiler    = callKernel
                 , GC.opsFatMemory   = True
                 }
    cuda_includes = unlines [ "#include <cuda.h>"
                            , "#include <nvrtc.h>"
                            ]

cliOptions :: [Option]
cliOptions = [ Option { optionLongName = "dump-cuda"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "FILE"
                      , optionAction = [C.cstm|{futhark_context_config_dump_program_to(cfg, optarg);
                                                entry_point = NULL;}|]
                      }
             , Option { optionLongName = "load-cuda"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "FILE"
                      , optionAction = [C.cstm|futhark_context_config_load_program_from(cfg, optarg);|]
                      }
             , Option { optionLongName = "dump-ptx"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "FILE"
                      , optionAction = [C.cstm|{futhark_context_config_dump_ptx_to(cfg, optarg);
                                                entry_point = NULL;}|]
                      }
             , Option { optionLongName = "load-ptx"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "FILE"
                      , optionAction = [C.cstm|futhark_context_config_load_ptx_from(cfg, optarg);|]
                      }
             , Option { optionLongName = "nodes"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "INT"
                      , optionAction = [C.cstm|futhark_context_config_set_num_nodes(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "nvrtc-option"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "OPT"
                      , optionAction = [C.cstm|futhark_context_config_add_nvrtc_option(cfg, optarg);|]
                      }
             , Option { optionLongName = "print-sizes"
                      , optionShortName = Nothing
                      , optionArgument = NoArgument
                      , optionAction = [C.cstm|{
                          int n = futhark_get_num_sizes();
                          for (int i = 0; i < n; i++) {
                            printf("%s (%s)\n", futhark_get_size_name(i),
                                                futhark_get_size_class(i));
                          }
                          exit(0);
                        }|]
                      }
             , Option { optionLongName = "tuning"
                 , optionShortName = Nothing
                 , optionArgument = RequiredArgument "FILE"
                 , optionAction = [C.cstm|{
                     char *fname = optarg;
                     char *ret = load_tuning_file(optarg, cfg, (int(*)(void*, const char*, size_t))
                                                               futhark_context_config_set_size);
                     if (ret != NULL) {
                       panic(1, "When loading tuning from '%s': %s\n", optarg, ret);
                     }}|]
                 }
             ]

writeCUDAScalar :: GC.WriteScalar OpenCL ()
writeCUDAScalar mem idx t "device" _ val = do
  val' <- newVName "write_tmp"
  GC.stm [C.cstm|{$ty:t $id:val' = $exp:val;
                  CUDA_SUCCEED(
                    cuMemcpyHtoD($exp:mem + $exp:idx,
                                 &$id:val',
                                 sizeof($ty:t)));
                 }|]
writeCUDAScalar _ _ _ space _ _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readCUDAScalar :: GC.ReadScalar OpenCL ()
readCUDAScalar mem idx t "device" _ = do
  val <- newVName "read_res"
  GC.decl [C.cdecl|$ty:t $id:val;|]
  GC.stm [C.cstm|CUDA_SUCCEED(
                   cuMemcpyDtoH(&$id:val,
                                $exp:mem + $exp:idx,
                                sizeof($ty:t)));
                |]
  return [C.cexp|$id:val|]
readCUDAScalar _ _ _ space _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

allocateCUDABuffer :: GC.Allocate OpenCL ()
allocateCUDABuffer mem size tag "device" = do
  node_id <- GC.getNodeId
  GC.stm [C.cstm|
      CUDA_SUCCEED(cuda_alloc(&ctx->cuda.nodes[$exp:node_id], $exp:size, $exp:tag, &$exp:mem));
    |]
allocateCUDABuffer _ _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' memory space."

deallocateCUDABuffer :: GC.Deallocate OpenCL ()
deallocateCUDABuffer mem tag "device" = do
  node_id <- GC.getNodeId
  GC.stm [C.cstm|
      CUDA_SUCCEED(cuda_free(&ctx->cuda.nodes[$exp:node_id], $exp:mem, $exp:tag));
    |]
deallocateCUDABuffer _ _ space =
  fail $ "Cannot deallocate in '" ++ space ++ "' memory space."

copyCUDAMemory :: GC.Copy OpenCL ()
copyCUDAMemory dstmem dstidx dstSpace srcmem srcidx srcSpace nbytes = do
  fn <- memcpyFun dstSpace srcSpace
  GC.stm [C.cstm|CUDA_SUCCEED(
                  $id:fn($exp:dstmem + $exp:dstidx,
                         $exp:srcmem + $exp:srcidx,
                         $exp:nbytes));
                |]
  where
    memcpyFun DefaultSpace (Space "device")     = return "cuMemcpyDtoH"
    memcpyFun (Space "device") DefaultSpace     = return "cuMemcpyHtoD"
    memcpyFun (Space "device") (Space "device") = return "cuMemcpy"
    memcpyFun _ _ = fail $ "Cannot copy to '" ++ show dstSpace
                           ++ "' from '" ++ show srcSpace ++ "'."

peerCopyCUDAMemory :: GC.PeerCopy OpenCL ()
peerCopyCUDAMemory dstmem dstidx dstpeer (Space "device") srcmem srcidx srcpeer (Space "device") nbytes =
  GC.stm [C.cstm|CUDA_SUCCEED(
                  cuMemcpyPeer($exp:dstmem + $exp:dstidx,
                               ctx->cuda.nodes[$exp:dstpeer].cu_ctx,
                               $exp:srcmem + $exp:srcidx,
                               ctx->cuda.nodes[$exp:srcpeer].cu_ctx,
                               $exp:nbytes));|]
peerCopyCUDAMemory _ _ _ dstSpace _ _ _ srcSpace _ =
  fail $ "CUDA backend cannot peer copy memory to '" ++ show dstSpace
         ++ "' from '" ++ show srcSpace ++ "'."

staticCUDAArray :: GC.StaticArray OpenCL ()
staticCUDAArray name "device" t vs = do
  let ct = GC.primTypeToCType t
  node_id <- GC.getNodeId
  name_realtype <- newVName $ baseString name ++ "_realtype"
  num_elems <- case vs of
    ArrayValues vs' -> do
      let vs'' = [[C.cinit|$exp:v|] | v <- map GC.compilePrimValue vs']
      GC.libDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:(length vs'')] = {$inits:vs''};|]
      return $ length vs''
    ArrayZeros n -> do
      GC.libDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:n];|]
      return n
  GC.nodeContextField $ pretty name
  -- During startup, copy the data to where we need it.
  GC.atNodeInit (\init_node_id -> [C.cstm|{
    ctx->node_ctx[$exp:init_node_id].$id:name.references = NULL;
    ctx->node_ctx[$exp:init_node_id].$id:name.size = 0;
    CUDA_SUCCEED(cuMemAlloc(&ctx->node_ctx[$exp:init_node_id].$id:name.mem,
                            ($int:num_elems > 0 ? $int:num_elems : 1)*sizeof($ty:ct)));
    if ($int:num_elems > 0) {
      CUDA_SUCCEED(cuMemcpyHtoD(ctx->node_ctx[$exp:init_node_id].$id:name.mem, $id:name_realtype,
                                $int:num_elems*sizeof($ty:ct)));
    }
  }|])
  GC.item [C.citem|struct memblock_device $id:name = ctx->node_ctx[$exp:node_id].$id:name;|]
staticCUDAArray _ space _ _ =
  fail $ "CUDA backend cannot create static array in '" ++ space
          ++ "' memory space"

cudaMemoryType :: GC.MemoryType OpenCL ()
cudaMemoryType "device" = return [C.cty|typename CUdeviceptr|]
cudaMemoryType space =
  fail $ "CUDA backend does not support '" ++ space ++ "' memory space."

defineHuskFunction :: HuskFunction -> [Param] -> [VName] -> Code -> GC.CompilerM OpenCL () ()
defineHuskFunction (HuskFunction name param_struct parts_offset parts_elems node_id src_elems)
                   params repl_mem body = do
  husk_params_p <- newVName "husk_params_p"
  husk_params <- newVName "husk_params"
  max_parts_elems <- newVName "max_parts_elems"
  struct_decls <- mapM toStructDecl params
  body_decls <- mapM (toDeclInit husk_params) params
  body' <- GC.localNodeBlock [C.cexp|$id:node_id|] $ do
    mapM_ (replMem husk_params) repl_mem
    src_elems' <- GC.compileExp src_elems
    GC.decl [C.cdecl|typename int32_t $id:max_parts_elems = 1 + sdiv32($exp:src_elems' - 1, ctx->cuda.cfg.num_nodes);|]
    GC.decl [C.cdecl|typename int32_t $id:parts_offset = $id:node_id * $id:max_parts_elems;|]
    GC.decl [C.cdecl|typename int32_t $id:parts_elems = smin32($id:max_parts_elems, $exp:src_elems' - $id:parts_offset);|]
    GC.compileCode body
    mapM_ freeRepl repl_mem
  GC.libDecl [C.cedecl|static int $id:name(struct futhark_context *ctx,
                                           typename int32_t $id:node_id,
                                           void *$id:husk_params_p);|]
  GC.huskDecl [C.cedecl|struct $id:param_struct {
      $sdecls:struct_decls
    };|]
  GC.huskDecl [C.cedecl|static int $id:name(struct futhark_context *ctx,
                                            typename int32_t $id:node_id,
                                            void *$id:husk_params_p) {
      struct $id:param_struct $id:husk_params =
        *(struct $id:param_struct *)$id:husk_params_p;
      $decls:body_decls
      $items:body'
      return 0;
    }|]
  where toCType (ScalarParam _ t) = return [C.cty|$ty:(GC.primTypeToCType t)|]
        toCType (MemParam _ space) = do
          t <- GC.memToCType space
          return [C.cty|$ty:t|]
        toStructDecl p = do
          t <- toCType p
          return [C.csdecl|$ty:t $id:(paramName p);|]
        toDeclInit hparam p = do
          let pn = paramName p
          t <- toCType p
          return $ if pn `elem` repl_mem
            then [C.cdecl|$ty:t $id:pn;|]
            else [C.cdecl|$ty:t $id:pn = $id:hparam.$id:pn;|]
        replMem hparam mem = do
          allocateCUDABuffer [C.cexp|$id:mem.mem|] [C.cexp|$id:hparam.$id:mem.size|]
            [C.cexp|$string:(pretty mem)|] "device"
          peerCopyCUDAMemory
            [C.cexp|$id:mem.mem|] [C.cexp|0|] [C.cexp|$id:node_id|] (Space "device")
            [C.cexp|$id:hparam.$id:mem.mem|] [C.cexp|0|] [C.cexp|0|] (Space "device")
            [C.cexp|$id:hparam.$id:mem.size|]
        freeRepl mem =
          deallocateCUDABuffer [C.cexp|$id:mem.mem|] [C.cexp|$string:(pretty mem)|] "device"

callKernel :: GC.OpCompiler OpenCL ()
callKernel (HostCode c) = GC.compileCode c
callKernel (GetSize v key) =
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key;|]
callKernel (CmpSizeLe v key x) = do
  x' <- GC.compileExp x
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key <= $exp:x';|]
callKernel (GetSizeMax v size_class) =
  let field = "max_" ++ cudaSizeClass size_class
  in GC.stm [C.cstm|$id:v = ctx->cuda.$id:field;|]
  where
    cudaSizeClass (SizeThreshold _) = "threshold"
    cudaSizeClass SizeGroup = "block_size"
    cudaSizeClass SizeNumGroups = "grid_size"
    cudaSizeClass SizeTile = "tile_size"
    cudaSizeClass SizeLocalMemory = "shared_memory"
callKernel (DistributeHusk num_nodes bparams repl_mem husk_func interm body red) = do
  let husk_id = baseTag $ hfunctionName husk_func
  err <- newVName "husk_err"
  params <- newVName "husk_params"
  GC.stm [C.cstm|$id:num_nodes = ctx->cuda.cfg.num_nodes;|]
  defineHuskFunction husk_func bparams repl_mem body
  GC.compileCode interm
  GC.decl [C.cdecl|struct $id:(hfunctionParamStruct husk_func) $id:params;|]
  GC.stms [[C.cstm|$id:params.$id:(paramName bparam) = $id:(paramName bparam);|] | bparam <- bparams]
  GC.stm [C.cstm|cuda_send_node_husk(&ctx->cuda, $int:husk_id, &$id:params);|]
  GC.decl [C.cdecl|int $id:err = cuda_node_first_error(&ctx->cuda);|]
  free_all_mem <- GC.unRefAllMem
  GC.stm [C.cstm|if($id:err != 0) {
      $items:free_all_mem
      panic($id:err, "Husk %d failed with error %d.", $int:husk_id, $id:err);
    }|]
  GC.compileCode red
callKernel (LaunchKernel name args num_blocks block_size) = do
  args_arr <- newVName "kernel_args"
  time_start <- newVName "time_start"
  time_end <- newVName "time_end"
  (args', shared_vars) <- unzip <$> mapM mkArgs args
  let (shared_sizes, shared_offsets) = unzip $ catMaybes shared_vars
      shared_offsets_sc = mkOffsets shared_sizes
      shared_args = zip shared_offsets shared_offsets_sc
      shared_tot = last shared_offsets_sc
  mapM_ (\(arg,offset) ->
           GC.decl [C.cdecl|unsigned int $id:arg = $exp:offset;|]
        ) shared_args

  (grid_x, grid_y, grid_z) <- mkDims <$> mapM GC.compileExp num_blocks
  (block_x, block_y, block_z) <- mkDims <$> mapM GC.compileExp block_size
  let perm_args
        | length num_blocks == 3 = [ [C.cinit|&perm[0]|], [C.cinit|&perm[1]|], [C.cinit|&perm[2]|] ]
        | otherwise = []
  let args'' = perm_args ++ [ [C.cinit|&$id:a|] | a <- args' ]
      sizes_nonzero = expsNotZero [grid_x, grid_y, grid_z,
                      block_x, block_y, block_z]
  node_id <- GC.getNodeId
  GC.stm [C.cstm|
    if ($exp:sizes_nonzero) {
      int perm[3] = { 0, 1, 2 };

      if ($exp:grid_y > (1<<16)) {
        perm[1] = perm[0];
        perm[0] = 1;
      }

      if ($exp:grid_z > (1<<16)) {
        perm[2] = perm[0];
        perm[0] = 2;
      }

      size_t grid[3];
      grid[perm[0]] = $exp:grid_x;
      grid[perm[1]] = $exp:grid_y;
      grid[perm[2]] = $exp:grid_z;

      void *$id:args_arr[] = { $inits:args'' };
      typename int64_t $id:time_start = 0, $id:time_end = 0;
      if (ctx->debugging) {
        fprintf(stderr, "Launching %s with grid size (", $string:name);
        $stms:(printSizes [grid_x, grid_y, grid_z])
        fprintf(stderr, ") and block size (");
        $stms:(printSizes [block_x, block_y, block_z])
        fprintf(stderr, ").\n");
        $id:time_start = get_wall_time();
      }
      CUDA_SUCCEED(
        cuLaunchKernel(ctx->node_ctx[$exp:node_id].$id:name,
                       grid[0], grid[1], grid[2],
                       $exp:block_x, $exp:block_y, $exp:block_z,
                       $exp:shared_tot, NULL,
                       $id:args_arr, NULL));
      if (ctx->debugging) {
        CUDA_SUCCEED(cuCtxSynchronize());
        $id:time_end = get_wall_time();
        fprintf(stderr, "Kernel %s runtime: %ldus\n",
                $string:name, $id:time_end - $id:time_start);
      }
    }|]
  where
    mkDims [] = ([C.cexp|0|] , [C.cexp|0|], [C.cexp|0|])
    mkDims [x] = (x, [C.cexp|1|], [C.cexp|1|])
    mkDims [x,y] = (x, y, [C.cexp|1|])
    mkDims (x:y:z:_) = (x, y, z)
    addExp x y = [C.cexp|$exp:x + $exp:y|]
    alignExp e = [C.cexp|$exp:e + ((8 - ($exp:e % 8)) % 8)|]
    mkOffsets = scanl (\a b -> a `addExp` alignExp b) [C.cexp|0|]
    expNotZero e = [C.cexp|$exp:e != 0|]
    expAnd a b = [C.cexp|$exp:a && $exp:b|]
    expsNotZero = foldl expAnd [C.cexp|1|] . map expNotZero
    mkArgs (ValueKArg e t) =
      (,Nothing) <$> GC.compileExpToName "kernel_arg" t e
    mkArgs (MemKArg v) = do
      v' <- GC.rawMem v
      arg <- newVName "kernel_arg"
      GC.decl [C.cdecl|typename CUdeviceptr $id:arg = $exp:v';|]
      return (arg, Nothing)
    mkArgs (SharedMemoryKArg (Count c)) = do
      num_bytes <- GC.compileExp c
      size <- newVName "shared_size"
      offset <- newVName "shared_offset"
      GC.decl [C.cdecl|unsigned int $id:size = $exp:num_bytes;|]
      return (offset, Just (size, offset))

    printSizes =
      intercalate [[C.cstm|fprintf(stderr, ", ");|]] . map printSize
    printSize e =
      [[C.cstm|fprintf(stderr, "%d", $exp:e);|]]
