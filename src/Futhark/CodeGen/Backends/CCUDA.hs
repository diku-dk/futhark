{-# LANGUAGE QuasiQuotes #-}
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
import Futhark.CodeGen.ImpGen (compileSubExpOfType)
import Futhark.Error
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax)
import Futhark.MonadFreshNames
import Futhark.CodeGen.ImpCode.OpenCL hiding (paramName)
import Futhark.CodeGen.Backends.CCUDA.Boilerplate
import Futhark.CodeGen.Backends.GenericC.Options

import Data.Maybe (catMaybes, fromMaybe)

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Program cuda_code cuda_prelude kernel_names _ sizes prog') ->
      let extra = generateBoilerplate cuda_code cuda_prelude
                                      kernel_names sizes
      in Right <$> GC.compileProg operations extra cuda_includes
                   [Space "device", Space "local", DefaultSpace] cliOptions prog'
  where
    operations :: GC.Operations OpenCL ()
    operations = GC.Operations
                 { GC.opsWriteScalar = writeCUDAScalar
                 , GC.opsReadScalar  = readCUDAScalar
                 , GC.opsAllocate    = allocateCUDABuffer
                 , GC.opsDeallocate  = deallocateCUDABuffer
                 , GC.opsCopy        = copyCUDAMemory
                 , GC.opsPartition   = partitionCUDAMemory
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
                    cuMemcpyHtoD($exp:mem.mems[0] + $exp:idx,
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
                                $exp:mem.mems[0] + $exp:idx,
                                sizeof($ty:t)));
                |]
  return [C.cexp|$id:val|]
readCUDAScalar _ _ _ space _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

allocateCUDABuffer :: GC.Allocate OpenCL ()
allocateCUDABuffer mem n size tag "device" = do
  GC.stm [C.cstm|$exp:mem.count = $exp:n;|]
  GC.stm [C.cstm|$exp:mem.mems = malloc($exp:n * sizeof(CUdeviceptr));|]
  GC.stm [C.cstm|
      if ($exp:n == 1) {
        CUDA_SUCCEED(cuda_alloc(&ctx->cuda, $exp:size, $exp:tag, &$exp:mem.mems[0]));
      } else {
        cuda_send_node_alloc(&ctx->cuda, $exp:mem, $exp:tag, $exp:size);
      }
    |]
allocateCUDABuffer _ _ _ _ "local" = return ()
allocateCUDABuffer _ _ _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' memory space."

deallocateCUDABuffer :: GC.Deallocate OpenCL ()
deallocateCUDABuffer mem tag "device" = do
  GC.stm [C.cstm|
      if($exp:mem.count == 1) {
        CUDA_SUCCEED(cuda_free(&ctx->cuda, $exp:mem.mems[0], $exp:tag));
      } else {
        cuda_send_node_free(&ctx->cuda, $exp:mem, $exp:tag);
      }
    |]
  GC.stm [C.cstm|free($exp:mem.mems);|]
deallocateCUDABuffer _ _ "local" = return ()
deallocateCUDABuffer _ _ space =
  fail $ "Cannot deallocate in '" ++ space ++ "' memory space."

copyCUDAMemory :: GC.Copy OpenCL ()
copyCUDAMemory dstmem dstidx dstSpace srcmem srcidx srcSpace nbytes = do
  fn <- memcpyFun dstSpace srcSpace
  ncpy <- nodeMemcpy dstSpace srcSpace
  GC.stm =<< maybe 
    [C.cstm|CUDA_SUCCEED($id:fn($exp:(firstMem dstSpace dstmem) + $exp:dstidx,
                                $exp:(firstMem srcSpace srcmem) + $exp:srcidx, $exp:nbytes));
      |]
    (const ncpy)
    <$> GC.getNodeCount
  where
    firstMem (Space "device") mem = [C.cexp|$exp:mem.mems[0]|]
    firstMem _ mem = mem
    memcpyFun DefaultSpace (Space "device")     = return "cuMemcpyDtoH"
    memcpyFun (Space "device") DefaultSpace     = return "cuMemcpyHtoD"
    memcpyFun (Space "device") (Space "device") = return "cuMemcpyDtoD"
    memcpyFun _ _ = fail $ "Cannot copy to '" ++ show dstSpace
                           ++ "' from '" ++ show srcSpace ++ "'."
    nodeMemcpy DefaultSpace (Space "device")     =
      return [C.cstm|cuda_send_node_memcpy_dtoh(&ctx->cuda, $exp:srcmem, $exp:srcidx,
                            $exp:dstmem + $exp:dstidx, $exp:nbytes);|]
    nodeMemcpy (Space "device") DefaultSpace     = 
      return [C.cstm|cuda_send_node_memcpy_htod(&ctx->cuda, $exp:srcmem + $exp:srcidx,
                            $exp:dstmem, $exp:dstidx, $exp:nbytes);|]
    nodeMemcpy (Space "device") (Space "device") = 
      return [C.cstm|cuda_send_node_memcpy_dtod(&ctx->cuda, $exp:srcmem, $exp:srcidx,
                            $exp:dstmem, $exp:dstidx, $exp:nbytes);|]
    nodeMemcpy _ _ = fail $ "Cannot copy to '" ++ show dstSpace
                           ++ "' from '" ++ show srcSpace ++ "'."

partitionCUDAMemory :: GC.Partition OpenCL ()
partitionCUDAMemory dstmem partsize srcmem dstsize "device" =
  GC.stm [C.cstm|cuda_send_node_memcpy_partition(&ctx->cuda, $exp:dstmem, $exp:srcmem,
                                                 $exp:dstsize, $exp:partsize);|]
partitionCUDAMemory _ _ _ _ space =
  fail $ "CUDA backend cannot partition memory in '" ++ space ++ "' memory space"

staticCUDAArray :: GC.StaticArray OpenCL ()
staticCUDAArray name "device" t vs = do
  let ct = GC.primTypeToCType t
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
  GC.atInit [C.cstm|{
    ctx->$id:name.references = NULL;
    ctx->$id:name.size = 0;
    ctx->$id:name.mem.count = ctx->cuda.cfg.num_nodes;
    ctx->$id:name.mem.mems = malloc(ctx->cuda.cfg.num_nodes * sizeof(CUdeviceptr));
    cuda_send_node_static(&ctx->cuda, ctx->$id:name.mem, $id:name_realtype, $int:num_elems, sizeof($ty:ct));
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->$id:name;|]
staticCUDAArray _ space _ _ =
  fail $ "CUDA backend cannot create static array in '" ++ space
          ++ "' memory space"

cudaMemoryType :: GC.MemoryType OpenCL ()
cudaMemoryType "device" = pure [C.cty|struct cuda_mem_ptrs|]
cudaMemoryType "local" = pure [C.cty|unsigned char|] -- dummy type
cudaMemoryType space =
  fail $ "CUDA backend does not support '" ++ space ++ "' memory space."

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
callKernel (DistributeHusk hspace _ num_nodes body) = do
  let HuskSpace _ src_elems _ parts_elems _ _ _ node_id_mem = hspace
  src_elems_e <- GC.compileExp $ compileSubExpOfType int32 src_elems
  GC.stm [C.cstm|$id:num_nodes = ctx->cuda.cfg.num_nodes;|]
  GC.stm [C.cstm|$id:parts_elems = ($exp:src_elems_e + $id:num_nodes - 1) / $id:num_nodes;|]
  GC.stm [C.cstm|$id:node_id_mem = ctx->device_node_ids;|]
  GC.localNodeCount [C.cexp|$id:num_nodes|] $ GC.compileCode body
callKernel (LaunchKernel name args num_blocks block_size) = do
  args_arr <- newVName "kernel_args"
  ind_args_arr <- newVName "ind_kernel_args"
  time_start <- newVName "time_start"
  time_end <- newVName "time_end"
  node_id <- newVName "node_id"
  free_node_id <- newVName "free_node_id"
  (args', shared_vars) <- unzip <$> mapM (mkArgs node_id) args
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
  let args'' = perm_args ++ args'
      sizes_nonzero = expsNotZero [grid_x, grid_y, grid_z,
                      block_x, block_y, block_z]
  node_count <- GC.getNodeCount
  let actual_node_count = fromMaybe [C.cexp|1|] node_count
      sync = maybe [C.cstm|CUDA_SUCCEED(cuCtxSynchronize());|]
                   (const [C.cstm|cuda_send_node_sync(&ctx->cuda);|])
                   node_count
      launch = maybe [C.cstm|CUDA_SUCCEED(
                              cuLaunchKernel(ctx->$id:name[0],
                                            grid[0], grid[1], grid[2],
                                            $exp:block_x, $exp:block_y, $exp:block_z,
                                            $exp:shared_tot, NULL,
                                            $id:args_arr[0], NULL));|]
                     (const [C.cstm|cuda_send_node_launch(&ctx->cuda, ctx->$id:name, grid[0], grid[1],
                                                          grid[2], $exp:block_x, $exp:block_y,
                                                          $exp:block_z, $exp:shared_tot, $id:args_arr);|])
                     node_count

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

      void ***$id:args_arr = malloc($exp:actual_node_count * sizeof(void*));
      for (int $id:node_id = 0; $id:node_id < $exp:actual_node_count; ++$id:node_id) {
        void *$id:ind_args_arr[] = { $inits:args'' };
        $id:args_arr[$id:node_id] = malloc(sizeof($id:ind_args_arr));
        memcpy($id:args_arr[$id:node_id], $id:ind_args_arr, sizeof($id:ind_args_arr));
      }

      typename int64_t $id:time_start = 0, $id:time_end = 0;
      if (ctx->debugging) {
        fprintf(stderr, "Launching %s on node %d with grid size (", $string:name,
                ctx->cuda.active_node);
        $stms:(printSizes [grid_x, grid_y, grid_z])
        fprintf(stderr, ") and block size (");
        $stms:(printSizes [block_x, block_y, block_z])
        fprintf(stderr, ").\n");
        $id:time_start = get_wall_time();
      }
      $stm:launch
      if (ctx->debugging) {
        $stm:sync
        $id:time_end = get_wall_time();
        fprintf(stderr, "Kernel %s runtime: %ldus\n",
                $string:name, $id:time_end - $id:time_start);
      }

      for (int $id:free_node_id = 0; $id:free_node_id < $exp:actual_node_count; ++$id:free_node_id) {
        free($id:args_arr[$id:free_node_id]);
      }
      free($id:args_arr);
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
    mkArgs _ (ValueKArg e t) = do
      arg <- GC.compileExpToName "kernel_arg" t e
      return ([C.cinit|&$id:arg|], Nothing)
    mkArgs n (MemKArg v) = do
      v' <- GC.rawMem v
      arg <- newVName "kernel_arg"
      GC.decl [C.cdecl|struct cuda_mem_ptrs $id:arg = $exp:v';|]
      return ([C.cinit|&$id:arg.mems[$id:n]|], Nothing)
    mkArgs _ (SharedMemoryKArg (Count c)) = do
      num_bytes <- GC.compileExp c
      size <- newVName "shared_size"
      offset <- newVName "shared_offset"
      GC.decl [C.cdecl|unsigned int $id:size = $exp:num_bytes;|]
      return ([C.cinit|&$id:offset|], Just (size, offset))

    printSizes =
      intercalate [[C.cstm|fprintf(stderr, ", ");|]] . map printSize
    printSize e =
      [[C.cstm|fprintf(stderr, "%d", $exp:e);|]]
