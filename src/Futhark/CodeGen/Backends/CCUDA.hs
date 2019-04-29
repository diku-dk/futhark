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
import Control.Monad

import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.ImpGen.CUDA as ImpGen
import Futhark.CodeGen.ImpGen (compileSubExpOfType)
import Futhark.Error
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax)
import Futhark.MonadFreshNames
import Futhark.CodeGen.ImpCode.OpenCL hiding (paramName)
import Futhark.CodeGen.Backends.CCUDA.Boilerplate
import Futhark.CodeGen.Backends.GenericC.Options

import Data.Maybe (catMaybes)

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
allocateCUDABuffer mem size tag "device" =
  GC.stm [C.cstm|CUDA_SUCCEED(cuda_alloc(&ctx->cuda, $exp:size, $exp:tag, &$exp:mem));|]
allocateCUDABuffer _ _ _ "local" = return ()
allocateCUDABuffer _ _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' memory space."

deallocateCUDABuffer :: GC.Deallocate OpenCL ()
deallocateCUDABuffer mem tag "device" =
  GC.stm [C.cstm|CUDA_SUCCEED(cuda_free(&ctx->cuda, $exp:mem, $exp:tag));|]
deallocateCUDABuffer _ _ "local" = return ()
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
    memcpyFun (Space "device") (Space "device") = return "cuMemcpyDtoD"
    memcpyFun _ _ = fail $ "Cannot copy to '" ++ show dstSpace
                           ++ "' from '" ++ show srcSpace ++ "'."

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
  GC.atInit [C.cstm|for(int i = ctx->cuda.cfg.num_nodes - 1; i >= 0; --i) {
    cuda_set_active_node(&ctx->cuda, i);
    ctx->node_ctx[i].$id:name.references = NULL;
    ctx->node_ctx[i].$id:name.size = 0;
    CUDA_SUCCEED(cuMemAlloc(&ctx->node_ctx[i].$id:name.mem,
                            ($int:num_elems > 0 ? $int:num_elems : 1)*sizeof($ty:ct)));
    if ($int:num_elems > 0) {
      CUDA_SUCCEED(cuMemcpyHtoD(ctx->node_ctx[i].$id:name.mem, $id:name_realtype,
                                $int:num_elems*sizeof($ty:ct)));
    }
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->node_ctx[ctx->cuda.active_node].$id:name;|]
staticCUDAArray _ space _ _ =
  fail $ "CUDA backend cannot create static array in '" ++ space
          ++ "' memory space"

cudaMemoryType :: GC.MemoryType OpenCL ()
cudaMemoryType "device" = return [C.cty|typename CUdeviceptr|]
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
callKernel (DistributeHusk hspace src_mem interm_mem interm_size red body after) = do
  let HuskSpace _ src_elems _ parts_elems parts_elems_offset parts_mem _ = hspace
      num_nodes_e = [C.cexp|ctx->cuda.cfg.num_nodes|]
  body_node_id <- newVName "body_node_id"
  red_node_id <- newVName "red_node_id"
  elems_per_node <- newVName "elems_per_node"
  interm_cols <- replicateM (length interm_mem) $ newVName "interm_col"
  body' <- GC.localNodeCount num_nodes_e $ GC.blockScope $ GC.compileCode body
  red' <- GC.localNodeCount num_nodes_e $ GC.blockScope $ GC.compileCode red
  after' <- GC.localNodeCount num_nodes_e $ GC.blockScope $ GC.compileCode after
  interm_size_e <- mapM (GC.compileExp . sizeToExp) interm_size
  src_elems_e <- GC.compileExp $ compileSubExpOfType int32 src_elems
  let decl_parts = [[C.citem|struct memblock_device $id:part;|] | part <- parts_mem]
      elem_sizes = [[C.cexp|$id:src.size / $exp:src_elems_e|] | src <- src_mem]
      parts_sizes = [[C.cexp|$exp:elem_size * $id:parts_elems|] | elem_size <- elem_sizes]
      alloc_parts = [[C.cstm|
          if(memblock_alloc_device(ctx, &$id:part, $exp:size, $string:(pretty part)))
            return 1;
        |] | (part, size) <- zip parts_mem parts_sizes]
      alloc_interm_col = [[C.cstm|
          if(memblock_alloc_device(ctx, $id:col + $id:body_node_id, $exp:size, $string:(pretty col)))
            return 1;
        |] | (col, size) <- zip interm_cols interm_size_e]
      get_interm_col node_id = [[C.cstm|
          $id:interm = $id:col[$id:node_id];
        |] | (col, interm) <- zip interm_cols interm_mem]
      main_node_set_part = [[C.cstm|
          $id:part = $id:src;
        |] | (part, src) <- zip parts_mem src_mem]
      src_mem_off = [[C.cexp|
          $id:src.mem + $id:parts_elems_offset * $exp:elem_size
        |] | (src, elem_size) <- zip src_mem elem_sizes]
      parts_memr = [[C.cexp|$id:part.mem|] | part <- parts_mem]
      release mems = [[C.cstm|
          if(memblock_unref_device(ctx, &$id:mem, $string:(pretty mem)) != 0)
            return 1;
        |] | mem <- mems]
      cpyToNode to from to_mem from_mem size = [C.cstm|
          CUDA_SUCCEED(cuMemcpyPeer($exp:to_mem,
                                    ctx->cuda.nodes[$exp:to].cu_ctx,
                                    $exp:from_mem, ctx->cuda.nodes[$exp:from].cu_ctx,
                                    $exp:size));
        |]
  GC.decl [C.cdecl|
      typename int32_t $id:elems_per_node = ($exp:src_elems_e + $exp:num_nodes_e - 1) / $exp:num_nodes_e;
    |]
  mapM_ GC.decl [[C.cdecl|struct memblock_device $id:interm;|] | interm <- interm_mem]
  mapM_ GC.decl [[C.cdecl|
      struct memblock_device *$id:col = calloc($exp:num_nodes_e, sizeof(struct memblock_device));
    |] | col <- interm_cols]
  GC.stm [C.cstm|
    for (int $id:body_node_id = $exp:num_nodes_e - 1; $id:body_node_id >= 0; --$id:body_node_id) {
      CUDA_SUCCEED(cuda_set_active_node(&ctx->cuda, $id:body_node_id));

      typename int32_t $id:parts_elems_offset = $id:body_node_id * $id:elems_per_node;
      typename int32_t $id:parts_elems = min($id:elems_per_node, $exp:src_elems_e - $id:parts_elems_offset);
      
      $stms:alloc_interm_col
      $stms:(get_interm_col body_node_id)

      $items:decl_parts
      if ($id:body_node_id != 0) {
        $stms:alloc_parts
        $stms:(zipWith3 (cpyToNode body_node_id (0::Int32)) parts_memr src_mem_off parts_sizes)
      } else {
        $stms:main_node_set_part
      }

      $items:body'

      if ($id:body_node_id != 0) {
        $stms:(release parts_mem)
      }
    }|]
  GC.stm [C.cstm|
    for (int $id:red_node_id = $exp:num_nodes_e - 1; $id:red_node_id >= 0; --$id:red_node_id) {
      CUDA_SUCCEED(cuda_set_active_node(&ctx->cuda, $id:red_node_id));
      $stms:(get_interm_col red_node_id)
      $items:red'
      $stms:(release interm_mem)
    }|]
  GC.stms [[C.cstm|free($id:col);|] | col <- interm_cols]
  GC.stm [C.cstm|CUDA_SUCCEED(cuda_set_active_node(&ctx->cuda, 0));|]
  mapM_ GC.item after'
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
        fprintf(stderr, "Launching %s on node %d with grid size (", $string:name,
                ctx->cuda.active_node);
        $stms:(printSizes [grid_x, grid_y, grid_z])
        fprintf(stderr, ") and block size (");
        $stms:(printSizes [block_x, block_y, block_z])
        fprintf(stderr, ").\n");
        $id:time_start = get_wall_time();
      }
      CUDA_SUCCEED(
        cuLaunchKernel(ctx->node_ctx[ctx->cuda.active_node].$id:name,
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
