{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Futhark.CodeGen.Backends.CCuda
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import qualified Language.C.Quote.OpenCL as C

import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.ImpGen.Cuda as ImpGen
import Futhark.Error
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax)
import Futhark.MonadFreshNames
import Futhark.CodeGen.ImpCode.OpenCL
import Futhark.CodeGen.Backends.CCuda.Boilerplate
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
                 { GC.opsWriteScalar = writeCudaScalar
                 , GC.opsReadScalar  = readCudaScalar
                 , GC.opsAllocate    = allocateCudaBuffer
                 , GC.opsDeallocate  = deallocateCudaBuffer
                 , GC.opsCopy        = copyCudaMemory
                 , GC.opsStaticArray = staticCudaArray
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
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_dump_program_to(cfg, optarg);|]
                      }
             , Option { optionLongName = "load-cuda"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_load_program_from(cfg, optarg);|]
                      }
             , Option { optionLongName = "dump-ptx"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_dump_ptx_to(cfg, optarg);|]
                      }
             , Option { optionLongName = "load-ptx"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument
                      , optionAction = [C.cstm|futhark_context_config_load_ptx_from(cfg, optarg);|]
                      }
             , Option { optionLongName = "print-sizes"
                      , optionShortName = Nothing
                      , optionArgument = NoArgument
                      , optionAction = [C.cstm|{
                          int n = futhark_get_num_sizes();
                          for (int i = 0; i < n; i++) {
                            if (strcmp(futhark_get_size_entry(i), entry_point) == 0) {
                              printf("%s (%s)\n", futhark_get_size_name(i),
                                                  futhark_get_size_class(i));
                            }
                          }
                          exit(0);
                        }|]
                      }
             ]

writeCudaScalar :: GC.WriteScalar OpenCL ()
writeCudaScalar mem idx t "device" _ val = do
  val' <- newVName "write_tmp"
  GC.stm [C.cstm|{$ty:t $id:val' = $exp:val;
                  CUDA_SUCCEED(
                    cuMemcpyHtoD($exp:mem + $exp:idx,
                                 &$id:val',
                                 sizeof($ty:t)));
                 }|]
writeCudaScalar _ _ _ space _ _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

readCudaScalar :: GC.ReadScalar OpenCL ()
readCudaScalar mem idx t "device" _ = do
  val <- newVName "read_res"
  GC.decl [C.cdecl|$ty:t $id:val;|]
  GC.stm [C.cstm|CUDA_SUCCEED(
                   cuMemcpyDtoH(&$id:val,
                                $exp:mem + $exp:idx,
                                sizeof($ty:t)));
                |]
  return [C.cexp|$id:val|]
readCudaScalar _ _ _ space _ =
  fail $ "Cannot write to '" ++ space ++ "' memory space."

allocateCudaBuffer :: GC.Allocate OpenCL ()
allocateCudaBuffer mem size tag "device" =
  GC.stm [C.cstm|CUDA_SUCCEED(cuda_alloc(&ctx->cuda, $exp:size, $exp:tag, &$exp:mem));|]
allocateCudaBuffer _ _ _ "local" = return ()
allocateCudaBuffer _ _ _ space =
  fail $ "Cannot allocate in '" ++ space ++ "' memory space."

deallocateCudaBuffer :: GC.Deallocate OpenCL ()
deallocateCudaBuffer mem tag "device" =
  GC.stm [C.cstm|CUDA_SUCCEED(cuda_free(&ctx->cuda, $exp:mem, $exp:tag));|]
deallocateCudaBuffer _ _ "local" = return ()
deallocateCudaBuffer _ _ space =
  fail $ "Cannot deallocate in '" ++ space ++ "' memory space."

copyCudaMemory :: GC.Copy OpenCL ()
copyCudaMemory dstmem dstidx dstSpace srcmem srcidx srcSpace nbytes = do
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

staticCudaArray :: GC.StaticArray OpenCL ()
staticCudaArray name "device" t vals = do
  let ct = GC.primTypeToCType t
      vals' = [[C.cinit|$exp:v|] | v <- map GC.compilePrimValue vals]
      num_elems = length vals
  name_realtype <- newVName $ baseString name ++ "_realtype"
  GC.libDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:num_elems] = {$inits:vals'};|]
  -- Fake a memory block.
  GC.contextField (pretty name) [C.cty|struct memblock_device|] Nothing
  -- During startup, copy the data to where we need it.
  GC.atInit [C.cstm|{
    ctx->$id:name.references = NULL;
    ctx->$id:name.size = 0;
    CUDA_SUCCEED(cuMemAlloc(&ctx->$id:name.mem,
                            ($int:num_elems > 0 ? $int:num_elems : 1)*sizeof($ty:ct)));
    if ($int:num_elems > 0) {
      CUDA_SUCCEED(cuMemcpyHtoD(ctx->$id:name.mem, $id:name_realtype,
                                $int:num_elems*sizeof($ty:ct)));
    }
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->$id:name;|]
staticCudaArray _ space _ _ =
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
callKernel (LaunchKernel name args kernel_size workgroup_size) = do
  args_arr <- newVName "kernel_args"
  (args', shared_vars) <- unzip <$> mapM mkArgs args
  let (shared_sizes, shared_offsets) = unzip $ catMaybes shared_vars
      shared_offsets_sc = mkOffsets shared_sizes
      shared_args = zip shared_offsets shared_offsets_sc
      shared_tot = last shared_offsets_sc
  mapM_ (\(arg,offset) -> do
           GC.decl [C.cdecl|unsigned int $id:arg = $exp:offset;|]
        ) shared_args

  (global_x, global_y, global_z) <- mkDims <$> mapM GC.compileExp kernel_size
  (block_x, block_y, block_z) <- mkDims <$> mapM GC.compileExp workgroup_size
  let args'' = [ [C.cinit|&$id:a|] | a <- args' ]
      sizes_nonzero = expsNotZero [global_x, global_y, global_z]
  GC.stm [C.cstm|
    if ($exp:sizes_nonzero) {
      void *$id:args_arr[] = {$inits:args''};
      CUDA_SUCCEED(
        cuLaunchKernel(ctx->$id:name,
                       $exp:global_x / $exp:block_x,
                       $exp:global_y / $exp:block_y,
                       $exp:global_z / $exp:block_z,
                       $exp:block_x, $exp:block_y, $exp:block_z,
                       $exp:shared_tot, NULL,
                       $id:args_arr, NULL));
    }|]
  where
    mkDims [] = ([C.cexp|0|] , [C.cexp|0|], [C.cexp|0|])
    mkDims [x] = (x, [C.cexp|1|], [C.cexp|1|])
    mkDims [x,y] = (x, y, [C.cexp|1|])
    mkDims (x:y:z:_) = (x, y, z)
    addExp x y = [C.cexp|$exp:x + $exp:y|]
    alignExp e = [C.cexp|$exp:e + ((8 - ($exp:e % 8)) % 8)|]
    mkOffsets = scanl (\a b -> a `addExp` (alignExp b)) [C.cexp|0|]
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

