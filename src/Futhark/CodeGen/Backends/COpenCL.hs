{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.COpenCL
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import Control.Monad hiding (mapM)
import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.Error
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax)
import Futhark.CodeGen.Backends.COpenCL.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenCL
import qualified Futhark.CodeGen.ImpGen.OpenCL as ImpGen
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Program opencl_code opencl_prelude kernel_names types sizes prog') ->
      Right <$> GC.compileProg operations
                (generateBoilerplate opencl_code opencl_prelude kernel_names types sizes)
                include_opencl_h [Space "device", DefaultSpace]
                cliOptions prog'
  where operations :: GC.Operations OpenCL ()
        operations = GC.Operations
                     { GC.opsCompiler = callKernel
                     , GC.opsWriteScalar = writeOpenCLScalar
                     , GC.opsReadScalar = readOpenCLScalar
                     , GC.opsAllocate = allocateOpenCLBuffer
                     , GC.opsDeallocate = deallocateOpenCLBuffer
                     , GC.opsCopy = copyOpenCLMemory
                     , GC.opsStaticArray = staticOpenCLArray
                     , GC.opsMemoryType = openclMemoryType
                     , GC.opsFatMemory = True
                     }
        include_opencl_h = unlines ["#define CL_USE_DEPRECATED_OPENCL_1_2_APIS",
                                    "#ifdef __APPLE__",
                                    "#define CL_SILENCE_DEPRECATION",
                                    "#include <OpenCL/cl.h>",
                                    "#else",
                                    "#include <CL/cl.h>",
                                    "#endif"]

cliOptions :: [Option]
cliOptions = [ Option { optionLongName = "platform"
                      , optionShortName = Just 'p'
                      , optionArgument = RequiredArgument "NAME"
                      , optionAction = [C.cstm|futhark_context_config_set_platform(cfg, optarg);|]
                      }
             , Option { optionLongName = "device"
                      , optionShortName = Just 'd'
                      , optionArgument = RequiredArgument "NAME"
                      , optionAction = [C.cstm|futhark_context_config_set_device(cfg, optarg);|]
                      }
             , Option { optionLongName = "default-group-size"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "INT"
                      , optionAction = [C.cstm|futhark_context_config_set_default_group_size(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "default-num-groups"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "INT"
                      , optionAction = [C.cstm|futhark_context_config_set_default_num_groups(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "default-tile-size"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "INT"
                      , optionAction = [C.cstm|futhark_context_config_set_default_tile_size(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "default-threshold"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "INT"
                      , optionAction = [C.cstm|futhark_context_config_set_default_threshold(cfg, atoi(optarg));|]
                      }
             , Option { optionLongName = "dump-opencl"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "FILE"
                      , optionAction = [C.cstm|{futhark_context_config_dump_program_to(cfg, optarg);
                                                entry_point = NULL;}|]
                      }
             , Option { optionLongName = "load-opencl"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "FILE"
                      , optionAction = [C.cstm|futhark_context_config_load_program_from(cfg, optarg);|]
                      }
             , Option { optionLongName = "dump-opencl-binary"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "FILE"
                      , optionAction = [C.cstm|{futhark_context_config_dump_binary_to(cfg, optarg);
                                                entry_point = NULL;}|]
                      }
             , Option { optionLongName = "load-opencl-binary"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "FILE"
                      , optionAction = [C.cstm|futhark_context_config_load_binary_from(cfg, optarg);|]
                      }
             , Option { optionLongName = "build-option"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "OPT"
                      , optionAction = [C.cstm|futhark_context_config_add_build_option(cfg, optarg);|]
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
             , Option { optionLongName = "size"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "NAME=INT"
                      , optionAction = [C.cstm|{
                          char *name = optarg;
                          char *equals = strstr(optarg, "=");
                          char *value_str = equals != NULL ? equals+1 : optarg;
                          int value = atoi(value_str);
                          if (equals != NULL) {
                            *equals = 0;
                            if (futhark_context_config_set_size(cfg, name, value) != 0) {
                              panic(1, "Unknown size: %s\n", name);
                            }
                          } else {
                            panic(1, "Invalid argument for size option: %s\n", optarg);
                          }}|]
                      }
             , Option { optionLongName = "tuning"
                      , optionShortName = Nothing
                      , optionArgument = RequiredArgument "FILE"
                      , optionAction = [C.cstm|{
                          char *ret = load_tuning_file(optarg, cfg, (int(*)(void*, const char*, size_t))
                                                                    futhark_context_config_set_size);
                          if (ret != NULL) {
                            panic(1, "When loading tuning from '%s': %s\n", optarg, ret);
                          }}|]
                      }
             ]

-- We detect the special case of writing a constant and turn it into a
-- non-blocking write.  This may be slightly faster, as it prevents
-- unnecessary synchronisation of the OpenCL command queue, and
-- writing a constant is fairly common.  This is only possible because
-- we can give the constant infinite lifetime (with 'static'), which
-- is not the case for ordinary variables.
writeOpenCLScalar :: GC.WriteScalar OpenCL ()
writeOpenCLScalar mem i t "device" _ val = do
  val' <- newVName "write_tmp"
  let (decl, blocking) =
        case val of
          C.Const{} -> ([C.citem|static $ty:t $id:val' = $exp:val;|], [C.cexp|CL_FALSE|])
          _         -> ([C.citem|$ty:t $id:val' = $exp:val;|], [C.cexp|CL_TRUE|])
  GC.stm [C.cstm|{$item:decl
                  OPENCL_SUCCEED_OR_RETURN(
                    clEnqueueWriteBuffer(ctx->opencl.queue, $exp:mem, $exp:blocking,
                                         $exp:i * sizeof($ty:t), sizeof($ty:t),
                                         &$id:val',
                                         0, NULL, NULL));
                }|]
writeOpenCLScalar _ _ _ space _ _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: GC.ReadScalar OpenCL ()
readOpenCLScalar mem i t "device" _ = do
  val <- newVName "read_res"
  GC.decl [C.cdecl|$ty:t $id:val;|]
  GC.stm [C.cstm|OPENCL_SUCCEED_OR_RETURN(
                   clEnqueueReadBuffer(ctx->opencl.queue, $exp:mem, CL_TRUE,
                                       $exp:i * sizeof($ty:t), sizeof($ty:t),
                                       &$id:val,
                                       0, NULL, NULL));
              |]
  return [C.cexp|$id:val|]
readOpenCLScalar _ _ _ space _ =
  error $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: GC.Allocate OpenCL ()
allocateOpenCLBuffer mem size tag "device" =
  GC.stm [C.cstm|OPENCL_SUCCEED_OR_RETURN(opencl_alloc(&ctx->opencl, $exp:size, $exp:tag, &$exp:mem));|]
allocateOpenCLBuffer _ _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' space."

deallocateOpenCLBuffer :: GC.Deallocate OpenCL ()
deallocateOpenCLBuffer mem tag "device" =
  GC.stm [C.cstm|OPENCL_SUCCEED_OR_RETURN(opencl_free(&ctx->opencl, $exp:mem, $exp:tag));|]
deallocateOpenCLBuffer _ _ space =
  error $ "Cannot deallocate in '" ++ space ++ "' space"


copyOpenCLMemory :: GC.Copy OpenCL ()
-- The read/write/copy-buffer functions fail if the given offset is
-- out of bounds, even if asked to read zero bytes.  We protect with a
-- branch to avoid this.
copyOpenCLMemory destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes =
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueReadBuffer(ctx->opencl.queue, $exp:srcmem, CL_TRUE,
                            $exp:srcidx, $exp:nbytes,
                            $exp:destmem + $exp:destidx,
                            0, NULL, NULL));
   }
  |]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes =
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueWriteBuffer(ctx->opencl.queue, $exp:destmem, CL_TRUE,
                             $exp:destidx, $exp:nbytes,
                             $exp:srcmem + $exp:srcidx,
                             0, NULL, NULL));
    }
  |]
copyOpenCLMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  -- Be aware that OpenCL swaps the usual order of operands for
  -- memcpy()-like functions.  The order below is not a typo.
  GC.stm [C.cstm|{
    if ($exp:nbytes > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueCopyBuffer(ctx->opencl.queue,
                            $exp:srcmem, $exp:destmem,
                            $exp:srcidx, $exp:destidx,
                            $exp:nbytes,
                            0, NULL, NULL));
      if (ctx->debugging) {
        OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
      }
    }
  }|]
copyOpenCLMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyOpenCLMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

openclMemoryType :: GC.MemoryType OpenCL ()
openclMemoryType "device" = pure [C.cty|typename cl_mem|]
openclMemoryType space =
  error $ "OpenCL backend does not support '" ++ space ++ "' memory space."

staticOpenCLArray :: GC.StaticArray OpenCL ()
staticOpenCLArray name "device" t vs = do
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
  -- Fake a memory block.
  GC.contextField (pretty name) [C.cty|struct memblock_device|] Nothing
  -- During startup, copy the data to where we need it.
  GC.atInit [C.cstm|{
    typename cl_int success;
    ctx->$id:name.references = NULL;
    ctx->$id:name.size = 0;
    ctx->$id:name.mem =
      clCreateBuffer(ctx->opencl.ctx, CL_MEM_READ_WRITE,
                     ($int:num_elems > 0 ? $int:num_elems : 1)*sizeof($ty:ct), NULL,
                     &success);
    OPENCL_SUCCEED_OR_RETURN(success);
    if ($int:num_elems > 0) {
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueWriteBuffer(ctx->opencl.queue, ctx->$id:name.mem, CL_TRUE,
                             0, $int:num_elems*sizeof($ty:ct),
                             $id:name_realtype,
                             0, NULL, NULL));
    }
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->$id:name;|]

staticOpenCLArray _ space _ _ =
  error $ "OpenCL backend cannot create static array in memory space '" ++ space ++ "'"

callKernel :: GC.OpCompiler OpenCL ()
callKernel (GetSize v key) =
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key;|]
callKernel (CmpSizeLe v key x) = do
  x' <- GC.compileExp x
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key <= $exp:x';|]
  GC.stm [C.cstm|if (ctx->logging) {
    fprintf(stderr, "Compared %s <= %d.\n", $string:(pretty key), $exp:x');
    }|]
callKernel (GetSizeMax v size_class) =
  let field = "max_" ++ pretty size_class
  in GC.stm [C.cstm|$id:v = ctx->opencl.$id:field;|]
callKernel (HostCode c) =
  GC.compileCode c

callKernel (LaunchKernel name args num_workgroups workgroup_size) = do
  zipWithM_ setKernelArg [(0::Int)..] args
  num_workgroups' <- mapM GC.compileExp num_workgroups
  workgroup_size' <- mapM GC.compileExp workgroup_size
  local_bytes <- foldM localBytes [C.cexp|0|] args
  launchKernel name num_workgroups' workgroup_size' local_bytes
  where setKernelArg i (ValueKArg e bt) = do
          v <- GC.compileExpToName "kernel_arg" bt e
          GC.stm [C.cstm|
            OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->$id:name, $int:i, sizeof($id:v), &$id:v));
          |]

        setKernelArg i (MemKArg v) = do
          v' <- GC.rawMem v
          GC.stm [C.cstm|
            OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->$id:name, $int:i, sizeof($exp:v'), &$exp:v'));
          |]

        setKernelArg i (SharedMemoryKArg num_bytes) = do
          num_bytes' <- GC.compileExp $ unCount num_bytes
          GC.stm [C.cstm|
            OPENCL_SUCCEED_OR_RETURN(clSetKernelArg(ctx->$id:name, $int:i, $exp:num_bytes', NULL));
            |]

        localBytes cur (SharedMemoryKArg num_bytes) = do
          num_bytes' <- GC.compileExp $ unCount num_bytes
          return [C.cexp|$exp:cur + $exp:num_bytes'|]
        localBytes cur _ = return cur

launchKernel :: C.ToExp a =>
                String -> [a] -> [a] -> a -> GC.CompilerM op s ()
launchKernel kernel_name num_workgroups workgroup_dims local_bytes = do
  global_work_size <- newVName "global_work_size"
  time_start <- newVName "time_start"
  time_end <- newVName "time_end"
  time_diff <- newVName "time_diff"
  local_work_size <- newVName "local_work_size"

  GC.stm [C.cstm|
    if ($exp:total_elements != 0) {
      const size_t $id:global_work_size[$int:kernel_rank] = {$inits:kernel_dims'};
      const size_t $id:local_work_size[$int:kernel_rank] = {$inits:workgroup_dims'};
      typename int64_t $id:time_start = 0, $id:time_end = 0;
      if (ctx->debugging) {
        fprintf(stderr, "Launching %s with global work size [", $string:kernel_name);
        $stms:(printKernelSize global_work_size)
        fprintf(stderr, "] and local work size [");
        $stms:(printKernelSize local_work_size)
        fprintf(stderr, "]; local memory parameters sum to %d bytes.\n", (int)$exp:local_bytes);
        $id:time_start = get_wall_time();
      }
      OPENCL_SUCCEED_OR_RETURN(
        clEnqueueNDRangeKernel(ctx->opencl.queue, ctx->$id:kernel_name, $int:kernel_rank, NULL,
                               $id:global_work_size, $id:local_work_size,
                               0, NULL, NULL));
      if (ctx->debugging) {
        OPENCL_SUCCEED_FATAL(clFinish(ctx->opencl.queue));
        $id:time_end = get_wall_time();
        long int $id:time_diff = $id:time_end - $id:time_start;
        ctx->$id:(kernelRuntime kernel_name) += $id:time_diff;
        ctx->$id:(kernelRuns kernel_name)++;
        fprintf(stderr, "kernel %s runtime: %ldus\n",
                $string:kernel_name, $id:time_diff);
      }
    }|]
  where kernel_rank = length kernel_dims
        kernel_dims = zipWith multExp num_workgroups workgroup_dims
        kernel_dims' = map toInit kernel_dims
        workgroup_dims' = map toInit workgroup_dims
        total_elements = foldl multExp [C.cexp|1|] kernel_dims

        toInit e = [C.cinit|$exp:e|]
        multExp x y = [C.cexp|$exp:x * $exp:y|]

        printKernelSize :: VName -> [C.Stm]
        printKernelSize work_size =
          intercalate [[C.cstm|fprintf(stderr, ", ");|]] $
          map (printKernelDim work_size) [0..kernel_rank-1]
        printKernelDim global_work_size i =
          [[C.cstm|fprintf(stderr, "%zu", $id:global_work_size[$int:i]);|]]
