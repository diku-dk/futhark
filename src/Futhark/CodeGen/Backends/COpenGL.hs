{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.COpenGL
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import Control.Monad hiding (mapM)
import Data.List

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Futhark.Error
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax)
import Futhark.CodeGen.Backends.COpenGL.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenGL
import qualified Futhark.CodeGen.ImpGen.OpenGL as ImpGen
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Program opengl_code opengl_prelude shader_names sizes prog') -> do
      let cost_centres =
            [copyDevToDev, copyDevToHost, copyHostToDev,
             copyScalarToDev, copyScalarFromDev]
            ++ shader_names
      Right <$> GC.compileProg operations
                (generateBoilerplate opengl_code opengl_prelude
                 shader_names sizes)
                include_opengl_h [Space "device", DefaultSpace]
                cliOptions prog'
  where operations :: GC.Operations OpenGL ()
        operations = GC.Operations
                     { GC.opsCompiler    = callShader
                     , GC.opsWriteScalar = writeOpenGLScalar
                     , GC.opsReadScalar  = readOpenGLScalar
                     , GC.opsAllocate    = allocateOpenGLBuffer
                     , GC.opsDeallocate  = deallocateOpenGLBuffer
                     , GC.opsCopy        = copyOpenGLMemory
                     , GC.opsStaticArray = staticOpenGLArray
                     , GC.opsMemoryType  = openglMemoryType
                     , GC.opsFatMemory   = True
                     }
        include_opengl_h = unlines []

copyDevToDev, copyDevToHost, copyHostToDev, copyScalarToDev, copyScalarFromDev :: String
copyDevToDev      = "copy_dev_to_dev"
copyDevToHost     = "copy_dev_to_host"
copyHostToDev     = "copy_host_to_dev"
copyScalarToDev   = "copy_scalar_to_dev"
copyScalarFromDev = "copy_scalar_from_dev"

cliOptions :: [Option]
cliOptions =
  [

  ] --TODO

writeOpenGLScalar :: GC.WriteScalar OpenGL ()
writeOpenGLScalar mem i t "device" _ val = do
  val' <- newVName "write_tmp"
  GC.stm [C.cstm|glMapBufferRange(GL_SHADER_STORAGE_BUFFER,
                                  $exp:i,
                                  sizeof($ty:t),
                                  GL_MAP_WRITE_BIT
                                 );
                |]
  GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]
  -- TODO: Might need to sync and unmap here.
writeOpenGLScalar _ _ _ space _ _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

readOpenGLScalar :: GC.ReadScalar OpenGL ()
readOpenGLScalar mem i t "device" _ = do
  val <- newVName "read_res"
  GC.decl [C.cdecl|$ty:t $id:val;|]
  GC.stm [C.cstm|glMapBufferRange(GL_SHADER_STORAGE_BUFFER,
                                  $exp:i,
                                  sizeof($ty:t),
                                  GL_MAP_READ_BIT
                                 );
                |]
  GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]
  -- TODO: Might need to sync and unmap here.
  return [C.cexp|$id:val|]
readOpenGLScalar _ _ _ space _ =
  error $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenGLBuffer :: GC.Allocate OpenGL ()
allocateOpenGLBuffer mem size tag "device" =
  GC.stm [C.cstm|opengl_alloc(&ctx->opengl,
                              $exp:size, $exp:tag, &$exp:mem);|]
allocateOpenGLBuffer _ _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' space."

deallocateOpenGLBuffer :: GC.Deallocate OpenGL ()
deallocateOpenGLBuffer mem tag "device" =
    GC.stm [C.cstm|opengl_free(&ctx->opengl, $exp:mem, $exp:tag);|]
deallocateOpenGLBuffer _ _ space =
  error $ "Cannot deallocate in '" ++ space ++ "' space"

copyOpenGLMemory :: GC.Copy OpenGL ()
copyOpenGLMemory destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes = do
  mapped_mem <- newVName "mapped_src_memory"
  mem_t      <- openglMemoryType "device"
  GC.decl [C.cdecl|$ty:mem_t *$id:mapped_mem;|]
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      $id:mapped_mem =
        ($ty:mem_t*)glMapNamedBuffer($exp:srcmem, GL_READ_ONLY);
        OPENGL_SUCCEED(glGetError());
   }|]
  GC.copyMemoryDefaultSpace destmem destidx [C.cexp|$id:mapped_mem|] srcidx nbytes
  GC.stm [C.cstm|
  if ($exp:nbytes > 0) {
    glUnmapNamedBuffer($exp:srcmem);
    OPENGL_SUCCEED(glGetError());
  }|]
copyOpenGLMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes = do
  mapped_mem <- newVName "mapped_dest_memory"
  mem_t      <- openglMemoryType "device"
  GC.decl [C.cdecl|$ty:mem_t *$id:mapped_mem;|]
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      $id:mapped_mem =
        ($ty:mem_t*)glMapNamedBuffer($exp:destmem, GL_WRITE_ONLY);
        OPENGL_SUCCEED(glGetError());
    }|]
  GC.copyMemoryDefaultSpace [C.cexp|$id:mapped_mem|] destidx srcmem srcidx nbytes
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      glUnmapNamedBuffer($exp:destmem);
      OPENGL_SUCCEED(glGetError());
    }|]
copyOpenGLMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  GC.stm [C.cstm|{
    if ($exp:nbytes > 0) {
      glCopyNamedBufferSubData($exp:srcmem, $exp:destmem,
                               $exp:srcidx, $exp:destidx,
                               $exp:nbytes);
      OPENGL_SUCCEED(glGetError());
    }
  }|]
copyOpenGLMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  GC.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyOpenGLMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

openglMemoryType :: GC.MemoryType OpenGL ()
openglMemoryType "device" = pure [C.cty|unsigned int|]
openglMemoryType space =
  error $ "OpenGL backend does not support '" ++ space ++ "' memory space."

staticOpenGLArray :: GC.StaticArray OpenGL ()
staticOpenGLArray name "device" t vs = do
  let ct = GC.primTypeToCType t
  name_realtype <- newVName $ baseString name ++ "_realtype"
  mapped_mem <- newVName "mapped_src_memory"
  mem_t <- openglMemoryType "device"
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
    ctx->$id:name.references = NULL;
    ctx->$id:name.size = 0;
    opengl_alloc(&ctx->opengl,
                 ($int:num_elems > 0 ? $int:num_elems : 1)*sizeof($ty:ct),
                 $string:(pretty name),
                 &ctx->$id:name.mem);
    OPENGL_SUCCEED(glGetError());
    if ($int:num_elems > 0) {
      $ty:mem_t *$id:mapped_mem;
      $id:mapped_mem =
        ($ty:mem_t*)glMapNamedBuffer(ctx->$id:name.mem, GL_READ_ONLY);
      memcpy($id:mapped_mem, &$id:name_realtype, $int:num_elems*sizeof($ty:ct));
      glUnmapNamedBuffer(ctx->$id:name.mem);
    }
    OPENGL_SUCCEED(glGetError());
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->$id:name;|]

staticOpenGLArray _ space _ _ =
  error $ "OpenGL backend cannot create static array in memory space '" ++ space ++ "'"

-- TODO:
callShader :: GC.OpCompiler OpenGL ()
callShader (GetSize v key) =
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key;|]
callShader (CmpSizeLe v key x) = do
  x' <- GC.compileExp x
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key <= $exp:x';|]
  GC.stm [C.cstm|if (ctx->logging) {
    fprintf(stderr, "Compared %s <= %d.\n", $string:(pretty key), $exp:x');
    }|]
callShader (GetSizeMax v size_class) =
  let field = "max_" ++ pretty size_class
  in GC.stm [C.cstm|$id:v = ctx->opencl.$id:field;|]

--TODO: Accommodate OpenGL {
callShader (LaunchShader name args num_workgroups workgroup_size) = do
  zipWithM_ setKernelArg [(0::Int)..] args
  num_workgroups' <- mapM GC.compileExp num_workgroups
  workgroup_size' <- mapM GC.compileExp workgroup_size
  local_bytes <- foldM localBytes [C.cexp|0|] args
  launchShader name num_workgroups' workgroup_size' local_bytes
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

--TODO: Accommodate OpenGL {
launchShader :: C.ToExp a =>
                String -> [a] -> [a] -> a -> GC.CompilerM op s ()
launchShader shader_name num_workgroups workgroup_dims local_bytes = do
  global_work_size <- newVName "global_work_size"
  time_start <- newVName "time_start"
  time_end <- newVName "time_end"
  time_diff <- newVName "time_diff"
  local_work_size <- newVName "local_work_size"

  GC.stm [C.cstm|
    if ($exp:total_elements != 0) {
    }|]
    where kernel_rank = length kernel_dims
          kernel_dims = zipWith multExp num_workgroups workgroup_dims
          kernel_dims' = map toInit kernel_dims
          workgroup_dims' = map toInit workgroup_dims
          total_elements = foldl multExp [C.cexp|1|] kernel_dims

          toInit e = [C.cinit|$exp:e|]
          multExp x y = [C.cexp|$exp:x * $exp:y|]
--TODO: Accommodate OpenGL}}
