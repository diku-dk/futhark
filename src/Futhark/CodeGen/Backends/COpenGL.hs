{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
module Futhark.CodeGen.Backends.COpenGL
  ( compileProg
  , GC.CParts(..)
  , GC.asLibrary
  , GC.asExecutable
  ) where

import Control.Monad hiding (mapM)
import Data.List
import qualified Data.Map as M

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Futhark.Error
import Futhark.Representation.ExplicitMemory hiding (GetSize, CmpSizeLe, GetSizeMax)
import Futhark.CodeGen.Backends.COpenGL.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.Backends.SimpleRepresentation as SR
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.ImpCode.OpenGL
import qualified Futhark.CodeGen.ImpGen.OpenGL as ImpGen
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either InternalError GC.CParts)
compileProg prog = do
  res <- ImpGen.compileProg prog
  case res of
    Left err -> return $ Left err
    Right (Program opengl_code opengl_prelude shaders types sizes prog') -> do
      let cost_centres =
            [copyDevToDev, copyDevToHost, copyHostToDev,
             copyScalarToDev, copyScalarFromDev]
            ++ M.keys shaders
      Right <$> GC.compileProg GC.TargetHost operations
                (generateBoilerplate opengl_code opengl_prelude
                 shaders sizes)
                include_opengl_h [Space "device", DefaultSpace]
                cliOptions prog'
  where operations :: GC.Operations OpenGL ()
        operations = GC.defaultOperations
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
  GC.decl [C.cdecl|$ty:t $id:val' = $exp:val;|]
  GC.stm [C.cstm|glNamedBufferSubData($exp:mem,
                                      $exp:i,
                                      sizeof($ty:t),
                                      &$exp:val'
                                     );
                |]
  GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]
  -- TODO: Might need to sync here.
writeOpenGLScalar _ _ _ space _ _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

readOpenGLScalar :: GC.ReadScalar OpenGL ()
readOpenGLScalar mem i t "device" _ = do
  val <- newVName "read_res"
  GC.decl [C.cdecl|$ty:t *$id:val;|]
  GC.stm [C.cstm|$id:val =
    ($ty:t*)glMapNamedBufferRange($exp:mem,
                                  $exp:i * sizeof($ty:t),
                                  sizeof($ty:t),
                                  GL_MAP_READ_BIT
                                 );
                |]
  GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]
  GC.stm [C.cstm|glUnmapNamedBuffer($exp:mem);|]
  GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]
  -- TODO: Might need to sync here.
  return [C.cexp|*$id:val|]
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
        ($ty:mem_t*)glMapNamedBufferRange($exp:srcmem,
                                          $exp:srcidx,
                                          $exp:nbytes,
                                          GL_MAP_READ_BIT);
        OPENGL_SUCCEED(glGetError());
        glUnmapNamedBuffer($exp:srcmem);
        OPENGL_SUCCEED(glGetError());
   }|]
  GC.copyMemoryDefaultSpace destmem destidx [C.cexp|$id:mapped_mem|] srcidx nbytes
copyOpenGLMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes =
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      glNamedBufferSubData($exp:destmem,
                           $exp:destidx,
                           $exp:nbytes,
                           ($exp:srcmem + $exp:srcidx)
                          );
      OPENGL_SUCCEED(glGetError());
    }|]
copyOpenGLMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  GC.stm [C.cstm|
    if ($exp:nbytes > 0) {
      glCopyNamedBufferSubData($exp:srcmem, $exp:destmem,
                               $exp:srcidx, $exp:destidx,
                               $exp:nbytes);
      OPENGL_SUCCEED(glGetError());
    }
  |]
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
  num_elems <- case vs of
    ArrayValues vs' -> do
      let vs'' = [[C.cinit|$exp:v|] | v <- map (GC.compilePrimValue GC.TargetHost) vs']
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
      glNamedBufferSubData(ctx->$id:name.mem, 0, $int:num_elems*sizeof($ty:ct),
                           $id:name_realtype);
      OPENGL_SUCCEED(glGetError());
    }
  }|]
  GC.item [C.citem|struct memblock_device $id:name = ctx->$id:name;|]

staticOpenGLArray _ space _ _ =
  error $ "OpenGL backend cannot create static array in memory space '" ++ space ++ "'"

callShader :: GC.OpCompiler OpenGL ()
callShader (GetSize v key) =
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key;|]
callShader (CmpSizeLe v key x) = do
  x' <- GC.compileExp GC.TargetHost x
  GC.stm [C.cstm|$id:v = ctx->sizes.$id:key <= $exp:x';|]
  GC.stm [C.cstm|if (ctx->logging) {
    fprintf(stderr, "Compared %s <= %d.\n", $string:(pretty key), $exp:x');
    }|]
callShader (GetSizeMax v size_class) =
  let field = "max_" ++ pretty size_class
  in GC.stm [C.cstm|$id:v = ctx->opengl.$id:field;|]

callShader (LaunchShader safety name args num_workgroups workgroup_size) = do
  -- FIXME: We might account for safety by using a uniform.
  when (safety == SafetyFull) $
    GC.stm [C.cstm|
    OPENGL_SUCCEED(glGetError());
    |]
  GC.stm [C.cstm|glUseProgram(ctx->opengl.program);|]
  zipWithM_ setShaderArg [(0::Int)..] args
  num_workgroups' <- mapM (GC.compileExp GC.TargetHost) num_workgroups
  workgroup_size' <- mapM (GC.compileExp GC.TargetHost) workgroup_size
  local_bytes     <- foldM localBytes [C.cexp|0|] args
  launchShader name num_workgroups' workgroup_size' local_bytes
  where setShaderArg i (ValueKArg e bt) = do
          v <- GC.compileExpToName GC.TargetHost "shader_arg" bt e
          let ty = SR.primTypeToGLSLType bt
          case ty of
            [C.cty|float|] ->
              GC.stm [C.cstm|glUniform1f($int:i, $id:v);|]
            [C.cty|double|] ->
              GC.stm [C.cstm|glUniform1d($int:i, $id:v);|]
            [C.cty|typename int64_t|] ->
              GC.stm [C.cstm|glUniform1i64ARB($int:i, $id:v);|]
            _ ->
              GC.stm [C.cstm|glUniform1i($int:i, (typename GLint)$id:v);|]
          GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]

        setShaderArg i (MemKArg v) = do
          v' <- GC.rawMem v
          GC.stm [C.cstm|glBindBufferBase(GL_SHADER_STORAGE_BUFFER, $int:i,
                                          $exp:v');|]
          GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]

        -- FIXME: This creates global memory storage, in contrast
        --        to its purpose; to create local memory per work group.
        -- FIXME: Deallocate after dispatching.
        setShaderArg i (SharedMemoryKArg num_bytes) = do
          num_bytes' <- GC.compileExp GC.TargetHost $ unCount num_bytes
          ssbo       <- newVName "ssbo"
          GC.libDecl [C.cedecl|typename GLuint $id:ssbo;|]
          GC.stm [C.cstm|glCreateBuffers(1, &$id:ssbo);|]
          GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]
          GC.stm [C.cstm|glNamedBufferData($id:ssbo, $exp:num_bytes',
                                           NULL, GL_DYNAMIC_DRAW);|]
          GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]
          GC.stm [C.cstm|glBindBufferBase(GL_SHADER_STORAGE_BUFFER, $int:i,
                                          $id:ssbo);|]
          GC.stm [C.cstm|OPENGL_SUCCEED(glGetError());|]

        localBytes cur (SharedMemoryKArg num_bytes) = do
          num_bytes' <- GC.compileExp GC.TargetHost $ unCount num_bytes
          return [C.cexp|$exp:cur + $exp:num_bytes'|]
        localBytes cur _ = return cur

launchShader :: C.ToExp a =>
                String -> [a] -> [a] -> a -> GC.CompilerM op s ()
launchShader shader_name num_workgroups workgroup_dims local_bytes = do
  global_work_size <- newVName "global_work_size"
  time_start       <- newVName "time_start"
  time_end         <- newVName "time_end"
  time_diff        <- newVName "time_diff"
  local_work_size  <- newVName "local_work_size"
  GC.stm [C.cstm|
    if ($exp:total_elements != 0) {
      typename GLuint $id:global_work_size[3] = {$inits:shader_dims'};
      typename GLuint $id:local_work_size[3]  = {$inits:workgroup_dims'};
      if($id:global_work_size[1] == NULL) {
        $id:global_work_size[1] = 1;
      }
      if($id:global_work_size[2] == NULL) {
        $id:global_work_size[2] = 1;
      }
      if($id:local_work_size[1] == NULL) {
        $id:local_work_size[1] = 1;
      }
      if($id:local_work_size[2] == NULL) {
        $id:local_work_size[2] = 1;
      }
      typename int64_t $id:time_start = 0, $id:time_end = 0;
    if (ctx->debugging) {
      fprintf(stderr, "Launching %s with global work size [", $string:shader_name);
      $stms:(printShaderSize global_work_size)
      fprintf(stderr, "] and local work size [");
      $stms:(printShaderSize local_work_size)
      fprintf(stderr, "]; local memory parameters sum to %d bytes.\n", (int)$exp:local_bytes);
      $id:time_start = get_wall_time();
    }
    OPENGL_SUCCEED(glGetError());
    glDispatchComputeGroupSizeARB($id:global_work_size[0], $id:global_work_size[1],
                                  $id:global_work_size[2], $id:local_work_size[0],
                                  $id:local_work_size[1],  $id:local_work_size[2]
                                 );
    glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT);
    OPENGL_SUCCEED(glGetError());
    glDeleteProgram(ctx->opengl.program);
    if (ctx->debugging) {
      glFinish();
      OPENGL_SUCCEED(glGetError());
      $id:time_end = get_wall_time();
      long int $id:time_diff = $id:time_end - $id:time_start;
      fprintf(stderr, "shader %s runtime: %ldus\n",
              $string:shader_name, $id:time_diff);
    }
    }|]
    where shader_rank     = length shader_dims
          shader_dims     = zipWith multExp num_workgroups workgroup_dims
          shader_dims'    = map toInit shader_dims
          workgroup_dims' = map toInit workgroup_dims
          total_elements  = foldl multExp [C.cexp|1|] shader_dims

          toInit e    = [C.cinit|$exp:e|]
          multExp x y = [C.cexp|$exp:x * $exp:y|]

          printShaderSize :: VName -> [C.Stm]
          printShaderSize work_size =
            intercalate [[C.cstm|fprintf(stderr, ", ");|]] $
            map (printKernelDim work_size) [0..shader_rank-1]
          printKernelDim global_work_size i =
            [[C.cstm|fprintf(stderr, "%zu", $id:global_work_size[$int:i]);|]]
