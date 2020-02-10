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
    Right (Program shader_names sizes prog') -> do
      let cost_centres =
            [copyDevToDev, copyDevToHost, copyHostToDev,
             copyScalarToDev, copyScalarFromDev]
            ++ shader_names
      Right <$> GC.compileProg operations
                (generateBoilerplate undefined undefined shader_names sizes)
                include_opengl_h [Space "device", DefaultSpace]
                cliOptions prog'
  where operations :: GC.Operations OpenGL ()
        operations = GC.Operations
                     { GC.opsCompiler = callShader
                     , GC.opsWriteScalar = writeOpenGLScalar
                     , GC.opsReadScalar = readOpenGLScalar
                     , GC.opsAllocate = allocateOpenGLBuffer
                     , GC.opsDeallocate = deallocateOpenGLBuffer
                     , GC.opsCopy = copyOpenGLMemory
                     , GC.opsStaticArray = staticOpenGLArray
                     , GC.opsMemoryType = openglMemoryType
                     , GC.opsFatMemory = True
                     }
        include_opengl_h = unlines [undefined] -- necessary headers

copyDevToDev, copyDevToHost, copyHostToDev, copyScalarToDev, copyScalarFromDev :: String
copyDevToDev = "copy_dev_to_dev"
copyDevToHost = "copy_dev_to_host"
copyHostToDev = "copy_host_to_dev"
copyScalarToDev = "copy_scalar_to_dev"
copyScalarFromDev = "copy_scalar_from_dev"

cliOptions :: [Option]
cliOptions =
  []

writeOpenGLScalar :: GC.WriteScalar OpenGL ()
writeOpenGLScalar mem i t "device" _ val = do
  undefined
writeOpenGLScalar _ _ _ space _ _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

readOpenGLScalar :: GC.ReadScalar OpenGL ()
readOpenGLScalar mem i t "device" _ = do
  undefined
readOpenGLScalar _ _ _ space _ =
  error $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenGLBuffer :: GC.Allocate OpenGL ()
allocateOpenGLBuffer mem size tag "device" =
  undefined
allocateOpenGLBuffer _ _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' space."

deallocateOpenGLBuffer :: GC.Deallocate OpenGL ()
deallocateOpenGLBuffer mem tag "device" =
  undefined
deallocateOpenGLBuffer _ _ space =
  error $ "Cannot deallocate in '" ++ space ++ "' space"

copyOpenGLMemory :: GC.Copy OpenGL ()
copyOpenGLMemory destmem destidx DefaultSpace srcmem srcidx (Space "device") nbytes =
  undefined
copyOpenGLMemory destmem destidx (Space "device") srcmem srcidx DefaultSpace nbytes =
  undefined
copyOpenGLMemory destmem destidx (Space "device") srcmem srcidx (Space "device") nbytes =
  undefined
copyOpenGLMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes =
  undefined
copyOpenGLMemory _ _ destspace _ _ srcspace _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

openglMemoryType :: GC.MemoryType OpenGL ()
openglMemoryType "device" = undefined
openglMemoryType space =
  error $ "OpenGL backend does not support '" ++ space ++ "' memory space."

staticOpenGLArray :: GC.StaticArray OpenGL ()
staticOpenGLArray name "device" t vs = do
  undefined

staticOpenGLArray _ space _ _ =
  error $ "OpenGL backend cannot create static array in memory space '" ++ space ++ "'"

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

callShader (LaunchShader name args num_workgroups workgroup_size) = do
  undefined

launchShader :: C.ToExp a =>
                String -> [a] -> [a] -> a -> GC.CompilerM op s ()
launchShader shader_name num_workgroups workgroup_dims local_bytes = do
  undefined
