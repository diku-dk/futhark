{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
-- | This module defines a translation from imperative code with
-- kernels to imperative code with OpenGL calls.
module Futhark.CodeGen.ImpGen.Kernels.ToOpenGL
  ( kernelsToOpenGL
  )
  where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Language.C.Syntax as C
import qualified Language.C.Quote as C

import qualified Language.C.Quote.OpenCL as C

import Futhark.Error
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.CodeGen.Backends.SimpleRepresentation
import Futhark.CodeGen.ImpCode.Kernels hiding (Program)
import qualified Futhark.CodeGen.ImpCode.Kernels as ImpKernels
import Futhark.CodeGen.ImpCode.OpenGL hiding (Program)
import qualified Futhark.CodeGen.ImpCode.OpenGL as ImpOpenGL
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeString)

kernelsToOpenGL :: ImpKernels.Program
                -> Either InternalError ImpOpenGL.Program
kernelsToOpenGL = translateKernels

-- | Translate a kernels-program to an OpenGL-program.
translateKernels :: ImpKernels.Program
                 -> Either InternalError ImpOpenGL.Program
translateKernels (ImpKernels.Functions funs) = do
  (prog', ToOpenGL shaders used_types sizes) <-
    runWriterT $ fmap Functions $ forM funs $ \(fname, fun) ->
    (fname,) <$> runReaderT (traverse (onHostOp fname) fun) fname
  let shaders'       = M.map fst shaders
      opengl_code    = openGlCode $ map snd $ M.elems shaders
      opengl_prelude = pretty $ genOpenGlPrelude used_types
  return $ ImpOpenGL.Program opengl_code opengl_prelude shaders'
    (S.toList used_types) sizes prog'

type LocalMemoryUse = (VName, Count Bytes Exp)

newtype KernelRequirements =
  KernelRequirements { kernelLocalMemory :: [LocalMemoryUse] }

instance Semigroup KernelRequirements where
  KernelRequirements lm1 <> KernelRequirements lm2 =
    KernelRequirements (lm1<>lm2)

instance Monoid KernelRequirements where
  mempty = KernelRequirements mempty

newtype OpenGlRequirements =
  OpenGlRequirements { openglUsedTypes :: S.Set PrimType }

instance Semigroup OpenGlRequirements where
  OpenGlRequirements ts1 <> OpenGlRequirements ts2 =
    OpenGlRequirements (ts1 <> ts2)

instance Monoid OpenGlRequirements where
  mempty = OpenGlRequirements mempty

data ToOpenGL = ToOpenGL { glShaders   :: M.Map ShaderName (Safety, C.Func)
                         , glUsedTypes :: S.Set PrimType
                         , glSizes     :: M.Map Name SizeClass
                         }

instance Semigroup ToOpenGL where
 ToOpenGL k1 r1 sz1 <> ToOpenGL k2 r2 sz2 =
   ToOpenGL (k1<>k2) (r1<>r2) (sz1<>sz2)

instance Monoid ToOpenGL where
 mempty = ToOpenGL mempty mempty mempty

type OnShaderM = ReaderT Name (WriterT ToOpenGL (Either InternalError))

onHostOp :: kernelsToOpenGL -> HostOp -> OnShaderM OpenGL
--onHostOp (CallShader s) = onShader s
onHostOp _ (ImpKernels.GetSize v key size_class) = do
 tell mempty { glSizes = M.singleton key size_class }
 return $ ImpOpenGL.GetSize v key
onHostOp _ (ImpKernels.CmpSizeLe v key size_class x) = do
 tell mempty { glSizes = M.singleton key size_class }
 return $ ImpOpenGL.CmpSizeLe v key x
onHostOp _ (ImpKernels.GetSizeMax v size_class) =
 return $ ImpOpenGL.GetSizeMax v size_class

--onShader :: Shader -> OnShaderM OpenGL

openGlCode :: [C.Func] -> String
openGlCode shaders =
 pretty [C.cunit|$edecls:funcs|]
 where funcs =
         [[C.cedecl|$func:shader_func|] |
          shader_func <- shaders ]

genOpenGlPrelude :: S.Set PrimType -> [C.Definition]
genOpenGlPrelude ts =
  [ [C.cedecl|$esc:("#version 450")|]
  , [C.cedecl|$esc:("#extension GL_ARB_compute_variable_group_size : enable")|]
  , [C.cedecl|$esc:("#extension GL_ARB_gpu_shader_int64 : enable")|]
  , [C.cedecl|$esc:("layout (local_size_variable) in;")|]
  ] ++ glIntOps  ++ glFloat32Ops  ++ glFloat32Funs ++
    glFloat64Ops ++ glFloat64Funs ++ glFloatConvOps
