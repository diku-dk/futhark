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

data ShaderState =
  ShaderState { shaderLocalMemory :: [LocalMemoryUse]
              , shaderNextSync    :: Int
              , shaderSyncPending :: Bool
                -- ^ Has a potential failure occurred since the last
                -- ErrorSync?
              , shaderHasBarriers :: Bool
              }

errorLabel :: ShaderState -> String
errorLabel = ("error_"++) . show . shaderNextSync

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
--onHostOp _ (CallShader s) = onShader s
onHostOp _ (ImpKernels.GetSize v key size_class) = do
 tell mempty { glSizes = M.singleton key size_class }
 return $ ImpOpenGL.GetSize v key
onHostOp _ (ImpKernels.CmpSizeLe v key size_class x) = do
 tell mempty { glSizes = M.singleton key size_class }
 return $ ImpOpenGL.CmpSizeLe v key x
onHostOp _ (ImpKernels.GetSizeMax v size_class) =
 return $ ImpOpenGL.GetSizeMax v size_class

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

nextErrorLabel :: GenericC.CompilerM KernelOp ShaderState String
nextErrorLabel =
  errorLabel <$> GenericC.getUserState

incErrorLabel :: GenericC.CompilerM KernelOp ShaderState ()
incErrorLabel =
  GenericC.modifyUserState $ \s -> s { shaderNextSync = shaderNextSync s + 1 }

pendingError :: Bool -> GenericC.CompilerM KernelOp ShaderState ()
pendingError b =
  GenericC.modifyUserState $ \s -> s { shaderSyncPending = b }

hasCommunication :: ImpKernels.KernelCode -> Bool
hasCommunication = any communicates
  where communicates ErrorSync{} = True
        communicates Barrier{}   = True
        communicates _           = False

pointerQuals ::  Monad m => String -> m [C.TypeQual]
-- Layout qualifiers:
--pointerQuals "layout"    = return [C.ctyquals|layout|]
-- Constant qualifier
pointerQuals "const"     = return [C.ctyquals|const|]
-- Uniform qualifiers:
--pointerQuals "uniform"   = return [C.ctyquals|uniform|]
-- Shader stage input and output qualifiers:
--pointerQuals "in"        = return [C.ctyquals|in|]
--pointerQuals "out"       = return [C.ctyquals|out|]
-- Shared variables:
--pointerQuals "shared"    = return [C.ctyquals|shared|]
-- Memory qualifiers:
--pointerQuals "coherent"  = return [C.ctyquals|coherent|]
pointerQuals "volatile"  = return [C.ctyquals|volatile|]
pointerQuals "restrict"  = return [C.ctyquals|restrict|]
--pointerQuals "readonly"  = return [C.ctyquals|readonly|]
--pointerQuals "writeonly" = return [C.ctyquals|writeonly|]
pointerQuals s           = error $ "'" ++ s ++ "' is not an OpenGL type qualifier."

inShaderOperations :: ImpKernels.KernelCode -> GenericC.Operations KernelOp ShaderState
inShaderOperations body =
  GenericC.Operations
  { GenericC.opsCompiler    = shaderOps
  , GenericC.opsMemoryType  = shaderMemoryType
  , GenericC.opsWriteScalar = shaderWriteScalar
  , GenericC.opsReadScalar  = shaderReadScalar
  , GenericC.opsAllocate    = cannotAllocate
  , GenericC.opsDeallocate  = cannotDeallocate
  , GenericC.opsCopy        = copyInShader
  , GenericC.opsStaticArray = noStaticArrays
  , GenericC.opsFatMemory   = False
  }
  where has_communication = hasCommunication body

        shaderOps :: GenericC.OpCompiler KernelOp ShaderState
        shaderOps (GetGroupId v i) =
          GenericC.stm [C.cstm|$id:v = get_group_id($int:i);|]
        shaderOps (GetLocalId v i) =
          GenericC.stm [C.cstm|$id:v = get_local_id($int:i);|]
        shaderOps (GetLocalSize v i) =
          GenericC.stm [C.cstm|$id:v = get_local_size($int:i);|]
        shaderOps (GetGlobalId v i) =
          GenericC.stm [C.cstm|$id:v = get_global_id($int:i);|]
        shaderOps (GetGlobalSize v i) =
          GenericC.stm [C.cstm|$id:v = get_global_size($int:i);|]
        shaderOps (GetLockstepWidth v) =
          GenericC.stm [C.cstm|$id:v = LOCKSTEP_WIDTH;|]
        shaderOps (Barrier f) = do
          GenericC.stm [C.cstm|barrier();|]
          GenericC.modifyUserState $ \s -> s { shaderHasBarriers = True }
        shaderOps (MemFence FenceLocal) =
          GenericC.stm [C.cstm|groupMemoryBarrier();|]
        shaderOps (MemFence FenceGlobal) =
          GenericC.stm [C.cstm|memoryBarrier();|]
        shaderOps (LocalAlloc name size) = do
          name' <- newVName $ pretty name ++ "_backing"
          GenericC.modifyUserState $ \s ->
            s { shaderLocalMemory = (name', size) : shaderLocalMemory s }
          GenericC.stm [C.cstm|$id:name = (__local char*) $id:name';|]
        shaderOps (ErrorSync f) = do
          label   <- nextErrorLabel
          pending <- shaderSyncPending <$> GenericC.getUserState
          when pending $ do
            pendingError False
            GenericC.stm [C.cstm|$id:label: barrier();|]
            GenericC.stm [C.cstm|if (local_failure) { return; }|]
          GenericC.stm [C.cstm|groupMemoryBarrier();|] -- intentional
          GenericC.modifyUserState $ \s -> s { shaderHasBarriers = True }
          incErrorLabel
        shaderOps (Atomic space aop) = atomicOps space aop

        atomicCast s t = do
          let volatile = [C.ctyquals|volatile|]
          quals <- case s of Space sid    -> pointerQuals sid
                             _            -> pointerQuals "uniform"
          return [C.cty|$tyquals:(volatile++quals) $ty:t|]

        doAtomic s old arr ind val op ty = do
          ind' <- GenericC.compileExp $ unCount ind
          val' <- GenericC.compileExp val
          cast <- atomicCast s ty
          GenericC.stm [C.cstm|$id:old = $id:op(&(($ty:cast *)$id:arr)[$exp:ind'], ($ty:ty) $exp:val');|]

        atomicOps s (AtomicAdd old arr ind val) =
          doAtomic s old arr ind val "atomic_add" [C.cty|int|]

        atomicOps s (AtomicSMax old arr ind val) =
          doAtomic s old arr ind val "atomic_max" [C.cty|int|]

        atomicOps s (AtomicSMin old arr ind val) =
          doAtomic s old arr ind val "atomic_min" [C.cty|int|]

        atomicOps s (AtomicUMax old arr ind val) =
          doAtomic s old arr ind val "atomic_max" [C.cty|uint|]

        atomicOps s (AtomicUMin old arr ind val) =
          doAtomic s old arr ind val "atomic_min" [C.cty|uint|]

        atomicOps s (AtomicAnd old arr ind val) =
          doAtomic s old arr ind val "atomic_and" [C.cty|uint|]

        atomicOps s (AtomicOr old arr ind val) =
          doAtomic s old arr ind val "atomic_or" [C.cty|uint|]

        atomicOps s (AtomicXor old arr ind val) =
          doAtomic s old arr ind val "atomic_xor" [C.cty|uint|]

        atomicOps s (AtomicCmpXchg old arr ind cmp val) = do
          ind' <- GenericC.compileExp $ unCount ind
          cmp' <- GenericC.compileExp cmp
          val' <- GenericC.compileExp val
          cast <- atomicCast s [C.cty|int|]
          GenericC.stm [C.cstm|$id:old = atomic_cmpxchg(&(($ty:cast *)$id:arr)[$exp:ind'], $exp:cmp', $exp:val');|]

        atomicOps s (AtomicXchg old arr ind val) = do
          ind' <- GenericC.compileExp $ unCount ind
          val' <- GenericC.compileExp val
          cast <- atomicCast s [C.cty|int|]
          GenericC.stm [C.cstm|$id:old = atomic_xchg(&(($ty:cast *)$id:arr)[$exp:ind'], $exp:val');|]

        cannotAllocate :: GenericC.Allocate KernelOp ShaderState
        cannotAllocate _ =
          error "Cannot allocate memory in shader."

        cannotDeallocate :: GenericC.Deallocate KernelOp ShaderState
        cannotDeallocate _ _ =
          error "Cannot deallocate memory in shader."

        copyInShader :: GenericC.Copy KernelOp ShaderState
        copyInShader _ _ _ _ _ _ _ =
          error "Cannot bulk copy in shader."

        noStaticArrays :: GenericC.StaticArray KernelOp ShaderState
        noStaticArrays _ _ _ _ =
          error "Cannot create static array in shader."

        shaderMemoryType space = do
          return [C.cty|$ty:defaultMemBlockType|]

        shaderWriteScalar =
          GenericC.writeScalarPointerWithQuals pointerQuals

        shaderReadScalar =
          GenericC.readScalarPointerWithQuals pointerQuals

typesInCode :: ImpKernels.KernelCode -> S.Set PrimType
typesInCode Skip                     = mempty
typesInCode (c1 :>>: c2)             = typesInCode c1 <> typesInCode c2
typesInCode (For _ it e c)           = IntType it `S.insert` typesInExp e <> typesInCode c
typesInCode (While e c)              = typesInExp e <> typesInCode c
typesInCode DeclareMem{}             = mempty
typesInCode (DeclareScalar _ _ t)    = S.singleton t
typesInCode (DeclareArray _ _ t _)   = S.singleton t
typesInCode (Allocate _ (Count e) _) = typesInExp e
typesInCode Free{}                   = mempty
typesInCode (Copy _ (Count e1) _ _ (Count e2) _ (Count e3)) =
  typesInExp e1 <> typesInExp e2 <> typesInExp e3
typesInCode (Write _ (Count e1) t _ _ e2) =
  typesInExp e1 <> S.singleton t <> typesInExp e2
typesInCode (SetScalar _ e)   = typesInExp e
typesInCode SetMem{}          = mempty
typesInCode (Call _ _ es)     = mconcat $ map typesInArg es
  where typesInArg MemArg{}   = mempty
        typesInArg (ExpArg e) = typesInExp e
typesInCode (If e c1 c2) =
  typesInExp e <> typesInCode c1 <> typesInCode c2
typesInCode (Assert e _ _)   = typesInExp e
typesInCode (Comment _ c)    = typesInCode c
typesInCode (DebugPrint _ v) = maybe mempty typesInExp v
typesInCode Op{} = mempty

typesInExp :: Exp -> S.Set PrimType
typesInExp (ValueExp v)       = S.singleton $ primValueType v
typesInExp (BinOpExp _ e1 e2) = typesInExp e1 <> typesInExp e2
typesInExp (CmpOpExp _ e1 e2) = typesInExp e1 <> typesInExp e2
typesInExp (ConvOpExp op e)   = S.fromList [from, to] <> typesInExp e
  where (from, to) = convOpType op
typesInExp (UnOpExp _ e)     = typesInExp e
typesInExp (FunExp _ args t) = S.singleton t <> mconcat (map typesInExp args)
typesInExp (LeafExp (Index _ (Count e) t _ _) _) = S.singleton t <> typesInExp e
typesInExp (LeafExp ScalarVar{} _) = mempty
typesInExp (LeafExp (SizeOf t) _)  = S.singleton t
