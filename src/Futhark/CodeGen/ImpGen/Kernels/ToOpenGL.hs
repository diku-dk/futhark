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
    flip runStateT initialOpenGL $ fmap Functions $ forM funs $ \(fname, fun) ->
    (fname,) <$> runReaderT (traverse (onHostOp fname) fun) fname
  let shaders'       = M.map fst shaders
      opengl_code    = openGlCode $ map snd $ M.elems shaders
      opengl_prelude = pretty $ genOpenGlPrelude used_types
  return $ ImpOpenGL.Program opengl_code opengl_prelude shaders'
    (S.toList used_types) (cleanSizes sizes) prog'

-- | Due to simplifications after kernel extraction, some threshold
-- parameters may contain KernelPaths that reference threshold
-- parameters that no longer exist.  We remove these here.
cleanSizes :: M.Map Name SizeClass -> M.Map Name SizeClass
cleanSizes m  = M.map clean m
  where known = M.keys m
        clean (SizeThreshold path) =
          SizeThreshold $ filter ((`elem` known) . fst) path
        clean s = s

type LocalMemoryUse = (VName, Count Bytes Exp)

data ShaderState =
  ShaderState { shaderLocalMemory :: [LocalMemoryUse]
              , shaderNextSync    :: Int
              , shaderSyncPending :: Bool
                -- ^ Has a potential failure occurred since the last
                -- ErrorSync?
              , shaderHasBarriers :: Bool
              }

newShaderState :: ShaderState
newShaderState = ShaderState mempty 0 False False

errorLabel :: ShaderState -> String
errorLabel = ("error_"++) . show . shaderNextSync

data ToOpenGL = ToOpenGL { glShaders   :: M.Map ShaderName (Safety, C.Func)
                         , glUsedTypes :: S.Set PrimType
                         , glSizes     :: M.Map Name SizeClass
                         }

initialOpenGL :: ToOpenGL
initialOpenGL = ToOpenGL mempty mempty mempty

type OnShaderM = ReaderT Name (StateT ToOpenGL (Either InternalError))

addSize :: Name -> SizeClass -> OnShaderM ()
addSize key sclass =
  modify $ \s -> s { glSizes = M.insert key sclass $ glSizes s }

onHostOp :: kernelsToOpenGL -> HostOp -> OnShaderM OpenGL
onHostOp _ (CallKernel s) = onShader s
onHostOp _ (ImpKernels.GetSize v key size_class) = do
 addSize key size_class
 return $ ImpOpenGL.GetSize v key
onHostOp _ (ImpKernels.CmpSizeLe v key size_class x) = do
 addSize key size_class
 return $ ImpOpenGL.CmpSizeLe v key x
onHostOp _ (ImpKernels.GetSizeMax v size_class) =
 return $ ImpOpenGL.GetSizeMax v size_class

onShader :: Kernel -> OnShaderM OpenGL
onShader shader = do
  let (shader_body, cstate) =
        GenericC.runCompilerM mempty (inShaderOperations (kernelBody shader))
        blankNameSource
        newShaderState $
        GenericC.blockScope $ GenericC.compileCode $ kernelBody shader
      s_state = GenericC.compUserState cstate

      use_params = mapMaybe useAsParam $ kernelUses shader

      (local_memory_args, local_memory_params, local_memory_init) =
        unzip3 $
        flip evalState (blankNameSource :: VNameSource) $
        mapM prepareLocalMemory $ shaderLocalMemory s_state

      (perm_params, block_dim_init) =
        (mempty,
         [[C.citem|const int block_dim0 = 0;|],
          [C.citem|const int block_dim1 = 1;|],
          [C.citem|const int block_dim2 = 2;|]]
        )

      (const_defs, const_undefs) = unzip $ mapMaybe constDef $ kernelUses shader

-- We do currently not account safety within shaders.
  let (safety, error_init) = (SafetyNone, [])

      params = perm_params ++ catMaybes local_memory_params ++ use_params

      shader_fun =
        [C.cfun|void $id:name ($params:params) {
                  $items:const_defs
                  $items:block_dim_init
                  $items:local_memory_init
                  $items:shader_body

                  $items:const_undefs
                }|]
  modify $ \s -> s
    { glShaders   = M.insert name (safety, shader_fun) $ glShaders s
    , glUsedTypes = typesInShader shader <> glUsedTypes s
    }

  let args = catMaybes local_memory_args ++
             kernelArgs shader

  return $ LaunchShader safety name args num_groups group_size
  where name = nameToString $ kernelName shader
        num_groups = kernelNumGroups shader
        group_size = kernelGroupSize shader

        prepareLocalMemory (mem, size) = do
          param <- newVName $ baseString mem ++ "_offset"
          return (Just $ SharedMemoryKArg size,
                  Just [C.cparam|uint $id:param|],
                  [C.citem|volatile int *$id:mem = &shared_mem[$id:param];|])

useAsParam :: KernelUse -> Maybe C.Param
useAsParam (ScalarUse name bt) =
  let ctp = case bt of
        Bool -> [C.cty|bool|]
        _    -> GenericC.primTypeToCType bt
  in Just [C.cparam|$ty:ctp $id:name|]
useAsParam (MemoryUse name) =
  Just [C.cparam|bool *$id:name|]
useAsParam ConstUse{} =
  Nothing

constDef :: KernelUse -> Maybe (C.BlockItem, C.BlockItem)
constDef _ = Nothing

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
  , [C.cedecl|$esc:("#extension GL_ARB_gpu_shader_fp64 : enable")|]
  , [C.cedecl|$esc:("layout (local_size_variable) in;")|]
  , [C.cedecl|$esc:("#define int8_t int")|]
  , [C.cedecl|$esc:("#define int16_t int")|]
  , [C.cedecl|$esc:("#define int32_t int")|]
  , [C.cedecl|$esc:("#define uint8_t uint")|]
  , [C.cedecl|$esc:("#define uint16_t uint")|]
  , [C.cedecl|$esc:("#define uint32_t uint")|]
  , [C.cedecl|$esc:("#define float32 float")|]
  , [C.cedecl|$esc:("#define float64 double")|]
  , [C.cedecl|$esc:("#define E_CONST 2.71828182845904523536028747135266250")|]
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

kernelArgs :: Kernel -> [ShaderArg]
kernelArgs = mapMaybe useToArg . kernelUses
  where useToArg (MemoryUse mem)  = Just $ MemKArg mem
        useToArg (ScalarUse v bt) = Just $ ValueKArg (LeafExp (ScalarVar v) bt) bt
        useToArg ConstUse{}       = Nothing

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
          GenericC.stm [C.cstm|$id:v = gl_WorkGroupID[$int:i];|]
        shaderOps (GetLocalId v i) =
          GenericC.stm [C.cstm|$id:v = gl_LocalInvocationID[$int:i];|]
        shaderOps (GetLocalSize v i) =
          GenericC.stm [C.cstm|$id:v = gl_LocalGroupSizeARB[$int:i];|]
        shaderOps (GetGlobalId v i) =
          GenericC.stm [C.cstm|$id:v = gl_GlobalInvocationID[$int:i];|]
        shaderOps (GetGlobalSize v i) =
          GenericC.stm [C.cstm|$id:v = gl_NumWorkGroups[$int:i];|]
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

-- Checking requirements

typesInShader :: Kernel -> S.Set PrimType
typesInShader shader = typesInCode $ kernelBody shader

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
