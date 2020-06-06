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

data ParamUse = AScalarUse
              | AMemoryUse
              | AConstUse

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

data ToOpenGL = ToOpenGL { glShaders   :: M.Map ShaderName (Safety, [[C.Definition]])
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
        GenericC.blockScope $ GenericC.compileCode GenericC.TargetShader
                                                 $ kernelBody shader
      s_state = GenericC.compUserState cstate

      vname_sizes =
        S.toList $ S.fromList $ catMaybes $ analyzeSizes $ kernelBody shader

      (use_params', uses') =
        unzip $ mapMaybe (useAsParam vname_sizes) $ kernelUses shader

      --FIXME: `local_memory_params` might be used as global arrays.
      (local_memory_args, _, local_memory_init) =
        unzip3 $
        flip evalState (blankNameSource :: VNameSource) $
        mapM (prepareLocalMemory vname_sizes) $ shaderLocalMemory s_state

      (perm_params, block_dim_init) =
        (mempty,
         [[C.citem|const typename int32_t block_dim0 = 0;|],
          [C.citem|const typename int32_t block_dim1 = 1;|],
          [C.citem|const typename int32_t block_dim2 = 2;|]]
        )

      const_defs = mapMaybe constDef $ kernelUses shader

      -- We do not account for safety within shaders.
      safety = SafetyNone

      --FIXME: These might be used as global arrays.
      params = perm_params -- ++ catMaybes local_memory_params ++ use_params

      (cat_use_params, cat_uses) =
        unzip $ zip local_memory_init $ replicate (length local_memory_init)
                                                  AMemoryUse

      (use_params, uses) = (cat_use_params ++ use_params', cat_uses ++ uses')

      layoutQuals = concat $
                    map (\(i, k, u) -> case u of
                                       AScalarUse ->
                                         "layout(location = " ++ show i ++
                                         ") uniform " ++ pretty k ++ "\n"
                                       AMemoryUse ->
                                         "layout(std430, binding = " ++
                                         show i ++ ") buffer SSBO" ++ show i
                                         ++ "\n{\n  "  ++ pretty k ++ "\n};\n"
                                       _ ->
                                         ""
                        ) $ zip3 [(0::Int)..] use_params uses

      cLayoutQuals = [C.cunit|$esc:(layoutQuals)|]

      shader_fun =
        [C.cunit|void $id:name ($params:params) {
                  $items:const_defs
                  $items:block_dim_init
                  $items:shader_body
                }|]

      shader_main =
        [C.cunit|void main (void) {
                 $id:name();
                }|]

      shader_code = cLayoutQuals ++ shader_fun ++ shader_main

  modify $ \s -> s
    { glShaders   =
           M.insert name (safety, [shader_code]) $ glShaders s
    , glUsedTypes = typesInShader shader <> glUsedTypes s
    }

  let args = catMaybes local_memory_args ++
             kernelArgs shader

  return $ LaunchShader safety name args num_groups group_size
  where name = nameToString $ kernelName shader
        num_groups = kernelNumGroups shader
        group_size = kernelGroupSize shader

        prepareLocalMemory vsizes (mem, size) = do
          let vsize = lookup mem vsizes
          let stm = "shared int32_t " ++ pretty mem ++ "[];"
          param <- newVName $ baseString mem ++ "_offset"
          return (Just $ SharedMemoryKArg size,
                  Just [C.cparam|typename uint32_t $id:param|],
                  -- FIXME: Make the array type generic
                  [C.citem|$escstm:(stm)|])

useAsParam :: [(VName, Int)]
           -> KernelUse
           -> Maybe (C.BlockItem, ParamUse)
useAsParam _ (ScalarUse name bt) =
  let ctp = case bt of
        Bool -> [C.cty|bool|]
        _    -> GenericC.primTypeToCType bt
  in Just ([C.citem|$ty:ctp $id:name;|], AScalarUse)
useAsParam sizes (MemoryUse name) =
-- FIXME: Make the array type generic
  Just ([C.citem|typename int32_t $id:name[];|], AMemoryUse)
useAsParam _ ConstUse{} =
  Nothing

compilePrimExp :: PrimExp KernelConst -> C.Exp
compilePrimExp e = runIdentity $ GenericC.compilePrimExp
                                   GenericC.TargetShader compileKernelConst e
  where compileKernelConst (SizeConst key) =
          return [C.cexp|$id:(zEncodeString (pretty key))|]

constDef :: KernelUse -> Maybe C.BlockItem
constDef (ConstUse v e) = Just [C.citem|$escstm:def|]
  where e' = compilePrimExp e
        def = "const int " ++ pretty (C.toIdent v mempty)
                           ++ " = " ++ pretty e' ++ ";"
constDef _ = Nothing

openGlCode :: [[[C.Definition]]] -> [String]
openGlCode shaders = map pretty (concat shaders)

genOpenGlPrelude :: S.Set PrimType -> [C.Definition]
genOpenGlPrelude ts =
  [ [C.cedecl|$esc:("#version 450")|]
  , [C.cedecl|$esc:("#extension GL_ARB_gpu_shader_int64 : enable")|]
  , [C.cedecl|$esc:("#extension GL_ARB_gpu_shader_fp64 : enable")|]
  , [C.cedecl|$esc:("layout(local_size_x = 256, local_size_y = 1, local_size_z = 1) in;")|]
  , [C.cedecl|$esc:("#define int8_t int")|]
  , [C.cedecl|$esc:("#define int16_t int")|]
  , [C.cedecl|$esc:("#define int32_t int")|]
  , [C.cedecl|$esc:("#define uint8_t uint")|]
  , [C.cedecl|$esc:("#define uint16_t uint")|]
  , [C.cedecl|$esc:("#define uint32_t uint")|]
  , [C.cedecl|$esc:("#define float32 float")|]
  , [C.cedecl|$esc:("#define float64 double")|]
  , [C.cedecl|$esc:("#define boolean bool")|]
  ] ++ glIntOps  ++ glFloat32Ops  ++ glFloat32Funs ++
    (if uses_float64 then glFloat64Ops ++ glFloat64Funs ++ glFloatConvOps
     else [])
    where uses_float64 = FloatType Float64 `S.member` ts

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
  , GenericC.opsError       = errorInShader
  }
  where has_communication = hasCommunication body

        shaderOps :: GenericC.OpCompiler KernelOp ShaderState
        shaderOps (GetGroupId v i) =
          GenericC.stm [C.cstm|$id:v = int32_t(gl_WorkGroupID[$int:i]);|]
        shaderOps (GetLocalId v i) =
          GenericC.stm [C.cstm|$id:v = int32_t(gl_LocalInvocationID[$int:i]);|]
        shaderOps (GetLocalSize v i) =
          GenericC.stm [C.cstm|$id:v = int32_t(gl_WorkGroupSize[0]);|]
        shaderOps (GetGlobalId v i) =
          GenericC.stm [C.cstm|$id:v = int32_t(gl_GlobalInvocationID[$int:i]);|]
        shaderOps (GetGlobalSize v i) =
          GenericC.stm [C.cstm|$id:v = int32_t(gl_NumWorkGroups[$int:i]);|]
        shaderOps (GetLockstepWidth v) =
          -- FIXME: Should vary between vendor. 64 for AMD and 32 for Nvidia,
          --        otherwise 1.
          GenericC.stm [C.cstm|$id:v = 1;|]
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

        atomicCast s t = return [C.cty|$ty:t|]

        doAtomic s old arr ind val op ty = do
          ind' <- GenericC.compileExp GenericC.TargetShader $ unCount ind
          val' <- GenericC.compileExp GenericC.TargetShader val
          cast <- atomicCast s ty
          GenericC.stm [C.cstm|$id:old = $id:op($esc:(pretty cast)
                                                ($id:arr[$exp:ind']),
                                                $esc:(pretty ty)($exp:val'));|]

        atomicOps s (AtomicAdd old arr ind val) =
          doAtomic s old arr ind val "atomicAdd" [C.cty|typename int32_t|]

        atomicOps s (AtomicSMax old arr ind val) =
          doAtomic s old arr ind val "atomicMax" [C.cty|typename int32_t|]

        atomicOps s (AtomicSMin old arr ind val) =
          doAtomic s old arr ind val "atomicMin" [C.cty|typename int32_t|]

        atomicOps s (AtomicUMax old arr ind val) =
          doAtomic s old arr ind val "atomicMax" [C.cty|typename uint32_t|]

        atomicOps s (AtomicUMin old arr ind val) =
          doAtomic s old arr ind val "atomicMin" [C.cty|typename uint32_t|]

        atomicOps s (AtomicAnd old arr ind val) =
          doAtomic s old arr ind val "atomicAnd" [C.cty|typename uint32_t|]

        atomicOps s (AtomicOr old arr ind val) =
          doAtomic s old arr ind val "atomicOr" [C.cty|typename uint32_t|]

        atomicOps s (AtomicXor old arr ind val) =
          doAtomic s old arr ind val "atomicXor" [C.cty|typename uint32_t|]

        atomicOps s (AtomicCmpXchg old arr ind cmp val) = do
          ind' <- GenericC.compileExp GenericC.TargetShader $ unCount ind
          cmp' <- GenericC.compileExp GenericC.TargetShader cmp
          val' <- GenericC.compileExp GenericC.TargetShader val
          cast <- atomicCast s [C.cty|int|]
          GenericC.stm [C.cstm|$id:old = atomicCompSwap($esc:(pretty cast)
                                                        ($id:arr[$exp:ind']),
                                                        $exp:cmp', $exp:val');|]

        atomicOps s (AtomicXchg old arr ind val) = do
          ind' <- GenericC.compileExp GenericC.TargetShader $ unCount ind
          val' <- GenericC.compileExp GenericC.TargetShader val
          cast <- atomicCast s [C.cty|int|]
          GenericC.stm [C.cstm|$id:old = atomicExchange($esc:(pretty cast)
                                                        ($id:arr[$exp:ind']),
                                                        $exp:val');|]

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
          return [C.cty|$ty:defaultMemBlockTypeGLSL|]

        shaderWriteScalar =
          GenericC.writeScalarShader

        shaderReadScalar =
          GenericC.readScalarShader

        errorInShader msg@(ErrorMsg parts) backtrace = do
          return ()

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

-- -- | The size of a value of a given floating-point type in bits.
floatbitSize :: Num a => FloatType -> a
floatbitSize Float32 = 32
floatbitSize Float64 = 64

-- -- | The size of a value of a given primitive type in eight-bit bytes for
--      `Int` types and in bits for `float` types. This ensures no overlapping
--      values during the analysis of sizes phase.
primSize :: Num a => PrimType -> a
primSize (IntType t)   = intByteSize t
primSize (FloatType t) = floatbitSize t
primSize Bool          = 1
primSize Cert          = 1

atomicOpExpr :: AtomicOp -> (VName, Exp, Maybe Exp)
atomicOpExpr (AtomicAdd _ rname _ e)  = (rname, e, Nothing)
atomicOpExpr (AtomicSMax _ rname _ e) = (rname, e, Nothing)
atomicOpExpr (AtomicSMin _ rname _ e) = (rname, e, Nothing)
atomicOpExpr (AtomicUMax _ rname _ e) = (rname, e, Nothing)
atomicOpExpr (AtomicUMin _ rname _ e) = (rname, e, Nothing)
atomicOpExpr (AtomicAnd _ rname _ e)  = (rname, e, Nothing)
atomicOpExpr (AtomicOr _ rname _ e)   = (rname, e, Nothing)
atomicOpExpr (AtomicXor _ rname _ e)  = (rname, e, Nothing)
atomicOpExpr (AtomicCmpXchg _ rname _ le re) = (rname, le, Just re)
atomicOpExpr (AtomicXchg _ rname _ e) = (rname, e, Nothing)

analyzeExprSizes :: Exp -> Maybe (VName, Int)
analyzeExprSizes (LeafExp (Index src _ restype _ _) _) =
  let size = primSize restype
  in Just (src, size)
analyzeExprSizes _ = Nothing

-- FIXME: Not all arrays are considered.
analyzeSizes :: ImpKernels.KernelCode -> [Maybe (VName, Int)]
analyzeSizes (Op (Atomic op s)) = do
  let (arr, expr, mexpr) = atomicOpExpr s
  case mexpr of
    Nothing -> [analyzeExprSizes expr]
    _       -> analyzeExprSizes expr : [analyzeExprSizes $ fromJust mexpr]
analyzeSizes (lc :>>: rc) = do
  analyzeSizes lc ++ analyzeSizes rc
analyzeSizes (Comment _ c) =
  analyzeSizes c
analyzeSizes (Copy _ (Count destoffset) _ _ (Count srcoffset) _ (Count size)) = do
  analyzeExprSizes destoffset : analyzeExprSizes srcoffset  : [analyzeExprSizes size]
analyzeSizes (Write dest (Count idx) elemtype _ _ elemexp) = do
  let size = primSize elemtype
  analyzeExprSizes elemexp : analyzeExprSizes idx : [Just (dest, size)]
analyzeSizes (DeclareArray name _ t _) =
  let size = primSize t
  in [Just (name, size)]
analyzeSizes (SetScalar _ src) =
  [analyzeExprSizes src]
analyzeSizes (If cond tbranch fbranch) = do
  analyzeExprSizes cond : (analyzeSizes tbranch ++ analyzeSizes fbranch)
analyzeSizes (While cond body) = do
  analyzeExprSizes cond : analyzeSizes body
analyzeSizes (For _ _ bound body) = do
  analyzeExprSizes bound : analyzeSizes body
analyzeSizes _ = [Nothing]
