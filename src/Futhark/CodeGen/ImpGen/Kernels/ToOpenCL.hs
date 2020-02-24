{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
-- | This module defines a translation from imperative code with
-- kernels to imperative code with OpenCL calls.
module Futhark.CodeGen.ImpGen.Kernels.ToOpenCL
  ( kernelsToOpenCL
  , kernelsToCUDA
  )
  where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Quote.CUDA as CUDAC

import Futhark.Error
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.CodeGen.Backends.SimpleRepresentation
import Futhark.CodeGen.ImpCode.Kernels hiding (Program)
import qualified Futhark.CodeGen.ImpCode.Kernels as ImpKernels
import Futhark.CodeGen.ImpCode.OpenCL hiding (Program)
import qualified Futhark.CodeGen.ImpCode.OpenCL as ImpOpenCL
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeString)

kernelsToCUDA, kernelsToOpenCL :: ImpKernels.Program
                               -> Either InternalError ImpOpenCL.Program
kernelsToCUDA = translateKernels TargetCUDA
kernelsToOpenCL = translateKernels TargetOpenCL

-- | Translate a kernels-program to an OpenCL-program.
translateKernels :: KernelTarget
                 -> ImpKernels.Program
                 -> Either InternalError ImpOpenCL.Program
translateKernels target (ImpKernels.Functions funs) = do
  (prog', ToOpenCL kernels used_types sizes failures) <-
    flip runStateT initialOpenCL $ fmap Functions $ forM funs $ \(fname, fun) ->
    (fname,) <$> runReaderT (traverse (onHostOp target) fun) fname
  let kernels' = M.map fst kernels
      opencl_code = openClCode $ map snd $ M.elems kernels
      opencl_prelude = pretty $ genPrelude target used_types
  return $ ImpOpenCL.Program opencl_code opencl_prelude kernels'
    (S.toList used_types) (cleanSizes sizes) failures prog'
  where genPrelude TargetOpenCL = genOpenClPrelude
        genPrelude TargetCUDA = const genCUDAPrelude

-- | Due to simplifications after kernel extraction, some threshold
-- parameters may contain KernelPaths that reference threshold
-- parameters that no longer exist.  We remove these here.
cleanSizes :: M.Map Name SizeClass -> M.Map Name SizeClass
cleanSizes m = M.map clean m
  where known = M.keys m
        clean (SizeThreshold path) =
          SizeThreshold $ filter ((`elem` known) . fst) path
        clean s = s

pointerQuals ::  Monad m => String -> m [C.TypeQual]
pointerQuals "global"     = return [C.ctyquals|__global|]
pointerQuals "local"      = return [C.ctyquals|__local|]
pointerQuals "private"    = return [C.ctyquals|__private|]
pointerQuals "constant"   = return [C.ctyquals|__constant|]
pointerQuals "write_only" = return [C.ctyquals|__write_only|]
pointerQuals "read_only"  = return [C.ctyquals|__read_only|]
pointerQuals "kernel"     = return [C.ctyquals|__kernel|]
pointerQuals s            = error $ "'" ++ s ++ "' is not an OpenCL kernel address space."

-- In-kernel name and per-workgroup size in bytes.
type LocalMemoryUse = (VName, Count Bytes Exp)

data KernelState =
  KernelState { kernelLocalMemory :: [LocalMemoryUse]
              , kernelFailures :: [FailureMsg]
              , kernelNextSync :: Int
              , kernelSyncPending :: Bool
                -- ^ Has a potential failure occurred sine the last
                -- ErrorSync?
              , kernelHasBarriers :: Bool
              }

newKernelState :: [FailureMsg] -> KernelState
newKernelState failures = KernelState mempty failures 0 False False

errorLabel :: KernelState -> String
errorLabel = ("error_"++) . show . kernelNextSync

data ToOpenCL = ToOpenCL { clKernels :: M.Map KernelName (Safety, C.Func)
                         , clUsedTypes :: S.Set PrimType
                         , clSizes :: M.Map Name SizeClass
                         , clFailures :: [FailureMsg]
                         }

initialOpenCL :: ToOpenCL
initialOpenCL = ToOpenCL mempty mempty mempty mempty

type OnKernelM = ReaderT Name (StateT ToOpenCL (Either InternalError))

addSize :: Name -> SizeClass -> OnKernelM ()
addSize key sclass =
  modify $ \s -> s { clSizes = M.insert key sclass $ clSizes s }

onHostOp :: KernelTarget -> HostOp -> OnKernelM OpenCL
onHostOp target (CallKernel k) = onKernel target k
onHostOp _ (ImpKernels.GetSize v key size_class) = do
  addSize key size_class
  return $ ImpOpenCL.GetSize v key
onHostOp _ (ImpKernels.CmpSizeLe v key size_class x) = do
  addSize key size_class
  return $ ImpOpenCL.CmpSizeLe v key x
onHostOp _ (ImpKernels.GetSizeMax v size_class) =
  return $ ImpOpenCL.GetSizeMax v size_class

onKernel :: KernelTarget -> Kernel -> OnKernelM OpenCL

onKernel target kernel = do
  failures <- gets clFailures
  let (kernel_body, cstate) =
        GenericC.runCompilerM mempty (inKernelOperations (kernelBody kernel))
        blankNameSource
        (newKernelState failures) $
        GenericC.blockScope $ GenericC.compileCode $ kernelBody kernel
      kstate = GenericC.compUserState cstate

      use_params = mapMaybe useAsParam $ kernelUses kernel

      (local_memory_args, local_memory_params, local_memory_init) =
        unzip3 $
        flip evalState (blankNameSource :: VNameSource) $
        mapM (prepareLocalMemory target) $ kernelLocalMemory kstate

      -- CUDA has very strict restrictions on the number of blocks
      -- permitted along the 'y' and 'z' dimensions of the grid
      -- (1<<16).  To work around this, we are going to dynamically
      -- permute the block dimensions to move the largest one to the
      -- 'x' dimension, which has a higher limit (1<<31).  This means
      -- we need to extend the kernel with extra parameters that
      -- contain information about this permutation, but we only do
      -- this for multidimensional kernels (at the time of this
      -- writing, only transposes).  The corresponding arguments are
      -- added automatically in CCUDA.hs.
      (perm_params, block_dim_init) =
        case (target, num_groups) of
          (TargetCUDA, [_, _, _]) -> ([[C.cparam|const int block_dim0|],
                                       [C.cparam|const int block_dim1|],
                                       [C.cparam|const int block_dim2|]],
                                      mempty)
          _ -> (mempty,
                [[C.citem|const int block_dim0 = 0;|],
                 [C.citem|const int block_dim1 = 1;|],
                 [C.citem|const int block_dim2 = 2;|]])

      const_defs = mapMaybe constDef $ kernelUses kernel

  let (safety, error_init)
        | length (kernelFailures kstate) == length failures =
            if kernelFailureTolerant kernel
            then (SafetyNone, [])
            else -- No possible failures in this kernel, so if we make
                 -- it past an initial check, then we are good to go.
                 (SafetyCheap,
                  [C.citems|if (*global_failure >= 0) { return; }|])

        | otherwise =
            if not (kernelHasBarriers kstate)
            then (SafetyFull,
                  [C.citems|if (*global_failure >= 0) { return; }|])
            else (SafetyFull,
                  [C.citems|
                     volatile __local bool local_failure;
                     if (failure_is_an_option) {
                       if (get_local_id(0) == 0) {
                         local_failure = *global_failure >= 0;
                       }
                       barrier(CLK_LOCAL_MEM_FENCE);
                       if (local_failure) { return; }
                     } else {
                       local_failure = false;
                     }
                     barrier(CLK_LOCAL_MEM_FENCE);
                  |])

      failure_params =
        [[C.cparam|__global int *global_failure|],
         [C.cparam|int failure_is_an_option|],
         [C.cparam|__global int *global_failure_args|]]

      params = perm_params ++
               take (numFailureParams safety) failure_params ++
               catMaybes local_memory_params ++
               use_params

      kernel_fun =
        [C.cfun|__kernel void $id:name ($params:params) {
                  $items:const_defs
                  $items:block_dim_init
                  $items:local_memory_init
                  $items:error_init
                  $items:kernel_body

                  $id:(errorLabel kstate): return;
                }|]
  modify $ \s -> s
    { clKernels = M.insert name (safety, kernel_fun) $ clKernels s
    , clUsedTypes = typesInKernel kernel <> clUsedTypes s
    , clFailures = kernelFailures kstate
    }

  -- The argument corresponding to the global_failure parameters is
  -- added automatically later.
  let args = catMaybes local_memory_args ++
             kernelArgs kernel

  return $ LaunchKernel safety name args num_groups group_size
  where name = nameToString $ kernelName kernel
        num_groups = kernelNumGroups kernel
        group_size = kernelGroupSize kernel

        prepareLocalMemory TargetOpenCL (mem, size) = do
          mem_aligned <- newVName $ baseString mem ++ "_aligned"
          return (Just $ SharedMemoryKArg size,
                  Just [C.cparam|__local volatile typename int64_t* $id:mem_aligned|],
                  [C.citem|__local volatile char* restrict $id:mem = (__local volatile char*)$id:mem_aligned;|])
        prepareLocalMemory TargetCUDA (mem, size) = do
          param <- newVName $ baseString mem ++ "_offset"
          return (Just $ SharedMemoryKArg size,
                  Just [C.cparam|uint $id:param|],
                  [C.citem|volatile char *$id:mem = &shared_mem[$id:param];|])

useAsParam :: KernelUse -> Maybe C.Param
useAsParam (ScalarUse name bt) =
  let ctp = case bt of
        -- OpenCL does not permit bool as a kernel parameter type.
        Bool -> [C.cty|unsigned char|]
        _    -> GenericC.primTypeToCType bt
  in Just [C.cparam|$ty:ctp $id:name|]
useAsParam (MemoryUse name) =
  Just [C.cparam|__global unsigned char *$id:name|]
useAsParam ConstUse{} =
  Nothing

constDef :: KernelUse -> Maybe C.BlockItem
constDef (ConstUse v e) = Just [C.citem|const $ty:t $id:v = $exp:e';|]
  where t = GenericC.primTypeToCType $ primExpType e
        e' = compilePrimExp e
constDef _ = Nothing

openClCode :: [C.Func] -> String
openClCode kernels =
  pretty [C.cunit|$edecls:funcs|]
  where funcs =
          [[C.cedecl|$func:kernel_func|] |
           kernel_func <- kernels ]

genOpenClPrelude :: S.Set PrimType -> [C.Definition]
genOpenClPrelude ts =
  -- Clang-based OpenCL implementations need this for 'static' to work.
  [ [C.cedecl|$esc:("#ifdef cl_clang_storage_class_specifiers")|]
  , [C.cedecl|$esc:("#pragma OPENCL EXTENSION cl_clang_storage_class_specifiers : enable")|]
  , [C.cedecl|$esc:("#endif")|]
  , [C.cedecl|$esc:("#pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable")|]]
  ++
  [[C.cedecl|$esc:("#pragma OPENCL EXTENSION cl_khr_fp64 : enable")|] | uses_float64] ++
  [C.cunit|
/* Some OpenCL programs dislike empty progams, or programs with no kernels.
 * Declare a dummy kernel to ensure they remain our friends. */
__kernel void dummy_kernel(__global unsigned char *dummy, int n)
{
    const int thread_gid = get_global_id(0);
    if (thread_gid >= n) return;
}

typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long int64_t;

typedef uchar uint8_t;
typedef ushort uint16_t;
typedef uint uint32_t;
typedef ulong uint64_t;

// NVIDIAs OpenCL does not create device-wide memory fences (see #734), so we
// use inline assembly if we detect we are on an NVIDIA GPU.
$esc:("#ifdef cl_nv_pragma_unroll")
static inline void mem_fence_global() {
  asm("membar.gl;");
}
$esc:("#else")
static inline void mem_fence_global() {
  mem_fence(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
}
$esc:("#endif")
static inline void mem_fence_local() {
  mem_fence(CLK_LOCAL_MEM_FENCE);
}
|] ++
  cIntOps ++ cFloat32Ops ++ cFloat32Funs ++
  (if uses_float64 then cFloat64Ops ++ cFloat64Funs ++ cFloatConvOps else [])
  where uses_float64 = FloatType Float64 `S.member` ts


cudaAtomicOps :: [C.Definition]
cudaAtomicOps = (mkOp <$> opNames <*> types) ++ extraOps
  where
    mkOp (clName, cuName) t =
      [C.cedecl|static inline $ty:t $id:clName(volatile $ty:t *p, $ty:t val) {
                 return $id:cuName(($ty:t *)p, val);
               }|]
    types = [ [C.cty|int|]
            , [C.cty|unsigned int|]
            , [C.cty|unsigned long long|]
            ]
    opNames = [ ("atomic_add",  "atomicAdd")
              , ("atomic_max",  "atomicMax")
              , ("atomic_min",  "atomicMin")
              , ("atomic_and",  "atomicAnd")
              , ("atomic_or",   "atomicOr")
              , ("atomic_xor",  "atomicXor")
              , ("atomic_xchg", "atomicExch")
              ]
    extraOps =
      [ [C.cedecl|static inline $ty:t atomic_cmpxchg(volatile $ty:t *p, $ty:t cmp, $ty:t val) {
                  return atomicCAS(($ty:t *)p, cmp, val);
                }|] | t <- types]

genCUDAPrelude :: [C.Definition]
genCUDAPrelude =
  cudafy ++ cudaAtomicOps ++ ops
  where ops = cIntOps ++ cFloat32Ops ++ cFloat32Funs ++ cFloat64Ops
                ++ cFloat64Funs ++ cFloatConvOps
        cudafy = [CUDAC.cunit|
typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int64_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
typedef uint8_t uchar;
typedef uint16_t ushort;
typedef uint32_t uint;
typedef uint64_t ulong;
$esc:("#define __kernel extern \"C\" __global__ __launch_bounds__(MAX_THREADS_PER_BLOCK)")
$esc:("#define __global")
$esc:("#define __local")
$esc:("#define __private")
$esc:("#define __constant")
$esc:("#define __write_only")
$esc:("#define __read_only")

static inline int get_group_id_fn(int block_dim0, int block_dim1, int block_dim2, int d)
{
  switch (d) {
    case 0: d = block_dim0; break;
    case 1: d = block_dim1; break;
    case 2: d = block_dim2; break;
  }
  switch (d) {
    case 0: return blockIdx.x;
    case 1: return blockIdx.y;
    case 2: return blockIdx.z;
    default: return 0;
  }
}
$esc:("#define get_group_id(d) get_group_id_fn(block_dim0, block_dim1, block_dim2, d)")

static inline int get_num_groups_fn(int block_dim0, int block_dim1, int block_dim2, int d)
{
  switch (d) {
    case 0: d = block_dim0; break;
    case 1: d = block_dim1; break;
    case 2: d = block_dim2; break;
  }
  switch(d) {
    case 0: return gridDim.x;
    case 1: return gridDim.y;
    case 2: return gridDim.z;
    default: return 0;
  }
}
$esc:("#define get_num_groups(d) get_num_groups_fn(block_dim0, block_dim1, block_dim2, d)")

static inline int get_local_id(int d)
{
  switch (d) {
    case 0: return threadIdx.x;
    case 1: return threadIdx.y;
    case 2: return threadIdx.z;
    default: return 0;
  }
}

static inline int get_local_size(int d)
{
  switch (d) {
    case 0: return blockDim.x;
    case 1: return blockDim.y;
    case 2: return blockDim.z;
    default: return 0;
  }
}

static inline int get_global_id_fn(int block_dim0, int block_dim1, int block_dim2, int d)
{
  return get_group_id(d) * get_local_size(d) + get_local_id(d);
}
$esc:("#define get_global_id(d) get_global_id_fn(block_dim0, block_dim1, block_dim2, d)")

static inline int get_global_size(int block_dim0, int block_dim1, int block_dim2, int d)
{
  return get_num_groups(d) * get_local_size(d);
}

$esc:("#define CLK_LOCAL_MEM_FENCE 1")
$esc:("#define CLK_GLOBAL_MEM_FENCE 2")
static inline void barrier(int x)
{
  __syncthreads();
}
static inline void mem_fence_local() {
  __threadfence_block();
}
static inline void mem_fence_global() {
  __threadfence();
}
$esc:("#define NAN (0.0/0.0)")
$esc:("#define INFINITY (1.0/0.0)")
extern volatile __shared__ char shared_mem[];
|]

compilePrimExp :: PrimExp KernelConst -> C.Exp
compilePrimExp e = runIdentity $ GenericC.compilePrimExp compileKernelConst e
  where compileKernelConst (SizeConst key) =
          return [C.cexp|$id:(zEncodeString (pretty key))|]

kernelArgs :: Kernel -> [KernelArg]
kernelArgs = mapMaybe useToArg . kernelUses
  where useToArg (MemoryUse mem)  = Just $ MemKArg mem
        useToArg (ScalarUse v bt) = Just $ ValueKArg (LeafExp (ScalarVar v) bt) bt
        useToArg ConstUse{}       = Nothing

nextErrorLabel :: GenericC.CompilerM KernelOp KernelState String
nextErrorLabel =
  errorLabel <$> GenericC.getUserState

incErrorLabel :: GenericC.CompilerM KernelOp KernelState ()
incErrorLabel =
  GenericC.modifyUserState $ \s -> s { kernelNextSync = kernelNextSync s + 1 }

pendingError :: Bool -> GenericC.CompilerM KernelOp KernelState ()
pendingError b =
  GenericC.modifyUserState $ \s -> s { kernelSyncPending = b }

hasCommunication :: ImpKernels.KernelCode -> Bool
hasCommunication = any communicates
  where communicates ErrorSync = True
        communicates LocalBarrier = True
        communicates GlobalBarrier = True
        communicates _ = False

inKernelOperations :: ImpKernels.KernelCode -> GenericC.Operations KernelOp KernelState
inKernelOperations body =
  GenericC.Operations
  { GenericC.opsCompiler = kernelOps
  , GenericC.opsMemoryType = kernelMemoryType
  , GenericC.opsWriteScalar = kernelWriteScalar
  , GenericC.opsReadScalar = kernelReadScalar
  , GenericC.opsAllocate = cannotAllocate
  , GenericC.opsDeallocate = cannotDeallocate
  , GenericC.opsCopy = copyInKernel
  , GenericC.opsStaticArray = noStaticArrays
  , GenericC.opsFatMemory = False
  , GenericC.opsError = errorInKernel
  }
  where has_communication = hasCommunication body

        kernelOps :: GenericC.OpCompiler KernelOp KernelState
        kernelOps (GetGroupId v i) =
          GenericC.stm [C.cstm|$id:v = get_group_id($int:i);|]
        kernelOps (GetLocalId v i) =
          GenericC.stm [C.cstm|$id:v = get_local_id($int:i);|]
        kernelOps (GetLocalSize v i) =
          GenericC.stm [C.cstm|$id:v = get_local_size($int:i);|]
        kernelOps (GetGlobalId v i) =
          GenericC.stm [C.cstm|$id:v = get_global_id($int:i);|]
        kernelOps (GetGlobalSize v i) =
          GenericC.stm [C.cstm|$id:v = get_global_size($int:i);|]
        kernelOps (GetLockstepWidth v) =
          GenericC.stm [C.cstm|$id:v = LOCKSTEP_WIDTH;|]
        kernelOps LocalBarrier = do
          GenericC.stm [C.cstm|barrier(CLK_LOCAL_MEM_FENCE);|]
          GenericC.modifyUserState $ \s -> s { kernelHasBarriers = True }
        kernelOps GlobalBarrier = do
          GenericC.stm [C.cstm|barrier(CLK_GLOBAL_MEM_FENCE);|]
          GenericC.modifyUserState $ \s -> s { kernelHasBarriers = True }
        kernelOps MemFenceLocal =
          GenericC.stm [C.cstm|mem_fence_local();|]
        kernelOps MemFenceGlobal =
          GenericC.stm [C.cstm|mem_fence_global();|]
        kernelOps (PrivateAlloc name size) = do
          size' <- GenericC.compileExp $ unCount size
          name' <- newVName $ pretty name ++ "_backing"
          GenericC.item [C.citem|__private char $id:name'[$exp:size'];|]
          GenericC.stm [C.cstm|$id:name = $id:name';|]
        kernelOps (LocalAlloc name size) = do
          name' <- newVName $ pretty name ++ "_backing"
          GenericC.modifyUserState $ \s ->
            s { kernelLocalMemory = (name', size) : kernelLocalMemory s }
          GenericC.stm [C.cstm|$id:name = (__local char*) $id:name';|]
        kernelOps ErrorSync = do
          label <- nextErrorLabel
          pending <- kernelSyncPending <$> GenericC.getUserState
          when pending $ do
            pendingError False
            GenericC.stm [C.cstm|$id:label: barrier(CLK_LOCAL_MEM_FENCE);|]
            GenericC.stm [C.cstm|if (local_failure) { return; }|]
          GenericC.stm [C.cstm|barrier(CLK_LOCAL_MEM_FENCE);|]
          GenericC.modifyUserState $ \s -> s { kernelHasBarriers = True }
          incErrorLabel
        kernelOps (Atomic space aop) = atomicOps space aop

        atomicCast s t = do
          let volatile = [C.ctyquals|volatile|]
          quals <- case s of Space sid    -> pointerQuals sid
                             _            -> pointerQuals "global"
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
          doAtomic s old arr ind val "atomic_max" [C.cty|unsigned int|]

        atomicOps s (AtomicUMin old arr ind val) =
          doAtomic s old arr ind val "atomic_min" [C.cty|unsigned int|]

        atomicOps s (AtomicAnd old arr ind val) =
          doAtomic s old arr ind val "atomic_and" [C.cty|unsigned int|]

        atomicOps s (AtomicOr old arr ind val) =
          doAtomic s old arr ind val "atomic_or" [C.cty|unsigned int|]

        atomicOps s (AtomicXor old arr ind val) =
          doAtomic s old arr ind val "atomic_xor" [C.cty|unsigned int|]

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

        cannotAllocate :: GenericC.Allocate KernelOp KernelState
        cannotAllocate _ =
          error "Cannot allocate memory in kernel"

        cannotDeallocate :: GenericC.Deallocate KernelOp KernelState
        cannotDeallocate _ _ =
          error "Cannot deallocate memory in kernel"

        copyInKernel :: GenericC.Copy KernelOp KernelState
        copyInKernel _ _ _ _ _ _ _ =
          error "Cannot bulk copy in kernel."

        noStaticArrays :: GenericC.StaticArray KernelOp KernelState
        noStaticArrays _ _ _ _ =
          error "Cannot create static array in kernel."

        kernelMemoryType space = do
          quals <- pointerQuals space
          return [C.cty|$tyquals:quals $ty:defaultMemBlockType|]

        kernelWriteScalar =
          GenericC.writeScalarPointerWithQuals pointerQuals

        kernelReadScalar =
          GenericC.readScalarPointerWithQuals pointerQuals

        errorInKernel msg@(ErrorMsg parts) backtrace = do
          n <- length . kernelFailures <$> GenericC.getUserState
          GenericC.modifyUserState $ \s ->
            s { kernelFailures = kernelFailures s ++ [FailureMsg msg backtrace] }
          let setArgs _ [] = return []
              setArgs i (ErrorString{} : parts') = setArgs i parts'
              setArgs i (ErrorInt32 x : parts') = do
                x' <- GenericC.compileExp x
                stms <- setArgs (i+1) parts'
                return $ [C.cstm|global_failure_args[$int:i] = $exp:x';|] : stms
          argstms <- setArgs (0::Int) parts
          label <- nextErrorLabel
          pendingError True
          let what_next
                | has_communication = [C.citems|local_failure = true;
                                                goto $id:label;|]
                | otherwise         = [C.citems|return;|]
          GenericC.stm [C.cstm|{ if (atomic_cmpxchg(global_failure, -1, $int:n) == -1)
                                 { $stms:argstms; }
                                 $items:what_next
                               }|]

--- Checking requirements

typesInKernel :: Kernel -> S.Set PrimType
typesInKernel kernel = typesInCode $ kernelBody kernel

typesInCode :: ImpKernels.KernelCode -> S.Set PrimType
typesInCode Skip = mempty
typesInCode (c1 :>>: c2) = typesInCode c1 <> typesInCode c2
typesInCode (For _ it e c) = IntType it `S.insert` typesInExp e <> typesInCode c
typesInCode (While e c) = typesInExp e <> typesInCode c
typesInCode DeclareMem{} = mempty
typesInCode (DeclareScalar _ _ t) = S.singleton t
typesInCode (DeclareArray _ _ t _) = S.singleton t
typesInCode (Allocate _ (Count e) _) = typesInExp e
typesInCode Free{} = mempty
typesInCode (Copy _ (Count e1) _ _ (Count e2) _ (Count e3)) =
  typesInExp e1 <> typesInExp e2 <> typesInExp e3
typesInCode (Write _ (Count e1) t _ _ e2) =
  typesInExp e1 <> S.singleton t <> typesInExp e2
typesInCode (SetScalar _ e) = typesInExp e
typesInCode SetMem{} = mempty
typesInCode (Call _ _ es) = mconcat $ map typesInArg es
  where typesInArg MemArg{} = mempty
        typesInArg (ExpArg e) = typesInExp e
typesInCode (If e c1 c2) =
  typesInExp e <> typesInCode c1 <> typesInCode c2
typesInCode (Assert e _ _) = typesInExp e
typesInCode (Comment _ c) = typesInCode c
typesInCode (DebugPrint _ v) = maybe mempty typesInExp v
typesInCode Op{} = mempty

typesInExp :: Exp -> S.Set PrimType
typesInExp (ValueExp v) = S.singleton $ primValueType v
typesInExp (BinOpExp _ e1 e2) = typesInExp e1 <> typesInExp e2
typesInExp (CmpOpExp _ e1 e2) = typesInExp e1 <> typesInExp e2
typesInExp (ConvOpExp op e) = S.fromList [from, to] <> typesInExp e
  where (from, to) = convOpType op
typesInExp (UnOpExp _ e) = typesInExp e
typesInExp (FunExp _ args t) = S.singleton t <> mconcat (map typesInExp args)
typesInExp (LeafExp (Index _ (Count e) t _ _) _) = S.singleton t <> typesInExp e
typesInExp (LeafExp ScalarVar{} _) = mempty
typesInExp (LeafExp (SizeOf t) _) = S.singleton t
