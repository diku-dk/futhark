{-# LANGUAGE QuasiQuotes #-}

-- | This module defines a translation from imperative code with
-- kernels to imperative code with OpenCL or CUDA calls.
module Futhark.CodeGen.ImpGen.GPU.ToOpenCL
  ( kernelsToOpenCL,
    kernelsToCUDA,
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC.Fun qualified as GC
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.ImpCode.GPU hiding (Program)
import Futhark.CodeGen.ImpCode.GPU qualified as ImpGPU
import Futhark.CodeGen.ImpCode.OpenCL hiding (Program)
import Futhark.CodeGen.ImpCode.OpenCL qualified as ImpOpenCL
import Futhark.CodeGen.RTS.C (atomicsH, halfH)
import Futhark.Error (compilerLimitationS)
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeText)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C
import NeatInterpolation (untrimming)

-- | Generate CUDA host and device code.
kernelsToCUDA :: ImpGPU.Program -> ImpOpenCL.Program
kernelsToCUDA = translateGPU TargetCUDA

-- | Generate OpenCL host and device code.
kernelsToOpenCL :: ImpGPU.Program -> ImpOpenCL.Program
kernelsToOpenCL = translateGPU TargetOpenCL

-- | Translate a kernels-program to an OpenCL-program.
translateGPU ::
  KernelTarget ->
  ImpGPU.Program ->
  ImpOpenCL.Program
translateGPU target prog =
  let env = envFromProg prog
      ( prog',
        ToOpenCL kernels device_funs used_types sizes failures
        ) =
          (`runState` initialOpenCL) . (`runReaderT` env) $ do
            let ImpGPU.Definitions
                  types
                  (ImpGPU.Constants ps consts)
                  (ImpGPU.Functions funs) = prog
            consts' <- traverse (onHostOp target) consts
            funs' <- forM funs $ \(fname, fun) ->
              (fname,) <$> traverse (onHostOp target) fun

            pure $
              ImpOpenCL.Definitions
                types
                (ImpOpenCL.Constants ps consts')
                (ImpOpenCL.Functions funs')

      (device_prototypes, device_defs) = unzip $ M.elems device_funs
      kernels' = M.map fst kernels
      opencl_code = T.unlines $ map snd $ M.elems kernels

      opencl_prelude =
        T.unlines
          [ genPrelude target used_types,
            definitionsText device_prototypes,
            T.unlines device_defs
          ]
   in ImpOpenCL.Program
        opencl_code
        opencl_prelude
        kernels'
        (S.toList used_types)
        (findParamUsers env prog' (cleanSizes sizes))
        failures
        prog'
  where
    genPrelude TargetOpenCL = genOpenClPrelude
    genPrelude TargetCUDA = const genCUDAPrelude

-- | Due to simplifications after kernel extraction, some threshold
-- parameters may contain KernelPaths that reference threshold
-- parameters that no longer exist.  We remove these here.
cleanSizes :: M.Map Name SizeClass -> M.Map Name SizeClass
cleanSizes m = M.map clean m
  where
    known = M.keys m
    clean (SizeThreshold path def) =
      SizeThreshold (filter ((`elem` known) . fst) path) def
    clean s = s

findParamUsers ::
  Env ->
  Definitions ImpOpenCL.OpenCL ->
  M.Map Name SizeClass ->
  ParamMap
findParamUsers env defs = M.mapWithKey onParam
  where
    cg = envCallGraph env

    getSize (ImpOpenCL.GetSize _ v) = Just v
    getSize (ImpOpenCL.CmpSizeLe _ v _) = Just v
    getSize (ImpOpenCL.GetSizeMax {}) = Nothing
    getSize (ImpOpenCL.LaunchKernel {}) = Nothing
    directUseInFun fun = mapMaybe getSize $ toList $ functionBody fun
    direct_uses = map (second directUseInFun) $ unFunctions $ defFuns defs

    calledBy fname = M.findWithDefault mempty fname cg
    indirectUseInFun fname =
      ( fname,
        foldMap snd $ filter ((`S.member` calledBy fname) . fst) direct_uses
      )
    indirect_uses = direct_uses <> map (indirectUseInFun . fst) direct_uses

    onParam k c = (c, S.fromList $ map fst $ filter ((k `elem`) . snd) indirect_uses)

pointerQuals :: String -> [C.TypeQual]
pointerQuals "global" = [C.ctyquals|__global|]
pointerQuals "local" = [C.ctyquals|__local|]
pointerQuals "private" = [C.ctyquals|__private|]
pointerQuals "constant" = [C.ctyquals|__constant|]
pointerQuals "write_only" = [C.ctyquals|__write_only|]
pointerQuals "read_only" = [C.ctyquals|__read_only|]
pointerQuals "kernel" = [C.ctyquals|__kernel|]
-- OpenCL does not actually have a "device" space, but we use it in
-- the compiler pipeline to defer to memory on the device, as opposed
-- to the host.  From a kernel's perspective, this is "global".
pointerQuals "device" = pointerQuals "global"
pointerQuals s = error $ "'" ++ s ++ "' is not an OpenCL kernel address space."

-- In-kernel name and per-workgroup size in bytes.
type LocalMemoryUse = (VName, Count Bytes Exp)

data KernelState = KernelState
  { kernelLocalMemory :: [LocalMemoryUse],
    kernelFailures :: [FailureMsg],
    kernelNextSync :: Int,
    -- | Has a potential failure occurred sine the last
    -- ErrorSync?
    kernelSyncPending :: Bool,
    kernelHasBarriers :: Bool
  }

newKernelState :: [FailureMsg] -> KernelState
newKernelState failures = KernelState mempty failures 0 False False

errorLabel :: KernelState -> String
errorLabel = ("error_" ++) . show . kernelNextSync

data ToOpenCL = ToOpenCL
  { clGPU :: M.Map KernelName (KernelSafety, T.Text),
    clDevFuns :: M.Map Name (C.Definition, T.Text),
    clUsedTypes :: S.Set PrimType,
    clSizes :: M.Map Name SizeClass,
    clFailures :: [FailureMsg]
  }

initialOpenCL :: ToOpenCL
initialOpenCL = ToOpenCL mempty mempty mempty mempty mempty

data Env = Env
  { envFuns :: ImpGPU.Functions ImpGPU.HostOp,
    envFunsMayFail :: S.Set Name,
    envCallGraph :: M.Map Name (S.Set Name)
  }

codeMayFail :: (a -> Bool) -> ImpGPU.Code a -> Bool
codeMayFail _ (Assert {}) = True
codeMayFail f (Op x) = f x
codeMayFail f (x :>>: y) = codeMayFail f x || codeMayFail f y
codeMayFail f (For _ _ x) = codeMayFail f x
codeMayFail f (While _ x) = codeMayFail f x
codeMayFail f (If _ x y) = codeMayFail f x || codeMayFail f y
codeMayFail f (Comment _ x) = codeMayFail f x
codeMayFail _ _ = False

hostOpMayFail :: ImpGPU.HostOp -> Bool
hostOpMayFail (CallKernel k) = codeMayFail kernelOpMayFail $ kernelBody k
hostOpMayFail _ = False

kernelOpMayFail :: ImpGPU.KernelOp -> Bool
kernelOpMayFail = const False

funsMayFail :: M.Map Name (S.Set Name) -> ImpGPU.Functions ImpGPU.HostOp -> S.Set Name
funsMayFail cg (Functions funs) =
  S.fromList $ map fst $ filter mayFail funs
  where
    base_mayfail =
      map fst $ filter (codeMayFail hostOpMayFail . ImpGPU.functionBody . snd) funs
    mayFail (fname, _) =
      any (`elem` base_mayfail) $ fname : S.toList (M.findWithDefault mempty fname cg)

envFromProg :: ImpGPU.Program -> Env
envFromProg prog = Env funs (funsMayFail cg funs) cg
  where
    funs = defFuns prog
    cg = ImpGPU.callGraph calledInHostOp funs

lookupFunction :: Name -> Env -> Maybe (ImpGPU.Function HostOp)
lookupFunction fname = lookup fname . unFunctions . envFuns

functionMayFail :: Name -> Env -> Bool
functionMayFail fname = S.member fname . envFunsMayFail

type OnKernelM = ReaderT Env (State ToOpenCL)

addSize :: Name -> SizeClass -> OnKernelM ()
addSize key sclass =
  modify $ \s -> s {clSizes = M.insert key sclass $ clSizes s}

onHostOp :: KernelTarget -> HostOp -> OnKernelM OpenCL
onHostOp target (CallKernel k) = onKernel target k
onHostOp _ (ImpGPU.GetSize v key size_class) = do
  addSize key size_class
  pure $ ImpOpenCL.GetSize v key
onHostOp _ (ImpGPU.CmpSizeLe v key size_class x) = do
  addSize key size_class
  pure $ ImpOpenCL.CmpSizeLe v key x
onHostOp _ (ImpGPU.GetSizeMax v size_class) =
  pure $ ImpOpenCL.GetSizeMax v size_class

genGPUCode ::
  Env ->
  OpsMode ->
  KernelCode ->
  [FailureMsg] ->
  GC.CompilerM KernelOp KernelState a ->
  (a, GC.CompilerState KernelState)
genGPUCode env mode body failures =
  GC.runCompilerM
    (inKernelOperations env mode body)
    blankNameSource
    (newKernelState failures)

-- Compilation of a device function that is not not invoked from the
-- host, but is invoked by (perhaps multiple) kernels.
generateDeviceFun :: Name -> ImpGPU.Function ImpGPU.KernelOp -> OnKernelM ()
generateDeviceFun fname device_func = do
  when (any memParam $ functionInput device_func) bad

  env <- ask
  failures <- gets clFailures

  let (func, kstate) =
        if functionMayFail fname env
          then
            let params =
                  [ [C.cparam|__global int *global_failure|],
                    [C.cparam|__global typename int64_t *global_failure_args|]
                  ]
                (f, cstate) =
                  genGPUCode env FunMode (functionBody device_func) failures $
                    GC.compileFun mempty params (fname, device_func)
             in (f, GC.compUserState cstate)
          else
            let (f, cstate) =
                  genGPUCode env FunMode (functionBody device_func) failures $
                    GC.compileVoidFun mempty (fname, device_func)
             in (f, GC.compUserState cstate)

  modify $ \s ->
    s
      { clUsedTypes = typesInCode (functionBody device_func) <> clUsedTypes s,
        clDevFuns = M.insert fname (second funcText func) $ clDevFuns s,
        clFailures = kernelFailures kstate
      }

  -- Important to do this after the 'modify' call, so we propagate the
  -- right clFailures.
  void $ ensureDeviceFuns $ functionBody device_func
  where
    memParam MemParam {} = True
    memParam ScalarParam {} = False

    bad = compilerLimitationS "Cannot generate GPU functions that use arrays."

-- Ensure that this device function is available, but don't regenerate
-- it if it already exists.
ensureDeviceFun :: Name -> ImpGPU.Function ImpGPU.KernelOp -> OnKernelM ()
ensureDeviceFun fname host_func = do
  exists <- gets $ M.member fname . clDevFuns
  unless exists $ generateDeviceFun fname host_func

calledInHostOp :: HostOp -> S.Set Name
calledInHostOp (CallKernel k) = calledFuncs calledInKernelOp $ kernelBody k
calledInHostOp _ = mempty

calledInKernelOp :: KernelOp -> S.Set Name
calledInKernelOp = const mempty

ensureDeviceFuns :: ImpGPU.KernelCode -> OnKernelM [Name]
ensureDeviceFuns code = do
  let called = calledFuncs calledInKernelOp code
  fmap catMaybes . forM (S.toList called) $ \fname -> do
    def <- asks $ lookupFunction fname
    case def of
      Just host_func -> do
        -- Functions are a priori always considered host-level, so we have
        -- to convert them to device code.  This is where most of our
        -- limitations on device-side functions (no arrays, no parallelism)
        -- comes from.
        let device_func = fmap toDevice host_func
        ensureDeviceFun fname device_func
        pure $ Just fname
      Nothing -> pure Nothing
  where
    bad = compilerLimitationS "Cannot generate GPU functions that contain parallelism."
    toDevice :: HostOp -> KernelOp
    toDevice _ = bad

isConst :: GroupDim -> Maybe T.Text
isConst (Left (ValueExp (IntValue x))) =
  Just $ prettyText $ intToInt64 x
isConst (Right (SizeConst v)) =
  Just $ zEncodeText $ nameToText v
isConst (Right (SizeMaxConst size_class)) =
  Just $ "max_" <> prettyText size_class
isConst _ = Nothing

onKernel :: KernelTarget -> Kernel -> OnKernelM OpenCL
onKernel target kernel = do
  called <- ensureDeviceFuns $ kernelBody kernel

  -- Crucial that this is done after 'ensureDeviceFuns', as the device
  -- functions may themselves define failure points.
  failures <- gets clFailures
  env <- ask

  let (kernel_body, cstate) =
        genGPUCode env KernelMode (kernelBody kernel) failures . GC.collect $ do
          body <- GC.collect $ GC.compileCode $ kernelBody kernel
          -- No need to free, as we cannot allocate memory in kernels.
          mapM_ GC.item =<< GC.declAllocatedMem
          mapM_ GC.item body
      kstate = GC.compUserState cstate

      (local_memory_args, local_memory_params, local_memory_init) =
        unzip3 . flip evalState (blankNameSource :: VNameSource) $
          mapM (prepareLocalMemory target) $
            kernelLocalMemory kstate

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
          (TargetCUDA, [_, _, _]) ->
            ( [ [C.cparam|const int block_dim0|],
                [C.cparam|const int block_dim1|],
                [C.cparam|const int block_dim2|]
              ],
              mempty
            )
          _ ->
            ( mempty,
              [ [C.citem|const int block_dim0 = 0;|],
                [C.citem|const int block_dim1 = 1;|],
                [C.citem|const int block_dim2 = 2;|]
              ]
            )

      (const_defs, const_undefs) = unzip $ mapMaybe constDef $ kernelUses kernel

  let (use_params, unpack_params) =
        unzip $ mapMaybe useAsParam $ kernelUses kernel

  -- The local_failure variable is an int despite only really storing
  -- a single bit of information, as some OpenCL implementations
  -- (e.g. AMD) does not like byte-sized local memory (and the others
  -- likely pad to a whole word anyway).
  let (safety, error_init)
        -- We conservatively assume that any called function can fail.
        | not $ null called =
            ( SafetyFull,
              [C.citems|volatile __local int local_failure;
                        // Harmless for all threads to write this.
                        local_failure = 0;|]
            )
        | length (kernelFailures kstate) == length failures =
            if kernelFailureTolerant kernel
              then (SafetyNone, [])
              else -- No possible failures in this kernel, so if we make
              -- it past an initial check, then we are good to go.

                ( SafetyCheap,
                  [C.citems|if (*global_failure >= 0) { return; }|]
                )
        | otherwise =
            if not (kernelHasBarriers kstate)
              then
                ( SafetyFull,
                  [C.citems|if (*global_failure >= 0) { return; }|]
                )
              else
                ( SafetyFull,
                  [C.citems|
                     volatile __local int local_failure;
                     if (failure_is_an_option) {
                       int failed = *global_failure >= 0;
                       if (failed) {
                         return;
                       }
                     }
                     // All threads write this value - it looks like CUDA has a compiler bug otherwise.
                     local_failure = 0;
                     barrier(CLK_LOCAL_MEM_FENCE);
                  |]
                )

      failure_params =
        [ [C.cparam|__global int *global_failure|],
          [C.cparam|int failure_is_an_option|],
          [C.cparam|__global typename int64_t *global_failure_args|]
        ]

      params =
        perm_params
          ++ take (numFailureParams safety) failure_params
          ++ catMaybes local_memory_params
          ++ use_params

      attribute =
        case (target, mapM isConst $ kernelGroupSize kernel) of
          (TargetOpenCL, Just [x, y, z]) ->
            "__attribute__((reqd_work_group_size" <> prettyText (x, y, z) <> "))\n"
          (TargetOpenCL, Just [x, y]) ->
            "__attribute__((reqd_work_group_size" <> prettyText (x, y, 1 :: Int) <> "))\n"
          (TargetOpenCL, Just [x]) ->
            "__attribute__((reqd_work_group_size" <> prettyText (x, 1 :: Int, 1 :: Int) <> "))\n"
          _ -> ""

      kernel_fun =
        attribute
          <> funcText
            [C.cfun|__kernel void $id:name ($params:params) {
                    $items:(mconcat unpack_params)
                    $items:const_defs
                    $items:block_dim_init
                    $items:local_memory_init
                    $items:error_init
                    $items:kernel_body

                    $id:(errorLabel kstate): return;

                    $items:const_undefs
                }|]
  modify $ \s ->
    s
      { clGPU = M.insert name (safety, kernel_fun) $ clGPU s,
        clUsedTypes = typesInKernel kernel <> clUsedTypes s,
        clFailures = kernelFailures kstate
      }

  -- The argument corresponding to the global_failure parameters is
  -- added automatically later.
  let args = catMaybes local_memory_args ++ kernelArgs kernel

  pure $ LaunchKernel safety name args num_groups group_size
  where
    name = kernelName kernel
    num_groups = kernelNumGroups kernel
    group_size = kernelGroupSize kernel

    prepareLocalMemory TargetOpenCL (mem, size) = do
      mem_aligned <- newVName $ baseString mem ++ "_aligned"
      pure
        ( Just $ SharedMemoryKArg size,
          Just [C.cparam|__local volatile typename int64_t* $id:mem_aligned|],
          [C.citem|__local volatile unsigned char* restrict $id:mem = (__local volatile unsigned char*) $id:mem_aligned;|]
        )
    prepareLocalMemory TargetCUDA (mem, size) = do
      param <- newVName $ baseString mem ++ "_offset"
      pure
        ( Just $ SharedMemoryKArg size,
          Just [C.cparam|uint $id:param|],
          [C.citem|volatile $ty:defaultMemBlockType $id:mem = &shared_mem[$id:param];|]
        )

useAsParam :: KernelUse -> Maybe (C.Param, [C.BlockItem])
useAsParam (ScalarUse name pt) = do
  let name_bits = zEncodeText (prettyText name) <> "_bits"
      ctp = case pt of
        -- OpenCL does not permit bool as a kernel parameter type.
        Bool -> [C.cty|unsigned char|]
        Unit -> [C.cty|unsigned char|]
        _ -> primStorageType pt
  if ctp == primTypeToCType pt
    then Just ([C.cparam|$ty:ctp $id:name|], [])
    else
      let name_bits_e = [C.cexp|$id:name_bits|]
       in Just
            ( [C.cparam|$ty:ctp $id:name_bits|],
              [[C.citem|$ty:(primTypeToCType pt) $id:name = $exp:(fromStorage pt name_bits_e);|]]
            )
useAsParam (MemoryUse name) =
  Just ([C.cparam|__global $ty:defaultMemBlockType $id:name|], [])
useAsParam ConstUse {} =
  Nothing

-- Constants are #defined as macros.  Since a constant name in one
-- kernel might potentially (although unlikely) also be used for
-- something else in another kernel, we #undef them after the kernel.
constDef :: KernelUse -> Maybe (C.BlockItem, C.BlockItem)
constDef (ConstUse v e) =
  Just
    ( [C.citem|$escstm:(T.unpack def)|],
      [C.citem|$escstm:(T.unpack undef)|]
    )
  where
    e' = compilePrimExp e
    def = "#define " <> idText (C.toIdent v mempty) <> " (" <> expText e' <> ")"
    undef = "#undef " <> idText (C.toIdent v mempty)
constDef _ = Nothing

genOpenClPrelude :: S.Set PrimType -> T.Text
genOpenClPrelude ts =
  [untrimming|
// Clang-based OpenCL implementations need this for 'static' to work.
#ifdef cl_clang_storage_class_specifiers
#pragma OPENCL EXTENSION cl_clang_storage_class_specifiers : enable
#endif
#pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable
$enable_f64
// Some OpenCL programs dislike empty progams, or programs with no kernels.
// Declare a dummy kernel to ensure they remain our friends.
__kernel void dummy_kernel(__global unsigned char *dummy, int n)
{
    const int thread_gid = get_global_id(0);
    if (thread_gid >= n) return;
}

#pragma OPENCL EXTENSION cl_khr_int64_base_atomics : enable
#pragma OPENCL EXTENSION cl_khr_int64_extended_atomics : enable

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
#ifdef cl_nv_pragma_unroll
static inline void mem_fence_global() {
  asm("membar.gl;");
}
#else
static inline void mem_fence_global() {
  mem_fence(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
}
#endif
static inline void mem_fence_local() {
  mem_fence(CLK_LOCAL_MEM_FENCE);
}
|]
    <> halfH
    <> cScalarDefs
    <> atomicsH
  where
    enable_f64
      | FloatType Float64 `S.member` ts =
          [untrimming|
         #pragma OPENCL EXTENSION cl_khr_fp64 : enable
         #define FUTHARK_F64_ENABLED
         |]
      | otherwise = mempty

genCUDAPrelude :: T.Text
genCUDAPrelude =
  [untrimming|
#define FUTHARK_CUDA
#define FUTHARK_F64_ENABLED

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
#define __kernel extern "C" __global__ __launch_bounds__(MAX_THREADS_PER_BLOCK)
#define __global
#define __local
#define __private
#define __constant
#define __write_only
#define __read_only

static inline int get_group_id_fn(int block_dim0, int block_dim1, int block_dim2, int d) {
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
#define get_group_id(d) get_group_id_fn(block_dim0, block_dim1, block_dim2, d)

static inline int get_num_groups_fn(int block_dim0, int block_dim1, int block_dim2, int d) {
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
#define get_num_groups(d) get_num_groups_fn(block_dim0, block_dim1, block_dim2, d)

static inline int get_local_id(int d) {
  switch (d) {
    case 0: return threadIdx.x;
    case 1: return threadIdx.y;
    case 2: return threadIdx.z;
    default: return 0;
  }
}

static inline int get_local_size(int d) {
  switch (d) {
    case 0: return blockDim.x;
    case 1: return blockDim.y;
    case 2: return blockDim.z;
    default: return 0;
  }
}

#define CLK_LOCAL_MEM_FENCE 1
#define CLK_GLOBAL_MEM_FENCE 2
static inline void barrier(int x) {
  __syncthreads();
}
static inline void mem_fence_local() {
  __threadfence_block();
}
static inline void mem_fence_global() {
  __threadfence();
}

#define NAN (0.0/0.0)
#define INFINITY (1.0/0.0)
extern volatile __shared__ unsigned char shared_mem[];
|]
    <> halfH
    <> cScalarDefs
    <> atomicsH

compilePrimExp :: PrimExp KernelConst -> C.Exp
compilePrimExp e = runIdentity $ GC.compilePrimExp compileKernelConst e
  where
    compileKernelConst (SizeConst key) =
      pure [C.cexp|$id:(zEncodeText (prettyText key))|]
    compileKernelConst (SizeMaxConst size_class) =
      pure [C.cexp|$id:("max_" <> prettyString size_class)|]

kernelArgs :: Kernel -> [KernelArg]
kernelArgs = mapMaybe useToArg . kernelUses
  where
    useToArg (MemoryUse mem) = Just $ MemKArg mem
    useToArg (ScalarUse v pt) = Just $ ValueKArg (LeafExp v pt) pt
    useToArg ConstUse {} = Nothing

nextErrorLabel :: GC.CompilerM KernelOp KernelState String
nextErrorLabel =
  errorLabel <$> GC.getUserState

incErrorLabel :: GC.CompilerM KernelOp KernelState ()
incErrorLabel =
  GC.modifyUserState $ \s -> s {kernelNextSync = kernelNextSync s + 1}

pendingError :: Bool -> GC.CompilerM KernelOp KernelState ()
pendingError b =
  GC.modifyUserState $ \s -> s {kernelSyncPending = b}

hasCommunication :: ImpGPU.KernelCode -> Bool
hasCommunication = any communicates
  where
    communicates ErrorSync {} = True
    communicates Barrier {} = True
    communicates _ = False

-- Whether we are generating code for a kernel or a device function.
-- This has minor effects, such as exactly how failures are
-- propagated.
data OpsMode = KernelMode | FunMode deriving (Eq)

inKernelOperations ::
  Env ->
  OpsMode ->
  ImpGPU.KernelCode ->
  GC.Operations KernelOp KernelState
inKernelOperations env mode body =
  GC.Operations
    { GC.opsCompiler = kernelOps,
      GC.opsMemoryType = kernelMemoryType,
      GC.opsWriteScalar = kernelWriteScalar,
      GC.opsReadScalar = kernelReadScalar,
      GC.opsAllocate = cannotAllocate,
      GC.opsDeallocate = cannotDeallocate,
      GC.opsCopy = copyInKernel,
      GC.opsFatMemory = False,
      GC.opsError = errorInKernel,
      GC.opsCall = callInKernel,
      GC.opsCritical = mempty
    }
  where
    has_communication = hasCommunication body

    fence FenceLocal = [C.cexp|CLK_LOCAL_MEM_FENCE|]
    fence FenceGlobal = [C.cexp|CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE|]

    kernelOps :: GC.OpCompiler KernelOp KernelState
    kernelOps (GetGroupId v i) =
      GC.stm [C.cstm|$id:v = get_group_id($int:i);|]
    kernelOps (GetLocalId v i) =
      GC.stm [C.cstm|$id:v = get_local_id($int:i);|]
    kernelOps (GetLocalSize v i) =
      GC.stm [C.cstm|$id:v = get_local_size($int:i);|]
    kernelOps (GetLockstepWidth v) =
      GC.stm [C.cstm|$id:v = LOCKSTEP_WIDTH;|]
    kernelOps (Barrier f) = do
      GC.stm [C.cstm|barrier($exp:(fence f));|]
      GC.modifyUserState $ \s -> s {kernelHasBarriers = True}
    kernelOps (MemFence FenceLocal) =
      GC.stm [C.cstm|mem_fence_local();|]
    kernelOps (MemFence FenceGlobal) =
      GC.stm [C.cstm|mem_fence_global();|]
    kernelOps (LocalAlloc name size) = do
      name' <- newVName $ prettyString name ++ "_backing"
      GC.modifyUserState $ \s ->
        s {kernelLocalMemory = (name', fmap untyped size) : kernelLocalMemory s}
      GC.stm [C.cstm|$id:name = (__local unsigned char*) $id:name';|]
    kernelOps (ErrorSync f) = do
      label <- nextErrorLabel
      pending <- kernelSyncPending <$> GC.getUserState
      when pending $ do
        pendingError False
        GC.stm [C.cstm|$id:label: barrier($exp:(fence f));|]
        GC.stm [C.cstm|if (local_failure) { return; }|]
      GC.stm [C.cstm|barrier($exp:(fence f));|]
      GC.modifyUserState $ \s -> s {kernelHasBarriers = True}
      incErrorLabel
    kernelOps (Atomic space aop) = atomicOps space aop

    atomicCast s t = do
      let volatile = [C.ctyquals|volatile|]
      let quals = case s of
            Space sid -> pointerQuals sid
            _ -> pointerQuals "global"
      pure [C.cty|$tyquals:(volatile++quals) $ty:t|]

    atomicSpace (Space sid) = sid
    atomicSpace _ = "global"

    doAtomic s t old arr ind val op ty = do
      ind' <- GC.compileExp $ untyped $ unCount ind
      val' <- GC.compileExp val
      cast <- atomicCast s ty
      GC.stm [C.cstm|$id:old = $id:op'(&(($ty:cast *)$id:arr)[$exp:ind'], ($ty:ty) $exp:val');|]
      where
        op' = op ++ "_" ++ prettyString t ++ "_" ++ atomicSpace s

    doAtomicCmpXchg s t old arr ind cmp val ty = do
      ind' <- GC.compileExp $ untyped $ unCount ind
      cmp' <- GC.compileExp cmp
      val' <- GC.compileExp val
      cast <- atomicCast s ty
      GC.stm [C.cstm|$id:old = $id:op(&(($ty:cast *)$id:arr)[$exp:ind'], $exp:cmp', $exp:val');|]
      where
        op = "atomic_cmpxchg_" ++ prettyString t ++ "_" ++ atomicSpace s
    doAtomicXchg s t old arr ind val ty = do
      cast <- atomicCast s ty
      ind' <- GC.compileExp $ untyped $ unCount ind
      val' <- GC.compileExp val
      GC.stm [C.cstm|$id:old = $id:op(&(($ty:cast *)$id:arr)[$exp:ind'], $exp:val');|]
      where
        op = "atomic_chg_" ++ prettyString t ++ "_" ++ atomicSpace s
    -- First the 64-bit operations.
    atomicOps s (AtomicAdd Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_add" [C.cty|typename int64_t|]
    atomicOps s (AtomicFAdd Float64 old arr ind val) =
      doAtomic s Float64 old arr ind val "atomic_fadd" [C.cty|double|]
    atomicOps s (AtomicSMax Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_smax" [C.cty|typename int64_t|]
    atomicOps s (AtomicSMin Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_smin" [C.cty|typename int64_t|]
    atomicOps s (AtomicUMax Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_umax" [C.cty|unsigned int64_t|]
    atomicOps s (AtomicUMin Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_umin" [C.cty|unsigned int64_t|]
    atomicOps s (AtomicAnd Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_and" [C.cty|typename int64_t|]
    atomicOps s (AtomicOr Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_or" [C.cty|typename int64_t|]
    atomicOps s (AtomicXor Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_xor" [C.cty|typename int64_t|]
    atomicOps s (AtomicCmpXchg (IntType Int64) old arr ind cmp val) =
      doAtomicCmpXchg s (IntType Int64) old arr ind cmp val [C.cty|typename int64_t|]
    atomicOps s (AtomicXchg (IntType Int64) old arr ind val) =
      doAtomicXchg s (IntType Int64) old arr ind val [C.cty|typename int64_t|]
    --
    atomicOps s (AtomicAdd t old arr ind val) =
      doAtomic s t old arr ind val "atomic_add" [C.cty|int|]
    atomicOps s (AtomicFAdd t old arr ind val) =
      doAtomic s t old arr ind val "atomic_fadd" [C.cty|float|]
    atomicOps s (AtomicSMax t old arr ind val) =
      doAtomic s t old arr ind val "atomic_smax" [C.cty|int|]
    atomicOps s (AtomicSMin t old arr ind val) =
      doAtomic s t old arr ind val "atomic_smin" [C.cty|int|]
    atomicOps s (AtomicUMax t old arr ind val) =
      doAtomic s t old arr ind val "atomic_umax" [C.cty|unsigned int|]
    atomicOps s (AtomicUMin t old arr ind val) =
      doAtomic s t old arr ind val "atomic_umin" [C.cty|unsigned int|]
    atomicOps s (AtomicAnd t old arr ind val) =
      doAtomic s t old arr ind val "atomic_and" [C.cty|int|]
    atomicOps s (AtomicOr t old arr ind val) =
      doAtomic s t old arr ind val "atomic_or" [C.cty|int|]
    atomicOps s (AtomicXor t old arr ind val) =
      doAtomic s t old arr ind val "atomic_xor" [C.cty|int|]
    atomicOps s (AtomicCmpXchg t old arr ind cmp val) =
      doAtomicCmpXchg s t old arr ind cmp val [C.cty|int|]
    atomicOps s (AtomicXchg t old arr ind val) =
      doAtomicXchg s t old arr ind val [C.cty|int|]

    cannotAllocate :: GC.Allocate KernelOp KernelState
    cannotAllocate _ =
      error "Cannot allocate memory in kernel"

    cannotDeallocate :: GC.Deallocate KernelOp KernelState
    cannotDeallocate _ _ =
      error "Cannot deallocate memory in kernel"

    copyInKernel :: GC.Copy KernelOp KernelState
    copyInKernel _ _ _ _ _ _ _ _ =
      error "Cannot bulk copy in kernel."

    kernelMemoryType space =
      pure [C.cty|$tyquals:(pointerQuals space) $ty:defaultMemBlockType|]

    kernelWriteScalar =
      GC.writeScalarPointerWithQuals pointerQuals

    kernelReadScalar =
      GC.readScalarPointerWithQuals pointerQuals

    whatNext = do
      label <- nextErrorLabel
      pendingError True
      pure $
        if has_communication
          then [C.citems|local_failure = 1; goto $id:label;|]
          else
            if mode == FunMode
              then [C.citems|return 1;|]
              else [C.citems|return;|]

    callInKernel dests fname args
      | functionMayFail fname env = do
          let out_args = [[C.cexp|&$id:d|] | d <- dests]
              args' =
                [C.cexp|global_failure|]
                  : [C.cexp|global_failure_args|]
                  : out_args
                  ++ args

          what_next <- whatNext
          GC.item [C.citem|if ($id:(funName fname)($args:args') != 0) { $items:what_next; }|]
      | otherwise = do
          let out_args = [[C.cexp|&$id:d|] | d <- dests]
              args' = out_args ++ args
          GC.item [C.citem|$id:(funName fname)($args:args');|]

    errorInKernel msg@(ErrorMsg parts) backtrace = do
      n <- length . kernelFailures <$> GC.getUserState
      GC.modifyUserState $ \s ->
        s {kernelFailures = kernelFailures s ++ [FailureMsg msg backtrace]}
      let setArgs _ [] = pure []
          setArgs i (ErrorString {} : parts') = setArgs i parts'
          -- FIXME: bogus for non-ints.
          setArgs i (ErrorVal _ x : parts') = do
            x' <- GC.compileExp x
            stms <- setArgs (i + 1) parts'
            pure $ [C.cstm|global_failure_args[$int:i] = (typename int64_t)$exp:x';|] : stms
      argstms <- setArgs (0 :: Int) parts

      what_next <- whatNext

      GC.stm
        [C.cstm|{ if (atomic_cmpxchg_i32_global(global_failure, -1, $int:n) == -1)
                                 { $stms:argstms; }
                                 $items:what_next
                               }|]

--- Checking requirements

typesInKernel :: Kernel -> S.Set PrimType
typesInKernel kernel = typesInCode $ kernelBody kernel

typesInCode :: ImpGPU.KernelCode -> S.Set PrimType
typesInCode Skip = mempty
typesInCode (c1 :>>: c2) = typesInCode c1 <> typesInCode c2
typesInCode (For _ e c) = typesInExp e <> typesInCode c
typesInCode (While (TPrimExp e) c) = typesInExp e <> typesInCode c
typesInCode DeclareMem {} = mempty
typesInCode (DeclareScalar _ _ t) = S.singleton t
typesInCode (DeclareArray _ t _) = S.singleton t
typesInCode (Allocate _ (Count (TPrimExp e)) _) = typesInExp e
typesInCode Free {} = mempty
typesInCode (Copy _ _ (Count (TPrimExp e1)) _ _ (Count (TPrimExp e2)) _ (Count (TPrimExp e3))) =
  typesInExp e1 <> typesInExp e2 <> typesInExp e3
typesInCode (Write _ (Count (TPrimExp e1)) t _ _ e2) =
  typesInExp e1 <> S.singleton t <> typesInExp e2
typesInCode (Read _ _ (Count (TPrimExp e1)) t _ _) =
  typesInExp e1 <> S.singleton t
typesInCode (SetScalar _ e) = typesInExp e
typesInCode SetMem {} = mempty
typesInCode (Call _ _ es) = mconcat $ map typesInArg es
  where
    typesInArg MemArg {} = mempty
    typesInArg (ExpArg e) = typesInExp e
typesInCode (If (TPrimExp e) c1 c2) =
  typesInExp e <> typesInCode c1 <> typesInCode c2
typesInCode (Assert e _ _) = typesInExp e
typesInCode (Comment _ c) = typesInCode c
typesInCode (DebugPrint _ v) = maybe mempty typesInExp v
typesInCode (TracePrint msg) = foldMap typesInExp msg
typesInCode Op {} = mempty

typesInExp :: Exp -> S.Set PrimType
typesInExp (ValueExp v) = S.singleton $ primValueType v
typesInExp (BinOpExp _ e1 e2) = typesInExp e1 <> typesInExp e2
typesInExp (CmpOpExp _ e1 e2) = typesInExp e1 <> typesInExp e2
typesInExp (ConvOpExp op e) = S.fromList [from, to] <> typesInExp e
  where
    (from, to) = convOpType op
typesInExp (UnOpExp _ e) = typesInExp e
typesInExp (FunExp _ args t) = S.singleton t <> mconcat (map typesInExp args)
typesInExp LeafExp {} = mempty
