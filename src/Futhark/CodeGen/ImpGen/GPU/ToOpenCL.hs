{-# LANGUAGE QuasiQuotes #-}

-- | This module defines a translation from imperative code with
-- kernels to imperative code with OpenCL, CUDA, HIP, or Metal calls.
module Futhark.CodeGen.ImpGen.GPU.ToOpenCL
  ( kernelsToOpenCL,
    kernelsToCUDA,
    kernelsToHIP,
    kernelsToMetal,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.List qualified as L
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
import Futhark.CodeGen.RTS.CUDA (preludeCU)
import Futhark.CodeGen.RTS.Metal (preludeMetal)
import Futhark.CodeGen.RTS.OpenCL (copyCL, preludeCL, transposeCL)
import Futhark.Error (compilerLimitationS)
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeText)
import Futhark.Util.IntegralExp (rem)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C
import NeatInterpolation (untrimming)
import Prelude hiding (rem)

-- | Generate HIP host and device code.
kernelsToHIP :: ImpGPU.Program -> ImpOpenCL.Program
kernelsToHIP = translateGPU TargetHIP

-- | Generate CUDA host and device code.
kernelsToCUDA :: ImpGPU.Program -> ImpOpenCL.Program
kernelsToCUDA = translateGPU TargetCUDA

-- | Generate OpenCL host and device code.
kernelsToOpenCL :: ImpGPU.Program -> ImpOpenCL.Program
kernelsToOpenCL = translateGPU TargetOpenCL

-- | Generate Metal host and device code.
kernelsToMetal :: ImpGPU.Program -> ImpOpenCL.Program
kernelsToMetal = translateGPU TargetMetal

-- | Translate a kernels-program to an OpenCL-program.
translateGPU ::
  KernelTarget ->
  ImpGPU.Program ->
  ImpOpenCL.Program
translateGPU target prog =
  let env = envFromProg target prog
      ( prog',
        ToOpenCL kernels device_funs used_types failures constants
        ) =
          (`runState` initialOpenCL) . (`runReaderT` env) $ do
            let ImpGPU.Definitions
                  params
                  types
                  (ImpGPU.Constants ps consts)
                  (ImpGPU.Functions funs) = prog
            consts' <- traverse (onHostOp target) consts
            funs' <- forM funs $ \(fname, fun) ->
              (fname,) <$> traverse (onHostOp target) fun

            pure $
              ImpOpenCL.Definitions
                (findParamUsers env prog' (cleanParams params))
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
   in if target == TargetMetal && FloatType Float64 `S.member` used_types
        then compilerLimitationS "Metal does not support double precision (f64) in GPU code."
        else
          ImpOpenCL.Program
            { openClProgram = opencl_code,
              openClPrelude = opencl_prelude,
              openClMacroDefs = constants,
              openClKernelNames = kernels',
              openClUsedTypes = S.toList used_types,
              openClFailures = failures,
              hostDefinitions = prog'
            }
  where
    genPrelude TargetOpenCL = genOpenClPrelude
    genPrelude TargetCUDA = const genCUDAPrelude
    genPrelude TargetHIP = const genHIPPrelude
    genPrelude TargetMetal = const genMetalPrelude

-- | Due to simplifications after kernel extraction, some threshold
-- parameters may contain KernelPaths that reference threshold
-- parameters that no longer exist.  We remove these here.
cleanParams :: ParamMap -> ParamMap
cleanParams m = M.map clean m
  where
    known = M.keys m
    clean (Just (SizeThreshold path def), users) =
      (Just $ SizeThreshold (filter ((`elem` known) . fst) path) def, users)
    clean s = s

findParamUsers ::
  Env ->
  Definitions ImpOpenCL.OpenCL ->
  ParamMap ->
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

    onParam k (c, _) = (c, S.fromList $ map fst $ filter ((k `elem`) . snd) indirect_uses)

pointerQuals :: String -> [C.TypeQual]
pointerQuals "global" = [C.ctyquals|__global|]
pointerQuals "shared" = [C.ctyquals|__local|]
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

-- In-kernel name and per-threadblock size in bytes.
type SharedMemoryUse = (VName, Count Bytes (TExp Int64))

data KernelState = KernelState
  { kernelSharedMemory :: [SharedMemoryUse],
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
    clFailures :: [FailureMsg],
    clConstants :: [(Name, KernelConstExp)]
  }

initialOpenCL :: ToOpenCL
initialOpenCL = ToOpenCL mempty mempty mempty mempty mempty

data Env = Env
  { envTarget :: KernelTarget,
    envFuns :: ImpGPU.Functions ImpGPU.HostOp,
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

envFromProg :: KernelTarget -> ImpGPU.Program -> Env
envFromProg target prog = Env target funs (funsMayFail cg funs) cg
  where
    funs = defFuns prog
    cg = ImpGPU.callGraph calledInHostOp funs

lookupFunction :: Name -> Env -> Maybe (ImpGPU.Function HostOp)
lookupFunction fname = lookup fname . unFunctions . envFuns

functionMayFail :: Name -> Env -> Bool
functionMayFail fname = S.member fname . envFunsMayFail

type OnKernelM = ReaderT Env (State ToOpenCL)

onHostOp :: KernelTarget -> HostOp -> OnKernelM OpenCL
onHostOp target (CallKernel k) = onKernel target k
onHostOp _ (ImpGPU.GetSize v key _) =
  pure $ ImpOpenCL.GetSize v key
onHostOp _ (ImpGPU.CmpSizeLe v key _ x) =
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
                  genGPUCode env FunMode (declsFirst $ functionBody device_func) failures $
                    GC.compileFun mempty params (fname, device_func)
             in (f, GC.compUserState cstate)
          else
            let (f, cstate) =
                  genGPUCode env FunMode (declsFirst $ functionBody device_func) failures $
                    GC.compileVoidFun mempty (fname, device_func)
             in (f, GC.compUserState cstate)

  let func'
        | envTarget env == TargetMetal = metalDeviceFun func
        | otherwise = func

  modify $ \s ->
    s
      { clUsedTypes = typesInCode (functionBody device_func) <> clUsedTypes s,
        clDevFuns = M.insert fname (second funcText func') $ clDevFuns s,
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

isConst :: BlockDim -> Maybe KernelConstExp
isConst (Left (ValueExp (IntValue x))) =
  Just $ ValueExp $ IntValue x
isConst (Right e) =
  Just e
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
          body <- GC.collect $ GC.compileCode $ declsFirst $ kernelBody kernel
          -- No need to free, as we cannot allocate memory in kernels.
          mapM_ GC.item =<< GC.declAllocatedMem
          mapM_ GC.item body
      kstate = GC.compUserState cstate

      (kernel_consts, (const_defs, const_undefs)) =
        second unzip $ unzip $ mapMaybe (constDef (kernelName kernel)) $ kernelUses kernel

  let (_, shared_memory_init) =
        L.mapAccumL prepareSharedMemory [C.cexp|0|] (kernelSharedMemory kstate)
      shared_memory_bytes = sum $ map (padTo8 . snd) $ kernelSharedMemory kstate

  let (use_params, unpack_params) =
        unzip $ mapMaybe useAsParam $ kernelUses kernel

  -- The local_failure variable is an int despite only really storing
  -- a single bit of information, as some OpenCL implementations
  -- (e.g. AMD) does not like byte-sized shared memory (and the others
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

      (shared_memory_param, prepare_shared_memory) =
        case target of
          TargetOpenCL ->
            ( [[C.cparam|__local typename uint64_t* shared_mem_aligned|]],
              [C.citems|__local unsigned char* shared_mem = (__local unsigned char*)shared_mem_aligned;|]
            )
          TargetCUDA -> (mempty, mempty)
          TargetHIP -> (mempty, mempty)
          -- The parameter is part of FUTHARK_KERNEL_PARAMS.
          TargetMetal ->
            ( mempty,
              [C.citems|__local unsigned char* shared_mem = (__local unsigned char*)shared_mem_aligned;|]
            )

      args_params = take (numFailureParams safety) failure_params ++ use_params

      params = shared_memory_param ++ args_params

      (kernel_body', end_label)
        | target == TargetMetal =
            (metalFailureFlag kernel_body <> eliminateGotos kernel_body, [])
        | otherwise =
            (kernel_body, [C.citems|$id:(errorLabel kstate): return;|])

      (attribute_consts, attribute) =
        case mapM isConst $ kernelBlockSize kernel of
          Just [x, y, z] ->
            ( [(xv, x), (yv, y), (zv, z)],
              "FUTHARK_KERNEL_SIZED" <> prettyText (xv, yv, zv) <> "\n"
            )
            where
              xv = nameFromText $ zEncodeText $ nameToText name <> "_dim1"
              yv = nameFromText $ zEncodeText $ nameToText name <> "_dim2"
              zv = nameFromText $ zEncodeText $ nameToText name <> "_dim3"
          Just [x, y] ->
            ( [(xv, x), (yv, y)],
              "FUTHARK_KERNEL_SIZED" <> prettyText (xv, yv, 1 :: Int) <> "\n"
            )
            where
              xv = nameFromText $ zEncodeText $ nameToText name <> "_dim1"
              yv = nameFromText $ zEncodeText $ nameToText name <> "_dim2"
          Just [x] ->
            ( [(xv, x)],
              "FUTHARK_KERNEL_SIZED" <> prettyText (xv, 1 :: Int, 1 :: Int) <> "\n"
            )
            where
              xv = nameFromText $ zEncodeText $ nameToText name <> "_dim1"
          _ -> (mempty, "FUTHARK_KERNEL\n")

      body_items =
        [C.citems|$items:(mconcat unpack_params)
                  $items:const_defs
                  $items:prepare_shared_memory
                  $items:(mconcat shared_memory_init)
                  $items:error_init
                  $items:kernel_body'

                  $items:end_label

                  $items:const_undefs|]

      kernel_fun
        | target == TargetMetal =
            metalKernel name attribute args_params body_items
        | otherwise =
            attribute
              <> funcText [C.cfun|void $id:name ($params:params) { $items:body_items }|]
  modify $ \s ->
    s
      { clGPU = M.insert name (safety, kernel_fun) $ clGPU s,
        clUsedTypes = typesInKernel kernel <> clUsedTypes s,
        clFailures = kernelFailures kstate,
        clConstants = attribute_consts <> kernel_consts <> clConstants s
      }

  -- The error handling stuff is automatically added later.
  let args = kernelArgs kernel

  pure $ LaunchKernel safety name shared_memory_bytes args num_tblocks tblock_size
  where
    name = kernelName kernel
    num_tblocks = kernelNumBlocks kernel
    tblock_size = kernelBlockSize kernel
    padTo8 e = e + ((8 - (e `rem` 8)) `rem` 8)

    prepareSharedMemory offset (mem, Count size) =
      let offset_v = nameFromText $ prettyText mem <> "_offset"
       in ( [C.cexp|$id:offset_v|],
            [C.citems|
             volatile __local $ty:defaultMemBlockType $id:mem = &shared_mem[$exp:offset];
             const typename int64_t $id:offset_v = $exp:offset + $exp:(padTo8 size);
             |]
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
constDef :: Name -> KernelUse -> Maybe ((Name, KernelConstExp), (C.BlockItem, C.BlockItem))
constDef kernel_name (ConstUse v e) =
  Just
    ( (nameFromText v', e),
      ( [C.citem|$escstm:(T.unpack def)|],
        [C.citem|$escstm:(T.unpack undef)|]
      )
    )
  where
    v' = zEncodeText $ nameToText kernel_name <> "." <> prettyText v
    def = "#define " <> idText (C.toIdent v mempty) <> " (" <> v' <> ")"
    undef = "#undef " <> idText (C.toIdent v mempty)
constDef _ _ = Nothing

commonPrelude :: T.Text
commonPrelude =
  halfH
    <> cScalarDefs
    <> atomicsH
    <> transposeCL
    <> copyCL

genOpenClPrelude :: S.Set PrimType -> T.Text
genOpenClPrelude ts =
  "#define FUTHARK_OPENCL\n"
    <> enable_f64
    <> preludeCL
    <> commonPrelude
  where
    enable_f64
      | FloatType Float64 `S.member` ts =
          [untrimming|#define FUTHARK_F64_ENABLED|]
      | otherwise = mempty

genCUDAPrelude :: T.Text
genCUDAPrelude =
  "#define FUTHARK_CUDA\n"
    <> preludeCU
    <> commonPrelude

genHIPPrelude :: T.Text
genHIPPrelude =
  "#define FUTHARK_HIP\n"
    <> preludeCU
    <> commonPrelude

genMetalPrelude :: T.Text
genMetalPrelude =
  "#define FUTHARK_METAL\n"
    <> preludeMetal
    <> commonPrelude

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
      GC.opsCopies = mempty,
      GC.opsFatMemory = False,
      GC.opsError = errorInKernel,
      GC.opsCall = callInKernel,
      GC.opsCritical = mempty,
      GC.opsGetParam = \name ->
        let name_val = "val_" <> nameToString name
            name_set = "set_" <> nameToString name
         in ([C.cexp|$id:name_val|], [C.cexp|$id:name_set|])
    }
  where
    has_communication = hasCommunication body

    fence FenceLocal = [C.cexp|CLK_LOCAL_MEM_FENCE|]
    fence FenceGlobal = [C.cexp|CLK_GLOBAL_MEM_FENCE | CLK_LOCAL_MEM_FENCE|]

    kernelOps :: GC.OpCompiler KernelOp KernelState
    kernelOps (GetBlockId v i) =
      GC.stm [C.cstm|$id:v = get_tblock_id($int:i);|]
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
    kernelOps (SharedAlloc name size) = do
      name' <- newVName $ nameFromText $ prettyText name <> "_backing"
      GC.modifyUserState $ \s ->
        s {kernelSharedMemory = (name', size) : kernelSharedMemory s}
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
    kernelOps (Atomic space aop)
      | envTarget env == TargetMetal,
        Just t <- atomicType aop,
        primByteSize t == (8 :: Int) =
          compilerLimitationS $
            "Metal does not support 64-bit atomic operations (on "
              <> prettyString t
              <> ")."
      | otherwise = atomicOps space aop

    atomicCast s t = do
      let volatile = [C.ctyquals|volatile|]
      let quals = case s of
            Space sid -> pointerQuals sid
            _ -> pointerQuals "global"
      pure [C.cty|$tyquals:(volatile++quals) $ty:t|]

    atomicSpace (Space sid) = sid
    atomicSpace ScalarSpace {} = error "atomicSpace: cannot do atomics on ScalarSpace"
    atomicSpace DefaultSpace = "global"

    doAtomic s t old arr ind val op ty = do
      ind' <- GC.compileExp $ untyped $ unCount ind
      val' <- GC.compileExp val
      cast <- atomicCast s ty
      GC.stm [C.cstm|$id:old = $id:op'(&(($ty:cast *)$id:arr)[$exp:ind'], $exp:val');|]
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
    -- 64-bit operations
    atomicOps s (AtomicAdd Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_add" [C.cty|typename int64_t|]
    atomicOps s (AtomicFAdd Float64 old arr ind val) =
      doAtomic s Float64 old arr ind val "atomic_fadd" [C.cty|double|]
    atomicOps s (AtomicSMax Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_smax" [C.cty|typename int64_t|]
    atomicOps s (AtomicSMin Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_smin" [C.cty|typename int64_t|]
    atomicOps s (AtomicUMax Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_umax" [C.cty|typename uint64_t|]
    atomicOps s (AtomicUMin Int64 old arr ind val) =
      doAtomic s Int64 old arr ind val "atomic_umin" [C.cty|typename uint64_t|]
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
    -- 32 bit operations
    atomicOps s (AtomicAdd Int32 old arr ind val) =
      doAtomic s Int32 old arr ind val "atomic_add" [C.cty|int|]
    atomicOps s (AtomicFAdd Float32 old arr ind val) =
      doAtomic s Float32 old arr ind val "atomic_fadd" [C.cty|float|]
    atomicOps s (AtomicSMax Int32 old arr ind val) =
      doAtomic s Int32 old arr ind val "atomic_smax" [C.cty|int|]
    atomicOps s (AtomicSMin Int32 old arr ind val) =
      doAtomic s Int32 old arr ind val "atomic_smin" [C.cty|int|]
    atomicOps s (AtomicUMax Int32 old arr ind val) =
      doAtomic s Int32 old arr ind val "atomic_umax" [C.cty|unsigned int|]
    atomicOps s (AtomicUMin Int32 old arr ind val) =
      doAtomic s Int32 old arr ind val "atomic_umin" [C.cty|unsigned int|]
    atomicOps s (AtomicAnd Int32 old arr ind val) =
      doAtomic s Int32 old arr ind val "atomic_and" [C.cty|int|]
    atomicOps s (AtomicOr Int32 old arr ind val) =
      doAtomic s Int32 old arr ind val "atomic_or" [C.cty|int|]
    atomicOps s (AtomicXor Int32 old arr ind val) =
      doAtomic s Int32 old arr ind val "atomic_xor" [C.cty|int|]
    atomicOps s (AtomicCmpXchg (IntType Int32) old arr ind cmp val) =
      doAtomicCmpXchg s Int32 old arr ind cmp val [C.cty|int|]
    atomicOps s (AtomicXchg (IntType Int32) old arr ind val) =
      doAtomicXchg s Int32 old arr ind val [C.cty|int|]
    -- 16 bit operations
    atomicOps s (AtomicAdd Int16 old arr ind val) =
      doAtomic s Int16 old arr ind val "atomic_add" [C.cty|typename int16_t|]
    atomicOps s (AtomicFAdd Float16 old arr ind val) =
      doAtomic s Float16 old arr ind val "atomic_fadd" [C.cty|typename uint16_t|]
    atomicOps s (AtomicSMax Int16 old arr ind val) =
      doAtomic s Int16 old arr ind val "atomic_smax" [C.cty|typename int16_t|]
    atomicOps s (AtomicSMin Int16 old arr ind val) =
      doAtomic s Int16 old arr ind val "atomic_smin" [C.cty|typename int16_t|]
    atomicOps s (AtomicUMax Int16 old arr ind val) =
      doAtomic s Int16 old arr ind val "atomic_umax" [C.cty|typename uint16_t|]
    atomicOps s (AtomicUMin Int16 old arr ind val) =
      doAtomic s Int16 old arr ind val "atomic_umin" [C.cty|typename uint16_t|]
    atomicOps s (AtomicAnd Int16 old arr ind val) =
      doAtomic s Int16 old arr ind val "atomic_and" [C.cty|typename int16_t|]
    atomicOps s (AtomicOr Int16 old arr ind val) =
      doAtomic s Int16 old arr ind val "atomic_or" [C.cty|typename int16_t|]
    atomicOps s (AtomicXor Int16 old arr ind val) =
      doAtomic s Int16 old arr ind val "atomic_xor" [C.cty|typename int16_t|]
    atomicOps s (AtomicCmpXchg (IntType Int16) old arr ind cmp val) =
      doAtomicCmpXchg s Int16 old arr ind cmp val [C.cty|typename int16_t|]
    atomicOps s (AtomicXchg (IntType Int16) old arr ind val) =
      doAtomicXchg s Int16 old arr ind val [C.cty|typename int16_t|]
    -- 8 bit operations
    atomicOps s (AtomicAdd Int8 old arr ind val) =
      doAtomic s Int8 old arr ind val "atomic_add" [C.cty|typename int8_t|]
    atomicOps s (AtomicSMax Int8 old arr ind val) =
      doAtomic s Int8 old arr ind val "atomic_smax" [C.cty|typename int8_t|]
    atomicOps s (AtomicSMin Int8 old arr ind val) =
      doAtomic s Int8 old arr ind val "atomic_smin" [C.cty|typename int8_t|]
    atomicOps s (AtomicUMax Int8 old arr ind val) =
      doAtomic s Int8 old arr ind val "atomic_umax" [C.cty|typename uint8_t|]
    atomicOps s (AtomicUMin Int8 old arr ind val) =
      doAtomic s Int8 old arr ind val "atomic_umin" [C.cty|typename uint8_t|]
    atomicOps s (AtomicAnd Int8 old arr ind val) =
      doAtomic s Int8 old arr ind val "atomic_and" [C.cty|typename int8_t|]
    atomicOps s (AtomicOr Int8 old arr ind val) =
      doAtomic s Int8 old arr ind val "atomic_or" [C.cty|typename int8_t|]
    atomicOps s (AtomicXor Int8 old arr ind val) =
      doAtomic s Int8 old arr ind val "atomic_xor" [C.cty|typename int8_t|]
    atomicOps s (AtomicCmpXchg (IntType Int8) old arr ind cmp val) =
      doAtomicCmpXchg s Int8 old arr ind cmp val [C.cty|typename int8_t|]
    atomicOps s (AtomicXchg (IntType Int8) old arr ind val) =
      doAtomicXchg s Int8 old arr ind val [C.cty|typename int8_t|]
    -- General
    atomicOps s (AtomicWrite t arr ind val) = do
      ind' <- GC.compileExp $ untyped $ unCount ind
      val' <- toStorage t <$> GC.compileExp val
      let quals = case s of
            Space sid -> pointerQuals sid
            _ -> pointerQuals "global"
      GC.stm [C.cstm|(($tyquals:quals $ty:(primStorageType t)*)$id:arr)[$exp:ind'] = $exp:val';|]
      GC.stm $
        case s of
          Space "shared" -> [C.cstm|mem_fence_local();|]
          _ -> [C.cstm|mem_fence_global();|]
    atomicOps _ op =
      error $ "atomicOp: unsupported " <> show op

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

-- | The type of the value updated by a read-modify-write atomic
-- operation.
atomicType :: AtomicOp -> Maybe PrimType
atomicType (AtomicAdd t _ _ _ _) = Just $ IntType t
atomicType (AtomicFAdd t _ _ _ _) = Just $ FloatType t
atomicType (AtomicSMax t _ _ _ _) = Just $ IntType t
atomicType (AtomicSMin t _ _ _ _) = Just $ IntType t
atomicType (AtomicUMax t _ _ _ _) = Just $ IntType t
atomicType (AtomicUMin t _ _ _ _) = Just $ IntType t
atomicType (AtomicAnd t _ _ _ _) = Just $ IntType t
atomicType (AtomicOr t _ _ _ _) = Just $ IntType t
atomicType (AtomicXor t _ _ _ _) = Just $ IntType t
atomicType (AtomicCmpXchg t _ _ _ _ _) = Just t
atomicType (AtomicXchg t _ _ _ _) = Just t
atomicType AtomicWrite {} = Nothing

--- Metal

-- | Metal kernels take their arguments as a struct in buffer 0 (see
-- @FUTHARK_KERNEL_PARAMS@ in @prelude.metal@), which we unpack into
-- local variables of the same names as the OpenCL kernel parameters.
-- The MSL attributes on the kernel parameters cannot be expressed
-- with language-c-quote, so we print the function header ourselves.
metalKernel :: Name -> T.Text -> [C.Param] -> [C.BlockItem] -> T.Text
metalKernel name attribute params body =
  definitionsText [[C.cedecl|struct $id:args_struct { $sdecls:fields };|]]
    <> attribute
    <> ("void " <> idText (C.toIdent name mempty))
    <> ("(FUTHARK_KERNEL_PARAMS(" <> args_struct <> "))\n")
    <> stmText [C.cstm|{ $items:unpack $items:body }|]
  where
    args_struct = idText (C.toIdent name mempty) <> "_args"
    (fields, unpack) = unzip $ map field params
    field (C.Param (Just v) spec decl loc) =
      let t = C.Type spec decl loc
       in ( [C.csdecl|$ty:t $id:v;|],
            [C.citem|$ty:t $id:v = futhark_args->$id:v;|]
          )
    field p = error $ "metalKernel: unnamed parameter " <> show p

-- | Metal requires an address space on every pointer, and does not
-- support @goto@ (and hence labels).  The only unqualified pointers
-- in device functions are the parameters for return values, which
-- point to private memory.
metalDeviceFun :: (C.Definition, C.Func) -> (C.Definition, C.Func)
metalDeviceFun
  ( C.DecDef (C.InitGroup dspec attrs [C.Init v (C.Proto d ps ploc) asm ini iattrs iloc] gloc) dloc,
    C.Func fspec fv fd fps body floc
    )
    | any (itemAny isGoto) body =
        compilerLimitationS "Metal does not support failing GPU functions with jumps."
    | otherwise =
        ( C.DecDef (C.InitGroup dspec attrs [C.Init v (C.Proto d (onParams ps) ploc) asm ini iattrs iloc] gloc) dloc,
          C.Func fspec fv fd (onParams fps) (map unlabel body) floc
        )
    where
      onParams (C.Params ps' variadic loc) = C.Params (map onParam ps') variadic loc
      onParam (C.Param p (C.DeclSpec st quals ts sloc) decl@C.Ptr {} loc)
        | not $ any isAddressSpace quals =
            C.Param p (C.DeclSpec st (C.TCLprivate mempty : quals) ts sloc) decl loc
      onParam p = p
      isAddressSpace C.TCLprivate {} = True
      isAddressSpace C.TCLlocal {} = True
      isAddressSpace C.TCLglobal {} = True
      isAddressSpace C.TCLconstant {} = True
      isAddressSpace _ = False
      unlabel (C.BlockStm (C.Label _ _ s _)) = C.BlockStm s
      unlabel item = item
metalDeviceFun _ = error "metalDeviceFun: unexpected function definition."

-- | Declare the flag used by 'eliminateGotos', if necessary.
metalFailureFlag :: [C.BlockItem] -> [C.BlockItem]
metalFailureFlag body
  | any (itemAny isGoto) body = [C.citems|bool futhark_failed = false;|]
  | otherwise = mempty

-- | In a kernel with barriers, a failing thread jumps to the next
-- 'ErrorSync', where all threads of the block check whether one of
-- them failed (see 'errorLabel').  Metal has no @goto@, so we
-- eliminate the jumps from the kernel body: a failing thread instead
-- sets the private flag @futhark_failed@, which guards all following
-- statements up to the next synchronisation point (the statement with
-- a label), and which breaks out of any loop that does not contain a
-- synchronisation point.  The synchronisation points themselves are
-- always executed, so the failing thread still takes part in the
-- barrier, after which all threads of the block return.
eliminateGotos :: [C.BlockItem] -> [C.BlockItem]
eliminateGotos = onItems False False
  where
    -- The first argument is whether these items are the body of a
    -- loop that contains no synchronisation point, such that a
    -- failure must break out of it.  The second is whether a failure
    -- may have happened since the last synchronisation point.
    onItems _ _ [] = []
    onItems loop pending items@(x : xs)
      | C.BlockStm (C.Label _ _ s _) <- x =
          C.BlockStm (onStm pending s) : onItems loop False xs
      | itemAny isLabel x =
          onItem pending x : onItems loop (pending || itemAny isGoto x) xs
      | pending =
          let (guarded, rest) = break (itemAny isLabel) items
              -- Declarations must remain visible after the guard.
              (decls, guarded')
                | null rest = ([], guarded)
                | otherwise = mconcat $ map hoistDecl guarded
           in decls
                ++ [C.citem|if (!futhark_failed) { $items:(onItems False False guarded') }|]
                : onItems loop True rest
      | itemAny isGoto x =
          onItem False x
            : [[C.citem|if (futhark_failed) { break; }|] | loop]
            ++ onItems loop (not loop) xs
      | otherwise = x : onItems loop False xs

    onItem pending (C.BlockStm s) = C.BlockStm $ onStm pending s
    onItem _ item = item

    onStm _ (C.Goto _ _) =
      [C.cstm|futhark_failed = true;|]
    onStm pending (C.Label _ _ s _) =
      onStm pending s
    onStm pending (C.Block items loc) =
      C.Block (onItems False pending items) loc
    onStm pending (C.If e t f loc) =
      C.If e (onStm pending t) (onStm pending <$> f) loc
    onStm pending (C.Comment c s loc) =
      C.Comment c (onStm pending s) loc
    onStm pending (C.For ini cond inc s loc) =
      C.For ini cond inc (onLoopBody pending s) loc
    onStm pending (C.While e s loc) =
      C.While e (onLoopBody pending s) loc
    onStm pending (C.DoWhile s e loc) =
      C.DoWhile (onLoopBody pending s) e loc
    onStm _ s = s

    -- A synchronisation point in a loop body may be reached from a
    -- failure in a previous iteration.
    onLoopBody pending (C.Block items loc)
      | any (itemAny isLabel) items =
          C.Block (onItems False (pending || any (itemAny isGoto) items) items) loc
      | otherwise =
          C.Block (onItems True False items) loc
    onLoopBody pending s =
      onLoopBody pending $ C.Block [C.BlockStm s] mempty

    -- Split a declaration into an uninitialised declaration and
    -- assignments.
    hoistDecl (C.BlockDecl (C.InitGroup (C.DeclSpec st quals ts sloc) attrs inits loc)) =
      ( [ C.BlockDecl $
            C.InitGroup
              (C.DeclSpec st (filter (not . isConst) quals) ts sloc)
              attrs
              [C.Init v d asm Nothing iattrs iloc | C.Init v d asm _ iattrs iloc <- inits]
              loc
        ],
        mapMaybe assign inits
      )
    hoistDecl item = ([], [item])
    assign (C.Init v _ _ (Just (C.ExpInitializer e _)) _ _) =
      Just [C.citem|$id:v = $exp:e;|]
    assign (C.Init _ _ _ Nothing _ _) =
      Nothing
    assign _ =
      compilerLimitationS "eliminateGotos: cannot hoist initialiser."
    isConst C.Tconst {} = True
    isConst _ = False

isGoto, isLabel :: C.Stm -> Bool
isGoto C.Goto {} = True
isGoto _ = False
isLabel C.Label {} = True
isLabel _ = False

-- | Does any statement in this item satisfy the predicate?
itemAny :: (C.Stm -> Bool) -> C.BlockItem -> Bool
itemAny p (C.BlockStm s) = stmAny p s
itemAny _ _ = False

-- | Does this statement, or any statement nested in it, satisfy the
-- predicate?
stmAny :: (C.Stm -> Bool) -> C.Stm -> Bool
stmAny p s = p s || any (stmAny p) (children s)
  where
    children (C.Label _ _ s' _) = [s']
    children (C.Case _ s' _) = [s']
    children (C.Default s' _) = [s']
    children (C.Block items _) = [s' | C.BlockStm s' <- items]
    children (C.If _ t f _) = t : maybeToList f
    children (C.Switch _ s' _) = [s']
    children (C.While _ s' _) = [s']
    children (C.DoWhile s' _ _) = [s']
    children (C.For _ _ _ s' _) = [s']
    children (C.Comment _ s' _) = [s']
    children _ = []

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
typesInCode (Copy t shape _ (Count (TPrimExp dstoffset), dststrides) _ (Count (TPrimExp srcoffset), srcstrides)) =
  S.singleton t
    <> foldMap (typesInExp . untyped . unCount) shape
    <> typesInExp dstoffset
    <> foldMap (typesInExp . untyped . unCount) dststrides
    <> typesInExp srcoffset
    <> foldMap (typesInExp . untyped . unCount) srcstrides
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
typesInCode (Meta _) = mempty
typesInCode (DebugPrint _ v) = maybe mempty typesInExp v
typesInCode (TracePrint msg) = foldMap typesInExp msg
typesInCode (GetUserParam _ _ def) = typesInExp (untyped def)
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
