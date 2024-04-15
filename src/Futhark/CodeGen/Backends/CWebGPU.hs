{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for WebGPU.
module Futhark.CodeGen.Backends.CWebGPU
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
  )
where

import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GPU
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Pretty (idText)
import Futhark.CodeGen.ImpCode.WebGPU
import Futhark.CodeGen.ImpGen.WebGPU qualified as ImpGen
import Futhark.CodeGen.RTS.C (backendsWebGPUH)
import Futhark.IR.GPUMem hiding
  ( HostOp,
    CmpSizeLe,
    GetSize,
    GetSizeMax,
  )
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeText)
import Language.C.Quote.C qualified as C
import NeatInterpolation (untrimming)

mkKernelInfos :: M.Map Name KernelInterface -> GC.CompilerM HostOp () ()
mkKernelInfos kernels = do
  mapM_ GC.earlyDecl
    [C.cunit|typedef struct wgpu_kernel_info {
               char *name;
               typename size_t num_scalars;
               typename size_t scalars_binding;
               typename size_t scalars_size;
               typename size_t *scalar_offsets;
               typename size_t num_bindings;
               typename uint32_t *binding_indices;
             } wgpu_kernel_info;
             static typename size_t wgpu_num_kernel_infos = $exp:num_kernels; |]
  mapM_ GC.earlyDecl $ concatMap sc_offs_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap bind_idxs_decl (M.toList kernels)
  mapM_ GC.earlyDecl
    [C.cunit|static struct wgpu_kernel_info wgpu_kernel_infos[]
               = {$inits:info_inits};|]
  where
    num_kernels = M.size kernels
    sc_offs_decl (n, k) =
      let offs = map (\o -> [C.cinit|$int:o|]) (scalarsOffsets k)
       in [C.cunit|static typename size_t $id:(n <> "_scalar_offsets")[] 
                     = {$inits:offs};|]
    bind_idxs_decl (n, k) =
      let idxs = map (\i -> [C.cinit|$int:i|]) (memBindSlots k)
       in [C.cunit|static typename uint32_t $id:(n <> "_binding_indices")[]
                     = {$inits:idxs};|]
    info_init (n, k) =
      let num_scalars = length (scalarsOffsets k)
          num_bindings = length (memBindSlots k)
       in [C.cinit|{ .name = $string:(T.unpack (idText (C.toIdent n mempty))),
                     .num_scalars = $int:num_scalars,
                     .scalars_binding = $int:(scalarsBindSlot k),
                     .scalars_size = $int:(scalarsSize k),
                     .scalar_offsets = $id:(n <> "_scalar_offsets"),
                     .num_bindings = $int:num_bindings,
                     .binding_indices = $id:(n <> "_binding_indices"),
                   }|]
    info_inits = map info_init (M.toList kernels)

mkBoilerplate ::
  T.Text ->
  [(Name, KernelConstExp)] ->
  M.Map Name KernelInterface ->
  [PrimType] ->
  [FailureMsg] ->
  GC.CompilerM HostOp () ()
mkBoilerplate wgsl_program macros kernels types failures = do
  mkKernelInfos kernels
  generateGPUBoilerplate
    wgsl_program
    macros
    backendsWebGPUH
    (M.keys kernels)
    types
    failures

  GC.headerDecl GC.InitDecl [C.cedecl|const char* futhark_context_config_get_program(struct futhark_context_config *cfg);|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_program(struct futhark_context_config *cfg, const char* s);|]

-- TODO: Check GPU.gpuOptions and see which of these make sense for us to
-- support.
cliOptions :: [Option]
cliOptions = []

webgpuMemoryType :: GC.MemoryType HostOp ()
webgpuMemoryType "device" = pure [C.cty|typename WGPUBuffer|]
webgpuMemoryType space = error $ "WebGPU backend does not support '" ++ space ++ "' memory space."

-- | Compile the program to C with calls to WebGPU.
compileProg :: (MonadFreshNames m) => T.Text -> Prog GPUMem
            -> m (ImpGen.Warnings, (GC.CParts, [T.Text]))
compileProg version prog = do
  ( ws,
    Program wgsl_code wgsl_prelude macros kernels params failures prog'
    ) <- ImpGen.compileProg prog
  (ws,) . (,entryPointExports prog') <$> GC.compileProg
      "webgpu"
      version
      params
      operations
      (mkBoilerplate (wgsl_prelude <> wgsl_code) macros kernels [] failures)
      webgpu_includes
      (Space "device", [Space "device", DefaultSpace])
      cliOptions
      prog'
  where
    entryPointExports prog' =
      let Functions fs = defFuns prog'
          entry (Function (Just (EntryPoint e _ _)) _ _ _) = Just $ 
            "futhark_entry_" <> nameToText e
          entry (Function Nothing _ _ _) = Nothing
       in mapMaybe (entry . snd) fs
      --map (zEncodeText . nameToText . ("_gpu_kernel_" <>)) (M.keys ks)
    operations :: GC.Operations HostOp ()
    operations =
      gpuOperations
        { GC.opsMemoryType = webgpuMemoryType }
    webgpu_includes =
      [untrimming|
       #ifdef USE_DAWN
         #include <dawn/webgpu.h>
       #else
         #include <webgpu/webgpu.h>
         #include <emscripten.h>
         #include <emscripten/html5_webgpu.h>
       #endif
      |]
