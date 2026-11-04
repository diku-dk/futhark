{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for WebGPU.
module Futhark.CodeGen.Backends.CWebGPU
  ( compileProg,
    runServer,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GPU
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Pretty (idText)
import Futhark.CodeGen.Backends.GenericWASM (emccExportNames, javascriptWrapper)
import Futhark.CodeGen.ImpCode.WebGPU
import Futhark.CodeGen.ImpGen.WebGPU qualified as ImpGen
import Futhark.CodeGen.RTS.C (backendsWebGPUH)
import Futhark.CodeGen.RTS.WGSL qualified as RTS
import Futhark.CodeGen.RTS.WebGPU (serverWsJs)
import Futhark.IR.GPUMem (GPUMem, Prog)
import Futhark.MonadFreshNames
import Futhark.Util (chunk)
import Language.C.Quote.C qualified as C
import NeatInterpolation (untrimming)

mkKernelInfos :: M.Map Name KernelInterface -> GC.CompilerM HostOp () ()
mkKernelInfos kernels = do
  mapM_
    GC.earlyDecl
    [C.cunit|typedef struct wgpu_kernel_info {
               char *name;
               typename size_t num_scalars;
               typename size_t scalars_binding;
               typename size_t scalars_size;
               typename size_t *scalar_offsets;
               typename size_t num_bindings;
               typename uint32_t *binding_indices;
               typename size_t num_dynamic_block_dims;
               typename uint32_t *dynamic_block_dim_indices;
               char **dynamic_block_dim_names;
               typename uint32_t num_shared_mem_overrides;
               char **shared_mem_overrides;
               const char **gpu_program;
             } wgpu_kernel_info;
             static typename size_t wgpu_num_kernel_infos = $exp:num_kernels; |]
  mapM_ GC.earlyDecl $ concatMap sc_offs_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap bind_idxs_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap dynamic_block_dim_indices_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap dynamic_block_dim_names_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap shared_mem_overrides_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap gpu_programs (M.toList kernels)
  mapM_
    GC.earlyDecl
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
    dynamic_block_dim_indices_decl (n, k) =
      let idxs = map ((\i -> [C.cinit|$int:i|]) . fst) (dynamicBlockDims k)
       in [C.cunit|static typename uint32_t $id:(n <> "_dynamic_block_dim_indices")[]
                    = {$inits:idxs};|]
    dynamic_block_dim_names_decl (n, k) =
      let names =
            map
              ((\d -> [C.cinit|$string:(T.unpack d)|]) . snd)
              (dynamicBlockDims k)
       in [C.cunit|static char* $id:(n <> "_dynamic_block_dim_names")[]
                    = {$inits:names};|]
    shared_mem_overrides_decl (n, k) =
      let names =
            map
              (\d -> [C.cinit|$string:(T.unpack d)|])
              (sharedMemoryOverrides k)
       in [C.cunit|static char* $id:(n <> "_shared_mem_overrides")[]
                    = {$inits:names};|]
    gpu_programs (n, k) =
      let program_fragments = [[C.cinit|$string:s|] | s <- chunk 2000 $ T.unpack $ gpuProgram k]
       in [C.cunit|static const char* $id:(n <> "_gpu_program")[]
                    = {$inits:program_fragments, NULL};|]

    info_init (n, k) =
      let num_scalars = length (scalarsOffsets k)
          num_bindings = length (memBindSlots k)
          num_dynamic_block_dims = length (dynamicBlockDims k)
          num_shared_mem_overrides = length (sharedMemoryOverrides k)
       in [C.cinit|{ .name = $string:(T.unpack (idText (C.toIdent n mempty))),
                     .num_scalars = $int:num_scalars,
                     .scalars_binding = $int:(scalarsBindSlot k),
                     .scalars_size = $int:(scalarsSize k),
                     .scalar_offsets = $id:(n <> "_scalar_offsets"),
                     .num_bindings = $int:num_bindings,
                     .binding_indices = $id:(n <> "_binding_indices"),
                     .num_dynamic_block_dims = $int:num_dynamic_block_dims,
                     .dynamic_block_dim_indices =
                        $id:(n <> "_dynamic_block_dim_indices"),
                     .dynamic_block_dim_names =
                        $id:(n <> "_dynamic_block_dim_names"),
                     .num_shared_mem_overrides = $int:num_shared_mem_overrides,
                     .shared_mem_overrides =
                        $id:(n <> "_shared_mem_overrides"),
                     .gpu_program = $id:(n <> "_gpu_program")
                   }|]
    info_inits = map info_init (M.toList kernels)

-- We need to generate kernel_infos for built-in kernels as well.
builtinKernels :: M.Map Name KernelInterface
builtinKernels =
  M.fromList $ concatMap generateKernels builtinKernelTemplates
  where
    builtinKernelTemplates =
      [ ("lmad_copy_NAME", copyScalars, RTS.lmadCopy),
        ("map_transpose_NAME", transposeScalars, RTS.mapTranspose),
        ("map_transpose_NAME_low_height", transposeScalars, RTS.mapTransposeLowHeight),
        ("map_transpose_NAME_low_width", transposeScalars, RTS.mapTransposeLowWidth),
        ("map_transpose_NAME_small", transposeScalars, RTS.mapTransposeSmall),
        ("map_transpose_NAME_large", transposeLargeScalars, RTS.mapTransposeLarge)
      ]

    -- The offsets of the fields of the scalars struct, and its size, which
    -- must be a multiple of 16 bytes for a uniform buffer.
    transposeScalars = ([0, 8, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56], 64)
    transposeLargeScalars = ([0, 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 92], 96)
    -- The 'r' field is aligned to 8 bytes despite being an i32, due to uniform
    -- buffer alignment requirements.
    copyScalars = ([0, 8 .. 216], 224)

    generateKernelProgram kernel name elemType atomic =
      let baseKernel =
            if atomic
              then T.replace "<ELEM_TYPE>" "<atomic<ELEM_TYPE>>" kernel
              else kernel
       in RTS.wgslPrelude
            <> T.replace "NAME" name (T.replace "ELEM_TYPE" elemType baseKernel)

    generateKernels (template, (offsets, size), program) =
      [ ( nameFromText (T.replace "NAME" name template),
          KernelInterface
            { safety = SafetyNone,
              scalarsOffsets = offsets,
              scalarsSize = size,
              scalarsBindSlot = 0,
              memBindSlots = [1, 2],
              dynamicBlockDims =
                [ (0, "block_size_x"),
                  (1, "block_size_y"),
                  (2, "block_size_z")
                ],
              sharedMemoryOverrides = [],
              gpuProgram = generateKernelProgram program name elemType atomic
            }
        )
      | (name, elemType, atomic) <-
          [ ("1b", "i8", True),
            ("2b", "i16", True),
            ("4b", "i32", False),
            ("8b", "i64", False)
          ]
      ]

mkBoilerplate ::
  T.Text ->
  [(Name, KernelConstExp)] ->
  M.Map Name KernelInterface ->
  [PrimType] ->
  [FailureMsg] ->
  GC.CompilerM HostOp () ()
mkBoilerplate wgsl_program macros kernels types failures = do
  mkKernelInfos (M.union kernels builtinKernels)
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

-- | Compile the program to C with calls to WebGPU, along with a JS wrapper
-- library.
compileProg ::
  (MonadFreshNames m) =>
  T.Text ->
  Prog GPUMem ->
  m (ImpGen.Warnings, (GC.CParts, T.Text, [String]))
compileProg version prog = do
  ( ws,
    Program wgsl_code wgsl_prelude macros kernels failures prog'
    ) <-
    ImpGen.compileProg prog
  c <-
    GC.compileProg
      "webgpu"
      version
      operations
      (mkBoilerplate (wgsl_prelude <> wgsl_code) macros kernels [] failures)
      webgpu_includes
      (Space "device", [Space "device", DefaultSpace])
      cliOptions
      prog'
  let manifest = GC.cManifest c
  pure (ws, (c, javascriptWrapper manifest, emccExportNames manifest))
  where
    operations :: GC.Operations HostOp ()
    operations =
      gpuOperations
        { GC.opsMemoryType = webgpuMemoryType
        }
    webgpu_includes =
      [untrimming|
       #ifdef USE_DAWN
         #include <dawn/webgpu.h>
       #else
         #include <webgpu/webgpu.h>
         #include <emscripten.h>
       #endif
      |]

-- | JavaScript that runs a server, in Node or in a browser.
runServer :: T.Text
runServer = serverWsJs <> "\nawait futhark_webgpu_server_main();\n"
