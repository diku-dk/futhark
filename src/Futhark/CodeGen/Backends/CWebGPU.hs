{-# LANGUAGE QuasiQuotes #-}

-- | Code generation for WebGPU.
module Futhark.CodeGen.Backends.CWebGPU
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
    asJSServer,
  )
where

import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GPU
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Pretty (idText)
import Futhark.CodeGen.ImpCode.WebGPU
import Futhark.CodeGen.ImpGen.WebGPU qualified as ImpGen
import Futhark.CodeGen.RTS.C (backendsWebGPUH)
import Futhark.CodeGen.RTS.WebGPU (serverWsJs, utilJs, valuesJs, wrappersJs)
import Futhark.IR.GPUMem (GPUMem, Prog)
import Futhark.MonadFreshNames
import Language.C.Quote.C qualified as C
import NeatInterpolation (text, untrimming)

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
               typename size_t num_overrides;
               char **used_overrides;
               typename size_t num_dynamic_block_dims;
               typename uint32_t *dynamic_block_dim_indices;
               char **dynamic_block_dim_names;
               typename uint32_t num_shared_mem_overrides;
               char **shared_mem_overrides;
             } wgpu_kernel_info;
             static typename size_t wgpu_num_kernel_infos = $exp:num_kernels; |]
  mapM_ GC.earlyDecl $ concatMap sc_offs_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap bind_idxs_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap used_overrides_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap dynamic_block_dim_indices_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap dynamic_block_dim_names_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap shared_mem_overrides_decl (M.toList kernels)
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
    used_overrides_decl (n, k) =
      let overrides =
            map
              (\o -> [C.cinit|$string:(T.unpack o)|])
              (overrideNames k)
       in [C.cunit|static char* $id:(n <> "_used_overrides")[]
                    = {$inits:overrides};|]
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
    info_init (n, k) =
      let num_scalars = length (scalarsOffsets k)
          num_bindings = length (memBindSlots k)
          num_overrides = length (overrideNames k)
          num_dynamic_block_dims = length (dynamicBlockDims k)
          num_shared_mem_overrides = length (sharedMemoryOverrides k)
       in [C.cinit|{ .name = $string:(T.unpack (idText (C.toIdent n mempty))),
                     .num_scalars = $int:num_scalars,
                     .scalars_binding = $int:(scalarsBindSlot k),
                     .scalars_size = $int:(scalarsSize k),
                     .scalar_offsets = $id:(n <> "_scalar_offsets"),
                     .num_bindings = $int:num_bindings,
                     .binding_indices = $id:(n <> "_binding_indices"),
                     .num_overrides = $int:num_overrides,
                     .used_overrides = $id:(n <> "_used_overrides"),
                     .num_dynamic_block_dims = $int:num_dynamic_block_dims,
                     .dynamic_block_dim_indices =
                        $id:(n <> "_dynamic_block_dim_indices"),
                     .dynamic_block_dim_names =
                        $id:(n <> "_dynamic_block_dim_names"),
                     .num_shared_mem_overrides = $int:num_shared_mem_overrides,
                     .shared_mem_overrides =
                        $id:(n <> "_shared_mem_overrides"),
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

jsBoilerplate :: Definitions a -> T.Text -> (T.Text, [T.Text])
jsBoilerplate prog manifest =
  let (context, exports) = mkJsContext prog manifest
      prelude = T.intercalate "\n" [utilJs, valuesJs, wrappersJs]
   in (prelude <> "\n" <> context, exports ++ builtinExports)
  where
    builtinExports =
      [ "malloc",
        "free",
        "futhark_context_config_new",
        "futhark_context_new",
        "futhark_context_config_free",
        "futhark_context_free",
        "futhark_context_sync",
        "futhark_context_clear_caches",
        "futhark_context_report",
        "futhark_context_pause_profiling",
        "futhark_context_unpause_profiling"
      ]

-- Argument should be a direct function call to a WASM function that needs to
-- be handled asynchronously. The return value evaluates to a Promise yielding
-- the function result when awaited.
-- Can currently only be used for code generated into the FutharkModule class.
asyncCall :: T.Text -> Bool -> [T.Text] -> T.Text
asyncCall func hasReturn args =
  [text|this.m.ccall('${func}', ${ret}, ${argTypes}, ${argList}, {async: true})|]
  where
    ret = if hasReturn then "'number'" else "null"
    argTypes =
      "[" <> T.intercalate ", " (replicate (length args) "'number'") <> "]"
    argList =
      "[" <> T.intercalate ", " args <> "]"

mkJsContext :: Definitions a -> T.Text -> (T.Text, [T.Text])
mkJsContext (Definitions _ _ (Functions funs)) manifest =
  ( [text|
   class FutharkModule {
     ${constructor}
     ${free}
     ${builtins}
   }|],
    entryExports ++ valueExports
  )
  where
    constructor =
      [text|
      constructor() {
        this.m = undefined;
        this.manifest = ${manifest};
      }
      async init(module) {
        this.m = module;
        this.cfg = this.m._futhark_context_config_new();
        this.ctx = await ${newContext};
        this.available_entry_points = {};
        this.types = {};
        ${valueClasses}
        ${entryPointFuns}
      }|]
    newContext = asyncCall "futhark_context_new" True ["this.cfg"]
    free =
      [text|
      free() {
        this.m._futhark_context_free(this.ctx);
        this.m._futhark_context_config_free(this.cfg);
      }|]
    entryPoints = mapMaybe (functionEntry . snd) funs
    (entryPointFuns, entryExports) = mkJsEntryPoints entryPoints
    (valueClasses, valueExports) = mkJsValueClasses entryPoints
    builtins =
      [text|
      malloc(nbytes) {
        return this.m._malloc(nbytes);
      }
      free(ptr) {
        return this.m._free(ptr);
      }
      async context_sync() {
        return await ${syncCall};
      }
      async clear_caches() {
        return await ${clearCall};
      }
      async report() {
        return await this.m.ccall('futhark_context_report', 'string',
          ['number'], [this.ctx], {async: true});
      }
      async pause_profiling() {
        return await ${pauseProfilingCall};
      }
      async unpause_profiling() {
        return await ${unpauseProfilingCall};
      }
      |]
    syncCall = asyncCall "futhark_context_sync" True ["this.ctx"]
    clearCall = asyncCall "futhark_context_clear_caches" True ["this.ctx"]
    pauseProfilingCall =
      asyncCall "futhark_context_pause_profiling" False ["this.ctx"]
    unpauseProfilingCall =
      asyncCall "futhark_context_unpause_profiling" False ["this.ctx"]

mkJsEntryPoints :: [EntryPoint] -> (T.Text, [T.Text])
mkJsEntryPoints entries = (T.intercalate "\n" entryFuns, entryExports)
  where
    entryNames = map (nameToText . entryPointName) entries
    entryFuns = map entryFun entryNames
    entryExports = map entryExport entryNames
    entryFun name =
      [text|this.entry_${name} = make_entry_function(this, '${name}');
            this.available_entry_points['${name}'] = this.entry_${name}.bind(this);|]
    entryExport name = "futhark_entry_" <> name

mkJsValueClasses :: [EntryPoint] -> (T.Text, [T.Text])
mkJsValueClasses entries =
  -- TODO: Only supports transparent arrays right now.
  let extVals =
        concatMap (map snd . entryPointResults) entries
          ++ concatMap (map snd . entryPointArgs) entries
      transpVals = [v | TransparentValue v <- extVals]
      arrVals =
        S.toList $
          S.fromList
            [(typ, sgn, shp) | ArrayValue _ _ typ sgn shp <- transpVals]
      (cls, exports) = unzip $ map mkJsArrayClass arrVals
   in (T.intercalate "\n" cls, concat exports)

mkJsArrayClass :: (PrimType, Signedness, [DimSize]) -> (T.Text, [T.Text])
mkJsArrayClass (typ, sign, shp) =
  ( [text|
    this.${name} = make_array_class(this, '${prettyName}');
    this.types['${prettyName}'] = this.${name};
  |],
    exports
  )
  where
    rank = length shp
    elemName = prettySigned (sign == Unsigned) typ
    prettyName = mconcat (replicate rank "[]") <> elemName
    name = elemName <> "_" <> prettyText rank <> "d"
    exports =
      [ "futhark_new_" <> name,
        "futhark_free_" <> name,
        "futhark_values_" <> name,
        "futhark_shape_" <> name
      ]

-- | Compile the program to C with calls to WebGPU, along with a JS wrapper
-- library.
compileProg ::
  (MonadFreshNames m) =>
  T.Text ->
  Prog GPUMem ->
  m (ImpGen.Warnings, (GC.CParts, T.Text, [T.Text]))
compileProg version prog = do
  ( ws,
    Program wgsl_code wgsl_prelude macros kernels params failures prog'
    ) <-
    ImpGen.compileProg prog
  c <-
    GC.compileProg
      "webgpu"
      version
      params
      operations
      (mkBoilerplate (wgsl_prelude <> wgsl_code) macros kernels [] failures)
      webgpu_includes
      (Space "device", [Space "device", DefaultSpace])
      cliOptions
      prog'
  let (js, exports) = jsBoilerplate prog' (GC.cJsonManifest c)
  pure (ws, (c, js, exports))
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
         #include <emscripten/html5_webgpu.h>
       #endif
      |]

-- | As server script. Speaks custom protocol to local Python server
-- wrapper that speaks the actual Futhark server protocol.
asJSServer :: GC.CParts -> (T.Text, T.Text)
asJSServer parts =
  let (_, c, _) = GC.asLibrary parts
   in (c, serverWsJs)
