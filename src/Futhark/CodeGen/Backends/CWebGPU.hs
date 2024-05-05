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
import Futhark.CodeGen.RTS.WebGPU (serverWsJs)
import Futhark.IR.GPUMem (Prog, GPUMem)
import Futhark.MonadFreshNames
import Language.C.Quote.C qualified as C
import NeatInterpolation (text, untrimming)
import Futhark.Util (showText)

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
               typename size_t num_overrides;
               char **used_overrides;
             } wgpu_kernel_info;
             static typename size_t wgpu_num_kernel_infos = $exp:num_kernels; |]
  mapM_ GC.earlyDecl $ concatMap sc_offs_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap bind_idxs_decl (M.toList kernels)
  mapM_ GC.earlyDecl $ concatMap used_overrides_decl (M.toList kernels)
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
    used_overrides_decl (n, k) =
      let overrides = map (\o -> [C.cinit|$string:(T.unpack o)|])
                        (overrideNames k)
       in [C.cunit|static char* $id:(n <> "_used_overrides")[]
                    = {$inits:overrides};|]
    info_init (n, k) =
      let num_scalars = length (scalarsOffsets k)
          num_bindings = length (memBindSlots k)
          num_overrides = length (overrideNames k)
       in [C.cinit|{ .name = $string:(T.unpack (idText (C.toIdent n mempty))),
                     .num_scalars = $int:num_scalars,
                     .scalars_binding = $int:(scalarsBindSlot k),
                     .scalars_size = $int:(scalarsSize k),
                     .scalar_offsets = $id:(n <> "_scalar_offsets"),
                     .num_bindings = $int:num_bindings,
                     .binding_indices = $id:(n <> "_binding_indices"),
                     .num_overrides = $int:num_overrides,
                     .used_overrides = $id:(n <> "_used_overrides"),
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
   in (context, exports ++ builtinExports)
  where
    builtinExports =
      ["malloc", "free",
       "futhark_context_config_new", "futhark_context_new",
       "futhark_context_config_free", "futhark_context_free",
       "futhark_context_sync", "futhark_context_clear_caches",
       "futhark_context_report", "futhark_context_pause_profiling",
       "futhark_context_unpause_profiling",
       "futhark_new_i32_1d", "futhark_free_i32_1d",
       "futhark_values_i32_1d", "futhark_shape_i32_1d"]

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
  ([text|
   class FutharkModule {
     ${constructor}
     ${free}
     ${entryPointFuns}
     ${builtins}
     ${valueFuns}
   }|], entryExports ++ valueExports)
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
        this.entry_points = {
          ${entryPointEntries}
        };
      }|]
    newContext = asyncCall "futhark_context_new" True ["this.cfg"]
    free =
      [text|
      free() {
        this.m._futhark_context_free(this.ctx);
        this.m._futhark_context_config_free(this.cfg);
      }|]
    entryPoints = mapMaybe (functionEntry . snd) funs
    jsEntryPoints = map mkJsEntryPoint entryPoints
    entryPointEntries = T.intercalate ",\n" $ map
      (\e -> let n = entryName e
                 f = entryFun e
              in [text|'${n}': this.${f}.bind(this)|]) jsEntryPoints
    entryPointFuns = T.intercalate "\n" $ map mkEntryFun jsEntryPoints
    entryExports = map entryInternalFun jsEntryPoints
    (valueFuns, valueExports) = mkJsValueFuns entryPoints
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

data JsEntryPoint = JsEntryPoint
  { entryName :: T.Text,
    entryFun :: T.Text,
    entryInternalFun :: T.Text,
    entryIn :: [T.Text],
    -- Currently only records size as we need that to allocate space for out
    -- parameters.
    entryOut :: [Int]
  }

mkJsEntryPoint :: EntryPoint -> JsEntryPoint
mkJsEntryPoint (EntryPoint name results args) = JsEntryPoint n fun ifun ins outs
  where
    n = nameToText name
    fun = "entry_" <> n
    ifun = "futhark_entry_" <> n
    ins = map (nameToText . fst . fst) args
    outs = map (valSize . snd) results
    -- TODO: Hardcoding sizeof(ptr) = 4 here seems not great
    valSize (OpaqueValue {}) = 4
    valSize (TransparentValue (ArrayValue {})) = 4
    valSize (TransparentValue (ScalarValue t _ _)) = primByteSize t

mkEntryFun :: JsEntryPoint -> T.Text
mkEntryFun e =
  [text|
  async ${fun}(${inputs}) {
    ${allocOuts}
    await ${internalCall};
    return [${outPtrs}];
  }
  |]
  where
    fun = entryFun e
    inputs = T.intercalate ", " (entryIn e)
    outNames = zipWith (\i _ -> "out" <> showText i) [(0::Int)..] (entryOut e)
    outSizes = map showText (entryOut e)
    outPtrs = T.intercalate ", " outNames
    allocOuts = T.intercalate "\n" $
      zipWith (\n sz -> [text|const ${n} = this.m._malloc(${sz});|])
        outNames outSizes
    internalCall = asyncCall
      (entryInternalFun e) True (["this.ctx"] ++ outNames ++ entryIn e)

mkJsValueFuns :: [EntryPoint] -> (T.Text, [T.Text])
mkJsValueFuns entries =
  -- TODO: Only supports transparent arrays right now.
  let extVals =
        concatMap (map snd . entryPointResults) entries
        ++ concatMap (map snd . entryPointArgs) entries
      transpVals = [v | TransparentValue v <- extVals]
      arrVals = S.toList $ S.fromList
        [(typ, sgn, shp) | ArrayValue _ _ typ sgn shp <- transpVals]
      (funs, exports) = unzip $ map mkJsArrayFuns arrVals
   in (T.intercalate "\n" funs, concat exports)

mkJsArrayFuns :: (PrimType, Signedness, [DimSize]) -> (T.Text, [T.Text])
mkJsArrayFuns (typ, sign, shp) =
  ([text|
  async new_${name}(data, ${shapeParams}) {
    return await ${newCall};
  }
  async free_${name}(arr) {
    return await ${freeCall};
  }
  async values_${name}(arr, data) {
    return await ${valuesCall};
  }
  shape_${name}(arr) {
    return this.m._futhark_shape_${name}(this.ctx, arr);
  }
  |], exports)
  where
    rank = length shp
    name = GC.arrayName typ sign rank
    shapeNames = ["dim" <> prettyText i | i <- [0 .. rank - 1]]
    shapeParams = T.intercalate ", " shapeNames
    newCall = asyncCall 
      ("futhark_new_" <> name) True (["this.ctx", "data"] ++ shapeNames)
    freeCall = asyncCall
      ("futhark_free_" <> name) True ["this.ctx", "arr"]
    valuesCall = asyncCall
      ("futhark_values_" <> name) True ["this.ctx", "arr", "data"]
    exports = ["futhark_new_" <> name, "futhark_free_" <> name,
               "futhark_values_" <> name, "futhark_shape_" <> name]

-- | Compile the program to C with calls to WebGPU, along with a JS wrapper
-- library.
compileProg :: (MonadFreshNames m) => T.Text -> Prog GPUMem
            -> m (ImpGen.Warnings, (GC.CParts, T.Text, [T.Text]))
compileProg version prog = do
  ( ws,
    Program wgsl_code wgsl_prelude macros kernels params failures prog'
    ) <- ImpGen.compileProg prog
  c <- GC.compileProg
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

-- | As server script. Speaks custom server protocol to local Python server
-- wrapper that speaks the actual server protocol.
asJSServer :: GC.CParts -> (T.Text, T.Text)
asJSServer parts =
  let (_, c, _) = GC.asLibrary parts
   in (c, serverWsJs)
