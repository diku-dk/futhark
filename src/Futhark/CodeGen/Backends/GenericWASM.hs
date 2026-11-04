{-# LANGUAGE QuasiQuotes #-}

-- | Code generation shared by the WASM backends.  The JavaScript API is
-- in @rts/javascript/wrappers.js@, driven by the manifest.
module Futhark.CodeGen.Backends.GenericWASM
  ( emccExportNames,
    javascriptWrapper,
    runServer,
    libraryExports,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.RTS.JavaScript
import Futhark.Manifest
import Futhark.Util (nubOrd)
import NeatInterpolation (text)

-- | The names of the C functions that must be exported from the WebAssembly
-- module. This is every function named in the manifest, as the server calls
-- these directly, as well as the context management functions.
emccExportNames :: Manifest -> [String]
emccExportNames manifest =
  map (\f -> "'_" ++ T.unpack f ++ "'") (nubOrd $ manifestFunctions manifest)
    ++ [ "_futhark_context_config_new",
         "_futhark_context_config_free",
         "_futhark_context_config_set_debugging",
         "_futhark_context_config_set_logging",
         "_futhark_context_config_set_profiling",
         "_futhark_context_config_set_tuning_param",
         "_futhark_get_tuning_param_count",
         "_futhark_get_tuning_param_name",
         "_futhark_get_tuning_param_class",
         "_futhark_context_new",
         "_futhark_context_free",
         "_futhark_context_get_error",
         "_futhark_context_sync",
         "_futhark_context_clear_caches",
         "_futhark_context_report",
         "_futhark_context_pause_profiling",
         "_futhark_context_unpause_profiling"
       ]

-- | Every C function named in the manifest.
manifestFunctions :: Manifest -> [CFuncName]
manifestFunctions manifest =
  map entryPointCFun (M.elems $ manifestEntryPoints manifest)
    ++ concatMap typeFunctions (M.elems $ manifestTypes manifest)
  where
    typeFunctions (TypeArray _ _ _ ops) =
      [ arrayFree ops,
        arrayShape ops,
        arrayValues ops,
        arrayNew ops,
        arrayNewRaw ops,
        arrayValuesRaw ops,
        arrayIndex ops,
        arraySet ops
      ]
    typeFunctions (TypeOpaque _ ops extra_ops _) =
      [opaqueFree ops, opaqueStore ops, opaqueRestore ops]
        ++ maybe [] extraFunctions extra_ops

    extraFunctions (OpaqueRecord ops) =
      recordNew ops : map recordFieldProject (recordFields ops)
    extraFunctions (OpaqueSum ops) =
      sumVariant ops : concatMap variantFunctions (sumVariants ops)
    extraFunctions (OpaqueArray ops) =
      [ opaqueArrayIndex ops,
        opaqueArrayShape ops,
        opaqueArrayNew ops,
        opaqueArraySet ops
      ]
    extraFunctions (OpaqueRecordArray ops) =
      [ recordArrayZip ops,
        recordArrayIndex ops,
        recordArrayShape ops,
        recordArrayNew ops,
        recordArraySet ops
      ]
        ++ map recordFieldProject (recordArrayFields ops)

    variantFunctions v = [sumVariantConstruct v, sumVariantDestruct v]

-- | The JavaScript code passed to @emcc@ with @--extern-post-js@.
javascriptWrapper :: Manifest -> T.Text
javascriptWrapper manifest =
  T.unlines
    [ serverJs,
      valuesJs,
      wrappersJs,
      "const futhark_manifest = " <> manifestToJSON manifest <> ";"
    ]

-- | Javascript code that can be appended to the generated module to
-- run a Futhark server instance when it is loaded.
runServer :: T.Text
runServer =
  [text|
   // Worker threads (wasm-multicore) also load this module.
   const is_main_thread = globalThis.process?.versions?.node
     ? (await import("node:worker_threads")).isMainThread
     : globalThis.WorkerGlobalScope === undefined;
   if (is_main_thread) {
     await futhark_server_main();
   }|]

-- | The names exported by the generated module.
libraryExports :: T.Text
libraryExports = "export {FutharkContext, FutharkArray, FutharkOpaque};"
