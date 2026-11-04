-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.  This C program is expected to
-- be converted to WebAssembly, so we also produce the intended
-- JavaScript wrapper.
module Futhark.CodeGen.Backends.MulticoreWASM
  ( compileProg,
  )
where

import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericWASM
import Futhark.CodeGen.Backends.MulticoreC qualified as MC
import Futhark.CodeGen.Backends.MulticoreC.Boilerplate (generateBoilerplate)
import Futhark.CodeGen.ImpGen.Multicore qualified as ImpGen
import Futhark.IR.MCMem
import Futhark.MonadFreshNames

-- | Compile Futhark program to wasm-multicore program (some assembly
-- required).
--
-- The triple that is returned consists of
--
-- * Generated C code (to be passed to Emscripten).
--
-- * JavaScript wrapper code that presents a nicer interface to the
--   Emscripten-produced code (passed to @emcc@ with
--   @--extern-post-js@).
--
-- * The names of the C functions that must be exported from the
--   WebAssembly module.
compileProg ::
  (MonadFreshNames m) =>
  T.Text ->
  Prog MCMem ->
  m (ImpGen.Warnings, (GC.CParts, T.Text, [String]))
compileProg version prog = do
  (ws, prog') <- ImpGen.compileProg prog

  prog'' <-
    GC.compileProg
      "wasm_multicore"
      version
      MC.operations
      generateBoilerplate
      ""
      (DefaultSpace, [DefaultSpace])
      MC.cliOptions
      prog'

  pure
    ( ws,
      ( prog'',
        javascriptWrapper (GC.cManifest prog''),
        "_futhark_context_config_set_num_threads" : emccExportNames (GC.cManifest prog'')
      )
    )
