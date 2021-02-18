-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.  This C program is expected to
-- be converted to WebAssembly, so we also produce the intended
-- JavaScript wrapper.
module Futhark.CodeGen.Backends.SequentialWASM
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
  )
where

import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.SequentialC.Boilerplate
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames

-- | Compile the program to sequential C with a JavaScript wrapper.
compileProg :: MonadFreshNames m => Prog SeqMem -> m (ImpGen.Warnings, (GC.CParts, String))
compileProg prog = do
  (ws, prog') <- ImpGen.compileProg prog
  prog'' <-
    GC.compileProg
      "wasm"
      operations
      generateBoilerplate
      ""
      [DefaultSpace]
      []
      prog'
  pure (ws, (prog'', undefined))
  where
    operations :: GC.Operations Imp.Sequential ()
    operations =
      GC.defaultOperations
        { GC.opsCompiler = const $ return ()
        }
