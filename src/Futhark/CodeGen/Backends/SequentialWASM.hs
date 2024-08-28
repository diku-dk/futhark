-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.  This C program is expected to
-- be converted to WebAssembly, so we also produce the intended
-- JavaScript wrapper.
module Futhark.CodeGen.Backends.SequentialWASM
  ( compileProg,
    runServer,
    libraryExports,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
  )
where

import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.Backends.GenericWASM
import Futhark.CodeGen.Backends.SequentialC.Boilerplate
import Futhark.CodeGen.ImpCode.Sequential qualified as Imp
import Futhark.CodeGen.ImpGen.Sequential qualified as ImpGen
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames

-- | Compile Futhark program to wasm program (some assembly
-- required).
--
-- The triple that is returned consists of
--
-- * Generated C code (to be passed to Emscripten).
--
-- * JavaScript wrapper code that presents a nicer interface to the
--   Emscripten-produced code (this should be put in a @.class.js@
--   file by itself).
--
-- * Options that should be passed to @emcc@.
compileProg :: (MonadFreshNames m) => T.Text -> Prog SeqMem -> m (ImpGen.Warnings, (GC.CParts, T.Text, [String]))
compileProg version prog = do
  (ws, prog') <- ImpGen.compileProg prog

  prog'' <-
    GC.compileProg
      "wasm"
      version
      mempty
      operations
      generateBoilerplate
      ""
      (DefaultSpace, [DefaultSpace])
      []
      prog'
  pure (ws, (prog'', javascriptWrapper (fRepMyRep prog'), emccExportNames (fRepMyRep prog')))
  where
    operations :: GC.Operations Imp.Sequential ()
    operations =
      GC.defaultOperations
        { GC.opsCompiler = const $ pure ()
        }

fRepMyRep :: Imp.Program -> [JSEntryPoint]
fRepMyRep prog =
  let Imp.Functions fs = Imp.defFuns prog
      function (Imp.Function entry _ _ _) = do
        Imp.EntryPoint n res args <- entry
        Just $
          JSEntryPoint
            { name = nameToString n,
              parameters = map (extToString . snd) args,
              ret = map (extToString . snd) res
            }
   in mapMaybe (function . snd) fs
