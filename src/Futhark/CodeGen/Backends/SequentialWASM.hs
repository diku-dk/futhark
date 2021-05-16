{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

--

-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program.  This C program is expected to
-- be converted to WebAssembly, so we also produce the intended
-- JavaScript wrapper.
module Futhark.CodeGen.Backends.SequentialWASM
  ( compileProg,
    runServer,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
  )
where

--import Language.Futhark.Core (nameToString) -- TODO (see if might be useful later)

import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.SequentialC.Boilerplate
import Futhark.CodeGen.Backends.GenericWASM
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames

-- | Compile the program to sequential C with a JavaScript wrapper.
compileProg :: MonadFreshNames m => Prog SeqMem -> m (ImpGen.Warnings, (GC.CParts, String, [String]))
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
  pure (ws, (prog'', javascriptWrapper (fRepMyRep prog'), emccExportNames (fRepMyRep prog')))
  where
    -- pure (ws, (prog'', undefined)

    operations :: GC.Operations Imp.Sequential ()
    operations =
      GC.defaultOperations
        { GC.opsCompiler = const $ return ()
        }

-- What do we need
-- Go from
-- prog' :: Imp.Program :: Imp.Definitions Sequential :: Definitions a :: Functions a :: Functions [(Name, Function a)] ... where Function a is Function
--

fRepMyRep :: Imp.Program -> [JSEntryPoint]
fRepMyRep prog =
  let Imp.Definitions _ (Imp.Functions fs) = prog
      entries = filter (\(_, Imp.Function isEntry _ _ _ _ _) -> isEntry) fs
      result = map (\(n, Imp.Function _ _ _ _ res args) -> JSEntryPoint {name = nameToString n, parameters = map extToString args, ret = map extToString res}) entries
   in -- TODO take futhark_entry from nameToString n
      result

