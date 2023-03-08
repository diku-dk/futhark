{-# LANGUAGE QuasiQuotes #-}

-- | Boilerplate for multicore C code.
module Futhark.CodeGen.Backends.MulticoreC.Boilerplate (generateBoilerplate) where

import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.RTS.C (backendsMulticoreH, schedulerH)
import Language.C.Quote.OpenCL qualified as C

-- | Generate the necessary boilerplate.
generateBoilerplate :: GC.CompilerM op s ()
generateBoilerplate = do
  mapM_ GC.earlyDecl [C.cunit|$esc:(T.unpack schedulerH)|]
  mapM_ GC.earlyDecl [C.cunit|$esc:(T.unpack backendsMulticoreH)|]
  GC.headerDecl GC.InitDecl [C.cedecl|void futhark_context_config_set_num_threads(struct futhark_context_config *cfg, int n);|]
  GC.generateProgramStruct
{-# NOINLINE generateBoilerplate #-}
