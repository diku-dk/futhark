{-# LANGUAGE QuasiQuotes #-}

-- | Boilerplate for sequential C code.
module Futhark.CodeGen.Backends.SequentialC.Boilerplate (generateBoilerplate) where

import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC qualified as GC
import Futhark.CodeGen.RTS.C (backendsCH)
import Language.C.Quote.OpenCL qualified as C

-- | Generate the necessary boilerplate.
generateBoilerplate :: GC.CompilerM op s ()
generateBoilerplate = do
  GC.earlyDecl [C.cedecl|static const int num_tuning_params = 0;|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_names[1];|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_vars[1];|]
  GC.earlyDecl [C.cedecl|static const char *tuning_param_classes[1];|]
  GC.earlyDecl [C.cedecl|static typename int64_t *tuning_param_defaults[1];|]
  GC.earlyDecl [C.cedecl|$esc:(T.unpack backendsCH)|]
  GC.generateProgramStruct
{-# NOINLINE generateBoilerplate #-}
