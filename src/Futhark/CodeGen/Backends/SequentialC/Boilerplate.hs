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
  GC.earlyDecl [C.cedecl|$esc:(T.unpack backendsCH)|]
  GC.generateProgramStruct
{-# NOINLINE generateBoilerplate #-}
