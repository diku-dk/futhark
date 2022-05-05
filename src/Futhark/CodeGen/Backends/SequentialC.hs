{-# LANGUAGE OverloadedStrings #-}

-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program. The C code is strictly
-- sequential, but can handle the full Futhark language.
module Futhark.CodeGen.Backends.SequentialC
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
  )
where

import Control.Monad
import qualified Data.Text as T
import qualified Futhark.CodeGen.Backends.GenericC as GC
import Futhark.CodeGen.Backends.SequentialC.Boilerplate
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames

-- | Compile the program to sequential C.
compileProg :: MonadFreshNames m => T.Text -> Prog SeqMem -> m (ImpGen.Warnings, GC.CParts)
compileProg version =
  traverse
    (GC.compileProg "c" version operations generateBoilerplate mempty [DefaultSpace] [])
    <=< ImpGen.compileProg
  where
    operations :: GC.Operations Imp.Sequential ()
    operations =
      GC.defaultOperations
        { GC.opsCompiler = const $ pure ()
        }
