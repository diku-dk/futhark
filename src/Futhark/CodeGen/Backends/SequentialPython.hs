-- | Code generation for sequential Python.
module Futhark.CodeGen.Backends.SequentialPython
  ( compileProg,
  )
where

import Control.Monad
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericPython qualified as GenericPython
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.ImpCode.Sequential qualified as Imp
import Futhark.CodeGen.ImpGen.Sequential qualified as ImpGen
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames

-- | Compile the program to Python.
compileProg ::
  (MonadFreshNames m) =>
  GenericPython.CompilerMode ->
  String ->
  Prog SeqMem ->
  m (ImpGen.Warnings, T.Text)
compileProg mode class_name =
  ImpGen.compileProg
    >=> traverse
      ( GenericPython.compileProg
          mode
          class_name
          GenericPython.emptyConstructor
          imports
          defines
          operations
          ()
          []
          []
      )
  where
    imports =
      [ Import "sys" Nothing,
        Import "numpy" $ Just "np",
        Import "ctypes" $ Just "ct",
        Import "time" Nothing
      ]
    defines = []
    operations :: GenericPython.Operations Imp.Sequential ()
    operations =
      GenericPython.defaultOperations
        { GenericPython.opsCompiler = const $ pure ()
        }
