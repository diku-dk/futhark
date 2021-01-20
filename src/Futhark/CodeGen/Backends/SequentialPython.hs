module Futhark.CodeGen.Backends.SequentialPython
  ( compileProg,
  )
where

import Control.Monad
import qualified Futhark.CodeGen.Backends.GenericPython as GenericPython
import Futhark.CodeGen.Backends.GenericPython.AST
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import Futhark.IR.SeqMem
import Futhark.MonadFreshNames

compileProg ::
  MonadFreshNames m =>
  GenericPython.CompilerMode ->
  String ->
  Prog SeqMem ->
  m (ImpGen.Warnings, String)
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
        { GenericPython.opsCompiler = const $ return (),
          GenericPython.opsCopy = copySequentialMemory
        }

copySequentialMemory :: GenericPython.Copy Imp.Sequential ()
copySequentialMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes _bt =
  GenericPython.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copySequentialMemory _ _ destspace _ _ srcspace _ _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace
