module Futhark.CodeGen.Backends.SequentialPython
     ( compileProg
     ) where

import Control.Monad

import Futhark.Error
import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import qualified Futhark.CodeGen.Backends.GenericPython as GenericPython
import Futhark.CodeGen.Backends.GenericPython.Definitions
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m =>
               Maybe String -> Prog ExplicitMemory -> m (Either InternalError String)
compileProg module_name =
  ImpGen.compileProg >=>
  traverse (GenericPython.compileProg
            module_name
            GenericPython.emptyConstructor
            imports
            defines
            operations () [] [])
  where imports = [Import "sys" Nothing,
                   Import "numpy" $ Just "np",
                   Import "ctypes" $ Just "ct",
                   Import "time" Nothing]
        defines = [Escape pyValues, Escape pyFunctions, Escape pyPanic]
        operations :: GenericPython.Operations Imp.Sequential ()
        operations = GenericPython.defaultOperations
                     { GenericPython.opsCompiler = const $ return ()
                     , GenericPython.opsCopy = copySequentialMemory
                     }

copySequentialMemory :: GenericPython.Copy Imp.Sequential ()
copySequentialMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes _bt =
  GenericPython.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copySequentialMemory _ _ destspace _ _ srcspace _ _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace
