module Futhark.CodeGen.Backends.SequentialCSharp
     ( compileProg
     ) where

import Control.Monad

import Futhark.IR.SeqMem
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import qualified Futhark.CodeGen.Backends.GenericCSharp as CS
import Futhark.CodeGen.Backends.GenericCSharp.AST ()
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m =>
               Maybe String -> Prog SeqMem -> m String
compileProg module_name =
  ImpGen.compileProg >=>
  CS.compileProg
  module_name
  CS.emptyConstructor
  []
  []
  operations
  ()
  empty
  []
  []
  []
  where operations :: CS.Operations Imp.Sequential ()
        operations = CS.defaultOperations
                     { CS.opsCompiler = const $ return ()
                     , CS.opsCopy = copySequentialMemory
                     }
        empty = return ()

copySequentialMemory :: CS.Copy Imp.Sequential ()
copySequentialMemory destmem destidx DefaultSpace srcmem srcidx DefaultSpace nbytes _bt =
  CS.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copySequentialMemory _ _ destspace _ _ srcspace _ _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace
