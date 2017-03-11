module Futhark.CodeGen.Backends.SequentialPython
     ( compileProg
     ) where

import Control.Monad
import Data.Traversable

import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import qualified Futhark.CodeGen.Backends.GenericPython as GenericPython
import Futhark.CodeGen.Backends.GenericPython.Definitions
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.MonadFreshNames

import Prelude

compileProg :: MonadFreshNames m => Maybe String -> Prog ExplicitMemory -> m (Either String String)
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
        defines = [Escape pyReader, Escape pyFunctions]
        operations :: GenericPython.Operations Imp.Sequential ()
        operations = GenericPython.defaultOperations {
          GenericPython.opsCompiler = const $ return ()
        }
