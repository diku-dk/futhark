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

compileProg :: MonadFreshNames m => Bool -> Prog -> m (Either String String)
compileProg as_module =
  ImpGen.compileProg >=>
  traverse (GenericPython.compileProg as_module imports defines operations () [] [])
  where imports = [Import "sys" Nothing,
                   Import "numpy" $ Just "np",
                   Import "ctypes" $ Just "ct",
                   Import "time" Nothing]
        defines = [Escape pyTestMain, Escape pyFunctions, Escape pyUtility]
        operations :: GenericPython.Operations Imp.Sequential ()
        operations = GenericPython.defaultOperations {
          GenericPython.opsCompiler = const $ return GenericPython.Done
        }
