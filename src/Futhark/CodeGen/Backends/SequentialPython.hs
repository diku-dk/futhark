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
import Futhark.MonadFreshNames

import Prelude

compileProg :: MonadFreshNames m => Bool -> Prog -> m (Either String String)
compileProg as_module =
  ImpGen.compileProg >=>
  traverse (GenericPython.compileProg as_module imports defines operations () [] [])
  where shebang = if as_module then [] else ["#!/usr/bin/env python"]
        imports = shebang ++ ["from numpy import *", "from ctypes import *", "import sys", "import re", "import time", "import math"]
        defines = [pyTestMain, pyFunctions, pyUtility] --we could create a seperatate py file that contains all the depenendies and just import it
        operations :: GenericPython.Operations Imp.Sequential ()
        operations = GenericPython.defaultOperations {
          GenericPython.opsCompiler = const $ return GenericPython.Done
        }
