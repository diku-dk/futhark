-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program. The C code is strictly
-- sequential and leaks memory like a sieve, so it's not very useful
-- yet.
module Futhark.CodeGen.Backends.SequentialC
  ( compileProg
  ) where

import Control.Monad
import Data.Traversable

import Prelude

import Futhark.Representation.ExplicitMemory
import qualified Futhark.CodeGen.ImpCode.Sequential as Imp
import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.MonadFreshNames

compileProg :: MonadFreshNames m => Prog ExplicitMemory -> m (Either String String)
compileProg = traverse (GenericC.compileProg operations () [DefaultSpace] [] [] [] [] []) <=<
              ImpGen.compileProg
  where operations :: GenericC.Operations Imp.Sequential ()
        operations = GenericC.defaultOperations {
          GenericC.opsCompiler = const $ return ()
          }
