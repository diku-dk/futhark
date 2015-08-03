-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program. The C code is strictly
-- sequential and leaks memory like a sieve, so it's not very useful
-- yet.
module Futhark.CodeGen.Backends.SequentialC
  ( compileProg
  ) where

import Futhark.Representation.ExplicitMemory

import qualified Futhark.CodeGen.ImpGen as ImpGen
import qualified Futhark.CodeGen.Backends.GenericC as GenericC

compileProg :: Prog -> Either String String
compileProg = fmap (GenericC.compileProg operations () [] []) .
              ImpGen.compileProgSimply
  where operations :: GenericC.Operations () ()
        operations = GenericC.defaultOperations {
          GenericC.opsCompiler = const $ return GenericC.Done
          }
