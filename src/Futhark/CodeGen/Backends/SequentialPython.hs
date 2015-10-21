module Futhark.CodeGen.Backends.SequentialPython
     ( compileProg
     ) where

   import Futhark.Representation.ExplicitMemory

   import qualified Futhark.CodeGen.ImpGen.Sequential as ImpGen
   import qualified Futhark.CodeGen.Backends.GenericPython as GenericPython

   compileProg :: Prog -> Either String String
   compileProg = fmap GenericPython.compileProg . ImpGen.compileProg
