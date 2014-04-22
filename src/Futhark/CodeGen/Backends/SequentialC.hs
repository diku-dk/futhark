-- | C code generator.  This module can convert a well-typed Futhark
-- program to an equivalent C program.  It is assumed that the Futhark
-- program does not contain any arrays of tuples (use
-- "Futhark.TupleTransform").  The C code is strictly sequential and leaks
-- memory like a sieve, so it's not very useful yet.
module Futhark.CodeGen.Backends.SequentialC (compileProg) where

import Control.Monad

import Data.Loc

import Futhark.InternalRep
import qualified Futhark.FirstOrderTransform as FOT
import Futhark.Tools

import qualified Futhark.CodeGen.ImpGen as ImpGen
import qualified Futhark.CodeGen.Backends.GenericC as GenericC

compileProg :: Prog -> String
compileProg = GenericC.compileProg undefined . ImpGen.compileProg
{-
  where expCompiler _ e
          | FOT.transformable e =
            liftM GenericC.CompileBody $ runBinder $ do
              es <- letTupExp "soac" =<< FOT.transformExp e
              return $ resultBody [] (map Var es) $ srclocOf e
          | otherwise           =
            return $ GenericC.CompileExp e
-}