-- | C code generator.  This module can convert a well-typed L0
-- program to an equivalent C program.  It is assumed that the L0
-- program does not contain any arrays of tuples (use
-- "L0C.TupleTransform").  The C code is strictly sequential and leaks
-- memory like a sieve, so it's not very useful yet.
module L0C.Backends.SequentialC (compileProg) where

import Control.Monad

import L0C.InternalRep
import qualified L0C.FirstOrderTransform as FOT

import qualified L0C.Backends.GenericC as GenericC

compileProg :: Prog -> String
compileProg = GenericC.compileProg expCompiler
  where expCompiler _ e
          | FOT.transformable e = liftM Left $ FOT.transformExp FOT.noDepthLimit e
          | otherwise           = return $ Left e
