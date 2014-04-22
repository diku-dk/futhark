-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent C program. The C code is strictly
-- sequential and leaks memory like a sieve, so it's not very useful
-- yet.
module Futhark.CodeGen.Backends.SequentialC (compileProg) where

import Control.Monad

import Data.Loc

import Futhark.InternalRep
import qualified Futhark.FirstOrderTransform as FOT
import Futhark.Tools

import qualified Futhark.CodeGen.ImpGen as ImpGen
import qualified Futhark.CodeGen.Backends.GenericC as GenericC

import Debug.Trace

compileProg :: Prog -> String
compileProg = GenericC.compileProg undefined . (\x -> trace (show x) x) . ImpGen.compileProg expCompiler
  where expCompiler targets e
          | FOT.transformable e =
            liftM ImpGen.CompileBindings $ do
              (e',bnds) <- runBinder'' $ FOT.transformExp e
              return $ bnds ++ [Let targets e']
          | otherwise           =
            return $ ImpGen.CompileExp e
