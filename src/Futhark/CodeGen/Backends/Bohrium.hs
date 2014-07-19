-- | Translate SOACs into calls to the Bohrium C API.  Quite unfinished.
--
-- The idea: For every SOAC expression we encounter, check whether it
-- can be directly translated to a Bohrium primitive.  If it can't,
-- the "Futhark.Backends.BohriumBackend" will translate it into a
-- sequential loop for us, but that's obviously not something we want
-- to happen.  Hence, another (currently unwritten) compiler pass
-- should do aggressive loop fission and other transformations in
-- order to make the program fit patterns recognised by this module.
--
-- For example, the SOAC @map(fn int (int x) => x+2,a)@ corresponds
-- nicely to the Bohrium function
-- @bh_multi_array_int32_add_scalar_rhs@.  And @map(fn int (int x, int
-- y) => x+y,zip(a,b))@ is @bh_multi_array_int32_add@.  This module
-- should eventually recognise all such simple patterns.
--
-- Current state: Simple unary and binary mappings across integer
-- arrays can be translated, nothing else.  Also significantly, arrays
-- are copied to Bohrium space before every operation, and back when
-- it's done.  This is massively wasteful.
module Futhark.CodeGen.Backends.Bohrium (compileProg) where

import Control.Monad.Writer

import Futhark.Representation.Basic

import qualified Futhark.CodeGen.ImpCode as Imp
import qualified Futhark.CodeGen.ImpGen as ImpGen
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import qualified Futhark.CodeGen.Backends.CUtils as C
import Futhark.CodeGen.Backends.BohriumCodeGen
import Futhark.CodeGen.Backends.BohriumOp
import Futhark.CodeGen.FirstOrderSOACS

compileProg :: Prog -> String
compileProg = addHeader . GenericC.compileProg bohriumCompiler. ImpGen.compileProg expCompiler
  where expCompiler [target] e
          | Just op <- compileSOACtoBohrium e = do
          tell $ Imp.Op (C.var $ textual $ identName target, op)
          return ImpGen.Done
        expCompiler targets e =
          firstOrderSOACS targets e

        addHeader = ("#include <bh_c.h>\n"++)
