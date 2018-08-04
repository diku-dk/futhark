{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | The Futhark prelude embedded into the Futhark compiler.  This
-- means we do not have to parse and type-check it whenever we compile
-- a Futhark program.
module Language.Futhark.Futlib.Builtin (builtinBasis) where

import Futhark.Compiler.Program
import Futhark.Compiler.TH
import Language.Futhark.Futlib()
import System.FilePath ((</>))

-- | The Futlib basis constructed from @futlib/prelude.fut@.  This is
-- only a subset of the full futlib, but contains pre-type checked
-- ASTs.  Set this to emptyBasis to speed up compilation of the
-- Futhark compiler (but slow down compilation of Futhark programs).
builtinBasis :: Basis
builtinBasis = $(embedBasis ("futlib" </> "prelude.fut") "/futlib/prelude")
