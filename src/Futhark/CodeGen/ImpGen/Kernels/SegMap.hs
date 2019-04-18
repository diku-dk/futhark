{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Kernels.SegMap
  ( compileSegMap ) where

import Control.Monad.Except

import Prelude hiding (quot, rem)

import Futhark.Representation.ExplicitMemory
import Futhark.CodeGen.ImpGen.Kernels.Base

-- | Compile 'SegMap' instance to host-level code with calls to
-- various kernels.
compileSegMap :: Pattern ExplicitMemory
              -> KernelSpace
              -> KernelBody InKernel
              -> CallKernelGen ()
compileSegMap pat space kbody = do
  (constants, init_constants) <- kernelInitialisation space

  sKernel constants "segmap" $ do
    init_constants
    compileKernelStms constants (stmsToList $ kernelBodyStms kbody) $
      zipWithM_ (compileKernelResult constants) (patternElements pat) $
      kernelBodyResult kbody
