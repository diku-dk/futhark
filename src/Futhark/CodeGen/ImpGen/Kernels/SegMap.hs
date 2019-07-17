{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.ImpGen.Kernels.SegMap
  ( compileSegMap )
where

import Control.Monad.Except

import Prelude hiding (quot, rem)

import Futhark.Representation.ExplicitMemory
import Futhark.CodeGen.ImpGen.Kernels.Base
import Futhark.CodeGen.ImpGen
import qualified Futhark.CodeGen.ImpCode.Kernels as Imp
import Futhark.Util.IntegralExp (quotRoundingUp)

-- | Compile 'SegMap' instance code.
compileSegMap :: Pattern ExplicitMemory
              -> SegLevel
              -> SegSpace
              -> KernelBody ExplicitMemory
              -> CallKernelGen ()

compileSegMap _ SegThreadScalar{} _ _ =
  fail "compileSegMap: SegThreadScalar cannot be compiled at top level."

compileSegMap pat lvl space kbody = do
  let (is, dims) = unzip $ unSegSpace space
  dims' <- mapM toExp dims

  num_groups' <- traverse toExp $ segNumGroups lvl
  group_size' <- traverse toExp $ segGroupSize lvl

  case lvl of
    SegThreadScalar{} ->
      fail "compileSegMap: SegThreadScalar cannot be compiled at top level."

    SegThread{} ->
      sKernelThread "segmap" num_groups' group_size' (segFlat space) $ \constants -> do
      let virt_num_groups = product dims' `quotRoundingUp` unCount group_size'
      virtualiseGroups constants (segVirt lvl) virt_num_groups $ \group_id -> do
        let global_tid = Imp.vi32 group_id * unCount group_size' +
                         kernelLocalThreadId constants

        zipWithM_ dPrimV_ is $ unflattenIndex dims' global_tid

        sWhen (isActive $ unSegSpace space) $
          compileStms mempty (kernelBodyStms kbody) $
          zipWithM_ (compileThreadResult space constants) (patternElements pat) $
          kernelBodyResult kbody

    SegGroup{} ->
      sKernelGroup "segmap_intragroup" num_groups' group_size' (segFlat space) $ \constants -> do
      let virt_num_groups = product dims'
      virtualiseGroups constants (segVirt lvl) virt_num_groups $ \group_id -> do

        zipWithM_ dPrimV_ is $ unflattenIndex dims' $ Imp.vi32 group_id

        compileStms mempty (kernelBodyStms kbody) $
          zipWithM_ (compileGroupResult space constants) (patternElements pat) $
          kernelBodyResult kbody
