{-# LANGUAGE TypeFamilies #-}

-- | A representation with flat parallelism via GPU-oriented kernels.
module Futhark.IR.GPU
  ( GPU,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,
    module Futhark.IR.GPU.Op,
    module Futhark.IR.GPU.Sizes,
    module Futhark.IR.SOACS.SOAC,
  )
where

import Futhark.Builder
import Futhark.Construct
import Futhark.IR.Aliases (Aliases)
import Futhark.IR.GPU.Op
import Futhark.IR.GPU.Sizes
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.SOACS.SOAC hiding (HistOp (..))
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import Futhark.IR.TypeCheck qualified as TC

-- | The phantom data type for the kernels representation.
data GPU

instance RepTypes GPU where
  type OpC GPU = HostOp SOAC

instance ASTRep GPU where
  expTypesFromPat = pure . expExtTypesFromPat

instance TC.Checkable GPU where
  checkOp = typeCheckGPUOp Nothing
    where
      -- GHC 9.2 goes into an infinite loop without the type annotation.
      typeCheckGPUOp ::
        Maybe SegLevel ->
        HostOp SOAC (Aliases GPU) ->
        TC.TypeM GPU ()
      typeCheckGPUOp lvl =
        typeCheckHostOp (typeCheckGPUOp . Just) lvl typeCheckSOAC

instance Buildable GPU where
  mkBody = Body ()
  mkExpPat idents _ = basicPat idents
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BuilderOps GPU

instance PrettyRep GPU

instance HasSegOp GPU where
  type SegOpLevel GPU = SegLevel
  asSegOp (SegOp op) = Just op
  asSegOp _ = Nothing
  segOp = SegOp

-- Note [GPU Terminology]
--
-- For lack of a better spot to put it, this Note summarises the
-- terminology used for GPU concepts in the Futhark compiler. The
-- terminology is based on CUDA terminology, and tries to match it as
-- closely as possible. However, this was not always the case (issue
-- #2062), so you may find some code that uses e.g. OpenCL
-- terminology. In most cases there is no ambiguity, but there are a
-- few instances where the same term is used for different things.
-- Please fix any instances you find.
--
-- The terminology is as follows:
--
-- Host: Essentially the CPU; whatever is controlling the GPU.
--
-- Kernel: A GPU program that can be launched from the host.
--
-- Grid: The geometry of the thread blocks launched for a kernel. The
-- size of a grid is always in terms of the number of thread blocks
-- ("grid size"). A grid can have up to 3 dimensions, although we do
-- not make much use of it - and not at all prior to code generation.
--
-- Thread block: Just as in CUDA. "Workgroup" in OpenCL. Abbretiation:
-- tblock. Never just call this "block"; there are too many things
-- called "block". Must match the dimensionality of the grid.
--
-- Thread: Just as in CUDA.  "Workitem" in OpenCL.
--
-- Global thread identifier: A globally unique number for a thread
-- along one dimension. Abbreviation: gtid. We also use this term for
-- the identifiers bound by SegOps. In OpenCL, corresponds to
-- get_global_id(). (Except when we virtualise the thread space.)
--
-- Local thread identifier: A locally unique number (within the thread
-- block) for each thread. Abbreviation: ltid. In OpenCL, corresponds
-- to get_local_id().  In CUDA, corresponds to threadIdx.
--
-- Thread block identifier: A number unique to each thread block in a
-- single dimension.  In CUDA, corresponds to blockIdx.
--
-- Local memory: Thread-local private memory. In CUDA, this is
-- sometimes put in registers (if you are very careful in how you use
-- it). In OpenCL, this is called "private memory", and "local memory"
-- is something else entirely.
--
-- Shared memory: Just as in CUDA. Fast scratchpad memory accessible
-- to all threads within the same thread block. In OpenCL, this is
-- "local memory".
--
-- Device memory: Sometimes also called "global memory"; this is the
-- big-but-slow memory on the GPU.
