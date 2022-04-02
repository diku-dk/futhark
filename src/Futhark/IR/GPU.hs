{-# LANGUAGE FlexibleInstances #-}
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
import Futhark.IR.GPU.Op
import Futhark.IR.GPU.Sizes
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.SOACS.SOAC hiding (HistOp (..))
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import qualified Futhark.IR.TypeCheck as TC

-- | The phantom data type for the kernels representation.
data GPU

instance RepTypes GPU where
  type Op GPU = HostOp GPU (SOAC GPU)

instance ASTRep GPU where
  expTypesFromPat = pure . expExtTypesFromPat

instance TC.CheckableOp GPU where
  checkOp = typeCheckGPUOp Nothing
    where
      typeCheckGPUOp lvl =
        typeCheckHostOp (typeCheckGPUOp . Just) lvl typeCheckSOAC

instance TC.Checkable GPU

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
