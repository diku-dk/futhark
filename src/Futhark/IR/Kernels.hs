{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | A representation with flat parallelism via GPU-oriented kernels.
module Futhark.IR.Kernels
  ( Kernels,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,
    module Futhark.IR.Kernels.Kernel,
    module Futhark.IR.Kernels.Sizes,
    module Futhark.IR.SOACS.SOAC,
  )
where

import Futhark.Binder
import Futhark.Construct
import Futhark.IR.Kernels.Kernel
import Futhark.IR.Kernels.Sizes
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.SOACS.SOAC hiding (HistOp (..))
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import qualified Futhark.TypeCheck as TypeCheck

-- | The phantom data type for the kernels representation.
data Kernels

instance RepTypes Kernels where
  type Op Kernels = HostOp Kernels (SOAC Kernels)

instance ASTRep Kernels where
  expTypesFromPattern = return . expExtTypesFromPattern

instance TypeCheck.CheckableOp Kernels where
  checkOp = typeCheckKernelsOp Nothing
    where
      typeCheckKernelsOp lvl =
        typeCheckHostOp (typeCheckKernelsOp . Just) lvl typeCheckSOAC

instance TypeCheck.Checkable Kernels

instance Bindable Kernels where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps Kernels

instance PrettyRep Kernels

instance HasSegOp Kernels where
  type SegOpLevel Kernels = SegLevel
  asSegOp (SegOp op) = Just op
  asSegOp _ = Nothing
  segOp = SegOp
