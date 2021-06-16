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
    module Futhark.IR.GPU.Kernel,
    module Futhark.IR.GPU.Sizes,
    module Futhark.IR.SOACS.SOAC,
  )
where

import Futhark.Binder
import Futhark.Construct
import Futhark.IR.GPU.Kernel
import Futhark.IR.GPU.Sizes
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.SOACS.SOAC hiding (HistOp (..))
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import qualified Futhark.TypeCheck as TypeCheck

-- | The phantom data type for the kernels representation.
data GPU

instance RepTypes GPU where
  type Op GPU = HostOp GPU (SOAC GPU)

instance ASTRep GPU where
  expTypesFromPattern = return . expExtTypesFromPattern

instance TypeCheck.CheckableOp GPU where
  checkOp = typeCheckGPUOp Nothing
    where
      typeCheckGPUOp lvl =
        typeCheckHostOp (typeCheckGPUOp . Just) lvl typeCheckSOAC

instance TypeCheck.Checkable GPU

instance Bindable GPU where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps GPU

instance PrettyRep GPU

instance HasSegOp GPU where
  type SegOpLevel GPU = SegLevel
  asSegOp (SegOp op) = Just op
  asSegOp _ = Nothing
  segOp = SegOp
