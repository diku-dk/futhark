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
