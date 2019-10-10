{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A representation with flat parallelism via GPU-oriented kernels.
module Futhark.Representation.Kernels
       ( -- * The Lore definition
         Kernels
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , module Futhark.Representation.Kernels.Kernel
       , module Futhark.Representation.Kernels.Sizes
       , module Futhark.Representation.SOACS.SOAC
       )
where

import Futhark.Representation.AST.Syntax
import Futhark.Representation.Kernels.Kernel
import Futhark.Representation.Kernels.Sizes
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Representation.SOACS.SOAC hiding (HistOp(..))
import Futhark.Binder
import Futhark.Construct
import qualified Futhark.TypeCheck as TypeCheck

-- This module could be written much nicer if Haskell had functors
-- like Standard ML.  Instead, we have to abuse the namespace/module
-- system.

data Kernels

instance Annotations Kernels where
  type Op Kernels = HostOp Kernels (SOAC Kernels)
instance Attributes Kernels where
  expTypesFromPattern = return . expExtTypesFromPattern

instance TypeCheck.CheckableOp Kernels where
  checkOp = typeCheckKernelsOp Nothing
    where typeCheckKernelsOp lvl =
            typeCheckHostOp (typeCheckKernelsOp . Just) lvl typeCheckSOAC

instance TypeCheck.Checkable Kernels where

instance Bindable Kernels where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpAttr _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps Kernels where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance PrettyLore Kernels where
