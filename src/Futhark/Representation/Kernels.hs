{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A representation with flat parallelism via GPU-oriented kernels.
module Futhark.Representation.Kernels
       ( -- * The Lore definition
         Kernels
       , InKernel
         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       , module Futhark.Representation.Kernels.Kernel
       , module Futhark.Representation.Kernels.KernelExp
       , module Futhark.Representation.Kernels.Sizes
       )
where

import Futhark.Representation.AST.Syntax
import Futhark.Representation.Kernels.Kernel
import Futhark.Representation.Kernels.KernelExp
import Futhark.Representation.Kernels.Sizes
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Binder
import Futhark.Construct
import qualified Futhark.TypeCheck as TypeCheck

-- This module could be written much nicer if Haskell had functors
-- like Standard ML.  Instead, we have to abuse the namespace/module
-- system.

data Kernels

instance Annotations Kernels where
  type Op Kernels = Kernel InKernel
instance Attributes Kernels where
  expTypesFromPattern = return . expExtTypesFromPattern

data InKernel
instance Annotations InKernel where
  type Op InKernel = KernelExp InKernel
instance Attributes InKernel where
  expTypesFromPattern = return . expExtTypesFromPattern
instance PrettyLore InKernel where

instance TypeCheck.CheckableOp Kernels where
  checkOp = TypeCheck.subCheck . typeCheckKernel

instance TypeCheck.CheckableOp InKernel where
  checkOp = TypeCheck.subCheck . typeCheckKernelExp

instance TypeCheck.Checkable Kernels where

instance TypeCheck.Checkable InKernel where

instance Bindable Kernels where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpAttr _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps Kernels where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance Bindable InKernel where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpAttr _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps InKernel where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance PrettyLore Kernels where
