{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A representation for multicore CPU parallelism.
module Futhark.Representation.MC
       ( -- * The Lore definition
         MC

         -- * Simplification
       , simplifyProg

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

import Futhark.Pass
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
import qualified Futhark.Optimise.Simplify.Engine as Engine
import qualified Futhark.Optimise.Simplify as Simplify
import Futhark.Optimise.Simplify.Rules

data MC

instance Annotations MC where
  type Op MC = SegOp () MC

instance Attributes MC where
  expTypesFromPattern = return . expExtTypesFromPattern

instance TypeCheck.CheckableOp MC where
  checkOp = typeCheckSegOp (const $ pure ())

instance TypeCheck.Checkable MC where

instance Bindable MC where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpAttr _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps MC where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance BinderOps (Engine.Wise MC) where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance PrettyLore MC where

simpleMC :: Simplify.SimpleOps MC
simpleMC = Simplify.bindableSimpleOps simplifySegOp

simplifyProg :: Prog MC -> PassM (Prog MC)
simplifyProg = Simplify.simplifyProg simpleMC standardRules blockers
  where blockers = Engine.noExtraHoistBlockers
