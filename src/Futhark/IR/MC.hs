{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | A representation for multicore CPU parallelism.
module Futhark.IR.MC
       ( -- * The Lore definition
         MC

         -- * Simplification
       , simplifyProg

         -- * Module re-exports
       , module Futhark.IR.Prop
       , module Futhark.IR.Traversals
       , module Futhark.IR.Pretty
       , module Futhark.IR.Syntax
       , module Futhark.IR.SegOp
       , module Futhark.IR.SOACS.SOAC
       )
where

import Futhark.Pass
import Futhark.IR.Syntax
import Futhark.IR.SegOp
import Futhark.IR.Prop
import Futhark.IR.Traversals
import Futhark.IR.Pretty
import Futhark.IR.SOACS.SOAC hiding (HistOp(..))
import Futhark.Binder
import Futhark.Construct
import qualified Futhark.TypeCheck as TypeCheck
import qualified Futhark.Optimise.Simplify.Engine as Engine
import qualified Futhark.Optimise.Simplify as Simplify
import Futhark.Optimise.Simplify.Rules

data MC

instance Decorations MC where
  type Op MC = SegOp () MC

instance ASTLore MC where
  expTypesFromPattern = return . expExtTypesFromPattern

instance TypeCheck.CheckableOp MC where
  checkOp = typeCheckSegOp (const $ pure ())

instance TypeCheck.Checkable MC where

instance Bindable MC where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps MC where
  mkExpDecB = bindableMkExpDecB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance BinderOps (Engine.Wise MC) where
  mkExpDecB = bindableMkExpDecB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance PrettyLore MC where

simpleMC :: Simplify.SimpleOps MC
simpleMC = Simplify.bindableSimpleOps simplifySegOp

simplifyProg :: Prog MC -> PassM (Prog MC)
simplifyProg = Simplify.simplifyProg simpleMC rules blockers
  where blockers = Engine.noExtraHoistBlockers
        rules = standardRules <> segOpRules

instance HasSegOp MC where
  type SegOpLevel MC = ()
  asSegOp = Just
  segOp = id

instance HasSegOp (Engine.Wise MC) where
  type SegOpLevel (Engine.Wise MC) = ()
  asSegOp = Just
  segOp = id
