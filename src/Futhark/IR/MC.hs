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
       , module Futhark.IR.MC.Op
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
import Futhark.IR.MC.Op
import qualified Futhark.IR.SOACS.Simplify as SOAC

data MC

instance Decorations MC where
  type Op MC = MCOp MC (SOAC MC)

instance ASTLore MC where
  expTypesFromPattern = return . expExtTypesFromPattern

instance TypeCheck.CheckableOp MC where
  checkOp = typeCheckMCOp typeCheckSOAC

instance TypeCheck.Checkable MC where

instance Bindable MC where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps MC where

instance BinderOps (Engine.Wise MC) where

instance PrettyLore MC where

simpleMC :: Simplify.SimpleOps MC
simpleMC = Simplify.bindableSimpleOps $ simplifyMCOp SOAC.simplifySOAC

simplifyProg :: Prog MC -> PassM (Prog MC)
simplifyProg = Simplify.simplifyProg simpleMC rules blockers
  where blockers = Engine.noExtraHoistBlockers
        rules = standardRules <> segOpRules

instance HasSegOp MC where
  type SegOpLevel MC = ()
  asSegOp = const Nothing
  segOp = ParOp Nothing

instance HasSegOp (Engine.Wise MC) where
  type SegOpLevel (Engine.Wise MC) = ()
  asSegOp = const Nothing
  segOp = ParOp Nothing
