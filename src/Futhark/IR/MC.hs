{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | A representation for multicore CPU parallelism.
module Futhark.IR.MC
  ( MC,

    -- * Simplification
    simplifyProg,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,
    module Futhark.IR.SegOp,
    module Futhark.IR.SOACS.SOAC,
    module Futhark.IR.MC.Op,
  )
where

import Futhark.Builder
import Futhark.Construct
import Futhark.IR.MC.Op
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.SOACS.SOAC hiding (HistOp (..))
import qualified Futhark.IR.SOACS.Simplify as SOAC
import Futhark.IR.SegOp
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import qualified Futhark.IR.TypeCheck as TypeCheck
import qualified Futhark.Optimise.Simplify as Simplify
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rules
import Futhark.Pass

data MC

instance RepTypes MC where
  type Op MC = MCOp MC (SOAC MC)

instance ASTRep MC where
  expTypesFromPat = pure . expExtTypesFromPat

instance TypeCheck.CheckableOp MC where
  checkOp = typeCheckMCOp typeCheckSOAC

instance TypeCheck.Checkable MC

instance Buildable MC where
  mkBody = Body ()
  mkExpPat idents _ = basicPat idents
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BuilderOps MC

instance BuilderOps (Engine.Wise MC)

instance PrettyRep MC

instance TraverseOpStms (Engine.Wise MC) where
  traverseOpStms = traverseMCOpStms traverseSOACStms

simpleMC :: Simplify.SimpleOps MC
simpleMC = Simplify.bindableSimpleOps $ simplifyMCOp SOAC.simplifySOAC

simplifyProg :: Prog MC -> PassM (Prog MC)
simplifyProg = Simplify.simplifyProg simpleMC rules blockers
  where
    blockers = Engine.noExtraHoistBlockers
    rules = standardRules <> segOpRules

instance HasSegOp MC where
  type SegOpLevel MC = ()
  asSegOp = const Nothing
  segOp = ParOp Nothing

instance HasSegOp (Engine.Wise MC) where
  type SegOpLevel (Engine.Wise MC) = ()
  asSegOp = const Nothing
  segOp = ParOp Nothing
