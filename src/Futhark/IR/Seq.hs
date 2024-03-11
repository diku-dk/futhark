{-# LANGUAGE TypeFamilies #-}

-- | A sequential representation.
module Futhark.IR.Seq
  ( Seq,

    -- * Simplification
    simplifyProg,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,
  )
where

import Futhark.Builder
import Futhark.Construct
import Futhark.IR.Aliases (Aliases)
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import Futhark.IR.TypeCheck qualified as TC
import Futhark.Optimise.Simplify qualified as Simplify
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rules
import Futhark.Pass

-- | The phantom type for the Seq representation.
data Seq

instance RepTypes Seq

instance ASTRep Seq where
  expTypesFromPat = pure . expExtTypesFromPat

instance TC.Checkable (Aliases Seq) where
  checkOp NoOp = pure ()

instance Buildable Seq where
  mkBody = Body ()
  mkExpPat idents _ = basicPat idents
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BuilderOps Seq

instance TraverseOpStms Seq where
  traverseOpStms _ = pure

instance PrettyRep Seq

instance BuilderOps (Engine.Wise Seq)

instance TraverseOpStms (Engine.Wise Seq) where
  traverseOpStms _ = pure

simpleSeq :: Simplify.SimpleOps Seq
simpleSeq = Simplify.bindableSimpleOps (const $ pure (NoOp, mempty))

-- | Simplify a sequential program.
simplifyProg :: Prog Seq -> PassM (Prog Seq)
simplifyProg = Simplify.simplifyProg simpleSeq standardRules blockers
  where
    blockers = Engine.noExtraHoistBlockers
