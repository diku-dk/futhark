{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | A sequential representation.
module Futhark.IR.Seq
  ( -- * The Lore definition
    Seq,

    -- * Simplification
    simplifyProg,

    -- * Module re-exports
    module Futhark.IR.Prop,
    module Futhark.IR.Traversals,
    module Futhark.IR.Pretty,
    module Futhark.IR.Syntax,
  )
where

import Futhark.Binder
import Futhark.Construct
import Futhark.IR.Pretty
import Futhark.IR.Prop
import Futhark.IR.Syntax
import Futhark.IR.Traversals
import qualified Futhark.Optimise.Simplify as Simplify
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rules
import Futhark.Pass
import qualified Futhark.TypeCheck as TypeCheck

-- | The phantom type for the Seq representation.
data Seq

instance Decorations Seq where
  type Op Seq = ()

instance ASTLore Seq where
  expTypesFromPattern = return . expExtTypesFromPattern

instance TypeCheck.CheckableOp Seq where
  checkOp = pure

instance TypeCheck.Checkable Seq

instance Bindable Seq where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps Seq

instance PrettyLore Seq

instance BinderOps (Engine.Wise Seq)

simpleSeq :: Simplify.SimpleOps Seq
simpleSeq = Simplify.bindableSimpleOps (const $ pure ((), mempty))

-- | Simplify a sequential program.
simplifyProg :: Prog Seq -> PassM (Prog Seq)
simplifyProg = Simplify.simplifyProg simpleSeq standardRules blockers
  where
    blockers = Engine.noExtraHoistBlockers
