{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A sequential representation.
module Futhark.IR.Seq
       ( -- * The Lore definition
         Seq

         -- * Simplification
       , simplifyProg

         -- * Module re-exports
       , module Futhark.IR.Prop
       , module Futhark.IR.Traversals
       , module Futhark.IR.Pretty
       , module Futhark.IR.Syntax
       )
where

import Futhark.Pass
import Futhark.IR.Syntax
import Futhark.IR.Prop
import Futhark.IR.Traversals
import Futhark.IR.Pretty
import Futhark.Binder
import Futhark.Construct
import qualified Futhark.TypeCheck as TypeCheck
import qualified Futhark.Optimise.Simplify.Engine as Engine
import qualified Futhark.Optimise.Simplify as Simplify
import Futhark.Optimise.Simplify.Rules

data Seq

instance Decorations Seq where
  type Op Seq = ()

instance ASTLore Seq where
  expTypesFromPattern = return . expExtTypesFromPattern

instance TypeCheck.CheckableOp Seq where
  checkOp = pure

instance TypeCheck.Checkable Seq where

instance Bindable Seq where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpDec _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps Seq where

instance PrettyLore Seq where

instance BinderOps (Engine.Wise Seq) where

simpleSeq :: Simplify.SimpleOps Seq
simpleSeq = Simplify.bindableSimpleOps (const $ pure ((), mempty))

simplifyProg :: Prog Seq -> PassM (Prog Seq)
simplifyProg = Simplify.simplifyProg simpleSeq standardRules blockers
  where blockers = Engine.noExtraHoistBlockers
