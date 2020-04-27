{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A sequential representation.
module Futhark.Representation.Seq
       ( -- * The Lore definition
         Seq

         -- * Simplification
       , simpleSeq
       , simplifyProg

         -- * Module re-exports
       , module Futhark.Representation.AST.Attributes
       , module Futhark.Representation.AST.Traversals
       , module Futhark.Representation.AST.Pretty
       , module Futhark.Representation.AST.Syntax
       )
where

import Futhark.Pass
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Pretty
import Futhark.Binder
import Futhark.Construct
import qualified Futhark.TypeCheck as TypeCheck
import qualified Futhark.Optimise.Simplify.Engine as Engine
import qualified Futhark.Optimise.Simplify as Simplify
import Futhark.Optimise.Simplify.Rules

data Seq

instance Annotations Seq where
  type Op Seq = ()

instance Attributes Seq where
  expTypesFromPattern = return . expExtTypesFromPattern

instance TypeCheck.CheckableOp Seq where
  checkOp = pure

instance TypeCheck.Checkable Seq where

instance Bindable Seq where
  mkBody = Body ()
  mkExpPat ctx val _ = basicPattern ctx val
  mkExpAttr _ _ = ()
  mkLetNames = simpleMkLetNames

instance BinderOps Seq where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

instance PrettyLore Seq where

instance BinderOps (Engine.Wise Seq) where
  mkExpAttrB = bindableMkExpAttrB
  mkBodyB = bindableMkBodyB
  mkLetNamesB = bindableMkLetNamesB

simpleSeq :: Simplify.SimpleOps Seq
simpleSeq = Simplify.bindableSimpleOps (const $ pure ((), mempty))

simplifyProg :: Prog Seq -> PassM (Prog Seq)
simplifyProg = Simplify.simplifyProg simpleSeq standardRules blockers
  where blockers = Engine.noExtraHoistBlockers
