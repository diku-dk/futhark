{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Futhark.Representation.Basic.Simplify
       ( simplifyBasic
       , simplifyBasicish
       , simplifyFun
       , simplifyLambda
       )
where

import Futhark.Representation.Basic
import qualified Futhark.Representation.AST.Syntax as AST
import qualified Futhark.Representation.AST.Annotations as Annotations
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Optimise.Simplifier as Simplifier
import Futhark.Optimise.Simplifier.Rules
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.Simple
import Futhark.Binder.Class

simplifyBasic :: MonadFreshNames m => Prog -> m Prog
simplifyBasic =
  simplifyBasicish

simplifyFun :: MonadFreshNames m => FunDec -> m FunDec
simplifyFun =
  Simplifier.simplifyFunWithRules bindableSimpleOps standardRules Engine.noExtraHoistBlockers

simplifyLambda :: (HasTypeEnv m, MonadFreshNames m) =>
                  Prog -> Lambda -> SubExp -> [Maybe VName] -> m Lambda
simplifyLambda =
  Simplifier.simplifyLambdaWithRules bindableSimpleOps standardRules Engine.noExtraHoistBlockers

simplifyBasicish :: (MonadFreshNames m, Bindable lore,
                     Engine.Simplifiable (Annotations.LetBound lore),
                     Engine.Simplifiable (Annotations.FParam lore),
                     Engine.Simplifiable (Annotations.LParam lore),
                     Engine.Simplifiable (Annotations.RetType lore),
                     Engine.SimplifiableOp lore (Op lore)) =>
                    AST.Prog lore -> m (AST.Prog lore)
simplifyBasicish =
  Simplifier.simplifyProgWithRules bindableSimpleOps standardRules Engine.noExtraHoistBlockers
