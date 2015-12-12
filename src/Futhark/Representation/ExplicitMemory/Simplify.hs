{-# LANGUAGE TypeFamilies #-}
module Futhark.Representation.ExplicitMemory.Simplify
       ( simplifyExplicitMemory
       )
where

import Prelude

import qualified Futhark.Representation.AST.Syntax as AST
import Futhark.Representation.AST.Syntax
  hiding (Prog, PrimOp, LoopOp, Exp, Body, Binding,
          Pattern, PatElem, Lambda, ExtLambda, FunDec, FParam, LParam,
          RetType)
import Futhark.Representation.ExplicitMemory
import Futhark.Representation.Kernels.Simplify()
import Futhark.Pass.ExplicitAllocations (simplifiable)
import qualified Futhark.Analysis.UsageTable as UT
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Optimise.Simplifier as Simplifier
import Futhark.Optimise.Simplifier.Rules
import Futhark.MonadFreshNames

simplifyExplicitMemory :: MonadFreshNames m => Prog -> m Prog
simplifyExplicitMemory =
  Simplifier.simplifyProgWithRules simplifiable standardRules blockers
  where blockers =
          Engine.HoistBlockers {
            Engine.blockHoistPar = isAlloc
          , Engine.blockHoistSeq = isResultAlloc
          }

isAlloc :: Op lore ~ MemOp lore => Engine.BlockPred lore
isAlloc _ (Let _ _ (Op Alloc{})) = True
isAlloc _ _                      = False

isResultAlloc :: Op lore ~ MemOp lore => Engine.BlockPred lore
isResultAlloc usage (Let (AST.Pattern [] [bindee]) _
                     (Op Alloc{})) =
  UT.isInResult (patElemName bindee) usage
isResultAlloc _ _ = False
