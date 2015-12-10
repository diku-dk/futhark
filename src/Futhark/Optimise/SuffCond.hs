module Futhark.Optimise.SuffCond
       (
         Futhark.Optimise.SuffCond.optimisePredicates
       )
       where

import Control.Monad.State

import Futhark.Representation.Basic
import Futhark.Representation.Basic.Simplify
import Futhark.Transform.Rename
import Futhark.MonadFreshNames
import Futhark.Optimise.SuffCond.OptPredicates
import Futhark.Optimise.SuffCond.GenPredicates
import Futhark.Optimise.Simplifier
import Futhark.Optimise.DeadVarElim
import Futhark.Pass

optimisePredicates :: Pass Basic Basic
optimisePredicates =
  Pass { passName = "Optimise predicates"
       , passDescription = "Optimise predicates by extracting sufficient conditions."
       , passFunction =
         Futhark.Optimise.SuffCond.OptPredicates.optimisePredicates standardRules
         <=< extractPredicates
       }

extractPredicates :: MonadFreshNames m => Prog -> m Prog
extractPredicates =
  liftM (Prog . concat) . mapM genPredicate' . progFunctions
  where genPredicate' fundec = do
          (predf,valf) <- genPredicate fundec
          -- FIXME: the simplifier is not good enough at dead code
          -- elimination, and it does not do fixpoint iteration.  This
          -- is horrible.
          predf' <- liftM deadCodeElimFun . simplifyFun =<<
                    liftM deadCodeElimFun . simplifyFun =<<
                    liftM deadCodeElimFun . simplifyFun =<<
                    liftM deadCodeElimFun . simplifyFun =<<
                    liftM deadCodeElimFun . simplifyFun =<<
                    liftM deadCodeElimFun . simplifyFun =<<
                    renameFun predf
          return [predf',valf]
