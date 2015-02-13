module Futhark.Optimise.SuffCond
       (
         optimiseProg
       )
       where

import Control.Monad.State

import Futhark.Representation.Basic
import Futhark.Renamer
import Futhark.MonadFreshNames
import Futhark.Optimise.SuffCond.OptPredicates
import Futhark.Optimise.SuffCond.GenPredicates
import Futhark.Optimise.Simplifier
import Futhark.Optimise.Simplifier.Simplifiable (bindableSimplifiable)
import Futhark.Optimise.DeadVarElim

optimiseProg :: Prog -> Prog
optimiseProg prog =
  let m = optimisePredicates standardRules =<< extractPredicates prog
  in evalState m $ newNameSourceForProg prog

extractPredicates :: MonadFreshNames m => Prog -> m Prog
extractPredicates =
  liftM (Prog . concat) . mapM genPredicate' . progFunctions
  where genPredicate' fundec = do
          (predf,valf) <- genPredicate fundec
          -- FIXME: the simplifier is not good enough at dead code
          -- elimination, and it does not do fixpoint iteration.  This
          -- is horrible.
          predf' <- return . deadCodeElimFun =<< simplifyFun' =<<
                    return . deadCodeElimFun =<< simplifyFun' =<<
                    return . deadCodeElimFun =<< simplifyFun' =<<
                    return . deadCodeElimFun =<< simplifyFun' =<<
                    return . deadCodeElimFun =<< simplifyFun' =<<
                    return . deadCodeElimFun =<< simplifyFun' =<<
                    renameFun predf
          return [predf',valf]
        simplifyFun' = simplifyFunWithRules bindableSimplifiable basicRules
