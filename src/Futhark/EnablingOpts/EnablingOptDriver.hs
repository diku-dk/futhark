module Futhark.EnablingOpts.EnablingOptDriver
  ( enablingOpts
  , copyCtProp
  , CallGraph
  , buildCallGraph
  , aggInlineDriver
  , deadFunElim
  , normCopyDeadOpts
  , normCopyOneLambda
  , EnablingOptError(..)
  )
  where

import Futhark.InternalRep
import Futhark.MonadFreshNames
import qualified Futhark.IndexInliner as II

import Futhark.EnablingOpts.InliningDeadFun
import Futhark.EnablingOpts.CopyCtPropFold
import Futhark.EnablingOpts.DeadVarElim
import Futhark.EnablingOpts.EnablingOptErrors
import Futhark.EnablingOpts.AlgSimplify

import Debug.Trace
--------------------------------------------------------------
---- Enabling Optimization Driver
--------------------------------------------------------------

enablingOpts :: Prog -> Either EnablingOptError Prog
enablingOpts prog0 = do
    scal <- canSimplify 1
    let str = "Predicate Result is: "++ppScalExp scal
    let prog = trace str prog0  
    prog_enopt1 <- normCopyDeadOpts prog
    prog_enopt2 <- normCopyDeadOpts prog_enopt1
    prog_deadf2 <- deadFunElim      prog_enopt2
    prog_flat_opt <- normCopyDeadOpts prog_deadf2

    normCopyDeadOpts prog_flat_opt

normCopyDeadOpts :: Prog -> Either EnablingOptError Prog
normCopyDeadOpts prog = do
    (_,prog_cp)    <- copyCtProp      prog
    (_, prog_dce)  <- deadCodeElim    prog_cp
    return prog_dce

normCopyOneLambda :: MonadFreshNames m => Prog -> Lambda ->
                     m (Either EnablingOptError Lambda)
normCopyOneLambda prog lam = modifyNameSource $ \src ->
  let res = do lam' <- copyCtPropOneLambda prog  lam
               return $ II.transformLambda src lam'
  in case res of Left e                -> (Left e, src)
                 Right (normLam, src') -> (Right normLam, src')
