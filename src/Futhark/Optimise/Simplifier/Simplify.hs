module Futhark.Optimise.Simplifier.Simplify
  ( Simplifiable
  , SimpleOps (..)
  , SimpleM
  , bindableSimpleOps
  , simplifyProg
  , simplifyFun
  , simplifyLambda
  )
  where

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import Futhark.Optimise.Simplifier.Lore (Wise)
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.Simple

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg :: Simplifiable lore =>
                SimpleOps (SimpleM lore)
             -> RuleBook (SimpleM lore)
             -> Prog lore
             -> Prog (Wise lore)
simplifyProg simpl rules prog =
  Prog $ fst $ runSimpleM (mapM Engine.simplifyFun $ progFunctions prog)
               simpl (Engine.emptyEnv rules $ Just prog) namesrc
  where namesrc = newNameSourceForProg prog

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyFun :: (MonadFreshNames m, Simplifiable lore) =>
               SimpleOps (SimpleM lore)
            -> RuleBook (SimpleM lore)
            -> FunDec lore
            -> m (FunDec (Wise lore))
simplifyFun simpl rules fundec =
  modifyNameSource $ runSimpleM (Engine.simplifyFun fundec) simpl $
  Engine.emptyEnv rules Nothing

-- | Simplify just a single 'Lambda'.
simplifyLambda :: (MonadFreshNames m, Simplifiable lore) =>
                  SimpleOps (SimpleM lore)
               -> RuleBook (SimpleM lore)
               -> Maybe (Prog lore) -> Lambda lore -> [Maybe VName]
               -> m (Lambda (Wise lore))
simplifyLambda simpl rules prog lam args =
  modifyNameSource $ runSimpleM (Engine.simplifyLambda lam args) simpl $
  Engine.emptyEnv rules prog
