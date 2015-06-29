module Futhark.Optimise.Simplifier.Simplify
  ( Simplifiable
  , SimpleOps (..)
  , SimpleM
  , bindableSimpleOps
  , simplifyProg
  , simplifyFun
  , simplifyLambda
  , simplifyBindings
  )
  where

import Data.Monoid

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Optimise.Simplifier.Lore (Wise)
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.Simple
import Futhark.Tools (intraproceduralTransformation)

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg :: Simplifiable lore =>
                SimpleOps (SimpleM lore)
             -> RuleBook (SimpleM lore)
             -> Prog lore
             -> Prog (Wise lore)
simplifyProg simpl rules prog =
  intraproceduralTransformation
  (simplifyFun' simpl rules (Just prog))
  prog

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyFun :: (MonadFreshNames m, Simplifiable lore) =>
               SimpleOps (SimpleM lore)
            -> RuleBook (SimpleM lore)
            -> FunDec lore
            -> m (FunDec (Wise lore))
simplifyFun simpl rules =
  simplifyFun' simpl rules Nothing

simplifyFun' :: (MonadFreshNames m, Simplifiable lore) =>
                SimpleOps (SimpleM lore)
             -> RuleBook (SimpleM lore)
             -> Maybe (Prog lore)
             -> FunDec lore
             -> m (FunDec (Wise lore))
simplifyFun' simpl rules prog fundec =
  modifyNameSource $ runSimpleM (Engine.simplifyFun fundec) simpl $
  Engine.emptyEnv rules prog

-- | Simplify just a single 'Lambda'.
simplifyLambda :: (MonadFreshNames m, HasTypeEnv m, Simplifiable lore) =>
                  SimpleOps (SimpleM lore)
               -> RuleBook (SimpleM lore)
               -> Maybe (Prog lore) -> Lambda lore -> [Maybe VName]
               -> m (Lambda (Wise lore))
simplifyLambda simpl rules prog lam args = do
  types <- askTypeEnv
  let m = Engine.localVtable (<> ST.fromTypeEnv types) $
          Engine.simplifyLambdaNoHoisting lam args
  modifyNameSource $ runSimpleM m simpl $
    Engine.emptyEnv rules prog

-- | Simplify a list of 'Binding's.
simplifyBindings :: (MonadFreshNames m, HasTypeEnv m, Simplifiable lore) =>
                    SimpleOps (SimpleM lore)
                 -> RuleBook (SimpleM lore)
                 -> Maybe (Prog lore) -> [Binding lore]
                 -> m [Binding (Wise lore)]
simplifyBindings simpl rules prog bnds = do
  types <- askTypeEnv
  let m = Engine.localVtable (<> ST.fromTypeEnv types) $
          fmap snd $ Engine.collectBindingsEngine $
          mapM_ Engine.simplifyBinding bnds
  modifyNameSource $ runSimpleM m simpl $
    Engine.emptyEnv rules prog
