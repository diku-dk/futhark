{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
-- |
--
-- Apply the simplification engine
-- ("Futhark.Optimise.Simplifier.Engine") to an entire program, using
-- some set of simplification rules.
--
module Futhark.Optimise.Simplifier
  ( -- * Simple interface
    simplifyProgWithStandardRules
  , simplifyFunWithStandardRules
  , simplifyLambdaWithStandardRules
    -- * More generic interface
  , Simplifiable (..)
  , bindableSimplifiable
  , simplifyProg
  , simplifyFun
  , simplifyLambda
  )
  where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.RWS
import Control.Arrow (second)

import qualified Futhark.Representation.Aliases as Aliases
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Representation.Aliases
  (Aliases, removeProgAliases, removeFunDecAliases, removeLambdaAliases)
import Futhark.Optimise.Simplifier.Rules

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProgWithStandardRules :: Proper lore =>
                                 Simplifiable lore
                              -> Prog lore -> Prog lore
simplifyProgWithStandardRules simpl =
  removeProgAliases .
  simplifyProg simpl standardRules

-- | Simplify just a single function declaration.
simplifyFunWithStandardRules :: (MonadFreshNames m, Proper lore) =>
                                Simplifiable lore
                             -> FunDec lore
                             -> m (FunDec lore)
simplifyFunWithStandardRules simpl =
  liftM removeFunDecAliases .
  simplifyFun simpl standardRules

-- | Simplify just a single 'Lambda'.
simplifyLambdaWithStandardRules :: (MonadFreshNames m, Proper lore) =>
                                   Simplifiable lore
                                -> Prog lore
                                -> Lambda lore
                                -> [Maybe SubExp]
                                -> m (Lambda lore)
simplifyLambdaWithStandardRules simpl prog lam args =
  liftM removeLambdaAliases $
  simplifyLambda simpl standardRules (Just prog) lam args

data Simplifiable lore =
  Simplifiable { mkLetS :: ST.SymbolTable (Aliases lore)
                        -> [Ident] -> Aliases.Exp lore
                        -> Binder (Aliases lore) (Aliases.Binding lore)
               , mkBodyS :: ST.SymbolTable (Aliases lore)
                         -> [Aliases.Binding lore] -> Result
                         -> Binder (Aliases lore) (Aliases.Body lore)
               }

bindableSimplifiable :: Bindable lore => Simplifiable lore
bindableSimplifiable = Simplifiable mkLetS' mkBodyS'
  where mkLetS' _ pat e = return $ mkLet pat e
        mkBodyS' _ bnds res = return $ mkBody bnds res

newtype SimpleM lore a = SimpleM (RWS
                                  (Simplifiable lore, Engine.Env (SimpleM lore))   -- Reader
                                  (Engine.Need (Aliases lore))                     -- Writer
                                  (Engine.State (SimpleM lore), NameSource VName)  -- State
                                  a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Engine.Need (Aliases lore)),
            MonadReader (Simplifiable lore, Engine.Env (SimpleM lore)),
            MonadState (Engine.State (SimpleM lore), NameSource VName))

instance MonadFreshNames (SimpleM lore) where
  getNameSource   = snd <$> get
  putNameSource y = modify $ \(x, _) -> (x,y)

instance Proper lore => MonadBinder (SimpleM lore) where
  addBinding      = Engine.addBindingEngine
  collectBindings = Engine.collectBindingsEngine

instance Proper lore => Engine.MonadEngine (SimpleM lore) where
  type InnerLore (SimpleM lore) = lore
  askEngineEnv = snd <$> ask
  localEngineEnv = local . second
  tellNeed = tell
  listenNeed = listen
  getEngineState   = fst <$> get
  putEngineState x = modify $ \(_, y) -> (x,y)
  passNeed = pass
  simplifyBody = Engine.defaultSimplifyBody

instance Proper lore => BindableM (SimpleM lore) where
  type Lore (SimpleM lore) = Aliases lore
  mkLetM pat e = do
    vtable <- Engine.getVtable
    simpl <- fst <$> ask
    (bnd, newbnds) <- runBinder'' $ mkLetS simpl vtable pat e
    mapM_ addBinding newbnds
    return bnd
  mkBodyM bnds res = do
    vtable <- Engine.getVtable
    simpl <- fst <$> ask
    (body, newbnds) <- runBinder'' $ mkBodyS simpl vtable bnds res
    mapM_ addBinding newbnds
    return body

runSimpleM :: SimpleM lore a
           -> Simplifiable lore
           -> Engine.Env (SimpleM lore)
           -> VNameSource
           -> (a, VNameSource)
runSimpleM (SimpleM m) simpl env src =
  let (x, (_, src'), _) = runRWS m (simpl, env) (Engine.emptyState, src)
  in (x, src')

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg :: Proper lore =>
                Simplifiable lore
             -> RuleBook (SimpleM lore)
             -> Prog lore
             -> Prog (Aliases lore)
simplifyProg simpl rules prog =
  Prog $ fst $ runSimpleM (mapM Engine.simplifyFun $ progFunctions prog)
               simpl (Engine.emptyEnv rules $ Just prog) namesrc
  where namesrc = newNameSourceForProg prog

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyFun :: (MonadFreshNames m, Proper lore) =>
               Simplifiable lore
            -> RuleBook (SimpleM lore)
            -> FunDec lore
            -> m (FunDec (Aliases lore))
simplifyFun simpl rules fundec =
  modifyNameSource $ runSimpleM (Engine.simplifyFun fundec) simpl $
  Engine.emptyEnv rules Nothing

-- | Simplify just a single 'Lambda'.
simplifyLambda :: (MonadFreshNames m, Proper lore) =>
                  Simplifiable lore
               -> RuleBook (SimpleM lore)
               -> Maybe (Prog lore) -> Lambda lore -> [Maybe SubExp]
               -> m (Lambda (Aliases lore))
simplifyLambda simpl rules prog lam args =
  modifyNameSource $ runSimpleM (Engine.simplifyLambda lam args) simpl $
  Engine.emptyEnv rules prog
