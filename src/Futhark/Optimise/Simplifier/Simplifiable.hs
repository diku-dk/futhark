{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Futhark.Optimise.Simplifier.Simplifiable
       ( Simplifiable (..)
       , simplifyProg
       , simplifyOneFun
       , simplifyOneLambda
       )
where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.RWS

import Futhark.Representation.Aliases (Aliases)
import qualified Futhark.Representation.Aliases as Aliases
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Optimise.Simplifier.Engine
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST

class Proper lore => Simplifiable lore where
  mkLetS :: ST.SymbolTable (Aliases lore)
         -> [Ident] -> Aliases.Exp lore
         -> Binder (Aliases lore) (Aliases.Binding lore)
  mkBodyS :: ST.SymbolTable (Aliases lore)
          -> [Aliases.Binding lore] -> Result
          -> Binder (Aliases lore) (Aliases.Body lore)

newtype SimpleM lore a = SimpleM (RWS
                                  (Env (SimpleM lore))                     -- Reader
                                  (Need (Aliases lore))                    -- Writer
                                  (State (SimpleM lore), NameSource VName) -- State
                                  a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Need (Aliases lore)),
            MonadReader (Env (SimpleM lore)),
            MonadState (State (SimpleM lore), NameSource VName))

instance MonadFreshNames (SimpleM lore) where
  getNameSource   = snd <$> get
  putNameSource y = modify $ \(x, _) -> (x,y)

instance (Proper lore, Simplifiable lore) =>
         MonadBinder (SimpleM lore) where
  addBinding      = addBindingEngine
  collectBindings = collectBindingsEngine

instance (Proper lore, Simplifiable lore) =>
         MonadEngine (SimpleM lore) where
  type InnerLore (SimpleM lore) = lore
  askEngineEnv = ask
  localEngineEnv = local
  tellNeed = tell
  listenNeed = listen
  getEngineState   = fst <$> get
  putEngineState x = modify $ \(_, y) -> (x,y)
  passNeed = pass
  simplifyBody = defaultSimplifyBody

instance (Proper lore, Simplifiable lore) => BindableM (SimpleM lore) where
  type Lore (SimpleM lore) = Aliases lore
  mkLetM pat e = do
    vtable <- getVtable
    (bnd, newbnds) <- runBinder'' $ mkLetS vtable pat e
    mapM_ addBinding newbnds
    return bnd
  mkBodyM bnds res = do
    vtable <- getVtable
    (body, newbnds) <- runBinder'' $ mkBodyS vtable bnds res
    mapM_ addBinding newbnds
    return body

runSimpleM :: SimpleM lore a
           -> Env (SimpleM lore)
           -> VNameSource
           -> (a, VNameSource)
runSimpleM (SimpleM m) env src = let (x, (_, src'), _) = runRWS m env (emptyState, src)
                                 in (x, src')

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg :: (Proper lore, Simplifiable lore) =>
                RuleBook (SimpleM lore)
             -> Prog lore -> Prog (Aliases lore)
simplifyProg rules prog =
  Prog $ fst $ runSimpleM (mapM simplifyFun $ progFunctions prog)
               (emptyEnv rules $ Just prog) namesrc
  where namesrc = newNameSourceForProg prog

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyOneFun :: (MonadFreshNames m, Simplifiable lore) =>
                  RuleBook (SimpleM lore)
               -> FunDec lore -> m (FunDec (Aliases lore))
simplifyOneFun rules fundec =
  modifyNameSource $ runSimpleM (simplifyFun fundec) (emptyEnv rules Nothing)

-- | Simplify just a single 'Lambda'.
simplifyOneLambda :: (MonadFreshNames m, Simplifiable lore) =>
                     RuleBook (SimpleM lore)
                  -> Maybe (Prog lore) -> Lambda lore -> [Maybe SubExp]
                  -> m (Lambda (Aliases lore))
simplifyOneLambda rules prog lam args =
  modifyNameSource $ runSimpleM (simplifyLambda lam args) (emptyEnv rules prog)
