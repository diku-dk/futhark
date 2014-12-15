{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Futhark.Optimise.Simplifier.Simplifiable
       ( Simplifiable (..)
       , bindableSimplifiable
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
import Control.Arrow (second)

import Futhark.Representation.Aliases (Aliases)
import qualified Futhark.Representation.Aliases as Aliases
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Optimise.Simplifier.Engine
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST

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
                                  (Simplifiable lore, Env (SimpleM lore)) -- Reader
                                  (Need (Aliases lore))                     -- Writer
                                  (State (SimpleM lore), NameSource VName)  -- State
                                  a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Need (Aliases lore)),
            MonadReader (Simplifiable lore, Env (SimpleM lore)),
            MonadState (State (SimpleM lore), NameSource VName))

instance MonadFreshNames (SimpleM lore) where
  getNameSource   = snd <$> get
  putNameSource y = modify $ \(x, _) -> (x,y)

instance Proper lore => MonadBinder (SimpleM lore) where
  addBinding      = addBindingEngine
  collectBindings = collectBindingsEngine

instance Proper lore => MonadEngine (SimpleM lore) where
  type InnerLore (SimpleM lore) = lore
  askEngineEnv = snd <$> ask
  localEngineEnv = local . second
  tellNeed = tell
  listenNeed = listen
  getEngineState   = fst <$> get
  putEngineState x = modify $ \(_, y) -> (x,y)
  passNeed = pass
  simplifyBody = defaultSimplifyBody

instance Proper lore => BindableM (SimpleM lore) where
  type Lore (SimpleM lore) = Aliases lore
  mkLetM pat e = do
    vtable <- getVtable
    simpl <- fst <$> ask
    (bnd, newbnds) <- runBinder'' $ mkLetS simpl vtable pat e
    mapM_ addBinding newbnds
    return bnd
  mkBodyM bnds res = do
    vtable <- getVtable
    simpl <- fst <$> ask
    (body, newbnds) <- runBinder'' $ mkBodyS simpl vtable bnds res
    mapM_ addBinding newbnds
    return body

runSimpleM :: SimpleM lore a
           -> Simplifiable lore
           -> Env (SimpleM lore)
           -> VNameSource
           -> (a, VNameSource)
runSimpleM (SimpleM m) simpl env src =
  let (x, (_, src'), _) = runRWS m (simpl, env) (emptyState, src)
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
  Prog $ fst $ runSimpleM (mapM simplifyFun $ progFunctions prog)
               simpl (emptyEnv rules $ Just prog) namesrc
  where namesrc = newNameSourceForProg prog

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyOneFun :: (MonadFreshNames m, Proper lore) =>
                  Simplifiable lore
               -> RuleBook (SimpleM lore)
               -> FunDec lore
               -> m (FunDec (Aliases lore))
simplifyOneFun simpl rules fundec =
  modifyNameSource $ runSimpleM (simplifyFun fundec) simpl $
  emptyEnv rules Nothing

-- | Simplify just a single 'Lambda'.
simplifyOneLambda :: (MonadFreshNames m, Proper lore) =>
                     Simplifiable lore
                  -> RuleBook (SimpleM lore)
                  -> Maybe (Prog lore) -> Lambda lore -> [Maybe SubExp]
                  -> m (Lambda (Aliases lore))
simplifyOneLambda simpl rules prog lam args =
  modifyNameSource $ runSimpleM (simplifyLambda lam args) simpl $
  emptyEnv rules prog
