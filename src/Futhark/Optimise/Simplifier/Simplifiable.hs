{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}
module Futhark.Optimise.Simplifier.Simplifiable
  ( Simplifiable (..)
  , SimpleM
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

import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import Futhark.Optimise.Simplifier.Rule
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Representation.Aliases (Aliases)

data Simplifiable m =
  Simplifiable { mkLetS :: ST.SymbolTable (Lore m)
                        -> Pattern (Lore m) -> Exp (Lore m)
                        -> m (Binding (Lore m))
               , mkBodyS :: ST.SymbolTable (Lore m)
                         -> [Binding (Lore m)] -> Result
                         -> m (Body (Lore m))
               , mkLetNamesS :: ST.SymbolTable (Lore m)
                             -> [VName] -> Exp (Lore m)
                             -> m (Binding (Lore m))
               , simplifyLetBoundLore :: Lore.LetBound (Engine.InnerLore m)
                                      -> m (Lore.LetBound (Engine.InnerLore m))
               , simplifyFParamLore :: Lore.FParam (Engine.InnerLore m)
                                    -> m (Lore.FParam (Engine.InnerLore m))
               , simplifyResType :: Lore.ResType (Engine.InnerLore m)
                                 -> m (Lore.ResType (Engine.InnerLore m))
               , computeBranchReturnType :: Body (Lore m) -> Body (Lore m)
                                         -> m (ResType (Engine.InnerLore m))
               }

bindableSimplifiable :: (Engine.MonadEngine m,
                         Bindable (Engine.InnerLore m),
                         Lore.LetBound (Engine.InnerLore m) ~ (),
                         Lore.FParam (Engine.InnerLore m) ~ (),
                         ResType (Engine.InnerLore m) ~ [ExtType]) =>
                        Simplifiable m
bindableSimplifiable =
  Simplifiable mkLetS' mkBodyS' mkLetNamesS'
  return return (mapM Engine.simplifyExtType) computeBranchReturnType'
  where mkLetS' _ pat e = return $ mkLet (patternIdents pat) e
        mkBodyS' _ bnds res = return $ mkBody bnds res
        mkLetNamesS' _ = mkLetNames
        computeBranchReturnType' b1 b2 =
          return $ bodyExtType b1 `generaliseExtTypes` bodyExtType b2

newtype SimpleM lore a =
  SimpleM (RWS
           (Simplifiable (SimpleM lore), Engine.Env (SimpleM lore)) -- Reader
           (Engine.Need (Aliases lore))                             -- Writer
           (Engine.State (SimpleM lore), NameSource VName)          -- State
           a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Engine.Need (Aliases lore)),
            MonadReader (Simplifiable (SimpleM lore), Engine.Env (SimpleM lore)),
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
  simplifyLetBoundLore lore = do
    simpl <- fst <$> ask
    simplifyLetBoundLore simpl lore
  simplifyFParamLore lore = do
    simpl <- fst <$> ask
    simplifyFParamLore simpl lore
  simplifyResType restype = do
    simpl <- fst <$> ask
    simplifyResType simpl restype

instance Proper lore => BindableM (SimpleM lore) where
  type Lore (SimpleM lore) = Aliases lore
  mkLetM pat e = do
    vtable <- Engine.getVtable
    simpl <- fst <$> ask
    mkLetS simpl vtable pat e
  mkBodyM bnds res = do
    vtable <- Engine.getVtable
    simpl <- fst <$> ask
    mkBodyS simpl vtable bnds res
  mkLetNamesM names e = do
    vtable <- Engine.getVtable
    simpl <- fst <$> ask
    mkLetNamesS simpl vtable names e
  branchReturnTypeM b1 b2 = do
    simpl <- fst <$> ask
    computeBranchReturnType simpl b1 b2

runSimpleM :: SimpleM lore a
           -> Simplifiable (SimpleM lore)
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
                Simplifiable (SimpleM lore)
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
               Simplifiable (SimpleM lore)
            -> RuleBook (SimpleM lore)
            -> FunDec lore
            -> m (FunDec (Aliases lore))
simplifyFun simpl rules fundec =
  modifyNameSource $ runSimpleM (Engine.simplifyFun fundec) simpl $
  Engine.emptyEnv rules Nothing

-- | Simplify just a single 'Lambda'.
simplifyLambda :: (MonadFreshNames m, Proper lore) =>
                  Simplifiable (SimpleM lore)
               -> RuleBook (SimpleM lore)
               -> Maybe (Prog lore) -> Lambda lore -> [Maybe SubExp]
               -> m (Lambda (Aliases lore))
simplifyLambda simpl rules prog lam args =
  modifyNameSource $ runSimpleM (Engine.simplifyLambda lam args) simpl $
  Engine.emptyEnv rules prog
