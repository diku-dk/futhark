{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, FlexibleContexts #-}
module Futhark.Optimise.Simplifier.Simple
       ( Engine.Simplifiable
       , SimpleOps (..)
       , SimpleM
       , bindableSimpleOps
       , runSimpleM
       )
  where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.RWS
import Control.Arrow (second)

import Prelude

import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import Futhark.Optimise.Simplifier.Lore (Wise)
import qualified Futhark.Analysis.SymbolTable as ST

data SimpleOps m =
  SimpleOps { mkLetS :: ST.SymbolTable (Lore m)
                        -> Pattern (Lore m) -> Exp (Lore m)
                        -> m (Binding (Lore m))
            , mkBodyS :: ST.SymbolTable (Lore m)
                         -> [Binding (Lore m)] -> Result
                         -> m (Body (Lore m))
            , mkLetNamesS :: ST.SymbolTable (Lore m)
                             -> [(VName,Bindage)] -> Exp (Lore m)
                             -> m (Binding (Lore m))
            , simplifyLetBoundLore :: Lore.LetBound (Engine.InnerLore m)
                                      -> m (Lore.LetBound (Engine.InnerLore m))
            , simplifyFParamLore :: Lore.FParam (Engine.InnerLore m)
                                    -> m (Lore.FParam (Engine.InnerLore m))
            , simplifyRetType :: Lore.RetType (Engine.InnerLore m)
                                 -> m (Lore.RetType (Engine.InnerLore m))
            }

bindableSimpleOps :: (Engine.MonadEngine m,
                      Bindable (Engine.InnerLore m),
                      Lore.LetBound (Engine.InnerLore m) ~ (),
                      Lore.FParam (Engine.InnerLore m) ~ (),
                      RetType (Engine.InnerLore m) ~ ExtRetType) =>
                     SimpleOps m
bindableSimpleOps =
  SimpleOps mkLetS' mkBodyS' mkLetNamesS'
  return return simplifyRetType'
  where mkLetS' _ pat e = return $ mkLet pat' e
          where pat' = [ (patElemIdent patElem, patElemBindage patElem)
                       | patElem <- patternElements pat ]
        mkBodyS' _ bnds res = return $ mkBody bnds res
        mkLetNamesS' _ = mkLetNames
        simplifyRetType' =
          liftM ExtRetType . mapM Engine.simplifyExtType . retTypeValues

newtype SimpleM lore a =
  SimpleM (RWS
           (SimpleOps (SimpleM lore), Engine.Env (SimpleM lore)) -- Reader
           (Engine.Need (Wise lore))                             -- Writer
           (Engine.State (SimpleM lore), NameSource VName)       -- State
           a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Engine.Need (Wise lore)),
            MonadReader (SimpleOps (SimpleM lore), Engine.Env (SimpleM lore)),
            MonadState (Engine.State (SimpleM lore), NameSource VName))

instance MonadFreshNames (SimpleM lore) where
  getNameSource   = snd <$> get
  putNameSource y = modify $ \(x, _) -> (x,y)

instance Engine.Simplifiable lore =>
         HasTypeEnv (SimpleM lore) where
  askTypeEnv = ST.typeEnv <$> Engine.getVtable
  lookupType name = do
    vtable <- Engine.getVtable
    case ST.lookupType name vtable of
      Just t -> return t
      Nothing -> fail $
                 "SimpleM.lookupType: cannot find variable " ++
                 pretty name ++ " in symbol table."

instance Engine.Simplifiable lore =>
         MonadBinder (SimpleM lore) where
  type Lore (SimpleM lore) = Wise lore
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

  addBinding      = Engine.addBindingEngine
  collectBindings = Engine.collectBindingsEngine

instance Engine.Simplifiable lore =>
         Engine.MonadEngine (SimpleM lore) where
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
  simplifyRetType restype = do
    simpl <- fst <$> ask
    simplifyRetType simpl restype

runSimpleM :: SimpleM lore a
           -> SimpleOps (SimpleM lore)
           -> Engine.Env (SimpleM lore)
           -> VNameSource
           -> (a, VNameSource)
runSimpleM (SimpleM m) simpl env src =
  let (x, (_, src'), _) = runRWS m (simpl, env) (Engine.emptyState, src)
  in (x, src')
