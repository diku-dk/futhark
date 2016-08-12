{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Optimise.Simplifier.Simple
       ( SimpleOps (..)
       , SimpleM
       , bindableSimpleOps
       , runSimpleM
       , subSimpleM
       , Wise
       )
  where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.RWS
import Control.Arrow (second)

import Prelude

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
            }

bindableSimpleOps :: (Engine.MonadEngine m,
                      Bindable (Engine.InnerLore m)) =>
                     SimpleOps m
bindableSimpleOps =
  SimpleOps mkLetS' mkBodyS' mkLetNamesS'
  where mkLetS' _ pat e = return $
                          mkLet (map asPair $ patternContextElements pat)
                          (map asPair $ patternValueElements pat)
                          e
          where asPair patElem = (patElemIdent patElem, patElemBindage patElem)
        mkBodyS' _ bnds res = return $ mkBody bnds res
        mkLetNamesS' _ = mkLetNames

newtype SimpleM lore a =
  SimpleM (RWS
           (SimpleOps (SimpleM lore), Engine.Env (SimpleM lore)) -- Reader
           (Engine.Need (Wise lore))                             -- Writer
           (Engine.State (SimpleM lore), VNameSource)       -- State
           a)
  deriving (Applicative, Functor, Monad,
            MonadWriter (Engine.Need (Wise lore)),
            MonadReader (SimpleOps (SimpleM lore), Engine.Env (SimpleM lore)),
            MonadState (Engine.State (SimpleM lore), VNameSource))

instance MonadFreshNames (SimpleM lore) where
  getNameSource   = snd <$> get
  putNameSource y = modify $ \(x, _) -> (x,y)

instance Engine.MonadEngine (SimpleM lore) =>
         HasScope (Wise lore) (SimpleM lore) where
  askScope = ST.typeEnv <$> Engine.getVtable
  lookupType name = do
    vtable <- Engine.getVtable
    case ST.lookupType name vtable of
      Just t -> return t
      Nothing -> fail $
                 "SimpleM.lookupType: cannot find variable " ++
                 pretty name ++ " in symbol table."

instance Engine.MonadEngine (SimpleM lore) =>
         LocalScope (Wise lore) (SimpleM lore) where
  localScope types = Engine.localVtable (<>ST.fromScope types)

instance Engine.MonadEngine (SimpleM lore) => MonadBinder (SimpleM lore) where
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

instance (Attributes lore,
          Engine.Simplifiable (LetAttr lore),
          Engine.Simplifiable (FParamAttr lore),
          Engine.Simplifiable (LParamAttr lore),
          Engine.Simplifiable (RetType lore),
          Engine.SimplifiableOp lore (Op lore)) => Engine.MonadEngine (SimpleM lore) where
  type InnerLore (SimpleM lore) = lore
  askEngineEnv = snd <$> ask
  localEngineEnv = local . second
  tellNeed = tell
  listenNeed = listen
  getEngineState   = fst <$> get
  putEngineState x = modify $ \(_, y) -> (x,y)
  passNeed = pass

runSimpleM :: SimpleM lore a
           -> SimpleOps (SimpleM lore)
           -> Engine.Env (SimpleM lore)
           -> VNameSource
           -> (a, VNameSource)
runSimpleM (SimpleM m) simpl env src =
  let (x, (_, src'), _) = runRWS m (simpl, env) (Engine.emptyState, src)
  in (x, src')

subSimpleM :: (MonadFreshNames m,
               Engine.MonadEngine (SimpleM lore),
               SameScope outerlore lore,
               ExpAttr outerlore ~ ExpAttr lore,
               BodyAttr outerlore ~ BodyAttr lore,
               RetType outerlore ~ RetType lore) =>
              SimpleOps (SimpleM lore)
           -> Engine.Env (SimpleM lore)
           -> ST.SymbolTable (Wise outerlore)
           -> SimpleM lore a
           -> m (a, [Binding (Wise lore)])
subSimpleM simpl env outer_vtable m = do
  let inner_vtable = ST.castSymbolTable outer_vtable
  modifyNameSource $ \src ->
    let SimpleM m' = Engine.localVtable (<>inner_vtable) m
        (x, (_, src'), need) =
          runRWS m' (simpl, env) (Engine.emptyState, src)
    in ((x, Engine.needBindings need), src')
