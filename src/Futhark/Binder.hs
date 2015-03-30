{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeFamilies #-}
-- | This module defines a convenience monad/typeclass for creating
-- normalised programs.
module Futhark.Binder
  ( Proper
  , Bindable (..)
  , mkLet'
  , mkLetNames'
  , MonadBinder (..)
  , mkLetNamesM'
  , bodyBindings
  , insertBindings
  , insertBinding
  , valueIdents
  , letBind
  , letBind_
  , letBindNames
  , letBindNames'
  , letBindNames_
  , letBindNames'_
  , bodyBind
  -- * A concrete @Binder@ monad.
  , BinderT
  , runBinderT
  , Binder
  , runBinder
  , runBinder'
  , runBinder''
  , runBinderWithNameSource
  -- * Writer convenience interface
  , addBindingWriter
  , collectBindingsWriter
  )
where

import qualified Data.DList as DL
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State

import Prelude

import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Substitute
import Futhark.Renamer (Renameable)

-- | A lore that supports some basic facilities.
class (Lore.Lore lore, PrettyLore lore,
       Renameable lore, Substitutable lore,
       FreeIn (Lore.Exp lore),
       FreeIn (Lore.LetBound lore),
       FreeIn (Lore.Body lore),
       FreeIn (Lore.FParam lore),
       FreeIn (Lore.RetType lore),
       IsRetType (RetType lore)) => Proper lore where

-- | The class of lores that can be constructed solely from an
-- expression, non-monadically, except for generating fresh names.
class (Proper lore, Lore.FParam lore ~ ()) => Bindable lore where
  mkLet :: [(Ident,Bindage)] -> Exp lore -> Binding lore
  mkBody :: [Binding lore] -> Result -> Body lore
  mkLetNames :: MonadFreshNames m =>
                [(VName, Bindage)] -> Exp lore -> m (Binding lore)

-- | A monad that supports the creation of bindings from expressions
-- and bodies from bindings, with a specific lore.  This is the main
-- typeclass that a monad must implement in order for it to be useful
-- for generating or modifying Futhark code.
--
-- Very important: the methods should not have any significant side
-- effects!  They may be called more often than you think, and the
-- results thrown away.  It is acceptable for them to create new
-- bindings, however.
class (Proper (Lore m),
       MonadFreshNames m, Applicative m, Monad m) =>
      MonadBinder m where
  type Lore m :: *
  mkLetM :: Pattern (Lore m) -> Exp (Lore m) -> m (Binding (Lore m))
  mkBodyM :: [Binding (Lore m)] -> Result -> m (Body (Lore m))
  mkLetNamesM :: [(VName, Bindage)] -> Exp (Lore m) -> m (Binding (Lore m))
  addBinding      :: Binding (Lore m) -> m ()
  collectBindings :: m a -> m (a, [Binding (Lore m)])

valueIdents :: Proper lore => Pattern lore -> Exp lore -> [Ident]
valueIdents pat e =
  snd $
  splitAt (patternSize pat - length (expExtType e)) $
  patternIdents pat

letBind :: MonadBinder m =>
           Pattern (Lore m) -> Exp (Lore m) -> m [Ident]
letBind pat e = do
  bnd <- mkLetM pat e
  addBinding bnd
  return $ valueIdents (bindingPattern bnd) e

letBind_ :: MonadBinder m =>
            Pattern (Lore m) -> Exp (Lore m) -> m ()
letBind_ pat e = void $ letBind pat e

mkLet' :: Bindable lore =>
          [Ident] -> Exp lore -> Binding lore
mkLet' = mkLet . map addBindVar
  where addBindVar name = (name, BindVar)

mkLetNamesM' :: MonadBinder m =>
                [VName] -> Exp (Lore m) -> m (Binding (Lore m))
mkLetNamesM' = mkLetNamesM . map addBindVar
  where addBindVar name = (name, BindVar)

mkLetNames' :: (Bindable lore, MonadFreshNames m) =>
               [VName] -> Exp lore -> m (Binding lore)
mkLetNames' = mkLetNames . map addBindVar
  where addBindVar name = (name, BindVar)

letBindNames :: MonadBinder m =>
                [(VName,Bindage)] -> Exp (Lore m) -> m [Ident]
letBindNames names e = do
  bnd <- mkLetNamesM names e
  addBinding bnd
  return $ valueIdents (bindingPattern bnd) e

letBindNames' :: MonadBinder m =>
                 [VName] -> Exp (Lore m) -> m [Ident]
letBindNames' = letBindNames . map addBindVar
  where addBindVar name = (name, BindVar)

letBindNames_ :: MonadBinder m =>
                [(VName,Bindage)] -> Exp (Lore m) -> m ()
letBindNames_ names e = void $ letBindNames names e

letBindNames'_ :: MonadBinder m =>
                  [VName] -> Exp (Lore m) -> m ()
letBindNames'_ names e = void $ letBindNames' names e

bodyBind :: MonadBinder m => Body (Lore m) -> m [SubExp]
bodyBind (Body _ bnds (Result es)) = do
  mapM_ addBinding bnds
  return es

-- | Add several bindings at the outermost level of a 'Body'.
insertBindings :: Bindable lore => [Binding lore] -> Body lore -> Body lore
insertBindings bnds1 (Body _ bnds2 res) =
  mkBody (bnds1++bnds2) res

-- | Add a single binding at the outermost level of a 'Body'.
insertBinding :: Bindable lore => Binding lore -> Body lore -> Body lore
insertBinding bnd = insertBindings [bnd]

addBindingWriter :: (MonadFreshNames m, Applicative m, MonadWriter (DL.DList (Binding lore)) m) =>
                    Binding lore -> m ()
addBindingWriter = tell . DL.singleton

collectBindingsWriter :: (MonadFreshNames m,
                          Applicative m,
                          MonadWriter (DL.DList (Binding lore)) m) =>
                         m a -> m (a, [Binding lore])
collectBindingsWriter m = pass $ do
                            (x, bnds) <- listen m
                            return ((x, DL.toList bnds), const DL.empty)

newtype BinderT lore m a = BinderT (WriterT
                                    (DL.DList (Binding lore))
                                    m
                                    a)
  deriving (Functor, Monad, Applicative,
            MonadWriter (DL.DList (Binding lore)))

instance MonadTrans (BinderT lore) where
  lift = BinderT . lift

type Binder lore = BinderT lore (State VNameSource)

instance MonadFreshNames m => MonadFreshNames (BinderT lore m) where
  getNameSource = BinderT . lift $ getNameSource
  putNameSource = BinderT . lift . putNameSource

instance (Proper lore, Bindable lore, MonadFreshNames m) =>
         MonadBinder (BinderT lore m) where
  type Lore (BinderT lore m) = lore
  mkBodyM bnds res = return $ mkBody bnds res
  mkLetM pat e = return $ mkLet pat' e
    where pat' = [ (patElemIdent patElem, patElemBindage patElem)
                 | patElem <- patternElements pat ]
  mkLetNamesM = mkLetNames
  addBinding      = addBindingWriter
  collectBindings = collectBindingsWriter

runBinderT :: Monad m =>
              BinderT lore m a
           -> m (a, [Binding lore])
runBinderT (BinderT m) = do (x, bnds) <- runWriterT m
                            return (x, DL.toList bnds)

runBinder :: (Bindable lore, MonadFreshNames m) =>
             Binder lore (Body lore) -> m (Body lore)
runBinder m = do
  (b, f) <- runBinder' m
  return $ f b

runBinder' :: (MonadFreshNames m, Bindable lore) =>
              Binder lore a -> m (a, Body lore -> Body lore)
runBinder' m = do
  (x, bnds) <- runBinder'' m
  return (x, insertBindings bnds)

runBinder'' :: MonadFreshNames m => Binder lore a -> m (a, [Binding lore])
runBinder'' = modifyNameSource . runBinderWithNameSource

runBinderWithNameSource :: Binder lore a -> VNameSource
                        -> ((a, [Binding lore]), VNameSource)
runBinderWithNameSource (BinderT m) src =
  let ((x,bnds),src') = runState (runWriterT m) src
  in ((x, DL.toList bnds), src')
