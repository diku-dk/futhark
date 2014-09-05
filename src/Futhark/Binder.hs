{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies, ScopedTypeVariables #-}
-- | This module defines a convenience monad/typeclass for creating
-- normalised programs.
module Futhark.Binder
  ( Proper
  , BindableM (..)
  , Bindable (..)
  , mkLet
  , mkLetPat
  , mkLetM
  , mkLetPatM
  , mkPattern
  , mkPatternM
  , MonadBinder (..)
  , ProperBinder
  , bodyBindings
  , insertBindingsM
  , letBind
  , letBindPat
  , letWithBind
  , loopBind
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
import Data.Loc
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State

import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Substitute
import Futhark.Renamer (Renameable)

-- | A lore that supports some basic facilities.
class (Lore.Lore lore,
       Renameable lore, Substitutable lore,
       FreeIn (Lore.Exp lore),
       FreeIn (Lore.Binding lore)) => Proper lore where

-- | The class of lores that can be constructed solely from an
-- expression, within some monad.  Very important: the methods should
-- not have any significant side effects!  They may be called more
-- often than you think, and the results thrown away.  If used
-- exclusively within a 'MonadBinder' instance, it is acceptable for
-- them to create new bindings, however.
class (Monad m, Applicative m) =>
      BindableM m where
  type Lore m :: *
  loreForExpM :: Exp (Lore m) -> m (Lore.Exp (Lore m))
  loreForBindingM :: Ident -> m (Lore.Binding (Lore m))

-- | The class of lores that can be constructed solely from an
-- expression, non-monadically.
class Lore.Lore lore => Bindable lore where
  loreForExp :: Exp lore -> Lore.Exp lore
  loreForBinding :: Const Ident lore -> Lore.Binding lore

mkPattern :: forall lore.Bindable lore => [Ident] -> Pattern lore
mkPattern vs =
  Pattern $ zipWith Bindee vs $ map (loreForBinding . tagLore) vs
  where tagLore :: Ident -> Const Ident lore
        tagLore = Const

mkPatternM :: BindableM m => [Ident] -> m (Pattern (Lore m))
mkPatternM idents =
  Pattern <$> zipWith Bindee idents <$> mapM loreForBindingM idents

mkLet :: Bindable lore => [Ident] -> Exp lore -> Binding lore
mkLet pat e = Let (mkPattern pat) (loreForExp e) e

mkLetPat :: Bindable lore => Pattern lore -> Exp lore -> Binding lore
mkLetPat pat e = Let pat (loreForExp e) e

mkLetM :: BindableM m =>
          [Ident] -> Exp (Lore m) -> m (Binding (Lore m))
mkLetM pat e = Let <$> mkPatternM pat <*> loreForExpM e <*> pure e

mkLetPatM :: BindableM m =>
             Pattern (Lore m) -> Exp (Lore m) -> m (Binding (Lore m))
mkLetPatM pat e = Let pat <$> loreForExpM e <*> pure e

class (BindableM m,
       MonadFreshNames m, Applicative m, Monad m) =>
      MonadBinder m where
  addBinding      :: Binding (Lore m) -> m ()
  collectBindings :: m a -> m (a, [Binding (Lore m)])

class (MonadBinder m, Proper (Lore m)) => ProperBinder m where

letBind :: MonadBinder m =>
           [Ident] -> Exp (Lore m) -> m ()
letBind pat e = do
  pat' <- mkPatternM pat
  letBindPat pat' e

letBindPat :: MonadBinder m =>
           Pattern (Lore m) -> Exp (Lore m) -> m ()
letBindPat pat e = do
  lore <- loreForExpM e
  addBinding $ Let pat lore e

letWithBind :: MonadBinder m =>
               Certificates -> Ident -> Ident -> [SubExp] -> SubExp -> m ()
letWithBind cs dest src idxs ve =
  letBind [dest] $ Update cs src idxs ve $ srclocOf src

loopBind :: MonadBinder m =>
            [(Ident, SubExp)] -> Ident -> SubExp -> Body (Lore m) -> m ()
loopBind merge i bound loopbody =
  letBind mergepat $
  DoLoop mergepat merge i bound loopbody $ srclocOf i
  where mergepat = map fst merge

bodyBind :: MonadBinder m => Body (Lore m) -> m [SubExp]
bodyBind (Body bnds (Result _ es _)) = do
  mapM_ addBinding bnds
  return es

-- | Evaluate the action, producing a body, then wrap it in all the
-- bindings it created using 'addBinding'.
insertBindingsM :: MonadBinder m => m (Body (Lore m)) -> m (Body (Lore m))
insertBindingsM m = do
  (e,bnds) <- collectBindings m
  return $ insertBindings bnds e

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

type Binder lore = BinderT lore (State VNameSource)

instance MonadFreshNames m => MonadFreshNames (BinderT lore m) where
  getNameSource = BinderT . lift $ getNameSource
  putNameSource = BinderT . lift . putNameSource

instance (Proper lore, Bindable lore, Monad m, Applicative m) => BindableM (BinderT lore m) where
  type Lore (BinderT lore m) = lore
  loreForExpM = return . loreForExp
  loreForBindingM = return . loreForBinding . (Const :: Ident -> Const Ident lore)

instance (Proper lore, Bindable lore, MonadFreshNames m) => MonadBinder (BinderT lore m) where
  addBinding      = addBindingWriter
  collectBindings = collectBindingsWriter

runBinderT :: Monad m =>
              BinderT lore m a
           -> m (a, [Binding lore])
runBinderT (BinderT m) = do (x, bnds) <- runWriterT m
                            return (x, DL.toList bnds)

runBinder :: MonadFreshNames m => Binder lore (Body lore)
          -> m (Body lore)
runBinder m = do
  (b, f) <- runBinder' m
  return $ f b

runBinder' :: MonadFreshNames m => Binder lore a
           -> m (a, Body lore -> Body lore)
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
