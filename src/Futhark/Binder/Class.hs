{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- | This module defines a convenience typeclass for creating
-- normalised programs.
module Futhark.Binder.Class
  ( Proper
  , Bindable (..)
  , mkLet'
  , mkLetNames'
  , MonadBinder (..)
  , mkLetNamesM'
  , bodyBindings
  , insertBindings
  , insertBinding
  , letBind
  , letBind_
  , letBindNames
  , letBindNames'
  , letBindNames_
  , letBindNames'_
  , collectBindings_
  , bodyBind
  )
where

import Control.Applicative
import Control.Monad.Writer

import Prelude

import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.MonadFreshNames
import Futhark.Transform.Substitute
import Futhark.Transform.Rename (Renameable)

-- | A lore that supports some basic facilities.
class (Lore.Lore lore,
       PrettyLore lore,
       Renameable lore, Substitutable lore,
       FreeIn (Annotations.Exp lore),
       FreeIn (Annotations.LetBound lore),
       FreeIn (Annotations.Body lore),
       FreeIn (Annotations.FParam lore),
       FreeIn (Annotations.LParam lore),
       FreeIn (Annotations.RetType lore),
       FreeIn (Annotations.Op lore),
       Typed (Annotations.FParam lore),
       Typed (Annotations.LParam lore),
       IsOp (Op lore),
       IsRetType (RetType lore)) => Proper lore where

-- | The class of lores that can be constructed solely from an
-- expression, within some monad.  Very important: the methods should
-- not have any significant side effects!  They may be called more
-- often than you think, and the results thrown away.  If used
-- exclusively within a 'MonadBinder' instance, it is acceptable for
-- them to create new bindings, however.
class (Proper lore,
       Annotations.FParam lore ~ DeclType,
       Annotations.LParam lore ~ Type,
       Annotations.RetType lore ~ ExtRetType,
       SetType (Annotations.LetBound lore)) =>
      Bindable lore where
  mkLet :: [(Ident,Bindage)] -> [(Ident,Bindage)] -> Exp lore -> Binding lore
  mkBody :: [Binding lore] -> Result -> Body lore
  mkLetNames :: (MonadFreshNames m, HasTypeEnv m) =>
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
       MonadFreshNames m, Applicative m, Monad m,
       HasTypeEnv m) =>
      MonadBinder m where
  type Lore m :: *
  mkLetM :: Pattern (Lore m) -> Exp (Lore m) -> m (Binding (Lore m))
  mkBodyM :: [Binding (Lore m)] -> Result -> m (Body (Lore m))
  mkLetNamesM :: [(VName, Bindage)] -> Exp (Lore m) -> m (Binding (Lore m))
  addBinding      :: Binding (Lore m) -> m ()
  collectBindings :: m a -> m (a, [Binding (Lore m)])

letBind :: MonadBinder m =>
           Pattern (Lore m) -> Exp (Lore m) -> m [Ident]
letBind pat e = do
  bnd <- mkLetM pat e
  addBinding bnd
  return $ patternValueIdents $ bindingPattern bnd

letBind_ :: MonadBinder m =>
            Pattern (Lore m) -> Exp (Lore m) -> m ()
letBind_ pat e = void $ letBind pat e

mkLet' :: Bindable lore =>
          [Ident] -> [Ident] -> Exp lore -> Binding lore
mkLet' context values = mkLet (map addBindVar context) (map addBindVar values)
  where addBindVar name = (name, BindVar)

mkLetNamesM' :: MonadBinder m =>
                [VName] -> Exp (Lore m) -> m (Binding (Lore m))
mkLetNamesM' = mkLetNamesM . map addBindVar
  where addBindVar name = (name, BindVar)

mkLetNames' :: (Bindable lore, MonadFreshNames m, HasTypeEnv m) =>
               [VName] -> Exp lore -> m (Binding lore)
mkLetNames' = mkLetNames . map addBindVar
  where addBindVar name = (name, BindVar)

letBindNames :: MonadBinder m =>
                [(VName,Bindage)] -> Exp (Lore m) -> m [Ident]
letBindNames names e = do
  bnd <- mkLetNamesM names e
  addBinding bnd
  return $ patternValueIdents $ bindingPattern bnd

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

collectBindings_ :: MonadBinder m => m a -> m [Binding (Lore m)]
collectBindings_ = liftM snd . collectBindings


bodyBind :: MonadBinder m => Body (Lore m) -> m [SubExp]
bodyBind (Body _ bnds es) = do
  mapM_ addBinding bnds
  return es

-- | Add several bindings at the outermost level of a 'Body'.
insertBindings :: Bindable lore => [Binding lore] -> Body lore -> Body lore
insertBindings bnds1 (Body _ bnds2 res) =
  mkBody (bnds1++bnds2) res

-- | Add a single binding at the outermost level of a 'Body'.
insertBinding :: Bindable lore => Binding lore -> Body lore -> Body lore
insertBinding bnd = insertBindings [bnd]
