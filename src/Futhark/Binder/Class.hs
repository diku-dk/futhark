{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- | This module defines a convenience typeclass for creating
-- normalised programs.
module Futhark.Binder.Class
  ( Bindable (..)
  , mkLet
  , MonadBinder (..)
  , mkLetM
  , bodyStms
  , insertStms
  , insertStm
  , letBind
  , letBind_
  , letBindNames
  , letBindNames_
  , collectStms_
  , bodyBind

  , module Futhark.MonadFreshNames
  )
where

import Control.Monad.Writer
import qualified Control.Monad.Fail as Fail
import qualified Data.Kind

import Futhark.Representation.AST
import Futhark.MonadFreshNames

-- | The class of lores that can be constructed solely from an
-- expression, within some monad.  Very important: the methods should
-- not have any significant side effects!  They may be called more
-- often than you think, and the results thrown away.  If used
-- exclusively within a 'MonadBinder' instance, it is acceptable for
-- them to create new bindings, however.
class (Attributes lore,
       FParamAttr lore ~ DeclType,
       LParamAttr lore ~ Type,
       RetType lore ~ DeclExtType,
       BranchType lore ~ ExtType,
       SetType (LetAttr lore)) =>
      Bindable lore where
  mkExpPat :: [Ident] -> [Ident] -> Exp lore -> Pattern lore
  mkExpAttr :: Pattern lore -> Exp lore -> ExpAttr lore
  mkBody :: Stms lore -> Result -> Body lore
  mkLetNames :: (MonadFreshNames m, HasScope lore m) =>
                [VName] -> Exp lore -> m (Stm lore)

-- | A monad that supports the creation of bindings from expressions
-- and bodies from bindings, with a specific lore.  This is the main
-- typeclass that a monad must implement in order for it to be useful
-- for generating or modifying Futhark code.
--
-- Very important: the methods should not have any significant side
-- effects!  They may be called more often than you think, and the
-- results thrown away.  It is acceptable for them to create new
-- bindings, however.
class (Attributes (Lore m),
       MonadFreshNames m, Applicative m, Monad m,
       LocalScope (Lore m) m,
       Fail.MonadFail m) =>
      MonadBinder m where
  type Lore m :: Data.Kind.Type
  mkExpAttrM :: Pattern (Lore m) -> Exp (Lore m) -> m (ExpAttr (Lore m))
  mkBodyM :: Stms (Lore m) -> Result -> m (Body (Lore m))
  mkLetNamesM :: [VName] -> Exp (Lore m) -> m (Stm (Lore m))
  addStm      :: Stm (Lore m) -> m ()
  addStm      = addStms . oneStm
  addStms     :: Stms (Lore m) -> m ()
  collectStms :: m a -> m (a, Stms (Lore m))
  certifying :: Certificates -> m a -> m a

mkLetM :: MonadBinder m => Pattern (Lore m) -> Exp (Lore m) -> m (Stm (Lore m))
mkLetM pat e = Let pat <$> (StmAux mempty <$> mkExpAttrM pat e) <*> pure e

letBind :: MonadBinder m =>
           Pattern (Lore m) -> Exp (Lore m) -> m [Ident]
letBind pat e = do
  bnd <- mkLetM pat e
  addStm bnd
  return $ patternValueIdents $ stmPattern bnd

letBind_ :: MonadBinder m =>
            Pattern (Lore m) -> Exp (Lore m) -> m ()
letBind_ pat e = void $ letBind pat e

mkLet :: Bindable lore => [Ident] -> [Ident] -> Exp lore -> Stm lore
mkLet ctx val e =
  let pat = mkExpPat ctx val e
      attr = mkExpAttr pat e
  in Let pat (StmAux mempty attr) e

letBindNames :: MonadBinder m =>
                [VName] -> Exp (Lore m) -> m [Ident]
letBindNames names e = do
  bnd <- mkLetNamesM names e
  addStm bnd
  return $ patternValueIdents $ stmPattern bnd

letBindNames_ :: MonadBinder m =>
                [VName] -> Exp (Lore m) -> m ()
letBindNames_ names e = void $ letBindNames names e

collectStms_ :: MonadBinder m => m a -> m (Stms (Lore m))
collectStms_ = fmap snd . collectStms

bodyBind :: MonadBinder m => Body (Lore m) -> m [SubExp]
bodyBind (Body _ bnds es) = do
  addStms bnds
  return es

-- | Add several bindings at the outermost level of a 'Body'.
insertStms :: Bindable lore => Stms lore -> Body lore -> Body lore
insertStms bnds1 (Body _ bnds2 res) = mkBody (bnds1<>bnds2) res

-- | Add a single binding at the outermost level of a 'Body'.
insertStm :: Bindable lore => Stm lore -> Body lore -> Body lore
insertStm = insertStms . oneStm
