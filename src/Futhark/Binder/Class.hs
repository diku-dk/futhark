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
  , attributing
  , auxing

  , module Futhark.MonadFreshNames
  )
where

import Control.Monad.Writer
import qualified Data.Kind

import Futhark.IR
import Futhark.MonadFreshNames

-- | The class of lores that can be constructed solely from an
-- expression, within some monad.  Very important: the methods should
-- not have any significant side effects!  They may be called more
-- often than you think, and the results thrown away.  If used
-- exclusively within a 'MonadBinder' instance, it is acceptable for
-- them to create new bindings, however.
class (ASTLore lore,
       FParamInfo lore ~ DeclType,
       LParamInfo lore ~ Type,
       RetType lore ~ DeclExtType,
       BranchType lore ~ ExtType,
       SetType (LetDec lore)) =>
      Bindable lore where
  mkExpPat :: [Ident] -> [Ident] -> Exp lore -> Pattern lore
  mkExpDec :: Pattern lore -> Exp lore -> ExpDec lore
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
class (ASTLore (Lore m),
       MonadFreshNames m, Applicative m, Monad m,
       LocalScope (Lore m) m) =>
      MonadBinder m where
  type Lore m :: Data.Kind.Type
  mkExpDecM :: Pattern (Lore m) -> Exp (Lore m) -> m (ExpDec (Lore m))
  mkBodyM :: Stms (Lore m) -> Result -> m (Body (Lore m))
  mkLetNamesM :: [VName] -> Exp (Lore m) -> m (Stm (Lore m))
  addStm      :: Stm (Lore m) -> m ()
  addStm      = addStms . oneStm
  addStms     :: Stms (Lore m) -> m ()
  collectStms :: m a -> m (a, Stms (Lore m))
  certifying :: Certificates -> m a -> m a
  certifying = censorStms . fmap . certify

-- | Apply a function to the statements added by this action.
censorStms :: MonadBinder m =>
              (Stms (Lore m) -> Stms (Lore m))
           -> m a -> m a
censorStms f m = do
  (x, stms) <- collectStms m
  addStms $ f stms
  return x

-- | Add the given attributes to any statements added by this action.
attributing :: MonadBinder m => Attrs -> m a -> m a
attributing attrs = censorStms $ fmap onStm
  where onStm (Let pat aux e) =
          Let pat aux { stmAuxAttrs = attrs <> stmAuxAttrs aux } e

-- | Add the certificates and attributes to any statements added by
-- this action.
auxing :: MonadBinder m => StmAux anylore -> m a -> m a
auxing (StmAux cs attrs _) = censorStms $ fmap onStm
  where onStm (Let pat aux e) =
          Let pat aux' e
          where aux' = aux { stmAuxAttrs = attrs <> stmAuxAttrs aux
                           , stmAuxCerts = cs <> stmAuxCerts aux
                           }

mkLetM :: MonadBinder m => Pattern (Lore m) -> Exp (Lore m) -> m (Stm (Lore m))
mkLetM pat e = Let pat <$> (defAux <$> mkExpDecM pat e) <*> pure e

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
      dec = mkExpDec pat e
  in Let pat (defAux dec) e

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

-- | Add several bindings at the outermost level of a t'Body'.
insertStms :: Bindable lore => Stms lore -> Body lore -> Body lore
insertStms bnds1 (Body _ bnds2 res) = mkBody (bnds1<>bnds2) res

-- | Add a single binding at the outermost level of a t'Body'.
insertStm :: Bindable lore => Stm lore -> Body lore -> Body lore
insertStm = insertStms . oneStm
