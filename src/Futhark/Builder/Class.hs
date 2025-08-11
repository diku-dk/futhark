{-# LANGUAGE TypeFamilies #-}

-- | This module defines a convenience typeclass for creating
-- normalised programs.
--
-- See "Futhark.Construct" for a high-level description.
module Futhark.Builder.Class
  ( Buildable (..),
    mkLet,
    mkLet',
    MonadBuilder (..),
    insertStms,
    insertStm,
    censorStms,
    letBind,
    letBindNames,
    collectStms_,
    bodyBind,
    attributing,
    auxing,
    module Futhark.MonadFreshNames,
  )
where

import Data.Kind qualified
import Futhark.IR
import Futhark.MonadFreshNames

-- | The class of representations that can be constructed solely from
-- an expression, within some monad.  Very important: the methods
-- should not have any significant side effects!  They may be called
-- more often than you think, and the results thrown away.  If used
-- exclusively within a 'MonadBuilder' instance, it is acceptable for
-- them to create new bindings, however.
class
  ( ASTRep rep,
    FParamInfo rep ~ DeclType,
    LParamInfo rep ~ Type,
    RetType rep ~ DeclExtType,
    BranchType rep ~ ExtType
  ) =>
  Buildable rep
  where
  mkExpPat :: [Ident] -> Exp rep -> Pat (LetDec rep)
  mkExpDec :: Pat (LetDec rep) -> Exp rep -> ExpDec rep
  mkBody :: (IsResult res) => Stms rep -> [res] -> GBody rep res
  mkLetNames ::
    (MonadFreshNames m, HasScope rep m) =>
    [VName] ->
    Exp rep ->
    m (Stm rep)

-- | A monad that supports the creation of bindings from expressions
-- and bodies from bindings, with a specific rep.  This is the main
-- typeclass that a monad must implement in order for it to be useful
-- for generating or modifying Futhark code.  Most importantly
-- maintains a current state of 'Stms' (as well as a 'Scope') that
-- have been added with 'addStm'.
--
-- Very important: the methods should not have any significant side
-- effects!  They may be called more often than you think, and the
-- results thrown away.  It is acceptable for them to create new
-- bindings, however.
class
  ( ASTRep (Rep m),
    MonadFreshNames m,
    Applicative m,
    Monad m,
    LocalScope (Rep m) m
  ) =>
  MonadBuilder m
  where
  type Rep m :: Data.Kind.Type
  mkExpDecM :: Pat (LetDec (Rep m)) -> Exp (Rep m) -> m (ExpDec (Rep m))
  mkBodyM :: (IsResult res) => Stms (Rep m) -> [res] -> m (GBody (Rep m) res)
  mkLetNamesM :: [VName] -> Exp (Rep m) -> m (Stm (Rep m))

  -- | Add a statement to the 'Stms' under construction.
  addStm :: Stm (Rep m) -> m ()
  addStm = addStms . oneStm

  -- | Add multiple statements to the 'Stms' under construction.
  addStms :: Stms (Rep m) -> m ()

  -- | Obtain the statements constructed during a monadic action,
  -- instead of adding them to the state.
  collectStms :: m a -> m (a, Stms (Rep m))

  -- | Add the provided certificates to any statements added during
  -- execution of the action.
  certifying :: Certs -> m a -> m a
  certifying = censorStms . fmap . certify

-- | Apply a function to the statements added by this action.
censorStms ::
  (MonadBuilder m) =>
  (Stms (Rep m) -> Stms (Rep m)) ->
  m a ->
  m a
censorStms f m = do
  (x, stms) <- collectStms m
  addStms $ f stms
  pure x

-- | Add the given attributes to any statements added by this action.
attributing :: (MonadBuilder m) => Attrs -> m a -> m a
attributing attrs = censorStms $ fmap $ attribute attrs

-- | Add the certificates and attributes to any statements added by
-- this action.
auxing :: (MonadBuilder m) => StmAux anyrep -> m a -> m a
auxing outer
  | stmAuxCerts outer == mempty,
    stmAuxAttrs outer == mempty,
    stmAuxLoc outer == mempty =
      id
  | otherwise = censorStms $ fmap onStm
  where
    onStm (Let pat aux e) =
      Let pat aux' e
      where
        aux' =
          aux
            { stmAuxAttrs = stmAuxAttrs outer <> stmAuxAttrs aux,
              stmAuxCerts = stmAuxCerts outer <> stmAuxCerts aux,
              stmAuxLoc =
                if stmAuxLoc aux == mempty
                  then stmAuxLoc outer
                  else stmAuxLoc aux
            }

-- | Add a statement with the given pattern and expression.
letBind ::
  (MonadBuilder m) =>
  Pat (LetDec (Rep m)) ->
  Exp (Rep m) ->
  m ()
letBind pat e =
  addStm =<< Let pat <$> (defAux <$> mkExpDecM pat e) <*> pure e

-- | Construct a 'Stm' from identifiers for the context- and value
-- part of the pattern, as well as the expression.
mkLet :: (Buildable rep) => [Ident] -> Exp rep -> Stm rep
mkLet ids e =
  let pat = mkExpPat ids e
      dec = mkExpDec pat e
   in Let pat (defAux dec) e

-- | Like mkLet, but also take attributes and certificates from the
-- given 'StmAux'.
mkLet' :: (Buildable rep) => [Ident] -> StmAux a -> Exp rep -> Stm rep
mkLet' ids (StmAux cs attrs loc _) e =
  let pat = mkExpPat ids e
      dec = mkExpDec pat e
   in Let pat (StmAux cs attrs loc dec) e

-- | Add a statement with the given pattern element names and
-- expression.
letBindNames :: (MonadBuilder m) => [VName] -> Exp (Rep m) -> m ()
letBindNames names e = addStm =<< mkLetNamesM names e

-- | As 'collectStms', but throw away the ordinary result.
collectStms_ :: (MonadBuilder m) => m a -> m (Stms (Rep m))
collectStms_ = fmap snd . collectStms

-- | Add the statements of the body, then return the body result.
bodyBind :: (MonadBuilder m) => Body (Rep m) -> m Result
bodyBind (Body _ stms res) = do
  addStms stms
  pure res

-- | Add several bindings at the outermost level of a t'Body'.
insertStms :: (Buildable rep, IsResult res) => Stms rep -> GBody rep res -> GBody rep res
insertStms stms1 (Body _ stms2 res) = mkBody (stms1 <> stms2) res

-- | Add a single binding at the outermost level of a t'Body'.
insertStm :: (Buildable rep, IsResult res) => Stm rep -> GBody rep res -> GBody rep res
insertStm = insertStms . oneStm
