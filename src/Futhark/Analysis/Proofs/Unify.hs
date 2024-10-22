-- Unification of expressions with bound variables.
-- This module lets us answer the question:
--   Are two expressions syntactically identical
--   up to the names of bound variables?
{-# OPTIONS_GHC -Wno-orphans #-}

module Futhark.Analysis.Proofs.Unify
  ( Unify (..),
    Renameable (..),
    Replacement,
    ReplacementBuilder (..),
    Replaceable (rep),
    Substitution (..),
    FreeVariables (..),
    Hole (justHole),
    freshName,
    renameSame,
    unifies_,
    sub,
    unifies,
    renameAnd,
  )
where

import Control.Monad (foldM, msum)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Futhark.Analysis.Proofs.IndexFn (IndexFnM)
import Futhark.Analysis.Proofs.Util (prettyName)
import Futhark.FreshNames qualified as FreshNames
import Futhark.MonadFreshNames (MonadFreshNames (getNameSource), VNameSource, newNameFromString)
import Futhark.SoP.SoP (SoP, Term, addSoPs, int2SoP, justSym, mulSoPs, sopFromList, sopToList, sopToLists, term2SoP, termToList, toTerm, zeroSoP)
import Futhark.SoP.SoP qualified as SoP
import Futhark.Util.Pretty (Pretty (pretty), braces, commastack)
import Language.Futhark (VName)

class (Ord a) => FreeVariables a where
  fv :: a -> S.Set VName

-- | Produce a fresh name, using the given name as a template.
-- Like "FreshNames.newName", except it lets us reuse the same name
-- source in multiple places while ensuring that the monadic name source
-- continues to generate unique names afterwards.
freshName :: (MonadFreshNames m) => VNameSource -> m (VName, VNameSource)
freshName vns = do
  -- All renamed names must have the same base name. Otherwise renaming two
  -- expressions with differently named bound variables will not produce
  -- identical renamings.
  x <- newNameFromString "x"
  -- Note that we are unnecessarily incrementing the monadic name source above.
  -- The monadic name source needs only be updated to the maximum tag over all
  -- places where freshName is used on successions of vns.
  let (j, vns') = FreshNames.newName vns x
  pure (j, vns')

class Renameable u where
  -- Implements subC(id,tau,e) from Sieg and Kaufmann where
  -- tau is a renaming of bound variables. The context C is given by
  -- VNameSource in MonadFreshNames.
  rename_ :: (MonadFreshNames m) => VNameSource -> M.Map VName VName -> u -> m u

  -- Rename bound variables in u. Equivalent to subC(id,id,e).
  rename :: (MonadFreshNames m) => VNameSource -> u -> m u
  rename vns = rename_ vns mempty

-- Rename bound variables in `a` and `b`. Renamed variables are
-- identical, if `a` and `b` are syntactically equivalent.
renameSame :: (MonadFreshNames m, Renameable a, Renameable b) => a -> b -> m (a, b)
renameSame a b = do
  vns <- getNameSource
  a' <- rename vns a
  b' <- rename vns b
  pure (a', b')

type Replacement u = M.Map VName (SoP u)

class Replaceable v u where
  -- Implements the replacement operation from Sieg and Kaufmann.
  rep :: Replacement u -> v -> SoP u

class ReplacementBuilder v u where
  addRep :: VName -> v -> Replacement u -> Replacement u
  mkRep :: VName -> v -> Replacement u
  mkRep vn x = addRep vn x mempty

data Substitution u = Substitution
  { mapping :: Replacement u,
    vns :: VNameSource
  }

instance (Show u) => Show (Substitution u) where
  show = show . mapping

instance (Eq u, Ord u) => Eq (Substitution u) where
  a == b = mapping a == mapping b

instance Semigroup (Substitution u) where
  a <> b =
    Substitution
      { mapping = mapping a <> mapping b,
        vns = vns a <> vns b
      }

instance (Pretty v) => Pretty (Substitution v) where
  pretty = braces . commastack . map prettyKV . M.toList . mapping
    where
      prettyKV (k, v) = prettyName k <> " : " <> pretty v

sub ::
  ( MonadFreshNames m,
    Renameable v,
    Replaceable v u
  ) =>
  Substitution u ->
  v ->
  m (SoP u)
sub s x = rep (mapping s) <$> rename (vns s) x

class Hole u where
  justHole :: u -> Maybe VName

class (Renameable v) => Unify v u where
  -- `unify_ k eq` is the unification algorithm from Sieg and Kauffmann,
  -- Unification for quantified formulae, 1993.
  -- Check whether x is a bound variable by `x >= k`.
  unify_ :: VName -> v -> v -> MaybeT IndexFnM (Replacement u)

  -- Unification on {subC(id,id,e) ~= subC(id,id,e')}
  --                  = {rename(e) ~= rename(e')}.
  unify :: (Pretty v) => v -> v -> IndexFnM (Maybe (Substitution u))
  unify = renameAnd unify_

renameAnd ::
  (Renameable t1, Renameable t2) =>
  (VName -> t1 -> t2 -> MaybeT IndexFnM (Replacement u)) ->
  t1 ->
  t2 ->
  IndexFnM (Maybe (Substitution u))
renameAnd unifier x y = runMaybeT $ do
  k <- newNameFromString "k"
  vns <- getNameSource
  a <- rename vns x
  b <- rename vns y
  s <- unifier k a b
  pure $ Substitution {mapping = s, vns = vns}

instance Renameable VName where
  rename_ _ tau x = pure $ M.findWithDefault x x tau

instance (Renameable u) => Renameable [u] where
  rename_ vns tau = mapM (rename_ vns tau)

instance (Renameable u, Ord u) => Renameable (Term u, Integer) where
  rename_ vns tau (x, c) = (,c) . toTerm <$> mapM (rename_ vns tau) (termToList x)

instance (Ord u, Renameable u) => Renameable (SoP u) where
  rename_ vns tau = fmap sopFromList . mapM (rename_ vns tau) . sopToList

instance FreeVariables VName where
  fv = S.singleton

instance (FreeVariables u) => FreeVariables (SoP u) where
  fv x = S.unions [fv t | (ts, _) <- sopToLists x, t <- ts]

instance (Ord u, Replaceable u u) => Replaceable (Term u, Integer) u where
  rep s (x, c) =
    SoP.scaleSoP c . foldr (mulSoPs . rep s) (int2SoP 1) . termToList $ x

instance (Ord u, Replaceable u u) => Replaceable (SoP u) u where
  rep s = foldr (addSoPs . rep s) zeroSoP . sopToList

instance (Ord u, Hole u) => ReplacementBuilder (SoP u) u where
  addRep _ x _
    | hasHole =
        error "Creating substitution for SoP with Hole. Are you trying to unify a Hole?"
    where
      hasHole = any (any (isJust . justHole) . fst) (sopToLists x)
  addRep vn x s = M.insert vn x s

unifies_ ::
  ( Replaceable v u,
    Replaceable u u,
    Unify v u,
    Unify u u,
    Ord u,
    Hole u
  ) =>
  VName ->
  [(v, v)] ->
  MaybeT IndexFnM (Replacement u)
unifies_ _ [] = pure mempty
unifies_ k (u : us) = do
  s0 <- uncurry (unify_ k) u
  foldM (\s (a, b) -> (s <>) <$> unify_ k (rep s a) (rep s b)) s0 us

unifies ::
  ( Replaceable v u,
    Replaceable u u,
    Unify v u,
    Unify u u,
    Ord u,
    Hole u
  ) =>
  [(v, v)] ->
  IndexFnM (Maybe (Substitution u))
unifies us =
  let (xs, ys) = unzip us
   in renameAnd (\k as bs -> unifies_ k (zip as bs)) xs ys

unifyAnyPerm ::
  ( Replaceable v u,
    Replaceable u u,
    Unify v u,
    Unify u u,
    Ord u,
    Hole u
  ) =>
  VName ->
  [v] ->
  [v] ->
  MaybeT IndexFnM (Replacement u)
unifyAnyPerm k xs ys
  | length xs == length ys =
      -- Extract left-most non-fail action, if there is one.
      msum $ map (unifies_ k . zip xs) (L.permutations ys)
  | otherwise = fail "unifyAnyPerm unequal lengths"

instance
  ( Replaceable u u,
    Unify u u,
    Hole u,
    Ord u
  ) =>
  Unify (Term u, Integer) u
  where
  -- Unify on permutations of symbols in term.
  unify_ k (xs, a) (ys, b)
    | Just h <- justSym (term2SoP xs a) >>= justHole =
        pure $ addRep h (term2SoP ys b) mempty
    -- Attempt to match hole to multiplicative constant b, if necessary.
    | a == 1,
      x' : xs' <- termToList xs,
      Just h <- justHole x',
      not (null ys) =
        msum
          [ unifyAnyPerm k (termToList xs) (termToList ys),
            do
              -- lift $ debugM $ "unify x = " <> prettyString xs <> "   y =" <> prettyString ys
              let s = addRep h (int2SoP b :: SoP u) mempty
              let x = rep s (term2SoP xs' 1)
              let y = rep s (term2SoP ys 1)
              (s <>) <$> unify_ k x y
          ]
    | a == b = unifyAnyPerm k (termToList xs) (termToList ys)
    | otherwise = fail "Unable to unify constants."

instance
  ( Replaceable u u,
    Unify u u,
    Ord u,
    Hole u
  ) =>
  Unify (SoP u) u
  where
  -- Unify on permutations of terms.
  unify_ k x y
    | Just h <- justSym x >>= justHole = pure $ addRep h y mempty
    | otherwise = unifyAnyPerm k (sopToList x) (sopToList y)
